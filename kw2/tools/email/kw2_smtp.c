#define _GNU_SOURCE

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <limits.h>

#include <signal.h>
#include <syslog.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/epoll.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <openssl/ssl.h>
#include <openssl/err.h>

#include <curl/curl.h>

#define VERIFY_SSL_impl(x, line) if (!(x)) { \
    unsigned long error_code = ERR_get_error(); \
    char error_buf[512]; \
    SSL_load_error_strings(); \
    ERR_error_string_n(error_code, error_buf, sizeof error_buf); \
    error_buf[511] = 0; \
    fprintf(stderr, __FILE__ "(" line "): %s", error_buf); \
    abort(); \
}
#define VERIFY_impl(x, line) if (!(x)) { perror(__FILE__ "(" line "): " #x); abort(); } else (void)0
#define STRINGIZE(x) STRINGIZE2(x)
#define STRINGIZE2(x) #x
#define VERIFY_SSL(x) VERIFY_SSL_impl(x, STRINGIZE(__LINE__))
#define VERIFY(x) VERIFY_impl(x, STRINGIZE(__LINE__)) 
#define UNUSED(x) (void)x
#define MIN(a,b) ((a)<(b)?(a):(b))

struct kw_text_chunk_t
{
    struct kw_text_chunk_t *next;
    const char *text;
};

enum kw_command_t
{
    COMMAND_NONE = 0,
    COMMAND_EHLO = 0x4f4c4845,
    COMMAND_HELO = 0x4f4c4548,
    COMMAND_MAIL = 0x4c49414d,
    COMMAND_RCPT = 0x54504352,
    COMMAND_DATA = 0x41544144,
    COMMAND_RSET = 0x54455352,
    COMMAND_VRFY = 0x59465256,
    COMMAND_EXPN = 0x4e505845,
    COMMAND_HELP = 0x504c4548,
    COMMAND_NOOP = 0x504f4f4e,
    COMMAND_QUIT = 0x54495551
};

enum kw_command_result_t
{
    RESULT_OK,
    RESULT_NEED_MORE_DATA
};

enum kw_state_t
{
    STATE_NEW,
    STATE_SSL_START,
    STATE_SSL_ACCEPT,
    STATE_HANDLE_COMMAND,
    STATE_DONE,
    STATE_INVALID = -1
};

enum kw_response_t
{
    RESPONSE_GREETING,
    RESPONSE_EHLO,
    RESPONSE_HELO,
    RESPONSE_OK,
    RESPONSE_HELP,
    RESPONSE_QUIT,
    RESPONSE_DATA,
    RESPONSE_ERROR_BAD_SEQUENCE,
    RESPONSE_ERROR_EXCEEDED_STORAGE,
    RESPONSE_ERROR_NOT_IMPLEMENTED,
    RESPONSE_NUM_RESPONSES
};

struct kw_connection_t;

typedef ssize_t (*kw_read_function_t)(struct kw_connection_t *, void *, size_t);
typedef ssize_t (*kw_write_function_t)(struct kw_connection_t *, const void *, size_t);

#define CONNECTION_BUFFER_SIZE (16 * 1024 * 1024)
#define MAX_MESSAGE_SIZE (15 * 1024 * 1024)
#define MAX_MESSAGE_SIZE_STRING "15728640"
#define CRLF "\x0D\x0A"
#define DOMAIN "mx.kobbweb.net"

struct kw_connection_t
{
    int fd;
    int valid;
    SSL *ssl;
    enum kw_state_t state;
    enum kw_command_t command;
    kw_read_function_t read;
    kw_write_function_t write;

    size_t post_fragment;
    size_t post_pos;
    size_t post_fragment_length;
    struct kw_text_chunk_t *to;
    struct kw_text_chunk_t *from;
    struct kw_text_chunk_t *data;
    char *buffer;
    char *buffer_end;
    char *buffer_ptr;
    char *buffer_recv_ptr;
};

static struct kw_connection_t **kw_connections;
static size_t kw_current_connection;
static size_t kw_num_connections;
static SSL_CTX *kw_ssl_ctx;

static void
kw_log_connection_error(struct kw_connection_t *conn, int priority)
{
#define BUFFER_SIZE 1024
    char buffer[BUFFER_SIZE + 1];

    if (conn->ssl && ERR_peek_error() != 0)
    {
        unsigned long error_code;
        SSL_load_error_strings();

        while (ERR_peek_error() != 0)
        {
            error_code = ERR_get_error();
            ERR_error_string_n(error_code, buffer, BUFFER_SIZE);
            buffer[BUFFER_SIZE] = 0;
            syslog(priority, "%s", buffer);
        }
    }

    if (errno)
    {
        strerror_r(errno, buffer, BUFFER_SIZE);
        buffer[BUFFER_SIZE] = 0;
        syslog(priority, "%s", buffer);
    }
}

static int
kw_socket_open(int port)
{
    int socket_fd;
    struct sockaddr_in addr;
    const int LISTEN_BACKLOG = 50;

    memset(&addr, 0, sizeof addr);
    socket_fd = socket(AF_INET, SOCK_STREAM, 0);
    VERIFY(socket_fd != -1);

    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_family = AF_INET;

    VERIFY(bind(socket_fd, (struct sockaddr *)&addr, sizeof addr) == 0);
    VERIFY(listen(socket_fd, LISTEN_BACKLOG) == 0);

    return socket_fd;
}

static void
kw_accept_and_add_to_epoll_list(int epoll_fd, int socket_fd)
{
    int new_connection_fd;
    struct epoll_event event;
    struct sockaddr_in connection;
    socklen_t size = sizeof connection;

    memset(&event, 0, sizeof event);

    new_connection_fd = accept(socket_fd, (struct sockaddr *)&connection, &size);
    VERIFY(new_connection_fd != -1);
    fcntl(new_connection_fd, F_SETFD, O_NONBLOCK);
    event.events = EPOLLIN | EPOLLOUT;
    event.data.fd = new_connection_fd;
    VERIFY(epoll_ctl(epoll_fd, EPOLL_CTL_ADD, new_connection_fd, &event) != -1);
}

static void *
kw_alloc(struct kw_connection_t *conn, size_t size)
{
    /* kw_alloc can only be called when there is no more data to be processed as
       it will possibly trample the area that conn->buffer_recv_ptr points to. */
    char *ptr = conn->buffer_ptr;
    size = (size + (sizeof(void *) - 1)) & (~(sizeof(void *) - 1));
    conn->buffer_ptr += size;
    return ptr;
}

static void
kw_remove_from_epoll_list(int epoll_fd, int socket_fd)
{
    VERIFY(epoll_ctl(epoll_fd, EPOLL_CTL_DEL, socket_fd, NULL) != -1);
}

static ssize_t
kw_read(struct kw_connection_t *conn, void *data, size_t size)
{
    ssize_t rv;
    
    rv = read(conn->fd, data, size);
    VERIFY(rv >= 0);
    return rv;
}

static ssize_t
kw_write(struct kw_connection_t *conn, const void *data, size_t size)
{
    size_t bytes_remaining;
    const char *ptr;

    bytes_remaining = size;
    ptr = data;

    while (bytes_remaining != 0)
    {
        ssize_t rv;

        rv = write(conn->fd, ptr, bytes_remaining);

        if (rv < 0)
        {
            return rv;
        }

        ptr += rv;
        bytes_remaining -= (size_t)rv;
    }
    
    return (ssize_t)size;
}

static ssize_t
kw_read_ssl(struct kw_connection_t *conn, void *data, size_t size)
{
    VERIFY(size < INT_MAX);
    return SSL_read(conn->ssl, data, (int)size);
}

static ssize_t
kw_write_ssl(struct kw_connection_t *conn, const void *data, size_t size)
{
    int int_size = (int)size;
    int rv;

    VERIFY(size < INT_MAX);
    rv = SSL_write(conn->ssl, data, int_size);
    VERIFY(rv == int_size);
    return rv;
}

static void
kw_initialize_connection(struct kw_connection_t *conn)
{
    VERIFY(conn->valid == 0);

    conn->fd = -1;
    conn->ssl = NULL;
    conn->state = STATE_NEW;
    conn->command = COMMAND_NONE;
    conn->read = kw_read;
    conn->write = kw_write;
    conn->to = NULL;
    conn->from = NULL;
    conn->data = NULL;
    conn->buffer_ptr = conn->buffer;
    conn->buffer_recv_ptr = conn->buffer;
}

static struct kw_connection_t *
kw_allocate_connection()
{
    size_t allocation_size;
    size_t new_num_elements;
    size_t i;
    struct kw_connection_t *new_connections;
    char *new_blocks;

    if (kw_current_connection != kw_num_connections)
    {
        struct kw_connection_t *conn = kw_connections[kw_current_connection++]; 
        kw_initialize_connection(conn);
        return conn;
    }

#define REALLOCATION_DELTA 16
    new_num_elements = kw_num_connections + REALLOCATION_DELTA;
    allocation_size = sizeof(struct kw_connection_t *) * new_num_elements;
    kw_connections = realloc(kw_connections, allocation_size);

    new_connections = calloc(REALLOCATION_DELTA, sizeof(struct kw_connection_t));
    new_blocks = malloc(REALLOCATION_DELTA * CONNECTION_BUFFER_SIZE);

    for (i = kw_num_connections; i < new_num_elements; ++i)
    {
        kw_connections[i] = new_connections + i;
        kw_connections[i]->buffer = &new_blocks[i * CONNECTION_BUFFER_SIZE];
        kw_connections[i]->buffer_end = &new_blocks[(i + 1) * CONNECTION_BUFFER_SIZE];
    }

    kw_num_connections = new_num_elements;
    return kw_allocate_connection();
}

static struct kw_connection_t *
kw_acquire_connection(int fd)
{
    size_t i;
    struct kw_connection_t *conn;

    for (i = 0; i < kw_current_connection; ++i)
    {
        if (kw_connections[i]->fd == fd)
        {
            return kw_connections[i];
        }
    }

    conn = kw_allocate_connection();
    conn->state = STATE_NEW;
    conn->fd = fd;
    conn->valid = 1;

    return conn;
}

static void
kw_start_tls(struct kw_connection_t *conn)
{
    SSL *ssl;
    ssl = SSL_new(kw_ssl_ctx);
    SSL_set_fd(ssl, conn->fd);
    conn->ssl = ssl;
}

static void
kw_accept_tls(struct kw_connection_t *conn)
{
    int rv = SSL_accept(conn->ssl);
    int error;
    char error_buf[512];

    if (rv == 1)
    {
        conn->read = kw_read_ssl;
        conn->write = kw_write_ssl;
        return;
    }

    error = SSL_get_error(conn->ssl, rv);
    strerror_r(errno, error_buf, 511);
    error_buf[511] = 0;

#define CASE(X) case X: syslog(LOG_NOTICE, "SSL ERROR: (%d) %s - %s", rv, #X, error_buf); break
    switch (error)
    {
        CASE(SSL_ERROR_NONE);
        CASE(SSL_ERROR_ZERO_RETURN);
        CASE(SSL_ERROR_WANT_READ);
        CASE(SSL_ERROR_WANT_WRITE);
        CASE(SSL_ERROR_WANT_CONNECT);
        CASE(SSL_ERROR_WANT_ACCEPT);
        CASE(SSL_ERROR_WANT_X509_LOOKUP);
        CASE(SSL_ERROR_SYSCALL);
        CASE(SSL_ERROR_SSL);
        default:
            syslog(LOG_NOTICE, "SSL ERROR: (%d) %d %s", rv, error, error_buf);
            break;
    }
    raise(SIGTRAP);
}

static void
kw_release_connection(struct kw_connection_t *conn, int fd)
{
    size_t i;

    VERIFY(conn->valid);
    VERIFY(conn->fd == fd);

    if (conn->ssl)
    {
        SSL_free(conn->ssl);
    }
    conn->valid = 0;
    conn->state = STATE_INVALID;

    for (i = 0; i < kw_current_connection; ++i)
    {
        if (kw_connections[i] == conn)
        {
            break;
        }
    }
    VERIFY(i < kw_current_connection);

    memset(conn->buffer, 0, CONNECTION_BUFFER_SIZE);

    --kw_current_connection;
    kw_connections[i] = kw_connections[kw_current_connection];
    kw_connections[kw_current_connection] = conn;
}

static void
kw_send_response(struct kw_connection_t *conn, enum kw_response_t response)
{
    static const char *response_strings[] = {
        /* RESPONSE_GREETING */                 "220 " DOMAIN " ESMTP" CRLF,
        /* RESPONSE_EHLO */                     "250-" DOMAIN " ready to accept your load" CRLF
            /* TODO: TLS/SSL is disabled because it's causing problems currently. */
/*                                    "250-STARTTLS" CRLF*/
                                                "250-8BITMIME" CRLF
                                                "250 SIZE " MAX_MESSAGE_SIZE_STRING CRLF,
        /* RESPONSE_HELO */                     "250 " DOMAIN " ready to accept your load" CRLF,
        /* RESPONSE_OK */                       "250 OK" CRLF,
        /* RESPONSE_HELP */                     "214 http://www.ietf.org/rfc/rfc2821.txt" CRLF,
        /* RESPONSE_QUIT */                     "221 " DOMAIN " bye!" CRLF,
        /* RESPONSE_DATA */                     "354 Start mail input; end with <CRLF>.<CRLF>" CRLF,
        /* RESPONSE_ERROR_BAD_SEQUENCE */       "503 Bad sequence of commands" CRLF,
        /* RESPONSE_ERROR_EXCEEDED_STORAGE */   "552 Exceeded storage allocation" CRLF,
        /* RESPONSE_ERROR_NOT_IMPLEMENTED */    "502 Command not implemented" CRLF
    };
    const char *string;
    size_t string_length;

    if (response >= RESPONSE_NUM_RESPONSES || response < RESPONSE_GREETING) 
    {
        response = RESPONSE_ERROR_BAD_SEQUENCE;
    }

    string = response_strings[response];
    string_length = strlen(string);
    conn->write(conn, string, string_length);
}

static enum kw_command_result_t
kw_command_ehlo(struct kw_connection_t *conn)
{
    kw_send_response(conn, RESPONSE_EHLO);
    return RESULT_OK;
}

static enum kw_command_result_t
kw_command_helo(struct kw_connection_t *conn)
{
    kw_send_response(conn, RESPONSE_HELO);
    return RESULT_OK;
}

static enum kw_command_result_t
kw_command_help(struct kw_connection_t *conn)
{
    kw_send_response(conn, RESPONSE_HELP);
    return RESULT_OK;
}

static enum kw_command_result_t
kw_command_noop(struct kw_connection_t *conn)
{
    kw_send_response(conn, RESPONSE_OK);
    return RESULT_OK;
}

static enum kw_command_result_t
kw_command_rset(struct kw_connection_t *conn)
{
    conn->buffer_ptr = conn->buffer;
    conn->to = NULL;
    conn->from = NULL;
    conn->data = NULL;
    kw_send_response(conn, RESPONSE_OK);
    return RESULT_OK;
}

static enum kw_command_result_t
kw_command_quit(struct kw_connection_t *conn)
{
    kw_send_response(conn, RESPONSE_QUIT);
    return RESULT_OK;
}

static char *
kw_extract_email_address(struct kw_connection_t *conn)
{
    size_t heap_size;
    size_t from_length;
    char *from_start;
    char *from_end;
    char *string;

    heap_size = conn->buffer_end - conn->buffer_ptr;
    from_start = memchr(conn->buffer_ptr, '<', heap_size);
    from_end = memchr(conn->buffer_ptr, '>', heap_size);
    from_length = from_end - from_start - 1;

    if (!from_start || !from_end)
    {
        return NULL;
    }

    string = kw_alloc(conn, from_length + 1);
    memmove(string, from_start + 1, from_length);
    string[from_length] = '\0';

    return string;
}

static enum kw_command_result_t
kw_command_mail(struct kw_connection_t *conn)
{
    char *string;
    struct kw_text_chunk_t *chunk;

    string = kw_extract_email_address(conn);
    if (string == NULL)
    {
        return RESULT_NEED_MORE_DATA;
    }

    chunk = kw_alloc(conn, sizeof(struct kw_text_chunk_t));
    chunk->text = string;
    chunk->next = conn->from;
    conn->from = chunk;

    kw_send_response(conn, RESPONSE_OK);
    return RESULT_OK;
}

static enum kw_command_result_t
kw_command_rcpt(struct kw_connection_t *conn)
{
    char *string;
    struct kw_text_chunk_t *chunk;

    string = kw_extract_email_address(conn);
    if (string == NULL)
    {
        return RESULT_NEED_MORE_DATA;
    }

    chunk = kw_alloc(conn, sizeof(struct kw_text_chunk_t));
    chunk->text = string;
    chunk->next = conn->from;
    conn->to = chunk;

    kw_send_response(conn, RESPONSE_OK);
    return RESULT_OK;
}

static enum kw_command_result_t
kw_command_data(struct kw_connection_t *conn)
{
    kw_send_response(conn, RESPONSE_DATA);
    return RESULT_OK;
}

static enum kw_command_result_t
kw_receive_data(struct kw_connection_t *conn)
{
    static const char needle[] = CRLF "." CRLF;
    static const size_t needle_size = (sizeof needle) - 1;

    char *haystack;
    char *data;
    char *result;
    size_t haystack_size;
    size_t data_size;
    struct kw_text_chunk_t *chunk;

    haystack = conn->buffer_ptr;
    haystack_size = conn->buffer_end - conn->buffer_ptr;
    result = memmem(haystack, haystack_size, needle, needle_size);
    data_size = result - conn->buffer_ptr;

    if (result == NULL)
    {
        return RESULT_NEED_MORE_DATA;
    }

    VERIFY(strstr(haystack, "QUIT" CRLF) == NULL);

    data = kw_alloc(conn, data_size + 1);
    memmove(data, haystack, data_size);
    data[data_size] = '\0';

    chunk = kw_alloc(conn, sizeof(struct kw_text_chunk_t));
    chunk->text = data;
    chunk->next = conn->data;
    conn->data = chunk;

    kw_send_response(conn, RESPONSE_OK);
    syslog(LOG_NOTICE, "End of data, sending OK");
    return RESULT_OK;
}

#if 0
static const char *kw_json[] = {
    "{\"to\":\"",
    "\",\"from\":\"",
    "\",\"data\":\"",
    "\"}" CRLF
};
static const size_t kw_json_entries = sizeof kw_json / sizeof kw_json[0];

static size_t
kw_curl_read_fn(void *ptr, size_t size, size_t nmemb, void *userdata)
{
    struct kw_connection_t *connection = userdata;
    const char *ptrs[7];
    
    const size_t num_fragments = sizeof ptrs / sizeof ptrs[0];
    size_t bytes_available = size * nmemb;
    size_t bytes_copied = 0;
    char *dest = ptr;

    ptrs[0] = kw_json[0];
    ptrs[1] = connection->to->text;
    ptrs[2] = kw_json[1];
    ptrs[3] = connection->from->text;
    ptrs[4] = kw_json[2];
    ptrs[5] = connection->data->text;
    ptrs[6] = kw_json[3];

    for (;;)
    {
        size_t bytes_to_copy;
        size_t i;

        if (bytes_available == 0 || connection->post_fragment == num_fragments)
        {
            break;
        }

        if (connection->post_fragment_length == 0)
        {
            connection->post_fragment_length = strlen(ptrs[connection->post_fragment]);
        }

        bytes_to_copy = MIN(connection->post_fragment_length - connection->post_pos, bytes_available);
        memcpy(dest, ptrs[connection->post_fragment] + connection->post_pos, bytes_to_copy);

        for (i = 0; i < bytes_to_copy; ++i)
        {
            putchar(*(ptrs[connection->post_fragment] + connection->post_pos + i));
        }

        dest += bytes_to_copy;
        bytes_copied += bytes_to_copy;
        bytes_available -= bytes_to_copy;
        connection->post_pos += bytes_to_copy;

        if (connection->post_pos >= connection->post_fragment_length)
        {
            ++connection->post_fragment;
            connection->post_fragment_length = 0;
            connection->post_pos = 0;
        }
    }

    return bytes_copied;
}

static long
kw_data_size(struct kw_connection_t *connection)
{
    return (long)strlen(connection->data->text);
}
#endif
static void
kw_post_data(struct kw_connection_t *connection)
{
    CURL *curl;
    long return_value;
    CURLcode curl_return_value;
    struct curl_httppost *first = NULL;
    struct curl_httppost *last = NULL;
    char error_buffer[CURL_ERROR_SIZE + 1] = { 0 };
   
    pid_t pid = fork();
    VERIFY(pid >= 0);
    if (pid > 0)
    {
        return;
    }

    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();
    VERIFY(curl != NULL);
    curl_return_value = curl_easy_setopt(curl, CURLOPT_URL, "http://192.168.1.103:8000/mail");
    VERIFY(curl_return_value == 0);
    VERIFY(curl_formadd(&first, &last,
            CURLFORM_PTRNAME, "to",
            CURLFORM_PTRCONTENTS, connection->to->text,
            CURLFORM_END) == 0);
    VERIFY(curl_formadd(&first, &last,
            CURLFORM_PTRNAME, "from",
            CURLFORM_PTRCONTENTS, connection->from->text,
            CURLFORM_END) == 0);
    VERIFY(curl_formadd(&first, &last,
            CURLFORM_PTRNAME, "data",
            CURLFORM_PTRCONTENTS, connection->data->text,
            CURLFORM_END) == 0);
    curl_easy_setopt(curl, CURLOPT_HTTPPOST, first);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, error_buffer);

    curl_return_value = curl_easy_perform(curl);
    if (curl_return_value == 0) 
    {
        curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &return_value);
        printf("Transfer returned %ld\n", return_value);
    }
    else
    {
        printf("curl_easy_perform returned %d\n", curl_return_value);
        printf("%s\n", error_buffer);
    }

    curl_formfree(first);
    
    curl_easy_cleanup(curl);
    curl_global_cleanup();

    exit(0);
}

static void
kw_handle_command(struct kw_connection_t *conn)
{
    /* kw_handle_command reads all pending data into the buffer as scratch. 
       It is up to the functions that are called by kw_handle_command to 
       update the buffer pointers. */

    /* This function also progressively adds data to the buffer as it is
       received if the handler functions return RESULT_NEED_MORE_DATA */

    int result;
    int i;
    static const char start_tls_token[] = "STARTTLS";
    static const size_t start_tls_token_size = (sizeof start_tls_token) - 1;
    enum kw_command_result_t rv = RESULT_OK;
    int update_command = 1;

    union command_union
    {
        enum kw_command_t command;
        char raw_data[4];
    };

    union command_union command_union;
    
    result = conn->read(conn, conn->buffer_recv_ptr, conn->buffer_end - conn->buffer_recv_ptr);
    if (result < 0)
    {
        syslog(LOG_NOTICE, "Read function returned %d", result);
        kw_log_connection_error(conn, LOG_NOTICE);
        conn->state = STATE_DONE;
        return;
    }
    else
    {
        conn->buffer_recv_ptr += result;
    }

    VERIFY(conn->buffer_recv_ptr <= conn->buffer_end);

    for (i = 0; i < 4; ++i)
    {
        command_union.raw_data[i] = toupper(conn->buffer_ptr[i]);
    }

    switch (command_union.command)
    {
        case COMMAND_EHLO:
            rv = kw_command_ehlo(conn);
            break;
        case COMMAND_HELO:
            rv = kw_command_helo(conn);
            break;
        case COMMAND_MAIL:
            rv = kw_command_mail(conn);
            break;
        case COMMAND_RCPT:
            rv = kw_command_rcpt(conn);
            break;
        case COMMAND_DATA:
            rv = kw_command_data(conn);
            break;
        case COMMAND_RSET:
            rv = kw_command_rset(conn);
            break;
        case COMMAND_VRFY:
            kw_send_response(conn, RESPONSE_ERROR_NOT_IMPLEMENTED);
            break;
        case COMMAND_EXPN:
            kw_send_response(conn, RESPONSE_ERROR_NOT_IMPLEMENTED);
            break;
        case COMMAND_HELP:
            rv = kw_command_help(conn);
            break;
        case COMMAND_NOOP:
            rv = kw_command_noop(conn);
            break;
        case COMMAND_QUIT:
            rv = kw_command_quit(conn);
            conn->state = STATE_DONE;
            break;
        default:
            update_command = 0;
            if (strncasecmp(conn->buffer_ptr, start_tls_token, start_tls_token_size) == 0)
            {
                conn->command = COMMAND_NONE;
                conn->state = STATE_SSL_START;
            }
            else if (conn->command == COMMAND_DATA)
            {
                rv = kw_receive_data(conn);
                if (rv == RESULT_NEED_MORE_DATA && conn->buffer_recv_ptr == conn->buffer_end)
                {
                    kw_send_response(conn, RESPONSE_ERROR_EXCEEDED_STORAGE);
                }
            }
            else
            {
                /* If an unknown message is received, kill the connection */
                conn->state = STATE_DONE;
            }
            break;
    }

    /* The command field in the connection is only updated if we have received a valid
       4-letter command string (ie: EHLO, DATA, etc.) */
    if (update_command)
    {
        conn->command = command_union.command;
    }

    if (rv == RESULT_OK)
    {
        syslog(LOG_NOTICE, "%c%c%c%c", command_union.raw_data[0], command_union.raw_data[1], command_union.raw_data[2], command_union.raw_data[3]);
        conn->buffer_recv_ptr = conn->buffer_ptr;
    }
}

static int
handle_connection(int fd)
{
    struct kw_connection_t *conn;
    int rv = 0;

    conn = kw_acquire_connection(fd);
    VERIFY(conn->valid);

    /* Some state-handling mechanisms like kw_handle_command will set the state
       to STATE_DONE if an error has occurred and the connection needs to be 
       shut down and cleaned up so this loop is here to handle that case properly.
       */
    do
    {
        switch (conn->state)
        {
            case STATE_NEW:
                kw_send_response(conn, RESPONSE_GREETING);
                conn->state = STATE_HANDLE_COMMAND;
                break;
            case STATE_HANDLE_COMMAND:
                kw_handle_command(conn);
                break;
            case STATE_SSL_START:
                kw_start_tls(conn);
                conn->state = STATE_SSL_ACCEPT;
                break;
            case STATE_SSL_ACCEPT:
                kw_accept_tls(conn);
                conn->state = STATE_HANDLE_COMMAND;
                break;
            case STATE_DONE:
                if (conn->to && conn->from && conn->data)
                    kw_post_data(conn);
                kw_release_connection(conn, fd);
                rv = 1;
                break;
            default: 
                break;
        }
    } while (conn->state == STATE_DONE || conn->state == STATE_SSL_START);

    return rv;
}

static void
kw_initialize_ssl_ctx()
{
    const SSL_METHOD *method;

    method = SSLv23_server_method();
    kw_ssl_ctx = SSL_CTX_new(method);
    VERIFY_SSL(kw_ssl_ctx != NULL);

    VERIFY_SSL(SSL_CTX_use_certificate_file(kw_ssl_ctx, "kw2_smtp.crt", SSL_FILETYPE_PEM) == 1);
    /* TODO: Use a keystore instead of the unencrypted key below. */
    VERIFY_SSL(SSL_CTX_use_PrivateKey_file(kw_ssl_ctx, "kw2_smtp.key", SSL_FILETYPE_PEM) == 1);
    VERIFY_SSL(SSL_CTX_load_verify_locations(kw_ssl_ctx, NULL, "/usr/lib/ssl/certs"));
    SSL_CTX_set_verify(kw_ssl_ctx, SSL_VERIFY_PEER, NULL);
}

static int
find_fd(int array[], int element, size_t array_size)
{
    size_t i;

    for (i = 0; i < array_size; ++i)
    {
        if (array[i] == element)
        {
            return array[i];
        }
    }

    return -1;
}

int
main(void)
{
    int epoll_fd;
#define MAX_NUM_EVENTS 128
    struct epoll_event event;
    struct epoll_event events[MAX_NUM_EVENTS];
    int sockets[2] = { 25, 587 };
    int socket_fds[2] = { -1, -1 };
    size_t i;

#ifndef NDEBUG
    int syslog_flags = LOG_PERROR;
#else
    int syslog_flags = 0;
#endif

    memset(&event, 0, sizeof event);
    memset(events, 0, sizeof events);

    openlog("kw2_smtp", syslog_flags, LOG_USER);
    SSL_library_init();
    kw_initialize_ssl_ctx();
    epoll_fd = epoll_create1(0);
    VERIFY(epoll_fd >= 0);

    for (i = 0; i < sizeof sockets / sizeof sockets[0]; ++i)
    {
        int socket = sockets[i];
        socket_fds[i] = kw_socket_open(socket);

        event.events = EPOLLIN;
        event.data.fd = socket_fds[i];
        VERIFY(epoll_ctl(epoll_fd, EPOLL_CTL_ADD, socket_fds[i], &event) == 0);
    }

    for (;;)
    {
        int num_ready_fds;
        int i;

        num_ready_fds = epoll_wait(epoll_fd, events, MAX_NUM_EVENTS, -1);
        VERIFY(num_ready_fds != -1);

        for (i = 0; i < num_ready_fds; ++i)
        {
            int current_fd = events[i].data.fd;
            int socket_fd = find_fd(socket_fds, current_fd, sizeof socket_fds / sizeof socket_fds[0]);

            if (socket_fd != -1)
            {
                kw_accept_and_add_to_epoll_list(epoll_fd, socket_fd);
            }
            else if (handle_connection(current_fd))
            {
                kw_remove_from_epoll_list(epoll_fd, current_fd);
                close(current_fd);
/*goto done;*/
            }
        }
    }
/*done:*/

    for (i = 0; i < sizeof sockets / sizeof sockets[0]; ++i)
    {
        close(socket_fds[i]);
    }

    close(epoll_fd);
    closelog();
    return 0;
}
