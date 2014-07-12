#include <stdio.h>
#include <time.h>

#define KEY_SIZE 32

static int
write_lisp_key(const char *generation_time, unsigned char *key, size_t key_size)
{
    size_t i;
    FILE *fp;

    fp = fopen("kw2_key.lisp", "w");

    if (!fp)
    {
        return 1;
    }

    fprintf(fp, ";;;; Key generated %s\n", generation_time);
    fprintf(fp, "(defparameter +kw2-key-size+ %lu)\n", key_size);
    fprintf(fp, "(defparameter +kw2-key+ #(");

    for (i = 0; i < key_size; ++i)
    {
        fprintf(fp, "%u%s", key[i], (i < (key_size - 1)) ? " " : "");
    }

    fprintf(fp, "))\n");
    fclose(fp);

    return 0;
}


static int
write_c_key(const char *generation_time, unsigned char *key, size_t key_size)
{
    size_t i;
    FILE *fp;

    fp = fopen("kw2_key.c", "w");

    if (!fp)
    {
        return 1;
    }

    fprintf(fp, "/*\n");
    fprintf(fp, " * Key generated %s", generation_time);
    fprintf(fp, " */\n\n");
    fprintf(fp, "#define KW2_KEY_SIZE %lu\n", key_size);
    fprintf(fp, "unsigned char kw2_key[KEY_SIZE] = {\n    ");

    for (i = 0; i < key_size; ++i)
    {
        fprintf(fp, "%u, ", key[i]);

        if (((i + 1) % (key_size / 4)) == 0 && i < (key_size - 1))
        {
            fprintf(fp, "\n    ");
        }
    }

    fprintf(fp, "\n};\n");
    fclose(fp);

    return 0;
}


int
main(void)
{
    FILE *fp;
    unsigned char buf[KEY_SIZE] = { 0 };
    time_t raw_time;
    struct tm *time_info;
    const char *time_string;

    time(&raw_time);
    time_info = localtime(&raw_time);
    time_string = asctime(time_info);

    fp = fopen("/dev/random", "rb");

    if (!fp)
    {
        fprintf(stderr, "Unable to open /dev/random, exiting\n");
        return 1;
    }

    if (fread(buf, 1, KEY_SIZE, fp) != KEY_SIZE) 
    {
        fprintf(stderr, "Unable to read from /dev/random, exiting\n");
        return 1;
    }

    if (write_lisp_key(time_string, buf, KEY_SIZE) 
            || write_c_key(time_string, buf, KEY_SIZE))
    {
        fprintf(stderr, "Unable to write key files, exiting\n");
        return 1;
    }

    return 0;
}
