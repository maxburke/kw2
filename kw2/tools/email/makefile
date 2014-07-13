CC = gcc
CFLAGS = -g -Wextra -Wall -pedantic -Werror
LIBS = -lssl -lcrypto -lcurl
LDFLAGS = -g 
OBJ = kw2_smtp.o kw2_key.o
%.o: %.c
	$(CC) -c -o $@ $< $(CFLAGS)

kw2_smtp: $(OBJ)
	gcc -o $@ $^ $(LDFLAGS) $(LIBS)

.PHONY: clean
clean:
	rm -rf *.o kw2_smtp
