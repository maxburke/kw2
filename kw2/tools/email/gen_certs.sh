#!/bin/sh
openssl genrsa -des3 -out kw2_smtp.key 1024
openssl req -new -key kw2_smtp.key -out kw2_smtp.csr
cp kw2_smtp.key kw2_smtp.key.org
openssl rsa -in kw2_smtp.key.org -out kw2_smtp.key
openssl x509 -req -days 365 -in kw2_smtp.csr -signkey kw2_smtp.key -out kw2_smtp.crt
