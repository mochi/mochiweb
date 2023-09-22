#!/bin/bash
set -e
cd "$(dirname "${BASH_SOURCE[0]}")"
# Create a new self-signed cert and key expiring in 10 years
openssl req \
    -newkey rsa:4096 \
    -x509 \
    -nodes \
    -days 3650 \
    -subj '/C=US/CN=localhost' \
    -keyout ./test_ssl_key.pem \
    -out ./test_ssl_cert.pem
