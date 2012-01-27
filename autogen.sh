#! /bin/sh

bootstrap() {
  libtoolize -c && \
  aclocal -I config && \
  automake -a -c && \
  autoconf
}

bootstrap
