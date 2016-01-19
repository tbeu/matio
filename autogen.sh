#! /bin/sh

bootstrap() {
  case `uname` in Darwin*) glibtoolize --copy ;;
                        *) libtoolize --copy ;; esac && \
  aclocal -I config && \
  automake -a -c && \
  autoconf
}

bootstrap
