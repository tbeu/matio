/*
 * Copyright (c) 2015-2026, The matio contributors
 * Copyright (c) 2005-2014, Christopher C. Hulbert
 * All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause */

#include <stdio.h>
#include <stdlib.h>

#include "matio_wrap.h"

int
main(int argc, char *argv[])
{
    if ( argc < 2 ) {
        fprintf(stderr, "Usage: %s <mat-file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    return MatioRead(argv[1]);
}
