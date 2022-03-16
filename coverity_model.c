/** @file coverity_model.c
 * @brief Modeling file for Coverity Scan to find resource leaks using hdf5 and zlib
 */
/*
 * Copyright (c) 2016-2022, The matio contributors
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#define hid_t void*
#define hsize_t size_t
#define herr_t void
typedef unsigned char  Byte;  /* 8 bits */
typedef unsigned char  Bytef; /* 8 bits */
typedef unsigned int   uInt;  /* 16 bits or more */
typedef unsigned long  uLong; /* 32 bits or more */
typedef void* (*alloc_func) (void* opaque, uInt items, uInt size);
typedef void   (*free_func) (void* opaque, void* address);

typedef enum {
    H5R_BADTYPE     =   (-1),   /*invalid Reference Type                     */
    H5R_OBJECT,                 /*Object reference                           */
    H5R_DATASET_REGION,         /*Dataset Region Reference                   */
    H5R_MAXTYPE                 /*highest type (Invalid as true type)        */
} H5R_type_t;

typedef enum H5T_class_t {
    H5T_NO_CLASS         = -1,  /*error                                      */
    H5T_INTEGER          = 0,   /*integer types                              */
    H5T_FLOAT            = 1,   /*floating-point types                       */
    H5T_TIME             = 2,   /*date and time types                        */
    H5T_STRING           = 3,   /*character string types                     */
    H5T_BITFIELD         = 4,   /*bit field types                            */
    H5T_OPAQUE           = 5,   /*opaque types                               */
    H5T_COMPOUND         = 6,   /*compound types                             */
    H5T_REFERENCE        = 7,   /*reference types                            */
    H5T_ENUM             = 8,   /*enumeration types                          */
    H5T_VLEN             = 9,   /*Variable-Length types                      */
    H5T_ARRAY            = 10,  /*Array types                                */
    H5T_NCLASSES                /*this must be last                          */
} H5T_class_t;

typedef enum H5S_class_t {
    H5S_NO_CLASS         = -1,  /*error                                      */
    H5S_SCALAR           = 0,   /*scalar variable                            */
    H5S_SIMPLE           = 1,   /*simple data space                          */
    H5S_NULL             = 2    /*null data space                            */
} H5S_class_t;

typedef struct z_stream_s {
    Bytef    *next_in; /* next input byte */
    uInt     avail_in; /* number of bytes available at next_in */
    uLong    total_in; /* total number of input bytes read so far */
    Bytef   *next_out; /* next output byte should be put there */
    uInt    avail_out; /* remaining free space at next_out */
    uLong   total_out; /* total number of bytes output so far */
    char         *msg; /* last error message, NULL if no error */
    void       *state; /* not visible by applications */
    alloc_func zalloc; /* used to allocate the internal state */
    free_func   zfree; /* used to free the internal state */
    void      *opaque; /* private data object passed to zalloc and zfree */
    int     data_type; /* best guess about the data type: binary or text */
    uLong       adler; /* adler32 value of the uncompressed data */
    uLong    reserved; /* reserved for future use */
} z_stream;

hid_t H5Dopen(hid_t file_id, const char *name, hid_t dapl_id) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Dclose");
    return id;
}

herr_t H5Dclose(hid_t id) {
    __coverity_free__(id);
    __coverity_mark_as_afm_freed__(id, "H5Dclose");
}

hid_t H5Dget_type(hid_t dataset_id) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Tclose");
    return id;
}

herr_t H5Tclose(hid_t id) {
    __coverity_free__(id);
    __coverity_mark_as_afm_freed__(id, "H5Tclose");
}

hid_t H5Aopen_by_name(hid_t loc_id, const char *obj_name, const char *attr_name, hid_t aapl_id, hid_t lapl_id) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Aclose");
    return id;
}

hid_t H5Acreate(hid_t loc_id, const char *attr_name, hid_t type_id, hid_t space_id, hid_t acpl_id, hid_t aapl_id) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Aclose");
    return id;
}

hid_t H5Acreate2(hid_t loc_id, const char *attr_name, hid_t type_id, hid_t space_id, hid_t acpl_id, hid_t aapl_id) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Aclose");
    return id;
}

herr_t H5Aclose(hid_t id) {
    __coverity_free__(id);
    __coverity_mark_as_afm_freed__(id, "H5Aclose");
}

hid_t H5Aget_space(hid_t attr_id) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Sclose");
    return id;
}

herr_t H5Sclose(hid_t id) {
    __coverity_free__(id);
    __coverity_mark_as_afm_freed__(id, "H5Sclose");
}

hid_t H5Rdereference(hid_t dataset, H5R_type_t ref_type, const void *ref) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Dclose");
    return id;
}

hid_t H5Aget_type(hid_t attr_id) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Tclose");
    return id;
}

hid_t H5Tcreate(H5T_class_t class, size_t size) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Tclose");
    return id;
}

hid_t H5Gopen(hid_t loc_id, const char * name, hid_t gapl_id) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Gclose");
    return id;
}

hid_t H5Gopen2(hid_t loc_id, const char * name, hid_t gapl_id) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Gclose");
    return id;
}

hid_t H5Gcreate(hid_t loc_id, const char *name, hid_t lcpl_id, hid_t gcpl_id, hid_t gapl_id) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Gclose");
    return id;
}

hid_t H5Gcreate2(hid_t loc_id, const char *name, hid_t lcpl_id, hid_t gcpl_id, hid_t gapl_id) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Gclose");
    return id;
}

hid_t H5Screate_simple(int rank, const hsize_t * current_dims, const hsize_t * maximum_dims) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Sclose");
    return id;
}

hid_t H5Dcreate(hid_t loc_id, const char *name, hid_t dtype_id, hid_t space_id, hid_t lcpl_id, hid_t dcpl_id, hid_t dapl_id) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Dclose");
    return id;
}

hid_t H5Dcreate2(hid_t loc_id, const char *name, hid_t dtype_id, hid_t space_id, hid_t lcpl_id, hid_t dcpl_id, hid_t dapl_id) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Dclose");
    return id;
}

herr_t H5Gclose(hid_t id) {
    __coverity_free__(id);
    __coverity_mark_as_afm_freed__(id, "H5Gclose");
}

hid_t H5Fopen(const char *name, unsigned flags, hid_t fapl_id) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Fclose");
    return id;
}

hid_t H5Fcreate( const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Fclose");
    return id;
}

herr_t H5Fclose(hid_t id) {
    __coverity_free__(id);
    __coverity_mark_as_afm_freed__(id, "H5Fclose");
}

hid_t H5Dget_space(hid_t dataset_id) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Sclose");
    return id;
}

hid_t H5Pcreate(hid_t cls_id) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Pclose");
    return id;
}

herr_t H5Pclose(hid_t id) {
    __coverity_free__(id);
    __coverity_mark_as_afm_freed__(id, "H5Pclose");
}

herr_t H5Rcreate(void *ref, hid_t loc_id, const char *name, H5R_type_t ref_type, hid_t space_id) {
    return __coverity_alloc__(10);
}

hid_t H5Tvlen_create(hid_t base_type_id) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Tclose");
    return id;
}

hid_t H5Tcopy(hid_t dtype_id) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Tclose");
    return id;
}

hid_t H5Screate(H5S_class_t type) {
    hid_t id = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(id, "H5Sclose");
    return id;
}

int inflateInit_(z_stream *strm, int level, const char *version, int stream_size) {
    strm->state = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(strm->state, "inflateEnd");
    __coverity_writeall__(strm);
    return 0;
}

int deflateInit_(z_stream *strm, int level, const char *version, int stream_size) {
    strm->state = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(strm->state, "deflateEnd");
    __coverity_writeall__(strm);
    return 0;
}

int inflateCopy(z_stream *dest, z_stream *source) {
    dest->state = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(dest->state, "inflateEnd");
    __coverity_writeall__(dest);
    return 0;
}

int deflateCopy(z_stream *dest, z_stream *source) {
    dest->state = __coverity_alloc__(10);
    __coverity_mark_as_afm_allocated__(dest->state, "deflateEnd");
    __coverity_writeall__(dest);
    return 0;
}

int deflateEnd(z_stream *strm) {
    __coverity_free__(strm->state);
    __coverity_mark_as_afm_freed__(strm->state, "deflateEnd");
    return 0;
}

int inflateEnd(z_stream *strm) {
    __coverity_free__(strm->state);
    __coverity_mark_as_afm_freed__(strm->state, "inflateEnd");
    return 0;
}
