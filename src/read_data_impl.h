/*
 * Copyright (c) 2015-2026, The matio contributors
 * Copyright (c) 2019-2014, Christopher C. Hulbert
 * All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#define READ_TYPE_DOUBLE_DATA CAT(READ_TYPED_FUNC1, Double)
#define READ_TYPE_SINGLE_DATA CAT(READ_TYPED_FUNC1, Single)
#define READ_TYPE_INT32_DATA CAT(READ_TYPED_FUNC1, Int32)
#define READ_TYPE_UINT32_DATA CAT(READ_TYPED_FUNC1, UInt32)
#define READ_TYPE_INT16_DATA CAT(READ_TYPED_FUNC1, Int16)
#define READ_TYPE_UINT16_DATA CAT(READ_TYPED_FUNC1, UInt16)
#define READ_TYPE_INT8_DATA CAT(READ_TYPED_FUNC1, Int8)
#define READ_TYPE_UINT8_DATA CAT(READ_TYPED_FUNC1, UInt8)
#ifdef HAVE_MAT_INT64_T
#define READ_TYPE_INT64_DATA CAT(READ_TYPED_FUNC1, Int64)
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
#define READ_TYPE_UINT64_DATA CAT(READ_TYPED_FUNC1, UInt64)
#endif /* HAVE_MAT_UINT64_T */

// Converts value from its own type to READ_TYPE, carefully ensuring the value fits, otherwise sets success to false.
static READ_TYPE
CAT(ConvertFromInt8To, READ_TYPE)(int8_t value, int *success)
{
    // 8 bit can convert losslessly to anything.
    *success = 1;
    return (READ_TYPE)value;
}

static READ_TYPE
CAT(ConvertFromUInt8To, READ_TYPE)(uint8_t value, int *success)
{
    // 8 bit can convert losslessly to anything.
    *success = 1;
    return (READ_TYPE)value;
}

static READ_TYPE
CAT(ConvertFromInt16To, READ_TYPE)(int16_t value, int *success)
{
    // 16 bit can convert losslessly to anything except maybe smaller sized integers.
#if READ_TYPE_TYPE == READ_TYPE_INT8
    if ( (value < INT8_MIN) || (value > INT8_MAX) ) {
        *success = 0;
        return 0;
    }
#elif READ_TYPE_TYPE == READ_TYPE_UINT8
    if ( (value < 0) || (value > UINT8_MAX) ) {
        *success = 0;
        return 0;
    }
#endif

    *success = 1;
    return (READ_TYPE)value;
}

static READ_TYPE
CAT(ConvertFromUInt16To, READ_TYPE)(uint16_t value, int *success)
{
    // 16 bit can convert losslessly to anything except maybe smaller sized integers.
#if READ_TYPE_TYPE == READ_TYPE_INT8
    if ( value > INT8_MAX ) {
        *success = 0;
        return 0;
    }
#elif READ_TYPE_TYPE == READ_TYPE_UINT8
    if ( value > UINT8_MAX ) {
        *success = 0;
        return 0;
    }
#endif

    *success = 1;
    return (READ_TYPE)value;
}

static READ_TYPE
CAT(ConvertFromInt32To, READ_TYPE)(int32_t value, int *success)
{
    // 32 bit can convert losslessly to anything except maybe smaller sized integers and to float which can be lossy but at least is well-defined.
#if READ_TYPE_TYPE == READ_TYPE_INT16
    if ( (value < INT16_MIN) || (value > INT16_MAX) ) {
        *success = 0;
        return 0;
    }
#elif READ_TYPE_TYPE == READ_TYPE_UINT16
    if ( (value < 0) || (value > UINT16_MAX) ) {
        *success = 0;
        return 0;
    }
#elif READ_TYPE_TYPE == READ_TYPE_INT8
    if ( (value < INT8_MIN) || (value > INT8_MAX) ) {
        *success = 0;
        return 0;
    }
#elif READ_TYPE_TYPE == READ_TYPE_UINT8
    if ( (value < 0) || (value > UINT8_MAX) ) {
        *success = 0;
        return 0;
    }
#endif

    *success = 1;
    return (READ_TYPE)value;
}

static READ_TYPE
CAT(ConvertFromUInt32To, READ_TYPE)(uint32_t value, int *success)
{
    // 32 bit can convert losslessly to anything except maybe smaller sized integers and to float which can be lossy but at least is well-defined. double can losslessly represent all int32.
#if READ_TYPE_TYPE == READ_TYPE_INT16
    if ( (value < INT16_MIN) || (value > INT16_MAX) ) {
        *success = 0;
        return 0;
    }
#elif READ_TYPE_TYPE == READ_TYPE_UINT16
    if ( (value < 0) || (value > UINT16_MAX) ) {
        *success = 0;
        return 0;
    }
#elif READ_TYPE_TYPE == READ_TYPE_INT8
    if ( (value < INT8_MIN) || (value > INT8_MAX) ) {
        *success = 0;
        return 0;
    }
#elif READ_TYPE_TYPE == READ_TYPE_UINT8
    if ( (value < 0) || (value > UINT8_MAX) ) {
        *success = 0;
        return 0;
    }
#endif

    *success = 1;
    return (READ_TYPE)value;
}

static READ_TYPE
CAT(ConvertFromInt64To, READ_TYPE)(int64_t value, int *success)
{
    // 64 bit can convert losslessly to anything except maybe smaller sized integers and to float/double which can be lossy but at least is well-defined.
#if READ_TYPE_TYPE == READ_TYPE_INT32
    if ( (value < INT32_MIN) || (value > INT32_MAX) ) {
        *success = 0;
        return 0;
    }
#elif READ_TYPE_TYPE == READ_TYPE_UINT32
    if ( (value < 0) || (value > UINT32_MAX) ) {
        *success = 0;
        return 0;
    }
#elif READ_TYPE_TYPE == READ_TYPE_INT16
    if ( (value < INT16_MIN) || (value > INT16_MAX) ) {
        *success = 0;
        return 0;
    }
#elif READ_TYPE_TYPE == READ_TYPE_UINT16
    if ( (value < 0) || (value > UINT16_MAX) ) {
        *success = 0;
        return 0;
    }
#elif READ_TYPE_TYPE == READ_TYPE_INT8
    if ( (value < INT8_MIN) || (value > INT8_MAX) ) {
        *success = 0;
        return 0;
    }
#elif READ_TYPE_TYPE == READ_TYPE_UINT8
    if ( (value < 0) || (value > UINT8_MAX) ) {
        *success = 0;
        return 0;
    }
#endif

    *success = 1;
    return (READ_TYPE)value;
}

static READ_TYPE
CAT(ConvertFromUInt64To, READ_TYPE)(uint64_t value, int *success)
{
    // 64 bit can convert losslessly to anything except maybe smaller sized integers and to float/double which can be lossy but at least is well-defined.
#if READ_TYPE_TYPE == READ_TYPE_INT32
    if ( (value < INT32_MIN) || (value > INT32_MAX) ) {
        *success = 0;
        return 0;
    }
#elif READ_TYPE_TYPE == READ_TYPE_UINT32
    if ( (value < 0) || (value > UINT32_MAX) ) {
        *success = 0;
        return 0;
    }
#elif READ_TYPE_TYPE == READ_TYPE_INT16
    if ( (value < INT16_MIN) || (value > INT16_MAX) ) {
        *success = 0;
        return 0;
    }
#elif READ_TYPE_TYPE == READ_TYPE_UINT16
    if ( (value < 0) || (value > UINT16_MAX) ) {
        *success = 0;
        return 0;
    }
#elif READ_TYPE_TYPE == READ_TYPE_INT8
    if ( (value < INT8_MIN) || (value > INT8_MAX) ) {
        *success = 0;
        return 0;
    }
#elif READ_TYPE_TYPE == READ_TYPE_UINT8
    if ( (value < 0) || (value > UINT8_MAX) ) {
        *success = 0;
        return 0;
    }
#endif

    *success = 1;
    return (READ_TYPE)value;
}

static READ_TYPE
CAT(ConvertFromFloatTo, READ_TYPE)(float value, int *success)
{
    *success = 1;
    return (READ_TYPE)value;
}

static READ_TYPE
CAT(ConvertFromDoubleTo, READ_TYPE)(double value, int *success)
{
    *success = 1;
    return (READ_TYPE)value;
}

static int
READ_TYPE_DOUBLE_DATA(mat_t *mat, READ_TYPE *data, size_t len)
{
    size_t readCount;
    int err;
#if READ_TYPE_TYPE == READ_TYPE_DOUBLE
    readCount = fread(data, sizeof(double), len, (FILE *)mat->fp);
    if ( readCount != len ) {
        return MATIO_E_GENERIC_READ_ERROR;
    }
    err = MATIO_E_NO_ERROR;
    if ( mat->byteswap ) {
        size_t i;
        for ( i = 0; i < len; i++ ) {
            (void)Mat_doubleSwap(data + i);
        }
    }
#else
    size_t i;
    const size_t data_size = sizeof(double);
    double v[READ_BLOCK_SIZE / sizeof(double)];
    READ_DATA(READ_TYPE, double, Mat_doubleSwap, CAT(ConvertFromDoubleTo, READ_TYPE));
#endif
    return err;
}

static int
READ_TYPE_SINGLE_DATA(mat_t *mat, READ_TYPE *data, size_t len)
{
    size_t readCount;
    int err;
#if READ_TYPE_TYPE == READ_TYPE_SINGLE
    readCount = fread(data, sizeof(float), len, (FILE *)mat->fp);
    if ( readCount != len ) {
        return MATIO_E_GENERIC_READ_ERROR;
    }
    err = MATIO_E_NO_ERROR;
    if ( mat->byteswap ) {
        size_t i;
        for ( i = 0; i < len; i++ ) {
            (void)Mat_floatSwap(data + i);
        }
    }
#else
    size_t i;
    const size_t data_size = sizeof(float);
    float v[READ_BLOCK_SIZE / sizeof(float)];
    READ_DATA(READ_TYPE, float, Mat_floatSwap, CAT(ConvertFromFloatTo, READ_TYPE));
#endif
    return err;
}

static int
READ_TYPE_INT32_DATA(mat_t *mat, READ_TYPE *data, size_t len)
{
    size_t readCount;
    int err;
#if READ_TYPE_TYPE == READ_TYPE_INT32
    readCount = fread(data, sizeof(mat_int32_t), len, (FILE *)mat->fp);
    if ( readCount != len ) {
        return MATIO_E_GENERIC_READ_ERROR;
    }
    err = MATIO_E_NO_ERROR;
    if ( mat->byteswap ) {
        size_t i;
        for ( i = 0; i < len; i++ ) {
            (void)Mat_int32Swap(data + i);
        }
    }
#else
    size_t i;
    const size_t data_size = sizeof(mat_int32_t);
    mat_int32_t v[READ_BLOCK_SIZE / sizeof(mat_int32_t)];
    READ_DATA(READ_TYPE, mat_int32_t, Mat_int32Swap, CAT(ConvertFromInt32To, READ_TYPE));
#endif
    return err;
}

static int
READ_TYPE_UINT32_DATA(mat_t *mat, READ_TYPE *data, size_t len)
{
    size_t readCount;
    int err;
#if READ_TYPE_TYPE == READ_TYPE_UINT32
    readCount = fread(data, sizeof(mat_uint32_t), len, (FILE *)mat->fp);
    if ( readCount != len ) {
        return MATIO_E_GENERIC_READ_ERROR;
    }
    err = MATIO_E_NO_ERROR;
    if ( mat->byteswap ) {
        size_t i;
        for ( i = 0; i < len; i++ ) {
            (void)Mat_uint32Swap(data + i);
        }
    }
#else
    size_t i;
    const size_t data_size = sizeof(mat_uint32_t);
    mat_uint32_t v[READ_BLOCK_SIZE / sizeof(mat_uint32_t)];
    READ_DATA(READ_TYPE, mat_uint32_t, Mat_uint32Swap, CAT(ConvertFromUInt32To, READ_TYPE));
#endif
    return err;
}

static int
READ_TYPE_INT16_DATA(mat_t *mat, READ_TYPE *data, size_t len)
{
    size_t readCount;
    int err;
#if READ_TYPE_TYPE == READ_TYPE_INT16
    readCount = fread(data, sizeof(mat_int16_t), len, (FILE *)mat->fp);
    if ( readCount != len ) {
        return MATIO_E_GENERIC_READ_ERROR;
    }
    err = MATIO_E_NO_ERROR;
    if ( mat->byteswap ) {
        size_t i;
        for ( i = 0; i < len; i++ ) {
            (void)Mat_int16Swap(data + i);
        }
    }
#else
    size_t i;
    const size_t data_size = sizeof(mat_int16_t);
    mat_int16_t v[READ_BLOCK_SIZE / sizeof(mat_int16_t)];
    READ_DATA(READ_TYPE, mat_int16_t, Mat_int16Swap, CAT(ConvertFromInt16To, READ_TYPE));
#endif
    return err;
}

static int
READ_TYPE_UINT16_DATA(mat_t *mat, READ_TYPE *data, size_t len)
{
    size_t readCount;
    int err;
#if READ_TYPE_TYPE == READ_TYPE_UINT16
    readCount = fread(data, sizeof(mat_uint16_t), len, (FILE *)mat->fp);
    if ( readCount != len ) {
        return MATIO_E_GENERIC_READ_ERROR;
    }
    err = MATIO_E_NO_ERROR;
    if ( mat->byteswap ) {
        size_t i;
        for ( i = 0; i < len; i++ ) {
            (void)Mat_uint16Swap(data + i);
        }
    }
#else
    size_t i;
    const size_t data_size = sizeof(mat_uint16_t);
    mat_uint16_t v[READ_BLOCK_SIZE / sizeof(mat_uint16_t)];
    READ_DATA(READ_TYPE, mat_uint16_t, Mat_uint16Swap, CAT(ConvertFromUInt16To, READ_TYPE));
#endif
    return err;
}

static int
READ_TYPE_INT8_DATA(mat_t *mat, READ_TYPE *data, size_t len)
{
    size_t readCount;
    int err;
#if READ_TYPE_TYPE == READ_TYPE_INT8
    readCount = fread(data, sizeof(mat_int8_t), len, (FILE *)mat->fp);
    if ( readCount != len ) {
        return MATIO_E_GENERIC_READ_ERROR;
    }
    err = MATIO_E_NO_ERROR;
#else
    size_t i;
    const size_t data_size = sizeof(mat_int8_t);
    mat_int8_t v[READ_BLOCK_SIZE / sizeof(mat_int8_t)];
    READ_DATA_NOSWAP(READ_TYPE, mat_int8_t, CAT(ConvertFromInt8To, READ_TYPE));
#endif
    return err;
}

static int
READ_TYPE_UINT8_DATA(mat_t *mat, READ_TYPE *data, size_t len)
{
    size_t readCount;
    int err;
#if READ_TYPE_TYPE == READ_TYPE_UINT8
    readCount = fread(data, sizeof(mat_uint8_t), len, (FILE *)mat->fp);
    if ( readCount != len ) {
        return MATIO_E_GENERIC_READ_ERROR;
    }
    err = MATIO_E_NO_ERROR;
#else
    size_t i;
    const size_t data_size = sizeof(mat_uint8_t);
    mat_uint8_t v[READ_BLOCK_SIZE / sizeof(mat_uint8_t)];
    READ_DATA_NOSWAP(READ_TYPE, mat_uint8_t, CAT(ConvertFromUInt8To, READ_TYPE));
#endif
    return err;
}

#ifdef HAVE_MAT_INT64_T
static int
READ_TYPE_INT64_DATA(mat_t *mat, READ_TYPE *data, size_t len)
{
    size_t readCount;
    int err;
#if READ_TYPE_TYPE == READ_TYPE_INT64
    readCount = fread(data, sizeof(mat_int64_t), len, (FILE *)mat->fp);
    if ( readCount != len ) {
        return MATIO_E_GENERIC_READ_ERROR;
    }
    err = MATIO_E_NO_ERROR;
    if ( mat->byteswap ) {
        size_t i;
        for ( i = 0; i < len; i++ ) {
            (void)Mat_int64Swap(data + i);
        }
    }
#else
    size_t i;
    const size_t data_size = sizeof(mat_int64_t);
    mat_int64_t v[READ_BLOCK_SIZE / sizeof(mat_int64_t)];
    READ_DATA(READ_TYPE, mat_int64_t, Mat_int64Swap, CAT(ConvertFromInt64To, READ_TYPE));
#endif
    return err;
}
#endif /* HAVE_MAT_INT64_T */

#ifdef HAVE_MAT_UINT64_T
static int
READ_TYPE_UINT64_DATA(mat_t *mat, READ_TYPE *data, size_t len)
{
    size_t readCount;
    int err;
#if READ_TYPE_TYPE == READ_TYPE_UINT64
    readCount = fread(data, sizeof(mat_uint64_t), len, (FILE *)mat->fp);
    if ( readCount != len ) {
        return MATIO_E_GENERIC_READ_ERROR;
    }
    err = MATIO_E_NO_ERROR;
    if ( mat->byteswap ) {
        size_t i;
        for ( i = 0; i < len; i++ ) {
            (void)Mat_uint64Swap(data + i);
        }
    }
#else
    size_t i;
    const size_t data_size = sizeof(mat_uint64_t);
    mat_uint64_t v[READ_BLOCK_SIZE / sizeof(mat_uint64_t)];
    READ_DATA(READ_TYPE, mat_uint64_t, Mat_uint64Swap, CAT(ConvertFromUInt64To, READ_TYPE));
#endif
    return err;
}
#endif /* HAVE_MAT_UINT64_T */

/** @brief Reads data of type @c data_type into a READ_TYPE type
 *
 * Reads from the MAT file @c len elements of data type @c data_type storing
 * them as READ_TYPE's in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param data Pointer to store the output values (len*sizeof(READ_TYPE))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval 0 on success
 */
int
READ_TYPED_FUNC1(mat_t *mat, READ_TYPE *data, enum matio_types data_type, size_t len)
{
    if ( mat == NULL || data == NULL || mat->fp == NULL )
        return MATIO_E_BAD_ARGUMENT;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
            return READ_TYPE_DOUBLE_DATA(mat, data, len);
        case MAT_T_SINGLE:
            return READ_TYPE_SINGLE_DATA(mat, data, len);
#ifdef HAVE_MAT_INT64_T
        case MAT_T_INT64:
            return READ_TYPE_INT64_DATA(mat, data, len);
#endif /* HAVE_MAT_UINT64_T */
#ifdef HAVE_MAT_UINT64_T
        case MAT_T_UINT64:
            return READ_TYPE_UINT64_DATA(mat, data, len);
#endif /* HAVE_MAT_UINT64_T */
        case MAT_T_INT32:
            return READ_TYPE_INT32_DATA(mat, data, len);
        case MAT_T_UINT32:
            return READ_TYPE_UINT32_DATA(mat, data, len);
        case MAT_T_INT16:
            return READ_TYPE_INT16_DATA(mat, data, len);
        case MAT_T_UINT16:
            return READ_TYPE_UINT16_DATA(mat, data, len);
        case MAT_T_INT8:
            return READ_TYPE_INT8_DATA(mat, data, len);
        case MAT_T_UINT8:
            return READ_TYPE_UINT8_DATA(mat, data, len);
        default:
            break;
    }
    return MATIO_E_GENERIC_READ_ERROR;
}

#undef READ_TYPE_DOUBLE_DATA
#undef READ_TYPE_SINGLE_DATA
#undef READ_TYPE_INT32_DATA
#undef READ_TYPE_UINT32_DATA
#undef READ_TYPE_INT16_DATA
#undef READ_TYPE_UINT16_DATA
#undef READ_TYPE_INT8_DATA
#undef READ_TYPE_UINT8_DATA
#ifdef HAVE_MAT_INT64_T
#undef READ_TYPE_INT64_DATA
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
#undef READ_TYPE_UINT64_DATA
#endif /* HAVE_MAT_UINT64_T */

#if HAVE_ZLIB

#define READ_TYPE_DOUBLE_DATA CAT(READ_TYPED_FUNC2, Double)
#define READ_TYPE_SINGLE_DATA CAT(READ_TYPED_FUNC2, Single)
#define READ_TYPE_INT32_DATA CAT(READ_TYPED_FUNC2, Int32)
#define READ_TYPE_UINT32_DATA CAT(READ_TYPED_FUNC2, UInt32)
#define READ_TYPE_INT16_DATA CAT(READ_TYPED_FUNC2, Int16)
#define READ_TYPE_UINT16_DATA CAT(READ_TYPED_FUNC2, UInt16)
#define READ_TYPE_INT8_DATA CAT(READ_TYPED_FUNC2, Int8)
#define READ_TYPE_UINT8_DATA CAT(READ_TYPED_FUNC2, UInt8)
#ifdef HAVE_MAT_INT64_T
#define READ_TYPE_INT64_DATA CAT(READ_TYPED_FUNC2, Int64)
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
#define READ_TYPE_UINT64_DATA CAT(READ_TYPED_FUNC2, UInt64)
#endif /* HAVE_MAT_UINT64_T */

static int
READ_TYPE_DOUBLE_DATA(mat_t *mat, z_streamp z, READ_TYPE *data, mat_uint32_t len)
{
    int err;
#if READ_TYPE_TYPE == READ_TYPE_DOUBLE
    err = InflateData(mat, z, data, len * sizeof(double));
    if ( err ) {
        return err;
    }
    if ( mat->byteswap ) {
        mat_uint32_t i;
        for ( i = 0; i < len; i++ ) {
            (void)Mat_doubleSwap(data + i);
        }
    }
#else
    mat_uint32_t i;
    const size_t data_size = sizeof(double);
    double v[READ_BLOCK_SIZE / sizeof(double)];
    READ_COMPRESSED_DATA(READ_TYPE, Mat_doubleSwap);
#endif
    return err;
}

static int
READ_TYPE_SINGLE_DATA(mat_t *mat, z_streamp z, READ_TYPE *data, mat_uint32_t len)
{
    int err;
#if READ_TYPE_TYPE == READ_TYPE_SINGLE
    err = InflateData(mat, z, data, len * sizeof(float));
    if ( err ) {
        return err;
    }
    if ( mat->byteswap ) {
        mat_uint32_t i;
        for ( i = 0; i < len; i++ ) {
            (void)Mat_floatSwap(data + i);
        }
    }
#else
    mat_uint32_t i;
    const size_t data_size = sizeof(float);
    float v[READ_BLOCK_SIZE / sizeof(float)];
    READ_COMPRESSED_DATA(READ_TYPE, Mat_floatSwap);
#endif
    return err;
}

#ifdef HAVE_MAT_INT64_T
static int
READ_TYPE_INT64_DATA(mat_t *mat, z_streamp z, READ_TYPE *data, mat_uint32_t len)
{
    int err;
#if READ_TYPE_TYPE == READ_TYPE_INT64
    err = InflateData(mat, z, data, len * sizeof(mat_int64_t));
    if ( err ) {
        return err;
    }
    if ( mat->byteswap ) {
        mat_uint32_t i;
        for ( i = 0; i < len; i++ ) {
            (void)Mat_int64Swap(data + i);
        }
    }
#else
    mat_uint32_t i;
    const size_t data_size = sizeof(mat_int64_t);
    mat_int64_t v[READ_BLOCK_SIZE / sizeof(mat_int64_t)];
    READ_COMPRESSED_DATA(READ_TYPE, Mat_int64Swap);
#endif
    return err;
}
#endif /* HAVE_MAT_INT64_T */

#ifdef HAVE_MAT_UINT64_T
static int
READ_TYPE_UINT64_DATA(mat_t *mat, z_streamp z, READ_TYPE *data, mat_uint32_t len)
{
    int err;
#if READ_TYPE_TYPE == READ_TYPE_UINT64
    err = InflateData(mat, z, data, len * sizeof(mat_uint64_t));
    if ( err ) {
        return err;
    }
    if ( mat->byteswap ) {
        mat_uint32_t i;
        for ( i = 0; i < len; i++ ) {
            (void)Mat_uint64Swap(data + i);
        }
    }
#else
    mat_uint32_t i;
    const size_t data_size = sizeof(mat_uint64_t);
    mat_uint64_t v[READ_BLOCK_SIZE / sizeof(mat_uint64_t)];
    READ_COMPRESSED_DATA(READ_TYPE, Mat_uint64Swap);
#endif
    return err;
}
#endif /* HAVE_MAT_UINT64_T */

static int
READ_TYPE_INT32_DATA(mat_t *mat, z_streamp z, READ_TYPE *data, mat_uint32_t len)
{
    int err;
#if READ_TYPE_TYPE == READ_TYPE_INT32
    err = InflateData(mat, z, data, len * sizeof(mat_int32_t));
    if ( err ) {
        return err;
    }
    if ( mat->byteswap ) {
        mat_uint32_t i;
        for ( i = 0; i < len; i++ ) {
            (void)Mat_int32Swap(data + i);
        }
    }
#else
    mat_uint32_t i;
    const size_t data_size = sizeof(mat_int32_t);
    mat_int32_t v[READ_BLOCK_SIZE / sizeof(mat_int32_t)];
    READ_COMPRESSED_DATA(READ_TYPE, Mat_int32Swap);
#endif
    return err;
}

static int
READ_TYPE_UINT32_DATA(mat_t *mat, z_streamp z, READ_TYPE *data, mat_uint32_t len)
{
    int err;
#if READ_TYPE_TYPE == READ_TYPE_UINT32
    err = InflateData(mat, z, data, len * sizeof(mat_uint32_t));
    if ( err ) {
        return err;
    }
    if ( mat->byteswap ) {
        mat_uint32_t i;
        for ( i = 0; i < len; i++ ) {
            (void)Mat_uint32Swap(data + i);
        }
    }
#else
    mat_uint32_t i;
    const size_t data_size = sizeof(mat_uint32_t);
    mat_uint32_t v[READ_BLOCK_SIZE / sizeof(mat_uint32_t)];
    READ_COMPRESSED_DATA(READ_TYPE, Mat_uint32Swap);
#endif
    return err;
}

static int
READ_TYPE_INT16_DATA(mat_t *mat, z_streamp z, READ_TYPE *data, mat_uint32_t len)
{
    int err;
#if READ_TYPE_TYPE == READ_TYPE_INT16
    err = InflateData(mat, z, data, len * sizeof(mat_int16_t));
    if ( err ) {
        return err;
    }
    if ( mat->byteswap ) {
        mat_uint32_t i;
        for ( i = 0; i < len; i++ ) {
            (void)Mat_int16Swap(data + i);
        }
    }
#else
    mat_uint32_t i;
    const size_t data_size = sizeof(mat_int16_t);
    mat_int16_t v[READ_BLOCK_SIZE / sizeof(mat_int16_t)];
    READ_COMPRESSED_DATA(READ_TYPE, Mat_int16Swap);
#endif
    return err;
}

static int
READ_TYPE_UINT16_DATA(mat_t *mat, z_streamp z, READ_TYPE *data, mat_uint32_t len)
{
    int err;
#if READ_TYPE_TYPE == READ_TYPE_UINT16
    err = InflateData(mat, z, data, len * sizeof(mat_uint16_t));
    if ( err ) {
        return err;
    }
    if ( mat->byteswap ) {
        mat_uint32_t i;
        for ( i = 0; i < len; i++ ) {
            (void)Mat_uint16Swap(data + i);
        }
    }
#else
    mat_uint32_t i;
    const size_t data_size = sizeof(mat_uint16_t);
    mat_uint16_t v[READ_BLOCK_SIZE / sizeof(mat_uint16_t)];
    READ_COMPRESSED_DATA(READ_TYPE, Mat_uint16Swap);
#endif
    return err;
}

static int
READ_TYPE_INT8_DATA(mat_t *mat, z_streamp z, READ_TYPE *data, mat_uint32_t len)
{
    int err;
#if READ_TYPE_TYPE == READ_TYPE_INT8
    err = InflateData(mat, z, data, len * sizeof(mat_int8_t));
#else
    mat_uint32_t i;
    const size_t data_size = sizeof(mat_int8_t);
    mat_int8_t v[READ_BLOCK_SIZE / sizeof(mat_int8_t)];
    READ_COMPRESSED_DATA_NOSWAP(READ_TYPE);
#endif
    return err;
}

static int
READ_TYPE_UINT8_DATA(mat_t *mat, z_streamp z, READ_TYPE *data, mat_uint32_t len)
{
    int err;
#if READ_TYPE_TYPE == READ_TYPE_UINT8
    err = InflateData(mat, z, data, len * sizeof(mat_uint8_t));
#else
    mat_uint32_t i;
    const size_t data_size = sizeof(mat_uint8_t);
    mat_uint8_t v[READ_BLOCK_SIZE / sizeof(mat_uint8_t)];
    READ_COMPRESSED_DATA_NOSWAP(READ_TYPE);
#endif
    return err;
}

/** @brief Reads data of type @c data_type into a READ_TYPE type
 *
 * Reads from the MAT file @c len compressed elements of data type @c data_type
 * storing them as READ_TYPE's in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param z Pointer to the zlib stream for inflation
 * @param data Pointer to store the output values (len*sizeof(READ_TYPE))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval 0 on success
 */
int
READ_TYPED_FUNC2(mat_t *mat, z_streamp z, READ_TYPE *data, enum matio_types data_type, int len)
{
    if ( mat == NULL || data == NULL || mat->fp == NULL )
        return MATIO_E_BAD_ARGUMENT;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
            return READ_TYPE_DOUBLE_DATA(mat, z, data, len);
        case MAT_T_SINGLE:
            return READ_TYPE_SINGLE_DATA(mat, z, data, len);
#ifdef HAVE_MAT_INT64_T
        case MAT_T_INT64:
            return READ_TYPE_INT64_DATA(mat, z, data, len);
#endif /* HAVE_MAT_UINT64_T */
#ifdef HAVE_MAT_UINT64_T
        case MAT_T_UINT64:
            return READ_TYPE_UINT64_DATA(mat, z, data, len);
#endif /* HAVE_MAT_UINT64_T */
        case MAT_T_INT32:
            return READ_TYPE_INT32_DATA(mat, z, data, len);
        case MAT_T_UINT32:
            return READ_TYPE_UINT32_DATA(mat, z, data, len);
        case MAT_T_INT16:
            return READ_TYPE_INT16_DATA(mat, z, data, len);
        case MAT_T_UINT16:
            return READ_TYPE_UINT16_DATA(mat, z, data, len);
        case MAT_T_INT8:
            return READ_TYPE_INT8_DATA(mat, z, data, len);
        case MAT_T_UINT8:
            return READ_TYPE_UINT8_DATA(mat, z, data, len);
        default:
            break;
    }
    return MATIO_E_GENERIC_READ_ERROR;
}

#undef READ_TYPE_DOUBLE_DATA
#undef READ_TYPE_SINGLE_DATA
#undef READ_TYPE_INT32_DATA
#undef READ_TYPE_UINT32_DATA
#undef READ_TYPE_INT16_DATA
#undef READ_TYPE_UINT16_DATA
#undef READ_TYPE_INT8_DATA
#undef READ_TYPE_UINT8_DATA
#ifdef HAVE_MAT_INT64_T
#undef READ_TYPE_INT64_DATA
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
#undef READ_TYPE_UINT64_DATA
#endif /* HAVE_MAT_UINT64_T */

#endif
