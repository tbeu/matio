INTEGER,PARAMETER :: mat_ptr = 4
INTEGER,PARAMETER :: mat_int = 4
INTEGER(mat_ptr),POINTER :: nullptr => null()

TYPE mat_t
SEQUENCE
    CHARACTER(LEN=128)  :: header
    INTEGER(mat_ptr)    :: mat_t_c_ptr = 0
END TYPE mat_t
TYPE matvar_t
SEQUENCE
    INTEGER                       :: nbytes
    INTEGER                       :: rank
    INTEGER                       :: data_type
    INTEGER                       :: data_size
    INTEGER                       :: class_type
    INTEGER                       :: isComplex
    INTEGER                       :: isGlobal
    INTEGER                       :: isLogical
    INTEGER,DIMENSION(7)          :: dims = (/0,0,0,0,0,0,0/)
    CHARACTER(LEN=64)             :: name
    INTEGER(mat_ptr)              :: matvar_t_c_ptr = 0
END TYPE matvar_t

    INTEGER,PARAMETER :: MAT_ACC_RDONLY   = 1
    INTEGER,PARAMETER :: MAT_ACC_RDWR     = 2

    INTEGER,PARAMETER :: MAT_T_UNKNOWN    =  0
    INTEGER,PARAMETER :: MAT_T_INT8       =  1
    INTEGER,PARAMETER :: MAT_T_UINT8      =  2
    INTEGER,PARAMETER :: MAT_T_INT16      =  3
    INTEGER,PARAMETER :: MAT_T_UINT16     =  4
    INTEGER,PARAMETER :: MAT_T_INT32      =  5
    INTEGER,PARAMETER :: MAT_T_UINT32     =  6
    INTEGER,PARAMETER :: MAT_T_SINGLE     =  7
    INTEGER,PARAMETER :: MAT_T_DOUBLE     =  9
    INTEGER,PARAMETER :: MAT_T_INT64      = 12
    INTEGER,PARAMETER :: MAT_T_UINT64     = 13
    INTEGER,PARAMETER :: MAT_T_MATRIX     = 14
    INTEGER,PARAMETER :: MAT_T_COMPRESSED = 15
    INTEGER,PARAMETER :: MAT_T_UTF8       = 16
    INTEGER,PARAMETER :: MAT_T_UTF16      = 17
    INTEGER,PARAMETER :: MAT_T_UTF32      = 18

    INTEGER,PARAMETER :: MAT_C_CELL       =  1
    INTEGER,PARAMETER :: MAT_C_STRUCT     =  2
    INTEGER,PARAMETER :: MAT_C_OBJECT     =  3
    INTEGER,PARAMETER :: MAT_C_CHAR       =  4
    INTEGER,PARAMETER :: MAT_C_SPARSE     =  5
    INTEGER,PARAMETER :: MAT_C_DOUBLE     =  6
    INTEGER,PARAMETER :: MAT_C_SINGLE     =  7
    INTEGER,PARAMETER :: MAT_C_INT8       =  8
    INTEGER,PARAMETER :: MAT_C_UINT8      =  9
    INTEGER,PARAMETER :: MAT_C_INT16      = 10
    INTEGER,PARAMETER :: MAT_C_UINT16     = 11
    INTEGER,PARAMETER :: MAT_C_INT32      = 12
    INTEGER,PARAMETER :: MAT_C_UINT32     = 13
    INTEGER,PARAMETER :: MAT_C_INT64      = 14
    INTEGER,PARAMETER :: MAT_C_UINT64     = 15
    INTEGER,PARAMETER :: MAT_C_FUNCTION   = 16

    INTEGER,PARAMETER :: COMPRESSION_NONE = 0
    INTEGER,PARAMETER :: COMPRESSION_ZLIB = 1
