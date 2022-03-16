!
! Copyright (c) 2015-2022, The matio contributors
! Copyright (c) 2005-2014, Christopher C. Hulbert
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! 1. Redistributions of source code must retain the above copyright notice, this
!    list of conditions and the following disclaimer.
!
! 2. Redistributions in binary form must reproduce the above copyright notice,
!    this list of conditions and the following disclaimer in the documentation
!    and/or other materials provided with the distribution.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
FUNCTION FMat_VarCreateA(rank,dims,name,class_type,data_type,matvar) RESULT(err)
    INTEGER(4)                         :: err
    INTEGER(4),INTENT(IN)              :: rank
    INTEGER(4),INTENT(IN)              :: class_type
    INTEGER(4),INTENT(IN)              :: data_type
    INTEGER(4),INTENT(IN),DIMENSION(*) :: dims
    CHARACTER(LEN=*) :: name
    TYPE(matvar_t),INTENT(INOUT)        :: matvar

    err = fmat_varcreate_c(rank,dims,name,class_type,matvar%data_type,matvar)
END FUNCTION

FUNCTION FMat_VarCreateDouble0(varname,double_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    DOUBLE PRECISION                :: double_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_DOUBLE
    matvar%data_type  = MAT_T_DOUBLE
    matvar%rank       = 2
    matvar%dims(1)    = 1
    matvar%dims(2)    = 1
    matvar%isComplex  = 0
    matvar%isGlobal   = 0
    matvar%isLogical  = 0
    matvar%name       = varname
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,          &
                           matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateDouble0

FUNCTION FMat_VarCreateDouble1(varname,double_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    DOUBLE PRECISION,DIMENSION(:)   :: double_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_DOUBLE
    matvar%data_type  = MAT_T_DOUBLE
    matvar%rank       = 2
    matvar%dims(1)    = SIZE(double_data)
    matvar%dims(2)    = 1
    matvar%isComplex  = 0
    matvar%isGlobal   = 0
    matvar%isLogical  = 0
    matvar%name       = varname
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateDouble1

FUNCTION FMat_VarCreateDouble2(varname,double_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    DOUBLE PRECISION,DIMENSION(:,:) :: double_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_DOUBLE
    matvar%data_type  = MAT_T_DOUBLE
    matvar%rank       = 2
    matvar%dims(1)    = SIZE(double_data,1)
    matvar%dims(2)    = SIZE(double_data,2)
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateDouble2

FUNCTION FMat_VarCreateDouble3(varname,double_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    DOUBLE PRECISION,DIMENSION(:,:,:) :: double_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_DOUBLE
    matvar%data_type  = MAT_T_DOUBLE
    matvar%rank       = 3
    matvar%dims(1)    = SIZE(double_data,1)
    matvar%dims(2)    = SIZE(double_data,2)
    matvar%dims(3)    = SIZE(double_data,3)
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateDouble3

FUNCTION FMat_VarCreateComplexDouble0(varname,double_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    DOUBLE COMPLEX                  :: double_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_DOUBLE
    matvar%data_type  = MAT_T_DOUBLE
    matvar%rank       = 2
    matvar%dims(1)    = 1
    matvar%dims(2)    = 1
    matvar%isComplex  = 1
    matvar%isGlobal   = 0
    matvar%isLogical  = 0
    matvar%name       = varname
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,          &
                           matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateComplexDouble0

FUNCTION FMat_VarCreateComplexDouble1(varname,double_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    DOUBLE COMPLEX,DIMENSION(:)   :: double_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_DOUBLE
    matvar%data_type  = MAT_T_DOUBLE
    matvar%rank       = 2
    matvar%dims(1)    = SIZE(double_data)
    matvar%dims(2)    = 1
    matvar%isComplex  = 1
    matvar%isGlobal   = 0
    matvar%isLogical  = 0
    matvar%name       = varname
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateComplexDouble1

FUNCTION FMat_VarCreateComplexDouble2(varname,double_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    DOUBLE COMPLEX,DIMENSION(:,:)   :: double_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_DOUBLE
    matvar%data_type  = MAT_T_DOUBLE
    matvar%rank       = 2
    matvar%dims(1)    = SIZE(double_data,1)
    matvar%dims(2)    = SIZE(double_data,2)
    matvar%isComplex  = 1
    matvar%isGlobal   = 0
    matvar%isLogical  = 0
    matvar%name       = varname
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateComplexDouble2

FUNCTION FMat_VarCreateComplexDouble3(varname,double_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    DOUBLE COMPLEX,DIMENSION(:,:,:) :: double_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_DOUBLE
    matvar%data_type  = MAT_T_DOUBLE
    matvar%rank       = 3
    matvar%dims(1)    = SIZE(double_data,1)
    matvar%dims(2)    = SIZE(double_data,2)
    matvar%dims(3)    = SIZE(double_data,3)
    matvar%isComplex  = 1
    matvar%isGlobal   = 0
    matvar%isLogical  = 0
    matvar%name       = varname
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateComplexDouble3

FUNCTION FMat_VarCreateSingle0(varname,single_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    REAL(4)                         :: single_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_SINGLE
    matvar%data_type  = MAT_T_SINGLE
    matvar%rank       = 2
    matvar%dims(1)    = 1
    matvar%dims(2)    = 1
    matvar%isComplex  = 0
    matvar%isGlobal   = 0
    matvar%isLogical  = 0
    matvar%name       = varname
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,          &
                           matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateSingle0

FUNCTION FMat_VarCreateSingle1(varname,single_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    REAL(4),DIMENSION(:)   :: single_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_SINGLE
    matvar%data_type  = MAT_T_SINGLE
    matvar%rank       = 2
    matvar%dims(1)    = SIZE(single_data)
    matvar%dims(2)    = 1
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateSingle1

FUNCTION FMat_VarCreateSingle2(varname,single_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    REAL(4),DIMENSION(:,:) :: single_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_SINGLE
    matvar%data_type  = MAT_T_SINGLE
    matvar%rank       = 2
    matvar%dims(1)    = SIZE(single_data,1)
    matvar%dims(2)    = SIZE(single_data,2)
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateSingle2

FUNCTION FMat_VarCreateSingle3(varname,single_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    REAL(4),DIMENSION(:,:,:) :: single_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_SINGLE
    matvar%data_type  = MAT_T_SINGLE
    matvar%rank       = 3
    matvar%dims(1)    = SIZE(single_data,1)
    matvar%dims(2)    = SIZE(single_data,2)
    matvar%dims(3)    = SIZE(single_data,3)
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateSingle3

FUNCTION FMat_VarCreateComplexSingle0(varname,single_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    COMPLEX(4)                      :: single_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_SINGLE
    matvar%data_type  = MAT_T_SINGLE
    matvar%rank       = 2
    matvar%dims(1)    = 1
    matvar%dims(2)    = 1
    matvar%isComplex  = 1
    matvar%isGlobal   = 0
    matvar%isLogical  = 0
    matvar%name       = varname
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,          &
                           matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateComplexSingle0

FUNCTION FMat_VarCreateComplexSingle1(varname,single_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    COMPLEX(4),DIMENSION(:)    :: single_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_SINGLE
    matvar%data_type  = MAT_T_SINGLE
    matvar%rank       = 2
    matvar%dims(1)    = SIZE(single_data)
    matvar%dims(2)    = 1
    matvar%isComplex  = 1
    matvar%isGlobal   = 0
    matvar%isLogical  = 0
    matvar%name       = varname
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateComplexSingle1

FUNCTION FMat_VarCreateComplexSingle2(varname,single_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    COMPLEX(4),DIMENSION(:,:)   :: single_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_SINGLE
    matvar%data_type  = MAT_T_SINGLE
    matvar%rank       = 2
    matvar%dims(1)    = SIZE(single_data,1)
    matvar%dims(2)    = SIZE(single_data,2)
    matvar%isComplex  = 1
    matvar%isGlobal   = 0
    matvar%isLogical  = 0
    matvar%name       = varname
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateComplexSingle2

FUNCTION FMat_VarCreateComplexSingle3(varname,single_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    COMPLEX(4),DIMENSION(:,:,:)     :: single_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_SINGLE
    matvar%data_type  = MAT_T_SINGLE
    matvar%rank       = 3
    matvar%dims(1)    = SIZE(single_data,1)
    matvar%dims(2)    = SIZE(single_data,2)
    matvar%dims(3)    = SIZE(single_data,3)
    matvar%isComplex  = 1
    matvar%isGlobal   = 0
    matvar%isLogical  = 0
    matvar%name       = varname
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateComplexSingle3

FUNCTION FMat_VarCreateInt32_0(varname,int32_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    INTEGER(4)                      :: int32_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_INT32
    matvar%data_type  = MAT_T_INT32
    matvar%rank       = 2
    matvar%dims(1)    = 1
    matvar%dims(2)    = 1
    matvar%isComplex  = 0
    matvar%isGlobal   = 0
    matvar%isLogical  = 0
    matvar%name       = varname
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,          &
                           matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateInt32_0

FUNCTION FMat_VarCreateInt32_1(varname,int32_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    INTEGER(4),DIMENSION(:)   :: int32_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_INT32
    matvar%data_type  = MAT_T_INT32
    matvar%rank       = 2
    matvar%dims(1)    = SIZE(int32_data)
    matvar%dims(2)    = 1
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateInt32_1

FUNCTION FMat_VarCreateInt32_2(varname,int32_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    INTEGER(4),DIMENSION(:,:) :: int32_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_INT32
    matvar%data_type  = MAT_T_INT32
    matvar%rank       = 2
    matvar%dims(1)    = SIZE(int32_data,1)
    matvar%dims(2)    = SIZE(int32_data,2)

    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateInt32_2

FUNCTION FMat_VarCreateInt32_3(varname,int32_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    INTEGER(4),DIMENSION(:,:,:) :: int32_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_INT32
    matvar%data_type  = MAT_T_INT32
    matvar%rank       = 3
    matvar%dims(1)    = SIZE(int32_data,1)
    matvar%dims(2)    = SIZE(int32_data,2)
    matvar%dims(3)    = SIZE(int32_data,3)
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateInt32_3

FUNCTION FMat_VarCreateInt16_0(varname,int16_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    INTEGER(2)                      :: int16_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_INT16
    matvar%data_type  = MAT_T_INT16
    matvar%rank       = 2
    matvar%dims(1)    = 1
    matvar%dims(2)    = 1
    matvar%isComplex  = 0
    matvar%isGlobal   = 0
    matvar%isLogical  = 0
    matvar%name       = varname
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,          &
                           matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateInt16_0

FUNCTION FMat_VarCreateInt16_1(varname,int16_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    INTEGER(2),DIMENSION(:)   :: int16_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_INT16
    matvar%data_type  = MAT_T_INT16
    matvar%rank       = 2
    matvar%dims(1)    = SIZE(int16_data)
    matvar%dims(2)    = 1
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateInt16_1

FUNCTION FMat_VarCreateInt16_2(varname,int16_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    INTEGER(2),DIMENSION(:,:) :: int16_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_INT16
    matvar%data_type  = MAT_T_INT16
    matvar%rank       = 2
    matvar%dims(1)    = SIZE(int16_data,1)
    matvar%dims(2)    = SIZE(int16_data,2)
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateInt16_2

FUNCTION FMat_VarCreateInt16_3(varname,int16_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    INTEGER(2),DIMENSION(:,:,:) :: int16_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_INT16
    matvar%data_type  = MAT_T_INT16
    matvar%rank       = 3
    matvar%dims(1)    = SIZE(int16_data,1)
    matvar%dims(2)    = SIZE(int16_data,2)
    matvar%dims(3)    = SIZE(int16_data,3)
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateInt16_3

FUNCTION FMat_VarCreateInt8_0(varname,int8_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    INTEGER(1)                      :: int8_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_INT8
    matvar%data_type  = MAT_T_INT8
    matvar%rank       = 2
    matvar%dims(1)    = 1
    matvar%dims(2)    = 1
    matvar%isComplex  = 0
    matvar%isGlobal   = 0
    matvar%isLogical  = 0
    matvar%name       = varname
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,          &
                           matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateInt8_0

FUNCTION FMat_VarCreateInt8_1(varname,int8_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    INTEGER(1),DIMENSION(:)   :: int8_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_INT8
    matvar%data_type  = MAT_T_INT8
    matvar%rank       = 2
    matvar%dims(1)    = SIZE(int8_data)
    matvar%dims(2)    = 1
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateInt8_1

FUNCTION FMat_VarCreateInt8_2(varname,int8_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    INTEGER(1),DIMENSION(:,:) :: int8_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_INT8
    matvar%data_type  = MAT_T_INT8
    matvar%rank       = 2
    matvar%dims(1)    = SIZE(int8_data,1)
    matvar%dims(2)    = SIZE(int8_data,2)
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateInt8_2

FUNCTION FMat_VarCreateInt8_3(varname,int8_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    INTEGER(1),DIMENSION(:,:,:) :: int8_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_INT8
    matvar%data_type  = MAT_T_INT8
    matvar%rank       = 3
    matvar%dims(1)    = SIZE(int8_data,1)
    matvar%dims(2)    = SIZE(int8_data,2)
    matvar%dims(3)    = SIZE(int8_data,3)
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateInt8_3

FUNCTION FMat_VarCreateChar_0(varname,char_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    CHARACTER                       :: char_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_CHAR
    matvar%data_type  = MAT_T_UINT8
    matvar%rank       = 2
    matvar%dims(1)    = 1
    matvar%dims(2)    = 1
    matvar%isComplex  = 0
    matvar%isGlobal   = 0
    matvar%isLogical  = 0
    matvar%name       = varname
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,          &
                           matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateChar_0

FUNCTION FMat_VarCreateChar_1(varname,char_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    CHARACTER(1),DIMENSION(:)       :: char_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_CHAR
    matvar%data_type  = MAT_T_UINT8
    matvar%rank       = 2
    matvar%dims(1)    = SIZE(char_data)
    matvar%dims(2)    = 1
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateChar_1

FUNCTION FMat_VarCreateChar_2(varname,char_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    CHARACTER(1),DIMENSION(:,:)     :: char_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_CHAR
    matvar%data_type  = MAT_T_UINT8
    matvar%rank       = 2
    matvar%dims(1)    = SIZE(char_data,1)
    matvar%dims(2)    = SIZE(char_data,2)
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateChar_2

FUNCTION FMat_VarCreateChar_3(varname,char_data,matvar) RESULT(err)
IMPLICIT NONE
    INTEGER                         :: err
    CHARACTER(LEN=*)                :: varname
    CHARACTER(1),DIMENSION(:,:,:)   :: char_data
    TYPE(matvar_t),INTENT(INOUT)    :: matvar

    INTEGER,EXTERNAL                :: fmat_varcreate_c

    matvar%class_type = MAT_C_CHAR
    matvar%data_type  = MAT_T_UINT8
    matvar%rank       = 3
    matvar%dims(1)    = SIZE(char_data,1)
    matvar%dims(2)    = SIZE(char_data,2)
    matvar%dims(3)    = SIZE(char_data,3)
    err = fmat_varcreate_c(matvar%rank,matvar%dims,varname,matvar%class_type,matvar%data_type,matvar)
END FUNCTION FMat_VarCreateChar_3
