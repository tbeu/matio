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

!   FIXME: Find a less memory hungry way to split the complex data

FUNCTION FMat_VarWriteDouble_0(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    REAL(8),INTENT(IN)                       :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteDouble_0

FUNCTION FMat_VarWriteDouble_1(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    REAL(8),INTENT(IN),DIMENSION(:)          :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteDouble_1

FUNCTION FMat_VarWriteDouble_2(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    REAL(8),INTENT(IN),DIMENSION(:,:)        :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteDouble_2

FUNCTION FMat_VarWriteDouble_3(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    REAL(8),INTENT(IN),DIMENSION(:,:,:)      :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteDouble_3

FUNCTION FMat_VarWriteComplexDouble_0(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    DOUBLE COMPLEX,INTENT(IN)                :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteComplexDouble_0

FUNCTION FMat_VarWriteComplexDouble_1(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    DOUBLE COMPLEX,INTENT(IN),DIMENSION(:)   :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress
    DOUBLE PRECISION,DIMENSION(2*SIZE(d))      :: d_split

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    d_split(1:SIZE(d))  = REAL(d)
    d_split(SIZE(d)+1:) = AIMAG(d)
    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d_split,compress)
    END IF
END FUNCTION FMat_VarWriteComplexDouble_1

FUNCTION FMat_VarWriteComplexDouble_2(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    DOUBLE COMPLEX,INTENT(IN),DIMENSION(:,:) :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress
    DOUBLE PRECISION,DIMENSION(2*PRODUCT(SHAPE(d))) :: d_split

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    d_split(1:PRODUCT(SHAPE(d)))  = REAL(RESHAPE(d,(/2*PRODUCT(SHAPE(d))/)))
    d_split(PRODUCT(SHAPE(d))+1:) = AIMAG(RESHAPE(d,(/2*PRODUCT(SHAPE(d))/)))
    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d_split,compress)
    END IF
END FUNCTION FMat_VarWriteComplexDouble_2

FUNCTION FMat_VarWriteComplexDouble_3(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                    :: err
    TYPE(mat_t)                                :: mat
    TYPE(matvar_t)                             :: matvar
    DOUBLE COMPLEX,INTENT(IN),DIMENSION(:,:,:) :: d
    INTEGER,INTENT(IN),OPTIONAL                :: compress
    DOUBLE PRECISION,DIMENSION(2*PRODUCT(SHAPE(d))) :: d_split

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    d_split(1:PRODUCT(SHAPE(d)))  = REAL(RESHAPE(d,(/2*PRODUCT(SHAPE(d))/)))
    d_split(PRODUCT(SHAPE(d))+1:) = AIMAG(RESHAPE(d,(/2*PRODUCT(SHAPE(d))/)))
    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d_split,compress)
    END IF
END FUNCTION FMat_VarWriteComplexDouble_3

FUNCTION FMat_VarWriteSingle_0(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    REAL(4),INTENT(IN)                    :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteSingle_0

FUNCTION FMat_VarWriteSingle_1(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    REAL(4),INTENT(IN),DIMENSION(:)          :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteSingle_1

FUNCTION FMat_VarWriteSingle_2(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    REAL(4),INTENT(IN),DIMENSION(:,:)        :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteSingle_2

FUNCTION FMat_VarWriteSingle_3(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    REAL(4),INTENT(IN),DIMENSION(:,:,:)      :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteSingle_3

FUNCTION FMat_VarWriteComplexSingle_0(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    COMPLEX(4),INTENT(IN)                    :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteComplexSingle_0

FUNCTION FMat_VarWriteComplexSingle_1(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    COMPLEX(4),INTENT(IN),DIMENSION(:)  :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress
    REAL(4),DIMENSION(2*SIZE(d))             :: d_split

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    d_split(1:SIZE(d))  = REAL(d)
    d_split(SIZE(d)+1:) = AIMAG(d)
    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d_split,compress)
    END IF
END FUNCTION FMat_VarWriteComplexSingle_1

FUNCTION FMat_VarWriteComplexSingle_2(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    COMPLEX(4),INTENT(IN),DIMENSION(:,:) :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress
    REAL(4),DIMENSION(2*PRODUCT(SHAPE(d))) :: d_split

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    d_split(1:PRODUCT(SHAPE(d)))  = REAL(RESHAPE(d,(/2*PRODUCT(SHAPE(d))/)))
    d_split(PRODUCT(SHAPE(d))+1:) = AIMAG(RESHAPE(d,(/2*PRODUCT(SHAPE(d))/)))
    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d_split,compress)
    END IF
END FUNCTION FMat_VarWriteComplexSingle_2

FUNCTION FMat_VarWriteComplexSingle_3(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                :: err
    TYPE(mat_t)                            :: mat
    TYPE(matvar_t)                         :: matvar
    COMPLEX(4),INTENT(IN),DIMENSION(:,:,:) :: d
    INTEGER,INTENT(IN),OPTIONAL            :: compress
    REAL(4),DIMENSION(2*PRODUCT(SHAPE(d))) :: d_split

    INTEGER,EXTERNAL                       :: fmat_varwrite_c

    d_split(1:PRODUCT(SHAPE(d)))  = REAL(RESHAPE(d,(/2*PRODUCT(SHAPE(d))/)))
    d_split(PRODUCT(SHAPE(d))+1:) = AIMAG(RESHAPE(d,(/2*PRODUCT(SHAPE(d))/)))
    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d_split,compress)
    END IF
END FUNCTION FMat_VarWriteComplexSingle_3

FUNCTION FMat_VarWriteInt32_0(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(4),INTENT(IN)                    :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteInt32_0

FUNCTION FMat_VarWriteInt32_1(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(4),INTENT(IN),DIMENSION(:)       :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteInt32_1

FUNCTION FMat_VarWriteInt32_2(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(4),INTENT(IN),DIMENSION(:,:)     :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteInt32_2

FUNCTION FMat_VarWriteInt32_3(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(4),INTENT(IN),DIMENSION(:,:,:)   :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteInt32_3

FUNCTION FMat_VarWriteInt16_0(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(2),INTENT(IN)                    :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteInt16_0

FUNCTION FMat_VarWriteInt16_1(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(2),INTENT(IN),DIMENSION(:)       :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteInt16_1

FUNCTION FMat_VarWriteInt16_2(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(2),INTENT(IN),DIMENSION(:,:)     :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteInt16_2

FUNCTION FMat_VarWriteInt16_3(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(2),INTENT(IN),DIMENSION(:,:,:)   :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteInt16_3

FUNCTION FMat_VarWriteInt8_0(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(1),INTENT(IN)                    :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteInt8_0

FUNCTION FMat_VarWriteInt8_1(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(1),INTENT(IN),DIMENSION(:)       :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteInt8_1

FUNCTION FMat_VarWriteInt8_2(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(1),INTENT(IN),DIMENSION(:,:)     :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteInt8_2

FUNCTION FMat_VarWriteInt8_3(mat,matvar,d,compress) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(1),INTENT(IN),DIMENSION(:,:,:)   :: d
    INTEGER,INTENT(IN),OPTIONAL              :: compress

    INTEGER,EXTERNAL                         :: fmat_varwrite_c

    IF ( .NOT. PRESENT(compress) ) THEN
        err = fmat_varwrite_c(mat,matvar,d,0)
    ELSE
        err = fmat_varwrite_c(mat,matvar,d,compress)
    END IF
END FUNCTION FMat_VarWriteInt8_3
