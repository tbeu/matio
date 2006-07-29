!
! Copyright (C) 2005-2006   Christopher C. Hulbert
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation; either
!  version 2.1 of the License, or (at your option) any later version.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, write to the Free Software
!  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
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
