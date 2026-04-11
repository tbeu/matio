!
! Copyright (c) 2015-2026, The matio contributors
! Copyright (c) 2005-2014, Christopher C. Hulbert
! All rights reserved.
!
| SPDX-License-Identifier: BSD-2-Clause
!

! FIXME: If stride is not present, assume 1

FUNCTION FMat_VarWriteDoubleData_0(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    DOUBLE PRECISION,INTENT(IN)              :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteDoubleData_0

FUNCTION FMat_VarWriteDoubleData_1(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    REAL(8),INTENT(IN),DIMENSION(:)          :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteDoubleData_1

FUNCTION FMat_VarWriteDoubleData_2(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    REAL(KIND=8),INTENT(IN),DIMENSION(:,:)   :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteDoubleData_2

FUNCTION FMat_VarWriteDoubleData_3(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    REAL(KIND=8),INTENT(IN),DIMENSION(:,:,:) :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteDoubleData_3

FUNCTION FMat_VarWriteComplexDoubleData_0(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    DOUBLE COMPLEX,INTENT(IN)                :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteComplexDoubleData_0

FUNCTION FMat_VarWriteComplexDoubleData_1(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    DOUBLE COMPLEX,INTENT(IN),DIMENSION(:)   :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge
    DOUBLE PRECISION,DIMENSION(2*SIZE(d))    :: d_split

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    d_split(1:SIZE(d))  = REAL(d)
    d_split(SIZE(d)+1:) = AIMAG(d)
    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d_split,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d_split,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteComplexDoubleData_1

FUNCTION FMat_VarWriteComplexDoubleData_2(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    DOUBLE COMPLEX,INTENT(IN),DIMENSION(:,:) :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge
    DOUBLE PRECISION,DIMENSION(2*PRODUCT(SHAPE(d))) :: d_split

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    d_split(1:PRODUCT(SHAPE(d)))  = REAL(RESHAPE(d,(/2*PRODUCT(SHAPE(d))/)))
    d_split(PRODUCT(SHAPE(d))+1:) = AIMAG(RESHAPE(d,(/2*PRODUCT(SHAPE(d))/)))
    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d_split,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d_split,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteComplexDoubleData_2

FUNCTION FMat_VarWriteComplexDoubleData_3(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                    :: err
    TYPE(mat_t)                                :: mat
    TYPE(matvar_t)                             :: matvar
    DOUBLE COMPLEX,INTENT(IN),DIMENSION(:,:,:) :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge
    DOUBLE PRECISION,DIMENSION(2*PRODUCT(SHAPE(d))) :: d_split

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    d_split(1:PRODUCT(SHAPE(d)))  = REAL(RESHAPE(d,(/2*PRODUCT(SHAPE(d))/)))
    d_split(PRODUCT(SHAPE(d))+1:) = AIMAG(RESHAPE(d,(/2*PRODUCT(SHAPE(d))/)))
    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d_split,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d_split,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteComplexDoubleData_3

FUNCTION FMat_VarWriteSingleData_0(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    REAL(4),INTENT(IN)                       :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteSingleData_0

FUNCTION FMat_VarWriteSingleData_1(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    REAL(4),INTENT(IN),DIMENSION(:)          :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteSingleData_1

FUNCTION FMat_VarWriteSingleData_2(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    REAL(4),INTENT(IN),DIMENSION(:,:)        :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteSingleData_2

FUNCTION FMat_VarWriteSingleData_3(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    REAL(4),INTENT(IN),DIMENSION(:,:,:)      :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteSingleData_3

FUNCTION FMat_VarWriteComplexSingleData_0(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    COMPLEX(4),INTENT(IN)                :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteComplexSingleData_0

FUNCTION FMat_VarWriteComplexSingleData_1(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    COMPLEX(4),INTENT(IN),DIMENSION(:)   :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge
    REAL(4),DIMENSION(2*SIZE(d))    :: d_split

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    d_split(1:SIZE(d))  = REAL(d)
    d_split(SIZE(d)+1:) = AIMAG(d)
    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d_split,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d_split,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteComplexSingleData_1

FUNCTION FMat_VarWriteComplexSingleData_2(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    COMPLEX(4),INTENT(IN),DIMENSION(:,:) :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge
    REAL(4),DIMENSION(2*PRODUCT(SHAPE(d))) :: d_split

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    d_split(1:PRODUCT(SHAPE(d)))  = REAL(RESHAPE(d,(/2*PRODUCT(SHAPE(d))/)))
    d_split(PRODUCT(SHAPE(d))+1:) = AIMAG(RESHAPE(d,(/2*PRODUCT(SHAPE(d))/)))
    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d_split,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d_split,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteComplexSingleData_2

FUNCTION FMat_VarWriteComplexSingleData_3(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                    :: err
    TYPE(mat_t)                                :: mat
    TYPE(matvar_t)                             :: matvar
    COMPLEX(4),INTENT(IN),DIMENSION(:,:,:) :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge
    REAL(4),DIMENSION(2*PRODUCT(SHAPE(d))) :: d_split

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    d_split(1:PRODUCT(SHAPE(d)))  = REAL(RESHAPE(d,(/2*PRODUCT(SHAPE(d))/)))
    d_split(PRODUCT(SHAPE(d))+1:) = AIMAG(RESHAPE(d,(/2*PRODUCT(SHAPE(d))/)))
    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d_split,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d_split,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteComplexSingleData_3

FUNCTION FMat_VarWriteInt32Data_0(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(4),INTENT(IN)                    :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteInt32Data_0

FUNCTION FMat_VarWriteInt32Data_1(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(4),INTENT(IN),DIMENSION(:)       :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteInt32Data_1

FUNCTION FMat_VarWriteInt32Data_2(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(4),INTENT(IN),DIMENSION(:,:)     :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteInt32Data_2

FUNCTION FMat_VarWriteInt32Data_3(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(4),INTENT(IN),DIMENSION(:,:,:)   :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteInt32Data_3

FUNCTION FMat_VarWriteInt16Data_0(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(2),INTENT(IN)                    :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteInt16Data_0

FUNCTION FMat_VarWriteInt16Data_1(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(2),INTENT(IN),DIMENSION(:)       :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteInt16Data_1

FUNCTION FMat_VarWriteInt16Data_2(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(2),INTENT(IN),DIMENSION(:,:)     :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteInt16Data_2

FUNCTION FMat_VarWriteInt16Data_3(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(2),INTENT(IN),DIMENSION(:,:,:)   :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteInt16Data_3

FUNCTION FMat_VarWriteInt8Data_0(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(1),INTENT(IN)                    :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteInt8Data_0

FUNCTION FMat_VarWriteInt8Data_1(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(1),INTENT(IN),DIMENSION(:)       :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteInt8Data_1

FUNCTION FMat_VarWriteInt8Data_2(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(1),INTENT(IN),DIMENSION(:,:)     :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteInt8Data_2

FUNCTION FMat_VarWriteInt8Data_3(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    INTEGER(1),INTENT(IN),DIMENSION(:,:,:)   :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteInt8Data_3

FUNCTION FMat_VarWriteCharData_0(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    CHARACTER,INTENT(IN)                     :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteCharData_0

FUNCTION FMat_VarWriteCharData_1(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    CHARACTER(1),INTENT(IN),DIMENSION(:)     :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteCharData_1

FUNCTION FMat_VarWriteCharData_2(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    CHARACTER(1),INTENT(IN),DIMENSION(:,:)   :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteCharData_2

FUNCTION FMat_VarWriteCharData_3(mat,matvar,d,start,stride,edge) RESULT(err)
IMPLICIT NONE
    INTEGER                                  :: err
    TYPE(mat_t)                              :: mat
    TYPE(matvar_t)                           :: matvar
    CHARACTER(1),INTENT(IN),DIMENSION(:,:,:) :: d
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: start
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: stride
    INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: edge

    INTEGER,EXTERNAL                         :: fmat_varwritedata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varwritedata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varwritedata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarWriteCharData_3
