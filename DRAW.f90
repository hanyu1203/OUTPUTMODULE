MODULE DRAW
USE BASIC_DATA
USE OUTPUT_PARAMETERS
IMPLICIT NONE
PRIVATE

PUBLIC :: COLUMN, TABLE, COLUMN_NODE, CURVE, RADIAL_LAYOUT, FMT_REAL, CORE_VIEW, TABLE_NODE, CURVE_NODE, &
          CORE_VIEW_NODE, RADIAL_LAYOUT_NODE

PUBLIC :: INSERT_STRING

PUBLIC :: COLUMN_FILLING

PUBLIC :: INSERT_RETURN

PUBLIC :: GENERATE_NAME_VALUE_PAIRS

PUBLIC :: GENERATE_LABLE

PUBLIC :: GENERATE_LOGO_TITLE

PUBLIC :: ECHO_INPUT

PUBLIC :: READ_EXTERNAL_TEXT

PUBLIC :: OUTPUT_TEXT

PUBLIC :: OUTPUT_FULTXT

PUBLIC :: CORE_VIEW_RADIAL_ASSEMBLY

PUBLIC :: TABLE_VIEW

INTERFACE INSERT_STRING
	MODULE PROCEDURE INSERT_STRING
	MODULE PROCEDURE INSERT_STRING_1
END INTERFACE INSERT_STRING

INTERFACE COLUMN_FILLING
	MODULE PROCEDURE COLUMN_FILLING
	MODULE PROCEDURE COLUMN_WITH_SUBCOLUMN_FILLING
END INTERFACE COLUMN_FILLING



TYPE COLUMN
    INTEGER(NM_INT)            :: TITLE_ROW_NUMBER
    INTEGER(NM_INT)            :: COLUMN_PRT_WIDTH
    INTEGER(NM_INT)            :: FMT_LEN = 0
    INTEGER(NM_INT)            :: COLUMN_FROM
    INTEGER(NM_INT)            :: COLUMN_TO
    INTEGER(NM_INT)            :: TITLE_ROW_FROM
    INTEGER(NM_INT)            :: TITLE_ROW_TO
    INTEGER(NM_INT)            :: CONTENT_ROW_FROM
    INTEGER(NM_INT)            :: SUB_COLUMN_NUMBER = 1
    INTEGER(NM_INT)            :: TOTAL_HEIGHT
    CHARACTER,          POINTER :: FMT(:) => NULL() 
    TYPE(ARRAY_OF_STR), POINTER :: TITLE_POINTER(:) => NULL()     ! POINTER OF AN ARRAY
    TYPE(COLUMN),       POINTER :: SUB_COLUMN_POINTER(:) => NULL()
    TYPE(DATA_VECTOR),  POINTER :: DATA_VEC => NULL()
END TYPE

TYPE TABLE
    CHARACTER, ALLOCATABLE :: NAME(:)
    INTEGER(NM_INT)       :: NAME_LEN = 0
    INTEGER(NM_INT)       :: COLUMN_NUMBER = 0
    TYPE(COLUMN), POINTER  :: COLUMN_POINTER(:) => NULL()
    INTEGER(NM_INT)       :: TABLE_PRT_WIDTH = 0
    INTEGER(NM_INT)       :: TABLE_PRT_HEIGHT = 0
    CHARACTER              :: ROW_SEPERATOR = '-'
    CHARACTER              :: COLUMN_SEPERATOR = ' '
END TYPE

TYPE COLUMN_NODE
    TYPE(COLUMN_NODE), POINTER :: UPPER_LEVEL => NULL()
    TYPE(COLUMN_NODE), POINTER :: LOWER_LEVEL => NULL()
    TYPE(COLUMN), POINTER      :: COLUMNS(:) => NULL()
    INTEGER(NM_INT)           :: INDEX_IN_LEVEL = 1
    INTEGER(NM_INT)           :: COLUMN_NUMBER
END TYPE COLUMN_NODE

TYPE CURVE
    CHARACTER, ALLOCATABLE     :: NAME(:)
    INTEGER(NM_INT)           :: NAME_LEN = 0
    CHARACTER                  :: CURVE_TYPE
    REAL(DP)                   :: UNIT
    REAL(DP)                   :: OFFSET
    CHARACTER                  :: POINT = '*'
    TYPE(DATA_VECTOR), POINTER :: DATA_VEC
END TYPE

TYPE RADIAL_LAYOUT
    CHARACTER, ALLOCATABLE    :: NAME(:)
    INTEGER(NM_INT)          :: NAME_LEN = 0
    INTEGER(NM_INT), POINTER :: LAYOUT(:,:)
    INTEGER(NM_INT)          :: LEN
END TYPE RADIAL_LAYOUT

TYPE FMT_REAL
    TYPE(ARRAY_OF_STR) :: REL_FMT
    INTEGER(NM_INT)   :: REL_PRINT_LEN
END TYPE

TYPE CORE_VIEW
    CHARACTER, ALLOCATABLE          :: NAME(:)
    INTEGER(NM_INT)                :: NAME_LEN = 0
    CHARACTER                       :: VIEW_TYPE
    INTEGER(NM_INT)                :: OVERLAP
    INTEGER(NM_INT)                :: NODALS = 0
    CHARACTER                       :: MESH_ON = 'F'
    TYPE(FMT_REAL)    , ALLOCATABLE :: FMT_ARRAY(:)
    TYPE(RADIAL_LAYOUT)   , POINTER :: LAYOUT_PTR => NULL()
    TYPE(DATA_MATRIX_NODE), POINTER :: DATA_LINK => NULL()
END TYPE

TYPE TABLE_NODE
    TYPE(TABLE)     , POINTER :: TAB_PTR
    TYPE(TABLE_NODE), POINTER :: NEXT_PTR => NULL()
END TYPE

TYPE CURVE_NODE
    TYPE(CURVE)     , POINTER :: CUR_PTR
    TYPE(CURVE_NODE), POINTER :: NEXT_PTR => NULL()
END TYPE

TYPE CORE_VIEW_NODE
    TYPE(CORE_VIEW)     , POINTER :: VIE_PTR
    TYPE(CORE_VIEW_NODE), POINTER :: NEXT_PTR => NULL()
END TYPE

TYPE RADIAL_LAYOUT_NODE
    TYPE(RADIAL_LAYOUT)     , POINTER :: LAY_PTR
    TYPE(RADIAL_LAYOUT_NODE), POINTER :: NEXT_PTR => NULL()
END TYPE

!TYPE(MATRIX_OF_CHA)           :: CHA_MATRIX_GLOBAL
!TYPE(STR_MATRIX), ALLOCATABLE :: STR_OF_MATRIX_GLOBAL(:)
!INTEGER(NM_INT)              :: LEN_OF_STR_MATRIX_GLOBAL

CONTAINS
	! FOR NONE SUBCOLUMN FILLING
	SUBROUTINE COLUMN_FILLING(COLUMN_PTR, DATA_VEC, COLUMN_PRT_WIDTH, TOTAL_HEIGHT, TITLE_ROW_NUMBER, &
		TITLE_ROW_TO, CONTENT_ROW_FROM, COLUMN_FROM, TITLE_POINTER, FMT_LEN, FMT)
		IMPLICIT NONE
		TYPE(COLUMN),        POINTER, INTENT(IN OUT) :: COLUMN_PTR
		TYPE(DATA_VECTOR),   POINTER, INTENT(IN)     :: DATA_VEC
		INTEGER(NM_INT),             INTENT(IN)     :: COLUMN_PRT_WIDTH
		INTEGER(NM_INT),             INTENT(IN)     :: TOTAL_HEIGHT
		INTEGER(NM_INT),             INTENT(IN)     :: TITLE_ROW_NUMBER
		INTEGER(NM_INT),             INTENT(IN)     :: TITLE_ROW_TO
		INTEGER(NM_INT),             INTENT(IN)     :: CONTENT_ROW_FROM
		INTEGER(NM_INT),             INTENT(IN)     :: COLUMN_FROM
		TYPE(ARRAY_OF_STR),  POINTER, INTENT(IN)     :: TITLE_POINTER(:)
		INTEGER(NM_INT),    INTENT(IN), OPTIONAL    :: FMT_LEN
		CHARACTER,  POINTER, INTENT(IN), OPTIONAL    :: FMT(:)
		
		IF (.NOT. ASSOCIATED(COLUMN_PTR)) THEN
			WRITE(*, *) 'ERROR IN COLUMN_FILLING'
			RETURN
		END IF
		
		COLUMN_PTR%TITLE_ROW_NUMBER =  TITLE_ROW_NUMBER
		COLUMN_PTR%COLUMN_PRT_WIDTH =  COLUMN_PRT_WIDTH
		COLUMN_PTR%COLUMN_FROM      =  COLUMN_FROM
		COLUMN_PTR%COLUMN_TO        =  COLUMN_FROM - 1 + COLUMN_PRT_WIDTH  
		COLUMN_PTR%TITLE_ROW_TO     =  TITLE_ROW_TO
		COLUMN_PTR%TITLE_ROW_FROM   =  TITLE_ROW_TO - TITLE_ROW_NUMBER + 1
		COLUMN_PTR%CONTENT_ROW_FROM =  CONTENT_ROW_FROM
		COLUMN_PTR%TOTAL_HEIGHT     =  TOTAL_HEIGHT
		COLUMN_PTR%DATA_VEC         => DATA_VEC
		COLUMN_PTR%TITLE_POINTER    => TITLE_POINTER
		IF (PRESENT(FMT_LEN) .AND. PRESENT(FMT)) THEN
			COLUMN_PTR%FMT_LEN      =  FMT_LEN
			COLUMN_PTR%FMT          => FMT
		END IF
	END SUBROUTINE COLUMN_FILLING
	
	SUBROUTINE COLUMN_WITH_SUBCOLUMN_FILLING(COLUMN_PTR, COLUMN_PRT_WIDTH, TOTAL_HEIGHT, TITLE_ROW_NUMBER, TITLE_ROW_TO, &
		CONTENT_ROW_FROM, COLUMN_FROM, TITLE_POINTER, SUB_COLUMN_NUMBER, SUBCOLUMN_PTR)
		IMPLICIT NONE
		TYPE(COLUMN),        POINTER, INTENT(IN OUT) :: COLUMN_PTR
		INTEGER(NM_INT),             INTENT(IN)     :: COLUMN_PRT_WIDTH
		INTEGER(NM_INT),             INTENT(IN)     :: TOTAL_HEIGHT
		INTEGER(NM_INT),             INTENT(IN)     :: TITLE_ROW_NUMBER
		INTEGER(NM_INT),             INTENT(IN)     :: TITLE_ROW_TO
		INTEGER(NM_INT),             INTENT(IN)     :: CONTENT_ROW_FROM
		INTEGER(NM_INT),             INTENT(IN)     :: COLUMN_FROM
		TYPE(ARRAY_OF_STR),  POINTER, INTENT(IN)     :: TITLE_POINTER(:)
		INTEGER(NM_INT),             INTENT(IN)     :: SUB_COLUMN_NUMBER
		TYPE(COLUMN),        POINTER, INTENT(IN)     :: SUBCOLUMN_PTR(:)
		
		IF (.NOT. ASSOCIATED(COLUMN_PTR) .OR. .NOT. ASSOCIATED(SUBCOLUMN_PTR)) THEN
			WRITE(*, *) 'ERROR IN COLUMN_WITH_SUBCOLUMN_FILLING'
			RETURN
		END IF
		
		COLUMN_PTR%TITLE_ROW_NUMBER   =  TITLE_ROW_NUMBER
		COLUMN_PTR%COLUMN_PRT_WIDTH   =  COLUMN_PRT_WIDTH
		COLUMN_PTR%COLUMN_FROM        =  COLUMN_FROM
		COLUMN_PTR%COLUMN_TO          =  COLUMN_FROM - 1 + COLUMN_PRT_WIDTH  
		COLUMN_PTR%TITLE_ROW_TO       =  TITLE_ROW_TO
		COLUMN_PTR%TITLE_ROW_FROM     =  TITLE_ROW_TO - TITLE_ROW_NUMBER + 1
		COLUMN_PTR%CONTENT_ROW_FROM   =  CONTENT_ROW_FROM
		COLUMN_PTR%TOTAL_HEIGHT       =  TOTAL_HEIGHT
		COLUMN_PTR%DATA_VEC           => NULL()
		COLUMN_PTR%TITLE_POINTER      => TITLE_POINTER
		COLUMN_PTR%SUB_COLUMN_NUMBER  =  SUB_COLUMN_NUMBER
		COLUMN_PTR%SUB_COLUMN_POINTER => SUBCOLUMN_PTR
		
	END SUBROUTINE COLUMN_WITH_SUBCOLUMN_FILLING

    SUBROUTINE INSERT_STRING(STR_LABLE, CHA_LNK_PTR, STEP_BACK)
        IMPLICIT NONE
        TYPE(ARRAY_OF_STR),                INTENT(IN)           :: STR_LABLE
        TYPE(MATRIX_OF_CHA_NODE), POINTER, INTENT(INOUT)        :: CHA_LNK_PTR
        INTEGER(NM_INT),                  INTENT(IN), OPTIONAL :: STEP_BACK
        
        INTEGER(NM_INT) :: I
        INTEGER(NM_INT) :: LOCAL_STEP_BACK
        INTEGER(NM_INT), PARAMETER :: DEFAULT_STEP_BACK = 1
        TYPE(MATRIX_OF_CHA_NODE), POINTER :: PTR
        
        IF (PRESENT(STEP_BACK)) THEN
            LOCAL_STEP_BACK = STEP_BACK
        ELSE
            LOCAL_STEP_BACK = DEFAULT_STEP_BACK
        END IF
        
        NULLIFY(PTR)
        ALLOCATE(PTR)
        PTR%NEXT => NULL()
        ALLOCATE(PTR%CONTENT)
        PTR%CONTENT%ROW = 1
        PTR%CONTENT%COLUMN = STR_LABLE%LENGTH_OF_STR + LOCAL_STEP_BACK
        ALLOCATE(PTR%CONTENT%MATRIX(PTR%CONTENT%COLUMN, PTR%CONTENT%ROW))
        PTR%CONTENT%MATRIX = ' '
        DO I = 1, PTR%CONTENT%COLUMN
            PTR%CONTENT%MATRIX(LOCAL_STEP_BACK + I, 1) =  STR_LABLE%STR(I)
        END DO
        
        IF (.NOT. ASSOCIATED(CHA_LNK_PTR)) THEN
            CHA_LNK_PTR => PTR
        ELSE IF (.NOT. ASSOCIATED(CHA_LNK_PTR%NEXT)) THEN
            CHA_LNK_PTR%NEXT => PTR
            CHA_LNK_PTR => CHA_LNK_PTR%NEXT
        ELSE
            PTR%NEXT => CHA_LNK_PTR%NEXT
            CHA_LNK_PTR%NEXT => PTR
            CHA_LNK_PTR => CHA_LNK_PTR%NEXT
        END IF
    END SUBROUTINE INSERT_STRING
    
    SUBROUTINE INSERT_STRING_1(STR_LABLE, CHA_LNK_PTR, STEP_BACK)
        IMPLICIT NONE
        CHARACTER(LEN=*),                  INTENT(IN)           :: STR_LABLE
        TYPE(MATRIX_OF_CHA_NODE), POINTER, INTENT(INOUT)        :: CHA_LNK_PTR
        INTEGER(NM_INT),                  INTENT(IN), OPTIONAL :: STEP_BACK
        
        INTEGER(NM_INT) :: I
        INTEGER(NM_INT) :: LOCAL_STEP_BACK
        INTEGER(NM_INT), PARAMETER :: DEFAULT_STEP_BACK = 1
        TYPE(MATRIX_OF_CHA_NODE), POINTER :: PTR
        
        IF (PRESENT(STEP_BACK)) THEN
            LOCAL_STEP_BACK = STEP_BACK
        ELSE
            LOCAL_STEP_BACK = DEFAULT_STEP_BACK
        END IF
        
        NULLIFY(PTR)
        ALLOCATE(PTR)
        PTR%NEXT => NULL()
        ALLOCATE(PTR%CONTENT)
        PTR%CONTENT%ROW = 1
        PTR%CONTENT%COLUMN = LEN(STR_LABLE) + LOCAL_STEP_BACK
        ALLOCATE(PTR%CONTENT%MATRIX(PTR%CONTENT%COLUMN, PTR%CONTENT%ROW))
        PTR%CONTENT%MATRIX = ' '
        DO I = 1, LEN(STR_LABLE)
            PTR%CONTENT%MATRIX(LOCAL_STEP_BACK + I, 1) =  STR_LABLE(I : I)
        END DO
        
        IF (.NOT. ASSOCIATED(CHA_LNK_PTR)) THEN
            CHA_LNK_PTR => PTR
        ELSE IF (.NOT. ASSOCIATED(CHA_LNK_PTR%NEXT)) THEN
            CHA_LNK_PTR%NEXT => PTR
            CHA_LNK_PTR => CHA_LNK_PTR%NEXT
        ELSE
            PTR%NEXT => CHA_LNK_PTR%NEXT
            CHA_LNK_PTR%NEXT => PTR
            CHA_LNK_PTR => CHA_LNK_PTR%NEXT
        END IF
    END SUBROUTINE INSERT_STRING_1
    
    SUBROUTINE INSERT_RETURN(NUM, CHA_LNK_PTR)
        IMPLICIT NONE
        INTEGER(NM_INT),                  INTENT(IN)    :: NUM
        TYPE(MATRIX_OF_CHA_NODE), POINTER, INTENT(INOUT) :: CHA_LNK_PTR
        
        TYPE(MATRIX_OF_CHA_NODE), POINTER :: PTR, PTR1
        
        INTEGER(NM_INT) :: I
        NULLIFY(PTR, PTR1)
        
        PTR1 => CHA_LNK_PTR
        DO I = 1, NUM
            NULLIFY(PTR)
            ALLOCATE(PTR)
            PTR%NEXT => NULL()
            ALLOCATE(PTR%CONTENT)
            PTR%CONTENT%ROW = 0
            PTR%CONTENT%COLUMN = 0
            IF (.NOT. ASSOCIATED(PTR1)) THEN
                PTR1 => PTR
            ELSE IF (.NOT. ASSOCIATED(PTR1%NEXT)) THEN
                PTR1%NEXT => PTR
                PTR1 => PTR
            ELSE
                PTR%NEXT => PTR1%NEXT
                PTR1%NEXT => PTR
                PTR1 => PTR
            END IF
        END DO
        
        CHA_LNK_PTR => PTR1
        
    END SUBROUTINE INSERT_RETURN

    SUBROUTINE GENERATE_NAME_VALUE_PAIRS(NAME_LIST, VALUE_LIST, LIST_LEN, CHA_MAT_PTR, STEP_BACK, EQ_SIGN)
        IMPLICIT NONE
        TYPE(POINTER_ARRAY_OF_STR), ALLOCATABLE, INTENT(IN)           :: NAME_LIST(:)
        TYPE(POINTER_ARRAY_OF_STR), ALLOCATABLE, INTENT(IN)           :: VALUE_LIST(:)
        INTEGER(NM_INT),                        INTENT(IN)           :: LIST_LEN
        TYPE(MATRIX_OF_CHA), POINTER,            INTENT(OUT)          :: CHA_MAT_PTR
        INTEGER(NM_INT),                        INTENT(IN), OPTIONAL :: STEP_BACK
        CHARACTER,                               INTENT(IN), OPTIONAL :: EQ_SIGN
        
        TYPE(MATRIX_OF_CHA), POINTER :: MPTR
        INTEGER(NM_INT)             :: MAX_WIDTH, MAX_HEIGHT, MID_INDEX
        INTEGER(NM_INT)             :: I, J, K
        INTEGER(NM_INT),  PARAMETER :: DEFAULT_STEP_BACK = 1
        CHARACTER,         PARAMETER :: DEFAULT_EQ_SIGN = '='
        INTEGER(NM_INT)             :: LOCAL_STEP_BACK
        CHARACTER                    :: LOCAL_EQ_SIGN
        
        NULLIFY(MPTR)
        IF (PRESENT(STEP_BACK)) THEN
            LOCAL_STEP_BACK = STEP_BACK
        ELSE
            LOCAL_STEP_BACK = DEFAULT_STEP_BACK
        END IF
        
        IF (PRESENT(EQ_SIGN)) THEN
            LOCAL_EQ_SIGN = EQ_SIGN
        ELSE
            LOCAL_EQ_SIGN = DEFAULT_EQ_SIGN
        END IF
        
        
        MAX_HEIGHT = LIST_LEN
        MAX_WIDTH = 0
        J = 0
        K = 0
        DO I = 1, LIST_LEN
            IF (J .LT. NAME_LIST(I)%PTR%LENGTH_OF_STR) THEN
                J = NAME_LIST(I)%PTR%LENGTH_OF_STR
            END IF
            IF (K .LT. VALUE_LIST(I)%PTR%LENGTH_OF_STR) THEN
                K = VALUE_LIST(I)%PTR%LENGTH_OF_STR
            END IF
        END DO

        MAX_WIDTH = LOCAL_STEP_BACK + J + K + 3 ! 3 FOR ' ', '=', ' '
        
        MID_INDEX = LOCAL_STEP_BACK + J + 2
        
        ALLOCATE(MPTR)
        MPTR%ROW = MAX_HEIGHT
        MPTR%COLUMN = MAX_WIDTH
        ALLOCATE(MPTR%MATRIX(MAX_WIDTH, MAX_HEIGHT))
        MPTR%MATRIX = ' '
        
        DO I = 1, MAX_HEIGHT
            MPTR%MATRIX(MID_INDEX, I) = LOCAL_EQ_SIGN
            DO J = 1, NAME_LIST(I)%PTR%LENGTH_OF_STR
                MPTR%MATRIX(LOCAL_STEP_BACK + J, I) = NAME_LIST(I)%PTR%STR(J)
            END DO
            DO K = 1, VALUE_LIST(I)%PTR%LENGTH_OF_STR
                MPTR%MATRIX(MID_INDEX + K + 1, I) = VALUE_LIST(I)%PTR%STR(K)
            END DO
        END DO
        
        CHA_MAT_PTR => MPTR
    END SUBROUTINE GENERATE_NAME_VALUE_PAIRS
    
    SUBROUTINE GENERATE_LABLE(STR_LABLE, LABLE_OUT, BLANKSPACE, BOUND_H_SIG, BOUND_V_SIG, CORNER_SIG)
        IMPLICIT NONE
        TYPE(ARRAY_OF_STR),  POINTER, INTENT(IN)  :: STR_LABLE
        TYPE(MATRIX_OF_CHA), POINTER, INTENT(OUT) :: LABLE_OUT
        INTEGER(NM_INT), INTENT(IN), OPTIONAL    :: BLANKSPACE
        CHARACTER       , INTENT(IN), OPTIONAL    :: BOUND_H_SIG
        CHARACTER       , INTENT(IN), OPTIONAL    :: BOUND_V_SIG
        CHARACTER       , INTENT(IN), OPTIONAL    :: CORNER_SIG
        
        INTEGER(NM_INT)            :: J
        INTEGER(NM_INT), PARAMETER :: DEFAULT_BLANK = 3
        CHARACTER       , PARAMETER :: DEFAULT_BD_H_SIG = '-'
        CHARACTER       , PARAMETER :: DEFAULT_BD_V_SIG = '|'
        CHARACTER       , PARAMETER :: DEFAULT_CR_SIG = '+'
        INTEGER(NM_INT)            :: LOCAL_BLANK
        CHARACTER                   :: LOCAL_BD_H_SIG
        CHARACTER                   :: LOCAL_BD_V_SIG
        CHARACTER                   :: LOCAL_CR_SIG
        
        LOCAL_BLANK = DEFAULT_BLANK
        IF (PRESENT(BLANKSPACE)) THEN
            IF (BLANKSPACE .GT. 0) THEN
                LOCAL_BLANK = BLANKSPACE
            END IF
        END IF
        
        IF (PRESENT(BOUND_H_SIG)) THEN
            LOCAL_BD_H_SIG = BOUND_H_SIG
        ELSE
            LOCAL_BD_H_SIG = DEFAULT_BD_H_SIG
        END IF
        
        IF (PRESENT(BOUND_V_SIG)) THEN
            LOCAL_BD_V_SIG = BOUND_V_SIG
        ELSE
            LOCAL_BD_V_SIG = DEFAULT_BD_V_SIG
        END IF
        
        IF (PRESENT(CORNER_SIG)) THEN
            LOCAL_CR_SIG = CORNER_SIG
        ELSE
            LOCAL_CR_SIG = DEFAULT_CR_SIG
        END IF
        
        IF (.NOT. ASSOCIATED(LABLE_OUT)) THEN
            ALLOCATE(LABLE_OUT)
        END IF
        LABLE_OUT%ROW = 3
        LABLE_OUT%COLUMN = STR_LABLE%LENGTH_OF_STR + LOCAL_BLANK * 2
        IF (ALLOCATED(LABLE_OUT%MATRIX)) THEN
            DEALLOCATE(LABLE_OUT%MATRIX)
        END IF
        ALLOCATE(LABLE_OUT%MATRIX(LABLE_OUT%COLUMN , LABLE_OUT%ROW))
        LABLE_OUT%MATRIX = LOCAL_BD_H_SIG
        
        
        DO J = 1, LOCAL_BLANK
            LABLE_OUT%MATRIX(J, 2) = ' '
            LABLE_OUT%MATRIX(LABLE_OUT%COLUMN - J + 1, 2) = ' '
        END DO
        
        DO J = 1, STR_LABLE%LENGTH_OF_STR
            LABLE_OUT%MATRIX(LOCAL_BLANK + J, 2) = STR_LABLE%STR(J)
        END DO
        
        LABLE_OUT%MATRIX(1,1) = LOCAL_CR_SIG
        LABLE_OUT%MATRIX(1,3) = LOCAL_CR_SIG
        LABLE_OUT%MATRIX(LABLE_OUT%COLUMN, 1) = LOCAL_CR_SIG
        LABLE_OUT%MATRIX(LABLE_OUT%COLUMN, 3) = LOCAL_CR_SIG
        LABLE_OUT%MATRIX(1, 2) = LOCAL_BD_V_SIG
        LABLE_OUT%MATRIX(LABLE_OUT%COLUMN, 2) = LOCAL_BD_V_SIG
    END SUBROUTINE GENERATE_LABLE
    
    SUBROUTINE GENERATE_LOGO_TITLE(LOGO_OUT, VALUE_LIST)
        IMPLICIT NONE
        TYPE(MATRIX_OF_CHA), POINTER, INTENT(OUT) :: LOGO_OUT
        TYPE(POINTER_ARRAY_OF_STR), ALLOCATABLE   :: VALUE_LIST(:)
        ! VALUE_LIST(1) --> CODE_VERSION
        ! VALUE_LIST(2) --> JOB_ID
        ! VALUE_LIST(3) --> TIME_STAMP
        ! VALUE_LIST(4) --> CASE_ID
        ! VALUE_LIST(5) --> REF_TIME
        ! VALUE_LIST(6) --> REF_BU
        ! VALUE_LIST(7) --> CASE_TITLE
        
        INTEGER(NM_INT), PARAMETER   :: LEAST_LOGO_LINES = 3
        INTEGER(NM_INT), PARAMETER   :: VALUE_NUM = 7
        INTEGER(NM_INT), ALLOCATABLE :: ITEM_WIDTH(:)
        INTEGER(NM_INT), ALLOCATABLE :: NAME_WIDTH(:)
        INTEGER                       :: I, J, K, L, M
        CHARACTER, POINTER     :: TEMP
        
        ALLOCATE(ITEM_WIDTH(VALUE_NUM))
        ALLOCATE(NAME_WIDTH(VALUE_NUM))
        
        ! COULD NOT AUTOMATICALLY
        
        DO I = 1, VALUE_NUM
            SELECT CASE (I)
            CASE (1)
                NAME_WIDTH(I) = LEN(LABLE_0012)
            CASE (2)
                NAME_WIDTH(I) = LEN(LABLE_0013)
            CASE (3)
                NAME_WIDTH(I) = LEN(LABLE_0014)
            CASE (4)
                NAME_WIDTH(I) = LEN(LABLE_0015)
            CASE (5)
                NAME_WIDTH(I) = LEN(LABLE_0016)
            CASE (6)
                NAME_WIDTH(I) = LEN(LABLE_0017)
            CASE (7)
                NAME_WIDTH(I) = LEN(LABLE_0018)
            CASE DEFAULT
                NAME_WIDTH(I) = -1
            END SELECT
        END DO
         
        ! SMALL_LOGO_ROW MUST .GE. 3
        IF (SMALL_LOGO_ROW .LT. LEAST_LOGO_LINES) THEN
            WRITE(*, *) 'LOGO IS TOO TINY'
            LOGO_OUT => NULL()
            RETURN
        ELSE
            IF (.NOT. ASSOCIATED(LOGO_OUT)) THEN
                ALLOCATE(LOGO_OUT)
            ELSE
                DEALLOCATE(LOGO_OUT%MATRIX)
            END IF
            LOGO_OUT%ROW = SMALL_LOGO_ROW
        END IF
        
        LOGO_OUT%COLUMN = SMALL_LOGO_COLUMN + VALUE_NUM ! VALUE_NUM FOR ALL BLANKET
        DO J = 1, VALUE_NUM
            IF (NAME_WIDTH(J) .GT. (VALUE_LIST(J)%PTR%LENGTH_OF_STR)) THEN
                ITEM_WIDTH(J) = NAME_WIDTH(J)
            ELSE
                ITEM_WIDTH(J) = VALUE_LIST(J)%PTR%LENGTH_OF_STR
            END IF
            LOGO_OUT%COLUMN = LOGO_OUT%COLUMN + ITEM_WIDTH(J)
        END DO
        
        ALLOCATE(LOGO_OUT%MATRIX(LOGO_OUT%COLUMN, LOGO_OUT%ROW))
        LOGO_OUT%MATRIX = ' '
        
        ! COPY THE SMALL LOGO
        DO I = 1, SMALL_LOGO_ROW
            DO J = 1, SMALL_LOGO_COLUMN
                K = (I - 1) * SMALL_LOGO_COLUMN + J
                LOGO_OUT%MATRIX(J, I) = SMALL_LOGO(K : K)
            END DO
        END DO
        
        ! FOR NAME LIST
        I = SMALL_LOGO_ROW - LEAST_LOGO_LINES + 1 ! FOR ROW COUNTER
        M = SMALL_LOGO_COLUMN + 2 ! FOR COLUMN COUNTER
        DO J = 1, VALUE_NUM
            K = INT((ITEM_WIDTH(J) - NAME_WIDTH(J)) / 2) + M
            SELECT CASE (J)
            CASE (1)
                DO L = 1, NAME_WIDTH(J)
                    LOGO_OUT%MATRIX(K, I) = LABLE_0012(L : L)
                    K = K + 1
                END DO
            CASE (2)
                DO L = 1, NAME_WIDTH(J)
                    LOGO_OUT%MATRIX(K, I) = LABLE_0013(L : L)
                    K = K + 1
                END DO
            CASE (3)
                DO L = 1, NAME_WIDTH(J)
                    LOGO_OUT%MATRIX(K, I) = LABLE_0014(L : L)
                    K = K + 1
                END DO
            CASE (4)
                DO L = 1, NAME_WIDTH(J)
                    LOGO_OUT%MATRIX(K, I) = LABLE_0015(L : L)
                    K = K + 1
                END DO
            CASE (5)
                DO L = 1, NAME_WIDTH(J)
                    LOGO_OUT%MATRIX(K, I) = LABLE_0016(L : L)
                    K = K + 1
                END DO
            CASE (6)
                DO L = 1, NAME_WIDTH(J)
                    LOGO_OUT%MATRIX(K, I) = LABLE_0017(L : L)
                    K = K + 1
                END DO
            CASE (7)
                DO L = 1, NAME_WIDTH(J)
                    LOGO_OUT%MATRIX(K, I) = LABLE_0018(L : L)
                    K = K + 1
                END DO
            END SELECT
            M = M + ITEM_WIDTH(J) + 1
        END DO
        
        I = I + 1
        K = SMALL_LOGO_COLUMN + 2
        DO J = 1, VALUE_NUM
            DO L = 1, ITEM_WIDTH(J)
                LOGO_OUT%MATRIX(K, I) = '-'
                K = K + 1
            END DO
            K = K + 1
        END DO

        ! FOR VALUE LIST
        I = I + 1
        M = SMALL_LOGO_COLUMN + 2 ! FOR COLUMN COUNTER
        DO J = 1, VALUE_NUM
            K = M + (ITEM_WIDTH(J) - VALUE_LIST(J)%PTR%LENGTH_OF_STR)
            DO L = 1, VALUE_LIST(J)%PTR%LENGTH_OF_STR
                LOGO_OUT%MATRIX(K, I) = VALUE_LIST(J)%PTR%STR(L)
                K = K + 1
            END DO
            M = M + ITEM_WIDTH(J) + 1
        END DO
    END SUBROUTINE GENERATE_LOGO_TITLE

    SUBROUTINE ECHO_INPUT(INPUT_FILE_UNIT, ECHO_FILE_UNIT, LINE_MARK)
        IMPLICIT NONE
        INTEGER  , INTENT(IN)           :: INPUT_FILE_UNIT
        INTEGER  , INTENT(IN)           :: ECHO_FILE_UNIT
        CHARACTER, INTENT(IN) ,OPTIONAL :: LINE_MARK

        INTEGER               :: STATUS
        INTEGER(NM_INT)      :: COUNT
        INTEGER(NM_INT)      :: I, J
        CHARACTER(LEN = 1000) :: STR_LINE

        J = 1
        DO WHILE (.TRUE.) 
            READ(INPUT_FILE_UNIT, FMT = '(A1000)', IOSTAT = STATUS) STR_LINE
            IF (STATUS .NE. 0) THEN
                EXIT
            END IF

            IF (PRESENT(LINE_MARK)) THEN
                WRITE(ECHO_FILE_UNIT, FMT = '(I5)', ADVANCE = 'NO') J
                WRITE(ECHO_FILE_UNIT, FMT = '(A2)', ADVANCE = 'NO') '| '
            END IF
            J = J + 1
            COUNT = LEN_TRIM(STR_LINE)
            DO I = 1, COUNT
                WRITE(ECHO_FILE_UNIT, FMT = '(A1)', ADVANCE = 'NO') STR_LINE(I : I)
            END DO
            WRITE(ECHO_FILE_UNIT, '(A1)') ' '
        END DO
    END SUBROUTINE ECHO_INPUT
    
    SUBROUTINE READ_EXTERNAL_TEXT(ROW, COLUMN, TEXT_CONTAINER, EXTERNAL_FILE_UNIT)
        IMPLICIT NONE
        INTEGER(NM_INT),    INTENT(IN)  :: ROW
        INTEGER(NM_INT),    INTENT(IN)  :: COLUMN
        TYPE(MATRIX_OF_CHA), INTENT(OUT) :: TEXT_CONTAINER
        INTEGER,             INTENT(IN)  :: EXTERNAL_FILE_UNIT

        CHARACTER(LEN=1000)         :: STR_LINE
        INTEGER(NM_INT), PARAMETER :: MAX_LINE_WIDTH = 1000
        INTEGER(NM_INT)            :: I, J
        INTEGER(NM_INT)            :: COLUMN_IN_USE

        COLUMN_IN_USE = COLUMN
        IF (COLUMN .GT. MAX_LINE_WIDTH) THEN
            COLUMN_IN_USE = MAX_LINE_WIDTH
        END IF

        IF (ALLOCATED(TEXT_CONTAINER%MATRIX)) THEN
            DEALLOCATE(TEXT_CONTAINER%MATRIX)
        END IF
        ALLOCATE(TEXT_CONTAINER%MATRIX(COLUMN_IN_USE, ROW))
        TEXT_CONTAINER%ROW = ROW
        TEXT_CONTAINER%COLUMN = COLUMN_IN_USE
        
        DO I = 1, ROW
            READ(EXTERNAL_FILE_UNIT, '(A1000)') STR_LINE
            DO J = 1, COLUMN_IN_USE
                TEXT_CONTAINER%MATRIX(J, I) = STR_LINE(J : J)
            END DO
        END DO
    END SUBROUTINE READ_EXTERNAL_TEXT

    SUBROUTINE OUTPUT_TEXT(TEXT_CONTAINER, FILE_UNIT)
        IMPLICIT NONE
        TYPE(MATRIX_OF_CHA), INTENT(IN) :: TEXT_CONTAINER
        INTEGER            , INTENT(IN) :: FILE_UNIT
        
        INTEGER(NM_INT) :: I, J

        DO I = 1, TEXT_CONTAINER%ROW
            DO J = 1, TEXT_CONTAINER%COLUMN
                WRITE(FILE_UNIT, '(A1)',ADVANCE='NO') TEXT_CONTAINER%MATRIX(J, I)
            END DO
            WRITE(FILE_UNIT, '(A1)') ' '
        END DO
    END SUBROUTINE OUTPUT_TEXT
    
    SUBROUTINE OUTPUT_FULTXT(TXT_LNK_HEAD, FILE_UNIT)
        IMPLICIT NONE
        TYPE(MATRIX_OF_CHA_NODE), POINTER, INTENT(IN) :: TXT_LNK_HEAD
        INTEGER                          , INTENT(IN) :: FILE_UNIT
        
        TYPE(MATRIX_OF_CHA_NODE), POINTER :: PTR
        
        NULLIFY(PTR)
        PTR=>TXT_LNK_HEAD
        IF (.NOT. ASSOCIATED(PTR)) THEN
            WRITE(*, *) 'EMPTY OUTPUT CONTENT IN OUTPUT_FULTXT()'
            RETURN
        END IF
        
        DO WHILE (.TRUE.)
            IF (ASSOCIATED(PTR)) THEN
                IF (PTR%CONTENT%ROW .GT. 0 .AND. PTR%CONTENT%COLUMN .GT. 0 .AND. ALLOCATED(PTR%CONTENT%MATRIX)) THEN
                    CALL OUTPUT_TEXT(PTR%CONTENT, FILE_UNIT)
                ELSE
                    !FOR 'RETURN' OR EMPTY LINE OUTPUT
                    WRITE(FILE_UNIT, '(A1)') ' '
                END IF
                PTR=>PTR%NEXT
            ELSE
                EXIT
            END IF
        END DO
        
    END SUBROUTINE OUTPUT_FULTXT
    
    SUBROUTINE CORE_VIEW_RADIAL_ASSEMBLY(BLOCK_ARRAY, DIMENSION_PER_BLOCK, CHA_MATRIX, MESH_ON, CORNER, BOUND_H, BOUND_V)
        IMPLICIT NONE
        TYPE(STR_MATRIX),    POINTER, INTENT(IN)  :: BLOCK_ARRAY(:)
        INTEGER(NM_INT),             INTENT(IN)  :: DIMENSION_PER_BLOCK
        TYPE(MATRIX_OF_CHA), POINTER, INTENT(OUT) :: CHA_MATRIX
        LOGICAL,                      INTENT(IN)  :: MESH_ON
        CHARACTER, OPTIONAL,          INTENT(IN)  :: CORNER
        CHARACTER, OPTIONAL,          INTENT(IN)  :: BOUND_H
        CHARACTER, OPTIONAL,          INTENT(IN)  :: BOUND_V

        INTEGER(NM_INT)     :: MAX_ROW
        INTEGER(NM_INT)     :: TOTAL_BLOCK_ROW
        INTEGER(NM_INT)     :: TOTAL_BLOCK_COLUMN
        INTEGER(NM_INT)     :: MIN_BLOCK_HEIGHT
        INTEGER(NM_INT)     :: MIN_BLOCK_WIDTH
        INTEGER(NM_INT)     :: BLOCK_HEIGHT
        INTEGER(NM_INT)     :: BLOCK_WIDTH
        INTEGER(NM_INT)     :: MAX_CHARACTERS
        INTEGER(NM_INT)     :: MAX_ROW_LENGTH
        INTEGER(NM_INT)     :: MAX_COLUMN_HEIGHT
        INTEGER(NM_INT)     :: I, J, K, L
        INTEGER(NM_INT)     :: HEAD_HEIGHT
        INTEGER(NM_INT)     :: LEFT_WIDTH
        INTEGER(NM_INT)     :: LINE_INDEX
        INTEGER(NM_INT)     :: ROW_COUNTER
        CHARACTER            :: CORNER_CHA
        CHARACTER            :: BOUNDARY_CHA_HOR
        CHARACTER            :: BOUNDARY_CHA_VER
        CHARACTER            :: SEPERATOR
        CHARACTER(LEN=24)    :: ROW_INDEX
        CHARACTER(LEN=2)     :: ROW_CHARACTER
        CHARACTER, PARAMETER :: DEFAULT_CORNER_CHA = '+'
        CHARACTER, PARAMETER :: DEFAULT_BOUNDARY_CHA_HOR = '-'
        CHARACTER, PARAMETER :: DEFAULT_BOUNDARY_CHA_VER = '|'
        
        MAX_ROW = 23
        HEAD_HEIGHT = 1
        LEFT_WIDTH = 5
        ROW_INDEX = 'ABCDEFGHJKLMNPQRSTUVWXY'
        IF (MESH_ON .AND. PRESENT(CORNER) .AND. PRESENT(BOUND_H) .AND. PRESENT(BOUND_V)) THEN
            CORNER_CHA = CORNER
            BOUNDARY_CHA_HOR = BOUND_H
            BOUNDARY_CHA_VER = BOUND_V
        ELSE IF (MESH_ON) THEN
            CORNER_CHA = DEFAULT_CORNER_CHA
            BOUNDARY_CHA_HOR = DEFAULT_BOUNDARY_CHA_HOR
            BOUNDARY_CHA_VER = DEFAULT_BOUNDARY_CHA_VER
        ELSE
            CORNER_CHA = ' '
            BOUNDARY_CHA_HOR = ' '
            BOUNDARY_CHA_VER = ' '
            SEPERATOR = ','
        END IF
        
        TOTAL_BLOCK_ROW = BLOCK_ARRAY(1)%ROW_DIMENSION
        TOTAL_BLOCK_COLUMN = BLOCK_ARRAY(1)%COLUMN_DIMENSION
        
        MAX_CHARACTERS = 0
        DO I = 1, DIMENSION_PER_BLOCK
            DO J = 1, BLOCK_ARRAY(I)%ROW_DIMENSION
                DO K = 1, BLOCK_ARRAY(I)%COLUMN_DIMENSION
                    IF (MAX_CHARACTERS < BLOCK_ARRAY(I)%MATRIX(K, J)%LENGTH_OF_STR) THEN
                        MAX_CHARACTERS = BLOCK_ARRAY(I)%MATRIX(K, J)%LENGTH_OF_STR
                    END IF
                END DO
            END DO
        END DO

        IF (MESH_ON) THEN
            BLOCK_WIDTH = MAX_CHARACTERS + 2
            BLOCK_HEIGHT = DIMENSION_PER_BLOCK + 2
        ELSE
            BLOCK_WIDTH = MAX_CHARACTERS
            BLOCK_HEIGHT = DIMENSION_PER_BLOCK
        END IF
        
        IF (.NOT. ASSOCIATED(CHA_MATRIX)) THEN
            ALLOCATE(CHA_MATRIX)
        END IF
        IF (DIMENSION_PER_BLOCK > 1) THEN
            MAX_COLUMN_HEIGHT = TOTAL_BLOCK_ROW * (BLOCK_HEIGHT + 1) + 1 + HEAD_HEIGHT
        ELSE
            MAX_COLUMN_HEIGHT = TOTAL_BLOCK_ROW * (BLOCK_HEIGHT) + 1 + HEAD_HEIGHT
        END IF
        MAX_ROW_LENGTH = TOTAL_BLOCK_COLUMN * (BLOCK_WIDTH + 1) + 1 + LEFT_WIDTH

        CHA_MATRIX%ROW = MAX_COLUMN_HEIGHT
        CHA_MATRIX%COLUMN = MAX_ROW_LENGTH

        IF (ALLOCATED(CHA_MATRIX%MATRIX)) THEN
            DEALLOCATE(CHA_MATRIX%MATRIX)
        END IF
        ALLOCATE(CHA_MATRIX%MATRIX(CHA_MATRIX%COLUMN, CHA_MATRIX%ROW))

        ROW_COUNTER = 1
        CHA_MATRIX%MATRIX = ' '
        !FOR COLUMN INDEX
        LINE_INDEX = LEFT_WIDTH + INT(BLOCK_WIDTH / 2) + 1
        DO I = 1, TOTAL_BLOCK_COLUMN
            WRITE(ROW_CHARACTER, '(I2)') I
            DO J = 1, 2
                CHA_MATRIX%MATRIX(LINE_INDEX, ROW_COUNTER) = ROW_CHARACTER(J:J)
                LINE_INDEX = LINE_INDEX + 1
            END DO
            LINE_INDEX = LINE_INDEX + BLOCK_WIDTH - 1
        END DO
        ROW_COUNTER = ROW_COUNTER + 1
        DO I = 1, TOTAL_BLOCK_ROW
            !ADD CEILING
            IF (DIMENSION_PER_BLOCK > 1) THEN
                LINE_INDEX = LEFT_WIDTH + 1
                DO J = 1, TOTAL_BLOCK_COLUMN
                    IF (BLOCK_ARRAY(1)%MATRIX(J, I)%LENGTH_OF_STR .NE. 0) THEN
                        CHA_MATRIX%MATRIX(LINE_INDEX, ROW_COUNTER) = CORNER_CHA
                        LINE_INDEX = LINE_INDEX + 1
                        DO K = 1, BLOCK_WIDTH
                            CHA_MATRIX%MATRIX(LINE_INDEX, ROW_COUNTER) = BOUNDARY_CHA_HOR
                            LINE_INDEX = LINE_INDEX + 1
                        END DO
                        CHA_MATRIX%MATRIX(LINE_INDEX, ROW_COUNTER) = CORNER_CHA
                    ELSE
                        LINE_INDEX = LINE_INDEX + BLOCK_WIDTH + 1
                    END IF
                END DO
                ROW_COUNTER = ROW_COUNTER + 1
            END IF
            DO J = 1, BLOCK_HEIGHT
                IF ((J .EQ. 1 .OR. J .EQ. BLOCK_HEIGHT) .AND. MESH_ON) THEN
                    LINE_INDEX = LEFT_WIDTH + 1
                    DO K = 1, TOTAL_BLOCK_COLUMN
                        IF (BLOCK_ARRAY(1)%MATRIX(K, I)%LENGTH_OF_STR .NE. 0) THEN
                            CHA_MATRIX%MATRIX(LINE_INDEX, ROW_COUNTER) = BOUNDARY_CHA_VER
                            LINE_INDEX = LINE_INDEX + BLOCK_WIDTH + 1
                            CHA_MATRIX%MATRIX(LINE_INDEX, ROW_COUNTER) = BOUNDARY_CHA_VER
                        ELSE
                            LINE_INDEX = LINE_INDEX + BLOCK_WIDTH + 1
                        END IF
                    END DO
                ELSE
                    !FOR ROW INDEX
                    IF (J .EQ. 1 + INT(BLOCK_HEIGHT / 2)) THEN
                        CHA_MATRIX%MATRIX(LEFT_WIDTH - 1, ROW_COUNTER) = ROW_INDEX(I:I)
                    END IF
                    !FOR MESH BOUNDARY
                    LINE_INDEX = LEFT_WIDTH + 1
                    DO K = 1, TOTAL_BLOCK_COLUMN
                        IF (BLOCK_ARRAY(1)%MATRIX(K, I)%LENGTH_OF_STR .NE. 0) THEN
                            CHA_MATRIX%MATRIX(LINE_INDEX, ROW_COUNTER) = BOUNDARY_CHA_VER
                            LINE_INDEX = LINE_INDEX + BLOCK_WIDTH + 1
                            CHA_MATRIX%MATRIX(LINE_INDEX, ROW_COUNTER) = BOUNDARY_CHA_VER
                        ELSE
                            LINE_INDEX = LINE_INDEX + BLOCK_WIDTH + 1
                        END IF
                    END DO

                    !FOR CONTENT
                    IF (MESH_ON) THEN
                        LINE_INDEX = LEFT_WIDTH + 1 + 1
                        DO K = 1, TOTAL_BLOCK_COLUMN
                            DO L = 1, BLOCK_ARRAY(J - 1)%MATRIX(K, I)%LENGTH_OF_STR   
                                CHA_MATRIX%MATRIX(LINE_INDEX + L, ROW_COUNTER) = BLOCK_ARRAY(J - 1)%MATRIX(K, I)%STR(L)
                            END DO
                            LINE_INDEX = LINE_INDEX + BLOCK_WIDTH + 1
                        END DO 
                    ELSE
                        LINE_INDEX = LEFT_WIDTH + 1
                        DO K = 1, TOTAL_BLOCK_COLUMN
                            DO L = 1, BLOCK_ARRAY(J)%MATRIX(K, I)%LENGTH_OF_STR   
                                CHA_MATRIX%MATRIX(LINE_INDEX + L, ROW_COUNTER) = BLOCK_ARRAY(J)%MATRIX(K, I)%STR(L)
                            END DO
                            LINE_INDEX = LINE_INDEX + BLOCK_WIDTH + 1
                            IF (.NOT. MESH_ON) THEN
                                CHA_MATRIX%MATRIX(LINE_INDEX, ROW_COUNTER) = SEPERATOR
                            END IF
                        END DO 
                    END IF               
                END IF

                ROW_COUNTER = ROW_COUNTER + 1
            END DO
            
            !ADD FLOOR
            IF (DIMENSION_PER_BLOCK > 1) THEN
                LINE_INDEX = LEFT_WIDTH + 1
                DO J = 1, TOTAL_BLOCK_COLUMN
                    IF (BLOCK_ARRAY(1)%MATRIX(J, I)%LENGTH_OF_STR .NE. 0) THEN
                        CHA_MATRIX%MATRIX(LINE_INDEX, ROW_COUNTER) = CORNER_CHA
                        LINE_INDEX = LINE_INDEX + 1
                        DO K = 1, BLOCK_WIDTH
                            CHA_MATRIX%MATRIX(LINE_INDEX, ROW_COUNTER) = BOUNDARY_CHA_HOR
                            LINE_INDEX = LINE_INDEX + 1
                        END DO
                        CHA_MATRIX%MATRIX(LINE_INDEX, ROW_COUNTER) = CORNER_CHA
                    ELSE
                        LINE_INDEX = LINE_INDEX + BLOCK_WIDTH + 1
                    END IF
                END DO
            END IF
        END DO
        
    END SUBROUTINE CORE_VIEW_RADIAL_ASSEMBLY
    
    SUBROUTINE TABLE_VIEW(TAB_DATA, CHA_MATRIX)
        IMPLICIT NONE
        TYPE(TABLE),                  INTENT(IN)     :: TAB_DATA
        TYPE(MATRIX_OF_CHA), POINTER, INTENT(IN OUT) :: CHA_MATRIX

        INTEGER(NM_INT)   :: I, J, K, L, M
        INTEGER(NM_INT)   :: IDX_1, IDX_2
        INTEGER(NM_INT)   :: TABLE_HEAD_HEIGHT
        LOGICAL(NM_INT)   :: IS_END, LEVEL_END
        INTEGER(NM_INT)   :: TEMP_INT
        CHARACTER(LEN=200) :: CELL_CONTENT
        CHARACTER(LEN=20)  :: FMT_STR
        CHARACTER          :: FMT_C
        TYPE(COLUMN_NODE), POINTER :: NODE_HEAD
        TYPE(COLUMN_NODE), POINTER :: NODE_INDEX
        
        FMT_STR = ' '
        IF (.NOT. ASSOCIATED(CHA_MATRIX)) THEN
            ALLOCATE(CHA_MATRIX)
        END IF
        CHA_MATRIX%ROW = TAB_DATA%TABLE_PRT_HEIGHT
        CHA_MATRIX%COLUMN = TAB_DATA%TABLE_PRT_WIDTH
        IF (ALLOCATED(CHA_MATRIX%MATRIX)) THEN
            DEALLOCATE(CHA_MATRIX%MATRIX)
        END IF
        ALLOCATE(CHA_MATRIX%MATRIX(CHA_MATRIX%COLUMN, CHA_MATRIX%ROW))
        CHA_MATRIX%MATRIX = ' '
        !FOR TABLE HEAD
        TABLE_HEAD_HEIGHT = 0

        ALLOCATE(NODE_HEAD)

        NODE_HEAD%COLUMNS => TAB_DATA%COLUMN_POINTER
        NODE_HEAD%COLUMN_NUMBER = TAB_DATA%COLUMN_NUMBER
        NODE_HEAD%INDEX_IN_LEVEL = 1 
        NODE_HEAD%UPPER_LEVEL => NODE_HEAD

        IS_END = .FALSE.
        NODE_INDEX => NODE_HEAD
        I = NODE_INDEX%INDEX_IN_LEVEL
        

        
        DO WHILE (.NOT. IS_END)
            ! TITLE FOR EACH COLUMN
            DO J = 1, NODE_INDEX%COLUMNS(I)%TITLE_ROW_NUMBER
            	IDX_1 = INT(NODE_INDEX%COLUMNS(I)%COLUMN_PRT_WIDTH - NODE_INDEX%COLUMNS(I)%TITLE_POINTER(J)%LENGTH_OF_STR)/2 &
            		+ NODE_INDEX%COLUMNS(I)%COLUMN_FROM
            	IDX_2 = NODE_INDEX%COLUMNS(I)%TITLE_ROW_FROM - 1 + J
                DO K = 1, NODE_INDEX%COLUMNS(I)%TITLE_POINTER(J)%LENGTH_OF_STR
                    CHA_MATRIX%MATRIX(IDX_1, IDX_2) = NODE_INDEX%COLUMNS(I)%TITLE_POINTER(J)%STR(K)
                	IDX_1 = IDX_1 + 1
                END DO
            END DO
            DO K = 1, NODE_INDEX%COLUMNS(I)%COLUMN_PRT_WIDTH

                CHA_MATRIX%MATRIX(NODE_INDEX%COLUMNS(I)%COLUMN_FROM - 1 + K, &
                    NODE_INDEX%COLUMNS(I)%TITLE_ROW_TO + 1) = TAB_DATA%ROW_SEPERATOR
            END DO

            IF (NODE_INDEX%COLUMNS(I)%SUB_COLUMN_NUMBER .GT. 1) THEN
                !ONLY FOR SUBCOLUMNS
                NODE_INDEX%INDEX_IN_LEVEL = I + 1
                ALLOCATE(NODE_INDEX%LOWER_LEVEL)
                NODE_INDEX%LOWER_LEVEL%UPPER_LEVEL => NODE_INDEX 
                NODE_INDEX%LOWER_LEVEL%COLUMN_NUMBER = NODE_INDEX%COLUMNS(I)%SUB_COLUMN_NUMBER
                NODE_INDEX%LOWER_LEVEL%COLUMNS => NODE_INDEX%COLUMNS(I)%SUB_COLUMN_POINTER
                NODE_INDEX%LOWER_LEVEL%INDEX_IN_LEVEL = 1
                NODE_INDEX => NODE_INDEX%LOWER_LEVEL
                I = NODE_INDEX%INDEX_IN_LEVEL
            ELSE
                !PRINT CONTENT
                FMT_STR = ''
                IF (NODE_INDEX%COLUMNS(I)%FMT_LEN .GT. 0) THEN
                    DO M = 1, NODE_INDEX%COLUMNS(I)%FMT_LEN
                        FMT_STR(M : M) = NODE_INDEX%COLUMNS(I)%FMT(M)
                    END DO
                    FMT_C = FMT_STR(1 : 1)
                END IF

                ! FOR INTEGER DATA
                IF (ASSOCIATED(NODE_INDEX%COLUMNS(I)%DATA_VEC%INT_PTR)) THEN
                    DO J = 1, NODE_INDEX%COLUMNS(I)%DATA_VEC%DATA_LENGTH
                        WRITE(CELL_CONTENT, FMT_STR) NODE_INDEX%COLUMNS(I)%DATA_VEC%INT_PTR(J)
                        DO K = 1, NODE_INDEX%COLUMNS(I)%COLUMN_PRT_WIDTH
                            CHA_MATRIX%MATRIX(NODE_INDEX%COLUMNS(I)%COLUMN_FROM - 1 + K, &
                                NODE_INDEX%COLUMNS(I)%CONTENT_ROW_FROM - 1 + J) = &
                                CELL_CONTENT(K : K)
                        END DO
                    END DO
                ! FOR FLOAT DATA
                ELSE IF (ASSOCIATED(NODE_INDEX%COLUMNS(I)%DATA_VEC%REL_PTR)) THEN
                    DO J = 1, NODE_INDEX%COLUMNS(I)%DATA_VEC%DATA_LENGTH
                        WRITE(CELL_CONTENT, FMT_STR) NODE_INDEX%COLUMNS(I)%DATA_VEC%REL_PTR(J)
                        DO K = 1, NODE_INDEX%COLUMNS(I)%COLUMN_PRT_WIDTH
                            CHA_MATRIX%MATRIX(NODE_INDEX%COLUMNS(I)%COLUMN_FROM - 1 + K, &
                                NODE_INDEX%COLUMNS(I)%CONTENT_ROW_FROM - 1 + J) = &
                                CELL_CONTENT(K : K)
                        END DO
                    END DO
                ! FOR CHARACTER DATA
                ELSE
                    DO J = 1, NODE_INDEX%COLUMNS(I)%DATA_VEC%DATA_LENGTH
                    	L = NODE_INDEX%COLUMNS(I)%DATA_VEC%STR_PTR(J)%LENGTH_OF_STR
                    	IF (NODE_INDEX%COLUMNS(I)%FMT_LEN .GT. 0 .AND. FMT_C .EQ. 'L') THEN
                        	DO K = 1, L
                            	CHA_MATRIX%MATRIX(NODE_INDEX%COLUMNS(I)%COLUMN_FROM - 1 + K, &
                                	NODE_INDEX%COLUMNS(I)%CONTENT_ROW_FROM - 1 + J) = &
                                	NODE_INDEX%COLUMNS(I)%DATA_VEC%STR_PTR(J)%STR(K)
                        	END DO
                        ELSE IF (NODE_INDEX%COLUMNS(I)%FMT_LEN .GT. 0 .AND. FMT_C .EQ. 'M') THEN
                        	M = INT((NODE_INDEX%COLUMNS(I)%COLUMN_PRT_WIDTH - &
                        		NODE_INDEX%COLUMNS(I)%DATA_VEC%STR_PTR(J)%LENGTH_OF_STR) / 2)
                        	DO K = 1, L
                            	CHA_MATRIX%MATRIX(NODE_INDEX%COLUMNS(I)%COLUMN_FROM - 1 + K + M, &
                                	NODE_INDEX%COLUMNS(I)%CONTENT_ROW_FROM - 1 + J) = &
                                	NODE_INDEX%COLUMNS(I)%DATA_VEC%STR_PTR(J)%STR(K)
                        	END DO
                        ELSE ! DEFAULT IS FOR RIGTH JUSTICE
                        	DO K = 1, L
                            	CHA_MATRIX%MATRIX(NODE_INDEX%COLUMNS(I)%COLUMN_TO + 1 - K, &
                                	NODE_INDEX%COLUMNS(I)%CONTENT_ROW_FROM - 1 + J) = &
                                	NODE_INDEX%COLUMNS(I)%DATA_VEC%STR_PTR(J)%STR(L + 1 - K)
                        	END DO
                        END IF
                    END DO
                END IF
                I = I + 1
            END IF

            LEVEL_END = .FALSE.
            DO WHILE (.NOT. LEVEL_END)
                IF (ASSOCIATED(NODE_INDEX, NODE_HEAD)) THEN
                    IF (I .GT. NODE_INDEX%COLUMN_NUMBER) THEN
                        IS_END = .TRUE.
                    END IF
                    LEVEL_END = .TRUE.
                ELSE
                    IF (I .GT. NODE_INDEX%COLUMN_NUMBER) THEN
                        NODE_INDEX => NODE_INDEX%UPPER_LEVEL
                        I = NODE_INDEX%INDEX_IN_LEVEL
                        IF (I .GT. NODE_INDEX%COLUMN_NUMBER) THEN
                            LEVEL_END = .FALSE.
                        ELSE
                            LEVEL_END = .TRUE.
                        END IF
                    ELSE
                        LEVEL_END = .TRUE.
                    END IF
                END IF
            END DO
        END DO

    END SUBROUTINE TABLE_VIEW
    
END MODULE DRAW