MODULE OUTPUT_PARAMETERS
USE BASIC_DATA
IMPLICIT NONE
PRIVATE

PUBLIC :: LABLE_0001,   LABLE_0002,   LABLE_0003,   LABLE_0004,   LABLE_0005,   &
          LABLE_0006,   LABLE_0007,   LABLE_0008,   LABLE_0009,   LABLE_0010,   &
          LABLE_0011,   LABLE_0012,   LABLE_0013,   LABLE_0014,   LABLE_0015,   &
          LABLE_0016,   LABLE_0017,   LABLE_0018,   LABLE_0019,   LABLE_0020_1, &
          LABLE_0020_2, LABLE_0021,   LABLE_0022_1, LABLE_0022_2, LABLE_0023,   &
          LABLE_0024,   LABLE_0025,   LABLE_0025_1, LABLE_0025_2, LABLE_0026,   &
          LABLE_0026_1, LABLE_0026_2, LABLE_0026_3, LABLE_0027,   LABLE_0028_1, &
          LABLE_0028_2, LABLE_0028_3, LABLE_0029_1, LABLE_0029_2, LABLE_0030_1, &
          LABLE_0030_2, LABLE_0030_3, LABLE_0031_2, LABLE_0032_2, LABLE_0033_2, &
          LABLE_0034_2

PUBLIC :: PARANAME_0001, PARANAME_0002, PARANAME_0003

PUBLIC :: STDVALUE_0001, STDVALUE_0002, STDVALUE_0003

PUBLIC :: SMALL_LOGO, SMALL_LOGO_ROW, SMALL_LOGO_COLUMN, TITLE_ITEM_NUM

PUBLIC :: LARGE_LOGO, LARGE_LOGO_ROW, LARGE_LOGO_COLUMN

PUBLIC :: CODE_VERSION

PUBLIC :: INITIAL_PARAMETERS

PUBLIC :: CASE_CD_READ_ITEM_NUM, CASE_CD_READ_01, CASE_CD_READ_02, CASE_CD_READ_03, CASE_CD_READ_04, &
          CASE_CD_READ_STR_01
          
PUBLIC :: CASE_FUEL_COMP_MAT_DIM, CASE_FUEL_COMP_01, CASE_FUEL_COMP_STR_01

PUBLIC :: CASE_FUEL_INST_FA_ITEM_NUM
PUBLIC :: CASE_FUEL_INST_FA_01, CASE_FUEL_INST_FA_02, CASE_FUEL_INST_FA_03, CASE_FUEL_INST_FA_04, &
          CASE_FUEL_INST_FA_05, CASE_FUEL_INST_FA_06, CASE_FUEL_INST_FA_07, CASE_FUEL_INST_FA_08, &
          CASE_FUEL_INST_FA_09, CASE_FUEL_INST_FA_10, CASE_FUEL_INST_FA_11, CASE_FUEL_INST_FA_12, &
          CASE_FUEL_INST_FA_13, CASE_FUEL_INST_FA_14
PUBLIC :: CASE_FUEL_INST_SUM_STR_01, CASE_FUEL_INST_SUM_STR_02, CASE_FUEL_INST_SUM_STR_03, CASE_FUEL_INST_SUM_STR_04
          
PUBLIC :: TABLE_COLUMN_GAP_WIDTH

CHARACTER(LEN=*), PARAMETER :: LABLE_0001   = 'core maps:'
CHARACTER(LEN=*), PARAMETER :: LABLE_0002   = 'fuel:'
CHARACTER(LEN=*), PARAMETER :: LABLE_0003   = 'control_rod:'
CHARACTER(LEN=*), PARAMETER :: LABLE_0004   = 'control_rod_group:'
CHARACTER(LEN=*), PARAMETER :: LABLE_0005   = 'CORE MAPS'
CHARACTER(LEN=*), PARAMETER :: LABLE_0006   = '/core maps'
CHARACTER(LEN=*), PARAMETER :: LABLE_0007   = '/fuel'
CHARACTER(LEN=*), PARAMETER :: LABLE_0008   = '/control_rod'
CHARACTER(LEN=*), PARAMETER :: LABLE_0009   = '/control_rod_group'
CHARACTER(LEN=*), PARAMETER :: LABLE_0010   = 'model_symmetry:'
CHARACTER(LEN=*), PARAMETER :: LABLE_0011   = '/model_symmetry'
CHARACTER(LEN=*), PARAMETER :: LABLE_0012   = 'Version'
CHARACTER(LEN=*), PARAMETER :: LABLE_0013   = 'Job'
CHARACTER(LEN=*), PARAMETER :: LABLE_0014   = 'Date/Time'
CHARACTER(LEN=*), PARAMETER :: LABLE_0015   = 'Case'
CHARACTER(LEN=*), PARAMETER :: LABLE_0016   = 'Ref Time'
CHARACTER(LEN=*), PARAMETER :: LABLE_0017   = 'Ref BU'
CHARACTER(LEN=*), PARAMETER :: LABLE_0018   = 'Title'
CHARACTER(LEN=*), PARAMETER :: LABLE_0019   = 'No.'
CHARACTER(LEN=*), PARAMETER :: LABLE_0020_1 = 'File Size'
CHARACTER(LEN=*), PARAMETER :: LABLE_0020_2 = '(Bytes)'
CHARACTER(LEN=*), PARAMETER :: LABLE_0021   = 'Last Modified'
CHARACTER(LEN=*), PARAMETER :: LABLE_0022_1 = 'AUTOLINK'
CHARACTER(LEN=*), PARAMETER :: LABLE_0022_2 = 'Ver.'
CHARACTER(LEN=*), PARAMETER :: LABLE_0023   = 'File Name'
CHARACTER(LEN=*), PARAMETER :: LABLE_0024   = 'Loc.'
CHARACTER(LEN=*), PARAMETER :: LABLE_0025   = 'Assembly'
CHARACTER(LEN=*), PARAMETER :: LABLE_0025_1 = 'ID'
CHARACTER(LEN=*), PARAMETER :: LABLE_0025_2 = 'TYPE'
CHARACTER(LEN=*), PARAMETER :: LABLE_0026   = 'Previous Loc.'
CHARACTER(LEN=*), PARAMETER :: LABLE_0026_1 = 'I-J'
CHARACTER(LEN=*), PARAMETER :: LABLE_0026_2 = 'PlantID'
CHARACTER(LEN=*), PARAMETER :: LABLE_0026_3 = 'Cycle'
CHARACTER(LEN=*), PARAMETER :: LABLE_0027   = 'Shuffle'
CHARACTER(LEN=*), PARAMETER :: LABLE_0028_1 = 'Average'
CHARACTER(LEN=*), PARAMETER :: LABLE_0028_2 = 'Burnup'
CHARACTER(LEN=*), PARAMETER :: LABLE_0028_3 = '(MWD/MTM)'
CHARACTER(LEN=*), PARAMETER :: LABLE_0029_1 = 'Maximum'
CHARACTER(LEN=*), PARAMETER :: LABLE_0029_2 = 'Rod BU'
CHARACTER(LEN=*), PARAMETER :: LABLE_0030_1 = 'Residence'
CHARACTER(LEN=*), PARAMETER :: LABLE_0030_2 = 'Time'
CHARACTER(LEN=*), PARAMETER :: LABLE_0030_3 = '(days)'
CHARACTER(LEN=*), PARAMETER :: LABLE_0031_2 = 'IFBA'
CHARACTER(LEN=*), PARAMETER :: LABLE_0032_2 = 'FRAC.'
CHARACTER(LEN=*), PARAMETER :: LABLE_0033_2 = 'Gad'
CHARACTER(LEN=*), PARAMETER :: LABLE_0034_2 = 'Erb'

CHARACTER(LEN=*), PARAMETER :: PARANAME_0001 = 'input'
CHARACTER(LEN=*), PARAMETER :: PARANAME_0002 = 'solution'
CHARACTER(LEN=*), PARAMETER :: PARANAME_0003 = 'output'

CHARACTER(LEN=*), PARAMETER :: STDVALUE_0001 = 'full'
CHARACTER(LEN=*), PARAMETER :: STDVALUE_0002 = '1/4'
CHARACTER(LEN=*), PARAMETER :: STDVALUE_0003 = '1/8'     

INTEGER(NM_INT), PARAMETER :: SMALL_LOGO_ROW = 4
INTEGER(NM_INT), PARAMETER :: SMALL_LOGO_COLUMN = 30
CHARACTER(LEN=*), PARAMETER :: SMALL_LOGO  = '  ___  _  _  ___    __    ___ ' &
                                          // ' / __)( \/ )/ __)  /__\  / __)' &
                                          // '( (__  \  /( (__  /(__)\ \__ \' &
                                          // ' \___) (__) \___)(__)(__)(___/'
                                          
INTEGER(NM_INT), PARAMETER :: LARGE_LOGO_ROW = 11
INTEGER(NM_INT), PARAMETER :: LARGE_LOGO_COLUMN = 68                                          
CHARACTER(LEN=*), PARAMETER :: LARGE_LOGO  = '      ___                       ___           ___           ___     ' &
                                          // '     /\__\                     /\__\         /\  \         /\__\    ' &  
                                          // '    /:/  /          ___       /:/  /        /::\  \       /:/ _/_   ' &
                                          // '   /:/  /          /|  |     /:/  /        /:/\:\  \     /:/ /\  \  ' &
                                          // '  /:/  /  ___     |:|  |    /:/  /  ___   /:/ /::\  \   /:/ /::\  \ ' &
                                          // ' /:/__/  /\__\    |:|  |   /:/__/  /\__\ /:/_/:/\:\__\ /:/_/:/\:\__\' &
                                          // ' \:\  \ /:/  /  __|:|__|   \:\  \ /:/  / \:\/:/  \/__/ \:\/:/ /:/  /' &
                                          // '  \:\  /:/  /  /::::\  \    \:\  /:/  /   \::/__/       \::/ /:/  / ' &
                                          // '   \:\/:/  /   ~~~~\:\  \    \:\/:/  /     \:\  \        \/_/:/  /  ' &
                                          // '    \::/  /         \:\__\    \::/  /       \:\__\         /:/  /   ' &
                                          // '     \/__/           \/__/     \/__/         \/__/         \/__/    '                                          
                                          
                                          
                                          
INTEGER(NM_INT), PARAMETER :: TITLE_ITEM_NUM = 7                                          

CHARACTER(LEN=*), PARAMETER :: CODE_VERSION = '0.9.8'

CHARACTER(LEN=*), PARAMETER :: CA_MOD_001 = ' '

INTEGER(NM_INT)             :: CASE_CD_READ_ITEM_NUM = 0    ! NUMBER OF TOTAL ITEMS IN THE TABLE
INTEGER(NM_INT),    POINTER :: CASE_CD_READ_01(:) => NULL() ! FOR FILE SIZE ARRAY
TYPE(ARRAY_OF_STR), POINTER :: CASE_CD_READ_02(:) => NULL() ! FOR LAST MODIFIED TIME ARRAY
TYPE(ARRAY_OF_STR), POINTER :: CASE_CD_READ_03(:) => NULL() ! FOR AUTOLINK VERSION
TYPE(ARRAY_OF_STR), POINTER :: CASE_CD_READ_04(:) => NULL() ! FOR FILE NAME 
CHARACTER(LEN=*), PARAMETER :: CASE_CD_READ_STR_01 = 'Cross Section Data Files Read' 

INTEGER(NM_INT)             :: CASE_FUEL_COMP_MAT_DIM = 0     ! NUMBER OF ELEMENTS PER CELL
TYPE(STR_MATRIX),   POINTER :: CASE_FUEL_COMP_01(:) => NULL() ! CELL CONTENT ARRAY
CHARACTER(LEN=*), PARAMETER :: CASE_FUEL_COMP_STR_01 = 'Fuel Composition and Orientation' 

INTEGER(NM_INT)             :: CASE_FUEL_INST_FA_ITEM_NUM = 0      ! NUMBER OF TOTAL ITEMS IN FUEL ASSEMBLY PLACEMENT
TYPE(ARRAY_OF_STR), POINTER :: CASE_FUEL_INST_FA_01(:) => NULL()   ! FOR Loc.
TYPE(ARRAY_OF_STR), POINTER :: CASE_FUEL_INST_FA_02(:) => NULL()   ! FOR ASSEMBLY ID
TYPE(ARRAY_OF_STR), POINTER :: CASE_FUEL_INST_FA_03(:) => NULL()   ! FOR ASSEMBLY TYPE
TYPE(ARRAY_OF_STR), POINTER :: CASE_FUEL_INST_FA_04(:) => NULL()   ! FOR Previous Loc. I-J 
TYPE(ARRAY_OF_STR), POINTER :: CASE_FUEL_INST_FA_05(:) => NULL()   ! FOR Previous Loc. Plant ID 
TYPE(ARRAY_OF_STR), POINTER :: CASE_FUEL_INST_FA_06(:) => NULL()   ! FOR Previous Loc. Cycle
TYPE(ARRAY_OF_STR), POINTER :: CASE_FUEL_INST_FA_07(:) => NULL()   ! FOR Shfl Cat
REAL(DP),           POINTER :: CASE_FUEL_INST_FA_08(:) => NULL()   ! FOR Average Burnup (MWD/MTM)
REAL(DP),           POINTER :: CASE_FUEL_INST_FA_09(:) => NULL()   ! FOR Maximum Rod BU (MWD/MTM)
REAL(DP),           POINTER :: CASE_FUEL_INST_FA_10(:) => NULL()   ! FOR Residence Time (days)
INTEGER(NM_INT),    POINTER :: CASE_FUEL_INST_FA_11(:) => NULL()   ! FOR No. IFBA
REAL(DP),           POINTER :: CASE_FUEL_INST_FA_12(:) => NULL()   ! FOR IFBA Frac.
INTEGER(NM_INT),    POINTER :: CASE_FUEL_INST_FA_13(:) => NULL()   ! FOR No. Gad
INTEGER(NM_INT),    POINTER :: CASE_FUEL_INST_FA_14(:) => NULL()   ! FOR No. Erb
CHARACTER(LEN=*), PARAMETER :: CASE_FUEL_INST_SUM_STR_01 = 'Fuel and Insert Placement Summary' 
CHARACTER(LEN=*), PARAMETER :: CASE_FUEL_INST_SUM_STR_02 = 'Fuel Assembly Placement'
CHARACTER(LEN=*), PARAMETER :: CASE_FUEL_INST_SUM_STR_03 = 'Depletable Insert Placement'
CHARACTER(LEN=*), PARAMETER :: CASE_FUEL_INST_SUM_STR_04 = 'Control Rod Placement'





INTEGER(NM_INT), PARAMETER :: TABLE_COLUMN_GAP_WIDTH = 2


CONTAINS

SUBROUTINE INITIAL_PARAMETERS()
    IMPLICIT NONE
    ! FOR NOW NOTHING TO DO
END SUBROUTINE INITIAL_PARAMETERS

END MODULE OUTPUT_PARAMETERS