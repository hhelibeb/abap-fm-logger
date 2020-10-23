*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZAFL_CLEANER
*   generation date: 2020/10/23 at 17:33:11
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZAFL_CLEANER       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
