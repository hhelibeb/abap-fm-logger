*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZAFL_CONFIG
*   generation date: 2020.08.06 at 19:33:12
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZAFL_CONFIG        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
