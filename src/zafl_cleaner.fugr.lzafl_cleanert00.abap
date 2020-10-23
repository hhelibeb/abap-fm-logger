*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2020/10/23 at 17:33:12
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZAFL_CLEANER....................................*
DATA:  BEGIN OF STATUS_ZAFL_CLEANER                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFL_CLEANER                  .
CONTROLS: TCTRL_ZAFL_CLEANER
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZAFL_CLEANER                  .
TABLES: ZAFL_CLEANER                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
