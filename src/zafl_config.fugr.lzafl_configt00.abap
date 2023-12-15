*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZAFL_CONFIG.....................................*
DATA:  BEGIN OF STATUS_ZAFL_CONFIG                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFL_CONFIG                   .
CONTROLS: TCTRL_ZAFL_CONFIG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZAFL_CONFIG                   .
TABLES: ZAFL_CONFIG                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
