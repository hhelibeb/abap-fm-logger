*&---------------------------------------------------------------------*
*& Report ZAFL_HISTORY_CLEANER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zafl_history_cleaner.

DATA: _log TYPE zafl_log.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t00.
SELECT-OPTIONS: s_fm FOR _log-fname NO INTERVALS.
PARAMETERS: p_check TYPE xfeld DEFAULT 'X'.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(45) TEXT-t01.
SELECTION-SCREEN END OF BLOCK b2.

START-OF-SELECTION.

  SELECT fname, retention_days, special_status
    FROM zafl_cleaner
    WHERE fname   IN @s_fm
      AND enabled  = 'X'
    INTO TABLE @DATA(configs).

  DATA: count TYPE int4.
  DATA: ts_cond TYPE timestamp.
  DATA: s_status TYPE RANGE OF zafl_log-status.

  GET TIME STAMP FIELD DATA(ts).

  LOOP AT configs ASSIGNING FIELD-SYMBOL(<config>).

    ts_cond = cl_abap_tstmp=>subtractsecs( tstmp = ts secs = <config>-retention_days * 86400 ).

    IF <config>-special_status IS NOT INITIAL.
      s_status = VALUE #( ( sign = 'I' option = 'EQ' low = <config>-special_status ) ).
    ENDIF.

    SELECT COUNT( * ) FROM zafl_log
      WHERE fname      = @<config>-fname
        AND status    IN @s_status
        AND timestamp  < @ts_cond
      INTO @DATA(count_temp).

    count = count + count_temp.

    IF p_check = abap_false.
      DELETE FROM zafl_log WHERE fname      = @<config>-fname
                             AND status    IN @s_status
                             AND timestamp  < @ts_cond.
      COMMIT WORK.
    ENDIF.

  ENDLOOP.

  DATA(msg) = COND #( WHEN p_check = abap_true THEN |{ count } records found|
                      ELSE                          |{ count } records deleted|
  ).

  MESSAGE msg TYPE 'S'.
