REPORT zafl_viewer.

DATA: _log TYPE zafl_log.

INCLUDE zafl_macros.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t00.

SELECT-OPTIONS: s_fm FOR _log-fname NO INTERVALS NO-EXTENSION.
SELECT-OPTIONS: s_guid FOR _log-guid.
SELECT-OPTIONS: s_cf1 FOR _log-cust_field1.
SELECT-OPTIONS: s_cf2 FOR _log-cust_field2.
SELECT-OPTIONS: s_cf3 FOR _log-cust_field3.
SELECT-OPTIONS: s_status FOR _log-status NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t01.
SELECTION-SCREEN BEGIN OF LINE .
SELECTION-SCREEN COMMENT 1(20) TEXT-t02 FOR FIELD p_dstart.
PARAMETERS: p_dstart TYPE edidc-upddat DEFAULT sy-datum.
SELECTION-SCREEN COMMENT 35(20) TEXT-t03 FOR FIELD p_dend.
PARAMETERS: p_dend TYPE edidc-upddat.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE .
SELECTION-SCREEN COMMENT 1(20) TEXT-t04 FOR FIELD p_tstart.
PARAMETERS: p_tstart TYPE edidc-updtim DEFAULT '000000'.
SELECTION-SCREEN COMMENT 35(20) TEXT-t05 FOR FIELD p_tend.
PARAMETERS: p_tend TYPE edidc-updtim DEFAULT '235959'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b2.

* Block: Number Of Hits.
SELECTION-SCREEN BEGIN OF BLOCK no_of_hits WITH FRAME TITLE TEXT-t06.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) FOR FIELD pv_nofhs.
PARAMETERS pv_nofhs TYPE i DEFAULT 1000 MODIF ID nh2.
SELECTION-SCREEN POSITION 40.
PARAMETERS pc_nhnl AS CHECKBOX USER-COMMAND nh1.
SELECTION-SCREEN COMMENT 47(30) TEXT-t07 FOR FIELD pc_nhnl.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK no_of_hits.

TYPES: ty_time_cond TYPE RANGE OF timestamp.

TYPES BEGIN OF ty_log.
INCLUDE TYPE zafl_log.
TYPES date TYPE sy-datum.
TYPES time TYPE sy-uzeit.
TYPES time_zone TYPE sy-zonlo.
TYPES END OF ty_log.

DATA: gt_log TYPE STANDARD TABLE OF ty_log.
DATA: gr_alv TYPE REF TO cl_salv_table.

CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,

      on_link_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column.
ENDCLASS.

CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_user_command.
    PERFORM handle_user_command USING e_salv_function.
  ENDMETHOD.                    "on_user_command
  "on_double_click

  METHOD on_link_click.

    DATA: value TYPE string.

    READ TABLE gt_log INDEX row ASSIGNING FIELD-SYMBOL(<row>).
    IF sy-subrc = 0.
      ASSIGN COMPONENT column OF STRUCTURE <row> TO FIELD-SYMBOL(<value>).
      IF sy-subrc = 0.
        value = <value>.
      ENDIF.
    ENDIF.

    IF column = 'FNAME'.

      DATA(fm_name) = CONV rs38l_fnam( value ).
      SET PARAMETER ID 'LIB' FIELD fm_name.
      CALL TRANSACTION 'SE37' AND SKIP FIRST SCREEN.

    ELSE.

      cl_demo_output=>display_json( <value> ).

    ENDIF.

  ENDMETHOD.                    "on_single_click
ENDCLASS.


INITIALIZATION.


START-OF-SELECTION.

  DATA(gr_events) = NEW lcl_handle_events( ).

  PERFORM get_data.

  PERFORM display.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.


  DATA: s_ts TYPE ty_time_cond.

  PERFORM get_time_cond USING p_dstart
                              p_dend
                              p_tstart
                              p_tend
                              s_ts.

  SELECT * FROM zafl_log
    WHERE guid        IN @s_guid
      AND fname       IN @s_fm
      AND cust_field1 IN @s_cf1
      AND cust_field2 IN @s_cf2
      AND cust_field3 IN @s_cf3
      AND status      IN @s_status
      AND timestamp   IN @s_ts
    *INTO TABLE @gt_log
    INTO CORRESPONDING FIELDS OF TABLE @gt_log
    UP TO @pv_nofhs ROWS.


  LOOP AT gt_log ASSIGNING FIELD-SYMBOL(<fs_log>)
                     WHERE timestamp IS NOT INITIAL.

    <fs_log>-time_zone = sy-zonlo.

    CONVERT TIME STAMP <fs_log>-timestamp
            TIME ZONE  <fs_log>-time_zone
       INTO DATE <fs_log>-date
            TIME <fs_log>-time.

  ENDLOOP.

ENDFORM.

FORM get_time_cond USING sdate  TYPE dats
                         edate  TYPE dats
                         stime  TYPE uzeit
                         etime  TYPE uzeit
                         result TYPE ty_time_cond.
  IF sdate IS INITIAL AND edate IS INITIAL.
    RETURN.
  ENDIF.

  DATA(start_date) = sdate.
  DATA(end_date) = edate.

  IF end_date IS INITIAL.
    end_date = start_date.
  ENDIF.

  IF start_date IS INITIAL.
    start_date = end_date.
  ENDIF.

  DATA(start_timestamp) = start_date && stime.
  DATA(end_timestamp)   = end_date && etime.

  result = VALUE #( sign = 'I' option = 'BT' (
      low  = start_timestamp
      high = end_timestamp
    )
  ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = gr_alv
        CHANGING
          t_table      = gt_log ).
    CATCH cx_salv_msg INTO DATA(lr_msg).
  ENDTRY.

  DATA(lr_cols) = CAST cl_salv_columns( gr_alv->get_columns( ) ).

  lr_cols->set_optimize( 'X' ).

  gr_alv->set_screen_status(
    pfstatus      =  'SALV_STANDARD'
    report        =  sy-repid
    set_functions = gr_alv->c_functions_all
  ).

  DATA(lr_selections) = gr_alv->get_selections( ).
  lr_selections->set_selection_mode( 3 ).

  DATA: lr_functions TYPE REF TO cl_salv_functions.

  IF zcl_afl_utilities=>get_distinct_count( tab_data = gt_log field_name = 'FNAME' ) = 1.
    SELECT SINGLE cust_name1, cust_name2, cust_name3 FROM zafl_config
      INTO @DATA(config).
  ENDIF.

  IF config IS INITIAL.
    config = VALUE #(
      cust_name1 = 'CUST_FIELD1'
      cust_name2 = 'CUST_FIELD2'
      cust_name3 = 'CUST_FIELD3'
    ).
  ENDIF.

  PERFORM set_column USING ''  lr_cols 'GUID'        'GUID' .
  PERFORM set_column USING 'X' lr_cols 'FNAME'       'Function Module' .
  PERFORM set_column USING ''  lr_cols 'CUST_FIELD1'  config-cust_name1.
  PERFORM set_column USING ''  lr_cols 'CUST_FIELD2'  config-cust_name2.
  PERFORM set_column USING ''  lr_cols 'CUST_FIELD3'  config-cust_name3.
  PERFORM set_column USING ''  lr_cols 'STATUS'      'Status Code' .
  PERFORM set_column USING ''  lr_cols 'TIMESTAMP'   'Timestamp' .
  PERFORM set_column USING ''  lr_cols 'DATE'        'Date' .
  PERFORM set_column USING ''  lr_cols 'TIME'        'Time' .
  PERFORM set_column USING ''  lr_cols 'TIME_ZONE'   'Zone' .  
  PERFORM set_column USING ''  lr_cols 'TIME_COST'   'Time Cost' .
  PERFORM set_column USING ''  lr_cols 'UNAME'       'User' .
  PERFORM set_column USING ''  lr_cols 'MESSAGE'     'Message' .
  PERFORM set_column USING 'X' lr_cols 'IMPORT'      'Import Data' .
  PERFORM set_column USING 'X' lr_cols 'EXPORT'      'Export Data' .
  PERFORM set_column USING 'X' lr_cols 'CHANGE_IN'   'Changing In' .
  PERFORM set_column USING 'X' lr_cols 'CHANGE_OUT'  'Changing Out' .
  PERFORM set_column USING 'X' lr_cols 'TABLE_IN'    'Tables In ' .
  PERFORM set_column USING 'X' lr_cols 'TABLE_OUT'   'Tables Out' .


  DATA(lr_events) = gr_alv->get_event( ).

  SET HANDLER gr_events->on_user_command FOR lr_events.

  SET HANDLER gr_events->on_link_click FOR lr_events.


  gr_alv->display( ).

ENDFORM.

FORM set_column  USING  i_hotspot TYPE xfeld
                        pr_cols TYPE REF TO cl_salv_columns
                        VALUE(fname)
                        VALUE(text).

  DATA: lr_column TYPE REF TO cl_salv_column_table.

  TRY.
      lr_column ?= pr_cols->get_column( fname ).
      lr_column->set_long_text( CONV #( text ) ).
      lr_column->set_medium_text( CONV #( text ) ).
      lr_column->set_short_text( CONV #( text ) ).
      IF i_hotspot = abap_true.
        lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      ENDIF.
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_SALV_FUNCTION  text
*----------------------------------------------------------------------*
FORM handle_user_command  USING  i_ucomm TYPE salv_de_function.

  CASE i_ucomm.
    WHEN 'PROCESS'.
      IF zcl_afl_utilities=>is_prd( ).
        DATA: ans TYPE c.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirm'(m01)
            text_question         = 'You have called an IDoc test transaction in a client flagged as "Productive".'(m02)
            text_button_1         = 'OK'
            icon_button_1         = 'ICON_CHECKED'
            text_button_2         = 'CANCEL'
            icon_button_2         = 'ICON_CANCEL'
            display_cancel_button = ' '
            popup_type            = 'ICON_MESSAGE_ERROR'
          IMPORTING
            answer                = ans.
        IF ans = 2.
          RETURN.
        ENDIF.
      ENDIF.
      PERFORM process_selected_rows.
    WHEN 'REFRESH'.
      PERFORM get_data.
      gr_alv->refresh( ).
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROCESS_SELECTED_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_selected_rows.

  DATA(lr_selections) = gr_alv->get_selections( ).
  DATA(lt_rows) = lr_selections->get_selected_rows( ).

  LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<row>).

    READ TABLE gt_log INDEX <row> ASSIGNING FIELD-SYMBOL(<log>).
    IF sy-subrc = 0.
      DATA(pass) = zcl_afl_utilities=>fm_authority_check( <log>-fname ).
      IF pass = abap_false.
        DATA(msg) = |You are not authorized to test function module { <log>-fname }|.
        MESSAGE msg TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ENDIF.

  ENDLOOP.


  LOOP AT lt_rows ASSIGNING <row>.

    READ TABLE gt_log INDEX <row> ASSIGNING <log>.
    IF sy-subrc = 0.
      zcl_afl_utilities=>re_process( <log>-guid ).
    ENDIF.

  ENDLOOP.

  msg = |{ lines( lt_rows ) } records processed|.

  MESSAGE msg TYPE 'S'.

ENDFORM.
