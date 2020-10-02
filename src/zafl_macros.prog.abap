*&---------------------------------------------------------------------*
*&  Include  ZAFL_MACROS
*&---------------------------------------------------------------------*
DEFINE /afl/log_init.

  DATA: /afl/comp_tab TYPE cl_abap_structdescr=>component_table,
        /afl/comp_wa  LIKE LINE OF /afl/comp_tab.
  DATA: /afl/struct_type    TYPE REF TO cl_abap_structdescr, "Structure
        /afl/parameter_data TYPE REF TO data.
  DATA: /afl/table_structure_type TYPE REF TO cl_abap_structdescr,
        /afl/table_type TYPE REF TO cl_abap_tabledescr.

  DATA: true_fieldname TYPE string.

  FIELD-SYMBOLS: </afl/parameter_data>       TYPE any,
                 </afl/parameter_data_field> TYPE any,
                 </afl/parameter>            TYPE any.


  DATA: /afl/callstack TYPE abap_callstack.

  DATA: /afl/log TYPE zafl_log.

  GET TIME.

  CALL FUNCTION 'SYSTEM_CALLSTACK'
    IMPORTING
      callstack = /afl/callstack.

  DATA(/afl/func_name) = VALUE #( /afl/callstack[ 1 ]-blockname OPTIONAL ).

    SELECT SINGLE * FROM zafl_config
      WHERE fname   = @/afl/func_name
        AND enabled = 'X'
      INTO @DATA(/afl/config).
  IF sy-subrc = 0.

    SELECT funcname, paramtype, pposition, parameter, structure
      FROM fupararef
      WHERE funcname = @/afl/func_name
      INTO TABLE @DATA(/afl/parameters_tab).
    IF sy-subrc = 0.

      SORT /afl/parameters_tab BY paramtype pposition.

      FIELD-SYMBOLS: </alf/parameters> LIKE LINE OF /afl/parameters_tab,
                     </alf/comp>       LIKE LINE OF /afl/comp_tab.

      IF /afl/config-import = abap_true.
        /afl/log_get_json 'I' /afl/log-import.
      ENDIF.
      IF /afl/config-export = abap_true.
        /afl/log_get_json 'E' /afl/log-export.
      ENDIF.

      IF /afl/config-table_in = abap_true.
        /afl/log_get_table_json /afl/log-table_in.
      ENDIF.

      DATA: /afl/start_time TYPE tzntstmpl.

      TRY.
          /afl/log-guid = cl_system_uuid=>create_uuid_x16_static( ).
        CATCH cx_uuid_error INTO DATA(/afl/oref).
      ENDTRY.

      GET TIME STAMP FIELD /afl/start_time.
      GET TIME STAMP FIELD /afl/log-timestamp.

      /afl/log = VALUE #( BASE /afl/log
        fname = /afl/func_name
        uname = sy-uname
      ).

    ENDIF.
  ENDIF.

END-OF-DEFINITION.

DEFINE /afl/log_get_json.

  CLEAR /afl/comp_tab.

  LOOP AT /afl/parameters_tab ASSIGNING </alf/parameters> WHERE paramtype = &1.
    /afl/comp_wa-name = </alf/parameters>-parameter.
    /afl/comp_wa-type ?= cl_abap_datadescr=>describe_by_name( </alf/parameters>-structure ).
    APPEND /afl/comp_wa TO /afl/comp_tab.
  ENDLOOP.

  /afl/struct_type = cl_abap_structdescr=>create( /afl/comp_tab ).

  CREATE DATA /afl/parameter_data TYPE HANDLE /afl/struct_type.

  ASSIGN /afl/parameter_data->* TO </afl/parameter_data>.

  LOOP AT /afl/comp_tab ASSIGNING </alf/comp>.
    ASSIGN (</alf/comp>-name) TO </afl/parameter>.
    ASSIGN COMPONENT </alf/comp>-name OF STRUCTURE </afl/parameter_data> TO </afl/parameter_data_field>.
    </afl/parameter_data_field> = </afl/parameter>.
  ENDLOOP.

  &2 = /ui2/cl_json=>serialize( data = </afl/parameter_data> ).

END-OF-DEFINITION.

DEFINE /afl/set_custom_fields.

  /afl/log = VALUE #( BASE /afl/log
    cust_field1 = &1
    cust_field2 = &2
    cust_field3 = &3
  ).

END-OF-DEFINITION.

DEFINE /afl/set_status .

  /afl/log = VALUE #( BASE /afl/log
    status  = &1
    message = &2
  ).

END-OF-DEFINITION.

DEFINE /afl/save .

  IF /afl/log-guid IS NOT INITIAL.

    DATA: /afl/end_time TYPE tzntstmpl.

    GET TIME.

    GET TIME STAMP FIELD /afl/end_time.

    /afl/log-time_cost = cl_abap_tstmp=>subtract( tstmp1 = /afl/end_time tstmp2 = /afl/start_time ).

    IF /afl/config-table_out = abap_true.
      /afl/log_get_table_json /afl/log-table_out.
    ENDIF.

    MODIFY zafl_log FROM @/afl/log.

    COMMIT WORK.

  ENDIF.


END-OF-DEFINITION.

DEFINE /afl/log_get_table_json.

  CLEAR /afl/comp_tab.

  LOOP AT /afl/parameters_tab ASSIGNING </alf/parameters> WHERE paramtype = 'T'.
    /afl/comp_wa-name = </alf/parameters>-parameter.
    /afl/table_structure_type = CAST cl_abap_structdescr( cl_abap_datadescr=>describe_by_name( </alf/parameters>-structure ) ).
    /afl/table_type = CAST cl_abap_tabledescr( cl_abap_tabledescr=>create( /afl/table_structure_type ) ).
    /afl/comp_wa-type ?= /afl/table_type.
    APPEND /afl/comp_wa TO /afl/comp_tab.
  ENDLOOP.

  /afl/struct_type = cl_abap_structdescr=>create( /afl/comp_tab ).

  CREATE DATA /afl/parameter_data TYPE HANDLE /afl/struct_type.

  ASSIGN /afl/parameter_data->* TO </afl/parameter_data>.

  LOOP AT /afl/comp_tab ASSIGNING </alf/comp>.
    true_fieldname = </alf/comp>-name && '[]'.
    ASSIGN (true_fieldname) TO </afl/parameter>.
    ASSIGN COMPONENT </alf/comp>-name OF STRUCTURE </afl/parameter_data> TO </afl/parameter_data_field>.
    </afl/parameter_data_field> = </afl/parameter>.
  ENDLOOP.

  &1 = /ui2/cl_json=>serialize( data = </afl/parameter_data> ).

END-OF-DEFINITION.
