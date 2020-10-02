CLASS zcl_afl_utilities DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS re_process
      IMPORTING
        !guid TYPE guid .
    CLASS-METHODS is_prd
      RETURNING VALUE(result) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AFL_UTILITIES IMPLEMENTATION.


  METHOD is_prd.

    DATA: role TYPE t000-cccategory.

    CALL FUNCTION 'TR_SYS_PARAMS'
      IMPORTING
        system_client_role = role
      EXCEPTIONS
        no_systemname      = 1
        no_systemtype      = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF role = 'P'.
      result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD re_process.

    DATA: data_ref TYPE REF TO data.

    SELECT SINGLE guid, fname, import, table_in
      FROM zafl_log
      WHERE guid = @guid
      INTO @DATA(record).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT funcname, paramtype, pposition, parameter, structure
      FROM fupararef
      WHERE funcname = @record-fname
      INTO TABLE @DATA(parameters_tab).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA: temp_dd04l TYPE STANDARD TABLE OF dd04l.
    LOOP AT parameters_tab ASSIGNING FIELD-SYMBOL(<ptab>).
      IF strlen( <ptab>-structure ) > 30.
        CONTINUE.
      ENDIF.
      temp_dd04l = VALUE #( BASE temp_dd04l ( domname = <ptab>-structure ) ).
    ENDLOOP.

    IF temp_dd04l IS NOT INITIAL.
      SELECT domname FROM dd04l
        FOR ALL ENTRIES IN @temp_dd04l
        WHERE domname = @temp_dd04l-domname
        INTO TABLE @DATA(data_elements).
    ENDIF.

    DATA: func      TYPE string,
          ptab      TYPE abap_func_parmbind_tab,
          ptab_line TYPE abap_func_parmbind,
          etab      TYPE abap_func_excpbind_tab,
          etab_line TYPE abap_func_excpbind.

    LOOP AT parameters_tab ASSIGNING FIELD-SYMBOL(<parameter>).

      CLEAR ptab_line.
      ptab_line-name = <parameter>-parameter.
      ptab_line-kind = COND #( WHEN <parameter>-paramtype = 'E' THEN abap_func_importing
                               WHEN <parameter>-paramtype = 'I' THEN abap_func_exporting
                               WHEN <parameter>-paramtype = 'T' THEN abap_func_tables
                               ELSE                                  ''
      ).

      DATA(json_field_name) = COND string( WHEN ptab_line-kind = abap_func_exporting THEN 'IMPORT'
                                           WHEN ptab_line-kind = abap_func_tables    THEN 'TABLE_IN'
                                           ELSE                                           ''
      ).

      IF json_field_name IS INITIAL.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT json_field_name OF STRUCTURE record TO FIELD-SYMBOL(<json_raw>).
      IF sy-subrc <> 0 OR <json_raw> IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA(json_data) = /ui2/cl_json=>generate( json = <json_raw> ).
      ASSIGN json_data->* TO FIELD-SYMBOL(<json_data>).


      ASSIGN COMPONENT <parameter>-parameter OF STRUCTURE <json_data> TO FIELD-SYMBOL(<parameter_val>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF ptab_line-kind = abap_func_exporting.

        CREATE DATA data_ref TYPE (<parameter>-structure).
        FIELD-SYMBOLS: <temp> TYPE any.
        ASSIGN <parameter_val>->* TO <temp>.

      ENDIF.

      IF ptab_line-kind = abap_func_tables.
        DATA(structure_type) = CAST cl_abap_structdescr( cl_abap_datadescr=>describe_by_name( <parameter>-structure ) ).
        DATA(table_type) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>create( structure_type ) ).
        CREATE DATA data_ref TYPE HANDLE table_type.
        ASSIGN <parameter_val>->* TO <temp>.
      ENDIF.

      IF data_ref IS BOUND.
        ASSIGN data_ref->* TO FIELD-SYMBOL(<data_ref>).
      ENDIF.

      IF line_exists( data_elements[ domname = <parameter>-structure ] ).
        <data_ref> = <temp>.
      ELSE.
        DATA(json_temp) = /ui2/cl_json=>serialize( data = <parameter_val> ).
        /ui2/cl_json=>deserialize( EXPORTING json = json_temp CHANGING data = <data_ref> ).
      ENDIF.

      GET REFERENCE OF <data_ref> INTO ptab_line-value.

      INSERT ptab_line INTO TABLE ptab.

    ENDLOOP.

    etab_line-name = 'OTHERS'.
    etab_line-value = 2.
    INSERT etab_line INTO TABLE etab.

    CALL FUNCTION record-fname
      PARAMETER-TABLE
      ptab
      EXCEPTION-TABLE
      etab.

  ENDMETHOD.
ENDCLASS.
