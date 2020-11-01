CLASS zcl_afl_utilities DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS re_process
      IMPORTING
        !guid TYPE guid .
    CLASS-METHODS is_prd
      RETURNING
        VALUE(result) TYPE abap_bool .
    CLASS-METHODS get_distinct_count
      IMPORTING
        !tab_data    TYPE ANY TABLE
        !field_name  TYPE clike
      RETURNING
        VALUE(count) TYPE int4 .
    CLASS-METHODS fm_authority_check
      IMPORTING !fm_name    TYPE rs38l_fnam
                !buffer     TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(pass) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_auth_result,
             fname TYPE zafl_config-fname,
             pass  TYPE abap_bool,
           END OF ty_auth_result.

    CLASS-DATA: auth_results_list TYPE HASHED TABLE OF ty_auth_result WITH UNIQUE KEY fname.

ENDCLASS.



CLASS ZCL_AFL_UTILITIES IMPLEMENTATION.


  METHOD fm_authority_check.

    DATA: auth_result LIKE LINE OF auth_results_list.

    IF buffer = abap_true.
      auth_result = VALUE #( auth_results_list[ fname = fm_name ] OPTIONAL ).
      IF auth_result IS NOT INITIAL.
        pass = auth_result-pass.
        RETURN.
      ENDIF.
    ENDIF.

    IF auth_result IS INITIAL.

      DELETE auth_results_list WHERE fname = fm_name.
      SELECT SINGLE no_auth_check FROM zafl_config WHERE fname = @fm_name
        INTO @DATA(no_auth_check) BYPASSING BUFFER.

      IF no_auth_check = abap_true.
        auth_result = VALUE #( fname = fm_name pass = abap_true ).
      ELSE.
        DATA: wa_tadir TYPE tadir,
              area  TYPE sobj_name.

        SELECT SINGLE area FROM enlfdir WHERE funcname = @fm_name INTO @area.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        CALL FUNCTION 'TR_TADIR_INTERFACE'
          EXPORTING
            wi_test_modus     = ' '
            wi_read_only      = 'X'
            wi_tadir_pgmid    = 'R3TR'
            wi_tadir_object   = 'FUGR'
            wi_tadir_obj_name = area
          IMPORTING
            new_tadir_entry   = wa_tadir
          EXCEPTIONS
            OTHERS            = 1.

        IF sy-subrc <> 0 OR wa_tadir-devclass IS INITIAL.
          AUTHORITY-CHECK OBJECT 'S_DEVELOP'
                 ID 'DEVCLASS' DUMMY
                 ID 'OBJTYPE' FIELD 'FUGR'
                 ID 'OBJNAME' FIELD area
                 ID 'P_GROUP' DUMMY
                 ID 'ACTVT' FIELD '16'.
        ELSE.
          AUTHORITY-CHECK OBJECT 'S_DEVELOP'
                 ID 'DEVCLASS' FIELD wa_tadir-devclass
                 ID 'OBJTYPE' FIELD 'FUGR'
                 ID 'OBJNAME' FIELD area
                 ID 'P_GROUP' DUMMY
                 ID 'ACTVT' FIELD '16'.
        ENDIF.
        IF sy-subrc = 0.
          auth_result = VALUE #( fname = fm_name pass = abap_true ).
        ELSE.
          auth_result = VALUE #( fname = fm_name pass = abap_false ).
        ENDIF.

      ENDIF.
    ENDIF.

    pass = auth_result-pass.

    INSERT auth_result INTO TABLE auth_results_list.

  ENDMETHOD.


  METHOD get_distinct_count.

    TYPES: BEGIN OF ty_temp,
             field TYPE string,
           END OF ty_temp.
    DATA: count_table TYPE HASHED TABLE OF ty_temp WITH UNIQUE KEY field,
          count_wa    LIKE LINE OF count_table.

    LOOP AT tab_data ASSIGNING FIELD-SYMBOL(<wa>).
      ASSIGN COMPONENT field_name OF STRUCTURE <wa> TO FIELD-SYMBOL(<field>).
      IF sy-subrc <> 0.
        RETURN.
      ELSE.
        count_wa-field = <field>.
        INSERT count_wa INTO TABLE count_table.
      ENDIF.
    ENDLOOP.

    count = lines( count_table ).

  ENDMETHOD.


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

    SELECT SINGLE guid, fname, import, change_in, table_in
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
                               WHEN <parameter>-paramtype = 'C' THEN abap_func_changing
                               ELSE                                  ''
      ).

      DATA(json_field_name) = COND string( WHEN ptab_line-kind = abap_func_exporting THEN 'IMPORT'
                                           WHEN ptab_line-kind = abap_func_tables    THEN 'TABLE_IN'
                                           WHEN ptab_line-kind = abap_func_changing  THEN 'CHANGE_IN'
                                           ELSE                                           ''
      ).

      IF json_field_name IS INITIAL.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT json_field_name OF STRUCTURE record TO FIELD-SYMBOL(<json_raw>).
      IF sy-subrc <> 0 OR <json_raw> IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA(json_data) = zcl_afl_json=>generate_new( json = <json_raw> ).
      ASSIGN json_data->* TO FIELD-SYMBOL(<json_data>).


      ASSIGN COMPONENT <parameter>-parameter OF STRUCTURE <json_data> TO FIELD-SYMBOL(<parameter_val>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF ptab_line-kind = abap_func_exporting OR ptab_line-kind = abap_func_changing.

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
        DATA(json_temp) = zcl_afl_json=>serialize( data = <parameter_val> ).
        zcl_afl_json=>deserialize( EXPORTING json = json_temp CHANGING data = <data_ref> ).
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
