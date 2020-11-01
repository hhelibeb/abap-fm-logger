CLASS zcl_afl_json DEFINITION
  PUBLIC
  INHERITING FROM /ui2/cl_json
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS: generate_int_new  IMPORTING
                                 !json          TYPE json
                               RETURNING
                                 VALUE(rr_data) TYPE REF TO data.
    CLASS-METHODS generate_new IMPORTING
                                 !json          TYPE json
                                 !pretty_name   TYPE pretty_name_mode DEFAULT pretty_mode-none
                                 !name_mappings TYPE name_mappings OPTIONAL
                               RETURNING
                                 VALUE(rr_data) TYPE REF TO data.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AFL_JSON IMPLEMENTATION.


  METHOD generate_int_new.

    TYPES: BEGIN OF ts_field,
             name  TYPE string,
             value TYPE json,
           END OF ts_field.

    DATA: length TYPE i,
          offset TYPE i.

    DATA: lt_json      TYPE STANDARD TABLE OF json WITH DEFAULT KEY,
          lv_json      LIKE LINE OF lt_json,
          lv_comp_name TYPE abap_compname,
          lt_fields    TYPE SORTED TABLE OF ts_field WITH UNIQUE KEY name,
          lo_type      TYPE REF TO cl_abap_datadescr,
          lt_comp      TYPE abap_component_tab,
          lt_names     TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line,
          cache        LIKE LINE OF mt_name_mappings_ex,
          ls_comp      LIKE LINE OF lt_comp.

    FIELD-SYMBOLS: <data>   TYPE any,
                   <struct> TYPE any,
                   <field>  LIKE LINE OF lt_fields,
                   <table>  TYPE STANDARD TABLE,
                   <cache>  LIKE LINE OF mt_name_mappings_ex.

    length = numofchar( json ).

    eat_white.

    CASE json+offset(1).
      WHEN `{`."result must be a structure
        restore_type( EXPORTING json = json length = length CHANGING  data = lt_fields ).
        IF lt_fields IS NOT INITIAL.
          ls_comp-type = cl_abap_refdescr=>get_ref_to_data( ).
          LOOP AT lt_fields ASSIGNING <field>.
            READ TABLE mt_name_mappings_ex WITH TABLE KEY json = <field>-name ASSIGNING <cache>.
            IF sy-subrc IS INITIAL.
              ls_comp-name = <cache>-abap.
            ELSE.
              cache-json = ls_comp-name = <field>-name.
              TRANSLATE ls_comp-name USING `/_:_~_._-_`. " remove characters not allowed in component names
              IF mv_pretty_name EQ pretty_mode-camel_case OR mv_pretty_name EQ pretty_mode-extended.
                REPLACE ALL OCCURRENCES OF REGEX `([a-z])([A-Z])` IN ls_comp-name WITH `$1_$2`. "#EC NOTEXT
              ENDIF.
              TRANSLATE ls_comp-name TO UPPER CASE.
              cache-abap = ls_comp-name = lv_comp_name = ls_comp-name. " truncate by allowed field name length
              INSERT cache INTO TABLE mt_name_mappings_ex.
            ENDIF.
            INSERT ls_comp-name INTO TABLE lt_names.
            IF sy-subrc IS INITIAL.
              APPEND ls_comp TO lt_comp.
            ELSE.
              DELETE lt_fields.
            ENDIF.
          ENDLOOP.
          TRY.
              lo_type = cl_abap_structdescr=>create( p_components = lt_comp p_strict = c_bool-false ).
              CREATE DATA rr_data TYPE HANDLE lo_type.
              ASSIGN rr_data->* TO <struct>.
              LOOP AT lt_fields ASSIGNING <field>.
                ASSIGN COMPONENT sy-tabix OF STRUCTURE <struct> TO <data>.
                <data> = generate_int_new( <field>-value ).
              ENDLOOP.
            CATCH cx_sy_create_data_error cx_sy_struct_creation.
          ENDTRY.
        ENDIF.
      WHEN `[`."result must be a table of ref
        restore_type( EXPORTING json = json length = length CHANGING  data = lt_json ).
        CREATE DATA rr_data TYPE TABLE OF REF TO data.
        ASSIGN rr_data->* TO <table>.
        LOOP AT lt_json INTO lv_json.
          APPEND INITIAL LINE TO <table> ASSIGNING <data>.
          <data> = generate_int_new( lv_json ).
        ENDLOOP.
      WHEN OTHERS.
        IF json+offset(1) EQ `"`.
          CREATE DATA rr_data TYPE string.
        ELSEIF json+offset(1) CA `-0123456789.`.
          IF json+offset CS '.'.
            CREATE DATA rr_data TYPE decfloat34.
          ELSEIF length GT 9.
            CREATE DATA rr_data TYPE p.
          ELSE.
            CREATE DATA rr_data TYPE i.
          ENDIF.
        ELSEIF json+offset EQ `true` OR json+offset EQ `false`.
          CREATE DATA rr_data TYPE abap_bool.
        ENDIF.
        IF rr_data IS BOUND.
          ASSIGN rr_data->* TO <data>.
          restore_type( EXPORTING json = json length = length CHANGING  data = <data> ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD generate_new.

    DATA: lo_json TYPE REF TO zcl_afl_json,
          lv_json LIKE json.

    lv_json = json.

    REPLACE ALL OCCURRENCES OF `\r\n` IN lv_json WITH cl_abap_char_utilities=>cr_lf.
    REPLACE ALL OCCURRENCES OF `\n`   IN lv_json WITH cl_abap_char_utilities=>newline.
    REPLACE ALL OCCURRENCES OF `\t`   IN lv_json WITH cl_abap_char_utilities=>horizontal_tab.

    CREATE OBJECT lo_json
      EXPORTING
        pretty_name      = pretty_name
        name_mappings    = name_mappings
        assoc_arrays     = c_bool-true
        assoc_arrays_opt = c_bool-true.

    TRY .
        rr_data = lo_json->generate_int_new( lv_json ).
      CATCH cx_sy_move_cast_error.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
