*"* use this source file for your ABAP unit test classes
CLASS ltc_get_distinct_count DEFINITION FINAL FOR TESTING
DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS: setup.
    METHODS: empty_input FOR TESTING.
    METHODS: general_input FOR TESTING.
    METHODS: error_input FOR TESTING.
ENDCLASS.

CLASS ltc_get_distinct_count IMPLEMENTATION.

  METHOD setup.

  ENDMETHOD.
  METHOD empty_input.

    DATA: test_table TYPE STANDARD TABLE OF sflight.

    DATA(count) = zcl_afl_utilities=>get_distinct_count( tab_data = test_table field_name = 'CARRID' ).
    cl_abap_unit_assert=>assert_equals(
      act = count
      exp = 0
      msg = |exp: 0, act:{ count }|
    ).

  ENDMETHOD.
  METHOD general_input.

    DATA: test_table TYPE STANDARD TABLE OF sflight.

    test_table = VALUE #(
      ( seatsmax = '1' )
      ( seatsmax = '55332' )
      ( seatsmax = '3' )
      ( seatsmax = '4' )
      ( seatsmax = '5' )
      ( seatsmax = '6' )
      ( seatsmax = '7' )
      ( seatsmax = '8' )
      ( seatsmax = '9' )
      ( seatsmax = '5' )
      ( seatsmax = '5' )
      ( seatsmax = '2' )
      ( seatsmax = '5' )
      ( seatsmax = '1' )
      ( seatsmax = '9' )
      ( seatsmax = '9' )
      ( seatsmax = '6' )
    ).

    DATA(count) = zcl_afl_utilities=>get_distinct_count( tab_data = test_table field_name = 'SEATSMAX' ).
    cl_abap_unit_assert=>assert_equals(
      act = count
      exp = 10
      msg = |exp: 10, act:{ count }|
    ).

  ENDMETHOD.
  METHOD error_input.

    DATA: test_table TYPE STANDARD TABLE OF sflight.

    DATA(count) = zcl_afl_utilities=>get_distinct_count( tab_data = test_table field_name = 'XCARRID' ).
    cl_abap_unit_assert=>assert_equals(
      act = count
      exp = 0
      msg = |exp: 0, act:{ count }|
    ).

  ENDMETHOD.

ENDCLASS.
