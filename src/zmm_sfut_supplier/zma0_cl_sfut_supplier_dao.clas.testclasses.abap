*"* use this source file for your ABAP unit test classes
CLASS lcl_sfut_supplier_dao_test DEFINITION FOR TESTING
RISK LEVEL HARMLESS DURATION SHORT.

  PUBLIC SECTION.

  PRIVATE SECTION.

    DATA supplier_dao TYPE REF TO zma0_cl_sfut_supplier_dao.
    DATA new_variable TYPE abap_bool.

    METHODS setup .

    METHODS get_status_reject_test FOR TESTING.

    METHODS get_status_proposed_test FOR TESTING.

    METHODS get_status_accepted_test FOR TESTING.

    METHODS get_status_new_test FOR TESTING.

    METHODS get_order_type_forecast_test FOR TESTING.

    METHODS get_order_type_firm_test FOR TESTING.

ENDCLASS.

CLASS lcl_sfut_supplier_dao_test IMPLEMENTATION.

  METHOD setup.

    DATA lt_filter TYPE if_rap_query_filter=>tt_name_range_pairs.

    me->supplier_dao = NEW #(
        im_offset    = 0
        im_page_size = 20
        im_sql_query = ``
        im_sort      = `PRIMARY KEY`
    ).


  ENDMETHOD.

  METHOD get_order_type_forecast_test.

    DATA(lv_result) = me->supplier_dao->get_order_type_text( 'X' ).

    new_variable = cl_abap_unit_assert=>assert_equals(
        act = lv_result
        exp = 'Forecast'
    ).

  ENDMETHOD.

  METHOD get_order_type_firm_test.

    DATA(lv_result) = me->supplier_dao->get_order_type_text( '' ).

    new_variable = cl_abap_unit_assert=>assert_equals(
        act = lv_result
        exp = 'Firm'
    ).

  ENDMETHOD.

  METHOD get_status_accepted_test.

    DATA(lv_result) = me->supplier_dao->get_status_text( '1' ).

    new_variable = cl_abap_unit_assert=>assert_equals(
        act = lv_result
        exp = 'Rejected By Deere'
   ).

  ENDMETHOD.

  METHOD get_status_proposed_test.

    DATA(lv_result) = me->supplier_dao->get_status_text( '2' ).

    new_variable = cl_abap_unit_assert=>assert_equals(
        act = lv_result
        exp = 'Proposed'
   ).

  ENDMETHOD.

  METHOD get_status_reject_test.


    DATA(lv_result) = me->supplier_dao->get_status_text( '3' ).

    new_variable = cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'Shipping On Time'
   ).

  ENDMETHOD.

  METHOD get_status_new_test.

    DATA(lv_result) = me->supplier_dao->get_status_text( '' ).

    new_variable = cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'Action Required'
   ).

  ENDMETHOD.

ENDCLASS.
