CLASS zma0_cl_sfut_supplier_dao DEFINITION PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES tt_suppliers_plants TYPE TABLE OF zma0_sfut_plant
      WITH DEFAULT KEY.

    TYPES tt_schedule_lines TYPE TABLE OF zma0_ce_supplier
      WITH DEFAULT KEY.

    TYPES tt_supplier_action TYPE TABLE OF zma0_sfut_action
      WITH DEFAULT KEY.

    TYPES tt_supplier_code TYPE TABLE OF
      zma0_ce_supplier-lifnr WITH NON-UNIQUE DEFAULT KEY.

    CLASS-DATA supplier_data TYPE tt_schedule_lines.

    METHODS constructor
      IMPORTING
        im_offset    TYPE int8
        im_page_size TYPE int8
        im_sql_query TYPE zma0_sfut_sql_query
        im_sort      TYPE string.

    METHODS get_total_records
      RETURNING VALUE(re_result) TYPE int8.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA total_records TYPE int8.

    METHODS retrieve_schedule_lines
      IMPORTING
        im_offset           TYPE int8
        im_page_size        TYPE int8
        im_suppliers_plants TYPE
                          zma0_cl_sfut_supplier_dao=>tt_suppliers_plants
        im_sql_query        TYPE zma0_sfut_sql_query
        im_sort             TYPE string
      RETURNING
        VALUE(re_result) TYPE zma0_sfut_supplier_if_t .

    METHODS retrieve_supplier_actions
      IMPORTING
        im_schedule_lines TYPE zma0_sfut_supplier_if_t
      RETURNING
        VALUE(re_result)
          TYPE zma0_cl_sfut_supplier_dao=>tt_supplier_action.

    METHODS set_supplier_data
      IMPORTING
        im_schedule_lines   TYPE zma0_sfut_supplier_if_t
        im_supplier_actions
          TYPE zma0_cl_sfut_supplier_dao=>tt_supplier_action
      RETURNING
        VALUE(re_result)
          TYPE zma0_cl_sfut_supplier_dao=>tt_schedule_lines.

    METHODS get_status_text
      IMPORTING
        im_status TYPE zma0_ce_supplier-status
      RETURNING
        VALUE(re_result) TYPE zma0_ce_supplier-status_txt.

    METHODS get_order_type_text
      IMPORTING
        im_order_type TYPE zma0_ce_supplier-order_type
      RETURNING
        VALUE(re_result)     TYPE zma0_ce_supplier-order_type_txt.

    METHODS set_status_color
      IMPORTING
        im_eindt  TYPE zma0_ce_supplier-eindt
        im_status TYPE zma0_ce_supplier-status
      RETURNING
        VALUE(re_result) TYPE zma0_ce_supplier-status_color.

    METHODS set_eindt_color
      IMPORTING
        im_eindt         TYPE zma0_ce_supplier-eindt
      RETURNING
        VALUE(re_result) TYPE zma0_ce_supplier-eindt_color.

    METHODS retrieve_suppliers_plants
      IMPORTING
        im_supplier      TYPE zma0_ce_supplier-lifnr
      RETURNING
        value(re_result) TYPE
                         zma0_cl_sfut_supplier_dao=>tt_suppliers_plants.

ENDCLASS.



CLASS ZMA0_CL_SFUT_SUPPLIER_DAO IMPLEMENTATION.


  METHOD constructor.

    DATA(suppliers_plants) = me->retrieve_suppliers_plants(
        im_supplier = '0000002666'
       ).

    DATA(schedule_lines) = me->retrieve_schedule_lines(
        im_offset           = im_offset
        im_page_size        = im_page_size
        im_suppliers_plants = suppliers_plants
        im_sql_query        = im_sql_query
        im_sort             = im_sort
    ).

    DATA(supplier_actions) = me->retrieve_supplier_actions(
        schedule_lines
    ).

    supplier_data = me->set_supplier_data(
        im_schedule_lines   = schedule_lines
        im_supplier_actions = supplier_actions
    ).

  ENDMETHOD.


  METHOD set_status_color.

    re_result = COND #(
        WHEN im_eindt LT cl_abap_context_info=>get_system_date( )
         AND im_status EQ 0 THEN 2
        WHEN im_status EQ 1 THEN 1
        WHEN im_status EQ 2 THEN 2
        WHEN im_status EQ 3 THEN 3
        WHEN im_status EQ 4 THEN 3
        ELSE 0
    ).

  ENDMETHOD.


  METHOD retrieve_supplier_actions.

    SELECT * "#EC CI_ALL_FIELDS_NEEDED
    FROM zma0_sfut_action
    FOR ALL ENTRIES IN @im_schedule_lines
    WHERE ebeln = @im_schedule_lines-ebeln
    AND   ebelp = @im_schedule_lines-ebelp
    AND   etenr = @im_schedule_lines-etenr
    ORDER BY PRIMARY KEY
    INTO TABLE @re_result.

  ENDMETHOD.


  METHOD set_supplier_data.

    DATA lw_supplier_data TYPE zma0_ce_supplier.

    LOOP AT im_schedule_lines INTO DATA(lw_schedule_line).
      TRY.
          DATA(lw_supplier_action) = im_supplier_actions[
                ebeln = lw_schedule_line-ebeln
                ebelp = lw_schedule_line-ebelp
                etenr = lw_schedule_line-etenr
          ].
        CATCH cx_sy_itab_line_not_found.
          CLEAR lw_supplier_action.
      ENDTRY.

      MOVE-CORRESPONDING lw_supplier_action TO lw_supplier_data.
      MOVE-CORRESPONDING lw_schedule_line TO lw_supplier_data.

      lw_supplier_data-status_txt = me->get_status_text(
        lw_supplier_data-status
      ).

      lw_supplier_data-status_color = me->set_status_color(
        im_eindt  = lw_supplier_data-eindt
        im_status = lw_supplier_data-status
      ).

      lw_supplier_data-eindt_color = me->set_eindt_color(
        lw_supplier_data-eindt
      ).

      lw_supplier_data-order_type_txt = me->get_order_type_text(
        lw_supplier_data-order_type
      ).

      APPEND lw_supplier_data TO re_result.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_status_text.

    re_result = COND #(
      WHEN im_status = 0 THEN 'Action Required'
      WHEN im_status = 1 THEN 'Rejected By Deere'
      WHEN im_status = 2 THEN 'Proposed'
      WHEN im_status = 3 THEN 'Shipping On Time'
      WHEN im_status = 4 THEN 'Accepted By Deere'
    ).

  ENDMETHOD.


  METHOD set_eindt_color.

    re_result = COND #(
        WHEN im_eindt LT cl_abap_context_info=>get_system_date( )
        THEN 1 ELSE 0
    ).

  ENDMETHOD.


  METHOD get_total_records.
    re_result = me->total_records.
  ENDMETHOD.


  METHOD get_order_type_text.

    re_result = COND #(
      WHEN im_order_type = 'X' THEN 'Forecast'
      ELSE 'Firm'
    ).

  ENDMETHOD.


  METHOD retrieve_schedule_lines.

    DATA lt_schedule_lines TYPE zma0_sfut_supplier_if_t.

    DATA lv_total_records TYPE int4.

    DATA lv_page_size TYPE int4.

    lv_page_size = COND #(
        WHEN im_page_size < 0 THEN 0
        ELSE im_page_size
    ).

    DATA(lv_rfc_dest_name) =
        zma0_cl_sfut_destination=>factory( )->get_destination( ).

    CHECK lv_rfc_dest_name IS NOT INITIAL.

    CLEAR me->total_records.

    " Check if the user is from the API, filter by the im_sql_query
    AUTHORITY-CHECK OBJECT 'Z_SFUT_API' ID 'ACTVT' FIELD '03' .
    IF sy-subrc IS NOT INITIAL.
      " Check if the user is from the Supplier App
      AUTHORITY-CHECK OBJECT 'Z_SFUT_SUP' ID 'ACTVT' FIELD '03'.
      CHECK sy-subrc IS INITIAL.
*      DATA(lt_authorized_suppliers) = me->get_supplier_codes( ).
*      CHECK lt_authorized_suppliers IS NOT INITIAL.
    ENDIF.

    CALL FUNCTION 'ZMA0_SFUT_GET_SUPPLIER_DATA'
      DESTINATION lv_rfc_dest_name
      EXPORTING
        im_offset             = CONV int4( im_offset )
        im_page_size          = lv_page_size
        im_suppliers_plants   = im_suppliers_plants
        im_search_expression  = im_sql_query
        im_sql_sort           = im_sort
      IMPORTING
        ex_schedule_data      = lt_schedule_lines
        ex_number_of_records  = lv_total_records
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        OTHERS                = 3.

    "Need to be converted because INT8 doesn't exist on the source
    me->total_records = CONV int8( lv_total_records ).

    re_result = lt_schedule_lines.

  ENDMETHOD.

  METHOD retrieve_suppliers_plants.

    SELECT supplier,
           plant
      FROM zma0_sfut_plant
      WHERE supplier = @im_supplier
      INTO CORRESPONDING FIELDS OF TABLE @re_result.

  ENDMETHOD.

ENDCLASS.
