CLASS lhc_SupplierData DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    CLASS-DATA selected_row TYPE zma0_ce_supplier.

    CLASS-DATA supplier_data TYPE zma0_ce_supplier.

    CLASS-DATA fields TYPE TABLE OF string.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys   REQUEST requested_features FOR SupplierData
      RESULT    result.

*    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
    METHODS get_authorizations FOR AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations
      FOR SupplierData RESULT result.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE SupplierData.

    METHODS read FOR READ
      IMPORTING keys FOR READ SupplierData RESULT result.

    METHODS accept FOR MODIFY
      IMPORTING keys FOR ACTION SupplierData~accept RESULT result.

    METHODS is_partial_date_valid
      RETURNING VALUE(re_result) TYPE abap_bool.

    METHODS is_total_qty_valid
      RETURNING VALUE(re_result) TYPE abap_bool.

    METHODS is_reason_valid
      RETURNING
        VALUE(r_result) TYPE abap_bool.

    METHODS is_new_date_valid
      RETURNING
        VALUE(re_result) TYPE abap_bool.

    METHODS is_new_qty_valid
      RETURNING
        VALUE(re_result) TYPE abap_bool.

    METHODS is_partial_qty_valid
      RETURNING
        VALUE(re_result) TYPE abap_bool.

    METHODS is_authorized_to_update
      RETURNING
        VALUE(re_result) TYPE abap_bool.

ENDCLASS.

CLASS lhc_SupplierData IMPLEMENTATION.

  METHOD get_instance_features.

    DATA lt_date TYPE cl_abap_context_info=>ty_system_date.
    lt_date = cl_abap_context_info=>get_system_date( ).

    READ ENTITY IN LOCAL MODE zma0_ce_supplier FROM VALUE #(
        FOR keyval IN keys (
            %key = keyval-%key
        )
    ) RESULT DATA(lt_supplier_data).

    " Can only accept when the status is 0 (New/Action Required)
    result = VALUE #(
        FOR lw_supplier_data IN lt_supplier_data
        LET is_accepted = COND #(
            " #TODO add the date
            WHEN lw_supplier_data-status = 3
            THEN if_abap_behv=>fc-o-disabled
            WHEN lw_supplier_data-status = 2
            THEN if_abap_behv=>fc-o-disabled
            ELSE if_abap_behv=>fc-o-enabled
        ) IN (
            %tky = lw_supplier_data-%tky
            %action-accept = is_accepted
        )
    ).

  ENDMETHOD.

  METHOD get_authorizations.

    DATA lt_date TYPE cl_abap_context_info=>ty_system_date.
    lt_date = cl_abap_context_info=>get_system_date( ).

    READ ENTITY IN LOCAL MODE zma0_ce_supplier FROM VALUE #(
        FOR keyval IN keys (
            %key = keyval-%key
        )
    ) RESULT DATA(lt_supplier_data).

    result = VALUE #(
        FOR lw_supplier_data IN lt_supplier_data (
            %tky = lw_supplier_data-%tky
            %update = COND #(
                WHEN me->is_authorized_to_update( )
                 AND lw_supplier_data-pkkey IS INITIAL " Not Kanban
                    THEN COND #(
                        "Status Action Required -> Can Propose new date
                        WHEN lw_supplier_data-status = 0
                        THEN if_abap_behv=>auth-allowed
                        "Status Rejected By Deere-> Can Propose new date
                        WHEN lw_supplier_data-status = 1
                        THEN if_abap_behv=>auth-allowed
                        "Status Shipping On Time w/ Ship Date in the
                        "past -> Can Propose new date
                        WHEN lw_supplier_data-status = 3
                        AND lw_supplier_data-eindt < lt_date
                        THEN if_abap_behv=>auth-allowed
                        ELSE if_abap_behv=>auth-unauthorized
                    ) ELSE if_abap_behv=>auth-unauthorized
            )
            %action-accept = COND #(
                WHEN me->is_authorized_to_update( )
                    THEN COND #(
                        "Status Action Required w/ Ship Date in the
                        "future -> Can say that will be Shipping On Time
                        WHEN lw_supplier_data-status = 0
                        AND lw_supplier_data-eindt > lt_date
                        THEN if_abap_behv=>auth-allowed
                        ELSE if_abap_behv=>auth-unauthorized
                    ) ELSE if_abap_behv=>auth-unauthorized
            )
        )
    ).

  ENDMETHOD.

  METHOD update.

    DATA lv_error TYPE abap_bool.

    LOOP AT entities INTO DATA(lw_entity).

      " Fill the variable to be used on the validation methods
      MOVE-CORRESPONDING lw_entity TO selected_row.

      " Get other data from the register being updated
      READ TABLE zma0_cl_sfut_supplier_dao=>supplier_data
      WITH KEY ebeln = lw_entity-ebeln
               ebelp = lw_entity-ebelp
               etenr = lw_entity-etenr
               pkkey = lw_entity-pkkey
               pabnum = lw_entity-pabnum
               pabpos = lw_entity-pabpos
          INTO supplier_data.

      IF lw_entity-status = 0.

        IF NOT me->is_new_date_valid( ).
          APPEND VALUE #(
              %tky        = lw_entity-%tky
              %state_area = 'VALIDATE_DATES'
              %msg        = NEW lcl_message_wrapper( im_msgno = '001' )
              %element-new_date = if_abap_behv=>mk-on
          ) TO reported-supplierdata.
          lv_error = abap_true.
        ENDIF.

        IF NOT me->is_new_qty_valid( ).
          APPEND VALUE #(
              %tky        = lw_entity-%tky
              %state_area = 'VALIDATE_DATES'
              %msg        = NEW lcl_message_wrapper( im_msgno = '002' )
              %element-new_qty = if_abap_behv=>mk-on
          ) TO reported-supplierdata.
          lv_error = abap_true.
        ENDIF.

        IF NOT me->is_partial_date_valid( ).
          APPEND VALUE #(
              %tky        = lw_entity-%tky
              %state_area = 'VALIDATE_DATES'
              %msg        = NEW lcl_message_wrapper( im_msgno = '003' )
              %element-partial_date = if_abap_behv=>mk-on
          ) TO reported-supplierdata.
          lv_error = abap_true.
        ENDIF.

        IF NOT me->is_partial_qty_valid( ).
          APPEND VALUE #(
              %tky        = lw_entity-%tky
              %state_area = 'VALIDATE_DATES'
              %msg        = NEW lcl_message_wrapper( im_msgno = '004' )
              %element-partial_qty = if_abap_behv=>mk-on
          ) TO reported-supplierdata.
          lv_error = abap_true.
        ENDIF.

        IF NOT me->is_total_qty_valid( ).
          APPEND VALUE #(
              %tky        = lw_entity-%tky
              %state_area = 'VALIDATE_DATES'
              %msg        = NEW lcl_message_wrapper( im_msgno = '005' )
              %element-new_qty  = if_abap_behv=>mk-on
              %element-partial_qty = if_abap_behv=>mk-on
          ) TO reported-supplierdata.
          lv_error = abap_true.
        ENDIF.

        IF NOT me->is_reason_valid( ).
          APPEND VALUE #(
              %tky        = lw_entity-%tky
              %state_area = 'VALIDATE_DATES'
              %msg        = NEW lcl_message_wrapper( im_msgno = '006' )
              %element-reason = if_abap_behv=>mk-on
          ) TO reported-supplierdata.
          lv_error = abap_true.
        ENDIF.

        CHECK lv_error IS INITIAL.
        lw_entity-status = 2.

      ENDIF.

      TRY.
          MODIFY zma0_sfut_action FROM @(
            VALUE #(
               ebeln            = supplier_data-ebeln
               ebelp            = supplier_data-ebelp
               etenr            = supplier_data-etenr
               pkkey            = supplier_data-pkkey
               pabnum           = supplier_data-pabnum
               pabpos           = supplier_data-pabpos
               status           = lw_entity-status
               reason           = lw_entity-reason
               curr_date        = supplier_data-eindt
               curr_qty         = supplier_data-bal_qty_w_asn
               new_date         = lw_entity-new_date
               new_qty          = lw_entity-new_qty
               partial_date     = lw_entity-partial_date
               partial_qty      = lw_entity-partial_qty
               supplier_comment = lw_entity-supplier_comment
               deere_action     = ''
               change_date      = sy-datum
               change_time      = sy-uzeit
               user_id          =
                cl_abap_context_info=>get_user_alias( )
               user_name        =
                cl_abap_context_info=>get_user_formatted_name( )
               )
          ).

        CATCH cx_abap_context_info_error.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.

  METHOD read.

    TYPES: BEGIN OF ty_string,
             line TYPE zma0_sfut_sql_query,
           END OF ty_string.

    DATA lv_sql_sort TYPE string.
    DATA lt_sql_query TYPE TABLE OF ty_string.
    DATA lv_sql_query TYPE zma0_sfut_sql_query.

    " If the buffer is empty, the class will need to retrieve the data
    " set for the source system with the clause set on the application
    IF zma0_cl_sfut_supplier_dao=>supplier_data IS INITIAL.

      lv_sql_sort = 'PRIMARY KEY'.

      LOOP AT keys INTO DATA(lw_key).

        APPEND VALUE #(
            line = COND #(
            WHEN lw_key-pkkey IS NOT INITIAL " Check if it is a Kanban
            THEN |{
                COND #(
                    WHEN sy-tabix = 1
                    THEN `( `
                    ELSE `OR ( ` ) }|
                    && |ebeln = '{ lw_key-ebeln }'|
                    && | AND ebelp = { lw_key-ebelp }|
                    && | AND pkkey = { lw_key-pkkey }|
                    && | AND pabnum = '{ lw_key-pabnum }'|
                    && | AND pabpos = { lw_key-pabpos } )|
            ELSE |{ " It is not a Kanban scenario...
                COND #(
                    WHEN sy-index = 1
                    THEN `( `
                    ELSE `OR ( ` ) }|
                    && |ebeln = '{ lw_key-ebeln }'|
                    && | AND ebelp = { lw_key-ebelp }|
                    && | AND etenr = { lw_key-etenr } )|
            )
        ) TO lt_sql_query.

      ENDLOOP.

      LOOP AT lt_sql_query INTO DATA(lw_sql_statement).
        lv_sql_query = COND string(
            WHEN lv_sql_query IS INITIAL
            THEN |{ lv_sql_query }{ CONV string( lw_sql_statement ) }|
            ELSE |{ lv_sql_query } { CONV string( lw_sql_statement ) }|
        ).
      ENDLOOP.

      DATA(lo_supplier_dao) = NEW zma0_cl_sfut_supplier_dao(
        im_offset    = 0
        im_page_size = 999999
        im_sql_query = lv_sql_query
        im_sort      = lv_sql_sort
      ).

      LOOP AT zma0_cl_sfut_supplier_dao=>supplier_data
      INTO DATA(lw_supplier_data).
        INSERT CORRESPONDING #( lw_supplier_data ) INTO TABLE result.
      ENDLOOP.

    ELSE.

      LOOP AT zma0_cl_sfut_supplier_dao=>supplier_data
      INTO DATA(lw_supplier_datas).
        INSERT CORRESPONDING #( lw_supplier_datas ) INTO TABLE result.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD accept.

    MODIFY ENTITIES OF zma0_ce_supplier IN LOCAL MODE
    ENTITY supplierData UPDATE FIELDS ( status ) WITH VALUE #(
        FOR key IN keys (
            %tky         = key-%tky
            %data-status = 3
        )
    ) FAILED failed REPORTED reported.

    READ ENTITY IN LOCAL MODE zma0_ce_supplier FROM VALUE #(
        FOR keyval IN keys (
            %key = keyval-%key
        )
    ) RESULT DATA(lt_supplier_data).

    result = VALUE #(
        FOR lw_supplier_data IN lt_supplier_data (
            %tky = lw_supplier_data-%tky
            %param = lw_supplier_data
        )
    ).

  ENDMETHOD.

  METHOD is_new_date_valid.

    CHECK selected_row-new_date >=
    cl_abap_context_info=>get_system_date( ).

    re_result = abap_true.

  ENDMETHOD.

  METHOD is_new_qty_valid.

    CHECK selected_row-new_qty > 0.

    re_result = abap_true.

  ENDMETHOD.

  METHOD is_partial_date_valid.

    IF selected_row-partial_date IS INITIAL AND
    selected_row-partial_qty IS INITIAL.
      re_result = abap_true.
      EXIT.
    ENDIF.

    CHECK selected_row-partial_date >=
        cl_abap_context_info=>get_system_date( ) AND
        selected_row-partial_date > selected_row-new_date.

    re_result = abap_true.

  ENDMETHOD.

  METHOD is_partial_qty_valid.

    IF selected_row-partial_date IS INITIAL AND
    selected_row-partial_qty IS INITIAL.
      re_result = abap_true.
      EXIT.
    ENDIF.

    CHECK selected_row-partial_qty > 0.

    re_result = abap_true.

  ENDMETHOD.

  METHOD is_total_qty_valid.

    CHECK selected_row-new_qty + selected_row-partial_qty
          = supplier_data-bal_qty_w_asn.

    re_result = abap_true.


  ENDMETHOD.


  METHOD is_reason_valid.

    SELECT * FROM zma0_sfut_reason
    WHERE reason = @selected_row-reason INTO TABLE @DATA(lt_reasons).

    CHECK sy-subrc IS INITIAL.
    r_result = abap_true.

  ENDMETHOD.


  METHOD is_authorized_to_update.

    AUTHORITY-CHECK OBJECT 'Z_SFUT_SUP' ID 'ACTVT' FIELD '02' .

    IF sy-subrc IS INITIAL.
      re_result = abap_true.
    ENDIF.

  ENDMETHOD.


ENDCLASS.

CLASS lsc_ZMA0_CE_SUPPLIER DEFINITION
  INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZMA0_CE_SUPPLIER IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.
  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_message_wrapper IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( ).

    if_abap_behv_message~m_severity = COND #(
        WHEN im_severity IS NOT INITIAL
        THEN im_severity ELSE if_abap_behv_message=>severity-error
    ).

    if_t100_message~t100key = VALUE #(
        msgid = message_class
        msgno = im_msgno
        attr1 = im_attr1
        attr2 = im_attr2
        attr3 = im_attr3
        attr4 = im_attr4
    ).

  ENDMETHOD.

ENDCLASS.
