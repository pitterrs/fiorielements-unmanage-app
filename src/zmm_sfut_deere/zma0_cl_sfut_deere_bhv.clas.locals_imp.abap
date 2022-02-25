*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lhc_DeereData DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys   REQUEST requested_features FOR DeereData
      RESULT    result.

    METHODS get_authorizations FOR AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations
      FOR DeereData RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ DeereData RESULT result.

    METHODS accept FOR MODIFY
      IMPORTING keys FOR ACTION DeereData~accept RESULT result.

    METHODS reject FOR MODIFY
      IMPORTING keys FOR ACTION DeereData~reject RESULT result.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE DeereData.

    METHODS is_authorized_to_update
      RETURNING
        VALUE(re_result) TYPE abap_bool.

ENDCLASS.

CLASS lhc_DeereData IMPLEMENTATION.

  METHOD get_instance_features.

    DATA lt_date TYPE cl_abap_context_info=>ty_system_date.
    lt_date = cl_abap_context_info=>get_system_date( ).

    READ ENTITY IN LOCAL MODE zma0_ce_deere FROM VALUE #(
        FOR keyval IN keys (
            %key = keyval-%key
        )
    ) RESULT DATA(lt_supplier_data).

    " Can only accept when the status is 0 (New/Action Required)
    result = VALUE #(
        FOR lw_supplier_data IN lt_supplier_data
        LET is_accepted = COND #(
            WHEN lw_supplier_data-status = 2
            THEN if_abap_behv=>fc-o-enabled
            ELSE if_abap_behv=>fc-o-disabled
        ) is_rejected =  COND #(
            WHEN lw_supplier_data-status = 2
            THEN if_abap_behv=>fc-o-enabled
            ELSE if_abap_behv=>fc-o-disabled
        ) IN (
            %tky = lw_supplier_data-%tky
            %action-accept = is_accepted
            %action-reject = is_rejected
        )
    ).

  ENDMETHOD.

  METHOD get_authorizations.

    DATA lt_date TYPE cl_abap_context_info=>ty_system_date.
    lt_date = cl_abap_context_info=>get_system_date( ).

    READ ENTITY IN LOCAL MODE zma0_ce_deere FROM VALUE #(
        FOR keyval IN keys (
            %key = keyval-%key
        )
    ) RESULT DATA(lt_supplier_data).

    result = VALUE #(
        FOR lw_supplier_data IN lt_supplier_data (
            %tky = lw_supplier_data-%tky
            %action-accept = COND #(
                WHEN me->is_authorized_to_update( )
                    THEN
                     COND #(
                        "Status Action Required w/ Ship Date in the
                        "future -> Can say that will be Shipping On Time
                        WHEN lw_supplier_data-status = 2
                        THEN if_abap_behv=>auth-allowed
                        ELSE if_abap_behv=>auth-unauthorized
                    ) ELSE
                    if_abap_behv=>auth-unauthorized
            )
            %action-reject = COND #(
                WHEN me->is_authorized_to_update( )
                    THEN
                    COND #(
                        "Status Action Required w/ Ship Date in the
                        "future -> Can say that will be Shipping On Time
                        WHEN lw_supplier_data-status = 2
                        THEN if_abap_behv=>auth-allowed
                        ELSE if_abap_behv=>auth-unauthorized
                    ) ELSE
                    if_abap_behv=>auth-unauthorized
            )
        )
    ).

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
    IF zma0_cl_sfut_deere_dao=>supplier_data IS INITIAL.

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
                    && | AND pabnum = { lw_key-pabnum }|
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

      DATA(lo_supplier_dao) = NEW zma0_cl_sfut_deere_dao(
        im_offset    = 0
        im_page_size = 999999
        im_sql_query = lv_sql_query
        im_sort      = lv_sql_sort
      ).

      LOOP AT zma0_cl_sfut_deere_dao=>supplier_data
      INTO DATA(lw_supplier_data).
        INSERT CORRESPONDING #( lw_supplier_data ) INTO TABLE result.
      ENDLOOP.

    ELSE.

      LOOP AT zma0_cl_sfut_deere_dao=>supplier_data
      INTO DATA(lw_supplier_datas).
        INSERT CORRESPONDING #( lw_supplier_datas ) INTO TABLE result.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD accept.

    READ ENTITY IN LOCAL MODE zma0_ce_deere FROM VALUE #(
      FOR keyval IN keys (
          %key = keyval-%key
      )
    ) RESULT DATA(lt_supplier_data).

    DATA(lo_deere_update) = NEW zma0_cl_sfut_deere_upd(
        im_schedule_lines = CONV #( lt_supplier_data )
    ).

    DATA(lt_result) = lo_deere_update->update_operation( ).

    MODIFY ENTITIES OF zma0_ce_deere IN LOCAL MODE
    ENTITY deereData UPDATE FIELDS ( status ) WITH VALUE #(
        FOR key IN keys (
            %tky         = key-%tky
            %data-status = 4
        )
    ) FAILED failed REPORTED reported.

    " Read data and set it again to avoid entering on the Details Page
    READ ENTITY IN LOCAL MODE zma0_ce_deere FROM VALUE #(
        FOR keyval IN keys (
            %key = keyval-%key
        )
    ) RESULT lt_supplier_data.

    result = VALUE #(
        FOR lw_supplier_data IN lt_supplier_data (
            %tky = lw_supplier_data-%tky
            %param = lw_supplier_data
        )
    ).

  ENDMETHOD.

  METHOD reject.

    MODIFY ENTITIES OF zma0_ce_deere IN LOCAL MODE
        ENTITY deereData UPDATE FIELDS ( status ) WITH VALUE #(
            FOR key IN keys (
                %tky         = key-%tky
                %data-status = 1
            )
    ) FAILED failed REPORTED reported.

    " Read data and set it again to avoid entering on the Details Page
    READ ENTITY IN LOCAL MODE zma0_ce_deere FROM VALUE #(
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

  METHOD is_authorized_to_update.

    AUTHORITY-CHECK OBJECT 'Z_SFUT_JDE' ID 'ACTVT' FIELD '02' .

    IF sy-subrc IS INITIAL.
      re_result = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD update.

    DATA selected_row TYPE zma0_ce_deere.

    LOOP AT entities INTO DATA(lw_entity).

      " Fill the variable to be used on the validation methods
      MOVE-CORRESPONDING lw_entity TO selected_row.

      UPDATE zma0_sfut_action
      SET status   = @selected_row-status
      WHERE ebeln  = @selected_row-ebeln
        AND ebelp  = @selected_row-ebelp
        AND etenr  = @selected_row-etenr
        AND pkkey  = @selected_row-pkkey
        AND pabnum = @selected_row-pabnum
        AND pabpos = @selected_row-pabpos.

    ENDLOOP.

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
