CLASS zma0_cl_sfut_deere_upd DEFINITION PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES tt_schedule_lines TYPE TABLE OF zma0_ce_deere
        WITH DEFAULT KEY.

    TYPES tt_last_schedule_lines
        TYPE TABLE OF zma0_sfut_deere_last_lines_if WITH DEFAULT KEY.

    TYPES tt_bapiret2 TYPE TABLE OF bapiret2 WITH DEFAULT KEY.

    TYPES tt_bapi_schedule TYPE TABLE OF zma0_sfut_deere_bapi_schedule.

    TYPES tt_bapi_schedulex
        TYPE TABLE OF zma0_sfut_deere_bapi_scheduleX.

    METHODS constructor
      IMPORTING
        im_schedule_lines
          TYPE zma0_cl_sfut_deere_upd=>tt_schedule_lines.

    METHODS update_operation
      RETURNING
        VALUE(re_result) TYPE zma0_cl_sfut_deere_upd=>tt_bapiret2.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA schedule_lines TYPE zma0_cl_sfut_deere_upd=>tt_schedule_lines.

    DATA rfc_dest_name TYPE string.

    DATA last_schedule_lines
        TYPE zma0_cl_sfut_deere_upd=>tt_last_schedule_lines.

    METHODS retrieve_last_schedule_lines
      IMPORTING
        im_schedule_lines
          TYPE zma0_cl_sfut_deere_upd=>tt_schedule_lines
      RETURNING
        VALUE(re_result)
          TYPE zma0_cl_sfut_deere_upd=>tt_last_schedule_lines.

    METHODS prepare_schedule_line_create
      IMPORTING
        im_line_data TYPE zma0_ce_deere
      CHANGING
        ch_schedule  TYPE zma0_cl_sfut_deere_upd=>tt_bapi_schedule
        ch_schedulex TYPE zma0_cl_sfut_deere_upd=>tt_bapi_schedulex.

    METHODS prepare_schedule_line_update
      IMPORTING
        im_line_data TYPE zma0_ce_deere
      CHANGING
        ch_schedule  TYPE zma0_cl_sfut_deere_upd=>tt_bapi_schedule
        ch_schedulex TYPE zma0_cl_sfut_deere_upd=>tt_bapi_schedulex.

    METHODS update_schedule_agreement
      IMPORTING
        im_line_data TYPE zma0_ce_deere
        im_schedule  TYPE zma0_cl_sfut_deere_upd=>tt_bapi_schedule
        im_schedulex TYPE zma0_cl_sfut_deere_upd=>tt_bapi_schedulex
      EXPORTING
        ex_return    TYPE zma0_cl_sfut_deere_upd=>tt_bapiret2.

    METHODS format_date
      IMPORTING
        im_date          TYPE zma0_sfut_date
      RETURNING
        VALUE(re_result) TYPE zma0_sfut_date.

    METHODS update_material_memo
      IMPORTING
        im_schedule_line TYPE zma0_ce_deere.

    METHODS get_next_schedule_line
      IMPORTING
        im_ebeln         TYPE zma0_ce_deere-ebeln
        im_ebelp         TYPE zma0_ce_deere-ebelp
      RETURNING
        VALUE(re_result) TYPE zma0_ce_deere-etenr.

ENDCLASS.



CLASS ZMA0_CL_SFUT_DEERE_UPD IMPLEMENTATION.


  METHOD constructor.

    me->schedule_lines = im_schedule_lines.

    me->rfc_dest_name =
        zma0_cl_sfut_destination=>factory( )->get_destination( ).

    me->last_schedule_lines = me->retrieve_last_schedule_lines(
        me->schedule_lines
    ).

  ENDMETHOD.


  METHOD retrieve_last_schedule_lines.

    DATA lt_schedule_lines
        TYPE zma0_cl_sfut_deere_upd=>tt_last_schedule_lines.
    DATA lv_msg TYPE  c LENGTH 255.

    CHECK im_schedule_lines IS NOT INITIAL
    AND me->rfc_dest_name IS NOT INITIAL.

    lt_schedule_lines = VALUE #(
        FOR lw_schedule_line IN im_schedule_lines (
            ebeln = lw_schedule_line-ebeln
            ebelp = lw_schedule_line-ebelp
        )
    ).

    " Will retrieve the last schedule line item number for each
    " selected schedule agreement
    CALL FUNCTION 'ZMA0_SFUT_GET_LAST_SCHEDULES'
      DESTINATION me->rfc_dest_name
      CHANGING
        ch_schedule_lines     = lt_schedule_lines
      EXCEPTIONS
        system_failure        = 1 MESSAGE lv_msg
        communication_failure = 2 MESSAGE lv_msg
        OTHERS                = 3.

    SORT lt_schedule_lines
    BY ebeln ebelp ASCENDING etenr DESCENDING.

    DELETE ADJACENT DUPLICATES FROM lt_schedule_lines
    COMPARING ebeln ebelp.

    re_result = lt_schedule_lines.

  ENDMETHOD.


  METHOD update_operation.

    DATA lt_schedule TYPE zma0_cl_sfut_deere_upd=>tt_bapi_schedule.
    DATA lt_schedulex TYPE zma0_cl_sfut_deere_upd=>tt_bapi_schedulex.
    DATA lt_return TYPE zma0_cl_sfut_deere_upd=>tt_bapiret2.

    LOOP AT schedule_lines INTO DATA(lw_schedule_line).

      " Prepare the internal tables for the update of the schedule line
      IF lw_schedule_line-new_date IS NOT INITIAL
      AND lw_schedule_line-new_qty IS NOT INITIAL.
        me->prepare_schedule_line_update(
            EXPORTING
                im_line_data = lw_schedule_line
            CHANGING
                ch_schedule  = lt_schedule
                ch_schedulex = lt_schedulex
        ).
      ENDIF.

      " Prepare internal tables for the creation of a new schedule line
      IF  lw_schedule_line-partial_date IS NOT INITIAL
      AND lw_schedule_line-partial_qty IS NOT INITIAL.
        me->prepare_schedule_line_create(
            EXPORTING
                im_line_data = lw_schedule_line
            CHANGING
                ch_schedule  = lt_schedule
                ch_schedulex = lt_schedulex
        ).
      ENDIF.

      " Update the schedule agreement schedule line data on the source
      me->update_schedule_agreement(
        EXPORTING
          im_line_data = lw_schedule_line
          im_schedule  = lt_schedule
          im_schedulex = lt_schedulex
        IMPORTING
          ex_return    = lt_return
      ).

      IF lw_schedule_line-supplier_comment IS NOT INITIAL.

        me->update_material_memo(
            lw_schedule_line
        ).

      ENDIF.

      re_result = lt_return.

      FREE lt_schedule.
      FREE lt_schedulex.
      CLEAR lw_schedule_line.

    ENDLOOP.

  ENDMETHOD.


  METHOD prepare_schedule_line_create.

    DATA(lv_partial_date) =
        me->format_date( im_line_data-partial_date ).

    DATA(lv_new_schedule_line) = me->get_next_schedule_line(
        im_ebeln = im_line_data-ebeln
        im_ebelp = im_line_data-ebelp
    ).

    APPEND VALUE zma0_sfut_deere_bapi_schedule(
      item_no       = im_line_data-ebelp
      sched_line    = lv_new_schedule_line
      delivery_date = lv_partial_date
      quantity      = im_line_data-partial_qty

    ) TO ch_schedule.

    APPEND VALUE zma0_sfut_deere_bapi_schedulex(
        item_no       = im_line_data-ebelp
        sched_line    = lv_new_schedule_line
        item_nox      = 'X'
        sched_linex   = 'X'
        delivery_date = 'X'
        quantity      = 'X'
    ) TO ch_schedulex.

  ENDMETHOD.


  METHOD prepare_schedule_line_update.

    DATA(lv_new_date) = me->format_date( im_line_data-new_date ).

    " Delivery Schedule Line Data Outline Agreement
    APPEND VALUE zma0_sfut_deere_bapi_schedule(
        item_no       = im_line_data-ebelp
        sched_line    = im_line_data-etenr
        delivery_date = lv_new_date
        quantity      = im_line_data-new_qty
    ) TO ch_schedule.

    APPEND VALUE zma0_sfut_deere_bapi_schedulex(
        item_no       = im_line_data-ebelp
        sched_line    = im_line_data-etenr
        delivery_date = 'X'
        quantity      = 'X'
    ) TO ch_schedulex.

  ENDMETHOD.


  METHOD update_schedule_agreement.

    DATA lt_return TYPE zma0_cl_sfut_deere_upd=>tt_bapiret2.

    CHECK me->rfc_dest_name IS NOT INITIAL.

    CALL FUNCTION 'BAPI_SCHEDULE_MAINTAIN'
      DESTINATION me->rfc_dest_name
      EXPORTING
        purchasingdocument    = im_line_data-ebeln
      TABLES
        schedule              = im_schedule
        schedulex             = im_schedulex
        return                = lt_return
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        OTHERS                = 3.

    IF line_exists( lt_return[ type = 'E' ] ). " Error exists?

      APPEND VALUE bapiret2(
         type       = 'E'
         log_msg_no = 7
         message_v1 = im_line_data-ebeln
         message_v2 = im_line_data-ebelp
         message_v3 = im_line_data-etenr
      ) TO ex_return.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        DESTINATION me->rfc_dest_name
        EXPORTING
          wait                  = 'X'
        EXCEPTIONS
          system_failure        = 1
          communication_failure = 2
          OTHERS                = 3.

    ENDIF.

  ENDMETHOD.


  METHOD format_date.

    re_result = |{ im_date+4(2) }| && |{ im_date+6(2) }| &&
                |{ im_date(4) }|.

  ENDMETHOD.


  METHOD update_material_memo.

    DATA(lo_material_memo_dao) = NEW zma0_cl_sfut_mat_memo_dao(
        im_material = im_schedule_line-matnr
        im_plant    = im_schedule_line-werks
    ).

    " Update the material memo internal table with the supplier comment
    lo_material_memo_dao->add_text( im_schedule_line-supplier_comment ).

    " Send changes to the SAP source system
    lo_material_memo_dao->commit_changes(  ).

  ENDMETHOD.


  METHOD get_next_schedule_line.

    re_result = COND #(
        WHEN line_exists( me->last_schedule_lines[
            ebeln = im_ebeln
            ebelp = im_ebelp
        ] )
        THEN me->last_schedule_lines[
            ebeln = im_ebeln
            ebelp = im_ebelp
        ]-etenr + 1
        ELSE 1
    ).

  ENDMETHOD.
ENDCLASS.
