CLASS zma0_cl_sfut_mat_memo_dao DEFINITION PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES tt_bapi_mltx TYPE TABLE OF zma0_sfut_deere_bapi_mltx
        WITH DEFAULT KEY.

    TYPES ty_tdname TYPE c LENGTH 70.

    TYPES ty_trtext TYPE c LENGTH 132.

    TYPES tt_trtext
        TYPE TABLE OF zma0_cl_sfut_mat_memo_dao=>ty_trtext
        WITH NON-UNIQUE DEFAULT KEY.

    METHODS constructor
      IMPORTING
        im_material TYPE zma0_ce_deere-matnr
        im_plant    TYPE zma0_ce_deere-werks.

    METHODS add_text
      IMPORTING
        im_new_text TYPE c.

    METHODS commit_changes.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA material TYPE zma0_ce_deere-matnr.

    DATA plant TYPE zma0_ce_deere-werks.

    DATA texts TYPE zma0_cl_sfut_mat_memo_dao=>tt_bapi_mltx.

    DATA rfc_dest_name TYPE string.

    CONSTANTS tdobject TYPE c LENGTH 10 VALUE 'MDTXT'.

    CONSTANTS tdid TYPE c LENGTH 4 VALUE 'LTXT'.

    CONSTANTS max_line_length TYPE i VALUE 132.

    METHODS retrieve_material_text
      RETURNING
        VALUE(re_result) TYPE zma0_cl_sfut_mat_memo_dao=>tt_bapi_mltx.

    METHODS prepare_text_name
      IMPORTING
        im_text_name     TYPE zma0_sfut_deere_bapi_mltx-text_name
      RETURNING
        VALUE(re_result) TYPE zma0_sfut_deere_bapi_mltx-text_name.

    METHODS text_wrapping
      IMPORTING
        im_new_text      TYPE c
      RETURNING
        VALUE(re_result) TYPE zma0_cl_sfut_mat_memo_dao=>tt_trtext.

    METHODS get_tdname
      RETURNING
        VALUE(re_result) TYPE ty_tdname.

ENDCLASS.



CLASS ZMA0_CL_SFUT_MAT_MEMO_DAO IMPLEMENTATION.


  METHOD constructor.

    me->material = im_material.
    me->plant = im_plant.

    me->rfc_dest_name =
        zma0_cl_sfut_destination=>factory( )->get_destination( ).

    me->texts = me->retrieve_material_text( ).

  ENDMETHOD.


  METHOD retrieve_material_text.

    DATA lt_current_material_text
        TYPE zma0_cl_sfut_mat_memo_dao=>tt_bapi_mltx.

    CALL FUNCTION 'BAPI_MATERIAL_GETALL'
      DESTINATION rfc_dest_name
      EXPORTING
        material              = me->material
        plant                 = me->plant
      TABLES
        materialtext          = lt_current_material_text
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        OTHERS                = 3.

    CHECK sy-subrc IS INITIAL.

    " Convert internal table type for the update operation
    re_result = VALUE zma0_cl_sfut_mat_memo_dao=>tt_bapi_mltx(
        FOR lw_current_line IN lt_current_material_text (
            applobject = lw_current_line-applobject
            text_name  = COND #(
                WHEN lw_current_line-applobject
                    = zma0_cl_sfut_mat_memo_dao=>tdobject
                THEN me->prepare_text_name( lw_current_line-text_name )
                ELSE lw_current_line-text_name
            )
            text_id    = lw_current_line-text_id
            langu      = lw_current_line-langu
            langu_iso  = lw_current_line-langu_iso
            format_col = lw_current_line-format_col
            text_line  = lw_current_line-text_line
        )
    ).

  ENDMETHOD.


  METHOD prepare_text_name.

    DATA lv_material TYPE zma0_ce_deere-matnr.
    DATA lv_plant TYPE zma0_ce_deere-werks.

    lv_material = im_text_name.
    lv_plant = im_text_name+19.

    CONCATENATE lv_material lv_plant
    INTO re_result RESPECTING BLANKS.

  ENDMETHOD.


  METHOD add_text.

    DATA lw_text TYPE LINE OF zma0_cl_sfut_mat_memo_dao=>tt_bapi_mltx .
    DATA lv_langu TYPE c LENGTH 2.

    DATA(lt_new_lines) = me->text_wrapping( im_new_text ).

    TRY.
        lv_langu =
            cl_abap_context_info=>get_user_language_iso_format( ).
      CATCH cx_abap_context_info_error.
    lv_langu = 'PT'.
    ENDTRY.

    lw_text-format_col = '*'.
    LOOP AT lt_new_lines INTO DATA(lv_new_line).
      lw_text-applobject = zma0_cl_sfut_mat_memo_dao=>tdobject.
      lw_text-text_name  = me->get_tdname( ).
      lw_text-text_id    = zma0_cl_sfut_mat_memo_dao=>tdid.
      lw_text-langu      = lv_langu.
      lw_text-langu_iso  = lv_langu.
      lw_text-text_line = lv_new_line.
      APPEND lw_text TO me->texts.
      CLEAR lw_text-format_col.
    ENDLOOP.

  ENDMETHOD.


  METHOD text_wrapping.

    CONSTANTS lc_delimiter TYPE c VALUE space.

    DATA lv_length TYPE i.
    DATA lv_index  TYPE i.
    DATA lv_line_aux TYPE c LENGTH 132.

    FIELD-SYMBOLS <new_text>       TYPE c.
    FIELD-SYMBOLS <char>           TYPE c.
    FIELD-SYMBOLS <remaining_text> TYPE c.

    ASSIGN im_new_text TO <new_text>.

    DO.
      lv_length = strlen( <new_text> ).

      IF lv_length LE me->max_line_length.
        lv_line_aux = <new_text>.
        APPEND lv_line_aux TO re_result.
        EXIT.
      ELSE.

        " Find a point to break the line
        lv_index = me->max_line_length.
        WHILE lv_index > 0.
          ASSIGN <new_text>+lv_index(1) TO <char>.
          IF <char> = lc_delimiter.
            EXIT.
          ENDIF.
          lv_index -= 1.
        ENDWHILE.
        IF lv_index = 0.
          lv_index = me->max_line_length.
        ENDIF.

        " Append line to result table
        lv_line_aux = <new_text>(lv_index).
        APPEND lv_line_aux  TO re_result.

        " Continue with the remaining text
        ASSIGN <new_text> TO <remaining_text>.
        UNASSIGN <new_text>.
        lv_index += 1.
        lv_length = strlen( <remaining_text> ) - lv_index.
        ASSIGN  <remaining_text>+lv_index(lv_length) TO <new_text>.
        UNASSIGN <remaining_text>.

      ENDIF.

    ENDDO.

  ENDMETHOD.


  METHOD get_tdname.

    CONCATENATE me->material me->plant
    INTO re_result RESPECTING BLANKS.

  ENDMETHOD.


  METHOD commit_changes.

    DATA lw_headdata TYPE zma0_sfut_deere_bapi_mathead.
    DATA lw_plantdata TYPE zma0_sfut_deere_bapi_marc.
    DATA lw_plantdatax TYPE zma0_sfut_deere_bapi_marcx.
    DATA lw_return TYPE bapiret2.

    CHECK me->rfc_dest_name IS NOT INITIAL
    AND me->texts IS NOT INITIAL.

    lw_headdata-material = me->material.
    lw_plantdata-plant = me->plant.
    lw_plantdatax-plant = me->plant.

    " Save the material memo information
    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      DESTINATION me->rfc_dest_name
      EXPORTING
        headdata              = lw_headdata
        plantdata             = lw_plantdata
        plantdatax            = lw_plantdatax
      IMPORTING
        return                = lw_return
      TABLES
        materiallongtext      = me->texts
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        OTHERS                = 3.

    CHECK sy-subrc IS INITIAL
    AND lw_return IS NOT INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      DESTINATION me->rfc_dest_name
      EXPORTING
        wait                  = 'X'
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        OTHERS                = 3.

  ENDMETHOD.
ENDCLASS.
