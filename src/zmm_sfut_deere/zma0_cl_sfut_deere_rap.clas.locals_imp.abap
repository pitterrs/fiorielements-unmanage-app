*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_source_system IMPLEMENTATION.

  METHOD create_query_expression.

    " It's required to remove double quotes from the search expression
    " for OData V4
    DATA(lv_search_expression) =
        lcl_source_system=>remove_double_quotes( im_search_expression ).

    IF lv_search_expression IS NOT INITIAL.

      DATA(lt_column_names) = lcl_source_system=>get_search_fields(
        im_search_expression = lv_search_expression
        im_screen_fields = im_screen_fields
      ).

      DATA(lt_sql_filter_aux) = VALUE string_table(
           FOR lw_screen_field IN lt_column_names (
               |{ lw_screen_field } LIKE '%{
                cl_abap_dyn_prg=>escape_quotes( lv_search_expression )
               }%'|
           )
         ).

      CONCATENATE LINES OF lt_sql_filter_aux INTO re_result
        SEPARATED BY ' OR '.

      IF im_filter_expression IS NOT INITIAL.
        re_result = |( { im_filter_expression } AND { re_result } )|.
      ENDIF.

    ELSE.
      re_result = im_filter_expression.
    ENDIF.

  ENDMETHOD.

  METHOD get_sorting_criteria.

    DATA(lt_sort_criteria) = VALUE
        string_table( FOR sort_element IN im_sort_elements
        ( sort_element-element_name &&
          COND #( WHEN sort_element-descending = abap_true
          THEN ` descending`
          ELSE ` ascending` )
          )
        ).

    re_result = COND #( WHEN lt_sort_criteria IS INITIAL
        THEN `primary key`
        ELSE concat_lines_of( table = lt_sort_criteria sep = `, ` ) ).

  ENDMETHOD.

  METHOD remove_double_quotes.

    DATA lv_len    TYPE i.
    DATA lv_offset TYPE i.

    CLEAR re_result.

    lv_len = strlen( im_value ).
    IF lv_len > 2 AND im_value(1) = '"'.
      lv_offset = lv_len - 1.
      IF im_value+lv_offset(1) = '"'.
        lv_offset = lv_offset - 1.
        re_result = im_value+1(lv_offset).
      ENDIF.
    ENDIF.

    IF re_result IS INITIAL.
      re_result = im_value.
    ENDIF.

  ENDMETHOD.

  METHOD get_search_fields.

    DATA lr_field_info TYPE REF TO cl_abap_typedescr.

    LOOP AT im_screen_fields INTO DATA(lv_screen_field).

      FREE lr_field_info.

      DATA(lv_field) = |ZMA0_SFUT_DEERE_IF-{ lv_screen_field }|.

      CALL METHOD cl_abap_elemdescr=>describe_by_name
        EXPORTING
          p_name         = lv_field
        RECEIVING
          p_descr_ref    = lr_field_info
        EXCEPTIONS
          type_not_found = 1
          OTHERS         = 2.

      CHECK sy-subrc IS INITIAL.

      "Avoid date and quantity fields
      CHECK lr_field_info->type_kind <> 'P'
        AND lr_field_info->type_kind <> 'D' .

      "Checks if the length (twice the field) is bigger than the search
      CHECK lr_field_info->length > strlen( im_search_expression ).


      APPEND lv_screen_field TO re_result.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
