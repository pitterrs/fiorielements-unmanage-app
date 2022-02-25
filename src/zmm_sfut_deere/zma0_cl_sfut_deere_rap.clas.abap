CLASS zma0_cl_sfut_deere_rap DEFINITION PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rap_query_provider.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZMA0_CL_SFUT_DEERE_RAP IMPLEMENTATION.


  METHOD if_rap_query_provider~select.

    DATA lt_supplier_data TYPE TABLE OF zma0_ce_deere.

    DATA lv_total_records TYPE int8.

    IF io_request->is_data_requested( ).

      DATA(lt_aggr_element) =
        io_request->get_aggregation( )->get_aggregated_elements( ).

      DATA(lv_searching) =
        lcl_source_system=>create_query_expression(
            im_search_expression = io_request->get_search_expression( )
            im_screen_fields     = io_request->get_requested_elements( )
            im_filter_expression =
                io_request->get_filter( )->get_as_sql_string( )
      ).

      DATA(lv_sorting) = lcl_source_system=>get_sorting_criteria(
        io_request->get_sort_elements( )
      ).

      DATA(lv_skip) = io_request->get_paging( )->get_offset( ).
      DATA(lv_top) = io_request->get_paging( )->get_page_size( ).

      DATA(lo_deere_dao) = NEW zma0_cl_sfut_deere_dao(
           im_offset     = lv_skip
           im_page_size  = lv_top
           im_sql_query  = lv_searching
           im_sort       = lv_sorting
      ).

      io_response->set_data( lo_deere_dao->supplier_data ).

    ENDIF.

    IF io_request->is_total_numb_of_rec_requested( ).
      io_response->set_total_number_of_records(
        lo_deere_dao->get_total_records( )
      ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
