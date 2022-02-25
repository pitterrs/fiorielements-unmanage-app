CLASS zma0_cl_sfut_supplier_rap DEFINITION PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rap_query_provider.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZMA0_CL_SFUT_SUPPLIER_RAP IMPLEMENTATION.


  METHOD if_rap_query_provider~select.

    IF io_request->is_data_requested( ).

      " Merge the filters with the query expression and prepare the
      " where clause for the schedule lines retrieval process
      DATA(lv_sql_query) = lcl_source_system=>create_query_expression(
          im_search_expression = io_request->get_search_expression( )
          im_screen_fields     = io_request->get_requested_elements( )
          im_filter_expression =
                io_request->get_filter( )->get_as_sql_string( )
      ).

      " Prepare the sorting operation requested by the user
      DATA(lv_sorting) = lcl_source_system=>get_sorting_criteria(
        io_request->get_sort_elements( )
      ).

      " Get the quantity and position to be queried based on the
      " scroll position at the data being displayed
      DATA(lv_skip) = io_request->get_paging( )->get_offset( ).
      DATA(lv_top) = io_request->get_paging( )->get_page_size( ).

      " Get Schedule Lines information from the source
      DATA(lo_supplier_dao) = NEW zma0_cl_sfut_supplier_dao(
          im_offset    = lv_skip
          im_page_size = lv_top
          im_sql_query = lv_sql_query
          im_sort      = lv_sorting
      ).

      " Set the data to be presented
      io_response->set_data( lo_supplier_dao->supplier_data ).

    ENDIF.

    " Set the total number of records on the source system to allow
    " scroll operations during the application display
    IF io_request->is_total_numb_of_rec_requested( ).
      io_response->set_total_number_of_records(
          lo_supplier_dao->get_total_records( )
      ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
