*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_source_system DEFINITION.

  PUBLIC SECTION.

    TYPES tt_column_fields TYPE STANDARD TABLE OF string
        WITH DEFAULT KEY.

    CLASS-METHODS create_query_expression
      IMPORTING
        im_search_expression TYPE string
        im_screen_fields     TYPE
          if_rap_query_request=>tt_requested_elements
        im_filter_expression TYPE string
      RETURNING
        VALUE(re_result)            TYPE zma0_sfut_sql_query.

    CLASS-METHODS get_sorting_criteria
      IMPORTING
        im_sort_elements TYPE if_rap_query_request=>tt_sort_elements
      RETURNING
        VALUE(re_result)        TYPE string.

  PRIVATE SECTION.

    CLASS-METHODS get_search_fields
      IMPORTING
        im_search_expression TYPE string
        im_screen_fields     TYPE
          if_rap_query_request=>tt_requested_elements
      RETURNING
        VALUE(re_result)     TYPE lcl_source_system=>tt_column_fields.

    CLASS-METHODS remove_double_quotes
      IMPORTING
        im_value         TYPE string
      RETURNING
        VALUE(re_result) TYPE string.

ENDCLASS.
