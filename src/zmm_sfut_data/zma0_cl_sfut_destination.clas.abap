CLASS zma0_cl_sfut_destination DEFINITION PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor.

    CLASS-METHODS factory
      RETURNING
        VALUE(re_result) TYPE REF TO zma0_cl_sfut_destination.

    METHODS get_destination
      RETURNING
        VALUE(re_result) TYPE rfcdest.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA rfc_destination TYPE rfcdest.

*    CONSTANTS destination_name TYPE string VALUE 'CAG_RFC'.
*    CONSTANTS destination_name TYPE string VALUE 'QAG_RFC'.
    CONSTANTS destination_name TYPE string VALUE 'PAG_RFC'.

ENDCLASS.



CLASS ZMA0_CL_SFUT_DESTINATION IMPLEMENTATION.


  METHOD constructor.

    TRY.
        DATA(lo_rfc_dest) =
            cl_rfc_destination_provider=>create_by_cloud_destination(
                i_name
                    = zma0_cl_sfut_destination=>destination_name
            ).

        me->rfc_destination = lo_rfc_dest->get_destination_name(  ).
      CATCH cx_rfc_dest_provider_error.
    ENDTRY.

  ENDMETHOD.


  METHOD factory.

    re_result = NEW #( ).

  ENDMETHOD.


  METHOD get_destination.
    re_result = me->rfc_destination.
  ENDMETHOD.
ENDCLASS.
