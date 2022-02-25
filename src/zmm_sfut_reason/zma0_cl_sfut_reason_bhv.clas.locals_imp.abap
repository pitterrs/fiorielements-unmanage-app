CLASS lhc_ZMA0_I_SFUT_REASON DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zma0_i_sfut_reason RESULT result.

ENDCLASS.

CLASS lhc_ZMA0_I_SFUT_REASON IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

ENDCLASS.
