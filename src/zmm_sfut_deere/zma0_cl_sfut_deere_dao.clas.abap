CLASS zma0_cl_sfut_deere_dao DEFINITION PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES tt_schedule_lines TYPE TABLE OF zma0_ce_deere
        WITH DEFAULT KEY.

    TYPES tt_supplier_action TYPE TABLE OF zma0_sfut_action
        WITH DEFAULT KEY.

    TYPES ty_dispo type c LENGTH 3.

    TYPES tt_mrp_controllers TYPE  STANDARD TABLE OF ty_dispo
        WITH DEFAULT KEY.

    CLASS-DATA supplier_data
        TYPE zma0_cl_sfut_deere_dao=>tt_schedule_lines.

    METHODS constructor
      IMPORTING
        im_offset    TYPE int8
        im_page_size TYPE int8
        im_sql_query TYPE zma0_sfut_sql_query
        im_sort      TYPE string.

    METHODS get_total_records
      RETURNING
        VALUE(re_result) TYPE int8.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA total_records TYPE int8.

    METHODS retrieve_schedule_lines
      IMPORTING
        im_offset        TYPE int8
        im_page_size     TYPE int8
        im_sql_query     TYPE zma0_sfut_sql_query
        im_sort          TYPE string
      RETURNING
        VALUE(re_result) TYPE zma0_sfut_deere_if_t .

    METHODS retrieve_supplier_actions
      IMPORTING
        im_schedule_lines TYPE zma0_sfut_deere_if_t
      RETURNING
        VALUE(re_result)
          TYPE zma0_cl_sfut_deere_dao=>tt_supplier_action.

    METHODS set_supplier_data
      IMPORTING
        im_schedule_lines   TYPE zma0_sfut_deere_if_t
        im_supplier_actions
          TYPE zma0_cl_sfut_deere_dao=>tt_supplier_action
      RETURNING
        VALUE(re_result)
          TYPE zma0_cl_sfut_deere_dao=>tt_schedule_lines.

    METHODS get_status_txt
      IMPORTING
        im_status        TYPE zma0_ce_deere-status
      RETURNING
        VALUE(re_result) TYPE zma0_ce_deere-status_txt.

    METHODS get_status_color
      IMPORTING
        im_status        TYPE zma0_ce_deere-status
      RETURNING
        VALUE(re_result) TYPE zma0_ce_deere-status_color.

    METHODS get_mrp_controllers
      RETURNING
        VALUE(re_result) TYPE tt_mrp_controllers.

ENDCLASS.



CLASS ZMA0_CL_SFUT_DEERE_DAO IMPLEMENTATION.


  METHOD constructor.

    DATA(schedule_lines) = me->retrieve_schedule_lines(
        im_offset         =   im_offset
        im_page_size      =   im_page_size
        im_sql_query      =   im_sql_query
        im_sort           =   im_sort
    ).

    DATA(supplier_actions) = me->retrieve_supplier_actions(
        schedule_lines
        ).

    supplier_data = me->set_supplier_data(
        im_schedule_lines = schedule_lines
        im_supplier_actions = supplier_actions
     ).

  ENDMETHOD.


  METHOD get_total_records.
    re_result = me->total_records.
  ENDMETHOD.


  METHOD retrieve_schedule_lines.

    DATA lv_msg TYPE  c LENGTH 255.

    DATA lt_schedule_lines TYPE zma0_sfut_deere_if_t.

    DATA lv_total_records TYPE int4.

    DATA lv_page_size TYPE int4.

    lv_page_size = COND #(
        WHEN im_page_size < 0
        THEN 0 ELSE im_page_size
    ).

    DATA(lv_rfc_dest_name) =
        zma0_cl_sfut_destination=>factory( )->get_destination( ).

    CHECK lv_rfc_dest_name IS NOT INITIAL.

    " Check if the user is a planner or buyer (John Deere Employee)
    AUTHORITY-CHECK OBJECT 'Z_SFUT_JDE' ID 'ACTVT' FIELD '03' .
    CHECK sy-subrc IS INITIAL.

    DATA lt_mrp_controllers TYPE TABLE OF ty_dispo.

    lt_mrp_controllers = me->get_mrp_controllers( ).

    CALL FUNCTION 'ZMA0_SFUT_GET_DEERE_DATA'
      DESTINATION lv_rfc_dest_name
      EXPORTING
        im_offset             = CONV int4( im_offset )
        im_page_size          = lv_page_size
        im_mrp_controllers    = lt_mrp_controllers
        im_search_expression  = im_sql_query
        im_sql_sort           = im_sort
      IMPORTING
        ex_schedule_data      = lt_schedule_lines
        ex_number_of_records  = lv_total_records
      EXCEPTIONS
        system_failure        = 1 MESSAGE lv_msg
        communication_failure = 2 MESSAGE lv_msg
        OTHERS                = 3.

    "Need to be converted because INT8 doesn't exiçst on the source
    me->total_records = CONV int8( lv_total_records ).

    re_result = lt_schedule_lines.

  ENDMETHOD.


  METHOD retrieve_supplier_actions.

    SELECT *
    FROM zma0_sfut_action
    FOR ALL ENTRIES IN @im_schedule_lines
    WHERE ebeln = @im_schedule_lines-ebeln
    AND   ebelp = @im_schedule_lines-ebelp
    AND   etenr = @im_schedule_lines-etenr
    ORDER BY PRIMARY KEY
    INTO TABLE @re_result. "#EC CI_ALL_FIELDS_NEEDED

  ENDMETHOD.


  METHOD set_supplier_data.

    DATA lw_supplier_data TYPE zma0_ce_deere.

    LOOP AT im_schedule_lines INTO DATA(lw_schedule_line).
      TRY.
          DATA(lw_supplier_action) = im_supplier_actions[
                ebeln = lw_schedule_line-ebeln
                ebelp = lw_schedule_line-ebelp
                etenr = lw_schedule_line-etenr
          ].
        CATCH cx_sy_itab_line_not_found.
          CLEAR lw_supplier_action.
      ENDTRY.

      MOVE-CORRESPONDING lw_supplier_action TO lw_supplier_data.
      MOVE-CORRESPONDING lw_schedule_line TO lw_supplier_data.

      lw_supplier_data-status_txt = me->get_status_txt(
        lw_supplier_data-status
      ).

      lw_supplier_data-status_color = me->get_status_color(
        lw_supplier_data-status
      ).

      APPEND lw_supplier_data TO re_result.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_status_txt.

    re_result = COND #(
      WHEN im_status = 1 THEN 'Rejected'
      WHEN im_status = 2 THEN 'Proposed'
      WHEN im_status = 3 THEN 'Accepted'
      WHEN im_status = 4 THEN 'Approved by Deere'
      ELSE 'Action Required'
    ).

  ENDMETHOD.


  METHOD get_status_color.

    re_result = COND #(
      WHEN im_status = 1 THEN 1 " Rejected
      WHEN im_status = 2 THEN 2 " Proposed
      WHEN im_status = 3 OR im_status = 4 THEN 3 " Accepted or Approved
      ELSE 0
    ).

  ENDMETHOD.


  METHOD get_mrp_controllers.

  RE_RESULT = VALUE #(
  ( 'C00' ) ( 'C01' ) ( 'C02' ) ( 'C03' ) ( 'C04' ) ( 'C05' ) ( 'C06' )
  ( 'C07' ) ( 'C08' ) ( 'C09' ) ( 'C0S' ) ( 'C1' )  ( 'C10' ) ( 'C11' )
  ( 'C12' ) ( 'C13' ) ( 'C14' ) ( 'C15' ) ( 'C16' ) ( 'C17' ) ( 'C18' )
  ( 'C19' ) ( 'C1B' ) ( 'C20' ) ( 'C21' ) ( 'C23' ) ( 'C25' ) ( 'C26' )
  ( 'C27' ) ( 'C28' ) ( 'C29' ) ( 'C2B' ) ( 'C30' ) ( 'C31' ) ( 'C32' )
  ( 'C33' ) ( 'C3B' ) ( 'C4B' ) ( 'CA2' ) ( 'CA3' ) ( 'CAB' ) ( 'CAD' )
  ( 'CAF' ) ( 'CAI' ) ( 'CAK' ) ( 'CAM' ) ( 'CAO' ) ( 'CAR' ) ( 'CAX' )
  ( 'CAZ' ) ( 'CB1' ) ( 'CC1' ) ( 'CC2' ) ( 'CC3' ) ( 'CC4' ) ( 'CC5' )
  ( 'CC6' ) ( 'CC7' ) ( 'CC8' ) ( 'CC9' ) ( 'CCF' ) ( 'CCN' ) ( 'CCR' )
  ( 'CCX' ) ( 'CD1' ) ( 'CD2' ) ( 'CD3' ) ( 'CD4' ) ( 'CD5' ) ( 'CD6' )
  ( 'CDA' ) ( 'CDB' ) ( 'CDC' ) ( 'CE1' ) ( 'CE2' ) ( 'CE3' ) ( 'CE4' )
  ( 'CF' )  ( 'CF1' ) ( 'CF2' ) ( 'CF3' ) ( 'CF4' ) ( 'CF5' ) ( 'CF6' )
  ( 'CF7' ) ( 'CFA' ) ( 'CFB' ) ( 'CFC' ) ( 'CFD' ) ( 'CFE' ) ( 'CFF' )
  ( 'CFG' ) ( 'CFH' ) ( 'CFI' ) ( 'CFJ' ) ( 'CFK' ) ( 'CFL' ) ( 'CFM' )
  ( 'CFO' ) ( 'CFR' ) ( 'CFT' ) ( 'CG1' ) ( 'CG3' ) ( 'CG5' ) ( 'CG8' )
  ( 'CG9' ) ( 'CGC' ) ( 'CGD' ) ( 'CGE' ) ( 'CGG' ) ( 'CGH' ) ( 'CGI' )
  ( 'CGJ' ) ( 'CGL' ) ( 'CGM' ) ( 'CGN' ) ( 'CGT' ) ( 'CGX' ) ( 'CH1' )
  ( 'CH2' ) ( 'CH3' ) ( 'CH4' ) ( 'CH5' ) ( 'CH6' ) ( 'CH7' ) ( 'CH8' )
  ( 'CHX' ) ( 'CI1' ) ( 'CI3' ) ( 'CI4' ) ( 'CI6' ) ( 'CI7' ) ( 'CI9' )
  ( 'CIC' ) ( 'CIP' ) ( 'CLA' ) ( 'CLB' ) ( 'CLC' ) ( 'CLD' ) ( 'CLE' )
  ( 'CLF' ) ( 'CLG' ) ( 'CLH' ) ( 'CLI' ) ( 'CLJ' ) ( 'CLK' ) ( 'CLL' )
  ( 'CLM' ) ( 'CLN' ) ( 'CLO' ) ( 'CLP' ) ( 'CLQ' ) ( 'CLV' ) ( 'CM2' )
  ( 'CM3' ) ( 'CM5' ) ( 'CM8' ) ( 'CMA' ) ( 'CMB' ) ( 'CMC' ) ( 'CMD' )
  ( 'CME' ) ( 'CMF' ) ( 'CMG' ) ( 'CMH' ) ( 'CMJ' ) ( 'CMK' ) ( 'CML' )
  ( 'CMM' ) ( 'CMN' ) ( 'CMO' ) ( 'CMP' ) ( 'CMQ' ) ( 'CMS' ) ( 'CMT' )
  ( 'CMU' ) ( 'CMV' ) ( 'CMW' ) ( 'CMX' ) ( 'CMY' ) ( 'CNO' ) ( 'CO1' )
  ( 'CO2' ) ( 'CO3' ) ( 'CP1' ) ( 'CP2' ) ( 'CP3' ) ( 'CP4' ) ( 'CP5' )
  ( 'CP6' ) ( 'CP7' ) ( 'CP8' ) ( 'CP9' ) ( 'CPA' ) ( 'CPB' ) ( 'CPC' )
  ( 'CPD' ) ( 'CPE' ) ( 'CPF' ) ( 'CPI' ) ( 'CPK' ) ( 'CPO' ) ( 'CPS' )
  ( 'CR1' ) ( 'CR2' ) ( 'CR3' ) ( 'CR4' ) ( 'CRM' ) ( 'CRX' ) ( 'CS2' )
  ( 'CS4' ) ( 'CS6' ) ( 'CS7' ) ( 'CSE' ) ( 'CSH' ) ( 'CSL' ) ( 'CSM' )
  ( 'CSV' ) ( 'CSW' ) ( 'CSY' ) ( 'CT1' ) ( 'CT2' ) ( 'CT6' ) ( 'CT7' )
  ( 'CTA' ) ( 'CTC' ) ( 'CTD' ) ( 'CTE' ) ( 'CTF' ) ( 'CTG' ) ( 'CTH' )
  ( 'CTI' ) ( 'CTJ' ) ( 'CTK' ) ( 'CTL' ) ( 'CTM' ) ( 'CTN' ) ( 'CTO' )
  ( 'CTP' ) ( 'CTR' ) ( 'CTS' ) ( 'CTT' ) ( 'CTU' ) ( 'CU1' ) ( 'CU2' )
  ( 'CU3' ) ( 'CU4' ) ( 'CU5' ) ( 'CU6' ) ( 'CU7' ) ( 'CU8' ) ( 'CU9' )
  ( 'CUC' ) ( 'CUE' ) ( 'CUH' ) ( 'CUR' ) ( 'CUT' ) ( 'CUV' ) ( 'CW1' )
  ( 'CXN' ) ( 'D00' ) ( 'D01' ) ( 'D02' ) ( 'D03' ) ( 'D04' ) ( 'D05' )
  ( 'D06' ) ( 'D07' ) ( 'D08' ) ( 'D09' ) ( 'D1' )  ( 'D10' ) ( 'D13' )
  ( 'D14' ) ( 'D15' ) ( 'D16' ) ( 'D17' ) ( 'D18' ) ( 'D19' ) ( 'D2' )
  ( 'D20' ) ( 'D21' ) ( 'D22' ) ( 'D23' ) ( 'D24' ) ( 'D25' ) ( 'D26' )
  ( 'D27' ) ( 'D29' ) ( 'D30' ) ( 'D31' ) ( 'D32' ) ( 'D33' ) ( 'D34' )
  ( 'D35' ) ( 'D3X' ) ( 'D40' ) ( 'D41' ) ( 'D42' ) ( 'D43' ) ( 'D45' )
  ( 'D48' ) ( 'D5' )  ( 'D50' ) ( 'D51' ) ( 'D52' ) ( 'D63' ) ( 'D66' )
  ( 'D67' ) ( 'D68' ) ( 'D69' ) ( 'D72' ) ( 'D82' ) ( 'D99' ) ( 'DED' )
  ( 'DF7' ) ( 'DI8' ) ( 'DMA' ) ( 'DP1' ) ( 'DR9' ) ( 'DRA' ) ( 'DRL' )
  ( 'DRV' ) ( 'DUA' ) ( 'DWW' ) ( 'DXA' ) ( 'DXC' ) ( 'DXL' ) ( 'CIM' )
  ).

  ENDMETHOD.
ENDCLASS.
