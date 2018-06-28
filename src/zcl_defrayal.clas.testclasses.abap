CLASS ltc_defrayal DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT.

  PRIVATE SECTION.
    DATA: m_cut TYPE REF TO zcl_defrayal.
    DATA: lv_defrayal_date  TYPE d,
          lt_defrayal_dates TYPE ztt_po_clearing_dates,
          ls_defrayal_date  TYPE zdt_po_clearing_dates.
    " standard scenarios
    METHODS po4500983429_in_290914_out FOR TESTING RAISING cx_static_check.
    METHODS fi1906004425_in_210714_out FOR TESTING RAISING cx_static_check.
    METHODS po5500153597_in_nodod_out FOR TESTING RAISING cx_static_check.
    METHODS fi1907014846_in_30082016_out FOR TESTING RAISING cx_static_check.
    METHODS po527462_in_22012016_out FOR TESTING RAISING cx_static_check.
    METHODS po5500134352_in_20160408_out FOR TESTING RAISING cx_static_check.
    METHODS fi1906022636_in_nodod_out FOR TESTING RAISING cx_static_check.

    " missing data
    METHODS no_docno FOR TESTING RAISING cx_static_check.
    METHODS no_doctyp FOR TESTING RAISING cx_static_check.
    METHODS no_gjahr FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_defrayal IMPLEMENTATION.

  METHOD po4500983429_in_290914_out.

    " when
    TRY.
        CALL METHOD zcl_defrayal=>get_date(
          EXPORTING
            iv_docno       = '4500983429'
            iv_itnum       = '00160'
            iv_doctyp      = 'PO'
            iv_gjahr       = '2014'
          RECEIVING
            et_defrayal_dd = lt_defrayal_dates ).
      CATCH cx_static_check.
    ENDTRY.

    READ TABLE lt_defrayal_dates INTO ls_defrayal_date INDEX 1.
    lv_defrayal_date = ls_defrayal_date-zdefrayal_date.

    " then
    cl_abap_unit_assert=>assert_equals( act = lv_defrayal_date
                                         exp = '20140929' ).

  ENDMETHOD.

  METHOD fi1906004425_in_210714_out.
    " when
    TRY.
        CALL METHOD zcl_defrayal=>get_date(
          EXPORTING
            iv_docno       = '1906004425'
            iv_itnum       = '00000'
            iv_doctyp      = 'FI'
            iv_gjahr       = '2013'
          RECEIVING
            et_defrayal_dd = lt_defrayal_dates ).

      CATCH cx_static_check.

    ENDTRY.

    READ TABLE lt_defrayal_dates INTO ls_defrayal_date INDEX 1.

    lv_defrayal_date = ls_defrayal_date-zdefrayal_date.

    " then
    cl_abap_unit_assert=>assert_equals( act = lv_defrayal_date
                                         exp = '20140721' ).

  ENDMETHOD.

  METHOD no_docno.
    " when
    TRY.
        CALL METHOD zcl_defrayal=>get_date(
          EXPORTING
            iv_docno       = space
            iv_itnum       = '00000'
            iv_doctyp      = 'FI'
            iv_gjahr       = '2013'
          RECEIVING
            et_defrayal_dd = lt_defrayal_dates ).

      CATCH cx_static_check.

    ENDTRY.

    READ TABLE lt_defrayal_dates INTO ls_defrayal_date INDEX 1.
    lv_defrayal_date = ls_defrayal_date-zdefrayal_date.

    " then
    cl_abap_unit_assert=>assert_equals( act = lv_defrayal_date
                                         exp = '00000000' ).
  ENDMETHOD.

  METHOD no_doctyp.
    " when
    TRY.
        CALL METHOD zcl_defrayal=>get_date(
          EXPORTING
            iv_docno       = '1906004425'
            iv_itnum       = '00000'
            iv_doctyp      = space
            iv_gjahr       = '2013'
          RECEIVING
            et_defrayal_dd = lt_defrayal_dates ).

      CATCH cx_static_check.

    ENDTRY.

    READ TABLE lt_defrayal_dates INTO ls_defrayal_date INDEX 1.
    lv_defrayal_date = ls_defrayal_date-zdefrayal_date.

    " then
    cl_abap_unit_assert=>assert_equals( act = lv_defrayal_date
                                         exp = '00000000' ).
  ENDMETHOD.

  METHOD no_gjahr.
    " when
    TRY.
        CALL METHOD zcl_defrayal=>get_date(
          EXPORTING
            iv_docno       = '1906004425'
            iv_itnum       = '00000'
            iv_doctyp      = 'FI'
            iv_gjahr       = 0000
          RECEIVING
            et_defrayal_dd = lt_defrayal_dates ).

      CATCH cx_static_check.

    ENDTRY.

    READ TABLE lt_defrayal_dates INTO ls_defrayal_date INDEX 1.
    lv_defrayal_date = ls_defrayal_date-zdefrayal_date.

    " then
    cl_abap_unit_assert=>assert_equals( act = lv_defrayal_date
                                         exp = '00000000' ).
  ENDMETHOD.

  METHOD po5500153597_in_nodod_out.
* PO 5500153597 clearing date is 27.07.2016
* Unusual one here due to credit and invoice on 31.7.16?
* WHERE THERE IS A CREDIT AGAINST THE PO THEN NO DOD IS REPORTED.

    " when
    TRY.
        CALL METHOD zcl_defrayal=>get_date(
          EXPORTING
            iv_docno       = '5500153597'
            iv_itnum       = '00001'
            iv_doctyp      = 'PO'
            iv_gjahr       = '2016'
          RECEIVING
            et_defrayal_dd = lt_defrayal_dates ).
      CATCH cx_static_check.
    ENDTRY.

    READ TABLE lt_defrayal_dates INTO ls_defrayal_date INDEX 1.
    lv_defrayal_date = ls_defrayal_date-zdefrayal_date.

    " then
    cl_abap_unit_assert=>assert_equals( act = lv_defrayal_date
                                         exp = '00000000' ).

  ENDMETHOD.

  METHOD fi1907014846_in_30082016_out.
* Expense DOD correct - clearing date is 25.08.2016

    " when
    TRY.
        CALL METHOD zcl_defrayal=>get_date(
          EXPORTING
            iv_docno       = '1907014846'
            iv_itnum       = '00001'
            iv_doctyp      = 'FI'
            iv_gjahr       = '2016'
          RECEIVING
            et_defrayal_dd = lt_defrayal_dates ).
      CATCH cx_static_check.
    ENDTRY.

    READ TABLE lt_defrayal_dates INTO ls_defrayal_date INDEX 1.
    lv_defrayal_date = ls_defrayal_date-zdefrayal_date.

    " then
    cl_abap_unit_assert=>assert_equals( act = lv_defrayal_date
                                         exp = '20160830' ).

  ENDMETHOD.

  METHOD po527462_in_22012016_out.
* Internal Trading so DOD is 22.01.16 same as clearing date

    " when
    TRY.
        CALL METHOD zcl_defrayal=>get_date(
          EXPORTING
            iv_docno       = '0000527462'
            iv_itnum       = '00010'
            iv_doctyp      = 'PO'
            iv_gjahr       = '2015'
          RECEIVING
            et_defrayal_dd = lt_defrayal_dates ).
      CATCH cx_static_check.
    ENDTRY.

    READ TABLE lt_defrayal_dates INTO ls_defrayal_date INDEX 1.
    lv_defrayal_date = ls_defrayal_date-zdefrayal_date.

    " then
    cl_abap_unit_assert=>assert_equals( act = lv_defrayal_date
                                         exp = '20160122' ).

  ENDMETHOD.

  METHOD po5500134352_in_20160408_out.
* PO 5500134352, clearing date is 8.4.16, payment method is F FOREIGN
* and currency is USD.  So DOD should be the same as the clearing date 8.4.16

    " when
    TRY.
        CALL METHOD zcl_defrayal=>get_date(
          EXPORTING
            iv_docno       = '5500134352'
            iv_itnum       = '00001'
            iv_doctyp      = 'PO'
            iv_gjahr       = '2015'
          RECEIVING
            et_defrayal_dd = lt_defrayal_dates ).
      CATCH cx_static_check.
    ENDTRY.

    READ TABLE lt_defrayal_dates INTO ls_defrayal_date INDEX 1.
    lv_defrayal_date = ls_defrayal_date-zdefrayal_date.

    " then
    cl_abap_unit_assert=>assert_equals( act = lv_defrayal_date
                                         exp = '20160408' ).

  ENDMETHOD.

  METHOD fi1906022636_in_nodod_out.
* Payment of Expenses, and the payment method was blank as it was paid by
* Santander in SGD.

    " when
    TRY.
        CALL METHOD zcl_defrayal=>get_date(
          EXPORTING
            iv_docno       = '1906022636'
            iv_itnum       = '00000'
            iv_doctyp      = 'FI'
            iv_gjahr       = '2016'
          RECEIVING
            et_defrayal_dd = lt_defrayal_dates ).
      CATCH cx_static_check.
    ENDTRY.

    READ TABLE lt_defrayal_dates INTO ls_defrayal_date INDEX 1.
    lv_defrayal_date = ls_defrayal_date-zdefrayal_date.

    " then
    cl_abap_unit_assert=>assert_equals( act = lv_defrayal_date
                                         exp = '00000000' ).
  ENDMETHOD.

ENDCLASS.
