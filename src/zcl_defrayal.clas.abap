class ZCL_DEFRAYAL definition
  public
  final
  create public .

public section.

  class-methods GET_DATE
    importing
      !IV_DOCNO type CHAR10
      !IV_ITNUM type CHAR5 default '00000'
      !IV_DOCTYP type ZDE_DOCTYP
      !IV_GJAHR type GJAHR
    returning
      value(ET_DEFRAYAL_DD) type ZTT_PO_CLEARING_DATES
    exceptions
      CX_DATE_CONVERT_ERROR
      CX_NO_DATA
      CX_NO_POSTING_DATE .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DEFRAYAL IMPLEMENTATION.


  METHOD GET_DATE.

    CONSTANTS: c_buzei_1 TYPE bseg-buzei  VALUE '001',
               c_po_type TYPE zde_doctyp  VALUE 'PO',
               c_fi_type TYPE zde_doctyp  VALUE 'FI',
               c_ir_flag TYPE vgabe       VALUE '2',
               c_gr_flag TYPE vgabe       VALUE '1',
               c_gb_cal  TYPE scal-fcalid VALUE 'GB',
               c_koart_k TYPE bseg-koart  VALUE 'K'.  "vendors

    DATA: lv_ebeln TYPE ekko-ebeln,
          lv_ebelp TYPE ekpo-ebelp,
          lv_belnr TYPE bkpf-belnr,
          lv_buzei TYPE bseg-buzei.

    DATA: lv_days    TYPE scal-facdate,
          lv_facdate TYPE scal-facdate,
          lv_zlsch   TYPE bseg-zlsch,
          lv_lines   TYPE i,
          lv_rzawe   TYPE reguh-rzawe,
          lv_fc_flag TYPE abap_bool VALUE abap_false.

    DATA: augdt  TYPE bsak-augdt, "clearing date
          dd_dat TYPE bsak-augdt. "date of defrayal

    DATA: wa_defrayal_dd TYPE zdt_po_clearing_dates.

    DATA: r_ebelp  TYPE RANGE OF ekpo-ebelp,
          wa_ebelp LIKE LINE OF r_ebelp,
          r_buzei  TYPE RANGE OF bseg-buzei,
          wa_buzei LIKE LINE OF r_buzei.

    IF iv_doctyp EQ c_po_type.                     "dealing with PO doc


      CLEAR   wa_ebelp.
      REFRESH r_ebelp.

      lv_ebeln = iv_docno.

      IF iv_itnum IS NOT INITIAL.

        wa_ebelp-low    = iv_itnum.
        wa_ebelp-sign   = 'I'.
        wa_ebelp-option = 'EQ'.
        APPEND wa_ebelp TO r_ebelp.

      ENDIF.  "iv_itnum

      SELECT
        ekbe~ebeln  AS ebeln
        ekbe~ebelp  AS ebelp
        bseg~augdt
        bkpf~blart
        bseg~zlsch
        bsak~waers
        INTO CORRESPONDING FIELDS OF TABLE et_defrayal_dd
        FROM ekbe AS ekbe
        INNER JOIN bsak AS bsak ON bsak~gjahr EQ ekbe~gjahr AND
                                   bsak~belnr EQ ekbe~belnr
        INNER JOIN bkpf AS bkpf ON bkpf~belnr EQ ekbe~belnr AND
                                   bkpf~gjahr EQ ekbe~gjahr
        INNER JOIN bseg AS bseg ON bseg~bukrs EQ bsak~bukrs AND
                                   bseg~belnr EQ bsak~augbl AND
                                   bseg~gjahr EQ bsak~gjahr AND
                                   bseg~koart EQ c_koart_k
        INNER JOIN lfa1 AS lfa1 ON lfa1~lifnr EQ bseg~lifnr
        WHERE ekbe~ebeln EQ lv_ebeln AND
              ekbe~ebelp IN r_ebelp  AND
              ekbe~gjahr EQ iv_gjahr AND
              bseg~augdt IS NOT NULL                              "only want entries with a clearing date
        ORDER BY ekbe~ebeln
                 ekbe~ebelp.

      DESCRIBE TABLE et_defrayal_dd LINES lv_lines.

      IF lv_lines EQ 1.            "more entries means a single item has been GR'ed more than 1. Difficult to get DofD
        READ TABLE et_defrayal_dd INTO wa_defrayal_dd INDEX 1.

        IF sy-subrc EQ 0.
          wa_defrayal_dd-doctyp = c_po_type.

          IF wa_defrayal_dd-blart EQ 'RE' AND  "RE and there is no payment method so it's internally cleared
             wa_defrayal_dd-zlsch IS INITIAL.
            wa_defrayal_dd-zlsch = 'I'.        "indicate payment is internally cleared
          ENDIF.

          MODIFY et_defrayal_dd FROM wa_defrayal_dd INDEX sy-tabix.
        ENDIF.
      ELSE.
        REFRESH et_defrayal_dd.
      ENDIF.

    ELSEIF iv_doctyp EQ c_fi_type.                 "FI type document
      CLEAR   wa_buzei.
      REFRESH r_buzei.

      lv_belnr = iv_docno.

      IF iv_itnum IS NOT INITIAL.

        wa_buzei-low    = iv_itnum.
        wa_buzei-sign   = 'I'.
        wa_buzei-option = 'EQ'.
        APPEND wa_buzei TO r_buzei.

      ENDIF.   "iv_itnum

      SELECT
        bkpf2~belnr AS belnr
        bseg2~buzei AS buzei
        bseg2~zlsch AS zlsch
        bkpf1~blart
        bseg2~augdt
        bkpf2~waers
      INTO CORRESPONDING FIELDS OF TABLE et_defrayal_dd
      FROM bkpf AS bkpf1
        INNER JOIN bseg  AS bseg1 ON bseg1~belnr EQ bkpf1~belnr AND    "find FI document in bkpf1
                                     bseg1~buzei EQ c_buzei_1   AND
                                     bseg1~gjahr EQ bkpf1~gjahr AND
                                     bseg1~bukrs EQ bkpf1~bukrs
        INNER JOIN bkpf  AS bkpf2 ON bkpf2~belnr EQ bseg1~augbl AND    "find clearing doc in bkpf2
                                     bkpf2~gjahr EQ bkpf1~gjahr AND
                                     bkpf2~bukrs EQ bkpf1~bukrs
        INNER JOIN bseg  AS bseg2 ON bseg2~belnr EQ bkpf2~belnr AND
                                     bseg2~koart EQ c_koart_k   AND
                                     bseg2~gjahr EQ bkpf2~gjahr AND    "INSERT WFSK936421
                                     bseg2~bukrs EQ bkpf2~bukrs        "INSERT WFSK936421
      WHERE bkpf1~belnr EQ lv_belnr AND
            bkpf1~gjahr EQ iv_gjahr.

      LOOP AT et_defrayal_dd INTO wa_defrayal_dd.
        wa_defrayal_dd-doctyp = c_fi_type.

        MODIFY et_defrayal_dd FROM wa_defrayal_dd INDEX sy-tabix.
      ENDLOOP.
    ENDIF. "doc type check

    IF sy-subrc EQ 0.
      LOOP AT et_defrayal_dd INTO wa_defrayal_dd.
        CASE wa_defrayal_dd-zlsch.                               "gets # of days for payment type
          WHEN 'E'.      "BACS
            lv_fc_flag = abap_true.
            lv_days = 2.
          WHEN 'C'.      "CHEQUE
            lv_fc_flag = abap_false.
            wa_defrayal_dd-zdefrayal_date = wa_defrayal_dd-augdt.     "cheque defrayal same as clearing date
            "lv_days = 0.
          WHEN 'F'.      "FOREIGN PAYMENTS
            IF wa_defrayal_dd-waers EQ 'USD' OR
               wa_defrayal_dd-waers EQ 'EUR'.
              lv_fc_flag = abap_false.
              wa_defrayal_dd-zdefrayal_date = wa_defrayal_dd-augdt.   "defrayal same as clearing date
            ELSE.
              lv_fc_flag = abap_true.
            ENDIF.
          WHEN 'I'.
            lv_fc_flag = abap_false.
            wa_defrayal_dd-zdefrayal_date = wa_defrayal_dd-augdt.     "Internal trading defrayal same as clearing date
          WHEN OTHERS.
            lv_days = 0.
        ENDCASE.

        IF lv_fc_flag EQ abap_true.               "calculate defrayal only if its not equal to clearing date

          CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'            "get factory date
            EXPORTING
*             CORRECT_OPTION               = '+'
              date                         = wa_defrayal_dd-augdt
              factory_calendar_id          = c_gb_cal
            IMPORTING
*             DATE                         =
              factorydate                  = lv_facdate
*             WORKINGDAY_INDICATOR         =
            EXCEPTIONS
              calendar_buffer_not_loadable = 1
              correct_option_invalid       = 2
              date_after_range             = 3
              date_before_range            = 4
              date_invalid                 = 5
              factory_calendar_not_found   = 6
              OTHERS                       = 7.
          IF sy-subrc <> 0.
*            RAISE cx_date_convert_error.
          ENDIF.

          ADD lv_days TO lv_facdate.                              "add processing/banking days onto clearing date

          CALL FUNCTION 'FACTORYDATE_CONVERT_TO_DATE'             "convert back to date
            EXPORTING
              factorydate                  = lv_facdate
              factory_calendar_id          = c_gb_cal
            IMPORTING
              date                         = wa_defrayal_dd-zdefrayal_date
            EXCEPTIONS
              calendar_buffer_not_loadable = 1
              factorydate_after_range      = 2
              factorydate_before_range     = 3
              factorydate_invalid          = 4
              factory_calendar_id_missing  = 5
              factory_calendar_not_found   = 6
              OTHERS                       = 7.

          IF sy-subrc <> 0.
*            RAISE cx_date_convert_error.
          ENDIF.
        ENDIF.  " lv_chq_flag

        MODIFY et_defrayal_dd FROM wa_defrayal_dd INDEX sy-tabix.
      ENDLOOP.
    ELSE.
*      RAISE cx_no_data.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
