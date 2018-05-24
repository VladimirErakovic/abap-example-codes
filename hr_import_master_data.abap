*&---------------------------------------------------------------------*
*& Report  /MKBS/HR_IMPORT_MASTER_DATA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  /mkbs/hr_import_master_data.

TYPES: BEGIN OF ty_it0000,
        error TYPE string,
        nachn TYPE pad_nachn,
        vorna TYPE pad_vorna,
        jmbg TYPE zjmbg,
        werks TYPE persa,
        btrtl TYPE btrtl,
        persg TYPE persg,
        persk TYPE persk,
        begda TYPE begda,
        endda TYPE endda,
        plans TYPE plans,
       END OF ty_it0000.

TYPES: BEGIN OF ty_it0006,
       jmbg TYPE zjmbg.
        INCLUDE STRUCTURE /mkbs/hr_address_s.
TYPES END OF ty_it0006.

TYPES: BEGIN OF ty_it0007,
        jmbg TYPE zjmbg,
       pernr TYPE persno,
       begda TYPE begda,
       endda TYPE endda,
       schkz TYPE schkn,
       END OF ty_it0007.

TYPES: BEGIN OF ty_it0009,
       jmbg TYPE zjmbg.
        INCLUDE STRUCTURE /mkbs/hr_bank_s.
TYPES END OF ty_it0009.

TYPES: BEGIN OF ty_it0016,
       jmbg TYPE zjmbg.
        INCLUDE STRUCTURE /mkbs/hr_contract_s.
TYPES END OF ty_it0016.

TYPES: BEGIN OF ty_it0021,
       jmbg TYPE zjmbg.
        INCLUDE STRUCTURE /mkbs/hr_family_s.
TYPES END OF ty_it0021.

TYPES: BEGIN OF ty_it0022,
       jmbg TYPE zjmbg.
        INCLUDE STRUCTURE /mkbs/hr_education_s.
TYPES END OF ty_it0022.

TYPES: BEGIN OF ty_it0024,
       jmbg TYPE zjmbg.
        INCLUDE STRUCTURE /mkbs/hr_qualification_s.
TYPES END OF ty_it0024.

TYPES: BEGIN OF ty_it0105,
       jmbg TYPE zjmbg.
        INCLUDE STRUCTURE /mkbs/hr_contact_s.
TYPES END OF ty_it0105.

TYPES: BEGIN OF ty_it0552,
       jmbg TYPE zjmbg.
        INCLUDE STRUCTURE /mkbs/hr_previous_emp_s.
TYPES END OF ty_it0552.

TYPES: BEGIN OF ty_it2006,
       jmbg TYPE zjmbg.
        INCLUDE STRUCTURE /mkbs/hr_absence_quotas_s.
TYPES END OF ty_it2006.

DATA: gt_0000 TYPE TABLE OF ty_it0000,
      gt_unemployed TYPE TABLE OF ty_it0000,
      gs_0000 TYPE ty_it0000,
      gt_0002 TYPE TABLE OF /mkbs/hr_personal_data_s,
      gs_0002 TYPE /mkbs/hr_personal_data_s,
      gt_0006 TYPE TABLE OF ty_it0006,
      gs_0006 TYPE ty_it0006,
      gt_0007 TYPE TABLE OF ty_it0007,
      gs_0007 TYPE ty_it0007,
      gt_0009 TYPE TABLE OF ty_it0009,
      gs_0009 TYPE ty_it0009,
      gt_0016 TYPE TABLE OF ty_it0016,
      gs_0016 TYPE ty_it0016,
      gt_0021 TYPE TABLE OF ty_it0021,
      gs_0021 TYPE ty_it0021,
      gt_0022 TYPE TABLE OF ty_it0022,
      gs_0022 TYPE ty_it0022,
      gt_0024 TYPE TABLE OF ty_it0024,
      gs_0024 TYPE ty_it0024,
      gt_0105 TYPE TABLE OF ty_it0105,
      gs_0105 TYPE ty_it0105,
      gt_0552 TYPE TABLE OF ty_it0552,
      gs_0552 TYPE ty_it0552,
      gt_2006 TYPE TABLE OF ty_it2006,
      gs_2006 TYPE ty_it2006.

DATA: gv_gbdat TYPE gbdat.

DATA: gs_pa0002 TYPE pa0002.

DATA: gt_excel TYPE STANDARD TABLE OF alsmex_tabline,
      gs_excel TYPE alsmex_tabline,
      gv_error TYPE string.

TYPES: BEGIN OF ty_bapi_errors,
        infty TYPE infty,
        rownum TYPE /mkbs/hr_row_number.
        INCLUDE STRUCTURE bapireturn1.
TYPES  END OF ty_bapi_errors.

DATA: gt_bapi_return TYPE TABLE OF ty_bapi_errors,
      gs_bapi_return TYPE ty_bapi_errors.

DATA: ok_code TYPE sy-ucomm,
      save_ok LIKE ok_code.

" ALV control
DATA: go_alv_cont TYPE REF TO cl_gui_custom_container,
      go_splitter TYPE REF TO cl_gui_splitter_container,
      go_parent_up TYPE REF TO cl_gui_container,
      go_parent_down TYPE REF TO cl_gui_container,
      go_alv_up TYPE REF TO cl_salv_table,
      go_alv_down TYPE REF TO cl_salv_table.
" Display settings
DATA: go_display TYPE REF TO cl_salv_display_settings.
" ALV columns
DATA go_cols TYPE REF TO cl_salv_columns.
*   Exception class
DATA gx_msg TYPE REF TO cx_salv_msg.

DATA: gv_folder TYPE localfile.

TYPES: BEGIN OF ty_infotype,
       infty TYPE infty,
       END OF ty_infotype.

DATA: gt_infty TYPE TABLE OF ty_infotype,
      gs_infty TYPE ty_infotype.

DATA: gt_errors TYPE TABLE OF bapireturn1.

CONSTANTS:
  co_max_col TYPE i VALUE 20,
  co_max_row TYPE i VALUE 9999.

DATA: gv_total_records TYPE i,
      gv_percentage TYPE p,
      gv_infotext TYPE string.


SELECTION-SCREEN BEGIN OF BLOCK main WITH FRAME TITLE infotype.

PARAMETERS:  p_file TYPE string OBLIGATORY.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK first WITH FRAME TITLE row_plus.

PARAMETERS: p_rows TYPE n LENGTH 2.

SELECTION-SCREEN END OF BLOCK first.

SELECTION-SCREEN END OF BLOCK main.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

* Browse the Directories
  CALL METHOD cl_gui_frontend_services=>directory_browse
    CHANGING
      selected_folder = p_file.


AT SELECTION-SCREEN OUTPUT.
  infotype = 'Odaberite folder sa Excel fajlovima'.
  row_plus = 'Broj redova u Excel fajlu pre podataka'.


START-OF-SELECTION.

  gs_infty-infty = '0000'.
  APPEND gs_infty TO gt_infty.
  gs_infty-infty = '0002'.
  APPEND gs_infty TO gt_infty.
  gs_infty-infty = '0006'.
  APPEND gs_infty TO gt_infty.
  gs_infty-infty = '0007'.
  APPEND gs_infty TO gt_infty.
  gs_infty-infty = '0009'.
  APPEND gs_infty TO gt_infty.
  gs_infty-infty = '0016'.
  APPEND gs_infty TO gt_infty.
  gs_infty-infty = '0021'.
  APPEND gs_infty TO gt_infty.
  gs_infty-infty = '0022'.
  APPEND gs_infty TO gt_infty.
  gs_infty-infty = '0024'.
  APPEND gs_infty TO gt_infty.
  gs_infty-infty = '0105'.
  APPEND gs_infty TO gt_infty.
  gs_infty-infty = '0552'.
  APPEND gs_infty TO gt_infty.
  gs_infty-infty = '2006'.
  APPEND gs_infty TO gt_infty.

  DESCRIBE TABLE gt_infty LINES gv_total_records.

  LOOP AT gt_infty INTO gs_infty.

    gv_percentage = (  ( sy-tabix - 1 ) / gv_total_records ) * 100.

    CONCATENATE 'Parsiranje podataka iz Excel tabele - IT' gs_infty INTO gv_infotext.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = gv_percentage
        text       = gv_infotext.

    PERFORM parse_excel_to_internal_tables USING gs_infty-infty.

  ENDLOOP.


  LOOP AT gt_infty INTO gs_infty.

    gv_percentage = ( ( sy-tabix - 1 ) / gv_total_records ) * 100.

    CONCATENATE 'Simuliranje unosa podataka - IT' gs_infty INTO gv_infotext.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = gv_percentage
        text       = gv_infotext.

    PERFORM simulation_and_save USING gs_infty-infty '0'.

  ENDLOOP.


  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = 'Prikazivanje rezultata simulacije'.


  CALL SCREEN 100.


*&---------------------------------------------------------------------*
*&      Form  parse_excel_files_to_internal_tables
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_INFTY    text
*----------------------------------------------------------------------*
FORM parse_excel_to_internal_tables USING p_infty TYPE infty.

  CONCATENATE p_file '\IT' p_infty '.xlsx' INTO gv_folder.
  CLEAR: gt_excel.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = gv_folder
      i_begin_col             = 1
      i_begin_row             = 5 " skip first row
      i_end_col               = co_max_col
      i_end_row               = co_max_row
    TABLES
      intern                  = gt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.


  SORT gt_excel BY row col.

  LOOP AT gt_excel INTO gs_excel.

    CASE p_infty.
      WHEN '0000'.

        CASE gs_excel-col.
          WHEN '1'.
            gs_0000-nachn = gs_excel-value.
          WHEN '2'.
            gs_0000-vorna = gs_excel-value.
          WHEN '3'.
            gs_0000-jmbg = gs_excel-value.
          WHEN '4'.
            gs_0000-werks = gs_excel-value.
          WHEN '5'.
            gs_0000-btrtl = gs_excel-value.
          WHEN '6'.
            gs_0000-persg = gs_excel-value.
          WHEN '7'.
            gs_0000-persk = gs_excel-value.
          WHEN '8'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_0000-begda = gv_gbdat.
          WHEN '9'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_0000-endda = gv_gbdat.
          WHEN '10'.
            gs_0000-plans = gs_excel-value.

        ENDCASE.

        AT END OF row.
          gs_0000-endda = '99991231'.
          APPEND gs_0000 TO gt_0000.
          CLEAR gs_0000.
        ENDAT.


      WHEN '0002'.

        CASE gs_excel-col.
          WHEN '2'.
            gs_0002-nachn = gs_excel-value.
            TRANSLATE gs_0002-nachn TO LOWER CASE.
            TRANSLATE gs_0002-nachn(1) TO UPPER CASE.
          WHEN '3'.
            gs_0002-vorna = gs_excel-value.
            TRANSLATE gs_0002-vorna TO LOWER CASE.
            TRANSLATE gs_0002-vorna(1) TO UPPER CASE.
          WHEN '4'.
            gs_0002-jmbg = gs_excel-value.
          WHEN '5'.
            gs_0002-anred = gs_excel-value.
          WHEN '6'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_0002-gbdat = gv_gbdat.
            gs_0002-begda = gv_gbdat.
          WHEN '7'.
            gs_0002-gbort = gs_excel-value.
          WHEN '8'.
            gs_0002-gbdep = gs_excel-value.
          WHEN '9'.
            gs_0002-gblnd = gs_excel-value.
          WHEN '10'.
            gs_0002-famst = gs_excel-value.
          WHEN '11'.
            gs_0002-srednjeime = gs_excel-value.
          WHEN '12'.
            gs_0002-brojlk = gs_excel-value.
          WHEN '13'.
            gs_0002-opstinalk = gs_excel-value.
          WHEN '14'.
            gs_0002-lbo = gs_excel-value.
          WHEN '15'.
            gs_0002-anzkd = gs_excel-value.
          WHEN '16'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_0002-datumslave = gv_gbdat.
          WHEN '17'.
            gs_0002-nazivslave = gs_excel-value.
        ENDCASE.

        AT END OF row.
          gs_0002-endda = '99991231'.
          gs_0002-sprsl = 'd'.
          gs_0002-natio = 'YU'.
          APPEND gs_0002 TO gt_0002.
          CLEAR gs_0002.
        ENDAT.


      WHEN '0006'.

        CASE gs_excel-col.
          WHEN '4'.
            gs_0006-jmbg = gs_excel-value.
          WHEN '5'.
            gs_0006-subty = gs_excel-value.
          WHEN '6'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_0006-begda = gv_gbdat.
          WHEN '7'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_0006-endda = gv_gbdat.
          WHEN '8'.
            gs_0006-pstlz = gs_excel-value.
          WHEN '9'.
            gs_0006-ort01 = gs_excel-value.
          WHEN '10'.
            gs_0006-stras = gs_excel-value.
          WHEN '11'.
            gs_0006-hsnmr = gs_excel-value.
          WHEN '12'.
            gs_0006-state = gs_excel-value.
        ENDCASE.

        AT END OF row.
          APPEND gs_0006 TO gt_0006.
          CLEAR gs_0006.
        ENDAT.

      WHEN '0007'.

        CASE gs_excel-col.
          WHEN '1'.
            "gs_0007-pernr = gs_excel-value.
            gs_0007-jmbg = gs_excel-value. " change on 17.01.2018
          WHEN '2'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_0007-begda = gv_gbdat.
          WHEN '3'.
            gs_0007-schkz = gs_excel-value.
        ENDCASE.

        AT END OF row.
          gs_0007-endda = '99991231'.
          APPEND gs_0007 TO gt_0007.
          CLEAR gs_0007.
        ENDAT.

      WHEN '0009'.

        CASE gs_excel-col.
          WHEN '4'.
            gs_0009-jmbg = gs_excel-value.
          WHEN '5'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_0009-begda = gv_gbdat.
          WHEN '6'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_0009-endda = gv_gbdat.
          WHEN '7'.
            gs_0009-subty = gs_excel-value.
            gs_0009-bnksa = gs_excel-value.
          WHEN '8'.
            gs_0009-emftx = gs_excel-value.
          WHEN '9'.
            gs_0009-bkplz = gs_excel-value.
          WHEN '10'.
            gs_0009-bkort = gs_excel-value.
          WHEN '11'.
            gs_0009-banks = gs_excel-value.
          WHEN '12'.
            gs_0009-bankl = gs_excel-value.
          WHEN '13'.
            gs_0009-bankn = gs_excel-value.
        ENDCASE.

        AT END OF row.
          gs_0009-zlsch = 'U'. " Metod placanja mora biti "U"
          APPEND gs_0009 TO gt_0009.
          CLEAR gs_0009.
        ENDAT.

      WHEN '0016'.

        CASE gs_excel-col.
          WHEN '4'.
            gs_0016-jmbg = gs_excel-value.
          WHEN '5'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_0016-begda = gv_gbdat.
          WHEN '6'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_0016-ctedt = gv_gbdat.
          WHEN '7'.
            gs_0016-cttyp = gs_excel-value.
            " gs_0016-subty = gs_excel-value. " tanja ne zna da postoji podtip za it0016!?
          WHEN '8'.
            gs_0016-brugovorahr = gs_excel-value.
          WHEN '9'.
            gs_0016-iznos = gs_excel-value.
          WHEN '10'.
            gs_0016-waers = gs_excel-value.
          WHEN '11'.
            gs_0016-brmeseci = gs_excel-value.
          WHEN '12'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_0016-eindt = gv_gbdat.
        ENDCASE.

        AT END OF row.
          gs_0016-endda = '99991231'.
          APPEND gs_0016 TO gt_0016.
          CLEAR gs_0016.
        ENDAT.

      WHEN '0021'.

        CASE gs_excel-col.
          WHEN '4'.
            gs_0021-jmbg = gs_excel-value.
          WHEN '5'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_0021-begda = gv_gbdat.
          WHEN '6'.
            gs_0021-favor = gs_excel-value.
          WHEN '7'.
            gs_0021-fanam = gs_excel-value.
          WHEN '8'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_0021-fgbdt = gv_gbdat.
*          WHEN '9'.
*            gs_0021-gesc1 = gs_excel-value. " ovo i ne treba jer je isto sto i fasex
*          WHEN '10'.
*            gs_0021-gesc2 = gs_excel-value.  " kljuc pola
          WHEN '11'.
            gs_0021-fasex = gs_excel-value.
        ENDCASE.

        AT END OF row.
          gs_0021-endda = '99991231'.
          gs_0021-famsa = '2'. " kljuc za dete
          gs_0021-subty = '2'.
          APPEND gs_0021 TO gt_0021.
          CLEAR gs_0021.
        ENDAT.

      WHEN '0022'.

        CASE gs_excel-col.
          WHEN '4'.
            gs_0022-jmbg = gs_excel-value.
          WHEN '5'.
            gs_0022-slart = gs_excel-value.
            gs_0022-subty = gs_excel-value.
          WHEN '6'.
            gs_0022-insti = gs_excel-value.
          WHEN '7'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_0022-begda = gv_gbdat.
          WHEN '8'.
            gs_0022-slabs = gs_excel-value.
          WHEN '9'.
            gs_0022-sland = gs_excel-value.
        ENDCASE.

        AT END OF row.
          gs_0022-endda = '99991231'.
          APPEND gs_0022 TO gt_0022.
          CLEAR gs_0022.
        ENDAT.

      WHEN '0024'.

        CASE gs_excel-col.
          WHEN '4'.
            gs_0024-jmbg = gs_excel-value.
          WHEN '5'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_0024-begda = gv_gbdat.
          WHEN '6'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_0024-endda = gv_gbdat.
          WHEN '7'.
            gs_0024-quali = gs_excel-value.
          WHEN '8'.
            gs_0024-auspr = gs_excel-value.
        ENDCASE.

        AT END OF row.
          APPEND gs_0024 TO gt_0024.
          CLEAR gs_0024.
        ENDAT.

      WHEN '0105'.

        CASE gs_excel-col.
          WHEN '4'.
            gs_0105-jmbg = gs_excel-value.
          WHEN '5'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_0105-begda = gv_gbdat.
          WHEN '6'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_0105-endda = gv_gbdat.
          WHEN '7'.
            gs_0105-subty = gs_excel-value.
            gs_0105-usrty = gs_excel-value. " ovo mozda i ne treba
          WHEN '8'.
            IF gs_0105-subty = '0010' OR gs_0105-subty = '0030'.
              gs_0105-usrid_long = gs_excel-value.
            ELSEIF gs_0105-subty = 'CELL'
              OR gs_0105-subty = 'MPHN' OR gs_0105-subty = 'SIND'.
              gs_0105-usrid = gs_excel-value.
            ENDIF.
        ENDCASE.

        AT END OF row.
          APPEND gs_0105 TO gt_0105.
          CLEAR gs_0105.
        ENDAT.

      WHEN '0552'.

        CASE gs_excel-col.
          WHEN '4'.
            gs_0552-jmbg = gs_excel-value.
          WHEN '5'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_0552-begda = gv_gbdat.
          WHEN '6'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_0552-endda = gv_gbdat.
          WHEN '7'.
            " text1 ????
          WHEN '8'.
            gs_0552-cvtyp = gs_excel-value.
            gs_0552-subty = gs_excel-value.
        ENDCASE.

        AT END OF row.
          APPEND gs_0552 TO gt_0552.
          CLEAR gs_0552.
        ENDAT.

      WHEN '2006'.

        CASE gs_excel-col.
          WHEN '4'.
            gs_2006-jmbg = gs_excel-value.
          WHEN '5'.
            gs_2006-subty = gs_excel-value.
            gs_2006-ktart = gs_excel-value. " ovo mozda i ne treba
          WHEN '6'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_2006-begda = gv_gbdat.
          WHEN '7'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_2006-endda = gv_gbdat.
          WHEN '8'.
            gs_2006-anzhl = gs_excel-value.
          WHEN '9'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_2006-desta = gv_gbdat.
          WHEN '10'.
            CONCATENATE gs_excel-value+6(4) gs_excel-value+3(2) gs_excel-value(2) INTO gv_gbdat.
            gs_2006-deend = gv_gbdat.
        ENDCASE.

        AT END OF row.
          APPEND gs_2006 TO gt_2006.
          CLEAR gs_2006.
        ENDAT.

    ENDCASE.

  ENDLOOP.

ENDFORM.                    "parse_excel_files_to_internal_tables
*&---------------------------------------------------------------------*
*&      Form  simulation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_INFTY    text
*----------------------------------------------------------------------*
FORM simulation_and_save USING p_infty TYPE infty
                               p_action TYPE c.

  DATA: lv_tabix LIKE sy-tabix.

  " JMBG function return paramers
  DATA: lv_jmbg_return TYPE c LENGTH 1,
        lv_jmbg_msg TYPE string.

  CASE p_infty.
    WHEN '0000'.
      LOOP AT gt_0000 INTO gs_0000.
        CLEAR gs_pa0002.
        " JMBG validation
        CALL FUNCTION '/MKBS/HR_CHECK_JMBG'
          EXPORTING
            im_jmbg   = gs_0000-jmbg
          IMPORTING
            ex_return = lv_jmbg_return
            ex_msg    = lv_jmbg_msg.

        IF lv_jmbg_return = 0.
          SELECT SINGLE * INTO gs_pa0002
            FROM pa0002
           WHERE jmbg = gs_0000-jmbg.
          IF sy-subrc <> 0.
            IF p_action = '0'.
              APPEND gs_0000 TO gt_unemployed.
            ELSE.
              PERFORM hire USING gs_0000.
            ENDIF.
          ENDIF.
        ELSE.
          gs_0000-error = lv_jmbg_msg. " error message from function
          APPEND gs_0000 TO gt_unemployed.
        ENDIF.
      ENDLOOP.
    WHEN '0002'.
      LOOP AT gt_0002 INTO gs_0002.
        lv_tabix = sy-tabix.
        CLEAR gs_pa0002.
        SELECT SINGLE * INTO gs_pa0002
          FROM pa0002
         WHERE jmbg = gs_0002-jmbg.
        IF sy-subrc = 0.
          IF p_action = '0'.
            PERFORM infotype_operation
              USING p_infty '' gs_0002-begda gs_0002-endda 'X' gs_0002 lv_tabix.
          ELSE.
            PERFORM infotype_operation
              USING p_infty '' gs_0002-begda gs_0002-endda '' gs_0002 lv_tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
    WHEN '0006'.
      LOOP AT gt_0006 INTO gs_0006.
        lv_tabix = sy-tabix.
        CLEAR gs_pa0002.
        SELECT SINGLE * INTO gs_pa0002
          FROM pa0002
         WHERE jmbg = gs_0006-jmbg.
        IF sy-subrc = 0.
          IF p_action = '0'.
            PERFORM infotype_operation
              USING p_infty gs_0006-subty gs_0006-begda gs_0006-endda 'X' gs_0006 lv_tabix.
          ELSE.
            PERFORM infotype_operation
              USING p_infty gs_0006-subty gs_0006-begda gs_0006-endda '' gs_0006 lv_tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
    WHEN '0007'.
      LOOP AT gt_0007 INTO gs_0007.
        lv_tabix = sy-tabix.
        CLEAR gs_pa0002.
*        SELECT SINGLE * INTO gs_pa0002
*          FROM pa0002
*         WHERE pernr = gs_0007-pernr.
        SELECT SINGLE * INTO gs_pa0002  " change on 17.01.2018
          FROM pa0002
         WHERE jmbg = gs_0007-jmbg.
        IF sy-subrc = 0.
          IF p_action = '0'.
            PERFORM infotype_operation
              USING p_infty '' gs_0007-begda gs_0007-endda 'X' gs_0007 lv_tabix.
          ELSE.
            PERFORM infotype_operation
              USING p_infty '' gs_0007-begda gs_0007-endda '' gs_0007 lv_tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
    WHEN '0009'.
      LOOP AT gt_0009 INTO gs_0009.
        lv_tabix = sy-tabix.
        CLEAR gs_pa0002.
        SELECT SINGLE * INTO gs_pa0002
          FROM pa0002
         WHERE jmbg = gs_0009-jmbg.
        IF sy-subrc = 0.
          IF p_action = '0'.
            PERFORM infotype_operation
              USING p_infty gs_0009-subty gs_0009-begda gs_0009-endda 'X' gs_0009 lv_tabix.
          ELSE.
            PERFORM infotype_operation
              USING p_infty gs_0009-subty gs_0009-begda gs_0009-endda '' gs_0009 lv_tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
    WHEN '0016'.
      LOOP AT gt_0016 INTO gs_0016.
        lv_tabix = sy-tabix.
        CLEAR gs_pa0002.
        SELECT SINGLE * INTO gs_pa0002
          FROM pa0002
         WHERE jmbg = gs_0016-jmbg.
        IF sy-subrc = 0.
          IF p_action = '0'.
            PERFORM infotype_operation  " izbaceno gs_0016-subty
              USING p_infty '' gs_0016-begda gs_0016-endda 'X' gs_0016 lv_tabix.
          ELSE.
            PERFORM infotype_operation  " izbaceno gs_0016-subty
              USING p_infty '' gs_0016-begda gs_0016-endda '' gs_0016 lv_tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
    WHEN '0021'.
      LOOP AT gt_0021 INTO gs_0021.
        lv_tabix = sy-tabix.
        CLEAR gs_pa0002.
        SELECT SINGLE * INTO gs_pa0002
          FROM pa0002
         WHERE jmbg = gs_0021-jmbg.
        IF sy-subrc = 0.
          IF p_action = '0'.
            PERFORM infotype_operation
              USING p_infty gs_0021-subty gs_0021-begda gs_0021-endda 'X' gs_0021 lv_tabix.
          ELSE.
            PERFORM infotype_operation
              USING p_infty gs_0021-subty gs_0021-begda gs_0021-endda '' gs_0021 lv_tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
    WHEN '0022'.
      LOOP AT gt_0022 INTO gs_0022.
        lv_tabix = sy-tabix.
        CLEAR gs_pa0002.
        SELECT SINGLE * INTO gs_pa0002
          FROM pa0002
         WHERE jmbg = gs_0022-jmbg.
        IF sy-subrc = 0.
          IF p_action = '0'.
            PERFORM infotype_operation
              USING p_infty gs_0022-subty gs_0022-begda gs_0022-endda 'X' gs_0022 lv_tabix.
          ELSE.
            PERFORM infotype_operation
              USING p_infty gs_0022-subty gs_0022-begda gs_0022-endda '' gs_0022 lv_tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
    WHEN '0024'.
      LOOP AT gt_0024 INTO gs_0024.
        lv_tabix = sy-tabix.
        CLEAR gs_pa0002.
        SELECT SINGLE * INTO gs_pa0002
          FROM pa0002
         WHERE jmbg = gs_0024-jmbg.
        IF sy-subrc = 0.
          IF p_action = '0'.
            PERFORM infotype_operation
              USING p_infty '' gs_0024-begda gs_0024-endda 'X' gs_0024 lv_tabix.
          ELSE.
            PERFORM infotype_operation
              USING p_infty '' gs_0024-begda gs_0024-endda '' gs_0024 lv_tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
    WHEN '0105'.
      LOOP AT gt_0105 INTO gs_0105.
        lv_tabix = sy-tabix.
        CLEAR gs_pa0002.
        SELECT SINGLE * INTO gs_pa0002
          FROM pa0002
         WHERE jmbg = gs_0105-jmbg.
        IF sy-subrc = 0.
          IF p_action = '0'.
            PERFORM infotype_operation
              USING p_infty gs_0105-subty gs_0105-begda gs_0105-endda 'X' gs_0105 lv_tabix.
          ELSE.
            PERFORM infotype_operation
              USING p_infty gs_0105-subty gs_0105-begda gs_0105-endda '' gs_0105 lv_tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
    WHEN '0552'.
      LOOP AT gt_0552 INTO gs_0552.
        lv_tabix = sy-tabix.
        CLEAR gs_pa0002.
        SELECT SINGLE * INTO gs_pa0002
          FROM pa0002
         WHERE jmbg = gs_0552-jmbg.
        IF sy-subrc = 0.
          IF p_action = '0'.
            PERFORM infotype_operation
              USING p_infty gs_0552-subty gs_0552-begda gs_0552-endda 'X' gs_0552 lv_tabix.
          ELSE.
            PERFORM infotype_operation
              USING p_infty gs_0552-subty gs_0552-begda gs_0552-endda '' gs_0552 lv_tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
    WHEN '2006'.
      LOOP AT gt_2006 INTO gs_2006.
        lv_tabix = sy-tabix.
        CLEAR gs_pa0002.
        SELECT SINGLE * INTO gs_pa0002
          FROM pa0002
         WHERE jmbg = gs_2006-jmbg.
        IF sy-subrc = 0.
          IF p_action = '0'.
            PERFORM infotype_operation
              USING p_infty gs_2006-subty gs_2006-begda gs_2006-endda 'X' gs_2006 lv_tabix.
          ELSE.
            PERFORM infotype_operation
              USING p_infty gs_2006-subty gs_2006-begda gs_2006-endda '' gs_2006 lv_tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
  ENDCASE.

ENDFORM.                    "simulation
*&---------------------------------------------------------------------*
*&      Form  hire
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DATA     text
*----------------------------------------------------------------------*
FORM hire USING p_data TYPE ty_it0000.

  DATA: lt_prop_values TYPE TABLE OF pprop,
        ls_prop_values TYPE pprop,
        ls_0002 TYPE /mkbs/hr_personal_data_s,
        ls_return TYPE bapireturn,
        ls_return1 TYPE bapireturn1,
        ls_hr_return TYPE hrhrmm_msg.


  LOOP AT gt_0002 INTO gs_0002.
    IF p_data-jmbg = gs_0002-jmbg.
      ls_0002 = gs_0002.
    ENDIF.
  ENDLOOP.

  ls_prop_values-infty = '0000'.
  ls_prop_values-fname = 'P0000-MASSG'.
  ls_prop_values-fval = '01'.
  ls_prop_values-seqnr = '00'.
  APPEND ls_prop_values TO lt_prop_values.
  CLEAR ls_prop_values.
  ls_prop_values-infty = '0001'.
  ls_prop_values-fname = 'P0001-BUKRS'.
  ls_prop_values-fval = p_data-werks.
  ls_prop_values-seqnr = '00'.
  APPEND ls_prop_values TO lt_prop_values.
  CLEAR ls_prop_values.
  ls_prop_values-infty = '0001'.
  ls_prop_values-fname = 'P0001-BTRTL'.
  ls_prop_values-fval = p_data-btrtl.
  ls_prop_values-seqnr = '00'.
  APPEND ls_prop_values TO lt_prop_values.
  CLEAR ls_prop_values.
  ls_prop_values-infty = '0002'.
  ls_prop_values-fname = 'P0002-ANRED'.
  ls_prop_values-fval = ls_0002-anred.
  ls_prop_values-seqnr = '00'.
  APPEND ls_prop_values TO lt_prop_values.
  CLEAR ls_prop_values.
  ls_prop_values-infty = '0002'.
  ls_prop_values-fname = 'P0002-VORNA'.
  ls_prop_values-fval = ls_0002-vorna.
  ls_prop_values-seqnr = '00'.
  APPEND ls_prop_values TO lt_prop_values.
  CLEAR ls_prop_values.
  ls_prop_values-infty = '0002'.
  ls_prop_values-fname = 'P0002-NACHN'.
  ls_prop_values-fval = ls_0002-nachn.
  ls_prop_values-seqnr = '00'.
  APPEND ls_prop_values TO lt_prop_values.
  CLEAR ls_prop_values.
  ls_prop_values-infty = '0002'.
  ls_prop_values-fname = 'P0002-GBDAT'.
  ls_prop_values-fval = ls_0002-gbdat.
  ls_prop_values-seqnr = '00'.
  APPEND ls_prop_values TO lt_prop_values.
  CLEAR ls_prop_values.
  ls_prop_values-infty = '0002'.
  ls_prop_values-fname = 'P0002-SPRSL'.
  ls_prop_values-fval = 'd'.
  ls_prop_values-seqnr = '00'.
  APPEND ls_prop_values TO lt_prop_values.
  CLEAR ls_prop_values.
  ls_prop_values-infty = '0002'.
  ls_prop_values-fname = 'P0002-NATIO'.
  ls_prop_values-fval = 'YU'.
  ls_prop_values-seqnr = '00'.
  APPEND ls_prop_values TO lt_prop_values.
  CLEAR ls_prop_values.
  ls_prop_values-infty = '0002'.
  ls_prop_values-fname = 'P0002-JMBG'.
  ls_prop_values-fval = p_data-jmbg.
  ls_prop_values-seqnr = '00'.
  APPEND ls_prop_values TO lt_prop_values.


  CALL FUNCTION 'HR_MAINTAIN_MASTERDATA'
    EXPORTING
      pernr              = '00000000'
      massn              = 'Z2' " tip radnje Z2 mini master
      actio              = 'INS'
      tclas              = 'A'
      begda              = p_data-begda
      endda              = p_data-endda
      werks              = p_data-werks
      persg              = p_data-persg
      persk              = p_data-persk
      plans              = p_data-plans
      dialog_mode        = '0' "(1 or 0-foreground or Background)
      no_existence_check = 'X'
    IMPORTING
      return             = ls_return
      return1            = ls_return1
      hr_return          = ls_hr_return
    TABLES
      proposed_values    = lt_prop_values. " must be filled before call


  IF ls_return1 IS NOT INITIAL.
    APPEND ls_return1 TO gt_errors.
  ENDIF.

ENDFORM.                    "hire
*&---------------------------------------------------------------------*
*&      Form  infotype_operation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM infotype_operation USING p_infty TYPE infty
                              p_subty TYPE subty
                              p_begda TYPE begda
                              p_endda TYPE endda
                              p_simul TYPE c
                              p_struc TYPE any
                              p_rownm TYPE sytabix.

  FIELD-SYMBOLS: <fs_infty> TYPE ANY,
                 <fs_ival> TYPE infty,
                 <fs_pernr> TYPE persno,
                 <fs_subty_copy> TYPE ANY.

  DATA: lo_data TYPE REF TO data.

  DATA: ls_return TYPE bapireturn1,
        it_name TYPE c LENGTH 5,
        lv_operation TYPE pspar-actio,
        ls_bapiret TYPE bapireturn1.


  CALL FUNCTION 'HR_INFOTYPE_CHECKEXISTENCE'
    EXPORTING
      number        = gs_pa0002-pernr
      infty         = p_infty
      subtype       = p_subty
      validitybegin = p_begda
      validityend   = p_endda
    IMPORTING
      return        = ls_bapiret.


  IF ls_bapiret IS INITIAL.
    lv_operation = 'MOD'.
  ELSE.
    lv_operation = 'INS'.
  ENDIF.

  CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
    EXPORTING
      number = gs_pa0002-pernr.

  CONCATENATE 'P' p_infty INTO it_name.
  CREATE DATA lo_data TYPE (it_name).

  ASSIGN lo_data->* TO <fs_infty>.

  MOVE-CORRESPONDING p_struc TO <fs_infty>.

  IF lv_operation = 'MOD'.
    ASSIGN COMPONENT 'INFTY' OF STRUCTURE <fs_infty> TO <fs_ival>.
    <fs_ival> = p_infty.
    ASSIGN COMPONENT 'PERNR' OF STRUCTURE <fs_infty> TO <fs_pernr>.
    <fs_pernr> = gs_pa0002-pernr.
    CASE p_infty.
      WHEN '0006'.
        ASSIGN COMPONENT 'ANSSA' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
        <fs_subty_copy> = p_subty.
      WHEN '0009'.
        ASSIGN COMPONENT 'BNKSA' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
        <fs_subty_copy> = p_subty.
      WHEN '0021'.
        ASSIGN COMPONENT 'FAMSA' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
        <fs_subty_copy> = p_subty.
      WHEN '0022'.
        ASSIGN COMPONENT 'SLART' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
        <fs_subty_copy> = p_subty.
      WHEN '0105'.
        ASSIGN COMPONENT 'USRTY' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
        <fs_subty_copy> = p_subty.
      WHEN '0552'.
        ASSIGN COMPONENT 'CVTYP' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
        <fs_subty_copy> = p_subty.
      WHEN '2006'.
        ASSIGN COMPONENT 'KTART' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
        <fs_subty_copy> = p_subty.
    ENDCASE.
  ENDIF.

  CALL FUNCTION 'HR_INFOTYPE_OPERATION'
    EXPORTING
      infty         = p_infty
      number        = gs_pa0002-pernr
      subtype       = p_subty
      validityend   = p_endda
      validitybegin = p_begda
      record        = <fs_infty>
      operation     = lv_operation
      nocommit      = p_simul
    IMPORTING
      return        = ls_return.

  IF ls_return IS NOT INITIAL.
    gs_bapi_return-infty = p_infty.
    gs_bapi_return-rownum = p_rownm + p_rows.
    MOVE-CORRESPONDING ls_return TO gs_bapi_return.
    APPEND gs_bapi_return TO gt_bapi_return.
  ENDIF.

  CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
    EXPORTING
      number = gs_pa0002-pernr.


ENDFORM.                    "infotype_operation
*----------------------------------------------------------------------*
*  MODULE status_0100 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET TITLEBAR 'T100'.
  SET PF-STATUS 'ST0100'.

  IF go_alv_cont IS NOT BOUND.
    PERFORM create_container_splitter.
    PERFORM create_alv USING gt_unemployed gt_bapi_return.
    IF gt_bapi_return IS INITIAL.
      MESSAGE
      'Simulacija je završena bez grešaka, možete sačuvati podatke.'
      TYPE 'S'.
    ENDIF.
  ENDIF.


ENDMODULE.                    "status_0100 OUTPUT
*----------------------------------------------------------------------*
*  MODULE user_command_0100 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'SAVE'.
      IF gt_bapi_return IS INITIAL.

        DESCRIBE TABLE gt_infty LINES gv_total_records.
        LOOP AT gt_infty INTO gs_infty.

          gv_percentage = (  ( sy-tabix - 1 ) / gv_total_records ) * 100.

          CONCATENATE 'Snimanje u bazu podataka - IT' gs_infty INTO gv_infotext.

          CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
            EXPORTING
              percentage = gv_percentage
              text       = gv_infotext.

          PERFORM simulation_and_save USING gs_infty-infty '1'.

        ENDLOOP.

        IF gt_errors IS NOT INITIAL.
          PERFORM show_errors.
        ELSE.
          MESSAGE 'Podaci su uspešno snimljeni.' TYPE 'S'.
        ENDIF.

      ELSE.
        MESSAGE 'Morate ispraviti sve greške pre snimanja.' TYPE 'E'.
      ENDIF.

  ENDCASE.

ENDMODULE.                    "user_command_0100 INPUT
*----------------------------------------------------------------------*
*  MODULE exit_command_0100 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE exit_command_0100 INPUT.

  DATA: lv_answer TYPE c LENGTH 1.

  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'EXIT' OR 'BACK' OR 'CANCEL'.

      /mkbs/cl_popup_to_confirm=>exit(
           IMPORTING  user_answer = lv_answer
      ).

      IF lv_answer = '1'.
        LEAVE PROGRAM.
      ENDIF.

  ENDCASE.

ENDMODULE.                    "exit_command_0100 INPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_splitter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_container_splitter.

  CREATE OBJECT go_alv_cont
    EXPORTING
      container_name = 'CC_ALV'.
*
**   create splitter container in which to place graphics
  CREATE OBJECT go_splitter
    EXPORTING
      parent  = go_alv_cont
      rows    = 2
      columns = 1
      align   = 15. " (splitter fills the hole custom container)

**   get part of splitter container for 1st table
  go_splitter->get_container(
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = go_parent_up ).
**   get part of splitter container for 2nd table
  go_splitter->get_container(
    EXPORTING
      row       = 2
      column    = 1
    RECEIVING
      container = go_parent_down ).
********************************************dodao 06.11.2013
  go_splitter->set_row_sash( id    = 1
                              type  = go_splitter->type_movable
                              value = go_splitter->false ).
************************************************************


ENDFORM.                    "create_container_splitter
*&---------------------------------------------------------------------*
*&      Form  create_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TABLE    text
*----------------------------------------------------------------------*
FORM create_alv USING p_table_up TYPE ANY TABLE
                      p_table_down TYPE ANY TABLE.

  TRY.
      cl_salv_table=>factory(
        EXPORTING
          r_container = go_parent_up
        IMPORTING
          r_salv_table = go_alv_up
        CHANGING
          t_table      = p_table_up ).
    CATCH cx_salv_msg INTO gx_msg.
  ENDTRY.

*   get display object
  go_display = go_alv_up->get_display_settings( ).
  go_display->set_list_header( 'Nezaposleni' ).
*
*   set ZEBRA pattern
  go_display->set_striped_pattern( 'X' ).

  go_cols = go_alv_up->get_columns( ).
*   set the Column optimization
  go_cols->set_optimize( 'X' ).

* Displaying the ALV
  go_alv_up->display( ).

***************************************************************

  TRY.
      cl_salv_table=>factory(
        EXPORTING
          r_container = go_parent_down
        IMPORTING
          r_salv_table = go_alv_down
        CHANGING
          t_table      = p_table_down ).
    CATCH cx_salv_msg INTO gx_msg.
  ENDTRY.

*   get display object
  go_display = go_alv_down->get_display_settings( ).
  go_display->set_list_header( 'Greške pri upisu' ).
*
*   set ZEBRA pattern
  go_display->set_striped_pattern( 'X' ).

  go_cols = go_alv_down->get_columns( ).
*   set the Column optimization
  go_cols->set_optimize( 'X' ).

* Displaying the ALV
  go_alv_down->display( ).

ENDFORM.                    "create_alv
*&---------------------------------------------------------------------*
*&      Form  show_errors
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM show_errors.

  DATA: lo_table TYPE REF TO cl_salv_table,
        lo_cols TYPE REF TO cl_salv_columns,
        lx_msg TYPE REF TO cx_salv_msg.

  TRY.
      cl_salv_table=>factory(
       IMPORTING
         r_salv_table = lo_table
 CHANGING
         t_table = gt_errors ).
    CATCH cx_salv_msg INTO lx_msg.
  ENDTRY.

  lo_cols = lo_table->get_columns( ).
*   set the Column optimization
  lo_cols->set_optimize( 'X' ).

  lo_table->set_screen_popup(
     start_column = 5
     end_column   = 180
     start_line   = 3
     end_line     = 23 ).

  lo_table->display( ).

ENDFORM.                    "show_errors