*&---------------------------------------------------------------------*
*& Report  /MKBS/HR_CONTRACTS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  /mkbs/hr_contracts.

INCLUDE ole2incl.

" Edit text control
DATA: go_container TYPE REF TO cl_gui_custom_container,
      go_editor TYPE REF TO cl_gui_textedit.
" ALV control
DATA: go_alv_dock_container TYPE REF TO cl_gui_docking_container,
      go_alv TYPE REF TO cl_salv_table.
*   PF-STATUS functions
DATA go_functions TYPE REF TO cl_salv_functions_list.
" ALV columns
DATA go_cols TYPE REF TO cl_salv_columns.
*   Display settings
DATA go_display TYPE REF TO cl_salv_display_settings.
*   Layout settings
DATA: go_layout TYPE REF TO cl_salv_layout,
      gv_variant TYPE slis_vari,
      gs_key TYPE salv_s_layout_key.
*   Events
DATA go_events TYPE REF TO cl_salv_events_table.
" ALV Exception class
DATA gx_msg TYPE REF TO cx_salv_msg.

DATA: gv_text TYPE string.

DATA: ok_code TYPE sy-ucomm,
      save_ok LIKE ok_code,
      gv_mode TYPE n,
      gv_anex TYPE n.

DATA: gw_cont TYPE /mkbs/hr_cntrct,
      gv_pernr TYPE persno,
      gv_pernr_old TYPE persno,
      gv_cntid TYPE /mkbs/hr_cntid,
      gv_bukrs TYPE bukrs,
      gv_ok_bukrs TYPE n,
      emp_name TYPE smnam,
      butxt TYPE butxt,
      gv_cttxt TYPE cttxt,
      gv_approved TYPE /mkbs/hr_ok.

TYPES: BEGIN OF ty_contracts,
        cntid TYPE /mkbs/hr_cntid,
        anxid TYPE /mkbs/hr_anxid,
        tmpid TYPE /mkbs/hr_tmpid,
        pernr TYPE persno,
        sname TYPE smnam,
        ctype TYPE cttyp,
        cttxt TYPE cttxt,
        begda TYPE begda,
        endda TYPE endda,
       END OF ty_contracts.

DATA: gt_contracts TYPE TABLE OF ty_contracts,
      gw_contracts TYPE ty_contracts.
FIELD-SYMBOLS <contract> TYPE ty_contracts.
"   For tcode url link
DATA: gv_url_content TYPE string.

TYPES: BEGIN OF ty_bukrs,
        sign TYPE sign,
        option TYPE option,
        low TYPE bukrs,
        high TYPE bukrs,
       END OF ty_bukrs.

DATA: ls_bukrs TYPE ty_bukrs,
        gv_msg TYPE string.

*&---------------------------------------------------------------------*
* O L E   O B J E C T
*&---------------------------------------------------------------------*
DATA : gv_directory     TYPE string, 
       gv_directory_url TYPE string.

*----------------------------------------------------------------------*
* EVENT HANDLER LOCAL CLASS DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.

    METHODS:
      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column.

ENDCLASS.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
* EVENT HANDLER LOCAL CLASS IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_double_click.

    IF gv_mode <> 2.
      READ TABLE gt_contracts INTO gw_contracts INDEX row.
      PERFORM load_data USING gw_contracts-cntid
                              gw_contracts-anxid
                              gw_contracts-pernr
                              gw_contracts-ctype
                              gw_contracts-begda
                              gw_contracts-endda.
      CALL SCREEN 100.
    ENDIF.

  ENDMETHOD.                    "on_link_click

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

DATA go_event_handler TYPE REF TO lcl_event_handler.

*----------------------------------------------------------------------*
*  MODULE status_0100 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET TITLEBAR 'T100'.

  DATA: lt_fcode TYPE TABLE OF sy-ucomm.

  CASE gv_mode.
    WHEN 0.
      CLEAR lt_fcode.
      APPEND 'CHANGE' TO lt_fcode.
      APPEND 'SAVE' TO lt_fcode.
      APPEND 'PRINT' TO lt_fcode.
      SET PF-STATUS 'ST0100' EXCLUDING lt_fcode.
    WHEN 1.
      CLEAR lt_fcode.
      APPEND 'CHANGE' TO lt_fcode.
      APPEND 'PRINT' TO lt_fcode.
      SET PF-STATUS 'ST0100' EXCLUDING lt_fcode.
    WHEN 2.
      CLEAR lt_fcode.
      APPEND 'CHANGE' TO lt_fcode.
      APPEND 'PRINT' TO lt_fcode.
      SET PF-STATUS 'ST0100' EXCLUDING lt_fcode.
    WHEN 3.
      CLEAR lt_fcode.
      APPEND 'SAVE' TO lt_fcode.
      IF gv_approved <> 'X'.
        APPEND 'PRINT' TO lt_fcode. " nesto ne radi proveri zasto
      ENDIF.
      SET PF-STATUS 'ST0100' EXCLUDING lt_fcode.
  ENDCASE.


  LOOP AT SCREEN.

    CASE gv_mode.
      WHEN 0. " Initial mode
        IF gv_ok_bukrs = 0.
          IF screen-name = 'GV_PERNR'.
            screen-input = 0.
          ENDIF.
        ENDIF.
        IF screen-name = 'GEN_BTN'.
          screen-input = 0.
        ENDIF.
        IF screen-group1 = 'KEY' OR
         screen-group1 = 'SRC'.
          screen-input = 0.
        ENDIF.
        IF screen-name = 'CHNG_BTN'.
          screen-invisible = 1.
          screen-input = 0.
        ENDIF.
      WHEN 1. " New mode
        IF ( screen-name <> 'GW_CONT-TMPID' AND
           screen-name <> 'GEN_BTN' ) OR
           screen-group1 = 'EMP'.
          screen-input = 0.
        ENDIF.
        IF screen-name = 'CHNG_BTN'.
          screen-invisible = 1.
          screen-input = 0.
        ENDIF.
      WHEN 2. " Change mode
        IF screen-group1 = 'KEY' OR
           screen-group1 = 'SRC' OR
           screen-group1 = 'EMP' OR
           screen-name = 'GEN_BTN'.
          screen-input = 0.
        ENDIF.
        IF screen-name = 'CHNG_BTN'.
          screen-invisible = 1.
          screen-input = 0.
        ENDIF.
      WHEN 3. " Saved mode
        IF screen-name = 'GEN_BTN' OR
           screen-group1 = 'EMP' OR
           screen-group1 = 'KEY' OR
           screen-group1 = 'SRC'.
          screen-input = 0.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

  IF gv_pernr IS NOT INITIAL AND ( gv_mode = 0 OR gv_mode = 3 ).

    SELECT SINGLE p16~brugovorahr INTO gv_cntid
      FROM pa0016 AS p16
      JOIN pa0001 AS p1
        ON p1~pernr = p16~pernr
     WHERE p1~pernr = gv_pernr
       AND p1~bukrs = gv_bukrs
       AND p1~begda <= sy-datum " dodao 05.01.2018
       AND p1~endda >= sy-datum.  " a mozda i PERSG = 1

    IF sy-subrc <> 0.
      CLEAR: gw_cont.
      go_editor->delete_text( ).
      CLEAR gv_text.
      IF go_alv_dock_container IS BOUND.
        FREE go_alv.
        go_alv_dock_container->free( ).
        FREE go_alv_dock_container.
      ENDIF.
    ENDIF.
  ENDIF.

  IF gv_pernr IS NOT INITIAL AND ( gv_mode = 0 OR gv_mode = 3 ).
    IF go_alv_dock_container IS NOT BOUND.
      PERFORM load_alv_data.
      PERFORM create_alv.
    ELSE.
      IF gv_pernr_old <> gv_pernr.
        PERFORM load_alv_data.
        go_alv->refresh( refresh_mode = if_salv_c_refresh=>soft ).
        cl_gui_cfw=>flush( ).
      ENDIF.
    ENDIF.
    gv_pernr_old = gv_pernr.
  ENDIF.

  IF go_container IS INITIAL.
    PERFORM create_edit_text.
  ENDIF.
  IF go_editor IS NOT INITIAL.
    IF gv_mode = 2.
      go_editor->set_readonly_mode(
        EXPORTING
          readonly_mode = 0 ).
    ELSE.
      go_editor->set_readonly_mode(
        EXPORTING
          readonly_mode = 1 ).
    ENDIF.

    go_editor->set_textstream(
      EXPORTING
        text = gv_text ).

  ENDIF.


ENDMODULE.                    "status_0100 OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_edit_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_edit_text .

  CREATE OBJECT go_container
    EXPORTING
      container_name              = 'CC_TEXT'.

  CREATE OBJECT go_editor
    EXPORTING
      parent                 = go_container.

  go_editor->set_toolbar_mode(
    EXPORTING
      toolbar_mode = cl_gui_textedit=>false ).

  go_editor->set_statusbar_mode(
    EXPORTING
      statusbar_mode = cl_gui_textedit=>false ).

ENDFORM.                    "create_edit_text
*----------------------------------------------------------------------*
*  MODULE user_command_0100 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: lv_bukrs TYPE bukrs.

  save_ok = ok_code.
  CLEAR ok_code.

  IF gv_bukrs IS NOT INITIAL.
      CASE gv_bukrs.
        WHEN 'MKC'.
          AUTHORITY-CHECK OBJECT 'P_ORGIN'
            ID 'AUTHC' FIELD '*'
            ID 'INFTY' DUMMY
            ID 'PERSA' FIELD 'MKG'
            ID 'PERSG' DUMMY
            ID 'PERSK' DUMMY
            ID 'SUBTY' DUMMY
            ID 'VDSK1' DUMMY.
        WHEN OTHERS.
          AUTHORITY-CHECK OBJECT 'P_ORGIN'
            ID 'AUTHC' FIELD '*'
            ID 'INFTY' DUMMY
            ID 'PERSA' FIELD gv_bukrs
            ID 'PERSG' DUMMY
            ID 'PERSK' DUMMY
            ID 'SUBTY' DUMMY
            ID 'VDSK1' DUMMY.
      ENDCASE.

      IF sy-subrc <> 0.
        CONCATENATE 'Nemate autorizaciju za kompaniju' gv_bukrs
          INTO gv_msg SEPARATED BY space.
        MESSAGE gv_msg TYPE 'E'.
      ENDIF.

  ENDIF.

  IF gv_bukrs IS NOT INITIAL AND ( gv_mode = 0 OR gv_mode = 3 ).
    SELECT SINGLE butxt INTO butxt
      FROM t001
     WHERE bukrs = gv_bukrs.
    IF sy-subrc <> 0.
      gv_ok_bukrs = 0.
      MESSAGE 'Uneta šifra kompanije ne postoji u sistemu!'
        TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ELSE.
      gv_ok_bukrs = 1.
    ENDIF.
  ENDIF.
  IF gv_pernr IS NOT INITIAL AND ( gv_mode = 0 OR gv_mode = 3 ).
    IF gv_bukrs IS NOT INITIAL.
      SELECT SINGLE bukrs INTO lv_bukrs
        FROM pa0001
       WHERE begda <= sy-datum
         AND endda >= sy-datum
         AND pernr = gv_pernr.
      IF sy-subrc = 0.
        IF lv_bukrs <> gv_bukrs.
          MESSAGE 'Kadrovski broj ne postoji za unetu šifru kompanije'
            TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ELSE.
          SELECT SINGLE sname INTO emp_name " Ovo ne treba svaki put da radi
            FROM pa0001
           WHERE pernr = gv_pernr.
        ENDIF.
      ELSE.
        MESSAGE 'Kadrovski broj ne postoji u sistemu' TYPE 'S' DISPLAY LIKE 'E'.
        CLEAR emp_name.
        EXIT.
      ENDIF.
    ELSE.
      MESSAGE 'Unesite šifru kompanije' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  go_editor->get_textstream(
    EXPORTING
      only_when_modified = 0
    IMPORTING
      text               = gv_text
    EXCEPTIONS
      OTHERS             = 1 ).

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*   Flush is required for working on the string content
  cl_gui_cfw=>flush(
    EXCEPTIONS
      cntl_system_error = 1
      cntl_error        = 2
      OTHERS            = 3 ).
  IF sy-subrc <> 0.
    RAISE error_cntl_call_method.
  ENDIF.

  CASE save_ok.
    WHEN ''. " Enter key

    WHEN 'CHANGE'.
      gv_mode = 2.
    WHEN 'SAVE'.
      PERFORM save_data.
      PERFORM load_alv_data.
      go_alv->refresh( ).
      gv_anex = 0.
    WHEN 'PRINT'.
      PERFORM download_word_template USING 'ZHR' 'SAP/PUBLIC/ZHR'.
      PERFORM print_to_word.
    WHEN 'GEN'.
      PERFORM generate_contract.
      gv_mode = 1.
    WHEN 'CHNG'.
      CLEAR: gw_cont, emp_name, gv_pernr.
      go_editor->delete_text( ).
      CLEAR gv_text.

      IF go_alv_dock_container IS BOUND.
        FREE go_alv.
        go_alv_dock_container->free( ).
        FREE go_alv_dock_container.
      ENDIF.

      gv_mode = 0.

  ENDCASE.

*  lv_old_pernr = lw_cont-pernr.

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

ENDMODULE.                 " EXIT_COMMAND_0100  INPUT
*----------------------------------------------------------------------*
*  MODULE tmpid_search_help INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE tmpid_search_help INPUT.

  TYPES: BEGIN OF ty_template,
         bukrs TYPE bukrs,
         tmpid TYPE /mkbs/hr_tmpid,
         spras TYPE spras,
         tname TYPE /mkbs/hr_tmpname,
         END OF ty_template.

  DATA lt_temp TYPE TABLE OF ty_template.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_temp
    FROM /mkbs/hr_tmplt
   WHERE ttype = '1'
     AND bukrs = gv_bukrs.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'TMPID'
      dynpprog    = sy-repid
      dynpnr      = '100'
      dynprofield = 'GW_CONT-TMPID'
      value_org   = 'S'
    TABLES
      value_tab   = lt_temp.

ENDMODULE.                    "tmpid_search_help INPUT
*----------------------------------------------------------------------*
*  MODULE tmpid_search_help INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE pernr_search_help INPUT.

  TYPES: BEGIN OF ty_emp,
         pernr TYPE persno,
         sname TYPE smnam,
         END OF ty_emp.

  DATA lt_emp TYPE TABLE OF ty_emp.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_emp
    FROM pa0001
   WHERE begda <= sy-datum
     AND endda >= sy-datum
     AND bukrs = gv_bukrs
     AND persg = '1'. " ne znam da li ovo treba jer moze biti i drugo


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'PERNR'
      dynpprog    = sy-repid
      dynpnr      = '100'
      dynprofield = 'GV_PERNR'
      value_org   = 'S'
    TABLES
      value_tab   = lt_emp.

ENDMODULE.                    "tmpid_search_help INPUT
*&---------------------------------------------------------------------*
*&      Form  save_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM save_data.

  DATA: lv_message TYPE string,
        lv_tmpid TYPE /mkbs/hr_tmpid,
        lv_tmp_langu TYPE spras.
  DATA: ls_pa0016 TYPE pa0016.


  IF gw_cont-anxid IS INITIAL.  " needed because it set 000
    gw_cont-anxid = space.
  ENDIF.

  IF gw_cont-tmpid IS INITIAL.
    MESSAGE 'Unesite oznaku šablona.' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSE.
    SELECT SINGLE spras INTO lv_tmp_langu
      FROM /mkbs/hr_tmplt
     WHERE tmpid = gw_cont-tmpid
       AND bukrs = gv_bukrs.
  ENDIF.

  IF gv_text IS NOT INITIAL.
    gw_cont-precf = STRLEN( gv_text ).
    gw_cont-ctext = gv_text.
  ELSE.
    MESSAGE 'Tekst ugovora je prazan.' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  gw_cont-bukrs = gv_bukrs.
  gw_cont-pernr = gv_pernr.
  gw_cont-spras = lv_tmp_langu.
  gw_cont-edate = sy-datum.
  gw_cont-euser = sy-uname.

  MODIFY /mkbs/hr_cntrct FROM gw_cont.

  IF sy-subrc = 0.
    CONCATENATE 'Ugovor' gw_cont-cntid 'je sačuvan.'
      INTO lv_message SEPARATED BY space.
    MESSAGE lv_message TYPE 'S'.
    PERFORM send_email.
    gv_mode = 3.
  ELSE.
    MESSAGE 'Greška. Ugovor nije sačuvan.' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.


ENDFORM.                    "save_data
*&---------------------------------------------------------------------*
*&      Form  load_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM load_data USING p_cntid TYPE /mkbs/hr_cntid
                     p_anxid TYPE /mkbs/hr_anxid
                     p_pernr TYPE persno
                     p_cttyp TYPE cttyp
                     p_begda TYPE begda
                     p_endda TYPE endda.

  DATA: lv_ans TYPE c LENGTH 1.

***********************************DA LI OVDE TREBA BUKRS I ODAKLE MI???

  SELECT SINGLE * INTO gw_cont
    FROM /mkbs/hr_cntrct
   WHERE cntid = p_cntid
     AND anxid = p_anxid
     AND pernr = p_pernr.

  IF sy-subrc = 0.

    gw_cont-ctype = p_cttyp.

    SELECT SINGLE cttxt INTO gv_cttxt
      FROM t547s
     WHERE sprsl = sy-langu
       AND cttyp = p_cttyp.
    "   Set text in editor
    gv_text = gw_cont-ctext.

    gv_mode = 3.


    " dodao zbog stampe samo ako je odobren
    SELECT SINGLE zzodobreno INTO gv_approved
      FROM pa0016
     WHERE pernr = p_pernr
       AND brugovorahr = p_cntid
       AND begda <= sy-datum
       AND endda >= sy-datum.

  ELSE.

    IF p_cntid IS INITIAL.

      MESSAGE 'Broj ugovora nije popunjen u HR matičnim podacima' TYPE 'I'
        DISPLAY LIKE 'E'.

      CLEAR: gw_cont.
      go_editor->delete_text( ).
      CLEAR: gv_text, gv_cttxt.

    ELSE.

      /mkbs/cl_popup_to_confirm=>custom_text(
        EXPORTING
          title          = 'Generisanje ugovora'
          question       = 'Da li želite da kreirate ugovor?'
          button_1       = 'Da'
          button_2       = 'Ne'
          display_cancel = space
        IMPORTING
          user_answer    = lv_ans ).

      IF lv_ans = 1.

        gw_cont-cntid = p_cntid.
        gw_cont-anxid = p_anxid.
        gw_cont-begda = p_begda.
        gw_cont-endda = p_endda.
        gw_cont-ctype = p_cttyp.
        CLEAR gw_cont-tmpid.
        go_editor->delete_text( ).
        CLEAR gv_text.

        SELECT SINGLE cttxt INTO gv_cttxt
          FROM t547s
         WHERE sprsl = sy-langu
           AND cttyp = p_cttyp.

        gv_mode = 1.
      ELSE.

        CLEAR: gw_cont.
        go_editor->delete_text( ).
        CLEAR: gv_text, gv_cttxt.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.                    "load_data
*&---------------------------------------------------------------------*
*&      Form  save_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM generate_contract.

  DATA: ls_template TYPE /mkbs/hr_tmplt,
        lv_tmpid TYPE /mkbs/hr_tmpid.
*  Infotype 0006
  DATA: lv_city TYPE ort01,
        lv_stras TYPE stras,  " Ulica
        lv_hsnmr TYPE pad_hsnmr,  " Kucni broj
        lv_posta TYPE pad_posta, " Stan
        lv_address TYPE stras,
        lv_jobcity TYPE pad_ort01. " Mesto obavaljanja posla
*  Infotype 0002
  DATA: lv_jmbg TYPE zjmbg,
        lv_vorna TYPE pad_vorna,
        lv_nachn TYPE pad_nachn,
        lv_emp_name TYPE string,
        lv_dat_slav TYPE zdatumslave,
        lv_naz_slav TYPE zslavaopis,
        lv_naz_dat_slav TYPE string.
*  Infotype 0016
  DATA: ls_0016 TYPE pa0016,
        lv_cttyp TYPE cttyp,
        lv_cttxt TYPE cttxt,
        lv_brmeseci TYPE zbrmeseci,
        lv_meseci TYPE c,
        lv_eindt TYPE eintr,
        lv_cont_num_date TYPE string, " new from here
        lv_cont_num TYPE c LENGTH 10,
        lv_anex_num TYPE c LENGTH 3,
        lv_anex_date TYPE zdatumaneksa,
        lv_neto TYPE p DECIMALS 2,
        lv_bruto TYPE p DECIMALS 2,
        lv_neto_old TYPE p DECIMALS 2,
        lv_bruto_old TYPE p DECIMALS 2,
        lv_amount_str TYPE string,
        lv_date_format TYPE string,
        lv_waers TYPE waers,
        lv_waers_old TYPE waers.
*  Infotype 0001
  DATA: lv_plstx TYPE plstx,
        lv_orgtx TYPE orgtx,
        lv_plstx_old TYPE plstx,
        lv_orgtx_old TYPE orgtx,
        lv_day_before TYPE endda.
*  Infotype 2001 - absence
  DATA: ls_2001 TYPE pa2001,
        lv_ab_type TYPE awart,
        lv_ab_days TYPE string,
        lv_ab_reason TYPE string, " razlog
        lv_ab_base TYPE string, " osnov
        lv_period TYPE string,
        lv_req_date TYPE aedtm. " ovo je zapravo datum kreiranja zapisa a zahtev je u app
*  Infotype 2006 - time quota
  DATA: lv_anzhl TYPE ptm_quonum,
        lv_quota TYPE c LENGTH 2.
*  Infotype 0000 - actions
  DATA: lv_leave_date TYPE begda.

  DATA: lv_butxt TYPE butxt,
        lv_ort01 TYPE ort01,
        lv_adrnr TYPE adrnr,
        lv_company TYPE string,
        lv_street TYPE ad_street,
        lv_house_num TYPE ad_hsnm1,
        lv_city1 TYPE ad_city1,
        lv_comp_address TYPE string,
        lv_comp_full TYPE string.

  IF gw_cont-cntid IS INITIAL.
    MESSAGE 'Unesite broj ugovora.' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  IF gw_cont-tmpid IS NOT INITIAL.

    SELECT SINGLE * INTO ls_template
      FROM /mkbs/hr_tmplt
     WHERE tmpid = gw_cont-tmpid
       AND bukrs = gv_bukrs. " nemam spras tako da ne sme imati isti naziv na vise jezika

    IF sy-subrc <> 0.
      MESSAGE 'Unesite ispravan šablon.' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ELSE.
    MESSAGE 'Unesite oznaku šablon.' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  gv_text = ls_template-ttext.
  lv_tmpid = ls_template-tmpid.

  "   Adresa (ulica i broj) i grad
  SELECT SINGLE stras ort01 hsnmr posta INTO (lv_stras, lv_city, lv_hsnmr, lv_posta)
    FROM pa0006
   WHERE pernr = gv_pernr
     AND subty = '1'
     AND begda <= sy-datum
     AND endda >= sy-datum.

  "   JMBG zaposlenog
  SELECT SINGLE nachn vorna jmbg datumslave nazivslave
    INTO (lv_nachn, lv_vorna, lv_jmbg, lv_dat_slav, lv_naz_slav)
    FROM pa0002
   WHERE pernr = gv_pernr
     AND begda <= sy-datum
     AND endda >= sy-datum.

  TRANSLATE lv_vorna TO LOWER CASE.
  TRANSLATE lv_vorna(1) TO UPPER CASE.

  TRANSLATE lv_nachn TO LOWER CASE.
  TRANSLATE lv_nachn(1) TO UPPER CASE.

  CONCATENATE lv_vorna lv_nachn INTO lv_emp_name SEPARATED BY space.

  CLEAR lv_date_format.
  CONCATENATE lv_dat_slav+6(2) '.' lv_dat_slav+4(2) '.' lv_dat_slav(4) INTO
   lv_date_format.
  CONCATENATE lv_naz_slav lv_date_format INTO lv_naz_dat_slav SEPARATED BY space.

  "   Tip ugovora zaposlenog
  SELECT SINGLE text~cttxt INTO lv_cttxt
    FROM pa0016 AS type
    JOIN t547s AS text
      ON text~cttyp = type~cttyp
   WHERE text~sprsl = sy-langu
     AND type~pernr = gv_pernr
     AND type~begda <= sy-datum
     AND type~endda >= sy-datum.

  " new metas
  SELECT SINGLE * INTO ls_0016
    FROM pa0016
   WHERE pernr = gv_pernr
     AND begda = gw_cont-begda " sy-datum nece raditi za anekse u buducnosti
     AND endda = gw_cont-endda.

  lv_day_before = gw_cont-begda - 1.

  lv_eindt = ls_0016-eindt.
  lv_meseci = ls_0016-brmeseci.
  " new metas
  lv_cont_num = ls_0016-brugovorahr.
  lv_anex_num = ls_0016-zzanexid.
  lv_anex_date = ls_0016-datumaneksa.
  lv_neto = ls_0016-iznosn.
  lv_bruto = ls_0016-iznosb.
  lv_waers = ls_0016-waers.
  CLEAR lv_date_format.
  CONCATENATE ls_0016-datumugovora+6(2) '.' ls_0016-datumugovora+4(2) '.' ls_0016-datumugovora(4) INTO
    lv_date_format.
  CONCATENATE lv_cont_num lv_date_format INTO lv_cont_num_date SEPARATED BY space.

  " CONTRACT BEFORE
  SELECT SINGLE * INTO ls_0016
    FROM pa0016
   WHERE pernr = gv_pernr
*     AND begda = gw_cont-begda
     AND endda = lv_day_before.

  lv_neto_old = ls_0016-iznosn.
  lv_bruto_old = ls_0016-iznosb.
  lv_waers_old = ls_0016-waers.

  IF lv_tmpid = 'PLA_ODSUST'.
    lv_ab_type = '0210'.  " proveri ovo
  ELSEIF lv_tmpid = 'PORODILJSK'.
    lv_ab_type = '0200'. " i ovo
  ELSEIF lv_tmpid = 'GODI_ODMOR'.
    lv_ab_type = '0100'.
  ENDIF.

  SELECT SINGLE * INTO ls_2001
    FROM pa2001
   WHERE pernr = gv_pernr
     AND begda <= sy-datum
     AND endda >= sy-datum
     AND subty = lv_ab_type.

  lv_ab_days = ls_2001-abwtg.
  CONCATENATE ls_2001-begda+6(2) '.' ls_2001-begda+4(2) '.' ls_2001-begda(4) INTO
   lv_date_format.
  CONCATENATE 'od' lv_date_format INTO lv_period SEPARATED BY space.
  CLEAR lv_date_format.
  CONCATENATE ls_2001-endda+6(2) '.' ls_2001-endda+4(2) '.' ls_2001-endda(4) INTO
   lv_date_format.
  CONCATENATE lv_period 'do' lv_date_format INTO lv_period SEPARATED BY space.
  lv_req_date = ls_2001-aedtm.

  SELECT SINGLE anzhl INTO lv_anzhl
    FROM pa2006
   WHERE pernr = gv_pernr
     AND begda <= sy-datum
     AND endda >= sy-datum.

  lv_quota = lv_anzhl.

  " Company data
  SELECT SINGLE butxt ort01 adrnr INTO (lv_butxt,lv_ort01,lv_adrnr)
    FROM t001
   WHERE bukrs = gv_bukrs.

  CONCATENATE lv_butxt lv_ort01 INTO lv_company SEPARATED BY space.

  SELECT SINGLE street house_num1 city1 INTO (lv_street,lv_house_num,lv_city1)
    FROM adrc
   WHERE addrnumber = lv_adrnr.

  CONCATENATE lv_street lv_house_num INTO lv_comp_address SEPARATED BY space.
  CONCATENATE lv_comp_address ',' INTO lv_comp_address.
  CONCATENATE lv_comp_address lv_city1 INTO lv_comp_address SEPARATED BY space.

  CONCATENATE lv_company ',' INTO lv_comp_full.
  CONCATENATE lv_comp_full lv_street lv_house_num INTO lv_comp_full
    SEPARATED BY space.

  SELECT SINGLE begda INTO lv_leave_date
    FROM pa0000
   WHERE pernr = gv_pernr
     AND begda <= sy-datum
     AND endda >= sy-datum
     AND massn = 'Z4'.

  "   Mesto obavljanja posla
  SELECT SINGLE ort01 INTO lv_jobcity
    FROM pa0006
   WHERE pernr = gv_pernr
     AND subty = '7'
     AND begda <= sy-datum
     AND endda >= sy-datum.

  "   Sektor sluzba - odeljenje
  SELECT SINGLE orgtx INTO lv_orgtx
    FROM pa0001 AS p
    JOIN t527x AS t
      ON t~orgeh = p~orgeh
   WHERE p~pernr = gv_pernr
     AND t~sprsl = sy-langu
     AND p~begda = gw_cont-begda " bilo sy-datum
     AND p~endda = gw_cont-endda. " bilo sy-datum

  "   Pozicija
  SELECT SINGLE plstx INTO lv_plstx
    FROM pa0001 AS p
    JOIN t528t AS t
      ON t~plans = p~plans
   WHERE p~pernr = gv_pernr
     AND t~sprsl = sy-langu
     AND t~otype = 'S'
     AND p~begda = gw_cont-begda " bilo sy-datum
     AND p~endda = gw_cont-endda. " bilo sy-datum

*  lv_day_before = gw_cont-begda - 1. " ima gore

  " OLD JOB ORGANISATION UNIT
  SELECT SINGLE orgtx INTO lv_orgtx_old
    FROM pa0001 AS p
    JOIN t527x AS t
      ON t~orgeh = p~orgeh
   WHERE p~pernr = gv_pernr
     AND t~sprsl = sy-langu
*     AND p~begda = gw_cont-begda
     AND p~endda = lv_day_before. " bilo sy-datum

  " OLD JOB POSITION
  SELECT SINGLE plstx INTO lv_plstx_old
    FROM pa0001 AS p
    JOIN t528t AS t
      ON t~plans = p~plans
   WHERE p~pernr = gv_pernr
     AND t~sprsl = sy-langu
     AND t~otype = 'S'
*     AND p~begda = gw_cont-begda
     AND p~endda = lv_day_before. " bilo sy-datum

  REPLACE ALL OCCURRENCES OF '<IME>' IN gv_text WITH lv_emp_name.
  REPLACE ALL OCCURRENCES OF '<GRAD>' IN gv_text WITH lv_city.

  CONCATENATE lv_stras lv_hsnmr INTO lv_address SEPARATED BY space.
  IF lv_posta IS NOT INITIAL.
    CONCATENATE lv_address lv_posta INTO lv_address SEPARATED BY '/'.
  ENDIF.
  REPLACE ALL OCCURRENCES OF '<ADRESA>' IN gv_text WITH lv_address.

  REPLACE ALL OCCURRENCES OF '<JMBG>' IN gv_text WITH lv_jmbg.
  REPLACE ALL OCCURRENCES OF '<TIP_UGOV>' IN gv_text WITH lv_cttxt.
  " probation period
  IF lv_meseci IS INITIAL.
    REPLACE ALL OCCURRENCES OF '<PR_MESEC>' IN gv_text WITH '__'.
  ELSE.
    REPLACE ALL OCCURRENCES OF '<PR_MESEC>' IN gv_text WITH lv_meseci.
  ENDIF.
  IF lv_eindt IS INITIAL.
    REPLACE ALL OCCURRENCES OF '<PR_GODINA>' IN gv_text WITH '_____'.
  ELSE.
    REPLACE ALL OCCURRENCES OF '<PR_GODINA>' IN gv_text WITH lv_eindt(4).
  ENDIF.

  REPLACE ALL OCCURRENCES OF '<GRAD_RADA>' IN gv_text WITH lv_jobcity.
  REPLACE ALL OCCURRENCES OF '<SEKTOR>' IN gv_text WITH lv_orgtx.
  REPLACE ALL OCCURRENCES OF '<POZICIJA>' IN gv_text WITH lv_plstx.

  REPLACE ALL OCCURRENCES OF '<PRETH_SEKTOR>' IN gv_text WITH lv_orgtx_old.
  REPLACE ALL OCCURRENCES OF '<PRETH_POZICIJA>' IN gv_text WITH lv_plstx_old.

  " new metas from 0016 - contract elements
  REPLACE ALL OCCURRENCES OF '<BR_DAT_UG>' IN gv_text WITH lv_cont_num_date.
  REPLACE ALL OCCURRENCES OF '<BROJ_UGOVORA>' IN gv_text WITH lv_cont_num.
  CLEAR lv_date_format.
  CONCATENATE ls_0016-datumugovora+6(2) '.' ls_0016-datumugovora+4(2) '.' ls_0016-datumugovora(4) INTO
    lv_date_format.
  REPLACE ALL OCCURRENCES OF '<DATUM_UGOVORA>' IN gv_text WITH lv_date_format.
  REPLACE ALL OCCURRENCES OF '<BROJ_ANEX>' IN gv_text WITH lv_anex_num.
  CLEAR lv_date_format.
  CONCATENATE lv_anex_date+6(2) '.' lv_anex_date+4(2) '.' lv_anex_date(4) INTO
    lv_date_format.
  REPLACE ALL OCCURRENCES OF '<DATUM_ANEX>' IN gv_text WITH lv_date_format.
  IF lv_neto IS NOT INITIAL.
    lv_amount_str = lv_neto.
    CONCATENATE lv_amount_str lv_waers INTO lv_amount_str SEPARATED BY space.
  ELSE.
    lv_amount_str = '___________'.  " if no data then line
  ENDIF.
  REPLACE ALL OCCURRENCES OF '<NETO_ZARADA>' IN gv_text WITH lv_amount_str.
  IF lv_bruto IS NOT INITIAL.
    CLEAR lv_amount_str.
    lv_amount_str = lv_bruto.
    CONCATENATE lv_amount_str lv_waers INTO lv_amount_str SEPARATED BY space.
  ENDIF.
  REPLACE ALL OCCURRENCES OF '<BRUTO_ZARADA>' IN gv_text WITH lv_amount_str.

  " OLD CONTRACT SALLARY
  IF lv_neto_old IS NOT INITIAL.
    CLEAR lv_amount_str.
    lv_amount_str = lv_neto_old.
    CONCATENATE lv_amount_str lv_waers_old INTO lv_amount_str SEPARATED BY space.
  ELSE.
    lv_amount_str = '___________'.  " if no data then line
  ENDIF.
  REPLACE ALL OCCURRENCES OF '<PRETH_NETO_Z>' IN gv_text WITH lv_amount_str.
  IF lv_bruto_old IS NOT INITIAL.
    CLEAR lv_amount_str.
    lv_amount_str = lv_bruto_old.
    CONCATENATE lv_amount_str lv_waers_old INTO lv_amount_str SEPARATED BY space.
  ENDIF.
  REPLACE ALL OCCURRENCES OF '<PRETH_BRUTO_Z>' IN gv_text WITH lv_amount_str.

  " new metas from 2001 - absences
  REPLACE ALL OCCURRENCES OF '<BROJ_DANA>' IN gv_text WITH lv_ab_days.
  lv_ab_reason = '____________________'. " ovo bi se trebalo dodati
  REPLACE ALL OCCURRENCES OF '<RAZLOG_ODS>' IN gv_text WITH lv_ab_reason.
  lv_ab_base = '____________________'.  " i ovo
  REPLACE ALL OCCURRENCES OF '<OSNOV_ODS>' IN gv_text WITH lv_ab_base.
  REPLACE ALL OCCURRENCES OF '<PERIOD_ODS>' IN gv_text WITH lv_period.
  CLEAR lv_date_format.
  CONCATENATE lv_req_date+6(2) '.' lv_req_date+4(2) '.' lv_req_date(4) INTO
    lv_date_format.
  REPLACE ALL OCCURRENCES OF '<DATUM_ZAHTEVA>' IN gv_text WITH lv_date_format.

  REPLACE ALL OCCURRENCES OF '<UKUPNO_GOD_ODM>' IN gv_text WITH lv_quota.

  CLEAR lv_date_format.
  CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum(4) INTO
    lv_date_format.
  REPLACE ALL OCCURRENCES OF '<SYS_DATUM>' IN gv_text WITH lv_date_format.
  CLEAR lv_date_format.
  CONCATENATE lv_leave_date+6(2) '.' lv_leave_date+4(2) '.' lv_leave_date(4) INTO
    lv_date_format.
  REPLACE ALL OCCURRENCES OF '<DATUM_PREST_RO>' IN gv_text WITH lv_date_format.
*  REPLACE ALL OCCURRENCES OF '<DATUM_ZAHT_OTKAZ>' IN gv_text WITH lv_plstx. " ne postoji
*  REPLACE ALL OCCURRENCES OF '<DATUM_STALN_ZAP>' IN gv_text WITH . "??
  REPLACE ALL OCCURRENCES OF '<NAZIV_SLAVE_DATUM>' IN gv_text WITH lv_naz_dat_slav.
  REPLACE ALL OCCURRENCES OF '<KOMPANIJA>' IN gv_text WITH lv_company.
  REPLACE ALL OCCURRENCES OF '<KOMP_PUNA_ADRESA>' IN gv_text WITH lv_comp_address.
  REPLACE ALL OCCURRENCES OF '<KOMPANIJA_ADRESA>' IN gv_text WITH lv_comp_full.

  " city of court of juridiction
  CASE gv_bukrs.
    WHEN '1000' OR '1700' OR '1800' OR '1900'.
      REPLACE ALL OCCURRENCES OF '<SUD>' IN gv_text WITH 'Novom Sadu'.
    WHEN OTHERS.
      REPLACE ALL OCCURRENCES OF '<SUD>' IN gv_text WITH 'Beogradu'.
  ENDCASE.


ENDFORM.                    "save_data
*&---------------------------------------------------------------------*
*&      Form  load_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM load_alv_data.

  DATA: lt_cont TYPE TABLE OF /mkbs/hr_cntrct,
        lw_cont TYPE /mkbs/hr_cntrct.

  CLEAR gt_contracts.

  SELECT p16~brugovorahr AS cntid
         p16~zzanexid AS anxid
         p16~pernr AS pernr
         p16~cttyp AS ctype
         p16~begda AS begda
         p16~endda AS endda
    INTO CORRESPONDING FIELDS OF TABLE gt_contracts
    FROM pa0016 AS p16
   WHERE p16~pernr = gv_pernr
     AND p16~begda >= ( SELECT MIN( begda )
                          FROM pa0001
                         WHERE pernr = gv_pernr
                           AND bukrs = gv_bukrs )
     AND p16~endda <= ( SELECT MAX( endda )
                          FROM pa0001
                         WHERE pernr = gv_pernr
                           AND bukrs = gv_bukrs ).

  IF sy-subrc = 0.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_cont
      FROM /mkbs/hr_cntrct
     WHERE pernr = gv_pernr
       AND bukrs = gv_bukrs.

    LOOP AT gt_contracts ASSIGNING <contract>.

      SELECT SINGLE sname INTO <contract>-sname
        FROM pa0001
       WHERE pernr = <contract>-pernr.

      SELECT SINGLE cttxt INTO <contract>-cttxt
        FROM t547s
       WHERE cttyp = <contract>-ctype
         AND sprsl = sy-langu.

      LOOP AT lt_cont INTO lw_cont.
        IF lw_cont-cntid = <contract>-cntid
           AND lw_cont-anxid = <contract>-anxid.
          <contract>-tmpid = lw_cont-tmpid.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

  ENDIF.

ENDFORM.                    "load_data
*&---------------------------------------------------------------------*
*&      Form  create_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_alv.

  CREATE OBJECT go_alv_dock_container
    EXPORTING
      side                        = cl_gui_docking_container=>dock_at_left
      extension                   = 465
      caption                     = 'Dokumenti' .

  TRY.
      cl_salv_table=>factory(
        EXPORTING
          r_container = go_alv_dock_container
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = gt_contracts ).
    CATCH cx_salv_msg INTO gx_msg.
  ENDTRY.

*   get functions object
  go_functions = go_alv->get_functions( ).
  go_functions->set_default( abap_true ).

  go_cols = go_alv->get_columns( ).
*   set the Column optimization
  go_cols->set_optimize( 'X' ).

*   get layout object
  go_layout = go_alv->get_layout( ).
*   set Layout save restriction
*   1. Set Layout Key .. Unique key identifies the Differenet ALVs
  gs_key-report = sy-repid.
  go_layout->set_key( gs_key ).
*   2. Remove Save layout the restriction.
  go_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*
*   set initial Layout
  gv_variant = '/DEFAULT'.
  go_layout->set_initial_layout( gv_variant ).

*   get display object
  go_display = go_alv->get_display_settings( ).
*   set ZEBRA pattern
  go_display->set_striped_pattern( 'X' ).

*   all events
  go_events = go_alv->get_event( ).

  CREATE OBJECT go_event_handler.
*   event handler
  SET HANDLER go_event_handler->on_double_click FOR go_events.

* Displaying the ALV
  go_alv->display( ).

ENDFORM.                    "create_alv
*&---------------------------------------------------------------------*
*&      Form  print_to_word
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM print_to_word.

  DATA: lv_before_t TYPE string,
        lv_title TYPE string,
        lv_after_t TYPE string,
        lv_before_mid TYPE string,
        lv_middle TYPE string,
        lv_after_mid TYPE string.

  DATA: lv_sub_num TYPE i,
        lv_bold_num TYPE i.

  DATA: lv_first_a TYPE string,
        lv_subt TYPE string,
        lv_rest TYPE string.

  DATA: ls_word TYPE ole2_object, " OLE object handle
        ls_documents TYPE ole2_object, " Documents
        ls_actdoc TYPE ole2_object, " Active document
        ls_application TYPE ole2_object, " Application
        ls_options TYPE ole2_object, " Application options
        ls_actwin TYPE ole2_object, " Active window
        ls_actpan TYPE ole2_object, " Active pan
        ls_view TYPE ole2_object, " View
        ls_selection TYPE ole2_object, " Selection
        ls_font TYPE ole2_object, " Font
        ls_parformat TYPE ole2_object, " Paragraph format
        ls_paragraph TYPE ole2_object. " Paragraph

  DATA: lv_pathdir TYPE string.


  " added 26.12.2017 - memorandums of companies
  CASE gv_bukrs.
    WHEN '1000'.
      CONCATENATE gv_directory '\MK_COMMERCE.doc'
         INTO lv_pathdir.
    WHEN '1600'.
      CONCATENATE gv_directory '\AGRIUM.doc'
         INTO lv_pathdir.
    WHEN '1700'.
      CONCATENATE gv_directory '\AGROCRNJA.doc'
         INTO lv_pathdir.
    WHEN '4031'.
      CONCATENATE gv_directory '\AGROGLOBE.doc'
         INTO lv_pathdir.
    WHEN 'AGUN'.
      CONCATENATE gv_directory '\AGROUNIJA.doc'
         INTO lv_pathdir.
    WHEN '5100'.
      CONCATENATE gv_directory '\BANAT_SEME.doc'
         INTO lv_pathdir.
    WHEN '4020'.
      CONCATENATE gv_directory '\BEO_REAL_STATE.doc'
         INTO lv_pathdir.
    WHEN '4030'.
      CONCATENATE gv_directory '\BEOGRADELEKTRO.doc'
         INTO lv_pathdir.
    WHEN '4037'.
      CONCATENATE gv_directory '\CARNEX.doc'
         INTO lv_pathdir.
    WHEN '4005'.
      CONCATENATE gv_directory '\KMD_SECURITY.doc'
         INTO lv_pathdir.
    WHEN '4036'.
      CONCATENATE gv_directory '\KUCA_VINA.doc'
         INTO lv_pathdir.
    WHEN '1300'.
      CONCATENATE gv_directory '\MAXIPROTEIN.doc'
         INTO lv_pathdir.
    WHEN 'MKAG'.
      CONCATENATE gv_directory '\MK_AGRICULTURE.doc'
         INTO lv_pathdir.
    WHEN 'MKH'.
      CONCATENATE gv_directory '\MK_HOLDING.doc'
         INTO lv_pathdir.
    WHEN 'MKI'.
      CONCATENATE gv_directory '\MK_INVEST.doc'
         INTO lv_pathdir.
    WHEN 'VOJ'.
      CONCATENATE gv_directory '\PD_VOJVODINA.doc'
         INTO lv_pathdir.
    WHEN '4033'.
      CONCATENATE gv_directory '\POBEDA_METALAC.doc'
         INTO lv_pathdir.
    WHEN '4034'.
      CONCATENATE gv_directory '\POBEDA_ZARA.doc'
         INTO lv_pathdir.
    WHEN '4035'.
      CONCATENATE gv_directory '\SELEKCIJA_ALEKSINAC.doc'
         INTO lv_pathdir.
    WHEN '1800'.
      CONCATENATE gv_directory '\SILOSI_NOVO_MILOSEVO.doc'
         INTO lv_pathdir.
    WHEN '1900'.
      CONCATENATE gv_directory '\AGROBREST.doc'
         INTO lv_pathdir.
    WHEN 'SUN'.
      CONCATENATE gv_directory '\SUNOKO.doc'
         INTO lv_pathdir.
    WHEN 'MKC'.
      CONCATENATE gv_directory '\MK_GROUP.doc'
         INTO lv_pathdir.
    WHEN 'PIKB'.
      CONCATENATE gv_directory '\PIK_BECEJ.doc'
         INTO lv_pathdir.
    WHEN 'PIKV'.
      CONCATENATE gv_directory '\PIK_VETERINA.doc'
         INTO lv_pathdir.
    WHEN '4038'.
      CONCATENATE gv_directory '\PP_ERDEVIK.doc'
         INTO lv_pathdir.
    WHEN '4039'.
      CONCATENATE gv_directory '\MK_LAKE_RESORT.doc'
         INTO lv_pathdir.
    WHEN 'MKBS'.
      CONCATENATE gv_directory '\MK_IT_BUSINESS_SOLUTIONS.docx'
         INTO lv_pathdir.
    WHEN OTHERS.
      CONCATENATE gv_directory '\HR_Template.doc'
         INTO lv_pathdir.
  ENDCASE.


  " split string with title
  SPLIT gv_text AT '@' INTO lv_before_t lv_title lv_after_t.


  " find number of subtitles
  FIND ALL OCCURRENCES OF '$' IN lv_after_t MATCH COUNT lv_sub_num.
  lv_sub_num = lv_sub_num / 2.

  " find numer of bold words
  FIND ALL OCCURRENCES OF '^' IN lv_after_t MATCH COUNT lv_bold_num.
  lv_bold_num = lv_bold_num / 2.


  "   Creating OLE object handle variable
  CREATE OBJECT ls_word 'Word.Application'.
  IF sy-subrc <> 0.
    MESSAGE 'Greška prilikom kreiranja Word dokumenta.'
      TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
  "   Setting object's visibility property
  SET PROPERTY OF ls_word 'Visible' = '1'.
*  "   Opening a new document
  "   New document from template document
  CALL METHOD OF ls_word 'Documents' = ls_documents.
  CALL METHOD OF ls_documents 'OPEN'
    EXPORTING
    #1 = lv_pathdir.

  "   Getting active document handle
  GET PROPERTY OF ls_word 'ActiveDocument' = ls_actdoc.
  "   Getting applications handle
  GET PROPERTY OF ls_actdoc 'Application' = ls_application.

  "   Setting the measurement unit
  GET PROPERTY OF ls_application 'Options' = ls_options.
  SET PROPERTY OF ls_options 'MeasurementUnit' = '1'. " CM

  "   Setting content before header
  "   Getting handle for the selection which is here the character at the cursor position
  GET PROPERTY OF ls_application 'Selection' = ls_selection.
  GET PROPERTY OF ls_selection 'Font' = ls_font.
  GET PROPERTY OF ls_selection 'ParagraphFormat' = ls_parformat.
  "   Setting font attributes
  SET PROPERTY OF ls_font 'Name' = 'Arial'.
  SET PROPERTY OF ls_font 'Size' = '11'.
  SET PROPERTY OF ls_font 'Bold' = '0'.
  "   Setting paragraph format attribute
  SET PROPERTY OF ls_parformat 'Alignment' = '3'. " Justified
  SET PROPERTY OF ls_parformat 'LineSpacingRule' = '0'.
  SET PROPERTY OF ls_parformat 'SpaceAfter' = '6'.
  CALL METHOD OF ls_selection 'TypeText'
    EXPORTING
    #1 = lv_before_t.


  "   Setting title
  CALL METHOD OF ls_selection 'TypeParagraph'.
  "   Setting font attributes
  SET PROPERTY OF ls_font 'Size' = '12'.
  SET PROPERTY OF ls_font 'Bold' = '1'.
  "   Setting paragraph format attribute
  SET PROPERTY OF ls_parformat 'Alignment' = '1'. " Centered
  CALL METHOD OF ls_selection 'TypeText'
    EXPORTING
    #1 = lv_title.

  IF lv_after_t CO '~'.
    " split string for middle word 'izmedju'
    SPLIT lv_after_t AT '~' INTO lv_before_mid lv_middle lv_after_mid.

    "   Setting middle bold italic word
    CALL METHOD OF ls_selection 'TypeParagraph'.
    "   Setting font attributes
    SET PROPERTY OF ls_font 'Size' = '11'.
    SET PROPERTY OF ls_font 'Bold' = '1'.
    SET PROPERTY OF ls_font 'Italic' = '1'.
    "   Setting paragraph format attribute
    SET PROPERTY OF ls_parformat 'Alignment' = '1'. " Centered
    CALL METHOD OF ls_selection 'TypeText'
      EXPORTING
      #1 = lv_middle.

  ELSE.
    lv_after_mid = lv_after_t.
  ENDIF.

  " for bolds
  DO lv_bold_num TIMES.

    SPLIT lv_after_mid AT '^' INTO lv_first_a lv_subt lv_rest.

    IF sy-index = 1.
      CALL METHOD OF ls_selection 'TypeParagraph'.
    ENDIF.
    "   Setting font attributes
    SET PROPERTY OF ls_font 'Bold' = '0'.
    SET PROPERTY OF ls_font 'Italic' = '0'.
    "   Setting paragraph format attribute
    SET PROPERTY OF ls_parformat 'Alignment' = '3'. " Justified
    SET PROPERTY OF ls_parformat 'SpaceBefore' = '0'.
    SET PROPERTY OF ls_parformat 'SpaceAfter' = '6'.
    CALL METHOD OF ls_selection 'TypeText'
      EXPORTING
      #1 = lv_first_a.

    "   Setting font attributes
    SET PROPERTY OF ls_font 'Size' = '11'.
    SET PROPERTY OF ls_font 'Bold' = '1'.
    "   Setting paragraph format attribute
    SET PROPERTY OF ls_parformat 'Alignment' = '3'. " Justified
    SET PROPERTY OF ls_parformat 'SpaceBefore' = '0'.
    SET PROPERTY OF ls_parformat 'SpaceAfter' = '0'.
    CALL METHOD OF ls_selection 'TypeText'
      EXPORTING
      #1 = lv_subt.

    lv_after_mid = lv_rest.

    CLEAR: lv_first_a, lv_subt, lv_rest.

  ENDDO.


  " for subtitles
  DO lv_sub_num TIMES.

    SPLIT lv_after_mid AT '$' INTO lv_first_a lv_subt lv_rest.

    CALL METHOD OF ls_selection 'TypeParagraph'.
    "   Setting font attributes
    SET PROPERTY OF ls_font 'Size' = '11'.
    SET PROPERTY OF ls_font 'Bold' = '0'.
    "   Setting paragraph format attribute
    SET PROPERTY OF ls_parformat 'Alignment' = '3'. " Justified
    SET PROPERTY OF ls_parformat 'SpaceBefore' = '0'.
    SET PROPERTY OF ls_parformat 'SpaceAfter' = '6'.
    CALL METHOD OF ls_selection 'TypeText'
      EXPORTING
      #1 = lv_first_a.


    CALL METHOD OF ls_selection 'TypeParagraph'.
    "   Setting font attributes
    SET PROPERTY OF ls_font 'Size' = '11'.
    SET PROPERTY OF ls_font 'Bold' = '1'.
    "   Setting paragraph format attribute
    SET PROPERTY OF ls_parformat 'Alignment' = '1'. " Centered
    SET PROPERTY OF ls_parformat 'SpaceBefore' = '0'.
    SET PROPERTY OF ls_parformat 'SpaceAfter' = '0'.
    CALL METHOD OF ls_selection 'TypeText'
      EXPORTING
      #1 = lv_subt.

    lv_after_mid = lv_rest.

    CLEAR: lv_first_a, lv_subt, lv_rest.

  ENDDO.


  CALL METHOD OF ls_selection 'TypeParagraph'.
  "   Setting font attributes
  SET PROPERTY OF ls_font 'Size' = '11'.
  SET PROPERTY OF ls_font 'Bold' = '0'.
  "   Setting paragraph format attribute
  SET PROPERTY OF ls_parformat 'Alignment' = '3'. " Justified
  SET PROPERTY OF ls_parformat 'SpaceBefore' = '0'.
  SET PROPERTY OF ls_parformat 'SpaceAfter' = '8'.
  CALL METHOD OF ls_selection 'TypeText'
    EXPORTING
    #1 = lv_after_mid.

  FREE OBJECT ls_word.

ENDFORM.                    "print_to_word
*&---------------------------------------------------------------------*
*&      Form  download_word_template
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FOLDER   text
*      -->P_MIME_F   text
*----------------------------------------------------------------------*
FORM download_word_template USING p_folder TYPE string
                                  p_mime_f TYPE string.

  DATA: lv_sapworkdir TYPE string,
        lv_dir_exist TYPE c.

*  tell user what is going on
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = text-002
    EXCEPTIONS
      OTHERS = 1.

  CALL METHOD cl_gui_frontend_services=>get_sapgui_workdir
    CHANGING
      sapworkdir = lv_sapworkdir.

  CONCATENATE lv_sapworkdir '/' p_folder INTO gv_directory.

  CONCATENATE 'file://' gv_directory INTO gv_directory_url.
  REPLACE ALL OCCURRENCES OF '\' IN gv_directory_url WITH '/'.

  "   check if directory exist
  CALL METHOD cl_gui_frontend_services=>directory_exist
    EXPORTING
      directory            = gv_directory
    RECEIVING
      result               = lv_dir_exist
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      wrong_parameter      = 3
      not_supported_by_gui = 4
      OTHERS               = 5.

  IF lv_dir_exist <> 'X'.

    "    kreiranje direktorijuma
    DATA: l_rc TYPE i.
    CALL METHOD cl_gui_frontend_services=>directory_create
      EXPORTING
        directory = gv_directory
      CHANGING
        rc        = l_rc.

  ENDIF.

  "    download mime objekata
  DATA: lt_mime TYPE rszwmd_tx_mime.
  DATA: lt_data TYPE rszw_t_blobdata.
  CALL FUNCTION 'RSWAD_MIME_DOWNLOAD'
    EXPORTING
      i_fe_root   = gv_directory
      i_mime_root = p_mime_f
    TABLES
      e_t_mime    = lt_mime
      e_t_data    = lt_data.

ENDFORM.                    "download_word_template
*&---------------------------------------------------------------------*
*&      Form  send_email
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_email.

  DATA: lo_send_request TYPE REF TO cl_bcs,
        lo_sender TYPE REF TO cl_sapuser_bcs,
        lo_recipient TYPE REF TO if_recipient_bcs,
        lo_document TYPE REF TO cl_document_bcs,
        lx_document_bcs TYPE REF TO cx_document_bcs,
        lo_bcs_exception TYPE REF TO cx_bcs.

  DATA: lv_email TYPE adr6-smtp_addr,
        lv_mailtext TYPE bcsy_text,
        lv_sent_to_all TYPE os_boolean,
        lv_body LIKE LINE OF lv_mailtext.

  CONSTANTS:
    lc_subject TYPE so_obj_des VALUE 'SAP HR ugovori',
    lc_raw     TYPE char03 VALUE 'RAW'.

  DATA: lt_conf TYPE TABLE OF /mkbs/hr_config,
        lw_conf TYPE /mkbs/hr_config.


  lw_conf-email = 'hr.office@mkgroup.rs'.

  IF lw_conf-email IS INITIAL.
    MESSAGE 'E-mail nije poslat. Nije definisan u konfiguracionoj tabeli' TYPE 'I'.
  ELSE.

    CONCATENATE gv_url_content '[System]' cl_abap_char_utilities=>cr_lf INTO gv_url_content.
    CONCATENATE gv_url_content 'Name=' sy-sysid cl_abap_char_utilities=>cr_lf INTO gv_url_content.
    CONCATENATE gv_url_content 'Client=' sy-mandt cl_abap_char_utilities=>cr_lf INTO gv_url_content.
    CONCATENATE gv_url_content '[User]' cl_abap_char_utilities=>cr_lf INTO gv_url_content.
    CONCATENATE gv_url_content 'Name=' sy-uname cl_abap_char_utilities=>cr_lf INTO gv_url_content.
    CONCATENATE gv_url_content 'Language=' sy-langu cl_abap_char_utilities=>cr_lf INTO gv_url_content.
    CONCATENATE gv_url_content '[Function]' cl_abap_char_utilities=>cr_lf INTO gv_url_content.
    CONCATENATE gv_url_content 'Command=/MKBS/HR_INBOX' cl_abap_char_utilities=>cr_lf INTO gv_url_content.
    CONCATENATE gv_url_content 'Title=Inbox' cl_abap_char_utilities=>cr_lf INTO gv_url_content.
    CONCATENATE gv_url_content 'Type=Transaction' cl_abap_char_utilities=>cr_lf INTO gv_url_content.

    DATA: lv_xcontent TYPE xstring,
          it_solix TYPE solix_tab.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = gv_url_content
      IMPORTING
        buffer = lv_xcontent.


    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = lv_xcontent
      TABLES
        binary_tab = it_solix.

    TRY.
        " Create send request
        lo_send_request = cl_bcs=>create_persistent( ).

        lo_sender = cl_sapuser_bcs=>create( sy-uname ).
        " Add sender to send request
        lo_send_request->set_sender(
          EXPORTING
            i_sender = lo_sender ).

        lv_email = lw_conf-email. " ovo treba da vuce odnekle
        lo_recipient = cl_cam_address_bcs=>create_internet_address( lv_email ).

        lo_send_request->add_recipient(
          EXPORTING
            i_recipient = lo_recipient
            i_express   = 'X' ).

        " Email BODY
        CONCATENATE 'Kreiran je novi ugovor za zaposlenog' emp_name
          INTO lv_body SEPARATED BY space.
        APPEND lv_body TO lv_mailtext.
        lo_document = cl_document_bcs=>create_document(
                        i_type    = lc_raw
                        i_text    = lv_mailtext
                        i_length  = '12'
                        i_subject = lc_subject ).

        " Send attachment
        TRY.
            lo_document->add_attachment(
            EXPORTING
            i_attachment_type = 'EXT'
            i_attachment_subject = 'INBOX_LINK.SAP'
            i_att_content_hex = it_solix  ).
          CATCH cx_document_bcs INTO lx_document_bcs.
        ENDTRY.

        " Add document to send request
        lo_send_request->set_document( lo_document ).

        " lo_send_request->set_send_immediately( 'X' ).
        " Send email
        lo_send_request->send(
          EXPORTING
            i_with_error_screen = 'X'
          RECEIVING
            result              = lv_sent_to_all ).
        IF lv_sent_to_all = 'X'.
          MESSAGE 'E-mail je prosleđen' TYPE 'S'.
        ENDIF.

        " Commit to send email
        COMMIT WORK.

        " Exception handling
      CATCH cx_bcs INTO lo_bcs_exception.
        WRITE:
          'Error!',
          'Error type:',
          lo_bcs_exception->error_type.
    ENDTRY.

  ENDIF.

ENDFORM.                    "send_email