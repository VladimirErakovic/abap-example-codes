METHOD employee_master_data.

  DATA: ls_master_data TYPE /mkbs/hr_employee_md_s.

  DATA: lt_address TYPE TABLE OF /mkbs/hr_address_s,
        lt_bank TYPE TABLE OF /mkbs/hr_bank_s,
        lt_contract TYPE TABLE OF /mkbs/hr_contract,
        lt_family TYPE TABLE OF /mkbs/hr_family_s,
        lt_education TYPE TABLE OF /mkbs/hr_education_s,
        lt_qualification TYPE TABLE OF /mkbs/hr_qualification_s,
        lt_contact TYPE TABLE OF /mkbs/hr_contact_s,
        lt_prevemp TYPE TABLE OF /mkbs/hr_previous_emp_s,
        lt_absence TYPE TABLE OF /mkbs/hr_absence_s,
        lt_aquatas TYPE TABLE OF /mkbs/hr_absence_quotas_s.

  DATA: lt_bapi_return TYPE TABLE OF /mkbs/hr_bapi_errors,
        ls_bapi_return TYPE /mkbs/hr_bapi_errors,
        ls_return TYPE bapireturn1.

  DATA: lv_tabix LIKE sy-tabix.

  FIELD-SYMBOLS: <fs_data> TYPE ANY.

* Dynamic Table creation
  DATA: lo_struct   TYPE REF TO cl_abap_structdescr,
        lo_table    TYPE REF TO  cl_abap_tabledescr,
        lt_comp     TYPE cl_abap_structdescr=>component_table,
        ls_comp     LIKE LINE OF lt_comp,
        lo_data TYPE REF TO data.

  DATA: ls_pa0002 TYPE pa0002.


* deserialize JSON string json into internal table lt_flight doing camelCase to ABAP like field name mapping
  /ui2/cl_json=>deserialize( EXPORTING json = import_data
                                       pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                             CHANGING  data = ls_master_data ).

  IF ls_master_data IS NOT INITIAL.

    lo_struct ?= cl_abap_typedescr=>describe_by_name( '/MKBS/HR_EMPLOYEE_MD_S' ).
    lt_comp  = lo_struct->get_components( ).


    "This code is requred and locks the record ready for modification
    CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = ls_master_data-pernr.


    LOOP AT lt_comp INTO ls_comp FROM 2.

      CASE ls_comp-name.

        WHEN 'P0006'.

          CLEAR: lv_tabix, ls_bapi_return.
          lt_address = ls_master_data-p0006.
          LOOP AT lt_address ASSIGNING <fs_data>.
            lv_tabix = sy-tabix.
            infotype_operation(
              EXPORTING
                im_pernr = ls_master_data-pernr
                im_name = ls_comp-name
                im_data = <fs_data>
              IMPORTING
                ex_return = ls_return
                ).
            IF ls_return IS NOT INITIAL.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              MOVE-CORRESPONDING ls_return TO ls_bapi_return.
              APPEND ls_bapi_return TO lt_bapi_return.
            ELSE.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              ls_bapi_return-message = 'OK'.
              APPEND ls_bapi_return TO lt_bapi_return.
            ENDIF.
          ENDLOOP.
          UNASSIGN <fs_data>.

        WHEN 'P0009'.

          CLEAR: lv_tabix, ls_bapi_return.
          lt_bank = ls_master_data-p0009.
          LOOP AT lt_bank ASSIGNING <fs_data>.
            lv_tabix = sy-tabix.
            infotype_operation(
              EXPORTING
                im_pernr = ls_master_data-pernr
                im_name = ls_comp-name
                im_data = <fs_data>
              IMPORTING
                ex_return = ls_return
                ).
            IF ls_return IS NOT INITIAL.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              MOVE-CORRESPONDING ls_return TO ls_bapi_return.
              APPEND ls_bapi_return TO lt_bapi_return.
            ELSE.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              ls_bapi_return-message = 'OK'.
              APPEND ls_bapi_return TO lt_bapi_return.
            ENDIF.
          ENDLOOP.
          UNASSIGN <fs_data>.

        WHEN 'P0016'.

          CLEAR: lv_tabix, ls_bapi_return.
          lt_contract = ls_master_data-p0016.
          LOOP AT lt_contract ASSIGNING <fs_data>.
            lv_tabix = sy-tabix.
            infotype_operation(
              EXPORTING
                im_pernr = ls_master_data-pernr
                im_name = ls_comp-name
                im_data = <fs_data>
              IMPORTING
                ex_return = ls_return
                ).
            IF ls_return IS NOT INITIAL.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              MOVE-CORRESPONDING ls_return TO ls_bapi_return.
              APPEND ls_bapi_return TO lt_bapi_return.
            ELSE.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              ls_bapi_return-message = 'OK'.
              APPEND ls_bapi_return TO lt_bapi_return.
            ENDIF.
          ENDLOOP.
          UNASSIGN <fs_data>.

        WHEN 'P0021'.

          CLEAR: lv_tabix, ls_bapi_return.
          lt_family = ls_master_data-p0021.
          LOOP AT lt_family ASSIGNING <fs_data>.
            lv_tabix = sy-tabix.
            infotype_operation(
              EXPORTING
                im_pernr = ls_master_data-pernr
                im_name = ls_comp-name
                im_data = <fs_data>
              IMPORTING
                ex_return = ls_return
                ).
            IF ls_return IS NOT INITIAL.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              MOVE-CORRESPONDING ls_return TO ls_bapi_return.
              APPEND ls_bapi_return TO lt_bapi_return.
            ELSE.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              ls_bapi_return-message = 'OK'.
              APPEND ls_bapi_return TO lt_bapi_return.
            ENDIF.
          ENDLOOP.
          UNASSIGN <fs_data>.

        WHEN 'P0022'.

          CLEAR: lv_tabix, ls_bapi_return.
          lt_education = ls_master_data-p0022.
          LOOP AT lt_education ASSIGNING <fs_data>.
            lv_tabix = sy-tabix.
            infotype_operation(
              EXPORTING
                im_pernr = ls_master_data-pernr
                im_name = ls_comp-name
                im_data = <fs_data>
              IMPORTING
                ex_return = ls_return
                ).
            IF ls_return IS NOT INITIAL.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              MOVE-CORRESPONDING ls_return TO ls_bapi_return.
              APPEND ls_bapi_return TO lt_bapi_return.
            ELSE.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              ls_bapi_return-message = 'OK'.
              APPEND ls_bapi_return TO lt_bapi_return.
            ENDIF.
          ENDLOOP.
          UNASSIGN <fs_data>.

        WHEN 'P0024'.

          CLEAR: lv_tabix, ls_bapi_return.
          lt_qualification = ls_master_data-p0024.
          LOOP AT lt_qualification ASSIGNING <fs_data>.
            lv_tabix = sy-tabix.
            infotype_operation(
              EXPORTING
                im_pernr = ls_master_data-pernr
                im_name = ls_comp-name
                im_data = <fs_data>
                im_nosubty = 'X'
              IMPORTING
                ex_return = ls_return
                ).
            IF ls_return IS NOT INITIAL.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              MOVE-CORRESPONDING ls_return TO ls_bapi_return.
              APPEND ls_bapi_return TO lt_bapi_return.
            ELSE.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              ls_bapi_return-message = 'OK'.
              APPEND ls_bapi_return TO lt_bapi_return.
            ENDIF.
          ENDLOOP.
          UNASSIGN <fs_data>.

        WHEN 'P0105'.

          CLEAR: lv_tabix, ls_bapi_return.
          lt_contact = ls_master_data-p0105.
          LOOP AT lt_contact ASSIGNING <fs_data>.
            lv_tabix = sy-tabix.
            infotype_operation(
              EXPORTING
                im_pernr = ls_master_data-pernr
                im_name = ls_comp-name
                im_data = <fs_data>
              IMPORTING
                ex_return = ls_return
                ).
            IF ls_return IS NOT INITIAL.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              MOVE-CORRESPONDING ls_return TO ls_bapi_return.
              APPEND ls_bapi_return TO lt_bapi_return.
            ELSE.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              ls_bapi_return-message = 'OK'.
              APPEND ls_bapi_return TO lt_bapi_return.
            ENDIF.
          ENDLOOP.
          UNASSIGN <fs_data>.

        WHEN 'P0552'.

          CLEAR: lv_tabix, ls_bapi_return.
          lt_prevemp = ls_master_data-p0552.
          LOOP AT lt_prevemp ASSIGNING <fs_data>.
            lv_tabix = sy-tabix.
            infotype_operation(
              EXPORTING
                im_pernr = ls_master_data-pernr
                im_name = ls_comp-name
                im_data = <fs_data>
              IMPORTING
                ex_return = ls_return
                ).
            IF ls_return IS NOT INITIAL.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              MOVE-CORRESPONDING ls_return TO ls_bapi_return.
              APPEND ls_bapi_return TO lt_bapi_return.
            ELSE.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              ls_bapi_return-message = 'OK'.
              APPEND ls_bapi_return TO lt_bapi_return.
            ENDIF.
          ENDLOOP.
          UNASSIGN <fs_data>.

        WHEN 'P2001'.

          CLEAR: lv_tabix, ls_bapi_return.
          lt_absence = ls_master_data-p2001.
          LOOP AT lt_absence ASSIGNING <fs_data>.
            lv_tabix = sy-tabix.
            infotype_operation(
              EXPORTING
                im_pernr = ls_master_data-pernr
                im_name = ls_comp-name
                im_data = <fs_data>
              IMPORTING
                ex_return = ls_return
                ).
            IF ls_return IS NOT INITIAL.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              MOVE-CORRESPONDING ls_return TO ls_bapi_return.
              APPEND ls_bapi_return TO lt_bapi_return.
            ELSE.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              ls_bapi_return-message = 'OK'.
              APPEND ls_bapi_return TO lt_bapi_return.
            ENDIF.
          ENDLOOP.
          UNASSIGN <fs_data>.

        WHEN 'P2006'.

          CLEAR: lv_tabix, ls_bapi_return.
          lt_aquatas = ls_master_data-p2006.
          LOOP AT lt_aquatas ASSIGNING <fs_data>.
            lv_tabix = sy-tabix.
            infotype_operation(
              EXPORTING
                im_pernr = ls_master_data-pernr
                im_name = ls_comp-name
                im_data = <fs_data>
              IMPORTING
                ex_return = ls_return
                ).
            IF ls_return IS NOT INITIAL.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              MOVE-CORRESPONDING ls_return TO ls_bapi_return.
              APPEND ls_bapi_return TO lt_bapi_return.
            ELSE.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              ls_bapi_return-message = 'OK'.
              APPEND ls_bapi_return TO lt_bapi_return.
            ENDIF.
          ENDLOOP.
          UNASSIGN <fs_data>.

        WHEN OTHERS.

          ASSIGN COMPONENT ls_comp-name OF STRUCTURE ls_master_data TO <fs_data>.

          CLEAR ls_bapi_return.
          lv_tabix = 1.
          IF ls_comp-name = 'P0002' OR ls_comp-name = 'P0007'
            OR ls_comp-name = 'P0016'.

            "BREAK verakovic.

*            IF ls_comp-name = 'P0002'.
*              SELECT SINGLE * INTO CORRESPONDING FIELDS OF ls_pa0002
*                FROM pa0002
*               WHERE pernr = ls_master_data-pernr
*                 AND begda = ls_master_data-p0002-begda
*                 AND endda = ls_master_data-p0002-endda.
*              IF sy-subrc = 0.
*                ls_pa0002-zzmisid = ls_master_data-p0002-misid.
*                MODIFY pa0002 FROM ls_pa0002.
*              ENDIF.
*            ENDIF.
            IF <fs_data> IS NOT INITIAL.
              infotype_operation(
                EXPORTING
                  im_pernr = ls_master_data-pernr
                  im_name = ls_comp-name
                  im_data = <fs_data>
                  im_nosubty = 'X'
                IMPORTING
                  ex_return = ls_return
                  ).
              IF ls_return IS NOT INITIAL.
                ls_bapi_return-infty = ls_comp-name+1(4).
                ls_bapi_return-rownum = lv_tabix.
                MOVE-CORRESPONDING ls_return TO ls_bapi_return.
                APPEND ls_bapi_return TO lt_bapi_return.
              ELSE.
                ls_bapi_return-infty = ls_comp-name+1(4).
                ls_bapi_return-rownum = lv_tabix.
                ls_bapi_return-message = 'OK'.
                APPEND ls_bapi_return TO lt_bapi_return.
              ENDIF.
              UNASSIGN <fs_data>.
            ENDIF.
            IF ls_comp-name = 'P0002'.
              SELECT SINGLE * INTO CORRESPONDING FIELDS OF ls_pa0002
                FROM pa0002
               WHERE pernr = ls_master_data-pernr
                 AND begda = ls_master_data-p0002-begda
                 AND endda = ls_master_data-p0002-endda.
              IF sy-subrc = 0.
                ls_pa0002-zzmisid = ls_master_data-p0002-misid.
                MODIFY pa0002 FROM ls_pa0002.
              ENDIF.
            ENDIF.
          ELSE.
            infotype_operation(
              EXPORTING
                im_pernr = ls_master_data-pernr
                im_name = ls_comp-name
                im_data = <fs_data>
              IMPORTING
                ex_return = ls_return
                ).
            IF ls_return IS NOT INITIAL.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              MOVE-CORRESPONDING ls_return TO ls_bapi_return.
              APPEND ls_bapi_return TO lt_bapi_return.
            ELSE.
              ls_bapi_return-infty = ls_comp-name+1(4).
              ls_bapi_return-rownum = lv_tabix.
              ls_bapi_return-message = 'OK'.
              APPEND ls_bapi_return TO lt_bapi_return.
            ENDIF.
            UNASSIGN <fs_data>.
          ENDIF.

      ENDCASE.

    ENDLOOP.

    "unlock record after modification
    CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = ls_master_data-pernr.


* serialize structure/table into JSON, skipping initial fields and converting ABAP field names into camelCase
    response_data = /ui2/cl_json=>serialize( data = lt_bapi_return " TABELA GRESAKA ILI OK
                                             compress = abap_false
                                             pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

  ELSE.

    response_data = /ui2/cl_json=>serialize( data = 'JSON nije dobro formatiran'
                                             compress = abap_false
                                             pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

  ENDIF.

ENDMETHOD.