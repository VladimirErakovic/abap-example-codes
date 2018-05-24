METHOD infotype_operation.

  FIELD-SYMBOLS: <fs_infty> TYPE ANY,
                 <fs_subty> TYPE ANY,
                 <fs_begda> TYPE ANY,
                 <fs_endda> TYPE ANY.

  " for modify operation
  FIELD-SYMBOLS: <fs_ival> TYPE infty,
                 <fs_pernr> TYPE persno,
                 <fs_subty_copy> TYPE ANY.

  DATA: lo_data TYPE REF TO data.

  DATA: ls_infotype_number TYPE c LENGTH 4,
        lv_operation TYPE pspar-actio,
        ls_bapiret TYPE bapireturn1.

  IF im_data IS NOT INITIAL.

    IF im_nosubty IS INITIAL.
      ASSIGN COMPONENT 'SUBTY' OF STRUCTURE im_data TO <fs_subty>.
    ENDIF.
    ASSIGN COMPONENT 'ENDDA' OF STRUCTURE im_data TO <fs_endda>.
    ASSIGN COMPONENT 'BEGDA' OF STRUCTURE im_data TO <fs_begda>.

    ls_infotype_number = im_name+1(4).

    IF im_nosubty IS INITIAL.
      CALL FUNCTION 'HR_INFOTYPE_CHECKEXISTENCE'
        EXPORTING
          number              = im_pernr
          infty               = ls_infotype_number
          subtype             = <fs_subty>
          validitybegin       = <fs_begda>
          validityend         = <fs_endda>
        IMPORTING
          return              = ls_bapiret.
    ELSE.
        CALL FUNCTION 'HR_INFOTYPE_CHECKEXISTENCE'
          EXPORTING
            number              = im_pernr
            infty               = ls_infotype_number
            validitybegin       = <fs_begda>
            validityend         = <fs_endda>
          IMPORTING
            return              = ls_bapiret.
    ENDIF.

    IF ls_bapiret IS INITIAL.
      lv_operation = 'MOD'.
    ELSE.
      lv_operation = 'INS'.
    ENDIF.


    CREATE DATA lo_data TYPE (im_name).

    ASSIGN lo_data->* TO <fs_infty>.
    MOVE-CORRESPONDING im_data TO <fs_infty>.

    IF lv_operation = 'MOD'.
      ASSIGN COMPONENT 'INFTY' OF STRUCTURE <fs_infty> TO <fs_ival>.
      <fs_ival> = ls_infotype_number.
      ASSIGN COMPONENT 'PERNR' OF STRUCTURE <fs_infty> TO <fs_pernr>.
      <fs_pernr> = im_pernr.
      CASE ls_infotype_number.
        WHEN '0006'.
          ASSIGN COMPONENT 'ANSSA' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
          ASSIGN <fs_subty_copy> TO <fs_subty>.
        WHEN '0009'.
          ASSIGN COMPONENT 'BNKSA' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
          ASSIGN <fs_subty_copy> TO <fs_subty>.
        WHEN '0021'.
          ASSIGN COMPONENT 'FAMSA' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
          ASSIGN <fs_subty_copy> TO <fs_subty>.
        WHEN '0022'.
          ASSIGN COMPONENT 'SLART' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
          ASSIGN <fs_subty_copy> TO <fs_subty>.
        WHEN '0105'.
          ASSIGN COMPONENT 'USRTY' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
          ASSIGN <fs_subty_copy> TO <fs_subty>.
        WHEN '0552'.
          ASSIGN COMPONENT 'CVTYP' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
          ASSIGN <fs_subty_copy> TO <fs_subty>.
        WHEN '2001'.
          ASSIGN COMPONENT 'AWART' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
          ASSIGN <fs_subty_copy> TO <fs_subty>.
        WHEN '2006'.
          ASSIGN COMPONENT 'KTART' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
          ASSIGN <fs_subty_copy> TO <fs_subty>.
      ENDCASE.
    ENDIF.

    IF lv_operation = 'INS'.
      CASE ls_infotype_number.
        WHEN '0009'.
          ASSIGN COMPONENT 'BNKSA' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
          IF <fs_subty_copy> <> <fs_subty>.
            ex_return-message = 'SUBTY i BNKSA nisu isti'.
            EXIT.
          ENDIF.
        WHEN '0021'.
          ASSIGN COMPONENT 'FAMSA' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
          IF <fs_subty_copy> <> <fs_subty>.
            ex_return-message = 'SUBTY i FAMSA nisu isti'.
            EXIT.
          ENDIF.
        WHEN '0022'.
          ASSIGN COMPONENT 'SLART' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
          IF <fs_subty_copy> <> <fs_subty>.
            ex_return-message = 'SUBTY i SLART nisu isti'.
            EXIT.
          ENDIF.
        WHEN '0105'.
          ASSIGN COMPONENT 'USRTY' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
          IF <fs_subty_copy> <> <fs_subty>.
            ex_return-message = 'SUBTY i USRTY nisu isti'.
            EXIT.
          ENDIF.
        WHEN '0552'.
          ASSIGN COMPONENT 'CVTYP' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
          IF <fs_subty_copy> <> <fs_subty>.
            ex_return-message = 'SUBTY i CVTYP nisu isti'.
            EXIT.
          ENDIF.
        WHEN '2001'.
          ASSIGN COMPONENT 'AWART' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
          IF <fs_subty_copy> <> <fs_subty>.
            ex_return-message = 'SUBTY i AWART nisu isti'.
            EXIT.
          ENDIF.
        WHEN '2006'.
          ASSIGN COMPONENT 'KTART' OF STRUCTURE <fs_infty> TO <fs_subty_copy>.
          IF <fs_subty_copy> <> <fs_subty>.
            ex_return-message = 'SUBTY i KTART nisu isti'.
            EXIT.
          ENDIF.
      ENDCASE.
    ENDIF.

    IF im_nosubty IS INITIAL.
      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
        EXPORTING
          infty         = ls_infotype_number
          number        = im_pernr
          subtype       = <fs_subty>
          validityend   = <fs_endda>
          validitybegin = <fs_begda>
          record        = <fs_infty>
          operation     = lv_operation
        IMPORTING
          return        = ex_return.
    ELSE.
      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
        EXPORTING
          infty         = ls_infotype_number
          number        = im_pernr
          validityend   = <fs_endda>
          validitybegin = <fs_begda>
          record        = <fs_infty>
          operation     = lv_operation
        IMPORTING
          return        = ex_return.
    ENDIF.

  ENDIF.


ENDMETHOD.