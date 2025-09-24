CLASS lhc_zr_tbnxt DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS:
      get_global_authorizations FOR GLOBAL AUTHORIZATION
        IMPORTING
        REQUEST requested_authorizations FOR ZrTbnxt
        RESULT result,
      CreateXNT FOR MODIFY
        IMPORTING keys FOR ACTION ZrTbnxt~CreateXNT.
ENDCLASS.

CLASS lhc_zr_tbnxt IMPLEMENTATION.
  METHOD get_global_authorizations.
  ENDMETHOD.
  METHOD CreateXNT.
    DATA: n TYPE i,
    n1 TYPE i.
    DATA: lt_bc_xnt TYPE TABLE OF zc_bc_xnt,
          lt_xnt    TYPE TABLE OF ztb_nxt.
    DATA: lt_nxt_cr TYPE TABLE FOR CREATE zr_tbnxt,
          ls_nxt_cr TYPE STRUCTURE FOR CREATE zr_tbnxt.
    DATA: lt_nxt_ud TYPE TABLE FOR UPDATE zr_tbnxt,
          ls_nxt_ud TYPE STRUCTURE FOR UPDATE zr_tbnxt.
    DATA: lt_nxt_dl TYPE TABLE FOR DELETE zr_tbnxt,
          ls_nxt_dl TYPE STRUCTURE FOR DELETE zr_tbnxt.

    READ TABLE keys INTO DATA(ls_key) INDEX 1.
    DATA(lw_fr) = ls_key-%param-perfr.
    DATA(lw_to) = ls_key-%param-perto.
    IF lw_to IS INITIAL OR lw_to < lw_fr.
      lw_to = lw_fr.
    ENDIF.

    WHILE lw_fr <= lw_to.
      SELECT SINGLE * FROM ztb_period
        WHERE zper = @lw_fr
        INTO @DATA(ls_period).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      CALL METHOD zcl_xnt=>get_xnt
        EXPORTING
          i_datefr = ls_period-zdatefr
          i_dateto = ls_period-zdateto
        IMPORTING
          e_nxt    = lt_bc_xnt.

      SELECT * FROM ztb_nxt WHERE zper = @lw_fr INTO TABLE @lt_xnt .
      LOOP AT lt_xnt INTO DATA(ls_xnt).
        READ TABLE lt_bc_xnt WITH KEY material = ls_xnt-material
                                              plant = ls_xnt-plant
                                              supplier = ls_xnt-supplier
                                              orderid = ls_xnt-orderid
                              ASSIGNING FIELD-SYMBOL(<ls_bc_xnt>).
        IF sy-subrc IS NOT INITIAL.
          APPEND INITIAL LINE TO lt_nxt_dl ASSIGNING FIELD-SYMBOL(<ls_nxt_dl>).
          <ls_nxt_dl>-Zper = lw_fr.
          <ls_nxt_dl>-Material =  ls_xnt-Material.
          <ls_nxt_dl>-Plant =  ls_xnt-Plant.
          <ls_nxt_dl>-Supplier =  ls_xnt-Supplier.
          <ls_nxt_dl>-Orderid =  ls_xnt-Orderid.
        ENDIF.
      ENDLOOP.

      n1 = 0.
      LOOP AT lt_bc_xnt INTO DATA(ls_bc_xnt).
          n1 = n1 + 1.
        READ TABLE lt_xnt WITH KEY material = ls_bc_xnt-material
                                              plant = ls_bc_xnt-plant
                                              supplier = ls_bc_xnt-supplier
                                              orderid = ls_bc_xnt-orderid
                              ASSIGNING FIELD-SYMBOL(<ls_xnt>).
        IF sy-subrc IS NOT INITIAL.
          APPEND INITIAL LINE TO lt_nxt_cr ASSIGNING FIELD-SYMBOL(<ls_xnt_cr>).
          MOVE-CORRESPONDING ls_bc_xnt TO <ls_xnt_cr>.
          <ls_xnt_cr>-Zper = lw_fr.
          <ls_xnt_cr>-quantityinbaseunit = ls_bc_xnt-TonCuoi.
          <ls_xnt_cr>-%cid = 'CRXNT' && n && n1.
        ELSE.
          APPEND INITIAL LINE TO lt_nxt_ud ASSIGNING FIELD-SYMBOL(<ls_xnt_ud>).
          MOVE-CORRESPONDING ls_bc_xnt TO <ls_xnt_ud>.
          <ls_xnt_ud>-Zper = lw_fr.
          <ls_xnt_ud>-quantityinbaseunit = ls_bc_xnt-TonCuoi.
        ENDIF.
      ENDLOOP.

      IF lt_nxt_cr[] IS NOT INITIAL.
        MODIFY ENTITIES OF zr_tbnxt IN LOCAL MODE
          ENTITY ZrTbnxt
            CREATE FIELDS ( Material Plant Supplier Orderid Zper Quantityinbaseunit Materialbaseunit )
            WITH lt_nxt_cr.
      ENDIF.

      IF lt_nxt_ud[] IS NOT INITIAL.
        MODIFY ENTITIES OF zr_tbnxt IN LOCAL MODE
          ENTITY ZrTbnxt
            UPDATE FIELDS ( Quantityinbaseunit Materialbaseunit )
            WITH lt_nxt_ud.
      ENDIF.

      IF lt_nxt_dl[] IS NOT INITIAL.
        MODIFY ENTITIES OF zr_tbnxt IN LOCAL MODE
            ENTITY ZrTbnxt
                DELETE
                FROM lt_nxt_dl.
      ENDIF.

      SELECT SINGLE * FROM ztb_period
        WHERE lastper = @lw_fr
        INTO @ls_period.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      lw_fr = ls_period-zper.

      n = n + 1.
      IF n > 12.
        EXIT.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.
