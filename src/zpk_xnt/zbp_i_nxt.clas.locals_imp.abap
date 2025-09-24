CLASS lhc_zi_nxt DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zi_nxt RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ zi_nxt RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zi_nxt.
    METHODS createxnt FOR MODIFY
      IMPORTING keys FOR ACTION zi_nxt~createxnt.

ENDCLASS.

CLASS lhc_zi_nxt IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD CreateXNT.
    DATA n TYPE i.
    DATA: lt_bc_xnt TYPE TABLE OF zc_bc_xnt,
          lt_xnt    TYPE TABLE OF ztb_nxt.

    READ TABLE keys INTO DATA(ls_key) INDEX 1.
    DATA(lw_fr) = ls_key-%param-perfr.
    DATA(lw_to) = ls_key-%param-perto.
    IF lw_to - lw_fr > 50.
      RETURN.
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
      LOOP AT lt_bc_xnt INTO DATA(ls_bc_xnt).
        APPEND INITIAL LINE TO lt_xnt ASSIGNING FIELD-SYMBOL(<ls_xnt>).
        MOVE-CORRESPONDING ls_bc_xnt TO <ls_xnt>.
        <ls_xnt>-quantityinbaseunit = ls_bc_xnt-TonCuoi.
      ENDLOOP.

*      DELETE FROM ztb_nxt WHERE zper = @lw_fr.
      MODIFY ztb_nxt FROM TABLE @lt_xnt.

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

CLASS lsc_ZI_NXT DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZI_NXT IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.
  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
