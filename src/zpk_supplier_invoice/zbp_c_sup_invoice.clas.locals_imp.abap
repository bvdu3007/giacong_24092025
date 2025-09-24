CLASS lhc_zc_sup_invoice DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zc_sup_invoice RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR zc_sup_invoice RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE zc_sup_invoice.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE zc_sup_invoice.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE zc_sup_invoice.

    METHODS read FOR READ
      IMPORTING keys FOR READ zc_sup_invoice RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zc_sup_invoice.

ENDCLASS.

CLASS lhc_zc_sup_invoice IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD create.
  ENDMETHOD.

  METHOD update.
  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZC_SUP_INVOICE DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZC_SUP_INVOICE IMPLEMENTATION.

  METHOD finalize.

  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.
*   MODIFY ENTITIES OF zc_sup_invoice IN LOCAL MODE
*      ENTITY zc_sup_invoice
*        CREATE FIELDS ( InvoiceId Trangthai )
*          WITH REPORTED-zc_sup_invoice
*        UPDATE FIELDS ( customerid total_amount )
*          WITH update-salesorder
*        DELETE FROM delete-salesorder
  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
