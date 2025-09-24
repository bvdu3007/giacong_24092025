CLASS lsc_zr_tb_maloi_hang DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zr_tb_maloi_hang IMPLEMENTATION.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_zr_tb_maloi_hang DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.
    METHODS:
      get_global_authorizations FOR GLOBAL AUTHORIZATION
        IMPORTING
        REQUEST requested_authorizations FOR ZrTbMaloiHang
        RESULT result,

      get_instance_features FOR INSTANCE FEATURES
        IMPORTING keys REQUEST requested_features FOR ZrTbMaloiHang RESULT result.

ENDCLASS.

CLASS lhc_zr_tb_maloi_hang IMPLEMENTATION.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD get_instance_features.
  ENDMETHOD.

ENDCLASS.
