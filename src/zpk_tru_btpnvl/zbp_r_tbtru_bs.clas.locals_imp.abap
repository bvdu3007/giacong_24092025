CLASS lsc_zr_tbtru_bs DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

ENDCLASS.

CLASS lsc_zr_tbtru_bs IMPLEMENTATION.

  METHOD save_modified.
    DATA : lt_hdr TYPE STANDARD TABLE OF ztb_tru_bs,
           ls_hdr TYPE                   ztb_tru_bs.

    DATA : lt_dtl TYPE STANDARD TABLE OF ztb_tru_bs_dtl,
           ls_dtl TYPE                   ztb_tru_bs_dtl.

    DATA: lw_dat      TYPE zde_date,
          lw_datetime TYPE abp_locinst_lastchange_tstmpl.

    TYPES: BEGIN OF ty_mapping,
             field_ent TYPE string,
             field_db  TYPE string,
           END OF ty_mapping.
    DATA: lw_field_name TYPE char72.
    DATA: gt_mapping TYPE STANDARD TABLE OF ty_mapping WITH DEFAULT KEY.

    "Insert các cặp mapping
    APPEND VALUE #( field_ent = 'HdrID'         field_db = 'hdr_id' )         TO gt_mapping.
    APPEND VALUE #( field_ent = 'Zper'          field_db = 'zper' )          TO gt_mapping.
    APPEND VALUE #( field_ent = 'Zperdesc'      field_db = 'zperdesc' )      TO gt_mapping.
    APPEND VALUE #( field_ent = 'CreatedBy'     field_db = 'created_by' )    TO gt_mapping.
    APPEND VALUE #( field_ent = 'CreatedAt'     field_db = 'created_at' )    TO gt_mapping.
    APPEND VALUE #( field_ent = 'LastChangedBy' field_db = 'last_changed_by' ) TO gt_mapping.
    APPEND VALUE #( field_ent = 'LastChangedAt' field_db = 'last_changed_at' ) TO gt_mapping.

    LOOP AT gt_mapping ASSIGNING FIELD-SYMBOL(<fs_mapping>).
      TRANSLATE <fs_mapping>-field_ent TO UPPER CASE.
      TRANSLATE <fs_mapping>-field_db TO UPPER CASE.
    ENDLOOP.
    DATA: gt_mapping_dtl TYPE STANDARD TABLE OF ty_mapping WITH DEFAULT KEY.

    APPEND VALUE #( field_ent = 'CreatedBy'     field_db = 'created_by' )    TO gt_mapping_dtl.
    APPEND VALUE #( field_ent = 'CreatedAt'     field_db = 'created_at' )    TO gt_mapping_dtl.
    APPEND VALUE #( field_ent = 'LastChangedBy' field_db = 'last_changed_by' ) TO gt_mapping_dtl.
    APPEND VALUE #( field_ent = 'LastChangedAt' field_db = 'last_changed_at' ) TO gt_mapping_dtl.

    LOOP AT gt_mapping_dtl ASSIGNING  <fs_mapping>.
      TRANSLATE <fs_mapping>-field_ent TO UPPER CASE.
      TRANSLATE <fs_mapping>-field_db TO UPPER CASE.
    ENDLOOP.

    IF create-zrtbtrubs IS NOT INITIAL.
      lt_hdr = CORRESPONDING #( create-zrtbtrubs MAPPING FROM ENTITY ).
      LOOP AT lt_hdr INTO ls_hdr.
        lw_dat = xco_cp=>sy->moment( xco_cp_time=>time_zone->user )->date->as( xco_cp_time=>format->abap )->value.
        ls_hdr-sumdate = lw_dat.
        CONVERT DATE cl_abap_context_info=>get_system_date( ) TIME cl_abap_context_info=>get_system_time( )
        INTO TIME STAMP lw_datetime TIME ZONE 'UTC-7'.
        ls_hdr-sumdatetime = lw_datetime.
        SELECT SINGLE * FROM zr_tbperiod
          WHERE Zper = @ls_hdr-zper
          INTO  @DATA(ls_tbperiod).
        IF sy-subrc = 0.
          ls_hdr-zperdesc = ls_tbperiod-Zdesc.
        ENDIF.
        CALL METHOD zcl_tru_btpnvl_bs=>get_tru_bs
          EXPORTING
            i_tru_bs     = ls_hdr
          IMPORTING
            e_tru_bs_dtl = lt_dtl.

        INSERT ztb_tru_bs FROM @ls_hdr.
        INSERT ztb_tru_bs_dtl FROM TABLE @lt_dtl.
      ENDLOOP.
    ENDIF.

    LOOP AT delete-zrtbtrubs INTO DATA(ls_detele)." WHERE HdrID IS NOT INITIAL.
      DELETE FROM ztb_tru_bs WHERE hdr_id = @ls_detele-HdrID.
    ENDLOOP.

    LOOP AT delete-zrtbtrubsdtl INTO DATA(ls_detele_dtl)." WHERE HdrID IS NOT INITIAL AND DtlID IS NOT INITIAL.
      DELETE FROM ztb_tru_bs_dtl WHERE hdr_id = @ls_detele_dtl-HdrID AND dtl_id = @ls_detele_dtl-DtlID.
    ENDLOOP.

    DATA update_struct TYPE REF TO cl_abap_structdescr.
    IF update-zrtbtrubs IS NOT INITIAL.
      LOOP AT update-zrtbtrubs INTO DATA(ls_zrtbtrubs).
        IF ls_zrtbtrubs-%control-Sumdate = if_abap_behv=>mk-on.
          SELECT SINGLE *
              FROM ztb_tru_bs WHERE hdr_id = @ls_zrtbtrubs-HdrID INTO @ls_hdr.
          IF sy-subrc IS INITIAL.
            ls_hdr-sumdate = ls_zrtbtrubs-Sumdate.
            ls_hdr-sumdatetime = ls_zrtbtrubs-SumDateTime.
            CALL METHOD zcl_tru_btpnvl_bs=>get_tru_bs
              EXPORTING
                i_tru_bs     = ls_hdr
              IMPORTING
                e_tru_bs_dtl = lt_dtl.
            DELETE FROM ztb_tru_bs_dtl WHERE hdr_id = @ls_hdr-hdr_id.
            MODIFY ztb_tru_bs FROM @ls_hdr.
            MODIFY ztb_tru_bs_dtl FROM TABLE @lt_dtl.
          ENDIF.
        ELSE.
          SELECT SINGLE * FROM ztb_tru_bs
           WHERE hdr_id = @ls_zrtbtrubs-HdrID
           INTO @DATA(ls_tru_bs_hdr).
          IF sy-subrc IS INITIAL.
            update_struct ?= cl_abap_structdescr=>describe_by_data( ls_zrtbtrubs-%control ).
            LOOP AT update_struct->components INTO DATA(field).
              IF ls_zrtbtrubs-%control-(field-name) = if_abap_behv=>mk-on.
                READ TABLE gt_mapping ASSIGNING <fs_mapping>
                    WITH KEY field_ent = field-name.
                IF sy-subrc IS INITIAL.
                  lw_field_name = <fs_mapping>-field_db.
                ELSE.
                  lw_field_name = field-name.
                ENDIF.
                ls_tru_bs_hdr-(lw_field_name) = ls_zrtbtrubs-(field-name).

              ENDIF.
            ENDLOOP.

            FREE update_struct.

            MODIFY ztb_tru_bs FROM @ls_hdr.
            MODIFY ztb_tru_bs_dtl FROM TABLE @lt_dtl.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF update-zrtbtrubsdtl IS NOT INITIAL.
      LOOP AT update-zrtbtrubsdtl INTO DATA(ls_zrtbtrubsdtl).
        SELECT SINGLE * FROM ztb_tru_bs_dtl
          WHERE hdr_id = @ls_zrtbtrubsdtl-HdrID AND dtl_id = @ls_zrtbtrubsdtl-DtlID
          INTO @ls_dtl.
        IF sy-subrc IS INITIAL.
          update_struct ?= cl_abap_structdescr=>describe_by_data( ls_zrtbtrubsdtl-%control ).
          LOOP AT update_struct->components INTO field.
            IF ls_zrtbtrubsdtl-%control-(field-name) = if_abap_behv=>mk-on.
              READ TABLE gt_mapping_dtl ASSIGNING <fs_mapping>
                  WITH KEY field_ent = field-name.
              IF sy-subrc IS INITIAL.
                lw_field_name = <fs_mapping>-field_db.
              ELSE.
                lw_field_name = field-name.
              ENDIF.
              ls_dtl-(lw_field_name) = ls_zrtbtrubsdtl-(field-name).

            ENDIF.
          ENDLOOP.

          CALL METHOD zcl_tru_btpnvl_bs=>update_tru_bs_dtl
            CHANGING
              c_tru_bs_dtl = ls_dtl.

          FREE update_struct.
          MODIFY ztb_tru_bs_dtl FROM @ls_dtl.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_zr_tbtru_bs DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS:
      get_global_authorizations FOR GLOBAL AUTHORIZATION
        IMPORTING
        REQUEST requested_authorizations FOR ZrTbtruBs
        RESULT result,
      get_instance_features FOR INSTANCE FEATURES
        IMPORTING keys REQUEST requested_features FOR ZrTbtruBs RESULT result.

    METHODS UpdateData FOR MODIFY
      IMPORTING keys FOR ACTION ZrTbtruBs~UpdateData RESULT result.
ENDCLASS.

CLASS lhc_zr_tbtru_bs IMPLEMENTATION.
  METHOD get_global_authorizations.
  ENDMETHOD.
  METHOD get_instance_features.
  ENDMETHOD.

  METHOD UpdateData.
    DATA : lt_dtl TYPE STANDARD TABLE OF ztb_xetduyet_dtl,
           ls_hdr TYPE                   ztb_tru_bs.
    DATA: lw_dat      TYPE zde_date,
          lw_datetime TYPE abp_locinst_lastchange_tstmpl.

    lw_dat = xco_cp=>sy->moment( xco_cp_time=>time_zone->user )->date->as( xco_cp_time=>format->abap )->value.
*    GET TIME STAMP FIELD lw_datetime.
    CONVERT DATE cl_abap_context_info=>get_system_date( ) TIME cl_abap_context_info=>get_system_time( )
        INTO TIME STAMP lw_datetime TIME ZONE 'UTC-7'.
    READ ENTITIES OF zr_tbtru_bs IN LOCAL MODE
     ENTITY ZrTbtrubs
     ALL FIELDS
     WITH CORRESPONDING #( keys )
     RESULT DATA(ZrTbtrubs).

    LOOP AT ZrTbtrubs INTO DATA(ls_ZrTbtrubs).
      ls_hdr = CORRESPONDING #( ls_ZrTbtrubs MAPPING FROM ENTITY ).
      MODIFY ENTITIES OF zr_tbtru_bs IN LOCAL MODE
        ENTITY ZrTbtrubs
      UPDATE FIELDS ( sumdate  SumDateTime )
      WITH VALUE #(
        ( %tky = ls_ZrTbtrubs-%tky
          sumdate =  lw_dat
          SumDateTime = lw_datetime ) ).
    ENDLOOP.

    "return result entities
    result = VALUE #( FOR ls_for_Hdr IN ZrTbtrubs ( %tky   = ls_for_Hdr-%tky
                                                  %param = ls_for_Hdr ) ).
  ENDMETHOD.

ENDCLASS.
