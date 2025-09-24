CLASS lsc_zr_tbtru_thieu DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

ENDCLASS.

CLASS lsc_zr_tbtru_thieu IMPLEMENTATION.

  METHOD save_modified.
    DATA : lt_hdr TYPE STANDARD TABLE OF ztb_tru_thieu,
           ls_hdr TYPE                   ztb_tru_thieu.

    DATA : lt_dtl TYPE STANDARD TABLE OF ztb_tru_thie_dtl,
           ls_dtl TYPE                   ztb_tru_thie_dtl.

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

    IF create-zrtbtruthieu IS NOT INITIAL.
      lt_hdr = CORRESPONDING #( create-zrtbtruthieu MAPPING FROM ENTITY ).
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
        CALL METHOD zcl_tru_btpnvl=>get_tru_thieu
          EXPORTING
            i_tru_thieu    = ls_hdr
          IMPORTING
            e_tru_thie_dtl = lt_dtl.

        INSERT ztb_tru_thieu FROM @ls_hdr.
        INSERT ztb_tru_thie_dtl FROM TABLE @lt_dtl.
      ENDLOOP.
    ENDIF.

    LOOP AT delete-zrtbtruthieu INTO DATA(ls_detele)." WHERE HdrID IS NOT INITIAL.
      DELETE FROM ztb_tru_thieu WHERE hdr_id = @ls_detele-HdrID.
    ENDLOOP.

    LOOP AT delete-zrtbtruthiedtl INTO DATA(ls_detele_dtl)." WHERE HdrID IS NOT INITIAL AND DtlID IS NOT INITIAL.
      DELETE FROM ztb_tru_thie_dtl WHERE hdr_id = @ls_detele_dtl-HdrID AND dtl_id = @ls_detele_dtl-DtlID.
    ENDLOOP.

    DATA update_struct TYPE REF TO cl_abap_structdescr.
    IF update-zrtbtruthieu IS NOT INITIAL.
      LOOP AT update-zrtbtruthieu INTO DATA(ls_zrtbtruthieu).
        IF ls_zrtbtruthieu-%control-Sumdate = if_abap_behv=>mk-on.
          SELECT SINGLE *
              FROM ztb_tru_thieu WHERE hdr_id = @ls_zrtbtruthieu-HdrID INTO @ls_hdr.
          IF sy-subrc IS INITIAL.
            ls_hdr-sumdate = ls_zrtbtruthieu-Sumdate.
            ls_hdr-sumdatetime = ls_zrtbtruthieu-SumDateTime.
            CALL METHOD zcl_tru_btpnvl=>get_tru_thieu
              EXPORTING
                i_tru_thieu    = ls_hdr
              IMPORTING
                e_tru_thie_dtl = lt_dtl.
            delete FROM ztb_tru_thie_dtl WHERE hdr_id = @ls_hdr-hdr_id.
            MODIFY ztb_tru_thieu FROM @ls_hdr.
            MODIFY ztb_tru_thie_dtl FROM TABLE @lt_dtl.
          ENDIF.
        ELSE.
          SELECT SINGLE * FROM ztb_tru_thieu
           WHERE hdr_id = @ls_zrtbtruthieu-HdrID
           INTO @DATA(ls_tru_thieu_hdr).
          IF sy-subrc IS INITIAL.
            update_struct ?= cl_abap_structdescr=>describe_by_data( ls_zrtbtruthieu-%control ).
            LOOP AT update_struct->components INTO DATA(field).
              IF ls_zrtbtruthieu-%control-(field-name) = if_abap_behv=>mk-on.
                READ TABLE gt_mapping ASSIGNING <fs_mapping>
                    WITH KEY field_ent = field-name.
                IF sy-subrc IS INITIAL.
                  lw_field_name = <fs_mapping>-field_db.
                ELSE.
                  lw_field_name = field-name.
                ENDIF.
                ls_tru_thieu_hdr-(lw_field_name) = ls_zrtbtruthieu-(field-name).

              ENDIF.
            ENDLOOP.

            FREE update_struct.

            MODIFY ztb_tru_thieu FROM @ls_hdr.
            MODIFY ztb_tru_thie_dtl FROM TABLE @lt_dtl.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF update-zrtbtruthiedtl IS NOT INITIAL.
      LOOP AT update-zrtbtruthiedtl INTO DATA(ls_zrtbtruthiedtl).
        SELECT SINGLE * FROM ztb_tru_thie_dtl
          WHERE hdr_id = @ls_zrtbtruthiedtl-HdrID AND dtl_id = @ls_zrtbtruthiedtl-DtlID
          INTO @ls_dtl.
        IF sy-subrc IS INITIAL.
          update_struct ?= cl_abap_structdescr=>describe_by_data( ls_zrtbtruthiedtl-%control ).
          LOOP AT update_struct->components INTO field.
            IF ls_zrtbtruthiedtl-%control-(field-name) = if_abap_behv=>mk-on.
              READ TABLE gt_mapping_dtl ASSIGNING <fs_mapping>
                  WITH KEY field_ent = field-name.
              IF sy-subrc IS INITIAL.
                lw_field_name = <fs_mapping>-field_db.
              ELSE.
                lw_field_name = field-name.
              ENDIF.
              ls_dtl-(lw_field_name) = ls_zrtbtruthiedtl-(field-name).

            ENDIF.
          ENDLOOP.

          CALL METHOD zcl_tru_btpnvl=>update_tru_thieu_dtl
            CHANGING
              c_tru_thie_dtl = ls_dtl.

          FREE update_struct.
          MODIFY ztb_tru_thie_dtl FROM @ls_dtl.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_zr_tbtru_thieu DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS:
      get_global_authorizations FOR GLOBAL AUTHORIZATION
        IMPORTING
        REQUEST requested_authorizations FOR ZrTbtruThieu
        RESULT result,
      get_instance_features FOR INSTANCE FEATURES
        IMPORTING keys REQUEST requested_features FOR  ZrTbtruThieu RESULT result,
      UpdateData FOR MODIFY
        IMPORTING keys FOR ACTION ZrTbtruThieu~UpdateData RESULT result.
ENDCLASS.

CLASS lhc_zr_tbtru_thieu IMPLEMENTATION.
  METHOD get_global_authorizations.
  ENDMETHOD.
  METHOD get_instance_features.
  ENDMETHOD.
  METHOD UpdateData.
    DATA : lt_dtl TYPE STANDARD TABLE OF ztb_xetduyet_dtl,
           ls_hdr TYPE                   ztb_tru_thieu.
    DATA: lw_dat      TYPE zde_date,
          lw_datetime TYPE abp_locinst_lastchange_tstmpl.

    lw_dat = xco_cp=>sy->moment( xco_cp_time=>time_zone->user )->date->as( xco_cp_time=>format->abap )->value.
*    GET TIME STAMP FIELD lw_datetime.
    CONVERT DATE cl_abap_context_info=>get_system_date( ) TIME cl_abap_context_info=>get_system_time( )
        INTO TIME STAMP lw_datetime TIME ZONE 'UTC-7'.
    READ ENTITIES OF zr_tbtru_thieu IN LOCAL MODE
     ENTITY ZrTbtruThieu
     ALL FIELDS
     WITH CORRESPONDING #( keys )
     RESULT DATA(ZrTbtruThieu).

    LOOP AT ZrTbtruThieu INTO DATA(ls_ZrTbtruThieu).
      ls_hdr = CORRESPONDING #( ls_ZrTbtruThieu MAPPING FROM ENTITY ).
      MODIFY ENTITIES OF zr_tbtru_thieu IN LOCAL MODE
        ENTITY ZrTbtruThieu
      UPDATE FIELDS ( sumdate  SumDateTime )
      WITH VALUE #(
        ( %tky = ls_ZrTbtruThieu-%tky
          sumdate =  lw_dat
          SumDateTime = lw_datetime ) ).
    ENDLOOP.

    "return result entities
    result = VALUE #( FOR ls_for_Hdr IN ZrTbtruThieu ( %tky   = ls_for_Hdr-%tky
                                                  %param = ls_for_Hdr ) ).
  ENDMETHOD.

ENDCLASS.
