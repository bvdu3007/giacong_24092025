CLASS lhc_zrtbdcgcdtl DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR ZrTbdcgcDtl RESULT result.

ENDCLASS.

CLASS lhc_zrtbdcgcdtl IMPLEMENTATION.

  METHOD get_instance_features.
    READ ENTITIES OF zr_tbdcgc_hdr IN LOCAL MODE
              ENTITY ZrTbdcgcHdr
                 FIELDS (  Trangthai )
                 WITH CORRESPONDING #( keys )
               RESULT DATA(lt_read_data)
               FAILED failed.
    READ ENTITIES OF zr_tbdcgc_hdr IN LOCAL MODE
            ENTITY ZrTbdcgcDtl
               FIELDS (  DtlID )
               WITH CORRESPONDING #( keys )
             RESULT DATA(lt_read_data_dtl)
             FAILED failed.
    READ TABLE lt_read_data INTO DATA(ls_data_hdr) INDEX 1.

    result = VALUE #( FOR ls_read_data_dtl IN lt_read_data_dtl
                   ( %tky                           = ls_read_data_dtl-%tky

                     %features-%update = COND #( WHEN ls_data_hdr-Trangthai > '0'
                                                              THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  )
                     %features-%delete = COND #( WHEN ls_data_hdr-Trangthai > '0'
                                                              THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  )

                  ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lsc_zr_tbdcgc_hdr DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

ENDCLASS.

CLASS lsc_zr_tbdcgc_hdr IMPLEMENTATION.

  METHOD save_modified.
    DATA : lt_hdr TYPE STANDARD TABLE OF ztb_dcgc_hdr,
           ls_hdr TYPE                   ztb_dcgc_hdr.

    DATA : lt_dtl TYPE STANDARD TABLE OF ztb_dcgc_dtl,
           ls_dtl TYPE                   ztb_dcgc_dtl.

    DATA: lw_dat      TYPE zde_date,
          lw_datetime TYPE abp_locinst_lastchange_tstmpl.

    TYPES: BEGIN OF ty_mapping,
             field_ent TYPE string,
             field_db  TYPE string,
           END OF ty_mapping.
    DATA: lw_field_name TYPE char72.
    DATA: gt_mapping TYPE STANDARD TABLE OF ty_mapping WITH DEFAULT KEY.

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

    IF create-zrtbdcgchdr IS NOT INITIAL.
      lt_hdr = CORRESPONDING #( create-zrtbdcgchdr MAPPING FROM ENTITY ).
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
        CALL METHOD zcl_dc_giacong=>get_data
          EXPORTING
            i_hdr   = ls_hdr
          IMPORTING
            e_hdr   = ls_hdr
            e_t_dtl = lt_dtl.

        CALL METHOD zcl_dc_giacong=>update_hdr
          EXPORTING
            i_t_dtl = lt_dtl
          CHANGING
            c_hdr   = ls_hdr.

        INSERT ztb_dcgc_hdr FROM @ls_hdr.
        INSERT ztb_dcgc_dtl FROM TABLE @lt_dtl.
      ENDLOOP.
    ENDIF.

    LOOP AT delete-zrtbdcgchdr INTO DATA(ls_detele).
      DELETE FROM ztb_dcgc_hdr WHERE hdr_id = @ls_detele-HdrID.
    ENDLOOP.

    IF create-zrtbdcgcdtl IS NOT INITIAL.
*      lt_dtl = CORRESPONDING #( create-zrtbxetduyetdtl MAPPING FROM ENTITY ).
*      INSERT ztb_xetduyet_dtl FROM TABLE @lt_dtl.
    ENDIF.

    LOOP AT delete-zrtbdcgcdtl INTO DATA(ls_detele_dtl)." WHERE HdrID IS NOT INITIAL AND DtlID IS NOT INITIAL.
      DELETE FROM ztb_dcgc_dtl WHERE hdr_id = @ls_detele_dtl-HdrID AND dtl_id = @ls_detele_dtl-DtlID.
    ENDLOOP.

    DATA update_struct TYPE REF TO cl_abap_structdescr.
    IF update-zrtbdcgchdr IS NOT INITIAL.
      LOOP AT update-zrtbdcgchdr INTO DATA(ls_ud_hdr).
        IF ls_ud_hdr-%control-Sumdate = if_abap_behv=>mk-on.

          SELECT SINGLE *
              FROM ztb_dcgc_hdr WHERE hdr_id = @ls_ud_hdr-HdrID INTO @ls_hdr.
          IF sy-subrc IS INITIAL.
            ls_hdr-sumdate = ls_ud_hdr-Sumdate.
            ls_hdr-sumdatetime = ls_ud_hdr-SumDateTime.
            IF ls_ud_hdr-%control-trangthai = if_abap_behv=>mk-on.
              ls_hdr-trangthai = ls_ud_hdr-trangthai.
            ENDIF.
            CALL METHOD zcl_dc_giacong=>get_data
              EXPORTING
                i_hdr   = ls_hdr
              IMPORTING
                e_hdr   = ls_hdr
                e_t_dtl = lt_dtl.
            CALL METHOD zcl_dc_giacong=>update_hdr
              EXPORTING
                i_t_dtl = lt_dtl
              CHANGING
                c_hdr   = ls_hdr.

            MODIFY ztb_dcgc_hdr FROM @ls_hdr.
            MODIFY ztb_dcgc_dtl FROM TABLE @lt_dtl.
          ENDIF.
        ELSE.
          SELECT SINGLE * FROM ztb_dcgc_hdr
           WHERE hdr_id = @ls_ud_hdr-HdrID
           INTO @ls_hdr.
          IF sy-subrc IS INITIAL.
            SELECT * FROM ztb_dcgc_dtl
              WHERE hdr_id = @ls_ud_hdr-HdrID
              INTO TABLE @lt_dtl.
            update_struct ?= cl_abap_structdescr=>describe_by_data( ls_ud_hdr-%control ).
            LOOP AT update_struct->components INTO DATA(field).
              IF ls_ud_hdr-%control-(field-name) = if_abap_behv=>mk-on.
                READ TABLE gt_mapping ASSIGNING <fs_mapping>
                    WITH KEY field_ent = field-name.
                IF sy-subrc IS INITIAL.
                  lw_field_name = <fs_mapping>-field_db.
                ELSE.
                  lw_field_name = field-name.
                ENDIF.
                ls_hdr-(lw_field_name) = ls_ud_hdr-(field-name).

              ENDIF.
            ENDLOOP.

            FREE update_struct.

            CALL METHOD zcl_dc_giacong=>update_hdr
              EXPORTING
                i_t_dtl = lt_dtl
              CHANGING
                c_hdr   = ls_hdr.

            MODIFY ztb_dcgc_hdr FROM @ls_hdr.
*            MODIFY ztb_dcgc_dtl FROM TABLE @lt_dtl_db.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF update-zrtbdcgcdtl IS NOT INITIAL.
      LOOP AT update-zrtbdcgcdtl INTO DATA(ls_dtl_ud).

        SELECT SINGLE * FROM ztb_dcgc_dtl
          WHERE hdr_id = @ls_dtl_ud-HdrID AND dtl_id = @ls_dtl_ud-DtlID
          INTO @ls_dtl.
        IF sy-subrc IS INITIAL.
          update_struct ?= cl_abap_structdescr=>describe_by_data( ls_dtl_ud-%control ).
          LOOP AT update_struct->components INTO field.
            IF ls_dtl_ud-%control-(field-name) = if_abap_behv=>mk-on.
              READ TABLE gt_mapping_dtl ASSIGNING <fs_mapping>
                  WITH KEY field_ent = field-name.
              IF sy-subrc IS INITIAL.
                lw_field_name = <fs_mapping>-field_db.
              ELSE.
                lw_field_name = field-name.
              ENDIF.
              ls_dtl-(lw_field_name) = ls_dtl_ud-(field-name).
            ENDIF.
          ENDLOOP.

          CALL METHOD zcl_dc_giacong=>update_dtl
            CHANGING
              c_dtl = ls_dtl.
          FREE update_struct.
          APPEND ls_dtl TO lt_dtl.
        ENDIF.
      ENDLOOP.

      MODIFY ztb_dcgc_dtl FROM TABLE @lt_dtl.
      READ TABLE lt_dtl INDEX 1 INTO ls_dtl.
      IF ls_dtl-hdr_id IS NOT INITIAL.
        SELECT SINGLE * FROM ztb_dcgc_hdr
            WHERE hdr_id = @ls_dtl-hdr_id
         INTO @ls_hdr.
        IF sy-subrc IS INITIAL.
          CALL METHOD zcl_dc_giacong=>update_hdr
            EXPORTING
              i_t_dtl = lt_dtl
            CHANGING
              c_hdr   = ls_hdr.

          MODIFY ztb_dcgc_hdr FROM @ls_hdr.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_zr_tbdcgc_hdr DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS:
      get_global_authorizations FOR GLOBAL AUTHORIZATION
        IMPORTING
        REQUEST requested_authorizations FOR ZrTbdcgcHdr
        RESULT result,
      get_instance_features FOR INSTANCE FEATURES
        IMPORTING keys REQUEST requested_features FOR ZrTbdcgcHdr RESULT result,
      UpdateData FOR MODIFY
        IMPORTING keys FOR ACTION ZrTbdcgcHdr~UpdateData RESULT result,
      CreateByPer FOR MODIFY
        IMPORTING keys FOR ACTION ZrTbdcgcHdr~CreateByPer RESULT result,
      PheDuyet FOR MODIFY
        IMPORTING keys FOR ACTION ZrTbdcgcHdr~PheDuyet RESULT result,
      HuyPheDuyet FOR MODIFY
        IMPORTING keys FOR ACTION ZrTbdcgcHdr~HuyPheDuyet RESULT result,
      FHuyPheDuyet FOR READ
        IMPORTING keys FOR FUNCTION ZrTbdcgcHdr~FHuyPheDuyet RESULT result.

    METHODS FPheDuyet FOR READ
      IMPORTING keys FOR FUNCTION ZrTbdcgcHdr~FPheDuyet RESULT result.

ENDCLASS.

CLASS lhc_zr_tbdcgc_hdr IMPLEMENTATION.
  METHOD get_global_authorizations.
  ENDMETHOD.


  METHOD get_instance_features.
    READ ENTITIES OF zr_tbdcgc_hdr IN LOCAL MODE
          ENTITY ZrTbdcgcHdr
             FIELDS (  Trangthai )
             WITH CORRESPONDING #( keys )
           RESULT DATA(lt_read_data)
           FAILED failed.

    result = VALUE #( FOR ls_data_for IN lt_read_data
                   ( %tky                           = ls_data_for-%tky

*                     %features-%action-PheDuyet = COND #( WHEN ls_data_for-Trangthai > '0'
*                                                              THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  )
*                     %features-%action-HuyPheDuyet = COND #( WHEN ls_data_for-Trangthai <> '1'
*                                                              THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  )
                     %features-%action-UpdateData = COND #( WHEN ls_data_for-Trangthai > '0'
                                                              THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  )
                     %features-%update = COND #( WHEN ls_data_for-Trangthai > '0'
                                                              THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  )
                     %features-%delete = COND #( WHEN ls_data_for-Trangthai > '0'
                                                              THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  )

                  ) ).
  ENDMETHOD.

  METHOD UpdateData.
    DATA : lt_dtl TYPE STANDARD TABLE OF ztb_dcgc_dtl,
           ls_hdr TYPE                   ztb_dcgc_hdr.
    DATA: lw_dat      TYPE zde_date,
          lw_datetime TYPE abp_locinst_lastchange_tstmpl.

    lw_dat = xco_cp=>sy->moment( xco_cp_time=>time_zone->user )->date->as( xco_cp_time=>format->abap )->value.

    CONVERT DATE cl_abap_context_info=>get_system_date( ) TIME cl_abap_context_info=>get_system_time( )
        INTO TIME STAMP lw_datetime TIME ZONE 'UTC-7'.
    READ ENTITIES OF zr_tbdcgc_hdr IN LOCAL MODE
     ENTITY ZrTbdcgcHdr
     ALL FIELDS
     WITH CORRESPONDING #( keys )
     RESULT DATA(ZrTbdcgcHdr).

    LOOP AT ZrTbdcgcHdr INTO DATA(ls_ZrTbdcgcHdr).
      ls_hdr = CORRESPONDING #( ls_ZrTbdcgcHdr MAPPING FROM ENTITY ).
      MODIFY ENTITIES OF zr_tbdcgc_hdr IN LOCAL MODE
        ENTITY ZrTbdcgcHdr
      UPDATE FIELDS ( sumdate  SumDateTime )
      WITH VALUE #(
        ( %tky = ls_ZrTbdcgcHdr-%tky
          sumdate =  lw_dat
          SumDateTime = lw_datetime ) ).
    ENDLOOP.

    "return result entities
    result = VALUE #( FOR ls_ZrTbdcgcHdr_for IN ZrTbdcgcHdr ( %tky   = ls_ZrTbdcgcHdr_for-%tky
                                                  %param = ls_ZrTbdcgcHdr_for ) ).
  ENDMETHOD.

  METHOD CreateByPer.
    DATA n TYPE i.
    DATA: lt_cr TYPE TABLE FOR CREATE zr_tbdcgc_hdr,
          ls_cr TYPE STRUCTURE FOR CREATE zr_tbdcgc_hdr.
    DATA: lw_lan TYPE zde_lan.
    READ TABLE keys INTO DATA(ls_key) INDEX 1.
    lw_lan = ls_key-%param-lan.
    IF lw_lan = ''.
      lw_lan = '0'.
    ENDIF.

    SELECT * FROM ztb_dcgc_hdr
      WHERE zper = @ls_key-%param-zper
      AND lan = @lw_lan
      AND bukrs = @ls_key-%param-bukrs
      INTO TABLE @DATA(lt_dcgc_hdr).
    SELECT SINGLE * FROM ztb_xetduyet_hdr
        WHERE zper = @ls_key-%param-zper
        AND bukrs = @ls_key-%param-bukrs
        INTO @DATA(ls_xetduyet_hdr).
    IF sy-subrc = 0.
      SELECT * FROM ztb_xetduyet_dtl
        WHERE hdr_id = @ls_xetduyet_hdr-hdr_id
        INTO TABLE @DATA(lt_xetduyet_dtl).
    ENDIF.
    LOOP AT lt_xetduyet_dtl INTO DATA(ls_xetduyet).
      READ TABLE lt_dcgc_hdr INTO DATA(ls_dcgc_hdr) WITH KEY supplier = ls_xetduyet-supplier.
      IF sy-subrc <> 0.
        n += 1.
        ls_cr = VALUE #(  %cid                   = |My%CID_{ n }|
                                        Bukrs  = ls_key-%param-bukrs
                                        Zper = ls_key-%param-zper
                                        Lan = lw_lan
                                        Supplier =  ls_xetduyet-supplier ) .
        APPEND ls_cr TO lt_cr.
      ENDIF.
    ENDLOOP.

    IF lt_cr[] IS NOT INITIAL.
      MODIFY ENTITIES OF zr_tbdcgc_hdr IN LOCAL MODE
        ENTITY ZrTbdcgcHdr
          CREATE FIELDS ( Bukrs Zper Lan Supplier )
          WITH lt_cr.
    ENDIF.

  ENDMETHOD.

  METHOD PheDuyet.
    DATA: lw_dat      TYPE zde_date,
          lw_datetime TYPE abp_locinst_lastchange_tstmpl.

    lw_dat = xco_cp=>sy->moment( xco_cp_time=>time_zone->user )->date->as( xco_cp_time=>format->abap )->value.

    CONVERT DATE cl_abap_context_info=>get_system_date( ) TIME cl_abap_context_info=>get_system_time( )
        INTO TIME STAMP lw_datetime TIME ZONE 'UTC-7'.

    LOOP AT keys INTO DATA(ls_key).

      SELECT SINGLE * FROM zr_tbdcgc_hdr
          WHERE HdrID = @ls_key-%param-hdrid AND trangthai = '0'
        INTO @DATA(ls_ZrHdr) .

      IF sy-subrc IS INITIAL.
        MODIFY ENTITIES OF zr_tbdcgc_hdr IN LOCAL MODE
          ENTITY ZrTbdcgcHdr
            UPDATE FIELDS ( trangthai sumdate SumDateTime )
            WITH VALUE #(
              ( %tky-HdrID = ls_ZrHdr-HdrID
                trangthai =  '1'
                Sumdate = lw_dat
                Sumdatetime = lw_datetime ) ).
      ENDIF.
    ENDLOOP.

*    "return result entities
*    result = VALUE #( FOR ls_for IN ZrHdr ( %tky   = ls_for-%tky
*                                                  %param = ls_for ) ).
  ENDMETHOD.

  METHOD HuyPheDuyet.
    LOOP AT keys INTO DATA(ls_key).

      SELECT SINGLE * FROM zr_tbdcgc_hdr
          WHERE HdrID = @ls_key-%param-hdrid AND trangthai = '1'
        INTO @DATA(ls_ZrHdr) .

      IF sy-subrc IS INITIAL.

        MODIFY ENTITIES OF zr_tbdcgc_hdr IN LOCAL MODE
          ENTITY ZrTbdcgcHdr
            UPDATE FIELDS ( trangthai )
            WITH VALUE #(
              ( %tky-HdrID = ls_ZrHdr-HdrID
                trangthai =  '0' ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD FHuyPheDuyet.
    READ ENTITIES OF zr_tbdcgc_hdr IN LOCAL MODE
           ENTITY ZrTbdcgcHdr
           ALL FIELDS
           WITH CORRESPONDING #( keys )
           RESULT DATA(ZrHdr).

    LOOP AT ZrHdr INTO DATA(ls_ZrHdr) WHERE trangthai = '1'.

      MODIFY ENTITIES OF zr_tbdcgc_hdr IN LOCAL MODE
        ENTITY ZrTbdcgcHdr
          UPDATE FIELDS ( trangthai )
          WITH VALUE #(
            ( %tky = ls_ZrHdr-%tky
              trangthai =  '0' ) ).
    ENDLOOP.

    "return result entities
    result = VALUE #( FOR ls_for IN ZrHdr ( %tky   = ls_for-%tky
                                                  %param = ls_for ) ).
  ENDMETHOD.

  METHOD FPheDuyet.
    READ ENTITIES OF zr_tbdcgc_hdr IN LOCAL MODE
     ENTITY ZrTbdcgcHdr
     ALL FIELDS
     WITH CORRESPONDING #( keys )
     RESULT DATA(ZrHdr).

    LOOP AT ZrHdr INTO DATA(ls_ZrHdr) WHERE trangthai = '0'.

      MODIFY ENTITIES OF zr_tbdcgc_hdr IN LOCAL MODE
        ENTITY ZrTbdcgcHdr
          UPDATE FIELDS ( trangthai )
          WITH VALUE #(
            ( %tky = ls_ZrHdr-%tky
              trangthai =  '1' ) ).
    ENDLOOP.

    "return result entities
    result = VALUE #( FOR ls_for IN ZrHdr ( %tky   = ls_for-%tky
                                                  %param = ls_for ) ).
  ENDMETHOD.

ENDCLASS.
