CLASS lsc_zr_tbxuat_hd DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

ENDCLASS.

CLASS lsc_zr_tbxuat_hd IMPLEMENTATION.

  METHOD save_modified.
    DATA : lt_hdr TYPE STANDARD TABLE OF ztb_xuat_hd,
           ls_hdr TYPE                   ztb_xuat_hd.

    DATA : lt_xn TYPE STANDARD TABLE OF ztb_xn_xuat_hd,
           ls_xn TYPE                   ztb_xn_xuat_hd.

    DATA : lt_ht TYPE STANDARD TABLE OF ztb_ht_hd,
           ls_ht TYPE                   ztb_ht_hd.

    DATA : lt_pb TYPE STANDARD TABLE OF ztb_pb_hd,
           ls_pb TYPE                   ztb_pb_hd.

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
    APPEND VALUE #( field_ent = 'CreatedBy'     field_db = 'created_by' )    TO gt_mapping.
    APPEND VALUE #( field_ent = 'CreatedAt'     field_db = 'created_at' )    TO gt_mapping.
    APPEND VALUE #( field_ent = 'LastChangedBy' field_db = 'last_changed_by' ) TO gt_mapping.
    APPEND VALUE #( field_ent = 'LastChangedAt' field_db = 'last_changed_at' ) TO gt_mapping.

    LOOP AT gt_mapping ASSIGNING FIELD-SYMBOL(<fs_mapping>).
      TRANSLATE <fs_mapping>-field_ent TO UPPER CASE.
      TRANSLATE <fs_mapping>-field_db TO UPPER CASE.
    ENDLOOP.

    IF create-zrtbxuathd IS NOT INITIAL.
      lt_hdr = CORRESPONDING #( create-zrtbxuathd MAPPING FROM ENTITY ).
      LOOP AT lt_hdr INTO ls_hdr.
        SELECT MAX( mahdnum ) FROM ztb_xuat_hd
        WHERE zper = @ls_hdr-zper
        INTO @DATA(lv_max_mahdnum).
        lv_max_mahdnum = lv_max_mahdnum + 1.
        ls_hdr-mahdnum = lv_max_mahdnum.
        ls_hdr-mahd = ls_hdr-zper && lv_max_mahdnum.

        SELECT SINGLE * FROM zr_tbdcgc_hdr
        WHERE Zper = @ls_hdr-zper AND Supplier = @ls_hdr-supplier AND lan = @ls_hdr-lan
        INTO @DATA(ls_tbdcgc_hdr).
        IF sy-subrc IS INITIAL.
          SELECT SINGLE * FROM zr_tbdcgc_dtl
              WHERE HdrID = @ls_tbdcgc_hdr-HdrID
              INTO @DATA(ls_tbdcgc_dtl).
          ls_hdr-tongtienxn = ls_tbdcgc_dtl-ct28.
        ENDIF.

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
        CALL METHOD zcl_xuat_hd=>get_data
          EXPORTING
            i_hdr  = ls_hdr
          IMPORTING
            e_t_xn = lt_xn
            e_t_ht = lt_ht
            e_t_pb = lt_pb
            e_hdr  = ls_hdr.

        INSERT ztb_xuat_hd FROM @ls_hdr.
        INSERT ztb_xn_xuat_hd FROM TABLE @lt_xn.
        INSERT ztb_ht_hd FROM TABLE @lt_ht.
        INSERT ztb_pb_hd FROM TABLE @lt_pb.
      ENDLOOP.
    ENDIF.

    LOOP AT delete-zrtbxuathd INTO DATA(ls_detele)." WHERE HdrID IS NOT INITIAL.
      DELETE FROM ztb_xuat_hd WHERE hdr_id = @ls_detele-HdrID.
    ENDLOOP.

    DATA update_struct TYPE REF TO cl_abap_structdescr.
    IF update-zrtbxuathd IS NOT INITIAL.
      LOOP AT update-zrtbxuathd INTO DATA(ls_zrtbxuathd).
        IF ls_zrtbxuathd-%control-Sumdate = if_abap_behv=>mk-on.
          IF ls_zrtbxuathd-Trangthai > '0'.
            CONTINUE.
          ENDIF.
          SELECT SINGLE *
              FROM ztb_xuat_hd WHERE hdr_id = @ls_zrtbxuathd-HdrID INTO @ls_hdr.
          IF sy-subrc IS INITIAL.
            ls_hdr-sumdate = ls_zrtbxuathd-Sumdate.
            ls_hdr-sumdatetime = ls_zrtbxuathd-SumDateTime.
            CALL METHOD zcl_xuat_hd=>get_data
              EXPORTING
                i_hdr  = ls_hdr
              IMPORTING
                e_t_xn = lt_xn
                e_t_ht = lt_ht
                e_t_pb = lt_pb
                e_hdr  = ls_hdr.
            DELETE FROM ztb_xn_xuat_hd WHERE hdr_id = @ls_hdr-hdr_id.
            MODIFY ztb_xuat_hd FROM @ls_hdr.
            MODIFY ztb_xn_xuat_hd FROM TABLE @lt_xn.
          ENDIF.
        ELSE.

          SELECT SINGLE * FROM ztb_xuat_hd
           WHERE hdr_id = @ls_zrtbxuathd-HdrID
           INTO @ls_hdr.
          IF sy-subrc IS INITIAL.
            SELECT * FROM ztb_xn_xuat_hd
              WHERE hdr_id = @ls_zrtbxuathd-HdrID
              INTO TABLE @lt_xn.
            DATA(lw_trangthai) = ls_hdr-trangthai.
            update_struct ?= cl_abap_structdescr=>describe_by_data( ls_zrtbxuathd-%control ).
            LOOP AT update_struct->components INTO DATA(field).
              IF ls_zrtbxuathd-%control-(field-name) = if_abap_behv=>mk-on.
                READ TABLE gt_mapping ASSIGNING <fs_mapping>
                    WITH KEY field_ent = field-name.
                IF sy-subrc IS INITIAL.
                  lw_field_name = <fs_mapping>-field_db.
                ELSE.
                  lw_field_name = field-name.
                ENDIF.
                ls_hdr-(lw_field_name) = ls_zrtbxuathd-(field-name).

              ENDIF.
            ENDLOOP.

*            if ls_zrtbxuathd-%control-supplierinvoice = if_abap_behv=>mk-on and ls_zrtbxuathd-supplierinvoice is NOT INITIAL.
*              ls_hdr-Trangthai = '2'.
*            endif.

            FREE update_struct.
            IF lw_trangthai = '0' AND ls_hdr-trangthai = '0'.
              CALL METHOD zcl_xuat_hd=>update_data
                CHANGING
                  ct_xn = lt_xn
                  ct_ht = lt_ht
                  ct_pb = lt_pb
                  c_hdr = ls_hdr.
              DELETE FROM ztb_xn_xuat_hd WHERE hdr_id = @ls_hdr-hdr_id.
              MODIFY ztb_xuat_hd FROM @ls_hdr.
              MODIFY ztb_xn_xuat_hd FROM TABLE @lt_xn.
            ENDIF.

            IF ls_hdr-trangthai = '1'.
              CALL METHOD zcl_xuat_hd=>update_data_ht
                CHANGING
                  ct_xn = lt_xn
                  ct_ht = lt_ht
                  ct_pb = lt_pb
                  c_hdr = ls_hdr.
              DELETE FROM ztb_ht_hd WHERE hdr_id = @ls_hdr-hdr_id.
              DELETE FROM ztb_pb_hd WHERE hdr_id = @ls_hdr-hdr_id.
              MODIFY ztb_xuat_hd FROM @ls_hdr.
              MODIFY ztb_ht_hd FROM TABLE @lt_ht.
              MODIFY ztb_pb_hd FROM TABLE @lt_pb.
            ENDIF.

            IF lw_trangthai = '0' AND ls_hdr-trangthai = '1'. "Xac nhan
              UPDATE ztb_dcgc_hdr SET trangthai = '2'
                WHERE zper = @ls_hdr-zper AND bukrs = @ls_hdr-bukrs
                  AND lan = @ls_hdr-lan AND supplier = @ls_hdr-supplier.
              SELECT SINGLE * FROM ztb_xetduyet_hdr
                WHERE zper = @ls_hdr-zper AND bukrs = @ls_hdr-bukrs
                  AND lan = @ls_hdr-lan AND trangthai = '0'
                INTO @DATA(ls_xetduyet_hdr).
              IF sy-subrc IS INITIAL.
                DATA: lw_check TYPE zde_check.
                CLEAR lw_check.
                SELECT * FROM ztb_xetduyet_dtl
                  WHERE hdr_id = @ls_xetduyet_hdr-hdr_id AND supplier <> @ls_hdr-supplier
                  INTO TABLE @DATA(lt_xetduyet_dtl).
                LOOP AT lt_xetduyet_dtl INTO DATA(ls_xetduyet_dtl).
                  SELECT SINGLE * FROM ztb_xuat_hd
                   WHERE zper = @ls_hdr-zper AND bukrs = @ls_hdr-bukrs
                     AND lan = @ls_hdr-lan AND supplier = @ls_xetduyet_dtl-supplier
                     AND trangthai >= '1'
                     INTO @DATA(ls_xuat_hd_check).
                  IF sy-subrc <> 0.
                    lw_check = 'X'.
                    EXIT.
                  ENDIF.
                ENDLOOP.
                IF lw_check IS INITIAL.
                  UPDATE ztb_xetduyet_hdr SET trangthai = '2'
                  WHERE hdr_id = @ls_xetduyet_hdr-hdr_id.
                ENDIF.
              ENDIF.

              SELECT SINGLE * FROM ztb_period
                WHERE zper = @ls_hdr-zper
                  INTO @DATA(ls_period).

              SELECT * FROM zr_tbbb_gc
                  WHERE NgayLapBb >= @ls_period-zdatefr AND NgayLapBb <= @ls_period-zdateto
                    AND CompanyCode = @ls_hdr-bukrs
                    AND supplier = @ls_hdr-supplier
                    INTO TABLE @DATA(lt_tbbb_gc).
              LOOP AT lt_tbbb_gc INTO DATA(ls_tbbb_gc).
                UPDATE ztb_bb_gc SET trangthai = '1'
                  WHERE hdr_id = @ls_tbbb_gc-HdrID.
              ENDLOOP.

            ENDIF.

            IF lw_trangthai >= '1' AND ls_hdr-trangthai >= '0'. "Huy xac nhan
              UPDATE ztb_dcgc_hdr SET trangthai = '1'
                WHERE zper = @ls_hdr-zper AND bukrs = @ls_hdr-bukrs
                  AND lan = @ls_hdr-lan AND supplier = @ls_hdr-supplier.
              UPDATE ztb_xetduyet_hdr SET trangthai = '1'
                 WHERE zper = @ls_hdr-zper AND bukrs = @ls_hdr-bukrs
                   AND lan = @ls_hdr-lan AND trangthai = '0'.

              SELECT * FROM zr_tbbb_gc
                    WHERE NgayLapBb >= @ls_period-zdatefr AND NgayLapBb <= @ls_period-zdateto
                      AND CompanyCode = @ls_hdr-bukrs
                      AND supplier = @ls_hdr-supplier
                      INTO TABLE @DATA(lt_tbbb_gc1).
              LOOP AT lt_tbbb_gc INTO DATA(ls_tbbb_gc1).
                SELECT SINGLE * FROM ztb_xuat_hd
                    WHERE zper = @ls_hdr-zper AND bukrs = @ls_hdr-bukrs
                    AND supplier = @ls_hdr-supplier
                      AND hdr_id <> @ls_hdr-hdr_id
                    INTO @DATA(ls_xuat_hd).
                IF sy-subrc <> 0.
                  UPDATE ztb_bb_gc SET trangthai = '0'
                    WHERE hdr_id = @ls_tbbb_gc1-HdrID.
                ENDIF.
              ENDLOOP.

            ENDIF.

*            IF ls_hdr-trangthai = '2' OR ls_hdr-trangthai = '3' .
            MODIFY ztb_xuat_hd FROM @ls_hdr.
*            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lhc_zr_tbxuat_hd DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS:
      get_global_authorizations FOR GLOBAL AUTHORIZATION
        IMPORTING
        REQUEST requested_authorizations FOR ZrTbxuatHd
        RESULT result,
      get_instance_features FOR INSTANCE FEATURES
        IMPORTING keys REQUEST requested_features FOR ZrTbxuatHd RESULT result,
      UpdateData FOR MODIFY
        IMPORTING keys FOR ACTION ZrTbxuatHd~UpdateData RESULT result,
      HachToan_HD FOR MODIFY
        IMPORTING keys FOR ACTION ZrTbxuatHd~HachToan_HD RESULT result,
      XacNhan FOR MODIFY
        IMPORTING keys FOR ACTION ZrTbxuatHd~XacNhan RESULT result,
      CreateByPer FOR MODIFY
        IMPORTING keys FOR ACTION ZrTbxuatHd~CreateByPer RESULT result,
      Huy_HD FOR MODIFY
        IMPORTING keys FOR ACTION ZrTbxuatHd~Huy_HD RESULT result,
      Cancel_Inv FOR DETERMINE ON SAVE
        IMPORTING keys FOR ZrTbxuatHd~Cancel_Inv,
      btnPrintPDF FOR MODIFY
        IMPORTING keys FOR ACTION ZrTbxuatHd~btnPrintPDF RESULT result,
      check_data FOR VALIDATE ON SAVE
        IMPORTING keys FOR ZrTbxuatHd~check_data,
      HuyXacNhan FOR MODIFY
        IMPORTING keys FOR ACTION ZrTbxuatHd~HuyXacNhan RESULT result.
ENDCLASS.

CLASS lhc_zr_tbxuat_hd IMPLEMENTATION.
  METHOD get_global_authorizations.
  ENDMETHOD.
  METHOD get_instance_features.
    READ ENTITIES OF zr_tbxuat_hd IN LOCAL MODE
      ENTITY ZrTbxuatHd
         FIELDS (  Trangthai )
         WITH CORRESPONDING #( keys )
       RESULT DATA(lt_read_data)
       FAILED failed.


    result = VALUE #( FOR ls_data_for IN lt_read_data
                       ( %tky                           = ls_data_for-%tky
                         %features-%field-Tongtienxn = COND #( WHEN ls_data_for-Trangthai > '0'
                                                               THEN if_abap_behv=>fc-f-read_only ELSE if_abap_behv=>fc-f-unrestricted  )
                         %features-%field-Thuesuat = COND #( WHEN ls_data_for-Trangthai > '0'
                                                               THEN if_abap_behv=>fc-f-read_only ELSE if_abap_behv=>fc-f-unrestricted  )
                         %features-%field-Tongtienht = COND #( WHEN ls_data_for-Trangthai = '2'
                                                               THEN if_abap_behv=>fc-f-read_only ELSE if_abap_behv=>fc-f-unrestricted  )
                         %features-%action-UpdateData = COND #( WHEN ls_data_for-Trangthai > '0'
                                                                  THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  )
                         %features-%action-XacNhan = COND #( WHEN ls_data_for-Trangthai > '0'
                                                                  THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  )
                         %features-%action-HuyXacNhan = COND #( WHEN ls_data_for-Trangthai = '0' OR ls_data_for-Trangthai = '2'
                                                                  THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  )
                         %features-%action-HachToan_HD = COND #( WHEN ls_data_for-Trangthai <> '1' AND ls_data_for-Trangthai <> '3'
                                                                  THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  )
                         %features-%action-Huy_HD = COND #( WHEN ls_data_for-Trangthai <> '2'
                                                                  THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  )

                      ) ).
  ENDMETHOD.

  METHOD UpdateData.
    DATA : ls_hdr TYPE                   ztb_xuat_hd.
    DATA: lw_dat      TYPE zde_date,
          lw_datetime TYPE abp_locinst_lastchange_tstmpl.

    lw_dat = xco_cp=>sy->moment( xco_cp_time=>time_zone->user )->date->as( xco_cp_time=>format->abap )->value.

    CONVERT DATE cl_abap_context_info=>get_system_date( ) TIME cl_abap_context_info=>get_system_time( )
        INTO TIME STAMP lw_datetime TIME ZONE 'UTC-7'.
    READ ENTITIES OF zr_tbxuat_hd IN LOCAL MODE
     ENTITY ZrTbxuatHd
     ALL FIELDS
     WITH CORRESPONDING #( keys )
     RESULT DATA(ZrTbxuatHd).

    LOOP AT ZrTbxuatHd INTO DATA(ls_ZrTbxuatHd).
      ls_hdr = CORRESPONDING #( ls_ZrTbxuatHd MAPPING FROM ENTITY ).
      MODIFY ENTITIES OF zr_tbxuat_hd IN LOCAL MODE
        ENTITY ZrTbxuatHd
      UPDATE FIELDS ( sumdate  SumDateTime )
      WITH VALUE #(
        ( %tky = ls_ZrTbxuatHd-%tky
          sumdate =  lw_dat
          SumDateTime = lw_datetime ) ).
    ENDLOOP.

    "return result entities
    result = VALUE #( FOR ls_for IN ZrTbxuatHd ( %tky   = ls_for-%tky
                                                  %param = ls_for ) ).
  ENDMETHOD.

  METHOD HachToan_HD.
    DATA : ls_hdr TYPE                   ztb_xuat_hd.

    READ ENTITIES OF zr_tbxuat_hd IN LOCAL MODE
     ENTITY ZrTbxuatHd
     ALL FIELDS
     WITH CORRESPONDING #( keys )
     RESULT DATA(ZrTbxuatHd).

    LOOP AT ZrTbxuatHd INTO DATA(ls_ZrTbxuatHd).
      ls_hdr = CORRESPONDING #( ls_ZrTbxuatHd MAPPING FROM ENTITY ).
      SELECT * FROM ztb_pb_hd
        WHERE hdr_id = @ls_hdr-hdr_id
        INTO TABLE @DATA(lt_pb).

      CALL METHOD zcl_xuat_hd=>post_invoice_api
        EXPORTING
          i_hdr      = ls_hdr
          i_t_pb     = lt_pb
        IMPORTING
          e_code     = DATA(lv_code)
          e_response = DATA(lv_response)
          e_suppliervoice = DATA(lv_suppliervoice).
      .

      IF lv_code < '300'.
        DATA(lw_trangthai) = '2'.
      ELSE.
        lw_trangthai = ls_hdr-trangthai.
      ENDIF.
      MODIFY ENTITIES OF zr_tbxuat_hd IN LOCAL MODE
      ENTITY ZrTbxuatHd
        UPDATE FIELDS (  supplierinvoice  code message  Trangthai invoicingparty )
        WITH VALUE #(
          ( %tky = ls_ZrTbxuatHd-%tky
            supplierinvoice =  lv_response-d-supplierinvoice
            code = lv_code
            invoicingparty = lv_suppliervoice
            message = lv_response-error-message-value
            Trangthai = lw_trangthai
             ) ).


    ENDLOOP.

    "return result entities
    result = VALUE #( FOR ls_for IN ZrTbxuatHd ( %tky   = ls_for-%tky
                                                  %param = ls_for ) ).
  ENDMETHOD.

  METHOD XacNhan.
    DATA : ls_hdr TYPE                   ztb_xuat_hd.

    READ ENTITIES OF zr_tbxuat_hd IN LOCAL MODE
     ENTITY ZrTbxuatHd
     ALL FIELDS
     WITH CORRESPONDING #( keys )
     RESULT DATA(ZrTbxuatHd).

    LOOP AT ZrTbxuatHd INTO DATA(ls_ZrTbxuatHd) WHERE Trangthai = '0'.
      ls_hdr = CORRESPONDING #( ls_ZrTbxuatHd MAPPING FROM ENTITY ).
      MODIFY ENTITIES OF zr_tbxuat_hd IN LOCAL MODE
        ENTITY ZrTbxuatHd
      UPDATE FIELDS ( Trangthai Tongtienht )
      WITH VALUE #(
        ( %tky = ls_ZrTbxuatHd-%tky
          Trangthai =  '1'
          Tongtienht = ls_hdr-tongtienxn ) ).

*      SELECT SINGLE * FROM zr_tbxetduyet_hdr
*        WHERE zper = @ls_hdr-zper
*        AND bukrs = @ls_hdr-bukrs
*         AND lan = @ls_hdr-lan
*        AND trangthai = '1'
*        INTO @DATA(ls_zr_tbxetduyet_hdr).
*      IF sy-subrc IS INITIAL.
*        MODIFY ENTITIES OF zr_tbxetduyet_hdr
*        ENTITY ZrTbxetduyetHdr
*          UPDATE FIELDS ( trangthai )
*          WITH VALUE #(
*            ( %tky-HdrID = ls_zr_tbxetduyet_hdr-HdrID
*              trangthai =  '2' ) ) .
*      ENDIF.
*
*      SELECT SINGLE * FROM zr_tbdcgc_hdr
*        WHERE zper = @ls_hdr-zper
*        AND bukrs = @ls_hdr-bukrs
*         AND lan = @ls_hdr-lan
*         AND supplier = @ls_hdr-supplier
*        AND trangthai = '1'
*        INTO @DATA(ls_zr_tbdcgc_hdr).
*      IF sy-subrc IS INITIAL.
*        MODIFY ENTITIES OF zr_tbdcgc_hdr
*        ENTITY ZrTbdcgcHdr
*          UPDATE FIELDS ( trangthai )
*          WITH VALUE #(
*            ( %key-HdrID = ls_zr_tbdcgc_hdr-HdrID
*              trangthai =  '2' ) ) .
*      ENDIF.

      APPEND INITIAL LINE TO result ASSIGNING FIELD-SYMBOL(<fs_result>).
      <fs_result>-%tky = ls_ZrTbxuatHd-%tky.
      <fs_result>-%param = ls_ZrTbxuatHd.

    ENDLOOP.

  ENDMETHOD.

  METHOD CreateByPer.
    DATA n TYPE i.
    DATA: lt_cr TYPE TABLE FOR CREATE zr_tbxuat_hd,
          ls_cr TYPE STRUCTURE FOR CREATE zr_tbxuat_hd.

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
      AND trangthai >= '1'
      INTO TABLE @DATA(lt_dcgc_hdr).
    SELECT * FROM ztb_xuat_hd
        WHERE zper = @ls_key-%param-zper
      AND lan = @lw_lan
      AND bukrs = @ls_key-%param-bukrs
        INTO TABLE @DATA(lt_xuat_hd).

    LOOP AT lt_dcgc_hdr INTO DATA(ls_dcgc).
      READ TABLE lt_xuat_hd INTO DATA(ls_xuat_hd) WITH KEY supplier = ls_dcgc-supplier.
      IF sy-subrc <> 0.
        n += 1.
        ls_cr = VALUE #(  %cid                   = |My%CID_{ n }|
                                        Bukrs  = ls_key-%param-bukrs
                                        Zper = ls_key-%param-zper
                                        Lan = lw_lan
                                        Supplier =  ls_dcgc-supplier
                                        Thuesuat = ls_key-%param-taxcode ) .
        APPEND ls_cr TO lt_cr.
      ENDIF.
    ENDLOOP.

    IF lt_cr[] IS NOT INITIAL.
      MODIFY ENTITIES OF zr_tbxuat_hd IN LOCAL MODE
        ENTITY ZrTbxuatHd
          CREATE FIELDS ( Bukrs Zper Lan Supplier Thuesuat )
          WITH lt_cr.
    ENDIF.
  ENDMETHOD.

  METHOD Huy_HD.
    DATA : ls_hdr TYPE                   ztb_xuat_hd.
    DATA: lw_supplierinvoice TYPE zr_tbxuat_hd-supplierinvoice.
    READ ENTITIES OF zr_tbxuat_hd IN LOCAL MODE
     ENTITY ZrTbxuatHd
     ALL FIELDS
     WITH CORRESPONDING #( keys )
     RESULT DATA(ZrTbxuatHd).

    LOOP AT ZrTbxuatHd INTO DATA(ls_ZrTbxuatHd).
      ls_hdr = CORRESPONDING #( ls_ZrTbxuatHd MAPPING FROM ENTITY ).
      IF ls_hdr-trangthai <> '2'.
        CONTINUE.
      ENDIF.

*      SELECT * FROM ztb_pb_hd
*        WHERE hdr_id = @ls_hdr-hdr_id
*        INTO TABLE @DATA(lt_pb).

*      CALL METHOD zcl_xuat_hd=>cancel_invoice
*        EXPORTING
*          i_hdr      = ls_hdr
*        IMPORTING
*          e_code     = DATA(lv_code)
*          e_response = DATA(lv_response).
      .

*      IF lv_code < '300'.
*        DATA(lw_trangthai) = '1'.
*        lw_supplierinvoice = ''.
*      ELSE.
*        lw_trangthai = ls_hdr-trangthai.
*        lw_supplierinvoice = ls_hdr-supplierinvoice.
*      ENDIF.
      MODIFY ENTITIES OF zr_tbxuat_hd IN LOCAL MODE
      ENTITY ZrTbxuatHd
        UPDATE FIELDS ( Trangthai )
        WITH VALUE #(
          ( %tky = ls_ZrTbxuatHd-%tky
            supplierinvoice = ''"  lw_supplierinvoice
*            code = lv_code
*            message = lv_response-error-message-value
            Trangthai = '3'
             ) ).


    ENDLOOP.

    "return result entities
    result = VALUE #( FOR ls_for IN ZrTbxuatHd ( %tky   = ls_for-%tky
                                                  %param = ls_for ) ).
  ENDMETHOD.

  METHOD Cancel_Inv.
    DATA : ls_hdr TYPE                   ztb_xuat_hd.
    DATA: lw_supplierinvoice TYPE zr_tbxuat_hd-supplierinvoice.
    READ ENTITIES OF zr_tbxuat_hd IN LOCAL MODE
     ENTITY ZrTbxuatHd
     ALL FIELDS
     WITH CORRESPONDING #( keys )
     RESULT DATA(ZrTbxuatHd).

    LOOP AT ZrTbxuatHd INTO DATA(ls_ZrTbxuatHd) WHERE Trangthai = '3' AND supplierinvoice <> ''.
      ls_hdr = CORRESPONDING #( ls_ZrTbxuatHd MAPPING FROM ENTITY ).
      SELECT SINGLE supplierinvoice, reversedocument
        FROM zi_supplierinvoiceapi01
        WHERE supplierinvoice = @ls_hdr-supplierinvoice
        INTO @DATA(ls_invoice).
      IF sy-subrc = 0 AND ls_invoice-reversedocument IS INITIAL.
        CALL METHOD zcl_xuat_hd=>cancel_invoice
          EXPORTING
            i_hdr      = ls_hdr
          IMPORTING
            e_code     = DATA(lv_code)
            e_response = DATA(lv_response).
        .
      ENDIF.
*      IF lv_code < '300'.
*        DATA(lw_trangthai) = '1'.
*        lw_supplierinvoice = ''.
*      ELSE.
*        lw_trangthai = ls_hdr-trangthai.
*        lw_supplierinvoice = ls_hdr-supplierinvoice.
*      ENDIF.

*      MODIFY ENTITIES OF zr_tbxuat_hd IN LOCAL MODE
*      ENTITY ZrTbxuatHd
*        UPDATE FIELDS ( Trangthai )
*        WITH VALUE #(
*          ( %tky = ls_ZrTbxuatHd-%tky
*            supplierinvoice = ''"  lw_supplierinvoice
**            code = lv_code
**            message = lv_response-error-message-value
*            Trangthai = '3'
*             ) ).


    ENDLOOP.
  ENDMETHOD.

  METHOD btnPrintPDF.
    zcl_XUAT_HD_ex_pdf=>btnprintpdf_pkt(
   EXPORTING
     keys     = keys
*      IMPORTING
*        O_PDF    = LV_FILE_CONTENT
   CHANGING
     result   = result
     mapped   = mapped
     failed   = failed
     reported = reported
 ).
  ENDMETHOD.

  METHOD check_data.

    READ ENTITIES OF zr_tbxuat_hd IN LOCAL MODE
       ENTITY ZrTbxuatHd
       ALL FIELDS
       WITH CORRESPONDING #( keys )
       RESULT DATA(ZrTbxuatHd).

    LOOP AT ZrTbxuatHd INTO DATA(ls_ZrTbxuatHd).
      APPEND VALUE #(  %tky           = ls_ZrTbxuatHd-%tky
                    %state_area    = 'CHECKDATA'
                  ) TO reported-zrtbxuathd.
      SELECT SINGLE * FROM ztb_dcgc_hdr
          WHERE zper = @ls_ZrTbxuatHd-zper
            AND lan = @ls_ZrTbxuatHd-lan
            AND bukrs = @ls_ZrTbxuatHd-bukrs
            AND supplier = @ls_ZrTbxuatHd-supplier
            AND trangthai >= '1'
            INTO @DATA(ls_dcgc_hdr).
      IF sy-subrc <> 0.
        APPEND VALUE #( %tky          = ls_ZrTbxuatHd-%tky
                        %state_area   = 'CHECKDATA'
                        %msg          = new_message_with_text(
                                        severity = if_abap_behv_message=>severity-error
                                        text     = 'Chưa có BB đối chiếu gia công được duyệt' )
                        %element-supplier = if_abap_behv=>mk-on
                      )
                      TO reported-zrtbxuathd
                      .

        APPEND VALUE #( %tky = ls_ZrTbxuatHd-%tky ) TO failed-zrtbxuathd.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD HuyXacNhan.
    DATA : ls_hdr TYPE                   ztb_xuat_hd.

    READ ENTITIES OF zr_tbxuat_hd IN LOCAL MODE
     ENTITY ZrTbxuatHd
     ALL FIELDS
     WITH CORRESPONDING #( keys )
     RESULT DATA(ZrTbxuatHd).

    LOOP AT ZrTbxuatHd INTO DATA(ls_ZrTbxuatHd) WHERE Trangthai = '1' OR Trangthai = '3'.
      ls_hdr = CORRESPONDING #( ls_ZrTbxuatHd MAPPING FROM ENTITY ).
      MODIFY ENTITIES OF zr_tbxuat_hd IN LOCAL MODE
        ENTITY ZrTbxuatHd
      UPDATE FIELDS ( Trangthai )
      WITH VALUE #(
        ( %tky = ls_ZrTbxuatHd-%tky
          Trangthai =  '0' ) ).

      APPEND INITIAL LINE TO result ASSIGNING FIELD-SYMBOL(<fs_result>).
      <fs_result>-%tky = ls_ZrTbxuatHd-%tky.
      <fs_result>-%param = ls_ZrTbxuatHd.

    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
