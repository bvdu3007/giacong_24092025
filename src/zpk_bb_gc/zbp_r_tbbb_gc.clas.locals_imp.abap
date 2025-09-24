CLASS lhc_zrtbgcloi DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR ZrTbgcLoi RESULT result.

ENDCLASS.

CLASS lhc_zrtbgcloi IMPLEMENTATION.

  METHOD get_instance_features.
    READ ENTITIES OF zr_tbbb_gc IN LOCAL MODE
                ENTITY ZrTbbbGc
                   FIELDS (  Trangthai )
                   WITH CORRESPONDING #( keys )
                 RESULT DATA(lt_read_data)
                 FAILED failed.
    READ ENTITIES OF zr_tbbb_gc IN LOCAL MODE
            ENTITY ZrTbgcLoi
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

CLASS lhc_ZrTbbbGc DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR ZrTbbbGc RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR ZrTbbbGc RESULT result.

    METHODS btnPrintPDF FOR MODIFY
      IMPORTING keys FOR ACTION ZrTbbbGc~btnPrintPDF RESULT result.

    METHODS Copy FOR MODIFY
      IMPORTING keys FOR ACTION ZrTbbbGc~Copy RESULT result.

    METHODS checkInputFields FOR DETERMINE ON MODIFY
      IMPORTING keys FOR ZrTbbbGc~checkInputFields.

    METHODS checkInputFields_u FOR DETERMINE ON MODIFY
      IMPORTING keys FOR ZrTbbbGc~checkInputFields_u.

    METHODS SetData_GC FOR DETERMINE ON SAVE
      IMPORTING keys FOR ZrTbbbGc~SetData_GC.

    METHODS checkInputFields_Save FOR VALIDATE ON SAVE
      IMPORTING keys FOR ZrTbbbGc~checkInputFields_Save.

ENDCLASS.

CLASS lhc_ZrTbbbGc IMPLEMENTATION.

  METHOD get_instance_features.
    READ ENTITIES OF zr_tbbb_gc IN LOCAL MODE
            ENTITY ZrTbbbGc
               FIELDS (  Trangthai )
               WITH CORRESPONDING #( keys )
             RESULT DATA(lt_read_data)
             FAILED failed.



    result = VALUE #( FOR ls_data_for IN lt_read_data
                   ( %tky                           = ls_data_for-%tky

                     %features-%update = COND #( WHEN ls_data_for-Trangthai > '0'
                                                              THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  )
                     %features-%delete = COND #( WHEN ls_data_for-Trangthai > '0'
                                                              THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  )
                     %features-%field-Ct12 = COND #( WHEN ls_data_for-SoBbBase = '0'
                                                              THEN if_abap_behv=>fc-f-read_only ELSE if_abap_behv=>fc-f-all  )
                    %features-%field-Ct13 = COND #( WHEN ls_data_for-SoBbBase = '0'
                                                              THEN if_abap_behv=>fc-f-read_only ELSE if_abap_behv=>fc-f-all  )

                  ) ).

  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD btnPrintPDF.

    zcl_bbgc_ex_pdf=>btnprintpdf_pkt(
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

  METHOD Copy.
    READ ENTITIES OF zr_tbbb_gc IN LOCAL MODE
        ENTITY ZrTbbbGc
        ALL FIELDS
        WITH CORRESPONDING #( keys )
        RESULT DATA(zr_tbbb_gc).

    LOOP AT zr_tbbb_gc INTO DATA(ls_gc).
      TRY.
          DATA(lv_cid) = cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ).
        CATCH cx_uuid_error.
          "Error handling
      ENDTRY.
      ls_gc-SoBbBase = ls_gc-SoBb.
      MODIFY ENTITIES OF zr_tbbb_gc IN LOCAL MODE
      ENTITY ZrTbbbGc
      CREATE FIELDS ( SoBbBase SoPo  ct12 ct13 )
           WITH VALUE #( (  %cid      = lv_cid
                            SoBbBase = ls_gc-SoBbBase
                            SoPo     = ls_gc-SoPo
                            Ct12     = ls_gc-Ct12
                            Ct13     = ls_gc-Ct13
                           ) )
            REPORTED DATA(ls_cr_reported).
    ENDLOOP.

    result = VALUE #( FOR ls_for IN zr_tbbb_gc ( %tky   = ls_for-%tky
                                                 %param = ls_for ) ).
  ENDMETHOD.

  METHOD checkInputFields.
    READ ENTITIES OF zr_tbbb_gc IN LOCAL MODE
       ENTITY ZrTbbbGc
       FIELDS ( SoPo )
       WITH CORRESPONDING #( keys )
       RESULT DATA(zr_tbbb_gc).

    LOOP AT zr_tbbb_gc INTO DATA(ls_gc).
      APPEND VALUE #(  %tky           = ls_gc-%tky
                      %state_area    = 'SoPO'
                    ) TO reported-zrtbbbgc.
      SELECT SINGLE * FROM zv_po_gc
        WHERE PurchaseOrder = @ls_gc-SoPo
        INTO @DATA(ls_po_check).
      IF sy-subrc IS NOT INITIAL.
*        APPEND VALUE #(  %tky = ls_gc-%tky ) TO failed-ZrTbbbGc.
        APPEND VALUE #( %tky          = ls_gc-%tky
                        %state_area   = 'SoPO'
                        %msg          = new_message_with_text(
                                severity = if_abap_behv_message=>severity-error
                                text     = 'Số PO gia công không tồn tại' )
                        %element-SoPo = if_abap_behv=>mk-on
                      ) TO reported-zrtbbbgc.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD checkInputFields_u.
  ENDMETHOD.

  METHOD SetData_GC.
    DATA: lt_dtl TYPE TABLE FOR CREATE zr_tbbb_gc\_dtl,
          ls_dtl TYPE STRUCTURE FOR CREATE zr_tbbb_gc\_dtl.
    DATA: lw_so_bb_num TYPE  ztb_bb_gc-so_bb_num,
          lw_so_bb_sub TYPE  ztb_bb_gc-so_bb_sub,
          lw_so_bb     TYPE  ztb_bb_gc-so_bb.

    "Read travel instances of the transferred keys
    READ ENTITIES OF zr_tbbb_gc IN LOCAL MODE
     ENTITY ZrTbbbGc
       FIELDS ( SoBb )
       WITH CORRESPONDING #( keys )
     RESULT DATA(GiaCong_data)
     FAILED DATA(read_failed).

    READ TABLE keys INTO DATA(ls_key) INDEX 1.
    READ TABLE GiaCong_data INTO DATA(ls_gia_cong) INDEX 1.
    CHECK sy-subrc IS INITIAL.
    CHECK ls_gia_cong-SoBbBase IS INITIAL.

*    SELECT so_bb_num, MAX( so_bb_sub ) AS so_bb_sub FROM ztb_bb_gc
*    WHERE loai_hang = @ls_gia_cong-LoaiHang AND so_po = @ls_gia_cong-SoPo
*     GROUP BY so_bb_num
*     INTO TABLE @DATA(lt_so_bb_sub).
*    IF sy-subrc IS INITIAL.
*      READ TABLE lt_so_bb_sub INDEX 1 INTO DATA(ls_so_bb_sub).
*      lw_so_bb_sub = ls_so_bb_sub-so_bb_sub + 1.
*      lw_so_bb_num = ls_so_bb_sub-so_bb_num.
*    ELSE.
    SELECT MAX( so_bb_num ) FROM ztb_bb_gc
        WHERE loai_hang = @ls_gia_cong-LoaiHang AND substring( ngay_lap_bb, 1, 6 ) = @ls_gia_cong-NgayLapBb(6)
         INTO @lw_so_bb_num.
    lw_so_bb_num = lw_so_bb_num + 1.
*    ENDIF.

    IF ls_gia_cong-LoaiHang = '1'.
      lw_so_bb = 'HO_' .
    ELSE.
      lw_so_bb = 'HV_'.
    ENDIF.
    lw_so_bb = lw_so_bb && ls_gia_cong-NgayLapBb+2(2) && '/'  && ls_gia_cong-NgayLapBb+4(2) && '_' && lw_so_bb_num.
    IF lw_so_bb_sub IS NOT INITIAL.
      lw_so_bb = lw_so_bb && '_' && lw_so_bb_sub.
    ENDIF.

    "else set overall travel status to open ('O')
    MODIFY ENTITIES OF zr_tbbb_gc IN LOCAL MODE
      ENTITY ZrTbbbGc
        UPDATE FIELDS ( SoBb SoBbNum SoBbSub )
        WITH VALUE #(
          ( %tky = ls_gia_cong-%tky
            SoBb = lw_so_bb
            SoBbNum = lw_so_bb_num
            SoBbSub = lw_so_bb_sub )
        ).

    SELECT * FROM ztb_loi_h_dtl
    WHERE loai_hang = @ls_gia_cong-LoaiHang
    INTO TABLE @DATA(lt_loi).
    SORT lt_loi BY loai_loi error_code.
    LOOP AT lt_loi INTO DATA(ls_loi).
      ls_dtl-HdrID = ls_key-HdrID.
      ls_dtl-%target =  VALUE #(  ( %cid = 'Dtl' && sy-tabix
                                                    LoaiLoi = ls_loi-loai_loi
                                                    LoaiHang = ls_loi-loai_hang
                                                    ErrorCode = ls_loi-error_code
                                                    Errordesc = ls_loi-errordesc
                                                    Bangi = ls_loi-bangi
                                                    Bangii = ls_loi-bangii
                                                    ) ) .
      APPEND ls_dtl TO lt_dtl.
    ENDLOOP.

    "else set overall travel status to open ('O')
    MODIFY ENTITIES OF zr_tbbb_gc IN LOCAL MODE
     ENTITY ZrTbbbGc
       CREATE BY \_dtl
             FIELDS ( LoaiLoi  LoaiHang   ErrorCode Errordesc   Bangi    Bangii  )
               WITH lt_dtl
               REPORTED DATA(update_reported1).
  ENDMETHOD.

  METHOD checkInputFields_Save.
    CLEAR: reported-zrtbbbgc[].
    READ ENTITIES OF zr_tbbb_gc IN LOCAL MODE
      ENTITY ZrTbbbGc
      FIELDS ( SoPo  ct12 ct13 )
      WITH CORRESPONDING #( keys )
      RESULT DATA(zr_tbbb_gc).

    LOOP AT zr_tbbb_gc INTO DATA(ls_gc).
      APPEND VALUE #(  %tky           = ls_gc-%tky
                      %state_area    = 'SoPO'
                    ) TO reported-zrtbbbgc.
      SELECT SINGLE * FROM zv_po_gc
        WHERE PurchaseOrder = @ls_gc-SoPo
        INTO @DATA(ls_po_check).
      IF sy-subrc IS NOT INITIAL. "AND sy-mandt <> '80'.
        APPEND VALUE #(  %tky = ls_gc-%tky ) TO failed-ZrTbbbGc.
        APPEND VALUE #( %tky          = ls_gc-%tky
                        %state_area   = 'SoPO'
                        %msg          = new_message_with_text(
                                severity = if_abap_behv_message=>severity-error
                                text     = 'Số PO gia công không tồn tại' )
                        %element-SoPo = if_abap_behv=>mk-on
                      ) TO reported-zrtbbbgc.
      ENDIF.

      IF ls_gc-Ct12 IS  INITIAL.
        APPEND VALUE #(  %tky = ls_gc-%tky ) TO failed-ZrTbbbGc.
        APPEND VALUE #( %tky          = ls_gc-%tky
                        %state_area   = 'Ct12'
                        %msg          = new_message_with_text(
                                severity = if_abap_behv_message=>severity-error
                                text     = 'Nhập Tổng cái' )
                        %element-Ct12 = if_abap_behv=>mk-on
                      ) TO reported-zrtbbbgc.
      ENDIF.

      IF  ls_gc-Ct13 IS  INITIAL.
        APPEND VALUE #(  %tky = ls_gc-%tky ) TO failed-ZrTbbbGc.
        APPEND VALUE #( %tky          = ls_gc-%tky
                        %state_area   = 'Ct13'
                        %msg          = new_message_with_text(
                                severity = if_abap_behv_message=>severity-error
                                text     = 'Nhập Số lượng kiểm' )
                        %element-Ct13 = if_abap_behv=>mk-on
                      ) TO reported-zrtbbbgc.
      ENDIF.
    ENDLOOP.

    READ ENTITIES OF zr_tbbb_gc IN LOCAL MODE
      ENTITY ZrTbgcLoi
      FIELDS ( LoaiLoi  ErrorCode )
      WITH CORRESPONDING #( keys )
      RESULT DATA(ZrTbgcLoi).
    DELETE ZrTbgcLoi WHERE LoaiLoi = 'A' OR LoaiLoi = 'B'.
    LOOP AT ZrTbgcLoi INTO DATA(ls_loi).
      APPEND VALUE #(  %tky           = ls_gc-%tky
                      %state_area    = 'ErrorCode'
                    ) TO reported-zrtbgcloi.
      LOOP AT ZrTbgcLoi INTO DATA(ls_loi_check)
        WHERE ErrorCode = ls_loi-ErrorCode AND %tky <> ls_loi-%tky.

        APPEND VALUE #(  %tky = ls_loi-%tky ) TO failed-zrtbgcloi.
        APPEND VALUE #( %tky          = ls_loi-%tky
                        %state_area   = 'ErrorCode'
                        %msg          = new_message_with_text(
                                severity = if_abap_behv_message=>severity-error
                                text     = 'Trùng mã lỗi' )
                        %element-ErrorCode = if_abap_behv=>mk-on
                      ) TO reported-zrtbgcloi.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZR_TBBB_GC DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZR_TBBB_GC IMPLEMENTATION.

  METHOD save_modified.
    DATA : lt_bb_gc    TYPE STANDARD TABLE OF ztb_bb_gc,
           lt_bb_gc_ud TYPE STANDARD TABLE OF ztb_bb_gc,
           ls_bb_gc    TYPE                   ztb_bb_gc,
           ls_bb_gc_db TYPE                   ztb_bb_gc,
           ls_bb_gc_ud TYPE                   ztb_bb_gc.

    DATA : lt_gc_loi TYPE STANDARD TABLE OF ztb_gc_loi,
           ls_gc_loi TYPE                   ztb_gc_loi.
    DATA: lw_field_name TYPE char72.

    TYPES: BEGIN OF ty_mapping,
             field_ent TYPE string,
             field_db  TYPE string,
           END OF ty_mapping.

    DATA: gt_mapping TYPE STANDARD TABLE OF ty_mapping WITH DEFAULT KEY.

    "Insert các cặp mapping
    APPEND VALUE #( field_ent = 'HdrID'         field_db = 'hdr_id' )         TO gt_mapping.
    APPEND VALUE #( field_ent = 'LoaiHang'      field_db = 'loai_hang' )     TO gt_mapping.
    APPEND VALUE #( field_ent = 'SoBb'          field_db = 'so_bb' )         TO gt_mapping.
    APPEND VALUE #( field_ent = 'SoBbNum'       field_db = 'so_bb_num' )     TO gt_mapping.
    APPEND VALUE #( field_ent = 'SoBbSub'       field_db = 'so_bb_sub' )     TO gt_mapping.
    APPEND VALUE #( field_ent = 'NgayLapBb'     field_db = 'ngay_lap_bb' )   TO gt_mapping.
    APPEND VALUE #( field_ent = 'SoPo'          field_db = 'so_po' )         TO gt_mapping.
    APPEND VALUE #( field_ent = 'NgayNhapHang'  field_db = 'ngay_nhap_hang') TO gt_mapping.
    APPEND VALUE #( field_ent = 'NgayTraBb'     field_db = 'ngay_tra_bb' )   TO gt_mapping.
    APPEND VALUE #( field_ent = 'NgayNhapKho'   field_db = 'ngay_nhap_kho' ) TO gt_mapping.
    APPEND VALUE #( field_ent = 'GhiChu'        field_db = 'ghi_chu' )       TO gt_mapping.
    APPEND VALUE #( field_ent = 'CreatedBy'     field_db = 'created_by' )    TO gt_mapping.
    APPEND VALUE #( field_ent = 'CreatedAt'     field_db = 'created_at' )    TO gt_mapping.
    APPEND VALUE #( field_ent = 'LastChangedBy' field_db = 'last_changed_by') TO gt_mapping.
    APPEND VALUE #( field_ent = 'LastChangedAt' field_db = 'last_changed_at') TO gt_mapping.
    LOOP AT gt_mapping ASSIGNING FIELD-SYMBOL(<fs_mapping>).
      TRANSLATE <fs_mapping>-field_ent TO UPPER CASE.
      TRANSLATE <fs_mapping>-field_db TO UPPER CASE.
    ENDLOOP.
    DATA: gt_mapping_loi TYPE STANDARD TABLE OF ty_mapping WITH DEFAULT KEY.

    "Insert các cặp mapping
    APPEND VALUE #( field_ent = 'HdrID'         field_db = 'hdr_id' )         TO gt_mapping_loi.
    APPEND VALUE #( field_ent = 'DtlID'         field_db = 'dtl_id' )         TO gt_mapping_loi.
    APPEND VALUE #( field_ent = 'LoaiHang'      field_db = 'loai_hang' )      TO gt_mapping_loi.
    APPEND VALUE #( field_ent = 'LoaiLoi'       field_db = 'loai_loi' )       TO gt_mapping_loi.
    APPEND VALUE #( field_ent = 'ErrorCode'     field_db = 'error_code' )     TO gt_mapping_loi.
    APPEND VALUE #( field_ent = 'SlLoi'         field_db = 'sl_loi' )         TO gt_mapping_loi.
    APPEND VALUE #( field_ent = 'CheckBangi'    field_db = 'check_bangi' )    TO gt_mapping_loi.
    APPEND VALUE #( field_ent = 'CheckBangii'   field_db = 'check_bangii' )   TO gt_mapping_loi.
    APPEND VALUE #( field_ent = 'GhiChu'        field_db = 'ghi_chu' )        TO gt_mapping_loi.
    APPEND VALUE #( field_ent = 'CreatedBy'     field_db = 'created_by' )     TO gt_mapping_loi.
    APPEND VALUE #( field_ent = 'CreatedAt'     field_db = 'created_at' )     TO gt_mapping_loi.
    APPEND VALUE #( field_ent = 'LastChangedBy' field_db = 'last_changed_by') TO gt_mapping_loi.
    APPEND VALUE #( field_ent = 'LastChangedAt' field_db = 'last_changed_at') TO gt_mapping_loi.

    LOOP AT gt_mapping_loi ASSIGNING  <fs_mapping>.
      TRANSLATE <fs_mapping>-field_ent TO UPPER CASE.
      TRANSLATE <fs_mapping>-field_db TO UPPER CASE.
    ENDLOOP.

    IF create-zrtbbbgc IS NOT INITIAL.
      lt_bb_gc = CORRESPONDING #( create-zrtbbbgc MAPPING FROM ENTITY ).

      LOOP AT lt_bb_gc INTO ls_bb_gc.
        IF ls_bb_gc-so_bb_base IS NOT INITIAL.
          DATA: lw_so_bb_sub TYPE ztb_bb_gc-so_bb_sub.
          lw_so_bb_sub = '00'.
          SELECT SINGLE * FROM ztb_bb_gc
            WHERE so_bb = @ls_bb_gc-so_bb_base
            INTO @DATA(ls_bb_gc_base).
          IF sy-subrc IS INITIAL.
            SELECT * FROM ztb_gc_loi
              WHERE hdr_id = @ls_bb_gc_base-hdr_id
              INTO TABLE @DATA(lt_gc_loi_base).

            SELECT so_bb_sub FROM ztb_bb_gc
              WHERE substring( so_bb,1,15 ) = @ls_bb_gc_base-so_bb(15)
              INTO TABLE @DATA(lt_bb_sub).
            DO 100 TIMES.
              lw_so_bb_sub = lw_so_bb_sub + 1.
              READ TABLE lt_bb_sub WITH KEY table_line = lw_so_bb_sub TRANSPORTING NO FIELDS.
              IF sy-subrc IS NOT INITIAL.
                EXIT.
              ENDIF.
            ENDDO.
            ls_bb_gc_base-hdr_id = ls_bb_gc-hdr_id.
            ls_bb_gc_base-so_bb_sub = lw_so_bb_sub.
            ls_bb_gc_base-so_bb_base = ls_bb_gc-so_bb_base.
            ls_bb_gc_base-so_bb = ls_bb_gc_base-so_bb && '_' && lw_so_bb_sub.

            CLEAR:      ls_bb_gc_base-ngay_lap_bb,
                          ls_bb_gc_base-ngay_nhap_kho,
                          ls_bb_gc_base-ngay_tra_bb,
                          ls_bb_gc_base-ghi_chu,
                          ls_bb_gc_base-Ct16,
                          ls_bb_gc_base-Ct321,
                          ls_bb_gc_base-Ct322,
                          ls_bb_gc_base-Ct323,
                          ls_bb_gc_base-Ct324,
                          ls_bb_gc_base-bs01,
                          ls_bb_gc_base-bs02,
                          ls_bb_gc_base-bs03,
                          ls_bb_gc_base-bs04,
                          ls_bb_gc_base-bs05,
                          ls_bb_gc_base-bs06,
                          ls_bb_gc_base-bs07,
                          ls_bb_gc_base-Ct33,
                          ls_bb_gc_base-Ct34,
                          ls_bb_gc_base-Ct35,
                          ls_bb_gc_base-Ct36,
                          ls_bb_gc_base-Ct37,
                          ls_bb_gc_base-Ct38,
                          ls_bb_gc_base-Ct39,
                          ls_bb_gc_base-Ct23,
                          ls_bb_gc_base-ct41,
                          ls_bb_gc_base-ct42,
                          ls_bb_gc_base-ct43,
                          ls_bb_gc_base-ct44,
                          ls_bb_gc_base-ct45,
                          ls_bb_gc_base-ct46,
                          ls_bb_gc_base-ct47.

            INSERT ztb_bb_gc FROM  @ls_bb_gc_base.
            LOOP AT lt_gc_loi_base INTO DATA(ls_gc_loi_base).
              ls_gc_loi_base-hdr_id = ls_bb_gc-hdr_id.
              INSERT ztb_gc_loi FROM  @ls_gc_loi_base.
            ENDLOOP.
          ENDIF.
        ELSE.
          IF ls_bb_gc-ct12 IS NOT INITIAL.
            ls_bb_gc-ct14 = ls_bb_gc-ct13 / ls_bb_gc-ct12  * 100.
          ENDIF.

          INSERT ztb_bb_gc FROM  @ls_bb_gc.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT delete-zrtbbbgc INTO DATA(ls_detele)." WHERE HdrID IS NOT INITIAL.
      DELETE FROM ztb_bb_gc WHERE hdr_id = @ls_detele-HdrID.
    ENDLOOP.

    IF create-zrtbgcloi IS NOT INITIAL.
      lt_gc_loi = CORRESPONDING #( create-zrtbgcloi MAPPING FROM ENTITY ).
      LOOP AT lt_gc_loi ASSIGNING FIELD-SYMBOL(<lf_gc_loi>).
        IF <lf_gc_loi>-loai_loi = ''.
          <lf_gc_loi>-loai_loi = 'C'.
        ENDIF.
      ENDLOOP.
      INSERT ztb_gc_loi FROM TABLE @lt_gc_loi.
      SORT lt_gc_loi BY hdr_id.
      DELETE ADJACENT DUPLICATES FROM lt_gc_loi COMPARING hdr_id.
      LOOP AT lt_gc_loi INTO ls_gc_loi.
        SELECT SINGLE * FROM ztb_bb_gc
              WHERE hdr_id = @ls_gc_loi-hdr_id
              INTO @ls_bb_gc.
        IF sy-subrc IS INITIAL.
          APPEND ls_bb_gc TO lt_bb_gc_ud.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF delete-zrtbgcloi IS NOT INITIAL.
      LOOP AT delete-zrtbgcloi INTO DATA(ls_detele_dtl)." WHERE HdrID IS NOT INITIAL AND DtlID IS NOT INITIAL.
        DELETE FROM ztb_gc_loi WHERE hdr_id = @ls_detele_dtl-HdrID AND dtl_id = @ls_detele_dtl-DtlID.
      ENDLOOP.

      lt_gc_loi =  CORRESPONDING #( delete-zrtbgcloi MAPPING FROM ENTITY ).
      SORT lt_gc_loi BY hdr_id.
      DELETE ADJACENT DUPLICATES FROM lt_gc_loi COMPARING hdr_id.
      LOOP AT lt_gc_loi INTO ls_gc_loi.
        SELECT SINGLE * FROM ztb_bb_gc
              WHERE hdr_id = @ls_gc_loi-hdr_id
              INTO @ls_bb_gc.
        IF sy-subrc IS INITIAL.
          APPEND ls_bb_gc TO lt_bb_gc_ud.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT lt_bb_gc_ud INTO ls_bb_gc.
      CALL METHOD zcl_gia_cong=>update_bb_gc
        EXPORTING
          i_bb_gc = ls_bb_gc.
    ENDLOOP.

    IF update-zrtbbbgc IS NOT INITIAL OR update-zrtbgcloi IS NOT INITIAL.
      CLEAR lt_bb_gc.
      CLEAR lt_gc_loi.
      lt_gc_loi = CORRESPONDING #( update-zrtbgcloi MAPPING FROM ENTITY ).
      lt_bb_gc = CORRESPONDING #( update-zrtbbbgc MAPPING FROM ENTITY ).
      IF lt_bb_gc IS NOT INITIAL.
        READ TABLE lt_bb_gc INDEX 1 INTO ls_bb_gc_ud.
        SELECT SINGLE * FROM ztb_bb_gc
           WHERE hdr_id = @ls_bb_gc_ud-hdr_id
           INTO @ls_bb_gc .
      ELSE.
        READ TABLE lt_gc_loi INDEX 1 INTO ls_gc_loi.
        SELECT SINGLE * FROM ztb_bb_gc
            WHERE hdr_id = @ls_gc_loi-hdr_id
            INTO @ls_bb_gc .
      ENDIF.

      DATA update_struct TYPE REF TO cl_abap_structdescr.
      IF update-zrtbbbgc IS NOT INITIAL.
        READ TABLE update-zrtbbbgc INDEX 1 INTO DATA(ls_update_zrtbbbgc).

        update_struct ?= cl_abap_structdescr=>describe_by_data( ls_update_zrtbbbgc-%control ).

        LOOP AT update_struct->components INTO DATA(field).

          IF ls_update_zrtbbbgc-%control-(field-name) = if_abap_behv=>mk-on.
            READ TABLE gt_mapping ASSIGNING <fs_mapping>
                WITH KEY field_ent = field-name.
            IF sy-subrc IS INITIAL.
              lw_field_name = <fs_mapping>-field_db.
            ELSE.
              lw_field_name = field-name.
            ENDIF.
            ls_bb_gc-(lw_field_name) = ls_update_zrtbbbgc-(field-name).
          ENDIF.
        ENDLOOP.

        FREE update_struct.
      ENDIF.

      SELECT * FROM ztb_gc_loi
          WHERE hdr_id = @ls_bb_gc-hdr_id
          INTO TABLE @lt_gc_loi.

      IF update-zrtbgcloi IS NOT INITIAL.
        READ TABLE update-zrtbgcloi INDEX 1 INTO DATA(ls_update_zrtbgcloi).
        update_struct ?= cl_abap_structdescr=>describe_by_data( ls_update_zrtbgcloi-%control ).
        LOOP AT update-zrtbgcloi INTO ls_update_zrtbgcloi.
          READ TABLE lt_gc_loi ASSIGNING <lf_gc_loi>
              WITH KEY hdr_id = ls_update_zrtbgcloi-HdrID dtl_id = ls_update_zrtbgcloi-DtlID.
          IF sy-subrc IS INITIAL.
            LOOP AT update_struct->components INTO field.
              IF ls_update_zrtbgcloi-%control-(field-name) = if_abap_behv=>mk-on.
                READ TABLE gt_mapping_loi ASSIGNING <fs_mapping>
                  WITH KEY field_ent = field-name.
                IF sy-subrc IS INITIAL.
                  lw_field_name = <fs_mapping>-field_db.
                ELSE.
                  lw_field_name = field-name.
                ENDIF.
                <lf_gc_loi>-(lw_field_name) = ls_update_zrtbgcloi-(field-name).
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDLOOP.
      ENDIF.

      MODIFY ztb_bb_gc FROM @ls_bb_gc.
      MODIFY ztb_gc_loi FROM TABLE @lt_gc_loi.

      CALL METHOD zcl_gia_cong=>update_bb_gc
        EXPORTING
          i_bb_gc = ls_bb_gc.

    ENDIF.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
