CLASS zcl_gia_cong DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES tt_xetduyet_dtl TYPE STANDARD TABLE OF ztb_xetduyet_dtl.
    CLASS-METHODS: get_xet_duyet
      IMPORTING
                i_xetduyet_hdr  TYPE ztb_xetduyet_hdr
      EXPORTING e_xet_duyet_dtl TYPE tt_xetduyet_dtl.

    CLASS-METHODS: get_vobao
      IMPORTING
                i_date_fr       TYPE zde_date
                i_date_to       TYPE zde_date
                i_xet_duyet_dtl TYPE ztb_xetduyet_dtl
      EXPORTING e_so_tien       TYPE zde_dec23_0.

    CLASS-METHODS: update_xet_duyet_dtl
      CHANGING
        c_xet_duyet_dtl TYPE ztb_xetduyet_dtl.

    CLASS-METHODS: update_bb_gc
      IMPORTING
        i_bb_gc TYPE ztb_bb_gc.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_GIA_CONG IMPLEMENTATION.


  METHOD get_xet_duyet.
    DATA lt_xetduyet_dtl TYPE tt_xetduyet_dtl.
    DATA ls_xetduyet_dtl TYPE ztb_xetduyet_dtl.
    DATA: lw_ho TYPE zde_flag,
          lw_hv TYPE zde_flag.
    DATA: lw_dtl_cr TYPE zde_flag.

    SELECT SINGLE * FROM zr_tbperiod
        WHERE Zper = @i_xetduyet_hdr-zper
        INTO  @DATA(ls_tbperiod).
    IF sy-subrc <> 0.
      RETURN. " No records found
    ENDIF.
    SELECT * FROM zi_penalty_price_1
    WHERE ValidFrom <= @ls_tbperiod-zdatefr
      AND ValidTo >= @ls_tbperiod-zdateto
      INTO TABLE @DATA(lt_penalty_price_1).
    LOOP AT lt_penalty_price_1 INTO DATA(ls_penalty_price_1).
      IF ls_penalty_price_1-ErrorCode = '02_01'.
        DATA(ls_penalty_price_1_02_01) = ls_penalty_price_1.
      ENDIF.
      IF ls_penalty_price_1-ErrorCode = '02_02'.
        DATA(ls_penalty_price_1_02_02) = ls_penalty_price_1.
      ENDIF.
      IF ls_penalty_price_1-ErrorCode = '03_01'.
        DATA(ls_penalty_price_1_03_01) = ls_penalty_price_1.
      ENDIF.

      IF ls_penalty_price_1-ErrorCode = '09_01'.
        DATA(ls_penalty_price_1_09_01) = ls_penalty_price_1.
      ENDIF.

      IF ls_penalty_price_1-ErrorCode = '06_01'.
        DATA(ls_penalty_price_1_06_01) = ls_penalty_price_1.
      ENDIF.

      IF ls_penalty_price_1-ErrorCode = '08_01'.
        DATA(ls_penalty_price_1_08_01) = ls_penalty_price_1.
      ENDIF.

      IF ls_penalty_price_1-ErrorCode = '04_01'.
        DATA(ls_penalty_price_1_04_01) = ls_penalty_price_1.
      ENDIF.

      IF ls_penalty_price_1-ErrorCode = '10_01'.
        DATA(ls_penalty_price_1_10_01) = ls_penalty_price_1.
      ENDIF.

      IF ls_penalty_price_1-ErrorCode = '11_01'.
        DATA(ls_penalty_price_1_11_01) = ls_penalty_price_1.
      ENDIF.

      IF ls_penalty_price_1-ErrorCode = '17_01'.
        DATA(ls_penalty_price_1_17_01) = ls_penalty_price_1.
      ENDIF.

      IF ls_penalty_price_1-ErrorCode = '18_01'.
        DATA(ls_penalty_price_1_18_01) = ls_penalty_price_1.
      ENDIF.
    ENDLOOP.

    SELECT * FROM zi_error_rate_rp
*        WHERE ValidFrom <= @ls_tbperiod-zdatefr
*          AND ValidTo >= @ls_tbperiod-zdateto
        INTO TABLE @DATA(lt_error_rate_rp).
    SORT lt_error_rate_rp BY ErrorCode ErrorRateFrom DESCENDING.

    READ TABLE  lt_error_rate_rp INTO DATA(ls_error_rate_rp_11_01)
        WITH KEY ErrorCode = '11_01'.

    READ TABLE  lt_error_rate_rp INTO DATA(ls_error_rate_rp_10_01)
        WITH KEY ErrorCode = '10_01'.

    SELECT SINGLE * FROM zr_tbtru_bs WHERE Zper = @i_xetduyet_hdr-zper AND Bukrs = @i_xetduyet_hdr-bukrs
      INTO @DATA(ls_tru_bs).
    IF sy-subrc = 0.
      SELECT * FROM zr_tbtru_bs_dtl WHERE HdrID = @ls_tru_bs-HdrID
        INTO TABLE @DATA(lt_tru_bs_dtl).
      SORT lt_tru_bs_dtl BY Supplier Material.
    ENDIF.

    SELECT SINGLE * FROM zr_tbtru_thieu WHERE Zper = @i_xetduyet_hdr-zper AND Bukrs = @i_xetduyet_hdr-bukrs
        INTO @DATA(ls_tru_thieu).
    IF sy-subrc = 0.
      SELECT * FROM zr_tbtru_thie_dtl WHERE HdrID = @ls_tru_thieu-HdrID
        INTO TABLE @DATA(lt_tru_thie_dtl).
      SORT lt_tru_thie_dtl BY Supplier Material.
    ENDIF.

    SELECT * FROM zr_tbbb_gc
        WHERE CompanyCode = @i_xetduyet_hdr-bukrs AND NgayLapBb >= @ls_tbperiod-zdatefr AND ngayLapBb <= @ls_tbperiod-zdateto
        INTO TABLE @DATA(lt_bb_gc).

    SELECT * FROM ztb_xetduyet_dtl
    WHERE hdr_id = @i_xetduyet_hdr-hdr_id
    INTO TABLE @DATA(lt_dtl_db).
    SORT lt_dtl_db BY supplier.
    DATA(lt_bb_gc_tmp) = lt_bb_gc.
    DATA lt_so_po TYPE TABLE OF ztb_bb_gc-so_po.

    SORT lt_bb_gc_tmp BY Supplier.
    DELETE ADJACENT DUPLICATES FROM lt_bb_gc_tmp COMPARING Supplier.
    LOOP AT lt_bb_gc_tmp INTO DATA(ls_bb_gc_tmp).

      CLEAR: lw_ho, lw_hv, ls_xetduyet_dtl, lw_dtl_cr, lt_so_po.
      READ TABLE lt_dtl_db
      WITH KEY supplier =  ls_bb_gc_tmp-supplier
      INTO DATA(ls_xetduyet_dtl_db).
      IF sy-subrc IS NOT INITIAL.

        TRY.
            DATA(lv_uuid) = cl_system_uuid=>create_uuid_x16_static( ).
          CATCH cx_uuid_error INTO DATA(lx_uuid).

        ENDTRY.
        ls_xetduyet_dtl-dtl_id = lv_uuid.
        lw_dtl_cr = 'X'.
      ELSE.
*        ls_xetduyet_dtl-dtl_id = ls_xetduyet_dtl_db-dtl_id.
        MOVE-CORRESPONDING ls_xetduyet_dtl_db TO ls_xetduyet_dtl.
        CLEAR ls_xetduyet_dtl-Ct04.
        CLEAR ls_xetduyet_dtl-Ct05.
        CLEAR ls_xetduyet_dtl-Ct06.
        CLEAR ls_xetduyet_dtl-Ct07.
        CLEAR ls_xetduyet_dtl-Ct08.
        CLEAR ls_xetduyet_dtl-Ct09.
        CLEAR ls_xetduyet_dtl-Ct10.
        CLEAR ls_xetduyet_dtl-Ct11.
        CLEAR ls_xetduyet_dtl-Ct12.
        CLEAR ls_xetduyet_dtl-Ct13.
        CLEAR ls_xetduyet_dtl-Ct14.
        CLEAR ls_xetduyet_dtl-Ct15.
        CLEAR ls_xetduyet_dtl-Ct16.
        CLEAR ls_xetduyet_dtl-Ct17.
        CLEAR ls_xetduyet_dtl-Ct18.
        CLEAR ls_xetduyet_dtl-Ct19.
        CLEAR ls_xetduyet_dtl-Ct20.
        CLEAR ls_xetduyet_dtl-Ct21.
        CLEAR ls_xetduyet_dtl-Ct22.
        CLEAR ls_xetduyet_dtl-Ct23.
        CLEAR ls_xetduyet_dtl-ct23a.
        CLEAR ls_xetduyet_dtl-Ct24.
        CLEAR ls_xetduyet_dtl-Ct25.
        CLEAR ls_xetduyet_dtl-Ct26.
        CLEAR ls_xetduyet_dtl-Ct27.
        CLEAR ls_xetduyet_dtl-Ct28.
        CLEAR ls_xetduyet_dtl-Ct29a.
        CLEAR ls_xetduyet_dtl-Ct29.
        CLEAR ls_xetduyet_dtl-Ct291.
        CLEAR ls_xetduyet_dtl-Ct292.
        CLEAR ls_xetduyet_dtl-Ct30.
        CLEAR ls_xetduyet_dtl-Ct31.
        CLEAR ls_xetduyet_dtl-Ct32.
        CLEAR ls_xetduyet_dtl-Ct33.
        CLEAR ls_xetduyet_dtl-Ct34.
        CLEAR ls_xetduyet_dtl-Ct35.
        CLEAR ls_xetduyet_dtl-Ct36.
        CLEAR ls_xetduyet_dtl-Ct37.
        CLEAR ls_xetduyet_dtl-Ct38.
        CLEAR ls_xetduyet_dtl-Ct38a.
        CLEAR ls_xetduyet_dtl-Ct39.
        CLEAR ls_xetduyet_dtl-ct39a.
        CLEAR ls_xetduyet_dtl-ct401.
        CLEAR ls_xetduyet_dtl-ct40a.
        CLEAR ls_xetduyet_dtl-ct40a1.
        CLEAR ls_xetduyet_dtl-ct40a2.
        CLEAR ls_xetduyet_dtl-ct40c.
        CLEAR ls_xetduyet_dtl-ct40d.
        CLEAR ls_xetduyet_dtl-ct40e.
        CLEAR ls_xetduyet_dtl-ct40f.
        CLEAR ls_xetduyet_dtl-Ct41.
        CLEAR ls_xetduyet_dtl-Ct42.
        CLEAR ls_xetduyet_dtl-Ct43.
        CLEAR ls_xetduyet_dtl-Ct44.
        CLEAR ls_xetduyet_dtl-Ct45.
        CLEAR ls_xetduyet_dtl-Ct46.
        CLEAR ls_xetduyet_dtl-Ct47.
        CLEAR ls_xetduyet_dtl-Ct48.
        CLEAR ls_xetduyet_dtl-Ct49.
        CLEAR ls_xetduyet_dtl-Ct50.
        CLEAR ls_xetduyet_dtl-Ct51.
        CLEAR ls_xetduyet_dtl-Ct52.
        CLEAR ls_xetduyet_dtl-Ct53.
        CLEAR ls_xetduyet_dtl-Ct55.
      ENDIF.
      ls_xetduyet_dtl-hdr_id = i_xetduyet_hdr-hdr_id.
      ls_xetduyet_dtl-supplier = ls_bb_gc_tmp-supplier.
      SELECT SINGLE * FROM zi_supplier_time
          WHERE CodeProcessing = @ls_bb_gc_tmp-supplier
            INTO @DATA(ls_supplier_time).
      IF sy-subrc = 0.
        ls_xetduyet_dtl-tgian_hdong = i_xetduyet_hdr-sumdate - ls_supplier_time-ValidFrom.

        ls_xetduyet_dtl-tgian_hdong = ls_xetduyet_dtl-tgian_hdong / 365 .
      ENDIF.
      LOOP AT lt_bb_gc INTO DATA(ls_bb_gc) WHERE Supplier = ls_bb_gc_tmp-Supplier.
        IF ls_bb_gc-LoaiHang = '1'.
          lw_ho = 'X'.
        ENDIF.
        IF ls_bb_gc-LoaiHang = '2'.
          lw_hv = 'X'.
        ENDIF.

        ls_xetduyet_dtl-ct04 = ls_xetduyet_dtl-ct04  + ls_bb_gc-Ct23.
        ls_xetduyet_dtl-ct07 += ls_bb_gc-ct18.
        ls_xetduyet_dtl-ct08 += ls_bb_gc-ct19.
        ls_xetduyet_dtl-ct09 += ls_bb_gc-ct20.
        ls_xetduyet_dtl-ct10 += ls_bb_gc-ct21.
        ls_xetduyet_dtl-ct11 += ls_bb_gc-ct40.
        DATA lw_ct39a like  ls_xetduyet_dtl-ct39a.
        CLEAR: lw_ct39a.
        IF ls_bb_gc-ct47 < 1000.
          ls_xetduyet_dtl-ct12 += ls_bb_gc-ct47.
        ELSE.
          ls_xetduyet_dtl-ct13 += ls_bb_gc-ct47.
          lw_ct39a = ls_bb_gc-ct47.
        ENDIF.
        ls_xetduyet_dtl-ct14 += ls_bb_gc-Ct321.
        lw_ct39a += ls_bb_gc-Ct321.
        ls_xetduyet_dtl-ct15 += ls_bb_gc-Ct322.
        ls_xetduyet_dtl-ct16 += ls_bb_gc-Ct323.
        ls_xetduyet_dtl-ct17 += ls_bb_gc-Ct324.
        ls_xetduyet_dtl-ct18 += ls_bb_gc-Ct22.

        SELECT SUM( NetPriceAmount ) AS NetPriceAmount, DocumentCurrency FROM ZI_PurchaseOrderItemAPI01
        WHERE PurchaseOrder = @ls_bb_gc-SoPo AND Material = ''
        GROUP BY DocumentCurrency
        INTO TABLE @DATA(lt_NetPriceAmount).
        IF sy-subrc = 0.
          READ TABLE lt_NetPriceAmount INTO DATA(ls_NetPriceAmount) INDEX 1.
          ls_xetduyet_dtl-ct23 += ls_NetPriceAmount-NetPriceAmount * ls_bb_gc-Ct23 * 100.
          DATA(lw_ct23) = ls_NetPriceAmount-NetPriceAmount * ls_bb_gc-Ct23 * 100.
          DATA(lw_NetPriceAmount) = ls_NetPriceAmount-NetPriceAmount.
        ENDIF.

        SELECT SINGLE NetPriceAmount, DocumentCurrency FROM ZI_PurchaseOrderItemAPI01
        WHERE PurchaseOrder = @ls_bb_gc-SoPo AND Material <> ''
        INTO @ls_NetPriceAmount.
        IF sy-subrc = 0.
          ls_xetduyet_dtl-ct24 += ls_NetPriceAmount-NetPriceAmount * ls_bb_gc-Ct23 * 100.
          lw_NetPriceAmount += ls_NetPriceAmount-NetPriceAmount.
        ENDIF.

        ls_xetduyet_dtl-ct39a += lw_ct39a * lw_NetPriceAmount * 100.
        CLEAR: lw_NetPriceAmount.

        IF ls_bb_gc-bs06 = 'X'.
          ls_xetduyet_dtl-ct40e += ls_penalty_price_1_17_01-PenaltyPrice.
        ENDIF.

        IF ls_bb_gc-bs07 = 'X'.
          ls_xetduyet_dtl-ct40f += ls_penalty_price_1_18_01-PenaltyPrice.
        ENDIF.

        IF ls_bb_gc-ct16 <> 0 .
          IF ls_bb_gc-LoaiHang = '1'.
            ls_xetduyet_dtl-ct40d += ls_bb_gc-ct16 * ls_error_rate_rp_10_01-DeductionPercent * ls_penalty_price_1_10_01-PenaltyPrice / 100.
          ELSE.
            ls_xetduyet_dtl-ct40d += ls_bb_gc-ct16 * ls_error_rate_rp_11_01-DeductionPercent  * ls_penalty_price_1_11_01-PenaltyPrice / 100.
          ENDIF.
        ENDIF.

        READ TABLE lt_so_po WITH KEY table_line = ls_bb_gc-SoPo TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND ls_bb_gc-SoPo TO lt_so_po.
          SELECT SUM( purordamountincompanycodecrcy ) FROM zI_PurchaseOrderHistory
                INNER JOIN ZI_PurchaseOrderItemAPI01
                    ON ZI_PurchaseOrderHistory~PurchaseOrder = ZI_PurchaseOrderItemAPI01~PurchaseOrder
                    AND ZI_PurchaseOrderHistory~PurchaseOrderItem = ZI_PurchaseOrderItemAPI01~PurchaseOrderItem
              WHERE zI_PurchaseOrderHistory~PurchaseOrder = @ls_bb_gc-SoPo AND purchasinghistorycategory = 'E'
              AND PostingDate >= @ls_tbperiod-zdatefr AND PostingDate <= @ls_tbperiod-zdateto
              AND ZI_PurchaseOrderItemAPI01~Material = ''
              INTO @DATA(lw_PURORDAMOUNT).
          IF sy-subrc IS INITIAL.
            ls_xetduyet_dtl-ct23a += lw_PURORDAMOUNT * 100.
          ENDIF.
        ENDIF.

      ENDLOOP.

      ls_xetduyet_dtl-ct40d -= ls_xetduyet_dtl-ct23.
      ls_xetduyet_dtl-ct40d += ls_xetduyet_dtl-ct23a.


      LOOP AT lt_tru_bs_dtl INTO DATA(ls_tru_bs_dtl)
         WHERE Supplier = ls_xetduyet_dtl-Supplier.

        SELECT SINGLE ProductGroup FROM zc_Product
         WHERE Product = @ls_tru_bs_dtl-Material
         INTO @DATA(lw_ProductGroup).
        IF sy-subrc IS INITIAL.
          IF lw_ProductGroup = '210014' OR
             lw_ProductGroup = '210003' OR
             lw_ProductGroup = '210011'.
            DATA(lw_ct40a) = ls_tru_bs_dtl-Tongtientru.
          ELSE.
            DATA: lw_ct40a1 LIKE ls_tru_bs_dtl-Tongtientru.
            lw_ct40a1 += ls_tru_bs_dtl-Tongtientru.
          ENDIF.
        ENDIF.
      ENDLOOP.
      ls_xetduyet_dtl-ct40a2 += lw_ct40a.
      IF lw_ct40a1 > 0.
        ls_xetduyet_dtl-ct40a1 += lw_ct40a1.
      ENDIF.

      CLEAR: lw_ct40a1, lw_ct40a.

      LOOP AT lt_tru_thie_dtl INTO DATA(ls_tru_thie_dtl)
         WHERE Supplier = ls_xetduyet_dtl-Supplier.
        ls_xetduyet_dtl-ct31 += ls_tru_thie_dtl-Tongtientru.
      ENDLOOP.


      CALL METHOD get_vobao
        EXPORTING
          i_date_fr       = ls_tbperiod-zdatefr
          i_date_to       = ls_tbperiod-zdateto
          i_xet_duyet_dtl = ls_xetduyet_dtl
        IMPORTING
          e_so_tien       = DATA(lw_so_tien).
      .

      ls_xetduyet_dtl-ct40c = lw_so_tien.

      CLEAR ls_xetduyet_dtl-kieu_tui.
      CLEAR lw_so_tien.
      IF lw_ho = 'X'.
        ls_xetduyet_dtl-kieu_tui = 'Hàng ống'.
      ENDIF.
      IF lw_hv = 'X'.
        ls_xetduyet_dtl-kieu_tui = ls_xetduyet_dtl-kieu_tui && '-Hàng viền'.
      ENDIF.
      IF ls_xetduyet_dtl-kieu_tui(1) = '-'.
        ls_xetduyet_dtl-kieu_tui = ls_xetduyet_dtl-kieu_tui+1(*).
      ENDIF.

      ls_xetduyet_dtl-ct05 = i_xetduyet_hdr-ct05.
      IF ls_xetduyet_dtl-ct05 IS NOT INITIAL.
        ls_xetduyet_dtl-ct06 = ls_xetduyet_dtl-ct04 / ls_xetduyet_dtl-ct05.
      ENDIF.

      IF ls_xetduyet_dtl-ct04 IS NOT INITIAL.
        ls_xetduyet_dtl-ct19 = ls_xetduyet_dtl-ct08 / ls_xetduyet_dtl-ct04 * 100.
        ls_xetduyet_dtl-ct20 = ls_xetduyet_dtl-ct09 / ls_xetduyet_dtl-ct04 * 100..
        ls_xetduyet_dtl-ct21 = ls_xetduyet_dtl-ct10 / ls_xetduyet_dtl-ct04 * 100..
        ls_xetduyet_dtl-ct22 = ls_xetduyet_dtl-ct18 / ls_xetduyet_dtl-ct04 * 100..
      ENDIF.

      LOOP AT lt_error_rate_rp INTO DATA(ls_error_rate_rp)
          WHERE ErrorCode = '01_01'
            AND ErrorRateFrom <= ls_xetduyet_dtl-ct19.
        ls_xetduyet_dtl-ct41 = ls_error_rate_rp-DeductionPercent.
        EXIT.
      ENDLOOP.

      LOOP AT lt_error_rate_rp INTO ls_error_rate_rp
           WHERE ErrorCode = '03_01'
             AND ErrorRateFrom <= ls_xetduyet_dtl-ct22.
        ls_xetduyet_dtl-ct42 = ls_error_rate_rp-DeductionPercent.
        EXIT.
      ENDLOOP.

      DATA: lw_tile LIKE ls_error_rate_rp-ErrorRateFrom.
      IF ls_xetduyet_dtl-ct04 > 0.
        lw_tile = ls_xetduyet_dtl-ct16 / ls_xetduyet_dtl-ct04 * 100.
      ELSE.
        lw_tile = 0.
      ENDIF.
      LOOP AT lt_error_rate_rp INTO ls_error_rate_rp
      WHERE ErrorCode = '04_01'
        AND ErrorRateFrom <= lw_tile.
        ls_xetduyet_dtl-ct43 = ls_error_rate_rp-DeductionPercent.
        EXIT.
      ENDLOOP.

      IF ls_xetduyet_dtl-ct04 > 0.
        lw_tile = ls_xetduyet_dtl-ct17 / ls_xetduyet_dtl-ct04 * 100.
      ELSE.
        lw_tile = 0.
      ENDIF.
*      LOOP AT lt_error_rate_rp INTO ls_error_rate_rp
*      WHERE ErrorCode = '05_01'
*        AND ErrorRateFrom <= lw_tile.
*        ls_xetduyet_dtl-ct44 = ls_error_rate_rp-DeductionPercent.
*        EXIT.
*      ENDLOOP.
      ls_xetduyet_dtl-ct44 = 100.

      IF ls_xetduyet_dtl-ct04 > 0.
        lw_tile = ls_xetduyet_dtl-ct15 / ls_xetduyet_dtl-ct04 * 100.
      ELSE.
        lw_tile = 0.
      ENDIF.
      LOOP AT lt_error_rate_rp INTO ls_error_rate_rp
      WHERE ErrorCode = '06_01'
        AND ErrorRateFrom <= lw_tile.
        ls_xetduyet_dtl-ct45 = ls_error_rate_rp-DeductionPercent.
        EXIT.
      ENDLOOP.

      LOOP AT lt_error_rate_rp INTO ls_error_rate_rp
        WHERE ErrorCode = '02_01'
            AND ErrorRateFrom <= ls_xetduyet_dtl-ct20.
        ls_xetduyet_dtl-ct46 = ls_error_rate_rp-DeductionPercent.
        EXIT.
      ENDLOOP.
      LOOP AT lt_error_rate_rp INTO ls_error_rate_rp
         WHERE ErrorCode = '02_02'
             AND ErrorRateFrom <= ls_xetduyet_dtl-ct21.
        ls_xetduyet_dtl-ct47 = ls_error_rate_rp-DeductionPercent.
        EXIT.
      ENDLOOP.

      LOOP AT lt_error_rate_rp INTO ls_error_rate_rp
       WHERE ErrorCode = '07_01'.
        ls_xetduyet_dtl-ct48 = ls_error_rate_rp-DeductionPercent.
        EXIT.
      ENDLOOP.

      IF ls_xetduyet_dtl-ct04 > 0.
        lw_tile = ls_xetduyet_dtl-ct14 / ls_xetduyet_dtl-ct04 * 100.
      ELSE.
        lw_tile = 0.
      ENDIF.

      LOOP AT lt_error_rate_rp INTO ls_error_rate_rp
      WHERE ErrorCode = '09_01'
        AND ErrorRateFrom <= lw_tile.
        ls_xetduyet_dtl-ct49 = ls_error_rate_rp-DeductionPercent.
        EXIT.
      ENDLOOP.

      IF ls_xetduyet_dtl-ct04 > 0.
        lw_tile = (  ls_xetduyet_dtl-ct12 + ls_xetduyet_dtl-ct13 ) / ls_xetduyet_dtl-ct04 * 100.
      ELSE.
        lw_tile = 0.
      ENDIF.

      LOOP AT lt_error_rate_rp INTO ls_error_rate_rp
        WHERE ErrorCode = '08_01'
        AND ErrorRateFrom <= lw_tile.
        ls_xetduyet_dtl-ct50 = ls_error_rate_rp-DeductionPercent.
        EXIT.
      ENDLOOP.

      IF lw_dtl_cr = 'X'.
        ls_xetduyet_dtl-ct411 = ls_xetduyet_dtl-ct41.
        ls_xetduyet_dtl-ct421 = ls_xetduyet_dtl-ct42.
        ls_xetduyet_dtl-ct431 = ls_xetduyet_dtl-ct43.
        ls_xetduyet_dtl-ct441 = ls_xetduyet_dtl-ct44.
        ls_xetduyet_dtl-ct451 = ls_xetduyet_dtl-ct45.
        ls_xetduyet_dtl-ct461 = ls_xetduyet_dtl-ct46.
        ls_xetduyet_dtl-ct471 = ls_xetduyet_dtl-ct47.
        ls_xetduyet_dtl-ct481 = ls_xetduyet_dtl-ct48.
        ls_xetduyet_dtl-ct491 = ls_xetduyet_dtl-ct49.
        ls_xetduyet_dtl-ct501 = ls_xetduyet_dtl-ct50.
      ELSE.
*        ls_xetduyet_dtl-ct411 = ls_xetduyet_dtl_db-ct41.
*        ls_xetduyet_dtl-ct421 = ls_xetduyet_dtl_db-ct42.
*        ls_xetduyet_dtl-ct431 = ls_xetduyet_dtl_db-ct43.
*        ls_xetduyet_dtl-ct441 = ls_xetduyet_dtl_db-ct44.
*        ls_xetduyet_dtl-ct451 = ls_xetduyet_dtl_db-ct45.
*        ls_xetduyet_dtl-ct461 = ls_xetduyet_dtl_db-ct46.
*        ls_xetduyet_dtl-ct471 = ls_xetduyet_dtl_db-ct47.
*        ls_xetduyet_dtl-ct481 = ls_xetduyet_dtl_db-ct48.
*        ls_xetduyet_dtl-ct491 = ls_xetduyet_dtl_db-ct49.
*        ls_xetduyet_dtl-ct501 = ls_xetduyet_dtl_db-ct50.
      ENDIF.

      DATA: lr_supplier TYPE zcl_xnt=>tt_ranges.
      APPEND INITIAL LINE TO lr_supplier ASSIGNING FIELD-SYMBOL(<ls_supplier>).
      <ls_supplier>-sign = 'I'.
      <ls_supplier>-option = 'EQ'.
      <ls_supplier>-low = ls_xetduyet_dtl-supplier.
      CALL METHOD zcl_xnt=>get_xnt_ps
        EXPORTING
          i_datefr    = ls_tbperiod-zdatefr
          i_dateto    = ls_tbperiod-zdatefr
          ir_supplier = lr_supplier
        IMPORTING
          e_nxt       = DATA(lt_nxt).

      LOOP AT lt_nxt INTO DATA(ls_nxt).
        ls_xetduyet_dtl-ct55 += ls_nxt-XuatTKy.
      ENDLOOP.
      CLEAR lr_supplier.

      ls_xetduyet_dtl-ct25 = ls_xetduyet_dtl-ct09 * ls_penalty_price_1_02_01-PenaltyPrice.
*      ls_xetduyet_dtl-ct26 = ls_xetduyet_dtl-ct25 * ls_xetduyet_dtl-ct461.
      ls_xetduyet_dtl-ct27 = ls_xetduyet_dtl-ct10 * ls_penalty_price_1_02_02-PenaltyPrice.
*      ls_xetduyet_dtl-ct28 = ls_xetduyet_dtl-ct27 * ls_xetduyet_dtl-ct471.
      ls_xetduyet_dtl-ct29a = ls_xetduyet_dtl-ct18 * ls_penalty_price_1_03_01-PenaltyPrice .
*      ls_xetduyet_dtl-ct29 = ls_xetduyet_dtl-ct29a * ls_xetduyet_dtl-ct421.

*        ls_xetduyet_dtl-ct30 = ? Bỏ
*      ls_xetduyet_dtl-ct31 = ls_xetduyet_dtl-ct30 * ls_penalty_price_1_pm_05-PenaltyPrice.
*           ls_xetduyet_dtl-ct32 = ls_xetduyet_dtl-ct31 * ?.
      ls_xetduyet_dtl-ct33 = ls_xetduyet_dtl-ct14 * ls_penalty_price_1_09_01-PenaltyPrice.
*      ls_xetduyet_dtl-ct34 = ls_xetduyet_dtl-ct33 * ls_xetduyet_dtl-ct491.
      ls_xetduyet_dtl-ct35 = ls_xetduyet_dtl-ct15 * ls_penalty_price_1_06_01-PenaltyPrice.
*      ls_xetduyet_dtl-ct36 = ls_xetduyet_dtl-ct35 * ls_xetduyet_dtl-ct451.
      ls_xetduyet_dtl-ct37 = ( ls_xetduyet_dtl-ct12 + ls_xetduyet_dtl-ct13 ) * ls_penalty_price_1_08_01-PenaltyPrice.
*      ls_xetduyet_dtl-ct38 = ls_xetduyet_dtl-ct37 * ls_xetduyet_dtl-ct501.
      ls_xetduyet_dtl-ct38a = ls_xetduyet_dtl-ct16 * ls_penalty_price_1_04_01-PenaltyPrice.

*      ls_xetduyet_dtl-ct39 = ls_xetduyet_dtl-ct38a  * ls_xetduyet_dtl-ct431.

*      ls_xetduyet_dtl-ct52 = ls_xetduyet_dtl-ct26 + ls_xetduyet_dtl-ct28  + ls_xetduyet_dtl-ct32  + ls_xetduyet_dtl-ct34  + ls_xetduyet_dtl-ct38.
      CALL METHOD update_xet_duyet_dtl
        CHANGING
          c_xet_duyet_dtl = ls_xetduyet_dtl.
      APPEND ls_xetduyet_dtl TO lt_xetduyet_dtl.
    ENDLOOP.
    e_xet_duyet_dtl = lt_xetduyet_dtl.
  ENDMETHOD.


  METHOD update_xet_duyet_dtl.
    c_xet_duyet_dtl-ct32 = c_xet_duyet_dtl-ct31 * c_xet_duyet_dtl-ct481 / 100.
    c_xet_duyet_dtl-ct26 = c_xet_duyet_dtl-ct25 * c_xet_duyet_dtl-ct461 / 100.
    c_xet_duyet_dtl-ct28 = c_xet_duyet_dtl-ct27 * c_xet_duyet_dtl-ct471 / 100.
    c_xet_duyet_dtl-ct29 = c_xet_duyet_dtl-ct29a * c_xet_duyet_dtl-ct421 / 100.
    c_xet_duyet_dtl-ct34 = c_xet_duyet_dtl-ct33 * c_xet_duyet_dtl-ct491 / 100.
    c_xet_duyet_dtl-ct36 = c_xet_duyet_dtl-ct35 * c_xet_duyet_dtl-ct451 / 100.
    c_xet_duyet_dtl-ct38 = c_xet_duyet_dtl-ct37 * c_xet_duyet_dtl-ct501 / 100.
    c_xet_duyet_dtl-ct39 = c_xet_duyet_dtl-ct38a  * c_xet_duyet_dtl-ct431 / 100.
    c_xet_duyet_dtl-ct39a1 = c_xet_duyet_dtl-ct39a  * c_xet_duyet_dtl-ct441 / 100.

    c_xet_duyet_dtl-ct40a = c_xet_duyet_dtl-ct40a2 + c_xet_duyet_dtl-ct40a1 - 3 / 1000 * ( c_xet_duyet_dtl-ct23 - c_xet_duyet_dtl-ct26 - c_xet_duyet_dtl-ct28 - c_xet_duyet_dtl-ct29 ) .

    c_xet_duyet_dtl-ct52 = c_xet_duyet_dtl-ct26 + c_xet_duyet_dtl-ct28 + c_xet_duyet_dtl-ct29
                         + c_xet_duyet_dtl-ct34 + c_xet_duyet_dtl-ct36
                         + c_xet_duyet_dtl-ct38 + c_xet_duyet_dtl-ct39 + c_xet_duyet_dtl-ct39a.


    c_xet_duyet_dtl-ct401 = - c_xet_duyet_dtl-ct40 + c_xet_duyet_dtl-ct40a
                            + c_xet_duyet_dtl-ct40c - c_xet_duyet_dtl-ct40d
                            + c_xet_duyet_dtl-ct40e + c_xet_duyet_dtl-ct40f.


*                            - c_xet_duyet_dtl-ct26 - c_xet_duyet_dtl-ct28
*                            - c_xet_duyet_dtl-ct29 - c_xet_duyet_dtl-ct292 - c_xet_duyet_dtl-ct34
*                            - c_xet_duyet_dtl-ct36 - c_xet_duyet_dtl-ct38 - c_xet_duyet_dtl-ct39
*                            - c_xet_duyet_dtl-ct40 .
    c_xet_duyet_dtl-ct53 = c_xet_duyet_dtl-ct32 + c_xet_duyet_dtl-ct401 .

    c_xet_duyet_dtl-ct531 = c_xet_duyet_dtl-ct23 + c_xet_duyet_dtl-ct24  - c_xet_duyet_dtl-ct52 - c_xet_duyet_dtl-ct53 .

    c_xet_duyet_dtl-ct51 =   c_xet_duyet_dtl-ct531 + c_xet_duyet_dtl-ct40b.

  ENDMETHOD.


  METHOD update_bb_gc.
    DATA : lt_bb_gc    TYPE STANDARD TABLE OF ztb_bb_gc,
           ls_bb_gc    TYPE                   ztb_bb_gc,
           ls_bb_gc_db TYPE                   ztb_bb_gc,
           ls_bb_gc_ud TYPE                   ztb_bb_gc.

    DATA : lt_gc_loi TYPE STANDARD TABLE OF ztb_gc_loi,
           ls_gc_loi TYPE                   ztb_gc_loi.

    SELECT SINGLE * FROM ztb_bb_gc WHERE hdr_id = @i_bb_gc-hdr_id
    INTO @ls_bb_gc.
    CHECK sy-subrc IS INITIAL.

    SELECT * FROM ztb_gc_loi
        WHERE hdr_id = @ls_bb_gc-hdr_id
        INTO TABLE @lt_gc_loi.

    CLEAR: ls_bb_gc-ct14, ls_bb_gc-Ct14,  ls_bb_gc-Ct18,  ls_bb_gc-Ct19,  ls_bb_gc-Ct20,  ls_bb_gc-Ct21,  ls_bb_gc-Ct22,
             ls_bb_gc-Ct24,  ls_bb_gc-Ct25,  ls_bb_gc-Ct26,  ls_bb_gc-Ct27,  ls_bb_gc-Ct28,  ls_bb_gc-Ct29,  ls_bb_gc-Ct30,
             ls_bb_gc-Ct31,  ls_bb_gc-Ct32,  ls_bb_gc-Ct40,  ls_bb_gc-Ct47,  ls_bb_gc-Ct48.
    LOOP AT lt_gc_loi ASSIGNING FIELD-SYMBOL(<lf_gc_loi>).
      CLEAR: <lf_gc_loi>-tile, <lf_gc_loi>-check_bangi, <lf_gc_loi>-check_bangii.
      IF <lf_gc_loi>-loai_loi = ''.
        <lf_gc_loi>-loai_loi = 'C'.
      ENDIF.
      IF ls_bb_gc-ct13 <> 0.
        <lf_gc_loi>-tile = <lf_gc_loi>-sl_loi / ls_bb_gc-ct13 * 100.
      ENDIF.

      IF <lf_gc_loi>-loai_loi <> 'C'.
        IF <lf_gc_loi>-tile > <lf_gc_loi>-bangi .
          <lf_gc_loi>-check_bangi = 'V'.
        ENDIF.

        IF <lf_gc_loi>-tile > <lf_gc_loi>-bangii .
          <lf_gc_loi>-check_bangii = 'V'.
        ENDIF.
      ENDIF.

      IF <lf_gc_loi>-loai_loi = 'C'.
        ls_bb_gc-ct25 = ls_bb_gc-ct25 + <lf_gc_loi>-sl_loi.
      ENDIF.

      IF <lf_gc_loi>-check_bangi = 'V'.
        ls_bb_gc-ct27 = ls_bb_gc-ct27 + <lf_gc_loi>-sl_loi.
      ENDIF.

      IF <lf_gc_loi>-check_bangii = 'V' AND <lf_gc_loi>-loai_loi = 'B'.
        ls_bb_gc-ct29 = ls_bb_gc-ct29 + <lf_gc_loi>-sl_loi.
      ENDIF.

      IF <lf_gc_loi>-check_bangii = 'V' AND <lf_gc_loi>-loai_loi = 'A'.
        ls_bb_gc-ct31 = ls_bb_gc-ct31 + <lf_gc_loi>-sl_loi.
      ENDIF.

    ENDLOOP.

    ls_bb_gc-ct24 = ls_bb_gc-ct13 - ls_bb_gc-ct27 - ls_bb_gc-ct25 .
    IF ls_bb_gc-ct12 IS NOT INITIAL.
      ls_bb_gc-ct14 = ls_bb_gc-ct13 / ls_bb_gc-ct12  * 100.
    ENDIF.


    IF ls_bb_gc-ct13 IS NOT INITIAL.
      ls_bb_gc-ct26 = ls_bb_gc-ct25 / ls_bb_gc-ct13 * 100.
      ls_bb_gc-ct28 = ls_bb_gc-ct27 / ls_bb_gc-ct13  * 100.
      ls_bb_gc-ct30 = ls_bb_gc-ct29 / ls_bb_gc-ct13  * 100.
      ls_bb_gc-ct32 = ls_bb_gc-ct31 / ls_bb_gc-ct13  * 100.
    ENDIF.

    IF ls_bb_gc-ct26 > 10.
      ls_bb_gc-ct22 =  (  ls_bb_gc-ct26 - 10  ) * ls_bb_gc-ct23 / 100.
    ENDIF.

    ls_bb_gc-ct19 = ls_bb_gc-ct23 * ls_bb_gc-ct28 / 100.
    ls_bb_gc-ct20 = ls_bb_gc-ct23 * ls_bb_gc-ct30 / 100.
    ls_bb_gc-ct21 = ls_bb_gc-ct23 * ls_bb_gc-ct32 / 100.

    ls_bb_gc-ct18 = ls_bb_gc-ct23 - ls_bb_gc-ct19 - ls_bb_gc-ct22.

    ls_bb_gc-ct40 = ls_bb_gc-ct33 + ls_bb_gc-ct34 + ls_bb_gc-ct35 + ls_bb_gc-ct36 +
                     ls_bb_gc-ct37 + ls_bb_gc-ct38 + ls_bb_gc-ct39.

    ls_bb_gc-ct47 = ls_bb_gc-ct41 + ls_bb_gc-ct42 + ls_bb_gc-ct43 + ls_bb_gc-ct44 +
                     ls_bb_gc-ct45 + ls_bb_gc-ct46 .

    ls_bb_gc-ct48 = ls_bb_gc-ct40 + ls_bb_gc-ct47.

    MODIFY ztb_bb_gc FROM @ls_bb_gc.
    MODIFY ztb_gc_loi FROM TABLE @lt_gc_loi.
  ENDMETHOD.


  METHOD get_vobao.
    SELECT MaterialDocumentYear, MaterialDocument, MaterialDocumentItem,  OrderID,  PurchaseOrder, PurchaseOrderItem, GoodsMovementType
          ,Supplier, mdi~Material AS Material,plant,
         InventorySpecialStockType, QuantityInBaseUnit, MaterialBaseUnit, isautomaticallycreated, YY1_LNhapTra_MMI, YY1_LenhGiaCong_MMI
   FROM zc_materialdocumentitem_2 AS mdi
   INNER JOIN ztb_mat_type AS mt ON mdi~Material = mt~Material AND mt~mattype = '1'
    WHERE PostingDate <= @i_date_to
    AND PostingDate >= @i_date_fr
    AND InventorySpecialStockType = 'O'
    AND reversedmaterialdocument = ''
    AND GoodsMovementType IN ('541','542')
    AND Supplier = @i_xet_duyet_dtl-supplier
    INTO TABLE @DATA(lt_MATERIALDOCUMENTITEM).
    SORT lt_MATERIALDOCUMENTITEM BY MaterialDocumentYear MaterialDocument MaterialDocumentItem.
    IF lt_MATERIALDOCUMENTITEM IS NOT INITIAL.
      SELECT ReversedMaterialDocumentYear, reversedmaterialdocument, reversedmaterialdocumentitem
           FROM zc_materialdocumentitem_2 AS mdi
           FOR ALL ENTRIES IN @lt_MATERIALDOCUMENTITEM
           WHERE ReversedMaterialDocumentYear = @lt_MATERIALDOCUMENTITEM-MaterialDocumentYear
             AND reversedmaterialdocument = @lt_MATERIALDOCUMENTITEM-MaterialDocument
             AND reversedmaterialdocumentitem = @lt_MATERIALDOCUMENTITEM-MaterialDocumentItem
            INTO TABLE @DATA(lt_MATERIALDOCUMENTITEM_rv).
      LOOP AT lt_MATERIALDOCUMENTITEM_rv ASSIGNING FIELD-SYMBOL(<ls_MATERIALDOCUMENTITEM_rv>).
        DELETE lt_MATERIALDOCUMENTITEM WHERE MaterialDocumentYear = <ls_MATERIALDOCUMENTITEM_rv>-ReversedMaterialDocumentYear
          AND MaterialDocument = <ls_MATERIALDOCUMENTITEM_rv>-reversedmaterialdocument
          AND MaterialDocumentItem = <ls_MATERIALDOCUMENTITEM_rv>-reversedmaterialdocumentitem.
      ENDLOOP.
    ENDIF.
    DATA: lw_orderid TYPE zde_char20.
    LOOP AT lt_MATERIALDOCUMENTITEM ASSIGNING FIELD-SYMBOL(<ls_MATERIALDOCUMENTITEM>).

      IF <ls_MATERIALDOCUMENTITEM>-YY1_LenhGiaCong_MMI IS NOT INITIAL.
        lw_orderid = <ls_MATERIALDOCUMENTITEM>-YY1_LenhGiaCong_MMI .
      ELSEIF <ls_MATERIALDOCUMENTITEM>-OrderID IS NOT INITIAL.
        lw_orderid = <ls_MATERIALDOCUMENTITEM>-OrderID .
      ELSEIF <ls_MATERIALDOCUMENTITEM>-PurchaseOrder IS NOT INITIAL." AND <ls_materialdocumentitem>-PurchaseOrderItem IS NOT INITIAL.
        SELECT SINGLE orderid
        FROM zi_purordaccountassignmentapi
        WHERE PurchaseOrder = @<ls_MATERIALDOCUMENTITEM>-PurchaseOrder "AND purchaseOrderItem = @<ls_MATERIALDOCUMENTITEM>-PurchaseOrderItem
        INTO @lw_orderid.
      ENDIF.
      IF lw_orderid IS INITIAL.
        CONTINUE.
      ENDIF.
      SELECT SINGLE SalesOrder, SalesOrderItem FROM zc_productionorder
       WHERE ProductionOrder = @lw_orderid AND ProductionOrderType IN ( '1014', '2012' )
       INTO @DATA(ls_productionorder).
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      SELECT SINGLE * FROM ztb_dg_btp_thieu
        WHERE material = @<ls_MATERIALDOCUMENTITEM>-material
        INTO @DATA(ls_dg_btp_thieu).
      IF sy-subrc IS INITIAL.
        IF <ls_MATERIALDOCUMENTITEM>-GoodsMovementType = '541'.
          e_so_tien += <ls_MATERIALDOCUMENTITEM>-QuantityInBaseUnit * ls_dg_btp_thieu-price.
        ELSEIF <ls_MATERIALDOCUMENTITEM>-GoodsMovementType = '542'.
          e_so_tien -= <ls_MATERIALDOCUMENTITEM>-QuantityInBaseUnit * ls_dg_btp_thieu-price.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
