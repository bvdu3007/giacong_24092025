CLASS zcl_dc_giacong DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES tt_data TYPE STANDARD TABLE OF ztb_dcgc_dtl.
    CLASS-METHODS: get_data
      IMPORTING
                i_hdr   TYPE ztb_dcgc_hdr
      EXPORTING e_t_dtl TYPE tt_data
                e_hdr   TYPE ztb_dcgc_hdr
      .
    CLASS-METHODS: update_dtl
      CHANGING
        c_dtl TYPE ztb_dcgc_dtl.

    CLASS-METHODS: update_hdr
      IMPORTING
        i_t_dtl TYPE tt_data
      CHANGING
        c_hdr   TYPE ztb_dcgc_hdr.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DC_GIACONG IMPLEMENTATION.


  METHOD get_data.
    DATA: lt_dtl TYPE tt_data,
          ls_dtl LIKE LINE OF lt_dtl.

    DATA: lt_dtl_collect TYPE tt_data,
          ls_dtl_collect LIKE LINE OF lt_dtl.

    DATA: lw_dtl_cr TYPE zde_flag.

    e_hdr = i_hdr.

    SELECT SINGLE * FROM zr_tbperiod
        WHERE Zper = @i_hdr-zper
        INTO  @DATA(ls_tbperiod).
    IF sy-subrc <> 0.
      RETURN. " No records found
    ENDIF.

    SELECT SINGLE * FROM zr_tbxetduyet_hdr
        WHERE Zper = @i_hdr-zper and lan = @i_hdr-lan and Bukrs = @i_hdr-bukrs
        INTO @DATA(ls_tbxetduyet_hdr).
    IF sy-subrc IS INITIAL.
      SELECT SINGLE * FROM zr_tbxetduyet_dtl
          WHERE HdrID = @ls_tbxetduyet_hdr-HdrID
          AND Supplier = @i_hdr-supplier
          INTO @DATA(ls_tbxetduyet_dtl).
    ENDIF.

*    e_hdr-ct01 = ls_tbxetduyet_dtl-Ct38.
    e_hdr-ct02 = ls_tbxetduyet_dtl-Ct32.
    e_hdr-ct03 = ls_tbxetduyet_dtl-Ct34.

*    e_hdr-ct011 = ls_tbxetduyet_dtl-Ct501.
    e_hdr-ct021 = ls_tbxetduyet_dtl-Ct481.
    e_hdr-ct031 = ls_tbxetduyet_dtl-Ct491.

    e_hdr-ct03a = ls_tbxetduyet_dtl-Ct36.
    e_hdr-ct03a1 = ls_tbxetduyet_dtl-Ct451.
    e_hdr-ct03b = ls_tbxetduyet_dtl-Ct39.
    e_hdr-ct03b1 = ls_tbxetduyet_dtl-Ct431.

    e_hdr-ct04 = ls_tbxetduyet_dtl-ct40a.
    e_hdr-ct05 = ls_tbxetduyet_dtl-ct40c.
    e_hdr-ct06 = ls_tbxetduyet_dtl-ct40e + ls_tbxetduyet_dtl-ct40f.
    e_hdr-ct07 = ls_tbxetduyet_dtl-ct40." + ls_tbxetduyet_dtl-ct40e + ls_tbxetduyet_dtl-ct40f .
    e_hdr-ct08 = ls_tbxetduyet_dtl-ct40b.

    CLEAR: e_hdr-ct12.
    SELECT * FROM ztb_xuat_hd
        WHERE bukrs = @i_hdr-bukrs
        AND supplier = @i_hdr-supplier
        AND zper = @i_hdr-zper
        AND trangthai = '2'
        AND lan < @i_hdr-lan
        INTO TABLE @DATA(lt_xuat_hd).
    LOOP AT lt_xuat_hd INTO DATA(ls_xuat_hd).
      e_hdr-ct12 += ls_xuat_hd-tongtienht.
    ENDLOOP.
    SELECT * FROM ztb_xuat_hd
        WHERE bukrs = @i_hdr-bukrs
        AND supplier = @i_hdr-supplier
        AND zper = @ls_tbperiod-LastPer
        AND trangthai = '2'
        INTO TABLE @DATA(lt_xuat_hd_last).
    IF sy-subrc = 0.
      APPEND LINES OF lt_xuat_hd_last TO lt_xuat_hd.
      SORT lt_xuat_hd_last BY lan DESCENDING.
      READ TABLE lt_xuat_hd_last INTO ls_xuat_hd INDEX 1.
      e_hdr-ct10 += ls_xuat_hd-tongtienht.
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

      IF ls_penalty_price_1-ErrorCode = '10_01'.
        DATA(ls_penalty_price_1_10_01) = ls_penalty_price_1.
      ENDIF.

      IF ls_penalty_price_1-ErrorCode = '11_01'.
        DATA(ls_penalty_price_1_11_01) = ls_penalty_price_1.
      ENDIF.

      IF ls_penalty_price_1-ErrorCode = '04_01'.
        DATA(ls_penalty_price_1_04_01) = ls_penalty_price_1.
      ENDIF.
    ENDLOOP.

    SELECT * FROM zi_error_rate_rp
        INTO TABLE @DATA(lt_error_rate_rp).
    SORT lt_error_rate_rp BY ErrorCode ErrorRateFrom DESCENDING.

    READ TABLE  lt_error_rate_rp INTO DATA(ls_error_rate_rp_11_01)
        WITH KEY ErrorCode = '11_01'.

    READ TABLE  lt_error_rate_rp INTO DATA(ls_error_rate_rp_10_01)
        WITH KEY ErrorCode = '10_01'.

    SELECT * FROM zr_tbbb_gc
        WHERE CompanyCode = @i_hdr-bukrs AND NgayLapBb >= @ls_tbperiod-zdatefr AND ngayLapBb <= @ls_tbperiod-zdateto
        AND Supplier = @i_hdr-supplier
        INTO TABLE @DATA(lt_bb_gc).

    SELECT * FROM ztb_dcgc_dtl
    WHERE hdr_id = @i_hdr-hdr_id
    INTO TABLE @DATA(lt_dtl_db).
    SORT lt_dtl_db BY sobbgc.

    DATA lt_so_po TYPE TABLE OF ztb_bb_gc-so_po.

    SORT lt_bb_gc BY SoBb.
    LOOP AT lt_bb_gc INTO DATA(ls_bb_gc).

      CLEAR: lw_dtl_cr, ls_dtl, ls_dtl_collect.
      READ TABLE lt_dtl_db
      WITH KEY sobbgc =  ls_bb_gc-SoBb
      INTO DATA(ls_dtl_db).
      IF sy-subrc IS NOT INITIAL.

        TRY.
            DATA(lv_uuid) = cl_system_uuid=>create_uuid_x16_static( ).
          CATCH cx_uuid_error INTO DATA(lx_uuid).

        ENDTRY.
        ls_dtl-dtl_id = lv_uuid.
        lw_dtl_cr = 'X'.
      ELSE.
        ls_dtl-dtl_id = ls_dtl_db-dtl_id.
      ENDIF.
      ls_dtl-hdr_id = i_hdr-hdr_id.
      ls_dtl-ngaynhaphang = ls_bb_gc-NgayNhapHang.
      ls_dtl-sobbgc =  ls_bb_gc-SoBb.
      ls_dtl-SoPo = ls_bb_gc-SoPo.
      ls_dtl-ct09 = ls_bb_gc-ct18.

      ls_dtl-ct10 = ls_bb_gc-ct19.
      ls_dtl-ct11 = ls_bb_gc-ct20.
      ls_dtl-ct12 = ls_bb_gc-ct21.
      ls_dtl-ct13 = ls_bb_gc-ct22.
      ls_dtl-ct14 = ls_bb_gc-ct23 * ls_bb_gc-ct26 / 100.
      ls_dtl-ct15 = ls_dtl-ct09 + ls_dtl-ct13.
      ls_dtl-ct16 = ls_bb_gc-ct23.

      SELECT SUM( NetPriceAmount ) AS NetPriceAmount  FROM ZI_PurchaseOrderItemAPI01
         WHERE PurchaseOrder = @ls_bb_gc-SoPo AND Material = ''
         INTO @DATA(lw_NetPriceAmount).
      IF sy-subrc = 0.
        ls_dtl-ct17 = lw_NetPriceAmount  * 100.
      ENDIF.

*      IF ls_bb_gc-Ct321 = 0.
      ls_dtl-ct18 = ls_dtl-ct17 * ls_dtl-ct16.
*      ELSE.
*        ls_dtl-ct18 = 0.
*      ENDIF.
*      ls_dtl-ct19 = ?.
      ls_dtl-ct20 = ls_tbxetduyet_dtl-ct461.
      ls_dtl-ct21 = ls_dtl-ct20 * ls_dtl-ct11 * ls_penalty_price_1_02_01-PenaltyPrice / 100.
      ls_dtl-ct22 = ls_tbxetduyet_dtl-ct471.
      ls_dtl-ct23 = ls_dtl-ct22 * ls_dtl-ct12 * ls_penalty_price_1_02_02-PenaltyPrice / 100.

      ls_dtl-ct24 = ls_dtl-ct13 * ls_penalty_price_1_03_01-PenaltyPrice * ls_tbxetduyet_dtl-Ct421 / 100.

      SELECT SUM( NetPriceAmount ) AS NetPriceAmount  FROM ZI_PurchaseOrderItemAPI01
        WHERE PurchaseOrder = @ls_bb_gc-SoPo AND Material <> ''
        INTO @lw_NetPriceAmount.
      IF sy-subrc IS INITIAL.
        ls_dtl-ct25 = lw_NetPriceAmount * 100.
      ENDIF.
*      IF ls_bb_gc-Ct321 = 0.
      ls_dtl-ct26 = ls_dtl-ct25 * ls_bb_gc-ct23.
*      ELSE.
*        ls_dtl-ct26 = 0.
*      ENDIF.

      ls_dtl-ct29 = ls_bb_gc-ct47 * ls_penalty_price_1_08_01-PenaltyPrice * ls_tbxetduyet_dtl-ct501 / 100.
      ls_dtl-ct30 = ls_bb_gc-Ct322 * ls_penalty_price_1_06_01-PenaltyPrice * ls_tbxetduyet_dtl-ct451 / 100.
      ls_dtl-ct31 = ls_bb_gc-Ct321 * ls_penalty_price_1_09_01-PenaltyPrice * ls_tbxetduyet_dtl-ct491 / 100.
      ls_dtl-ct32 = ls_bb_gc-Ct323 * ls_penalty_price_1_04_01-PenaltyPrice * ls_tbxetduyet_dtl-ct431 / 100.

      IF ls_bb_gc-ct16 <> 0 .
        IF ls_bb_gc-LoaiHang = '1'.
          ls_dtl-ct27 += ls_bb_gc-ct16 * ls_error_rate_rp_10_01-DeductionPercent * ls_penalty_price_1_10_01-PenaltyPrice / 100.
        ELSE.
          ls_dtl-ct27 += ls_bb_gc-ct16 * ls_error_rate_rp_11_01-DeductionPercent * ls_penalty_price_1_11_01-PenaltyPrice / 100.
        ENDIF.
      ENDIF.

      ls_dtl_collect-sopo = ls_bb_gc-SoPo.
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
          ls_dtl_collect-ct27 += lw_PURORDAMOUNT * 100.
        ENDIF.
      ENDIF.

      ls_dtl_collect-ct27 -= ls_dtl-ct17 * ls_dtl-ct16.
      COLLECT ls_dtl_collect INTO lt_dtl_collect.

      IF ls_bb_gc-ct47 >= 1000.
        DATA(lw_ct13) = ls_bb_gc-ct47.
      ENDIF.

      lw_ct13 = lw_ct13 + ls_bb_gc-Ct321.
      ls_dtl-ct33 = lw_ct13 * (  ls_dtl-ct17 + ls_dtl-ct25 ) * ls_tbxetduyet_dtl-Ct441 / 100.
      CLEAR: lw_ct13.

      CALL METHOD update_dtl
        CHANGING
          c_dtl = ls_dtl.
      APPEND ls_dtl TO lt_dtl.
    ENDLOOP.

    LOOP AT lt_dtl_collect INTO ls_dtl_collect.
      READ TABLE lt_dtl ASSIGNING FIELD-SYMBOL(<ls_dtl_exist>) WITH KEY sopo = ls_dtl_collect-sopo.
      IF sy-subrc IS INITIAL.
        <ls_dtl_exist>-ct27 = <ls_dtl_exist>-ct27 + ls_dtl_collect-ct27.
        CALL METHOD update_dtl
          CHANGING
            c_dtl = <ls_dtl_exist>.
      ENDIF.
    ENDLOOP.
    e_t_dtl = lt_dtl.
  ENDMETHOD.


  METHOD update_dtl.
    c_dtl-ct28 = c_dtl-ct18 + c_dtl-ct26 + c_dtl-ct27 - c_dtl-ct21
                - c_dtl-ct23 - c_dtl-ct24  - c_dtl-ct29  - c_dtl-ct30  - c_dtl-ct31  - c_dtl-ct32  - c_dtl-ct33.
  ENDMETHOD.


  METHOD update_hdr.
    CLEAR c_hdr-ct09.
    LOOP AT i_t_dtl INTO DATA(ls_dtl).
      c_hdr-ct09 += ls_dtl-ct28.
    ENDLOOP.

    c_hdr-ct09 = c_hdr-ct09 - c_hdr-ct02 - c_hdr-ct04 - c_hdr-ct05 - c_hdr-ct06 + c_hdr-ct07 + c_hdr-ct08.
    c_hdr-ct11 = c_hdr-ct09 + c_hdr-ct10.
    c_hdr-ct13 = c_hdr-ct11 - c_hdr-ct12.

  ENDMETHOD.
ENDCLASS.
