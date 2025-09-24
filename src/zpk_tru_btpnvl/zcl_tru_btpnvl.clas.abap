CLASS zcl_tru_btpnvl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES tt_tru_thie_dtl TYPE STANDARD TABLE OF ztb_tru_thie_dtl.
    CLASS-METHODS: get_tru_thieu
      IMPORTING
                i_tru_thieu    TYPE ztb_tru_thieu
      EXPORTING e_tru_thie_dtl TYPE tt_tru_thie_dtl.

    CLASS-METHODS: update_tru_thieu_dtl
      CHANGING
        c_tru_thie_dtl TYPE ztb_tru_thie_dtl.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TRU_BTPNVL IMPLEMENTATION.


  METHOD get_tru_thieu.
    DATA: ls_tru_thie_dtl TYPE ztb_tru_thie_dtl,
          lt_tru_thie_dtl TYPE TABLE OF ztb_tru_thie_dtl.
    DATA: lw_dtl_cr TYPE zde_flag.
    DATA: lw_lenhsx TYPE zde_char20.

    SELECT * FROM ztb_dg_btp_thieu
    INTO TABLE @DATA(lt_dg_btp_thieu).

    SELECT SINGLE * FROM zr_tbperiod
        WHERE Zper = @i_tru_thieu-zper
        INTO  @DATA(ls_tbperiod).
    IF sy-subrc <> 0.
      RETURN. " No records found
    ENDIF.

    SELECT * FROM ztb_tru_thie_dtl
    WHERE hdr_id = @i_tru_thieu-hdr_id
    INTO TABLE @DATA(lt_dtl_db).
    SORT lt_dtl_db BY material supplier.

    SELECT MaterialDocumentYear, MaterialDocument, MaterialDocumentItem,  OrderID,  PurchaseOrder, PurchaseOrderItem, GoodsMovementType,Supplier, Material,plant,
           InventorySpecialStockType, QuantityInBaseUnit, MaterialBaseUnit, isautomaticallycreated, YY1_LNhapTra_MMI, YY1_LenhGiaCong_MMI
     FROM zc_materialdocumentitem_2 AS mdi
      WHERE PostingDate <= @ls_tbperiod-zdateto
      AND PostingDate >= @ls_tbperiod-zdatefr
*      AND InventorySpecialStockType = 'O'
      AND reversedmaterialdocument = ''
      AND GoodsMovementType IN ('X43','Y21','543')
      AND CompanyCode = @i_tru_thieu-bukrs
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

    LOOP AT lt_MATERIALDOCUMENTITEM ASSIGNING FIELD-SYMBOL(<ls_MATERIALDOCUMENTITEM>).
      IF <ls_MATERIALDOCUMENTITEM>-YY1_LenhGiaCong_MMI IS NOT INITIAL.
        lw_lenhsx = <ls_MATERIALDOCUMENTITEM>-YY1_LenhGiaCong_MMI .
      ELSEIF <ls_MATERIALDOCUMENTITEM>-OrderID IS NOT INITIAL.
        lw_lenhsx = <ls_MATERIALDOCUMENTITEM>-OrderID .
      ELSEIF <ls_MATERIALDOCUMENTITEM>-PurchaseOrder IS NOT INITIAL." AND <ls_materialdocumentitem>-PurchaseOrderItem IS NOT INITIAL.
        SELECT SINGLE orderid
        FROM zi_purordaccountassignment
        WHERE PurchaseOrder = @<ls_MATERIALDOCUMENTITEM>-PurchaseOrder "AND purchaseOrderItem = @<ls_MATERIALDOCUMENTITEM>-PurchaseOrderItem
        INTO @lw_lenhsx.
      ENDIF.
      IF lw_lenhsx IS INITIAL .
        CONTINUE.
      ENDIF.
      SELECT SINGLE SalesOrder, SalesOrderItem FROM zc_productionorder
         WHERE ProductionOrder = @lw_lenhsx AND ProductionOrderType IN ( '1014', '2012' )
         INTO @DATA(ls_productionorder).
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE lt_tru_thie_dtl ASSIGNING FIELD-SYMBOL(<lf_tru_thie_dtl>)
      WITH KEY supplier = <ls_MATERIALDOCUMENTITEM>-Supplier
               material = <ls_MATERIALDOCUMENTITEM>-Material.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO lt_tru_thie_dtl ASSIGNING <lf_tru_thie_dtl>.
        <lf_tru_thie_dtl>-material = <ls_MATERIALDOCUMENTITEM>-Material.
        <lf_tru_thie_dtl>-supplier = <ls_MATERIALDOCUMENTITEM>-Supplier.
        <lf_tru_thie_dtl>-materialbaseunit = <ls_MATERIALDOCUMENTITEM>-materialbaseunit.
      ENDIF.
      IF <ls_MATERIALDOCUMENTITEM>-GoodsMovementType = 'X43' AND <ls_MATERIALDOCUMENTITEM>-InventorySpecialStockType = 'O'.
        <lf_tru_thie_dtl>-thieu += <ls_MATERIALDOCUMENTITEM>-QuantityInBaseUnit.
      ELSEIF <ls_MATERIALDOCUMENTITEM>-GoodsMovementType = 'Y21'.
        <lf_tru_thie_dtl>-thua += <ls_MATERIALDOCUMENTITEM>-QuantityInBaseUnit.
      ELSEIF <ls_MATERIALDOCUMENTITEM>-GoodsMovementType = '543'  AND <ls_MATERIALDOCUMENTITEM>-InventorySpecialStockType = 'O'.
        <lf_tru_thie_dtl>-nhap += <ls_MATERIALDOCUMENTITEM>-QuantityInBaseUnit.
      ENDIF.
    ENDLOOP.

    IF lt_tru_thie_dtl IS NOT INITIAL.
      SELECT Product, ProductGroup FROM zc_Product
          FOR ALL ENTRIES IN @lt_tru_thie_dtl
          WHERE Product = @lt_tru_thie_dtl-material
          INTO TABLE @DATA(lt_Product).
      SORT lt_Product BY Product.
    ENDIF.

    LOOP AT lt_tru_thie_dtl ASSIGNING <lf_tru_thie_dtl>.
      READ TABLE lt_dtl_db INTO DATA(ls_dtl_db)
      WITH KEY material = <lf_tru_thie_dtl>-material
               supplier = <lf_tru_thie_dtl>-supplier.
      IF sy-subrc IS INITIAL.
        <lf_tru_thie_dtl>-hdr_id = ls_dtl_db-hdr_id.
        <lf_tru_thie_dtl>-dtl_id = ls_dtl_db-dtl_id.
        <lf_tru_thie_dtl>-duocphep = ls_dtl_db-duocphep.
      ELSE.
        TRY.
            DATA(lv_uuid) = cl_system_uuid=>create_uuid_x16_static( ).
          CATCH cx_uuid_error INTO DATA(lx_uuid).

        ENDTRY.
        <lf_tru_thie_dtl>-hdr_id = i_tru_thieu-hdr_id.
        <lf_tru_thie_dtl>-dtl_id = lv_uuid.
        <lf_tru_thie_dtl>-duocphep = 5 / 100.
      ENDIF.

      DATA: lv_result TYPE abap_bool.
      READ TABLE lt_dg_btp_thieu
          INTO DATA(ls_dg_btp_thieu)
          WITH KEY material = <lf_tru_thie_dtl>-material.
      IF sy-subrc IS INITIAL.
        <lf_tru_thie_dtl>-dongiatru = ls_dg_btp_thieu-price.
      ELSE.
        READ TABLE lt_Product INTO DATA(ls_Product)
            WITH KEY Product = <lf_tru_thie_dtl>-material.
        IF sy-subrc IS INITIAL.
          SELECT * FROM zc_product_charact
            WHERE ClfnObjectID = @<lf_tru_thie_dtl>-material
            INTO TABLE @DATA(lt_product_charact).
          LOOP AT lt_product_charact INTO DATA(ls_product_charact).
            LOOP AT lt_dg_btp_thieu
              INTO ls_dg_btp_thieu
              WHERE material = '' AND (  productgroup = ls_Product-ProductGroup OR productgroup = '' ).
              IF ls_dg_btp_thieu-charcvalue = ls_product_charact-CharcValue.
                lv_result = 'X'.
              ELSE.
                lv_result = zcl_utility=>check_rule(
                    iv_rule  = |{ ls_dg_btp_thieu-charcvalue }|
                    iv_value = |{ ls_product_charact-CharcValue }|
                  ).
              ENDIF.
              IF lv_result = 'X'.
                <lf_tru_thie_dtl>-charcvalue = ls_product_charact-CharcValue.
                <lf_tru_thie_dtl>-dongiatru = ls_dg_btp_thieu-price.
                EXIT.
              ENDIF.
            ENDLOOP.
          ENDLOOP.
        ENDIF.
      ENDIF.

      CALL METHOD zcl_tru_btpnvl=>update_tru_thieu_dtl
        CHANGING
          c_tru_thie_dtl = <lf_tru_thie_dtl>.
    ENDLOOP.

    delete lt_tru_thie_dtl WHERE thieu = 0 AND thua = 0.
    e_tru_thie_dtl = lt_tru_thie_dtl.
  ENDMETHOD.


  METHOD update_tru_thieu_dtl.
    c_tru_thie_dtl-slduocphep = c_tru_thie_dtl-nhap * c_tru_thie_dtl-duocphep / 100.
    c_tru_thie_dtl-slduocphep = round(
              val = c_tru_thie_dtl-slduocphep
              dec = 0
            ).
    IF c_tru_thie_dtl-thua > 0.
      c_tru_thie_dtl-sltru = c_tru_thie_dtl-thua * -1.
    ELSE.
      c_tru_thie_dtl-sltru = c_tru_thie_dtl-thieu - c_tru_thie_dtl-slduocphep.
      IF c_tru_thie_dtl-sltru < 0.
        c_tru_thie_dtl-sltru = 0.
      ENDIF.
    ENDIF.
    c_tru_thie_dtl-tongtientru = c_tru_thie_dtl-sltru * c_tru_thie_dtl-dongiatru.
  ENDMETHOD.
ENDCLASS.
