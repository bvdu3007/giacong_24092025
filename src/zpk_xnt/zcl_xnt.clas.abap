CLASS zcl_xnt DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: tt_c_nxt TYPE TABLE OF zc_bc_xnt.
    TYPES: BEGIN OF ty_range_option,
             sign   TYPE c LENGTH 1,
             option TYPE c LENGTH 2,
             low    TYPE string,
             high   TYPE string,
           END OF ty_range_option,

           tt_ranges TYPE TABLE OF ty_range_option.


    CLASS-METHODS: get_xnt_ps
      IMPORTING
                i_datefr    TYPE zde_date
                i_dateto    TYPE zde_date
                ir_material TYPE tt_ranges OPTIONAL
                ir_plant    TYPE tt_ranges OPTIONAL
                ir_supplier TYPE tt_ranges OPTIONAL
                ir_orderid  TYPE tt_ranges OPTIONAL
      EXPORTING e_nxt       TYPE tt_c_nxt.

    CLASS-METHODS: get_xnt
      IMPORTING
                i_datefr    TYPE zde_date
                i_dateto    TYPE zde_date
                ir_material TYPE tt_ranges OPTIONAL
                ir_plant    TYPE tt_ranges OPTIONAL
                ir_supplier TYPE tt_ranges OPTIONAL
                ir_orderid  TYPE tt_ranges OPTIONAL
      EXPORTING e_nxt       TYPE tt_c_nxt.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_XNT IMPLEMENTATION.


  METHOD get_xnt_ps.
    DATA: ls_nxt TYPE zc_bc_xnt.
    SELECT MaterialDocumentYear, MaterialDocument, MaterialDocumentItem,  OrderID,  PurchaseOrder, PurchaseOrderItem, GoodsMovementType,Supplier, Material,plant,
           InventorySpecialStockType, QuantityInBaseUnit, MaterialBaseUnit, isautomaticallycreated, YY1_LNhapTra_MMI, YY1_LenhGiaCong_MMI
     FROM zc_materialdocumentitem_2 AS mdi
      WHERE PostingDate <= @i_dateto
      AND PostingDate >= @i_datefr
      AND InventorySpecialStockType = 'O'
*      AND isautomaticallycreated = 'X'
*      AND reversedmaterialdocumentitem = ''
      AND reversedmaterialdocument = ''
      AND GoodsMovementType IN ('541','542','543','X43')
      AND Material IN @ir_material
      AND plant IN @ir_plant
      AND Supplier IN @ir_supplier
      INTO TABLE @DATA(lt_MATERIALDOCUMENTITEM).
    SORT lt_MATERIALDOCUMENTITEM BY MaterialDocumentYear MaterialDocument MaterialDocumentItem.
    IF lt_MATERIALDOCUMENTITEM IS NOT INITIAL.
      SELECT ReversedMaterialDocumentYear, reversedmaterialdocument, reversedmaterialdocumentitem
           FROM i_materialdocumentitem_2 AS mdi
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
      CLEAR ls_nxt.
      IF <ls_MATERIALDOCUMENTITEM>-YY1_LenhGiaCong_MMI IS NOT INITIAL.
        ls_nxt-orderid = <ls_MATERIALDOCUMENTITEM>-YY1_LenhGiaCong_MMI .
      ELSEIF <ls_MATERIALDOCUMENTITEM>-OrderID IS NOT INITIAL.
        ls_nxt-orderid = <ls_MATERIALDOCUMENTITEM>-OrderID .
      ELSEIF <ls_MATERIALDOCUMENTITEM>-PurchaseOrder IS NOT INITIAL." AND <ls_materialdocumentitem>-PurchaseOrderItem IS NOT INITIAL.
        SELECT SINGLE orderid
        FROM i_purordaccountassignmentapi01
        WHERE PurchaseOrder = @<ls_MATERIALDOCUMENTITEM>-PurchaseOrder "AND purchaseOrderItem = @<ls_MATERIALDOCUMENTITEM>-PurchaseOrderItem
        INTO @ls_nxt-orderid.
      ENDIF.
      IF ls_nxt-orderid IS INITIAL OR ls_nxt-orderid NOT IN ir_orderid.
        CONTINUE.
      ENDIF.
      SELECT SINGLE SalesOrder, SalesOrderItem FROM i_productionorder
       WHERE ProductionOrder = @ls_nxt-orderid AND ProductionOrderType IN ( '1014', '2012' )
       INTO @DATA(ls_productionorder).
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE e_nxt ASSIGNING FIELD-SYMBOL(<ls_nxt>)
        WITH KEY orderid = ls_nxt-orderid
                   supplier = <ls_MATERIALDOCUMENTITEM>-Supplier
                material = <ls_MATERIALDOCUMENTITEM>-Material
                plant    = <ls_MATERIALDOCUMENTITEM>-plant.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO e_nxt ASSIGNING <ls_nxt>.
        <ls_nxt>-orderid = ls_nxt-orderid.
        <ls_nxt>-material = <ls_MATERIALDOCUMENTITEM>-Material.
        <ls_nxt>-supplier = <ls_MATERIALDOCUMENTITEM>-Supplier.
        <ls_nxt>-plant = <ls_MATERIALDOCUMENTITEM>-plant.
        <ls_nxt>-MaterialBaseUnit = <ls_MATERIALDOCUMENTITEM>-MaterialBaseUnit.

        IF ls_productionorder-SalesOrder IS NOT INITIAL.
          <ls_nxt>-SalesOrder = ls_productionorder-SalesOrder && '/' && ls_productionorder-SalesOrderItem.
        ENDIF.

        SELECT SINGLE OrderIsTechnicallyCompleted
            FROM I_MfgOrderWithStatus
            WHERE  manufacturingorder = @ls_nxt-orderid
            INTO @DATA(lv_OrderIsTechnicallyCompleted).
        IF sy-subrc = 0 AND lv_OrderIsTechnicallyCompleted = 'X'.
          <ls_nxt>-DonHangVet = 'X'.
        ENDIF.
      ENDIF.
      IF <ls_MATERIALDOCUMENTITEM>-GoodsMovementType = '541'.
        <ls_nxt>-XuatTKy = <ls_nxt>-XuatTKy + <ls_MATERIALDOCUMENTITEM>-QuantityInBaseUnit.
      ELSEIF <ls_MATERIALDOCUMENTITEM>-GoodsMovementType = '543'.
        <ls_nxt>-SLBTPNVLDaNhapVe = <ls_nxt>-SLBTPNVLDaNhapVe + <ls_MATERIALDOCUMENTITEM>-QuantityInBaseUnit.
      ELSEIF <ls_MATERIALDOCUMENTITEM>-GoodsMovementType = '542'.
        IF <ls_MATERIALDOCUMENTITEM>-YY1_LNhapTra_MMI = '1'.
          <ls_nxt>-NhapTraBTPDat = <ls_nxt>-NhapTraBTPDat + <ls_MATERIALDOCUMENTITEM>-QuantityInBaseUnit.
        ELSEIF <ls_MATERIALDOCUMENTITEM>-YY1_LNhapTra_MMI = '2'.
          <ls_nxt>-NhapTraBTPLoiCTy = <ls_nxt>-NhapTraBTPLoiCTy + <ls_MATERIALDOCUMENTITEM>-QuantityInBaseUnit.
        ELSEIF <ls_MATERIALDOCUMENTITEM>-YY1_LNhapTra_MMI = '3'.
          <ls_nxt>-NhapTraBTPLoiGC = <ls_nxt>-NhapTraBTPLoiGC + <ls_MATERIALDOCUMENTITEM>-QuantityInBaseUnit.
        ENDIF.
      ELSEIF <ls_MATERIALDOCUMENTITEM>-GoodsMovementType = 'X43'.
        <ls_nxt>-NhapTruBTPThieu = <ls_nxt>-NhapTruBTPThieu + <ls_MATERIALDOCUMENTITEM>-QuantityInBaseUnit.
      ENDIF.
    ENDLOOP.

    LOOP AT e_nxt ASSIGNING <ls_nxt>.
      <ls_nxt>-BTPLoi = <ls_nxt>-NhapTraBTPLoiCTy + <ls_nxt>-NhapTraBTPLoiGC + <ls_nxt>-NhapTruBTPThieu.
      <ls_nxt>-NhapTKy = <ls_nxt>-SLBTPNVLDaNhapVe + <ls_nxt>-NhapTraBTPDat + <ls_nxt>-BTPLoi.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_xnt.
    DATA: ls_nxt TYPE zc_bc_xnt.
    DATA: lw_ky_truoc TYPE zde_period,
          lw_ky_nay   TYPE zde_period.
    DATA: lw_datefr TYPE zde_date,
          lw_dateto TYPE zde_date.

    lw_ky_nay =  i_datefr(6).
    SELECT SINGLE * FROM ztb_period WHERE zper = @lw_ky_nay INTO @DATA(ls_ky_nay) .
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    SELECT * FROM ztb_nxt
      WHERE  zper = @ls_ky_nay-lastper
      AND Material IN @ir_material
      AND plant IN @ir_plant
      AND Supplier IN @ir_supplier
      AND orderid IN @ir_orderid
      INTO TABLE @DATA(lt_nxt_ky_truoc).
    IF i_datefr+6(2) <> '01'.
      lw_datefr = i_datefr(6) && '01'.
      lw_dateto = i_dateto - 1.
      CALL METHOD get_xnt_ps
        EXPORTING
          i_datefr    = lw_datefr
          i_dateto    = lw_dateto
          ir_material = ir_material
          ir_plant    = ir_plant
          ir_supplier = ir_supplier
          ir_orderid  = ir_orderid
        IMPORTING
          e_nxt       = DATA(lt_nxt_ps_kt).
      LOOP AT lt_nxt_ps_kt ASSIGNING FIELD-SYMBOL(<ls_nxt_ps_kt>) .
        READ TABLE lt_nxt_ky_truoc ASSIGNING  FIELD-SYMBOL(<ls_nxt_ky_truoc>)
        WITH KEY orderid = <ls_nxt_ps_kt>-orderid
                supplier = <ls_nxt_ps_kt>-supplier
              material = <ls_nxt_ps_kt>-material
              plant    = <ls_nxt_ps_kt>-plant.
        IF sy-subrc = 0.
          <ls_nxt_ky_truoc>-quantityinbaseunit = <ls_nxt_ky_truoc>-quantityinbaseunit + <ls_nxt_ps_kt>-XuatTKy - <ls_nxt_ps_kt>-NhapTKy.
        ELSE.
          APPEND INITIAL LINE TO lt_nxt_ky_truoc ASSIGNING <ls_nxt_ky_truoc>.
          <ls_nxt_ky_truoc>-orderid = <ls_nxt_ps_kt>-orderid.
          <ls_nxt_ky_truoc>-supplier = <ls_nxt_ps_kt>-supplier.
          <ls_nxt_ky_truoc>-material = <ls_nxt_ps_kt>-material.
          <ls_nxt_ky_truoc>-plant = <ls_nxt_ps_kt>-plant.
          <ls_nxt_ky_truoc>-quantityinbaseunit = <ls_nxt_ps_kt>-XuatTKy - <ls_nxt_ps_kt>-NhapTKy.
          <ls_nxt_ky_truoc>-materialbaseunit = <ls_nxt_ps_kt>-MaterialBaseUnit.
        ENDIF.
      ENDLOOP.
    ENDIF.

    lw_datefr = i_datefr.
    lw_dateto = i_dateto.
    CALL METHOD get_xnt_ps
      EXPORTING
        i_datefr    = lw_datefr
        i_dateto    = lw_dateto
        ir_material = ir_material
        ir_plant    = ir_plant
        ir_supplier = ir_supplier
        ir_orderid  = ir_orderid
      IMPORTING
        e_nxt       = DATA(lt_nxt_ps).
    LOOP AT lt_nxt_ky_truoc ASSIGNING <ls_nxt_ky_truoc>.
      READ TABLE lt_nxt_ps ASSIGNING FIELD-SYMBOL(<ls_nxt_ps>)
      WITH KEY orderid = <ls_nxt_ky_truoc>-orderid
              supplier = <ls_nxt_ky_truoc>-supplier
            material = <ls_nxt_ky_truoc>-material.
      IF sy-subrc = 0.
        <ls_nxt_ps>-DauKy = <ls_nxt_ps>-DauKy + <ls_nxt_ky_truoc>-quantityinbaseunit.
      ELSE.
        APPEND INITIAL LINE TO lt_nxt_ps ASSIGNING <ls_nxt_ps>.
        <ls_nxt_ps>-orderid = <ls_nxt_ky_truoc>-orderid.
        <ls_nxt_ps>-supplier = <ls_nxt_ky_truoc>-supplier.
        <ls_nxt_ps>-material = <ls_nxt_ky_truoc>-material.
        <ls_nxt_ps>-plant = <ls_nxt_ky_truoc>-plant.
        <ls_nxt_ps>-DauKy = <ls_nxt_ky_truoc>-quantityinbaseunit.
        <ls_nxt_ps>-MaterialBaseUnit = <ls_nxt_ky_truoc>-materialbaseunit.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_nxt_ps ASSIGNING <ls_nxt_ps>.
      <ls_nxt_ps>-TonCuoi = <ls_nxt_ps>-DauKy + <ls_nxt_ps>-XuatTKy  - <ls_nxt_ps>-NhapTKy.
    ENDLOOP.

    MOVE-CORRESPONDING lt_nxt_ps TO e_nxt.

  ENDMETHOD.
ENDCLASS.
