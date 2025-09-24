CLASS zcl_xuat_hd DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: tt_xn TYPE STANDARD TABLE OF ztb_xn_xuat_hd,
           tt_ht TYPE STANDARD TABLE OF ztb_ht_hd,
           tt_pb TYPE STANDARD TABLE OF ztb_pb_hd.

    CLASS-METHODS: get_data
      IMPORTING
        i_hdr  TYPE ztb_xuat_hd
      EXPORTING
        e_t_xn TYPE tt_xn
        e_t_ht TYPE tt_ht
        e_t_pb TYPE tt_pb
        e_hdr  TYPE ztb_xuat_hd
      .

    CLASS-METHODS: update_data
      CHANGING
        ct_xn TYPE tt_xn
        ct_ht TYPE tt_ht
        ct_pb TYPE tt_pb
        c_hdr TYPE ztb_xuat_hd
      .

    CLASS-METHODS: update_data_ht
      CHANGING
        ct_xn TYPE tt_xn
        ct_ht TYPE tt_ht
        ct_pb TYPE tt_pb
        c_hdr TYPE ztb_xuat_hd
      .

    CLASS-METHODS: post_invoice_ENTITY
      IMPORTING
        i_hdr      TYPE ztb_xuat_hd
        i_t_pb     TYPE tt_pb
      EXPORTING
        e_code     TYPE zde_return_cdoe
        e_response TYPE zst_odata_return.

    CLASS-METHODS: cancel_invoice
      IMPORTING
        i_hdr      TYPE ztb_xuat_hd
      EXPORTING
        e_code     TYPE zde_return_cdoe
        e_response TYPE zst_odata_return.

    CLASS-METHODS: post_invoice_api
      IMPORTING
        i_hdr      TYPE ztb_xuat_hd
        i_t_pb     TYPE tt_pb
      EXPORTING
        e_code     TYPE zde_return_cdoe
        e_suppliervoice TYPE zr_tbxuat_hd-Supplier
        e_response TYPE zst_odata_return.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_xuat_hd IMPLEMENTATION.


  METHOD get_data.
    DATA: lt_xn TYPE tt_xn,
          ls_xn LIKE LINE OF lt_xn.
    DATA: lt_ht TYPE tt_ht,
          ls_ht LIKE LINE OF lt_ht.
    DATA: lt_pb TYPE tt_pb,
          ls_pb LIKE LINE OF lt_pb.

    DATA: lw_dtl_cr TYPE zde_flag.

    e_hdr = i_hdr.

    SELECT SINGLE * FROM zr_tbperiod
        WHERE Zper = @i_hdr-zper
        INTO  @DATA(ls_tbperiod).
    IF sy-subrc <> 0.
      RETURN. " No records found
    ENDIF.

    SELECT * FROM ztb_xuat_hd
     WHERE bukrs  = @i_hdr-bukrs
       AND zper = @i_hdr-zper
       AND lan < @i_hdr-lan
       AND supplier = @i_hdr-supplier
     INTO TABLE @DATA(lt_xuat_hd_lan_truoc).
    IF sy-subrc = 0.
      SELECT *
      FROM ztb_ht_hd
      FOR ALL ENTRIES IN @lt_xuat_hd_lan_truoc
      WHERE hdr_id = @lt_xuat_hd_lan_truoc-hdr_id
      INTO TABLE @DATA(lt_ht_lan_truoc).
    ENDIF.

    SELECT SINGLE * FROM ztb_dcgc_hdr
    WHERE bukrs = @i_hdr-bukrs
      AND zper = @i_hdr-zper
      AND lan = @i_hdr-lan
      AND supplier = @i_hdr-supplier
      INTO @DATA(ls_dcgc_hdr).
    IF sy-subrc = 0.
      SELECT * FROM ztb_dcgc_dtl
          WHERE hdr_id = @ls_dcgc_hdr-hdr_id
          INTO TABLE @DATA(lt_dcgc_dtl).
    ENDIF.

    SELECT * FROM zr_tbbb_gc
        WHERE CompanyCode = @i_hdr-bukrs AND NgayLapBb >= @ls_tbperiod-zdatefr AND ngayLapBb <= @ls_tbperiod-zdateto
        AND Supplier = @i_hdr-supplier
        INTO TABLE @DATA(lt_bb_gc).

    SELECT * FROM ztb_xn_xuat_hd
    WHERE hdr_id = @i_hdr-hdr_id
    INTO TABLE @DATA(lt_xn_hd_db).
    SORT lt_xn_hd_db BY sopo.

    e_hdr-tongtienxn = ls_dcgc_hdr-ct13.
    CLEAR e_hdr-tongtienpo.
    DATA(lt_bb_gc_tmp) = lt_bb_gc.
    SORT lt_bb_gc_tmp BY SoPo.
    DELETE ADJACENT DUPLICATES FROM lt_bb_gc COMPARING SoPo.
    LOOP AT lt_bb_gc INTO DATA(ls_bb_gc).

      CLEAR: lw_dtl_cr, ls_xn.
      READ TABLE lt_xn_hd_db
      WITH KEY sopo =  ls_bb_gc-sopo
      INTO DATA(ls_xnhd_db).
      IF sy-subrc IS NOT INITIAL.

        TRY.
            DATA(lv_uuid) = cl_system_uuid=>create_uuid_x16_static( ).
          CATCH cx_uuid_error INTO DATA(lx_uuid).

        ENDTRY.
        ls_xn-xnhd_id = lv_uuid.
        lw_dtl_cr = 'X'.
      ELSE.
        ls_xn-xnhd_id = ls_xnhd_db-xnhd_id.
      ENDIF.
      ls_xn-hdr_id = i_hdr-hdr_id.
      ls_xn-material = ls_bb_gc-Material.
      ls_xn-productdescription =  ls_bb_gc-ProductDescription.
      ls_xn-SoPo = ls_bb_gc-SoPo.
      LOOP AT lt_bb_gc_tmp INTO DATA(ls_bb_gc_tmp_1) WHERE SoPo = ls_bb_gc-SoPo.
        ls_xn-soluong += ls_bb_gc_tmp_1-ct23.
      ENDLOOP.
      LOOP AT lt_ht_lan_truoc INTO DATA(ls_ht_lan_truoc) WHERE sopo = ls_xn-sopo.
        ls_xn-soluong -= ls_ht_lan_truoc-soluong.
      ENDLOOP.

      SELECT SUM( NetPriceAmount ) AS NetPriceAmount  FROM ZI_PurchaseOrderItemAPI01
       WHERE PurchaseOrder = @ls_bb_gc-SoPo "AND Material = ''
       INTO @DATA(lw_NetPriceAmount).
      IF sy-subrc = 0.
        ls_xn-ct07 = lw_NetPriceAmount  * 100.
      ENDIF.
      ls_xn-ct08 = ls_xn-soluong * ls_xn-ct07.


      e_hdr-tongtienpo += ls_xn-ct08.
*      CALL METHOD update_dtl
*        CHANGING
*          c_dtl = ls_dtl.
      IF ls_xn-soluong > 0.
        APPEND ls_xn TO lt_xn.
      ENDIF.

    ENDLOOP.

    CALL METHOD update_data
      CHANGING
        ct_xn = lt_xn
        ct_ht = lt_ht
        ct_pb = lt_pb
        c_hdr = e_hdr.

    e_t_xn = lt_xn.
    e_t_ht = lt_ht.
    e_t_pb = lt_pb.
  ENDMETHOD.


  METHOD update_data.

    DATA: lt_xn TYPE tt_xn,
          ls_xn LIKE LINE OF lt_xn.
    DATA: lt_ht TYPE tt_ht,
          ls_ht LIKE LINE OF lt_ht.
    DATA: lt_pb TYPE tt_pb,
          ls_pb LIKE LINE OF lt_pb.

    SELECT SINGLE conditionrateratio
        FROM zI_TaxCodeRate
        WHERE TaxCode = @c_hdr-thuesuat
        INTO @DATA(lw_taxrate).

    SELECT * FROM ztb_ht_hd
    WHERE hdr_id = @c_hdr-hdr_id
    INTO TABLE @DATA(lt_ht_hd_db).
    SORT lt_ht_hd_db BY sopo.

    SELECT * FROM ztb_pb_hd
    WHERE hdr_id = @c_hdr-hdr_id
    INTO TABLE @DATA(lt_pb_hd_db).
    SORT lt_pb_hd_db BY sopo poitem.
    c_hdr-tongtienxnst = c_hdr-tongtienxn * (  1 + lw_taxrate / 100 ).

    LOOP AT ct_xn ASSIGNING FIELD-SYMBOL(<fs_xn>).
      <fs_xn>-ct11 = c_hdr-tongtienxn / c_hdr-tongtienpo * <fs_xn>-ct08.
      <fs_xn>-ct13 = <fs_xn>-ct11 * (  1 + lw_taxrate / 100 ).
      <fs_xn>-ct10 = <fs_xn>-ct11 / <fs_xn>-soluong.
*      c_hdr-tongtienxnst += <fs_xn>-ct13.
    ENDLOOP.
    ct_xn = ct_xn.
    ct_ht = lt_ht.
    ct_pb = lt_pb.
  ENDMETHOD.


  METHOD post_invoice_ENTITY.

*    DATA: ls_invoice TYPE zst_supplier_invoice_hdr.
    DATA: lw_inv_item TYPE zde_numc4.
    DATA: lw_UnplannedDeliveryCost TYPE zde_dec23_0.
    DATA: lw_taxamount TYPE zde_dec23_0.

    IF i_hdr-ngaydh IS INITIAL.
      e_code = '999'.
      e_response-error-message-value = 'Ngày hóa đơn không được để trống'.
      RETURN.
    ENDIF.

    IF i_hdr-ngayht IS INITIAL.
      e_code = '999'.
      e_response-error-message-value = 'Ngày hạch toán không được để trống'.
      RETURN.
    ENDIF.

    DATA ls_invoice TYPE STRUCTURE FOR ACTION IMPORT i_supplierinvoicetp~create.
    DATA lt_invoice TYPE TABLE FOR ACTION IMPORT i_supplierinvoicetp~create.

    " The %cid (temporary primary key) has always to be supplied (is omitted in further examples)
    TRY.
        DATA(lv_cid) = cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ).
      CATCH cx_uuid_error.
        "Error handling
    ENDTRY.

    ls_invoice-%cid                                 = lv_cid.
    ls_invoice-%param-supplierinvoiceiscreditmemo   = abap_false.
    ls_invoice-%param-companycode                   = i_hdr-bukrs.
    ls_invoice-%param-invoicingparty                = i_hdr-supplier.
    ls_invoice-%param-postingdate                   = i_hdr-ngayht.
    ls_invoice-%param-documentdate                  = i_hdr-ngaydh.
    ls_invoice-%param-documentcurrency              = 'VND'.
    ls_invoice-%param-invoicegrossamount            = i_hdr-tongtienhtst.
    ls_invoice-%param-taxiscalculatedautomatically  = abap_false.

    ls_invoice-%param-DocumentHeaderText        = `Hóa đơn gia công ` && i_hdr-zper+4(2) && '.' && i_hdr-zper(4).

    LOOP AT i_t_pb INTO DATA(ls_pb).
      lw_inv_item += 1.
      APPEND INITIAL LINE TO ls_invoice-%param-_itemswithporeference ASSIGNING FIELD-SYMBOL(<lf_item>).
      <lf_item>-supplierinvoiceitem         = lw_inv_item.
      <lf_item>-purchaseorder               = ls_pb-sopo.
      <lf_item>-purchaseorderitem           = ls_pb-poitem.
      <lf_item>-documentcurrency            = 'VND'.
      <lf_item>-supplierinvoiceitemamount   = ls_pb-ct11.
      <lf_item>-purchaseorderquantityunit   = ls_pb-materialbaseunit.
      <lf_item>-quantityinpurchaseorderunit = ls_pb-soluong.
      <lf_item>-taxcode                     = i_hdr-thuesuat.
      <lf_item>-referencedocument           = ls_pb-purchasinghistorydocument.
      <lf_item>-referencedocumentfiscalyear = ls_pb-purchasinghistorydocumentyear.
      <lf_item>-referencedocumentitem       = ls_pb-purchasinghistorydocumentitem.
    ENDLOOP.

    ls_invoice-%param-_taxes = VALUE #(
     ( taxcode           = i_hdr-thuesuat
       documentcurrency  = 'VND'
       taxamountindoccry = i_hdr-tongtienhtst - i_hdr-tongtienht )
   ).

    INSERT ls_invoice INTO TABLE lt_invoice.

    "Register the action
    MODIFY ENTITIES OF i_supplierinvoicetp
    ENTITY supplierinvoice
    EXECUTE create FROM lt_invoice
    FAILED DATA(ls_failed)
    REPORTED DATA(ls_reported)
    MAPPED DATA(ls_mapped).

    IF ls_failed IS NOT INITIAL.
      DATA lo_message TYPE REF TO if_message.
      lo_message = ls_reported-supplierinvoice[ 1 ]-%msg.
      e_code = '500'.
      e_response-error-message-value = lo_message->get_text( ).
      "Error handling
    ENDIF.

    "Execution the action
    COMMIT ENTITIES
      RESPONSE OF i_supplierinvoicetp
      FAILED DATA(ls_commit_failed)
      REPORTED DATA(ls_commit_reported).

    IF ls_commit_reported IS NOT INITIAL.
      LOOP AT ls_commit_reported-supplierinvoice ASSIGNING FIELD-SYMBOL(<ls_invoice>).
        IF <ls_invoice>-supplierinvoice IS NOT INITIAL AND
           <ls_invoice>-supplierinvoicefiscalyear IS NOT INITIAL.
          e_code = '200'.
          e_response-d-supplierinvoice = <ls_invoice>-supplierinvoice.
        ELSE.
          e_code = '500'.
          e_response-error-message-value = <ls_invoice>-%msg->if_message~get_text( ).
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF ls_commit_failed IS NOT INITIAL.
      LOOP AT ls_commit_reported-supplierinvoice ASSIGNING <ls_invoice>.
        e_code = '500'.
        e_response-error-message-value = <ls_invoice>-%msg->if_message~get_text( ).
        "Error handling
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD update_data_ht.
    DATA: lt_xn TYPE tt_xn,
          ls_xn LIKE LINE OF lt_xn.
    DATA: lt_ht TYPE tt_ht,
          ls_ht LIKE LINE OF lt_ht.
    DATA: lt_pb TYPE tt_pb,
          ls_pb LIKE LINE OF lt_pb.

    SELECT SINGLE * FROM zr_tbperiod
    WHERE Zper = @c_hdr-zper
    INTO  @DATA(ls_tbperiod).
    IF sy-subrc <> 0.
      RETURN. " No records found
    ENDIF.

    SELECT SINGLE conditionrateratio
        FROM zI_TaxCodeRate
        WHERE TaxCode = @c_hdr-thuesuat
        INTO @DATA(lw_taxrate).

    SELECT * FROM ztb_ht_hd
        WHERE hdr_id = @c_hdr-hdr_id
        INTO TABLE @DATA(lt_ht_hd_db).
    SORT lt_ht_hd_db BY sopo.

    SELECT * FROM ztb_pb_hd
        WHERE hdr_id = @c_hdr-hdr_id
        INTO TABLE @DATA(lt_pb_hd_db).
    SORT lt_pb_hd_db BY sopo poitem purchasinghistorydocumentyear purchasinghistorydocument purchasinghistorydocumentitem .
    c_hdr-tongtienhtst = 0.
    c_hdr-tongtiengr = 0.

    c_hdr-tongtienhtst += c_hdr-tongtienht * (  1 + lw_taxrate / 100 ).

    LOOP AT ct_xn ASSIGNING FIELD-SYMBOL(<fs_xn>).
      IF c_hdr-trangthai = '1'.
        MOVE-CORRESPONDING <fs_xn> TO ls_ht.
        READ TABLE lt_ht_hd_db
        WITH KEY sopo =  <fs_xn>-sopo
        INTO DATA(ls_hthd_db) BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          ls_ht-hthd_id = ls_hthd_db-hthd_id.
        ELSE.
          TRY.
              DATA(lv_uuid) = cl_system_uuid=>create_uuid_x16_static( ).
            CATCH cx_uuid_error INTO DATA(lx_uuid).

          ENDTRY.
          ls_ht-hthd_id = lv_uuid.
        ENDIF.

        ls_ht-ct11 = c_hdr-tongtienht / c_hdr-tongtienpo * ls_ht-ct08.
        ls_ht-ct13 = ls_ht-ct11 * (  1 + lw_taxrate / 100 ).
        ls_ht-ct10 = ls_ht-ct11 / ls_ht-soluong.
        APPEND ls_ht TO lt_ht.

        SELECT po~PurchaseOrderItem, po~PurchaseOrderItemText,  PurchasingHistoryDocumentYear,
                PurchasingHistoryDocument,PurchasingHistoryDocumentItem,history~PurchaseOrderQuantityUnit,
                SUM( purordamountincompanycodecrcy ) AS purordamountincompanycodecrcy,
                SUM( Quantity ) AS Quantity
              FROM zi_purchaseorderhistory_gr AS history
                INNER JOIN ZI_PurchaseOrderItemAPI01 AS po
                    ON history~PurchaseOrder = po~PurchaseOrder
                    AND history~PurchaseOrderItem = po~PurchaseOrderItem
              WHERE history~PurchaseOrder = @ls_ht-SoPo AND purchasinghistorycategory = 'E'
              AND PostingDate >= @ls_tbperiod-zdatefr AND PostingDate <= @ls_tbperiod-zdateto
              GROUP BY po~PurchaseOrderItem, po~PurchaseOrderItemText,  PurchasingHistoryDocumentYear,
                PurchasingHistoryDocument,PurchasingHistoryDocumentItem, history~PurchaseOrderQuantityUnit
              INTO TABLE @DATA(lt_history).
        DELETE lt_history WHERE purordamountincompanycodecrcy = 0.
        LOOP AT lt_history INTO DATA(ls_history).
          MOVE-CORRESPONDING ls_ht TO ls_pb.

          READ TABLE lt_pb_hd_db INTO DATA(ls_pbhd_db)
          WITH KEY sopo = ls_ht-sopo
                   poitem = ls_history-PurchaseOrderItem
                  purchasinghistorydocumentyear  = ls_history-purchasinghistorydocumentyear
                  purchasinghistorydocument  = ls_history-purchasinghistorydocument
                  purchasinghistorydocumentitem = ls_history-purchasinghistorydocumentitem
                  BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            ls_pb-pbhd_id = ls_pbhd_db-pbhd_id.
          ELSE.
            TRY.
                lv_uuid = cl_system_uuid=>create_uuid_x16_static( ).
              CATCH cx_uuid_error INTO lx_uuid.

            ENDTRY.
            ls_pb-pbhd_id = lv_uuid.
          ENDIF.
          ls_pb-poitem = ls_history-PurchaseOrderItem.
          ls_pb-purchasinghistorydocumentyear = ls_history-PurchasingHistoryDocumentYear.
          ls_pb-purchasinghistorydocument = ls_history-PurchasingHistoryDocument.
          ls_pb-purchasinghistorydocumentitem = ls_history-PurchasingHistoryDocumentItem.
          ls_pb-purchaseorderitemtext = ls_history-PurchaseOrderItemText.
          ls_pb-ct08 = ls_history-purordamountincompanycodecrcy * 100.
          ls_pb-soluong = ls_history-Quantity.
          ls_pb-materialbaseunit = ls_history-PurchaseOrderQuantityUnit.
          c_hdr-tongtiengr += ls_pb-ct08.
          IF ls_pb-soluong > 0.
            ls_pb-ct07 = ls_pb-ct08 / ls_pb-soluong.
          ENDIF.

          APPEND ls_pb TO lt_pb.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    DATA: lw_tong_pb TYPE zde_dec23_0.
    LOOP AT lt_pb ASSIGNING FIELD-SYMBOL(<ls_pb>).
      <ls_pb>-ct11 = c_hdr-tongtienht / c_hdr-tongtiengr * <ls_pb>-ct08.
      lw_tong_pb += <ls_pb>-ct11.
      <ls_pb>-ct13 = <ls_pb>-ct11 * (  1 + lw_taxrate / 100 ).
      <ls_pb>-ct10 = <ls_pb>-ct11 / <ls_pb>-soluong.
    ENDLOOP.
    IF <ls_pb> IS ASSIGNED.
      IF c_hdr-tongtienht <> lw_tong_pb.
        <ls_pb>-ct11 +=  c_hdr-tongtienht - lw_tong_pb.
      ENDIF.
    ENDIF.

    ct_xn = ct_xn.
    ct_ht = lt_ht.
    ct_pb = lt_pb.
  ENDMETHOD.


  METHOD post_invoice_api.
    DATA: ls_invoice TYPE zst_supplier_invoice_hdr.
    DATA: lw_inv_item TYPE zde_numc6.
    DATA: lw_UnplannedDeliveryCost TYPE zde_dec23_0.
    DATA: lw_taxamount TYPE zde_dec23_0.

    IF i_hdr-ngaydh IS INITIAL.
      e_code = '999'.
      e_response-error-message-value = 'Ngày hóa đơn không được để trống'.
      RETURN.
    ENDIF.

    IF i_hdr-ngayht IS INITIAL.
      e_code = '999'.
      e_response-error-message-value = 'Ngày hạch toán không được để trống'.
      RETURN.
    ENDIF.

    SELECT  SINGLE
        i_supplierpartnerfunc~supplier,
        i_supplierpartnerfunc~partnerfunction,
        i_supplierpartnerfunc~referencesupplier
         FROM i_supplierpartnerfunc
         WHERE partnerfunction = 'RS' AND supplier = @i_hdr-supplier
         INTO @DATA(ls_supplierpartnerfunc).
    IF sy-subrc IS NOT INITIAL.
      e_code = '999'.
      e_response-error-message-value = 'Chưa cấu hình Invoicing Party'.
      RETURN.
    ENDIF.

    e_suppliervoice = ls_supplierpartnerfunc-referencesupplier.

    lw_UnplannedDeliveryCost = i_hdr-tongtiengr - i_hdr-tongtienht.
    lw_taxamount = i_hdr-tongtienhtst - i_hdr-tongtienht.

    ls_invoice-_company_code = i_hdr-bukrs.
    ls_invoice-_document_date   = zcl_utility=>to_json_date( iv_date = i_hdr-ngaydh ).

    ls_invoice-_tax_determination_date    = ls_invoice-_document_date.
    ls_invoice-_tax_reporting_date         = ls_invoice-_document_date.
    ls_invoice-_tax_fulfillment_date        = ls_invoice-_document_date.
    ls_invoice-_invoice_receipt_date       = ls_invoice-_document_date.
    ls_invoice-_retention_due_date         = ls_invoice-_document_date.

    ls_invoice-_posting_date    = zcl_utility=>to_json_date( iv_date = i_hdr-ngayht ) .
    ls_invoice-supplierinvoiceidbyinvcgparty  = i_hdr-sohd.
    ls_invoice-_invoicing_party               = ls_supplierpartnerfunc-referencesupplier. "i_hdr-supplier.
    ls_invoice-_document_currency               = 'VND'.
    ls_invoice-_invoice_gross_amount            = i_hdr-tongtienhtst.
    CONDENSE ls_invoice-_invoice_gross_amount.
*    IF lw_UnplannedDeliveryCost < 0.
*      ls_invoice-_unplanned_delivery_cost = '-' && |{ abs( lw_UnplannedDeliveryCost ) }|.
*    ELSE.
*      ls_invoice-_unplanned_delivery_cost = |{ lw_UnplannedDeliveryCost }|.
*    ENDIF.
*    ls_invoice-_unplanned_delivery_cost = lw_UnplannedDeliveryCost.
    CONDENSE ls_invoice-_unplanned_delivery_cost.
    ls_invoice-_document_header_text            = `Hóa đơn gia công ` && i_hdr-zper+4(2) && '.' && i_hdr-zper(4).
    ls_invoice-_due_calculation_base_date         = ls_invoice-_document_date.

    APPEND INITIAL LINE TO ls_invoice-to_supplierinvoicetax-results ASSIGNING FIELD-SYMBOL(<lf_tax>).

    <lf_tax>-_document_currency = 'VND'.
    <lf_tax>-_tax_code   = i_hdr-thuesuat.
    <lf_tax>-_tax_determination_date   = ls_invoice-_document_date.
    <lf_tax>-_tax_base_amount_in_trans_crcy = i_hdr-tongtienht.
    CONDENSE <lf_tax>-_tax_base_amount_in_trans_crcy.
    <lf_tax>-_tax_amount = lw_taxamount.
    CONDENSE <lf_tax>-_tax_amount.

    LOOP AT i_t_pb INTO DATA(ls_ht).

      lw_inv_item = lw_inv_item + 1.
      APPEND INITIAL LINE TO ls_invoice-to_suplrinvcitempurordref-results ASSIGNING FIELD-SYMBOL(<lf_item>).
      <lf_item>-_supplier_invoice_item =  lw_inv_item.
      <lf_item>-_purchase_order = ls_ht-sopo.
      <lf_item>-_purchase_order_item = ls_ht-poitem.

      <lf_item>-_tax_code   = i_hdr-thuesuat.
      <lf_item>-_supplier_invoice_item_amount  = ls_ht-ct11.
      <lf_item>-_document_currency = 'VND'.
      CONDENSE <lf_item>-_supplier_invoice_item_amount.
      <lf_item>-quantityinpurchaseorderunit = ls_ht-soluong.
      CONDENSE <lf_item>-quantityinpurchaseorderunit.
      SELECT SINGLE unitofmeasure_e FROM I_UnitOfMeasure
         WHERE UnitOfMeasureSAPCode = @ls_ht-materialbaseunit INTO @<lf_item>-_purchase_order_quantity_unit .
*      <lf_item>-_purchase_order_quantity_unit = ls_ht-materialbaseunit.
      <lf_item>-_tax_determination_date = ls_invoice-_document_date.
      <lf_item>-referencedocumentfiscalyear = ls_ht-PurchasingHistoryDocumentYear.
      <lf_item>-_reference_document = ls_ht-PurchasingHistoryDocument.
      <lf_item>-_reference_document_item = ls_ht-PurchasingHistoryDocumentItem.

    ENDLOOP.

    DATA(lw_json_body) = /ui2/cl_json=>serialize(
                    data = ls_invoice
                    compress = abap_true
                    pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    REPLACE ALL OCCURRENCES OF 'supplierinvoiceidbyinvcgparty'
       IN lw_json_body WITH 'SupplierInvoiceIDByInvcgParty'.
    REPLACE ALL OCCURRENCES OF 'toSuplrinvcitempurordref'
       IN lw_json_body WITH 'to_SuplrInvcItemPurOrdRef'.
    REPLACE ALL OCCURRENCES OF 'quantityinpurchaseorderunit'
      IN lw_json_body WITH 'QuantityInPurchaseOrderUnit'.
    REPLACE ALL OCCURRENCES OF 'referencedocumentfiscalyear'
       IN lw_json_body WITH 'ReferenceDocumentFiscalYear'.
    REPLACE ALL OCCURRENCES OF 'toSupplierinvoicetax'
       IN lw_json_body WITH 'to_SupplierInvoiceTax'.


    DATA: lo_http_client TYPE REF TO if_web_http_client.
    DATA: response TYPE string.
    DATA: ls_odata_return TYPE zst_odata_return.
    TRY.
        DATA(lv_host) = cl_abap_context_info=>get_system_url( ).
      CATCH cx_abap_context_info_error.
        "handle exception
    ENDTRY.
    DATA(lv_url) = |https://{ lv_host }/sap/opu/odata/sap/API_SUPPLIERINVOICE_PROCESS_SRV/A_SupplierInvoice|.
    TRY.
        DATA(lo_http_destination) =
             cl_http_destination_provider=>create_by_url( lv_url ).
        "create HTTP client by destination
        DATA(lo_web_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_http_destination ) .

        "adding headers
        DATA(lo_web_http_request) = lo_web_http_client->get_http_request( ).
        lo_web_http_request->set_header_fields( VALUE #(
         (  name = 'DataServiceVersion' value = '2.0' )
        (  name = 'Accept' value = 'application/json' )
         ) ).
        "Authorization
        lo_web_http_request->set_header_field(  i_name = 'username' i_value = 'PB9_LO' ).
        lo_web_http_request->set_header_field(  i_name = 'password' i_value = 'Qwertyuiop@1234567890' ).
        lo_web_http_request->set_authorization_basic( i_username = 'PB9_LO' i_password = 'Qwertyuiop@1234567890' ).
        lo_web_http_request->set_content_type( |application/json| ).
        lo_web_http_request->set_header_field( i_name = 'Accept' i_value = 'application/json' ).
        lo_web_http_request->set_header_field( i_name = 'x-csrf-token' i_value = 'Fetch' ).

        DATA(lo_response) = lo_web_http_client->execute( i_method = if_web_http_client=>get ).
        DATA(lv_token)    = lo_response->get_header_field( 'x-csrf-token' ).
        lo_web_http_request->set_header_field( i_name = 'x-csrf-token' i_value = lv_token ).

        lo_web_http_request->set_text( lw_json_body ).
        "set request method and execute request
        DATA(lo_web_http_response) = lo_web_http_client->execute( if_web_http_client=>post ).
        DATA(lv_response) = lo_web_http_response->get_text( ).

        /ui2/cl_json=>deserialize(
          EXPORTING json = lv_response
          CHANGING  data = e_response ).
        DATA(lv_status) = lo_web_http_response->get_status( ).
        e_code = lv_status-code.

      CATCH cx_http_dest_provider_error cx_web_http_client_error cx_web_message_error.
        "error handling
    ENDTRY.
  ENDMETHOD.


  METHOD cancel_invoice.
    DATA ls_cancel TYPE STRUCTURE FOR ACTION IMPORT i_supplierinvoicetp~cancel.
    DATA lt_cancel TYPE TABLE FOR ACTION IMPORT i_supplierinvoicetp~cancel.

    " The %cid (temporary primary key) has always to be supplied (is omitted in further examples)
    TRY.
        DATA(lv_cid) = cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ).
      CATCH cx_uuid_error.
        "Error handling
    ENDTRY.

    ls_cancel-%cid                                 = lv_cid.

    " Fill Parameters for Action
    ls_cancel-SupplierInvoice            = i_hdr-supplierinvoice.
    ls_cancel-SupplierInvoiceFiscalYear  = i_hdr-ngayht(4).
    ls_cancel-%param-ReversalReason      = '01'.
    ls_cancel-%param-PostingDate         = i_hdr-ngayht.

    INSERT ls_cancel INTO TABLE lt_cancel.

    "Register the action
    MODIFY ENTITIES OF i_supplierinvoicetp
    ENTITY supplierinvoice
    EXECUTE cancel FROM lt_cancel
    FAILED DATA(ls_failed)
    REPORTED DATA(ls_reported)
    MAPPED DATA(ls_mapped).

    IF ls_failed IS NOT INITIAL.
      DATA lo_message TYPE REF TO if_message.
      lo_message = ls_reported-supplierinvoice[ 1 ]-%msg.
      e_code = '500'.
      e_response-error-message-value = lo_message->get_text( ).
      "Error handling
    ELSE.
      e_code = '200'.
*        e_response-d-supplierinvoice = i_hdr-supplierinvoice.
    ENDIF.

*    "Execution the action
*    COMMIT ENTITIES
*      RESPONSE OF i_supplierinvoicetp
*      FAILED DATA(ls_commit_failed)
*      REPORTED DATA(ls_commit_reported).
*
*    IF ls_commit_reported IS NOT INITIAL.
*      LOOP AT ls_commit_reported-supplierinvoice ASSIGNING FIELD-SYMBOL(<ls_invoice>).
*        IF <ls_invoice>-supplierinvoice IS NOT INITIAL AND
*           <ls_invoice>-supplierinvoicefiscalyear IS NOT INITIAL.
*          "Success case
*          e_code = '200'.
*          DATA(lv_ReverseDocument) = <ls_invoice>-supplierinvoice.   "Document Number of created Reversal Document
*          DATA(lv_ReverseDocumentFiscalYear) = <ls_invoice>-supplierinvoicefiscalyear.  "Fiscal of created Reversal Document
*          e_response-d-supplierinvoice = lv_ReverseDocument.
*        ELSE.
*          "Error handling
*           e_code = '500'.
*          e_response-error-message-value = <ls_invoice>-%msg->if_message~get_text( ).
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
*
*    IF ls_commit_failed IS NOT INITIAL.
*      LOOP AT ls_commit_reported-supplierinvoice ASSIGNING <ls_invoice>.
*        e_code = '500'.
*        e_response-error-message-value = <ls_invoice>-%msg->if_message~get_text( ).
*        "Error handling
*      ENDLOOP.
*    ENDIF.

  ENDMETHOD.
ENDCLASS.
