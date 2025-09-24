@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'I_PurchaseOrderHistoryNoRev'
define view entity ZI_PURCHASEORDERHISTORY_GR
  as select from zI_PurchaseOrderHistory as gr
  association [0..1] to ZI_PURCHASEORDERHISTORY_MR as mr on  mr.PurchaseOrder                 = gr.PurchaseOrder
                                                      and mr.PurchaseOrderItem             = gr.PurchaseOrderItem
                                                      and mr.ReferenceDocumentFiscalYear   = gr.PurchasingHistoryDocumentYear
                                                      and mr.ReferenceDocument             = gr.PurchasingHistoryDocument
                                                      and mr.ReferenceDocumentItem         = gr.PurchasingHistoryDocumentItem
{

  key gr.PurchaseOrder as PurchaseOrder,

  key PurchaseOrderItem,

  key AccountAssignmentNumber,

  key PurchasingHistoryDocumentType,

  key PurchasingHistoryDocumentYear,

  key PurchasingHistoryDocument,

  key PurchasingHistoryDocumentItem,

      PurchasingHistoryCategory,

      GoodsMovementType,

      PostingDate,
      Currency,

      DebitCreditCode,

      IsCompletelyDelivered,

      ReferenceDocumentFiscalYear,

      ReferenceDocument,

      ReferenceDocumentItem,

      Material,

      Plant,

      RvslOfGoodsReceiptIsAllowed,

      PricingDocument,

      TaxCode,

      DocumentDate,

      InventoryValuationType,

      DocumentReferenceID,
      DeliveryQuantityUnit,

      ManufacturerMaterial,

      AccountingDocumentCreationDate,
      PurgHistDocumentCreationTime,

      @Semantics.quantity.unitOfMeasure:'PurchaseOrderQuantityUnit'
      Quantity         as Quantity,
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      case
      when mr.PurOrdAmountInCompanyCodeCrcy is not null
         then PurOrdAmountInCompanyCodeCrcy - mr.PurOrdAmountInCompanyCodeCrcy
      else PurOrdAmountInCompanyCodeCrcy
      end              as PurOrdAmountInCompanyCodeCrcy,
      @Semantics.amount.currencyCode: 'Currency'
      PurchaseOrderAmount,
      @Semantics.quantity.unitOfMeasure:'OrderPriceUnit'
      QtyInPurchaseOrderPriceUnit,
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      GRIRAcctClrgAmtInCoCodeCrcy,
      @Semantics.quantity.unitOfMeasure:'PurchaseOrderQuantityUnit'
      GdsRcptBlkdStkQtyInOrdQtyUnit,
      @Semantics.quantity.unitOfMeasure:'OrderPriceUnit'
      GdsRcptBlkdStkQtyInOrdPrcUnit,
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      InvoiceAmtInCoCodeCrcy,

      ShipgInstrnSupplierCompliance,
      @Semantics.amount.currencyCode: 'Currency'
      InvoiceAmountInFrgnCurrency,
      @Semantics.quantity.unitOfMeasure:'DeliveryQuantityUnit'
      QuantityInDeliveryQtyUnit,
      @Semantics.amount.currencyCode: 'Currency'
      GRIRAcctClrgAmtInTransacCrcy,
      @Semantics.quantity.unitOfMeasure: 'BaseUnit'
      QuantityInBaseUnit,
      Batch,
      @Semantics.amount.currencyCode: 'DocumentCurrency'
      GRIRAcctClrgAmtInOrdTrnsacCrcy,
      @Semantics.amount.currencyCode: 'DocumentCurrency'
      InvoiceAmtInPurOrdTransacCrcy,
      @Semantics.quantity.unitOfMeasure:'PurchaseOrderQuantityUnit'
      VltdGdsRcptBlkdStkQtyInOrdUnit,
      @Semantics.quantity.unitOfMeasure:'OrderPriceUnit'
      VltdGdsRcptBlkdQtyInOrdPrcUnit,

      IsToBeAcceptedAtOrigin,

      gr.ExchangeRate,

      DeliveryDocument,

      DeliveryDocumentItem,

      OrderPriceUnit,
      PurchaseOrderQuantityUnit,
      BaseUnit,
      DocumentCurrency,
      CompanyCodeCurrency
}
where
      GoodsMovementType         <> '102'
  and PurchasingHistoryCategory =  'E'
