@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'I_PurchaseOrderHistoryNoRev'
define view entity zI_PurchaseOrderHistory as select from I_PurchaseOrderHistoryAPI01 as history
association [0..1] to I_PurchaseOrderHistoryAPI01 as revert 
    on revert.PurchaseOrder = history.PurchaseOrder
    and revert.PurchaseOrderItem = history.PurchaseOrderItem
    and revert.PurchasingHistoryDocumentType = history.PurchasingHistoryDocumentType
    and revert.ReferenceDocumentFiscalYear = history.ReferenceDocumentFiscalYear
    and  revert.ReferenceDocument = history.ReferenceDocument
    and revert.ReferenceDocumentItem = history.ReferenceDocumentItem
//    and revert.PurchasingHistoryDocument <> history.PurchasingHistoryDocument
    and revert.DebitCreditCode = 'H'
    and revert.PurchasingHistoryCategory = history.PurchasingHistoryCategory
{
   

    
  key history.PurchaseOrder as PurchaseOrder,

  key PurchaseOrderItem,

  key AccountAssignmentNumber,

  key PurchasingHistoryDocumentType,

  key PurchasingHistoryDocumentYear,

  key PurchasingHistoryDocument,

  key PurchasingHistoryDocumentItem,

      PurchasingHistoryCategory,

      GoodsMovementType ,

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
          case
      when revert.Quantity is not null
         then Quantity - revert.Quantity
      else Quantity
      end              as Quantity,

      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      case
      when revert.PurOrdAmountInCompanyCodeCrcy is not null
         then PurOrdAmountInCompanyCodeCrcy - revert.PurOrdAmountInCompanyCodeCrcy
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
      
      history.ExchangeRate,
      
      DeliveryDocument,
      
      DeliveryDocumentItem,
      
      OrderPriceUnit,
      PurchaseOrderQuantityUnit,
      BaseUnit,
      DocumentCurrency,
      CompanyCodeCurrency
} 
where revert.PurchasingHistoryDocument is null and DebitCreditCode = 'S';
