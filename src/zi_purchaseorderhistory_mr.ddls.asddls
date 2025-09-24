@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'I_PurchaseOrderHistoryNoRev'
define view entity ZI_PURCHASEORDERHISTORY_MR
  as select from I_PurchaseOrderHistoryAPI01 as history
{
  key    PurchaseOrder,
  key    PurchaseOrderItem,
  key    ReferenceDocumentFiscalYear,

  key    ReferenceDocument,

  key    ReferenceDocumentItem,
         DocumentCurrency,
         CompanyCodeCurrency,
         @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
         sum(
             case DebitCreditCode
                  when 'H' then -PurOrdAmountInCompanyCodeCrcy
                  else         PurOrdAmountInCompanyCodeCrcy
             end
         ) as PurOrdAmountInCompanyCodeCrcy


}
where
  PurchasingHistoryCategory = 'Q'
group by
  PurchaseOrder,
  PurchaseOrderItem,
  ReferenceDocumentFiscalYear,
  ReferenceDocument,
  ReferenceDocumentItem,
  DocumentCurrency,
  CompanyCodeCurrency
