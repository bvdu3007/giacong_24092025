@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'I_PurchaseOrderItemAPI01'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_PurchaseOrderItemAPI01 as select from I_PurchaseOrderItemAPI01
{
    key PurchaseOrder,
    key PurchaseOrderItem,
    PurchaseOrderItemText,
    DocumentCurrency,
    MaterialGroup,
    Material,
          @Semantics.amount.currencyCode: 'DocumentCurrency'
    NetPriceAmount,
    PurchaseOrderQuantityUnit
}
