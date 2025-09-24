@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'i_purordaccountassignmentapi01'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zi_purordaccountassignment as select from I_PurOrdAccountAssignmentAPI01
{
    key PurchaseOrder,
    key PurchaseOrderItem,
    key AccountAssignmentNumber,
    CostCenter,
    MasterFixedAsset,
    ProjectNetwork,
    @Semantics: { quantity.unitOfMeasure : 'PurchaseOrderQuantityUnit' }
    Quantity,
    PurchaseOrderQuantityUnit,
    MultipleAcctAssgmtDistrPercent,
    @Semantics: { amount.currencyCode: 'DocumentCurrency' }
    PurgDocNetAmount,
    DocumentCurrency,
    IsDeleted,
    GLAccount,
    BusinessArea,
    SalesOrder,
    SalesOrderItem,
    SalesOrderScheduleLine,
    FixedAsset,
    OrderID,
    UnloadingPointName,
    ControllingArea,
    CostObject,
    ProfitabilitySegment,
    ProfitabilitySegment_2,
    ProfitCenter,
    WBSElementInternalID,
    WBSElementInternalID_2,
    ProjectNetworkInternalID,
    CommitmentItem,
    CommitmentItemShortID,
    FundsCenter,
    Fund,
    FunctionalArea,
    GoodsRecipientName,
    IsFinallyInvoiced,
    RealEstateObject,
    REInternalFinNumber,
    NetworkActivityInternalID,
    PartnerAccountNumber,
    JointVentureRecoveryCode,
    SettlementReferenceDate,
    OrderInternalID,
    OrderIntBillOfOperationsItem,
    TaxCode,
    TaxJurisdiction,
    @Semantics: { amount.currencyCode: 'DocumentCurrency' }
    NonDeductibleInputTaxAmount,
    CostCtrActivityType,
    BusinessProcess,
    GrantID,
    BudgetPeriod,
    EarmarkedFundsDocument,
    EarmarkedFundsItem,
    EarmarkedFundsDocumentItem,
    ServiceDocumentType,
    ServiceDocument,
    ServiceDocumentItem,
    /* Associations */
    _PurchaseOrder,
    _PurchaseOrderItem
}
