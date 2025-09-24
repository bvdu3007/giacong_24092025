@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'I_TaxCodeRate'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zI_TaxCodeRate as select from I_TaxCodeRate
{   
    key TaxCode,
    ConditionRateRatio,
    ConditionRateRatioUnit
} where Country = 'VN' and TaxCalculationProcedure = '0TXVN' and VATConditionType = 'MWVS' and TaxType = 'V'
