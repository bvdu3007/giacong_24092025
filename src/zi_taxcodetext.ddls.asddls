@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'I_TaxCodeText'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zI_TaxCodeText as select from I_TaxCodeText
{
    key TaxCode,
    TaxCodeName
} where TaxCalculationProcedure = '0TXVN' and Language = $session.system_language 
and TaxCodeName <> 'Not use'
