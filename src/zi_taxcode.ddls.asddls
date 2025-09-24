@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Tax code'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zi_taxcode as select distinct from I_TaxCode
{
    key TaxCode
} where TaxCalculationProcedure = '0TXVN' and TaxType = 'V'
