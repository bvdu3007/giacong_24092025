@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Product Group'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zc_ProductGroup_SH as select distinct from I_ProductGroupText_2
{
    key ProductGroup,
    ProductGroupName,
    ProductGroupText
} where Language = 'E'
