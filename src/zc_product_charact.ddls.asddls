@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Product Characteristics'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zc_product_charact as 
    select distinct from I_ClfnObjectCharcValForKeyDate( P_KeyDate: $session.system_date ) as ProChar
    left outer join ZC_Characteristics as Char on Char.CharcInternalId = ProChar.CharcInternalID
{
    key ProChar.CharcInternalID,
    key ProChar.ClfnObjectID,
    Char.Characteristic,
    ProChar.CharcValue    
}
where ProChar.ClassType = '001'
