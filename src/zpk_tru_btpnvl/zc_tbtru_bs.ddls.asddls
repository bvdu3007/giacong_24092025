@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: '###GENERATED Core Data Service Entity'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZTBTRU_BS'
}
@AccessControl.authorizationCheck: #MANDATORY
define root view entity ZC_TBTRU_BS
  provider contract transactional_query
  as projection on ZR_TBTRU_BS
{
  key HdrID,
   @Consumption.valueHelpDefinition:[
      { entity                       : { name : 'ZVI_PERIOD' , element: 'Zper' } }
     ]
  Zper,
   @Consumption.valueHelpDefinition:[
          { entity                       : { name: 'I_CompanyCodeStdVH', element: 'CompanyCode' }
          }]
  Bukrs,
  Zperdesc,
  Sumdate,
  @Semantics: {
    systemDateTime.localInstanceLastChangedAt: true
  }
  Sumdatetime,
  @Semantics: {
    user.createdBy: true
  }
  CreatedBy,
  @Semantics: {
    systemDateTime.createdAt: true
  }
  CreatedAt,
  @Semantics: {
    user.localInstanceLastChangedBy: true
  }
  LastChangedBy,
  @Semantics: {
    systemDateTime.localInstanceLastChangedAt: true
  }
  LastChangedAt,
    _dtl : redirected to composition child ZC_TBTRU_bs_DTL
}
