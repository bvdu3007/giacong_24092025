@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: 'Bảng trừ tiền BTP thừa thiếu'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZTBTRU_THIEU'
}
@AccessControl.authorizationCheck: #MANDATORY
define root view entity ZC_TBTRU_THIEU
  provider contract transactional_query
  as projection on ZR_TBTRU_THIEU
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
   _dtl : redirected to composition child ZC_TBTRU_THIE_DTL
}
