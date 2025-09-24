@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: 'Bảng xét duyệt tỷ lệ lỗi'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZTBXETDUYET_HDR'
}
@AccessControl.authorizationCheck: #MANDATORY
define root view entity ZC_TBXETDUYET_HDR
  provider contract transactional_query
  as projection on ZR_TBXETDUYET_HDR
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
  lan,
   @Consumption.valueHelpDefinition:[
          { entity                       : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
          additionalBinding              : [{ element: 'domain_name',
                    localConstant        : 'ZDE_TT_DCGC', usage: #FILTER }]
                    , distinctValues     : true
          }]
     @ObjectModel.text.element: ['trangthaiDesc']
      trangthai,
      trangthaiDesc,
  Ct05,
  Zperdesc,
  Zstatus,
  Sumdate,
  SumDateTime,
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
    _dtl : redirected to composition child ZC_TBXETDUYET_DTL
}
