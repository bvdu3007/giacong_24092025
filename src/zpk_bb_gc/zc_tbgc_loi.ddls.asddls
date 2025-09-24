@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: 'Biên bản gia công'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZTBGC_LOI'
}
@AccessControl.authorizationCheck: #NOT_REQUIRED

@UI.presentationVariant: [{
    sortOrder: [
      { by: 'LoaiLoi',   direction: #ASC },
      { by: 'ErrorCode', direction: #ASC }
    ]
}]

define view entity ZC_TBGC_LOI
  as projection on ZR_TBGC_LOI
{
  key HdrID,
  key DtlID,
  LoaiHang,
       @Consumption.valueHelpDefinition:[
      { entity                       : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
      additionalBinding              : [{ element: 'domain_name',
                localConstant        : 'ZDE_LOAI_LOI', usage: #FILTER }]
                , distinctValues     : true
      }]
      @ObjectModel.text.element: ['LoaiLoiDesc']
  LoaiLoi,
  LoaiLoiDesc,
  ErrorCode,
  Errordesc,
  SlLoi,
  tile,
  Bangi,
  CheckBangi,
  Bangii,
  CheckBangii,
  GhiChu,
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
  _hdr  : redirected to parent ZC_TBBB_GC
}
