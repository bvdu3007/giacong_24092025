@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: 'Đối chiếu gia công'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZTBDCGC_HDR'
}
@AccessControl.authorizationCheck: #MANDATORY
define root view entity ZC_TBDCGC_HDR
  provider contract transactional_query
  as projection on ZR_TBDCGC_HDR

{
  key HdrID,
      @Consumption.valueHelpDefinition:[
          { entity                       : { name : 'ZVI_PERIOD' , element: 'Zper' } }
         ]
      Zper,
      Zperdesc,
      lan,
      @Consumption.valueHelpDefinition:[
           { entity                       : { name: 'I_CompanyCodeStdVH', element: 'CompanyCode' }
           }]


      Bukrs,
      @Consumption.valueHelpDefinition:[
          { entity                       : { name: 'zi_Supplier_sh', element: 'Supplier' }
          }]
      Supplier,
       @Consumption.valueHelpDefinition:[
          { entity                       : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
          additionalBinding              : [{ element: 'domain_name',
                    localConstant        : 'ZDE_TT_DCGC', usage: #FILTER }]
                    , distinctValues     : true
          }]
     @ObjectModel.text.element: ['trangthaiDesc']
      trangthai,
      trangthaiDesc,
      SearchTerm1,
      Sumdate,
      Sumdatetime,
      Ct01,
      Ct02,
      Ct03,
      Ct011,
      Ct021,
      Ct031,
      ct03a,
      ct03b,
      ct03a1,
      ct03b1,
      Ct04,
      Ct05,
      Ct06,
      Ct07,
      Ct08,
      Ct09,
      ct10,
      ct11,
      ct12,
      ct13,
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
      _dtl : redirected to composition child ZC_TBDCGC_DTL
}
