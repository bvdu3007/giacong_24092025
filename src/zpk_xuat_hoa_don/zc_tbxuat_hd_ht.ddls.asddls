@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: 'Xuất hóa đơn'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZTBXUAT_HD'
}
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity ZC_TBXUAT_HD_HT
  provider contract transactional_query
  as projection on ZR_TBXUAT_HD
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

      Mahd,
      mahdnum,
      @Consumption.valueHelpDefinition:[
              { entity                       : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
              additionalBinding              : [{ element: 'domain_name',
                        localConstant        : 'ZDE_TT_HD', usage: #FILTER }]
                        , distinctValues     : true
              }]
      @ObjectModel.text.element: ['tt_desc']
      Trangthai,
      tt_desc,
      sohd,
      Ngayht,
      Ngaydh,
      @Consumption.valueHelpDefinition:[
              { entity                       : { name: 'zi_Supplier_sh', element: 'Supplier' }
              }]
      Supplier,
      Searchterm1,
      supplierinvoice,
      invoicingparty,
      invoicingpartyName,
      code,
      message,
      Thuesuat,
      Tongtienxn,
      Tongtienxnst,
      Tongtienht,
      Tongtienhtst,
      Sumdate,
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
      //  _xn : redirected to composition child ZC_TBXN_XUAT_HD,
      _ht : redirected to composition child ZC_TBHT_HD,
      _pb : redirected to composition child ZC_TBPB_HD
}
where
  Trangthai > '0'
