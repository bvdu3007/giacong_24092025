@Metadata.allowExtensions: true
//@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: 'Biên bản gia công'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZTBBB_GC'
}
@AccessControl.authorizationCheck: #NOT_REQUIRED

@UI.presentationVariant: [ {
  sortOrder: [{ by: 'SoBb', direction: #DESC }],
  groupBy: [ 'LoaiHang' ],
  visualizations: [{ type: #AS_LINEITEM }]
} ]

define root view entity ZC_TBBB_GC
  provider contract transactional_query
  as projection on ZR_TBBB_GC
{
  key HdrID,
      @Consumption.valueHelpDefinition:[
          { entity                       : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
          additionalBinding              : [{ element: 'domain_name',
                    localConstant        : 'ZDE_LOAI_HANG', usage: #FILTER }]
                    , distinctValues     : true
          }]

      @ObjectModel.text.element: ['LoaiHangDesc']
      LoaiHang,
      LoaiHangDesc,
      SoBb,
      SoBbBase,
      SoBbNum,
      SoBbSub,
      NgayLapBb,
       @Consumption.valueHelpDefinition:[
          { entity                       : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
          additionalBinding              : [{ element: 'domain_name',
                    localConstant        : 'ZDE_TT_BBGC', usage: #FILTER }]
                    , distinctValues     : true
          }]
     @ObjectModel.text.element: ['trangthaiDesc']
      trangthai,
      trangthaiDesc,
      @Consumption.valueHelpDefinition:[
       { entity                       : { name : 'ZV_PO_GC' , element: 'PurchaseOrder' } }
      ]
      SoPo,
      Supplier,
         @Consumption.valueHelpDefinition:[
          { entity                       : { name: 'I_CompanyCodeStdVH', element: 'CompanyCode' }
          }]
      CompanyCode,
      SupplierName,
      SupplierName1,
      OrderID,
      SalesOrder,
      NgayNhapHang,
      NgayTraBb,
      NgayNhapKho,
      Material,
      ProductDescription,
      ProdUnivHierarchyNode,
      Ct12,

      Ct13,
      Ct14,
      Ct16,
      GhiChu,
      Ct18,
      Ct19,
      Ct20,
      Ct21,
      Ct22,
      Ct23,
      Ct24,
      Ct25,
      Ct26,
      Ct27,
      Ct28,
      Ct29,
      Ct30,
      Ct31,
      Ct32,
      Ct321,
      Ct322,
      Ct323,
      Ct324,
      bs01,
      bs02,
      bs03,
      bs04,
      bs05,    
            @Consumption.valueHelpDefinition:[
          { entity                       : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
          additionalBinding              : [{ element: 'domain_name',
                    localConstant        : 'ZDE_CHECK', usage: #FILTER }]
                    , distinctValues     : true
          }]  
      bs06,
                  @Consumption.valueHelpDefinition:[
          { entity                       : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
          additionalBinding              : [{ element: 'domain_name',
                    localConstant        : 'ZDE_CHECK', usage: #FILTER }]
                    , distinctValues     : true
          }]  
      bs07,
      Ct33,
      Ct34,
      Ct35,
      Ct36,
      Ct37,
      Ct38,
      Ct39,
      Ct40,
      Ct41,
      Ct42,
      Ct43,
      Ct44,
      Ct45,
      Ct46,
      Ct47,
      Ct48,
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
      _dtl : redirected to composition child ZC_TBGC_LOI
}
