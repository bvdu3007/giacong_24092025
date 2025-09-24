@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: '###GENERATED Core Data Service Entity'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZTBMAT_TYPE'
}
@AccessControl.authorizationCheck: #MANDATORY
define root view entity ZC_TBMAT_TYPE
  provider contract transactional_query
  as projection on ZR_TBMAT_TYPE
  association [1..1] to ZR_TBMAT_TYPE as _BaseEntity on $projection.Mattype = _BaseEntity.Mattype and $projection.Material = _BaseEntity.Material
{
 @Consumption.valueHelpDefinition:[
          { entity                       : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
          additionalBinding              : [{ element: 'domain_name',
                    localConstant        : 'ZDE_MATERIAL_TYPE', usage: #FILTER }]
                    , distinctValues     : true
          }]

  key Mattype,
  
     @Consumption.valueHelpDefinition:[
          { entity                       : { name: 'zc_product_sh', element: 'Product' }
          }]
  
  
  key Material,
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
  _BaseEntity
}
