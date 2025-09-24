@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: '###GENERATED Core Data Service Entity'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZTBDG_BTP_BS'
}
@AccessControl.authorizationCheck: #MANDATORY
define root view entity ZC_TBDG_BTP_BS
  provider contract transactional_query
  as projection on ZR_TBDG_BTP_BS
  association [1..1] to ZR_TBDG_BTP_BS as _BaseEntity on $projection.ID = _BaseEntity.ID
{
  key ID,
  @Consumption.valueHelpDefinition:[
           { entity                       : { name : 'zc_product_sh' , element: 'Product' } }
          ]
      Material,
      @Consumption.valueHelpDefinition:[
           { entity                       : { name : 'zc_ProductGroup_SH' , element: 'ProductGroup' } }
          ]
      Productgroup,
      ProductName,
      ProductGroupName,
      @Consumption.valueHelpDefinition:[
       { entity                       : { name : 'ZC_Characteristics' , element: 'Characteristic' } }
      ]
  Characteristic,
  Charcvalue,
  Price,
  @Consumption: {
    valueHelpDefinition: [ {
      entity.element: 'UnitOfMeasure', 
      entity.name: 'I_UnitOfMeasureStdVH', 
      useForValidation: true
    } ]
  }
  Unit,
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
