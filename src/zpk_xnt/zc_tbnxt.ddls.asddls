@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: 'Tồn kho'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZTBNXT'
}
@AccessControl.authorizationCheck: #MANDATORY
define root view entity ZC_TBNXT
  provider contract transactional_query
  as projection on ZR_TBNXT
  association [1..1] to ZR_TBNXT as _BaseEntity on $projection.Material = _BaseEntity.Material and $projection.Plant = _BaseEntity.Plant and $projection.Supplier = _BaseEntity.Supplier and $projection.Orderid = _BaseEntity.Orderid and $projection.Zper = _BaseEntity.Zper
{
  key Material,
  key Plant,
  key Supplier,
  key Orderid,
  key Zper,
  @Semantics: {
    quantity.unitOfMeasure: 'Materialbaseunit'
  }
  Quantityinbaseunit,
  @Consumption: {
    valueHelpDefinition: [ {
      entity.element: 'UnitOfMeasure', 
      entity.name: 'I_UnitOfMeasureStdVH', 
      useForValidation: true
    } ]
  }
  Materialbaseunit,
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
