@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZTBNXT'
@EndUserText.label: 'Tồn kho'
define root view entity ZR_TBNXT
  as select from ztb_nxt
{
  key material as Material,
  key plant as Plant,
  key supplier as Supplier,
  key orderid as Orderid,
  key zper as Zper,
  @Semantics.quantity.unitOfMeasure: 'Materialbaseunit'
  quantityinbaseunit as Quantityinbaseunit,
  @Consumption.valueHelpDefinition: [ {
    entity.name: 'I_UnitOfMeasureStdVH', 
    entity.element: 'UnitOfMeasure', 
    useForValidation: true
  } ]
  materialbaseunit as Materialbaseunit,
  @Semantics.user.createdBy: true
  created_by as CreatedBy,
  @Semantics.systemDateTime.createdAt: true
  created_at as CreatedAt,
  @Semantics.user.localInstanceLastChangedBy: true
  last_changed_by as LastChangedBy,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  last_changed_at as LastChangedAt
}
