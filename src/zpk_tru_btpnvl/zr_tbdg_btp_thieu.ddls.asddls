@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZTBDG_BTP_THIEU'
@EndUserText.label: 'Bảng đơn giá trừ tiền'
define root view entity ZR_TBDG_BTP_THIEU
  as select from ztb_dg_btp_thieu
  association [0..1] to I_ProductText    as _ProdT on $projection.Material =  _ProdT.Product and _ProdT.Language = 'E'
  association [0..1] to I_ProductGroupText_2    as _ProdGT on $projection.Productgroup = _ProdGT.ProductGroup and _ProdGT.Language = 'E'
  
{
  key id as ID,
  cast (material as matnr preserving type )  as Material,
  _ProdT.ProductName,
  productgroup as Productgroup,
  _ProdGT.ProductGroupName,
  characteristic as Characteristic,
  charcvalue as Charcvalue,
  price as Price,
  @Consumption.valueHelpDefinition: [ {
    entity.name: 'I_UnitOfMeasureStdVH', 
    entity.element: 'UnitOfMeasure', 
    useForValidation: true
  } ]
  unit as Unit,
  @Semantics.user.createdBy: true
  created_by as CreatedBy,
  @Semantics.systemDateTime.createdAt: true
  created_at as CreatedAt,
  @Semantics.user.localInstanceLastChangedBy: true
  last_changed_by as LastChangedBy,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  last_changed_at as LastChangedAt
}
