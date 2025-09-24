@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZTBHT_HD'
@EndUserText.label: 'Hạch toán hóa đơn'
define view entity ZR_TBHT_HD
  as select from ztb_ht_hd
  association        to parent ZR_TBXUAT_HD as _hdr on $projection.HdrID = _hdr.HdrID
{
  key hdr_id as HdrID,
  key hthd_id as HthdID,
  material as Material,
  productdescription as Productdescription,
  sopo as Sopo,
  @Semantics.quantity.unitOfMeasure: 'Materialbaseunit'
  soluong as Soluong,
  @Consumption.valueHelpDefinition: [ {
    entity.name: 'I_UnitOfMeasureStdVH', 
    entity.element: 'UnitOfMeasure', 
    useForValidation: true
  } ]
  materialbaseunit as Materialbaseunit,
  ct07 as Ct07,
  ct08 as Ct08,
  ct10 as Ct10,
  ct11 as Ct11,
  ct13 as Ct13,
  @Semantics.user.createdBy: true
  created_by as CreatedBy,
  @Semantics.systemDateTime.createdAt: true
  created_at as CreatedAt,
  @Semantics.user.localInstanceLastChangedBy: true
  last_changed_by as LastChangedBy,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  last_changed_at as LastChangedAt,
  _hdr
}
