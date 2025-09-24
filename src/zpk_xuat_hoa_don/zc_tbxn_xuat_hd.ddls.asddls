@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: 'Xác nhận xuất hóa đơn'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZTBXN_XUAT_HD'
}
@AccessControl.authorizationCheck: #MANDATORY
define view entity ZC_TBXN_XUAT_HD
  as projection on ZR_TBXN_XUAT_HD
{
  key HdrID,
  key XnhdID,
  Material,
  Productdescription,
  Sopo,
  @Semantics: {
    quantity.unitOfMeasure: 'Materialbaseunit'
  }
  Soluong,
  @Consumption: {
    valueHelpDefinition: [ {
      entity.element: 'UnitOfMeasure', 
      entity.name: 'I_UnitOfMeasureStdVH', 
      useForValidation: true
    } ]
  }
  Materialbaseunit,
  Ct07,
  Ct08,
  Ct10,
  Ct11,
  Ct13,
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
  _hdr : redirected to parent ZC_TBXUAT_HD
}
