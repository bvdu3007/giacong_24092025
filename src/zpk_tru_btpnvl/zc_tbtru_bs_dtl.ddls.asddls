@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: '###GENERATED Core Data Service Entity'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZTBTRU_BS_DTL'
}
@AccessControl.authorizationCheck: #MANDATORY
define view entity ZC_TBTRU_BS_DTL
  as projection on ZR_TBTRU_BS_DTL
{
  key HdrID,
  key DtlID,
  Supplier,
  SupplierName,
  Material,
  ProductName,
  Charcvalue,
  @Semantics: {
    quantity.unitOfMeasure: 'Materialbaseunit'
  }
  Thieu,
  @Semantics: {
    quantity.unitOfMeasure: 'Materialbaseunit'
  }
  Thua,
  @Semantics: {
    quantity.unitOfMeasure: 'Materialbaseunit'
  }
  Nhap,
  Duocphep,
  @Semantics: {
    quantity.unitOfMeasure: 'Materialbaseunit'
  }
  Slduocphep,
  @Semantics: {
    quantity.unitOfMeasure: 'Materialbaseunit'
  }
  Sltru,
  Dongiatru,
  Tongtientru,
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
     _hdr  : redirected to parent ZC_TBTRU_BS
}
