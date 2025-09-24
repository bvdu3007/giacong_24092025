@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZTBTRU_BS_DTL'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define view entity ZR_TBTRU_BS_DTL
  as select from ztb_tru_bs_dtl
  association        to parent ZR_TBTRU_BS as _hdr on  $projection.HdrID = _hdr.HdrID
  association [0..1] to I_ProductText    as _ProdT on $projection.Material =  _ProdT.Product and _ProdT.Language = 'E'
  association [0..1] to I_BusinessPartner   as _BP on $projection.Supplier = _BP.BusinessPartner
{  
  key hdr_id as HdrID,
  key dtl_id as DtlID,
  supplier as Supplier,
  _BP.SearchTerm1 as SupplierName,
  material as Material,
  _ProdT.ProductName,
  charcvalue as Charcvalue,
  @Semantics.quantity.unitOfMeasure: 'Materialbaseunit'
  thieu as Thieu,
  @Semantics.quantity.unitOfMeasure: 'Materialbaseunit'
  thua as Thua,
  @Semantics.quantity.unitOfMeasure: 'Materialbaseunit'
  nhap as Nhap,
  duocphep as Duocphep,
  @Semantics.quantity.unitOfMeasure: 'Materialbaseunit'
  slduocphep as Slduocphep,
  @Semantics.quantity.unitOfMeasure: 'Materialbaseunit'
  sltru as Sltru,
  dongiatru as Dongiatru,
  tongtientru as Tongtientru,
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
  last_changed_at as LastChangedAt,
  _hdr
}
