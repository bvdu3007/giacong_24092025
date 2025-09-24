@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: 'Phân bổ hóa đơn'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZTBPB_HD'
}
@AccessControl.authorizationCheck: #MANDATORY
define view entity ZC_TBPB_HD
  as projection on ZR_TBPB_HD
{
  key HdrID,
  key PbhdID,
      Material,
      Productdescription,
      Sopo,
      Poitem,
      Purchaseorderitemtext,
      purchasinghistorydocumentyear,
      purchasinghistorydocument,
      purchasinghistorydocumentitem,
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
      _hdr : redirected to parent ZC_TBXUAT_HD_HT
}
