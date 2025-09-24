@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: 'Đối chiếu gia công'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZTBDCGC_DTL'
}
@AccessControl.authorizationCheck: #MANDATORY
define view entity ZC_TBDCGC_DTL
  as projection on ZR_TBDCGC_DTL

{
  key HdrID,
  key DtlID,
      Ngaynhaphang,
      Sobbgc,
      Sopo,
      OrderID,
      SalesOrder,
      Material,
      ProductDescription,
      Ct09,
      Ct10,
      Ct11,
      Ct12,
      Ct13,
      Ct14,
      Ct15,
      Ct16,
      Ct17,
      Ct18,
      Ct19,
      Ct20,
      Ct21,
      Ct22,
      Ct23,
      Ct24,
      Ct25,
      Ct26,
      Ct27,
      Ct28,
            ct29,
      ct30,
      ct31,
      ct32,
      ct33,
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
      _hdr : redirected to parent ZC_TBDCGC_HDR
}
