@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: 'Bảng xét duyệt tỷ lệ lỗi'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZTBXETDUYET_DTL'
}
@AccessControl.authorizationCheck: #MANDATORY
define view entity ZC_TBXETDUYET_DTL
  as projection on ZR_TBXETDUYET_DTL
{
  key HdrID,
  key DtlID,
      Supplier,
      SupplierName,
      KieuTui,
      TgianHdong,
      Ct04,
      Ct05,
      Ct06,
      Ct07,
      Ct08,
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
      ct23a,
      Ct24,
      Ct25,
      Ct26,
      Ct27,
      Ct28,
      Ct29a,
      Ct29,
      Ct291,
      Ct292,
      Ct30,
      Ct31,
      Ct32,
      Ct33,
      Ct34,
      Ct35,
      Ct36,
      Ct37,
      Ct38,
      Ct38a,
      Ct39,
      ct39a,
      ct39a1,
      ct401,
      ct40,
      ct40a,
      ct40b,
      ct40c,
      ct40d,
      ct40e,
      ct40f,
      Ct41,
      Ct42,
      Ct43,
      Ct44,
      Ct45,
      Ct46,
      Ct47,
      Ct48,
      Ct49,
      Ct50,
      Ct411,
      Ct421,
      Ct431,
      Ct441,
      Ct451,
      Ct461,
      Ct471,
      Ct481,
      Ct491,
      Ct501,
      Ct51,
      Ct52,
      Ct53,
      ct531,
      Ghichu,
      Ct55,
      Ct56,
      Ct57,
      Ct58,
      Ct59,
      Ct60,
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
      _hdr : redirected to parent ZC_TBXETDUYET_HDR
}
