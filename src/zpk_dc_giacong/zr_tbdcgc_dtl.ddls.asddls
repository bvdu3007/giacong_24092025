@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZTBDCGC_DTL'
@EndUserText.label: 'Đối chiếu gia công'
define view entity ZR_TBDCGC_DTL
  as select from ztb_dcgc_dtl
  association        to parent ZR_TBDCGC_HDR as _hdr on $projection.HdrID = _hdr.HdrID
  association [0..1] to ZV_PO_GC             as _Po  on $projection.Sopo = _Po.PurchaseOrder
{
  key hdr_id                                              as HdrID,
  key dtl_id                                              as DtlID,
      ngaynhaphang                                        as Ngaynhaphang,
      sobbgc                                              as Sobbgc,
      sopo                                                as Sopo,
      _Po.OrderID                                         as OrderID,
      _Po.SalesOrder                                      as SalesOrder,
      cast( ltrim( _Po.Material, '0' ) as abap.char(40) ) as Material,
      _Po.ProductDescription,
      ct09                                                as Ct09,
      ct10                                                as Ct10,
      ct11                                                as Ct11,
      ct12                                                as Ct12,
      ct13                                                as Ct13,
      ct14                                                as Ct14,
      ct15                                                as Ct15,
      ct16                                                as Ct16,
      ct17                                                as Ct17,
      ct18                                                as Ct18,
      ct19                                                as Ct19,
      ct20                                                as Ct20,
      ct21                                                as Ct21,
      ct22                                                as Ct22,
      ct23                                                as Ct23,
      ct24                                                as Ct24,
      ct25                                                as Ct25,
      ct26                                                as Ct26,
      ct27                                                as Ct27,
      ct28                                                as Ct28,
      ct29,
      ct30,
      ct31,
      ct32,
      ct33,
      @Semantics.user.createdBy: true
      created_by                                          as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at                                          as CreatedAt,
      @Semantics.user.localInstanceLastChangedBy: true
      last_changed_by                                     as LastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      last_changed_at                                     as LastChangedAt,
      _hdr
}
