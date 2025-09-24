@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'Mã lỗi theo loại hàng'
define view entity ZR_TB_LOI_H_DTL
  as select from ztb_loi_h_dtl
  association to parent ZR_TB_MALOI_HANG as _hdr on $projection.LoaiHang = _hdr.LoaiHang 
and $projection.LoaiLoi = _hdr.LoaiLoi 
   { 
  key loai_hang as LoaiHang,  
  key error_code as ErrorCode,
  key loai_loi as LoaiLoi,
  errordesc,
  bangi,
  bangii,
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
