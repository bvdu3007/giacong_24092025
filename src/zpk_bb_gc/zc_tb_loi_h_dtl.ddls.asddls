@Metadata.allowExtensions: true
@EndUserText.label: 'Mã lỗi theo loại hàng'
@AccessControl.authorizationCheck: #CHECK
define view entity ZC_TB_LOI_H_DTL
  as projection on ZR_TB_LOI_H_DTL
{ 
  key LoaiHang,
  key ErrorCode,
  key LoaiLoi,
  errordesc,
  bangi,
  bangii,
  CreatedBy,
  CreatedAt,
  LastChangedBy,
  LastChangedAt,
  _hdr  : redirected to parent ZC_TB_MALOI_HANG
  
}
