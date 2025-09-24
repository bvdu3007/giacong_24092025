@Metadata.allowExtensions: true
@EndUserText.label: 'Mã lỗi theo loại hàng'
@AccessControl.authorizationCheck: #CHECK
define root view entity ZC_TB_MALOI_HANG
  provider contract transactional_query
  as projection on ZR_TB_MALOI_HANG
{
  key LoaiHang,
  key LoaiLoi,
  LoaiHangDesc,
  LoaiLoiDesc,
  CreatedBy,
  CreatedAt,
  LastChangedBy,
  LastChangedAt,
   _dtl  : redirected to composition child ZC_TB_LOI_H_DTL
}
