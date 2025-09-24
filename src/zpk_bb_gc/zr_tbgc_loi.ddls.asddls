@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZTBGC_LOI'
@EndUserText.label: 'Biên bản gia công'
define view entity ZR_TBGC_LOI
  as select from ztb_gc_loi 
  association        to parent ZR_TBBB_GC as _hdr on  $projection.HdrID = _hdr.HdrID
{ 
  key hdr_id as HdrID,
  key dtl_id as DtlID,
  loai_hang as LoaiHang,
  loai_loi as LoaiLoi,
  cast(
    case
        when loai_loi = 'A' then 'Các loại lỗi đặc biệt nghiêm trọng'
        when loai_loi = 'B' then 'Các loại lỗi nghiêm trọng'
        when loai_loi = 'C' then 'Các loại lỗi kém chất lượng'
        else ''
    end
    as abap.char(50)
) as LoaiLoiDesc,
  error_code as ErrorCode,
  errordesc as Errordesc,
  sl_loi as SlLoi,
  tile   ,
  bangi as Bangi,
  check_bangi as CheckBangi,
  bangii as Bangii,
  check_bangii as CheckBangii,
  ghi_chu as GhiChu,
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
