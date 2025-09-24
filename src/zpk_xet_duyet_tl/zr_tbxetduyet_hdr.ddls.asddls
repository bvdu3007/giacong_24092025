@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZTBXETDUYET_HDR'
@EndUserText.label: 'Bảng xét duyệt tỷ lệ lỗi'
define root view entity ZR_TBXETDUYET_HDR
  as select from ztb_xetduyet_hdr
  composition [0..*] of ZR_TBXETDUYET_DTL as _dtl
{
  key hdr_id          as HdrID,
      zper            as Zper,
      bukrs           as Bukrs,
      lan,
      @ObjectModel.text.element: ['trangthaiDesc']
      trangthai,
      cast(
          case
              when trangthai = '0' then 'Tạo mới'
              when trangthai = '1' then 'Phê duyệt'
              when trangthai = '2' then 'Đóng'
              else ''
          end
          as abap.char(15)
      )               as trangthaiDesc,
      ct05            as Ct05,
      zperdesc        as Zperdesc,
      zstatus         as Zstatus,
      sumdate         as Sumdate,
      sumdatetime     as SumDateTime,
      @Semantics.user.createdBy: true
      created_by      as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at      as CreatedAt,
      @Semantics.user.localInstanceLastChangedBy: true
      last_changed_by as LastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      last_changed_at as LastChangedAt,
      _dtl
}
