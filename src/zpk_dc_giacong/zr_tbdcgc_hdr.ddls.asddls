@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZTBDCGC_HDR'
@EndUserText.label: 'Đối chiếu gia công'
define root view entity ZR_TBDCGC_HDR
  as select from ztb_dcgc_hdr
  composition [0..*] of ZR_TBDCGC_DTL     as _dtl
  association [0..1] to I_BusinessPartner as _Bus on $projection.Supplier = _Bus.BusinessPartner
{
  key hdr_id          as HdrID,
      zper            as Zper,
      zperdesc        as Zperdesc,
      lan,
      bukrs           as Bukrs,
      supplier        as Supplier,
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
      )                                                   as trangthaiDesc,
      _Bus.SearchTerm1,
      sumdate         as Sumdate,
      sumdatetime     as Sumdatetime,
      ct01            as Ct01,
      ct02            as Ct02,
      ct03            as Ct03,
      ct011           as Ct011,
      ct021           as Ct021,
      ct031           as Ct031,
      ct03a,
      ct03b,
      ct03a1,
      ct03b1,
      ct04            as Ct04,
      ct05            as Ct05,
      ct06            as Ct06,
      ct07            as Ct07,
      ct08            as Ct08,
      ct09            as Ct09,
      ct10,
      ct11,
      ct12,
      ct13,
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
