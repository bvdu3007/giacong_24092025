@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZTBXUAT_HD'
@EndUserText.label: 'Xuất hóa đơn'
define root view entity ZR_TBXUAT_HD
  as select from ztb_xuat_hd
  composition [0..*] of ZR_TBXN_XUAT_HD   as _xn
  composition [0..*] of ZR_TBHT_HD        as _ht
  composition [0..*] of ZR_TBPB_HD        as _pb
  association [0..1] to I_BusinessPartner as _Bus on $projection.Supplier = _Bus.BusinessPartner
  association [0..1] to I_BusinessPartner as _Bus1 on $projection.invoicingparty = _Bus1.BusinessPartner
  

{
  key hdr_id           as HdrID,
      zper             as Zper,
      zperdesc         as Zperdesc,
      lan,
      bukrs            as Bukrs,
      mahd             as Mahd,
      mahdnum,
      sohd,
      trangthai        as Trangthai,
      cast(
               case
                   when trangthai = '0' then 'Tạo mới'
                   when trangthai = '1' then 'Xác nhận'
                   when trangthai = '2' then 'Đã hạch toán'
                   when trangthai = '3' then 'Đã hủy'
                   else ''
               end
               as abap.char(15)
          )            as tt_desc,
      ngayht           as Ngayht,
      ngaydh           as Ngaydh,
      supplier         as Supplier,
      _Bus.SearchTerm1 as Searchterm1,
      supplierinvoice,
      invoicingparty ,
      _Bus1.SearchTerm1 as invoicingpartyName,
      code,
      message,
      tongtienpo,
      tongtiengr,
      thuesuat         as Thuesuat,
      tongtienxn       as Tongtienxn,
      tongtienht       as Tongtienht,
      tongtienxnst     as Tongtienxnst,
      tongtienhtst     as Tongtienhtst,
      sumdate          as Sumdate,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      sumdatetime      as Sumdatetime,
      @Semantics.user.createdBy: true
      created_by       as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at       as CreatedAt,
      @Semantics.user.localInstanceLastChangedBy: true
      last_changed_by  as LastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      last_changed_at  as LastChangedAt,
      _xn,
      _ht,
      _pb
}
