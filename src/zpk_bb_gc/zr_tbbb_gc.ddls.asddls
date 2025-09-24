@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZTBBB_GC'
@EndUserText.label: 'Biên bản gia công'
define root view entity ZR_TBBB_GC
  as select from ztb_bb_gc
  composition [0..*] of ZR_TBGC_LOI as _dtl
  association [0..1] to ZV_PO_GC    as _Po on $projection.SoPo = _Po.PurchaseOrder
{
  key hdr_id                                              as HdrID,
      @Consumption.valueHelpDefinition:[
         { entity                       : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
         additionalBinding              : [{ element: 'domain_name',
                   localConstant        : 'ZDE_LOAI_HANG', usage: #FILTER }]
                   , distinctValues     : true
         }]

      @ObjectModel.text.element: ['LoaiHangDesc']
      loai_hang                                           as LoaiHang,

      cast(
           case
               when loai_hang = '1' then 'Hàng ống'
               when loai_hang = '2' then 'Hàng viền'
               else ''
           end
           as abap.char(15)
      )                                                   as LoaiHangDesc,

      so_bb                                               as SoBb,
      so_bb_base                                               as SoBbBase,
      so_bb_num                                           as SoBbNum,
      so_bb_sub                                           as SoBbSub,
      ngay_lap_bb                                         as NgayLapBb,
      @ObjectModel.text.element: ['trangthaiDesc']
      trangthai,
       cast(
           case
               when trangthai = '0' then 'Tạo mới'
               when trangthai = '1' then 'Đóng'
               else ''
           end
           as abap.char(15)
      )                                                   as trangthaiDesc,
      @Search.defaultSearchElement   : true
      @Consumption.valueHelpDefinition:[
      { entity                       : { name : 'ZV_PO_GC' , element: 'PurchaseOrder' }
      }]
      @ObjectModel.foreignKey.association: '_Po'
      so_po                                               as SoPo,
      _Po.CompanyCode,
      // Lấy Supplier trực tiếp từ association _Po
      _Po.Supplier                                        as Supplier,
      _Po.SearchTerm1                                     as SupplierName1,
      // Lấy tên Supplier từ association _Sup
      _Po.SupplierName,
      _Po.OrderID                                         as OrderID,
      _Po.SalesOrder                                      as SalesOrder,
      ngay_nhap_hang                                      as NgayNhapHang,
      ngay_tra_bb                                         as NgayTraBb,
      ngay_nhap_kho                                       as NgayNhapKho,
      _Po.Material, //cast( ltrim( _Po.Material, '0' ) as abap.char(40) ) as Material,
      //      _Po.Material    as Material,
      _Po.ProductDescription,
      _Po.ProdUnivHierarchyNode,

      ct12                                                as Ct12,
      ct13                                                as Ct13,
      ct14                                                as Ct14,
      ct16                                                as Ct16,
      ghi_chu                                             as GhiChu,
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
      ct29                                                as Ct29,
      ct30                                                as Ct30,
      ct31                                                as Ct31,
      ct32                                                as Ct32,
      ct321                                               as Ct321,
      ct322                                               as Ct322,
      ct323                                               as Ct323,
      ct324                                               as Ct324,
      bs01,
      bs02,
      bs03,
      bs04,
      bs05,
      bs06,
      bs07,
      ct33                                                as Ct33,
      ct34                                                as Ct34,
      ct35                                                as Ct35,
      ct36                                                as Ct36,
      ct37                                                as Ct37,
      ct38                                                as Ct38,
      ct39                                                as Ct39,
      ct40                                                as Ct40,
      ct41                                                as Ct41,
      ct42                                                as Ct42,
      ct43                                                as Ct43,
      ct44                                                as Ct44,
      ct45                                                as Ct45,
      ct46                                                as Ct46,
      ct47                                                as Ct47,
      ct48                                                as Ct48,
      @Semantics.user.createdBy: true
      created_by                                          as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at                                          as CreatedAt,
      @Semantics.user.localInstanceLastChangedBy: true
      last_changed_by                                     as LastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      last_changed_at                                     as LastChangedAt,
      _dtl,
      _Po
}
