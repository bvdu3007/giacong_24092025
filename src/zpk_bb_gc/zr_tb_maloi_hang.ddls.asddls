@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'Mã lỗi theo loại hàng'
define root view entity ZR_TB_MALOI_HANG
  as select from ztb_maloi_hang
  composition [0..*] of ZR_TB_LOI_H_DTL as _dtl 
  
{
   @Consumption.valueHelpDefinition:[
      { entity                       : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
      additionalBinding              : [{ element: 'domain_name',
                localConstant        : 'ZDE_LOAI_HANG', usage: #FILTER }]
                , distinctValues     : true
      }]
  key loai_hang as LoaiHang,
     @Consumption.valueHelpDefinition:[
      { entity                       : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
      additionalBinding              : [{ element: 'domain_name',
                localConstant        : 'ZDE_LOAI_LOI', usage: #FILTER }]
                , distinctValues     : true
      }]
 key loai_loi as LoaiLoi,
  cast(
    case
        when loai_hang = '1' then 'Hàng ống'
        when loai_hang = '2' then 'Hàng viền'
        else ''
    end
    as abap.char(15)
) as LoaiHangDesc,
cast(
    case
        when loai_loi = 'A' then 'Các loại lỗi đặc biệt nghiêm trọng'
        when loai_loi = 'B' then 'Các loại lỗi nghiêm trọng'
        when loai_loi = 'C' then 'Các loại lỗi kém chất lượng'
        else ''
    end
    as abap.char(50)
) as LoaiLoiDesc,
  @Semantics.user.createdBy: true
  created_by as CreatedBy,
  @Semantics.systemDateTime.createdAt: true
  created_at as CreatedAt,
  @Semantics.user.localInstanceLastChangedBy: true
  last_changed_by as LastChangedBy,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  last_changed_at as LastChangedAt,
  _dtl
  
}
