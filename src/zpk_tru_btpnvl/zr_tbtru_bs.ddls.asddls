@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZTBTRU_BS'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_TBTRU_BS
  as select from ztb_tru_bs
  composition [0..*] of ZR_TBTRU_BS_DTL as _dtl
{
  key hdr_id as HdrID,
  zper as Zper,
  bukrs as Bukrs,
  zperdesc as Zperdesc,
  sumdate as Sumdate,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  sumdatetime as Sumdatetime,
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
