@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZTBMAT_TYPE'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_TBMAT_TYPE
  as select from ZTB_MAT_TYPE
{
  key mattype as Mattype,
  key material as Material,
  @Semantics.user.createdBy: true
  created_by as CreatedBy,
  @Semantics.systemDateTime.createdAt: true
  created_at as CreatedAt,
  @Semantics.user.localInstanceLastChangedBy: true
  last_changed_by as LastChangedBy,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  last_changed_at as LastChangedAt
}
