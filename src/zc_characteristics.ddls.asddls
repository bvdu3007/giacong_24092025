@ObjectModel.usageType.serviceQuality: #B
@ObjectModel.usageType.dataClass: #MASTER
@ObjectModel.supportedCapabilities:
  [  #CDS_MODELING_DATA_SOURCE,
     #CDS_MODELING_ASSOCIATION_TARGET,
     #SQL_DATA_SOURCE                  ]
define view entity ZC_Characteristics 
  as select distinct from I_ClfnClassCharcForKeyDate ( P_KeyDate: $session.system_date ) as CharID
  left outer join I_ClfnCharcDescForKeyDate ( P_KeyDate: $session.system_date ) as CharDesc
    on CharID.CharcInternalID = CharDesc.CharcInternalID
{
  key CharID.CharcInternalID as CharcInternalId,
  CharID.Characteristic  as Characteristic,
  CharDesc.CharcDescription as CharacteristicName
}
where CharID.ClassType = '001' and CharDesc.Language = 'E'
