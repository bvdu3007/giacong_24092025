@EndUserText.label: 'create_by_per'
define abstract entity zc_create_by_per
{
  @Consumption.valueHelpDefinition:[
      { entity                       : { name : 'ZVI_PERIOD' , element: 'Zper' } }
     ]
  key  zper  : zde_period;
       @Consumption.valueHelpDefinition:[
          { entity                       : { name: 'I_CompanyCodeStdVH', element: 'CompanyCode' }
          }]
  key bukrs : bukrs;
  key lan   : zde_lan;
}
