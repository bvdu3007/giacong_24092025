@EndUserText.label: 'create_by_per'
define abstract entity ZC_CREATE_BY_PER1
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
  @Consumption.valueHelpDefinition:[
          { entity                       : { name: 'zI_TaxCodeText', element: 'TaxCode' }
          }]
  key taxcode : zde_thue_suat;
}
