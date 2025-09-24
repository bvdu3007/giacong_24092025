@EndUserText.label: 'Custom entity for Search Help'

@ObjectModel.dataCategory: #VALUE_HELP
@ObjectModel.query.implementedBy: 'ABAP:ZCL_PRODUCTIONORDER_CF'
define custom entity ZC_PRODUCTIONORDER_CF
{

      @Search: {defaultSearchElement: true, ranking: #HIGH, fuzzinessThreshold: 0.9}
  key ProductionOrder : aufnr; 

}
