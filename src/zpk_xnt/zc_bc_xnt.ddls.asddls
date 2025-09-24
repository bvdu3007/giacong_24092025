@EndUserText.label: 'Báo cáo xuất nhập tồn'
@ObjectModel: {
    query: {
            implementedBy: 'ABAP:ZCL_DATA_BC_XNT' }
    }
@Metadata.allowExtensions: true
@UI.presentationVariant: [{
    sortOrder: [
      { by: 'orderid',   direction: #ASC },
      { by: 'supplier', direction: #ASC },
      { by: 'material', direction: #ASC }
    ]
}]

define custom entity ZC_BC_XNT
  // with parameters parameter_name : parameter_type
{   
//  key DateFR : abap.dats;
//  key DateTo : abap.dats;
      @Consumption.valueHelpDefinition:[
          { entity                       : { name : 'ZVI_PERIOD' , element: 'Zper' } }
         ]
  key zper : zde_period;
  key material       : matnr ;
  key plant          : werks_d ;
  key supplier       : elifn ;
  key orderid        : aufnr ;
  ProductDescription : maktx;
  SupplierName          : name1_gp;
  @Semantics.quantity.unitOfMeasure : 'materialbaseunit'
  DauKy : menge_d;
  @Semantics.quantity.unitOfMeasure : 'materialbaseunit'
  XuatTKy : menge_d;
  @Semantics.quantity.unitOfMeasure : 'materialbaseunit'
  NhapTKy : menge_d;
  @Semantics.quantity.unitOfMeasure : 'materialbaseunit'
  SLBTPNVLDaNhapVe : menge_d;
  @Semantics.quantity.unitOfMeasure : 'materialbaseunit'
  NhapTraBTPDat : menge_d;
  @Semantics.quantity.unitOfMeasure : 'materialbaseunit'
  BTPLoi : menge_d;
  @Semantics.quantity.unitOfMeasure : 'materialbaseunit'
  NhapTraBTPLoiCTy : menge_d;
  @Semantics.quantity.unitOfMeasure : 'materialbaseunit'
  NhapTraBTPLoiGC : menge_d;
  @Semantics.quantity.unitOfMeasure : 'materialbaseunit'
  NhapTruBTPThieu : menge_d;
  @Semantics.quantity.unitOfMeasure : 'materialbaseunit'
  TonCuoi : menge_d;
  materialbaseunit   : meins;
  DonHangVet : abap.char( 1 );
  SalesOrder : abap.char( 15 );
  BTPSauMay : matnr;
  TenBTPSauMay : maktx;
}
