@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Thông tin PO gia công'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
//@ObjectModel.representativeKey: 'PurchaseOrder'
//@Search.searchable: true
define view entity ZV_PO_GC as select distinct
from I_PurchaseOrderAPI01 as Po 
left outer join I_Supplier_VH as Sup on Po.Supplier = Sup.Supplier
left outer join I_BusinessPartner as bus on Po.Supplier = bus.BusinessPartner
left outer join I_PurOrdAccountAssignmentAPI01   as Pur on Po.PurchaseOrder = Pur.PurchaseOrder
left outer join I_ManufacturingOrderItem as Man on Pur.OrderID = Man.ManufacturingOrder
left outer join I_ProductDescription as Pdes on Man.Material = Pdes.Product 
left outer join I_Product as Pro on Man.Material = Pro.Product 

{ 
@Search.defaultSearchElement: true
    key Po.PurchaseOrder,
    Po.CompanyCode,
    @Search.defaultSearchElement: true
    Po.Supplier,
    @Search.defaultSearchElement: true
    Sup.SupplierName,
    bus.SearchTerm1,
    Pur.OrderID,
    Man.SalesOrder,
    Man.Material,
    Pdes.ProductDescription,
    cast( Pro.ProductHierarchy as abap.char( 24 ) ) as ProdUnivHierarchyNode    
    
}

where Po.PurchaseOrderType = 'ZPO4'
