@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Bảng tồn'
@Metadata.ignorePropagatedAnnotations: true
define root view entity zi_nxt as select from ztb_nxt
{
    key material as Material,
    key plant as Plant,
    key supplier as Supplier,
    key orderid as Orderid,
    key zper as Zper,
    @Semantics.quantity.unitOfMeasure : 'materialbaseunit'
    quantityinbaseunit as Quantityinbaseunit,
    materialbaseunit as Materialbaseunit
}
