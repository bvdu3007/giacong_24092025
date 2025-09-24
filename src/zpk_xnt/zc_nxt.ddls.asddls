@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Bảng tồn'
//@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity zc_nxt
  provider contract transactional_query as projection on zi_nxt
 
{
    key Material,
    key Plant,
    key Supplier,
    key Orderid,
    key Zper,
    @Semantics.quantity.unitOfMeasure : 'materialbaseunit'
    Quantityinbaseunit,
    Materialbaseunit
}
