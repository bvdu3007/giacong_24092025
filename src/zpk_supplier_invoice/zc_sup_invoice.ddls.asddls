@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Supplier invoice'
@Metadata.ignorePropagatedAnnotations: true
define root view entity zc_sup_invoice as select from ztb_supinvoice
{
    key invoice_id as InvoiceId,
    trangthai as Trangthai
}
