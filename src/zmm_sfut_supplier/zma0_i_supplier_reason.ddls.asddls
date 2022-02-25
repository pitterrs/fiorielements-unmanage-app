@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'View Entity for Supplier Reason'
@ObjectModel.resultSet.sizeCategory: #XS
@ObjectModel.dataCategory: #VALUE_HELP
define view entity ZMA0_I_SUPPLIER_REASON
  as select from zma0_sfut_reason as Reason
{

  key reason

}
