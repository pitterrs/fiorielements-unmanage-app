@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Supplier Follow-Up: Reason Data model'
define root view entity ZMA0_I_SFUT_REASON
  as select from zma0_sfut_reason as Reason
{

  key reason
}
