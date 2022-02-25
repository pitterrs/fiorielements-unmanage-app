@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Supplier Follow-Up: Supplier plants'
define root view entity ZMA0_I_SFUT_PLANT
    as select from zma0_sfut_plant {
    key supplier as Supplier,
    key plant as Plant
}
