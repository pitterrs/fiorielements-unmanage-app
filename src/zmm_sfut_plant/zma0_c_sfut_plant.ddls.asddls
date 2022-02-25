@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Supplier Follow-Up: Supplier plants'
@Metadata.allowExtensions: true
@UI: {
    headerInfo: {
        typeName: 'Plant',
        typeNamePlural: 'Plants',
        title: { type: #STANDARD, value: 'Plant' }
    }
}
@Search.searchable: true
define root view entity ZMA0_C_SFUT_PLANT
  as projection on ZMA0_I_SFUT_PLANT
{
      @UI.facet: [ { id:              'Plant',
                          purpose:         #STANDARD,
                          type:            #IDENTIFICATION_REFERENCE,
                          label:           'Plant',
                          position:        10 } ]
      @UI: {
            lineItem:       [ { position: 10, importance: #HIGH } ],
            identification: [ { position: 10, label: 'Supplier' } ] }
      @Search.defaultSearchElement: true
  key Supplier,
        @UI: {
            lineItem:       [ { position: 20, importance: #HIGH } ],
            identification: [ { position: 20, label: 'Plant' } ] }
      @Search.defaultSearchElement: true
  key Plant
}
