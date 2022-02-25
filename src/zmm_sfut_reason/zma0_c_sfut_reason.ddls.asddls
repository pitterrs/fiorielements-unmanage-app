@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Supplier Follow-Up: Reason Projection'
@Metadata.allowExtensions: true

@UI: {
    headerInfo: {
        typeName: 'Reason',
        typeNamePlural: 'Reasons',
        title: { type: #STANDARD, value: 'Reason' }
    }
}

@Search.searchable: true
define root view entity ZMA0_C_SFUT_REASON
  as projection on ZMA0_I_SFUT_REASON
{


       @UI.facet: [ { id:              'Reason',
                        purpose:         #STANDARD,
                        type:            #IDENTIFICATION_REFERENCE,
                        label:           'Reason',
                        position:        10 } ]
       @UI: {
             lineItem:       [ { position: 10, importance: #HIGH } ],
             identification: [ { position: 10, label: 'Reason' } ] }
       @Search.defaultSearchElement: true
  key  reason as Reason

}
