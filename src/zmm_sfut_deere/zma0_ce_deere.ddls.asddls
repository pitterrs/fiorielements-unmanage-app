@EndUserText.label: 'Root Custom Entity for Deere Data Set'

@ObjectModel.query.implementedBy: 'ABAP:ZMA0_CL_SFUT_DEERE_RAP'

@UI: {
    headerInfo: {
        typeName: 'Order Information',
        typeNamePlural: 'Open Orders to Review',
        title: {
            type: #STANDARD
        }
    }
}

@Search.searchable: true
define root custom entity zma0_ce_deere
{
      @UI              : {
          identification : [{position: 10}],
          lineItem     : [
              {
                  position   : 10,
                  importance : #HIGH
              },
              {
                  type : #FOR_ACTION,
                  invocationGrouping: #CHANGE_SET,
                  position   : 1,
                  dataAction : 'accept',
                  label: 'Accept',
                  criticality: 'status'
              },
              {
                  type : #FOR_ACTION,
                  position   : 2,
                  dataAction : 'reject',
                  label: 'Reject',
                  criticality: 'status'
              }
          ] ,
          facet        : [
              {
                  id   : 'orderData',
                  purpose: #STANDARD,
                  position: 10,
                  label: 'Order Information',
                  type : #IDENTIFICATION_REFERENCE
              },
              {
                  type : #FIELDGROUP_REFERENCE,
                  label: 'Follow Up',
                  targetQualifier: 'fgSupplierFollowUp',
                  id   : 'supplierFollowUp',
                  position: 20
              }
          ]
      }

      @EndUserText.label: 'Purchase Document (PO)'
      @Search.defaultSearchElement: true
  key ebeln            : abap.char(10);

      @UI              : {
          lineItem     : [{position: 20, importance: #HIGH }],
          identification: [{position: 20}]
      }

      @EndUserText.label: 'Purchase Item'
  key ebelp            : abap.numc(5);

      @UI              : {
          lineItem     : [{position: 30, importance: #HIGH}],
          identification: [{position: 30}]
      }

      @EndUserText.label: 'Schedule Line'
  key etenr            : abap.numc(4);

      @EndUserText.label: 'Kanban ID'
  key pkkey            : abap.numc(10);

      @UI.masked
      @EndUserText.label: 'JIT Call Number'
  key pabnum           : abap.char(10);

      @UI.masked
      @EndUserText.label: 'JIT Call Item'
  key pabpos           : abap.numc(4);

      @UI              : {
          lineItem     : [{position: 40, importance: #MEDIUM}],
          identification: [{position: 40}],
          selectionField: [{position: 40}]
      }
      @EndUserText.label: 'Material'
      @Search.defaultSearchElement: true
      matnr            : abap.char(18);

      @UI              : {
          lineItem     : [{position: 50, importance: #MEDIUM}],
          identification: [{position: 50}]
      }
      @EndUserText.label: 'Material Description'
      maktx            : abap.char(40);

      @UI              : {
          lineItem     : [{position: 60, importance: #MEDIUM}],
          identification: [{position: 60}],
          selectionField: [{position: 60}]
      }
      @EndUserText.label: 'Plant'
      werks            : abap.char(4);

      @UI              : {
          lineItem     : [{position: 70, importance: #MEDIUM}],
          identification: [{position: 70}],
          selectionField: [{position: 70}]
      }
      @EndUserText.label: 'Plant Name'
      plant_name       : abap.char(30);

      @UI              : {
          lineItem     : [{position: 80, importance: #MEDIUM}],
          identification: [{position: 80}]
      }
      @EndUserText.label: 'Item Delivery Date'
      eindt            : abap.dats;

      @UI              : {
          lineItem     : [{
                  position: 90,
                  importance: #MEDIUM,
                  exclude: true
          }],
          identification: [{position: 90}]
      }
      @EndUserText.label: 'Kanban Indicator'
      kanba            : abap.char(1);

      @UI              : {
          lineItem     : [
              {
                  position: 100,
                  importance: #MEDIUM,
                  exclude: true
              }
          ],
          identification: [{position: 100}]
      }
      @EndUserText.label: 'MRP Area'
      berid            : abap.char(10);

      @UI.masked
      @EndUserText.label: 'MRP Area Text'
      bertx            : abap.char(40);

      @UI.masked
      @EndUserText.label: 'Vendor'
      lifnr            : abap.char(10);

      @UI.masked
      @EndUserText.label: 'Vendor Name'
      vendor_name      : abap.char(35);

      @UI.masked
      @EndUserText.label: 'Purchasing Group'
      ekgrp            : abap.char(3);

      @UI.masked
      @EndUserText.label: 'MRP Controller'
      dispo            : abap.char(3);

      @UI.masked
      @EndUserText.label: 'Ship Date'
      calc_ship_date   : abap.dats;

      @UI.masked
      @EndUserText.label: 'In-Transit Qty'
      in_transit_qty   : abap.dec(13,3);

      @UI.masked
      @EndUserText.label: 'Balance Qty ASN'
      bal_qty_w_asn    : abap.dec(13,3);

      @UI.masked
      @EndUserText.label: 'Ship Status'
      ship_status      : abap.char(50);

      @UI.masked
      @EndUserText.label: 'Order Type'
      order_type       : abap.char(50);

      @UI.masked
      @EndUserText.label: 'Deere Contact Name'
      full_name        : abap.char(50);

      @UI.masked
      @EndUserText.label: 'Deere E-mail Address'
      smtp_addr        : abap.char(50);

      @UI              : {
          lineItem     : [{
              position : 110,
              criticality:'STATUS_COLOR',
              importance : #MEDIUM
          }],
          identification : [{position: 110}]
      }

      @EndUserText.label: 'Status'
      status_txt       : zma0_sfut_status_text;

      @UI.hidden       : true
      status           : zma0_sfut_status;

      @UI.hidden       : true
      @Consumption.filter.hidden: true
      status_color     : abap.int1;

      @UI.fieldGroup   : [{
          qualifier    : 'fgSupplierFollowUp',
          position     : 120
      }]
      @EndUserText.label: 'Follow Up Date by Supplier'
      curr_date        : abap.dats;

      @UI.fieldGroup   : [{
          qualifier    : 'fgSupplierFollowUp',
          position     : 130
      }]
      @EndUserText.label: 'Current Quantity'
      curr_qty         : abap.dec(13,3);

      @UI.fieldGroup   : [{
          qualifier    : 'fgSupplierFollowUp',
          position     : 140
      }]
      @EndUserText.label: 'New Delivery Date'
      new_date         : abap.dats;

      @UI.fieldGroup   : [{
          qualifier    : 'fgSupplierFollowUp',
          position     : 150
      }]
      @EndUserText.label: 'New Quantity'
      new_qty          : abap.dec(13,3);

      @UI.fieldGroup   : [{
          qualifier    : 'fgSupplierFollowUp',
          position     : 160
      }]
      @EndUserText.label: 'Partial Delivery Date'
      partial_date     : abap.dats;

      @UI.fieldGroup   : [{
          qualifier    : 'fgSupplierFollowUp',
          position     : 170
      }]
      @EndUserText.label: 'Partial Quantity'
      partial_qty      : abap.dec(13,3);

      @UI.fieldGroup   : [{
          qualifier    : 'fgSupplierFollowUp',
          position     : 180
      }]
      @EndUserText.label: 'Reason'
      reason           : abap.char(60);

      @UI.fieldGroup   : [{
          qualifier    : 'fgSupplierFollowUp',
          position     : 190
      }]
      @EndUserText.label: 'Supplier Comment'
      supplier_comment : abap.char(255);


}
