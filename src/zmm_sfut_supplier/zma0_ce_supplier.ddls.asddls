@EndUserText.label: 'Consumption layer for supplier data'
@ObjectModel.query.implementedBy: 'ABAP:ZMA0_CL_SFUT_SUPPLIER_RAP'

@UI: {
    headerInfo: {
        typeName: 'Supplier Order Information',
        typeNamePlural: 'Open Orders',
        title: {
            type: #STANDARD,
            value: 'MAKTX'
        }
    }
}

@Search.searchable: true
define root custom entity zma0_ce_supplier
{

      @UI              : {
        identification : [{position: 10}],
        selectionField : [{position: 10}],
        fieldGroup     :[{qualifier : 'fgOrderFollowUp'}],
        lineItem       : [
        {
            position   : 10,
            criticality:'EBELN',
            importance : #HIGH
        },
        {
            type       : #FOR_ACTION,
            invocationGrouping: #CHANGE_SET,
            position   : 10,
            dataAction : 'accept',
            label      : 'Shipping on Time'
        }
        ]
        ,
        facet          : [
            {
                type   : #IDENTIFICATION_REFERENCE,
                purpose: #STANDARD,
                label  : 'Order Information',
                id     : 'orderData',
                position: 10
            },
            {
                type   : #FIELDGROUP_REFERENCE,
                label  : 'Follow Up',
                targetQualifier: 'fgSupplierFollowUp',
                id     : 'supplierFollowUp',
                position: 20
            }
        ]
      }

      @EndUserText.label: 'Purchase Document'
  key ebeln            : abap.char(10);

      @UI.masked
      @EndUserText.label: 'Purchase Item'
  key ebelp            : abap.numc(5);

      @UI.masked
      @EndUserText.label: 'Schedule Line'
  key etenr            : abap.numc(4);

      @UI.masked
      @EndUserText.label: 'Kanban Id'
  key pkkey            : abap.numc(10);

      @UI.masked
      @EndUserText.label: 'JIT Call Number'
  key pabnum           : abap.char(10);

      @UI.masked
      @EndUserText.label: 'JIT Call Item'
  key pabpos           : abap.numc(4);

      @UI              : {
        lineItem         : [{position: 20, importance: #MEDIUM}],
        identification   : [{position: 20}],
        selectionField   : [{position: 20}]
      }
      @EndUserText.label: 'PO Type'
      bsart            : abap.char(4);

      @UI              : {
        lineItem       : [{position: 30, importance: #MEDIUM}],
        identification : [{position: 30}],
        selectionField : [{position: 30}]
      }
      @EndUserText.label: 'Material'
      @Search.defaultSearchElement: true
      matnr            : abap.char(18);

      @UI              : {
        lineItem       : [{position: 40, importance: #MEDIUM}],
        identification : [{position: 40}]
      }
      @EndUserText.label: 'Material Description'
      MAKTX            : abap.char(40);

      @UI              : {
        lineItem       : [{position: 50, importance: #MEDIUM}],
        identification : [{position: 50}],
        selectionField : [{position: 50}]
      }
      @EndUserText.label: 'Plant'
      werks            : abap.char(4);

      @UI              : {
        lineItem       : [{position: 60, importance: #MEDIUM}],
        identification : [{position: 60}]
      }

      @EndUserText.label: 'Plant Name'
      plant_name       : abap.char(30);

      @UI              : {
        lineItem       : [{position: 70, importance: #MEDIUM}],
        identification : [{position: 70}]
      }
      @EndUserText.label: 'Quantity'
      bal_qty_w_asn    : abap.dec(13,3);

      @UI              : {
        lineItem       :[{
         criticality   : 'EINDT_COLOR'
        }]
      }
      @UI              : {
        lineItem       : [{position: 80, importance: #MEDIUM}],
        identification : [{position: 80}]
      }
      @EndUserText.label: 'Due Date'
      eindt            : abap.dats;

      @UI              : {
              lineItem : [{position: 90, importance: #HIGH, exclude: true}],
              identification : [{position: 90}]
            }
      @EndUserText.label: 'MRP Area'
      berid            : abap.char(10);

      @UI.masked
      @EndUserText.label: 'MRP Area Text'
      bertx            : abap.char(40);

      @UI              : {
              lineItem : [{position: 100, importance: #MEDIUM, exclude:true}],
              identification : [{position: 100}]
            }
      @Consumption.filter.hidden: true
      @EndUserText.label: 'Vendor Order From'
      lifnr            : abap.char(10);

      @UI              : {
              lineItem : [{position: 110, importance: #MEDIUM, exclude:true}],
              identification : [{position: 110}]
            }
      @Consumption.filter.hidden: true
      @EndUserText.label: 'Vendor Order From Name'
      vendor_name      : abap.char(35);

      @UI              : {
              lineItem : [{position: 120, importance: #MEDIUM, exclude:true}],
              identification : [{position: 120}]
            }
      @EndUserText.label: 'Vendor Ship From'
      ship_from        : abap.char(35);

      @UI              : {
              lineItem : [{position: 130, importance: #MEDIUM, exclude:true}],
              identification : [{position: 130}]
            }
      @EndUserText.label: 'Vendor Ship From Name'
      ship_from_name   : abap.char(35);


      @UI              : {
             lineItem  : [{position: 140, importance: #MEDIUM, exclude:true}],
             identification : [{position: 140}]
           }
      @EndUserText.label: 'Ship to Location'
      ship_to          : abap.char(10);

      @UI.masked
      @EndUserText.label: 'Purchasing Group'
      ekgrp            : abap.char(3);

      @UI.masked
      @EndUserText.label: 'MRP Controller'
      dispo            : abap.char(3);

      @UI              : {
              lineItem : [{position: 150, importance: #MEDIUM}],
              identification : [{position: 150}]
            }
      @EndUserText.label: 'Ship Date'
      calc_ship_date   : abap.dats;

      @UI              : {
              lineItem : [{position: 160, importance: #MEDIUM, exclude:true}],
              identification : [{position: 160}]
            }
      @EndUserText.label: 'In-Transit Qty'
      in_transit_qty   : abap.dec(13,3);

      @UI              : {
        lineItem       : [{position: 170, importance: #MEDIUM, exclude:true}],
        identification : [{position: 170}]
      }
      @EndUserText.label: 'Deere Contact'
      full_name        : abap.char(80);

      @UI              : {
        lineItem       : [{position: 180, importance: #MEDIUM, exclude:true}],
        identification : [{position: 180}]
      }
      @EndUserText.label: 'Deere Address'

      smtp_addr        : abap.char(241);

      @UI              : {
        lineItem       : [{position: 190, importance: #MEDIUM}],
        identification : [{position: 190}]
      }
      @EndUserText.label: 'Order Type'
      order_type_txt   : abap.char(50);

      @EndUserText.label: 'Order Type Status'
      order_type       : abap.char(5);

      @UI.masked
      @EndUserText.label: 'Ship Status'
      ship_status      : abap.char(50);

      @UI              : {
        lineItem       : [{
            position   : 200,
            criticality:'STATUS_COLOR',
            importance : #MEDIUM
        }],
        identification : [{ position: 200 }]
      }
      @EndUserText.label: 'Status'
      status_txt       : zma0_sfut_status_text;

      @UI.hidden       : true
      status           : zma0_sfut_status;

      @EndUserText.label: 'Tracking Number (CH Robinson)'
      tracking_number  : abap.char(20);

      @UI.fieldGroup   : [{ qualifier: 'fgSupplierFollowUp', position: 210 }]
      @EndUserText.label: 'Current Date'
      curr_date        : abap.dats;

      @UI.fieldGroup   : [{ qualifier: 'fgSupplierFollowUp', position: 220 }]
      @EndUserText.label: 'Current Quantity'
      curr_qty         : abap.dec(13,3);

      @UI.fieldGroup   : [{ qualifier: 'fgSupplierFollowUp', position: 230 }]
      @EndUserText.label: 'New Delivery Date'
      new_date         : abap.dats;

      @UI.fieldGroup   : [{ qualifier: 'fgSupplierFollowUp', position: 240 }]
      @EndUserText.label: 'New Quantity'
      new_qty          : abap.dec(13,3);

      @UI.fieldGroup   : [{ qualifier: 'fgSupplierFollowUp', position: 250 }]
      @EndUserText.label: 'Partial Delivery Date'
      partial_date     : abap.dats;

      @UI.fieldGroup   : [{ qualifier: 'fgSupplierFollowUp', position: 260 }]
      @EndUserText.label: 'Partial Quantity'
      partial_qty      : abap.dec(13,3);
      
      @Consumption.valueHelpDefinition: [{
        entity         : {
            name       : 'ZMA0_I_SUPPLIER_REASON' ,
            element    : 'reason'
        }}]
      @UI.fieldGroup   : [{  qualifier: 'fgSupplierFollowUp', position: 270 }]
      @EndUserText.label: 'Reason'
      reason           : abap.char(60);

      @UI.fieldGroup   : [{ qualifier: 'fgSupplierFollowUp', position: 280 }]
      @EndUserText.label: 'Supplier Comment'
      supplier_comment : abap.char(255);

      @UI.hidden       : true
      @Consumption.filter.hidden: true
      status_color     : abap.int1;

      @UI.hidden       : true
      @Consumption.filter.hidden: true
      eindt_color      : abap.int1;

}
