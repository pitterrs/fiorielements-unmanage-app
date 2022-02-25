*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_message_wrapper DEFINITION
INHERITING FROM cx_static_check.

  PUBLIC SECTION.

    INTERFACES if_abap_behv_message .

    METHODS constructor
      IMPORTING
        im_msgno    TYPE symsgno OPTIONAL
        im_attr1    TYPE string OPTIONAL
        im_attr2    TYPE string OPTIONAL
        im_attr3    TYPE string OPTIONAL
        im_attr4    TYPE string OPTIONAL
        im_previous LIKE previous OPTIONAL
        im_severity TYPE if_abap_behv_message=>t_severity OPTIONAL.

  PRIVATE SECTION.

    CONSTANTS message_class TYPE string VALUE 'ZMA0_SFUT_MESSAGE'.

ENDCLASS.
