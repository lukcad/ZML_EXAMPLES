CLASS zml_sflight DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
    METHODS itab_to_json_output
      IMPORTING
                !it_tab        TYPE ANY TABLE
      RETURNING VALUE(ev_json) TYPE string .
    METHODS itab_to_xml_output
      IMPORTING
                !it_tab        TYPE ANY TABLE
      RETURNING VALUE(ev_xml) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zml_sflight IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    TYPES: wa TYPE sflight.
    DATA: itab TYPE TABLE OF wa WITH EMPTY KEY.

    SELECT *
           FROM sflight
           WHERE carrid = 'LH'
           INTO CORRESPONDING FIELDS OF TABLE @itab.

    " Example of representation the extracted data as a TABLE
    out->write( itab ).

    " Example of representation the extracted data as an JSON.
    out->write( data = me->itab_to_json_output( itab ) ).

    " Example of representation the extracted data as XML.
    out->write( data = me->itab_to_xml_output( itab ) ).

  ENDMETHOD.

  METHOD itab_to_json_output.

    DATA: lv_json   TYPE /ui2/cl_json=>json.

    lv_json = /ui2/cl_json=>serialize( data          = it_tab
                                       pretty_name   = /ui2/cl_json=>pretty_mode-camel_case
                                       compress      = abap_true
                                      ).
    IF ( sy-subrc = 0 ).
      ev_json = lv_json.
    ENDIF.

  ENDMETHOD.

  METHOD itab_to_xml_output.

    DATA lv_xml TYPE string.

    CALL TRANSFORMATION id
      SOURCE model = it_tab
      RESULT XML lv_xml.
    IF ( sy-subrc = 0 ).
      ev_xml = lv_xml.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
