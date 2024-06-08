CLASS zml_sflight DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zml_sflight IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    TYPES wa TYPE sflight.

    DATA itab TYPE TABLE OF wa WITH EMPTY KEY.

    SELECT *
           FROM sflight
           WHERE carrid = 'LH'
                 "AND connid = `0400`
                 "AND fldate BETWEEN @( sy-datum - ( 365 * 10 ) ) and @( sy-datum )
           INTO CORRESPONDING FIELDS OF TABLE @itab.
    out->write( itab ).

  ENDMETHOD.

ENDCLASS.
