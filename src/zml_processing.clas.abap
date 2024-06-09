CLASS zml_processing DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
    METHODS zml_test_ranges_spec
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out .
    METHODS zml_test_ranges
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zml_processing IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    me->zml_test_ranges_spec( out ).
    me->zml_test_ranges( out ).
  ENDMETHOD.


  METHOD zml_test_ranges_spec.
    DATA: text TYPE c LENGTH 8 VALUE '12345678'.
    FIELD-SYMBOLS: <fs1> TYPE any,
                   <fs2> TYPE any.
    out->write( |TEST RANGES SPEC| ).
    ASSIGN text+3(3) TO <fs1>.

    DO 8 TIMES.
      ASSIGN <fs1>(sy-index) TO <fs2>.
      IF <fs2> IS ASSIGNED.
        out->write( |{ <fs2> }| ).
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD zml_test_ranges.
    TYPES: BEGIN OF sub_struc,
             col1 TYPE c LENGTH 10,
             col2 TYPE c LENGTH 10,
           END OF sub_struc.

    DATA BEGIN OF struc.
    INCLUDE TYPE: sub_struc AS comp1 RENAMING WITH SUFFIX _1,
                  sub_struc AS comp2 RENAMING WITH SUFFIX _2,
                  sub_struc AS comp3 RENAMING WITH SUFFIX _3,
                  sub_struc AS comp4 RENAMING WITH SUFFIX _4,
                  sub_struc AS comp5 RENAMING WITH SUFFIX _5.
    DATA END OF struc.

    FIELD-SYMBOLS <sub> TYPE sub_struc.

    out->write( |TEST RANGES| ).

    struc = VALUE #( col1_1 = 'col1_1'  col2_1 = 'col2_1'
                     col1_2 = 'col1_2'  col2_2 = 'col2_2'
                     col1_3 = 'col1_3'  col2_3 = 'col2_3'
                     col1_4 = 'col1_4'  col2_4 = 'col2_4'
                     col1_5 = 'col1_5'  col2_5 = 'col2_5' ).

    DATA inc TYPE i.

    WHILE sy-subrc = 0.
      inc = sy-index  - 1.
      ASSIGN struc-comp1 INCREMENT inc TO <sub> CASTING
                                                RANGE struc.
      IF sy-subrc = 0.
        out->write( <sub> ).
      ENDIF.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.
