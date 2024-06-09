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
    METHODS zml_test_grouping1
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out .
    METHODS zml_insert_lines1
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zml_processing IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    me->zml_test_ranges_spec( out ).
    me->zml_test_ranges( out ).
    " Example: Grouping with LOOP and FOR
    me->zml_test_grouping1( out ).
    " Example demonstrates how lines are inserted into internal table
    me->zml_insert_lines1( out ).
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

  METHOD zml_test_grouping1.
    TYPES:
      BEGIN OF line,
        key TYPE i,
        num TYPE i,
      END OF line,
      itab TYPE STANDARD TABLE OF line WITH EMPTY KEY.
    DATA: numbers TYPE itab.
    DATA:
      BEGIN OF aggregate,
        sum TYPE i,
        max TYPE i,
        min TYPE i,
        avg TYPE decfloat34,
      END OF aggregate.
    DATA(keys) = 3.
    DATA(lines) = 5.
    DATA(lo_rand) = cl_abap_random=>create( ).
    numbers = VALUE #( FOR j = 1 UNTIL j > lines ( key = lo_rand->intinrange( low = 1 high = 9 ) num = lo_rand->intinrange( low = 1 high = 9 ) ) ).
    out->write( name = |Generated table:| data = numbers ).

    LOOP AT numbers ASSIGNING FIELD-SYMBOL(<wa>)
     GROUP BY ( key = <wa>-key  count = GROUP SIZE )
     ASCENDING
     ASSIGNING FIELD-SYMBOL(<group_key>).

      out->write( |Group Key: { <group_key>-key }| ).
      DATA(members) = VALUE itab( FOR m IN GROUP <group_key> ( m ) ).
      aggregate-sum = REDUCE i( INIT sum = 0
                                FOR m IN GROUP <group_key>
                                NEXT sum = sum + m-num ).
      aggregate-max = REDUCE i( INIT max = 0
                                FOR m IN GROUP <group_key>
                                NEXT max = nmax( val1 = max
                                                 val2 = m-num ) ).
      aggregate-min = REDUCE i( INIT min = 101
                                FOR m IN GROUP <group_key>
                                NEXT min = nmin( val1 = min
                                                 val2 = m-num ) ).
      aggregate-avg = aggregate-sum / <group_key>-count.

      SORT members BY num DESCENDING.
      out->write( members
        )->write( aggregate ).
    ENDLOOP.

  ENDMETHOD.

  METHOD zml_insert_lines1.
    TYPES: BEGIN OF line,
             col1 TYPE i,
             col2 TYPE i,
           END OF line.

    DATA: itab  TYPE TABLE OF line WITH EMPTY KEY,
          jtab  LIKE itab,

          itab1 TYPE TABLE OF line WITH EMPTY KEY,
          jtab1 LIKE itab,
          itab2 TYPE TABLE OF line WITH EMPTY KEY,
          jtab2 TYPE SORTED TABLE OF line
                WITH NON-UNIQUE KEY col1 col2.

    itab = VALUE #( FOR i = 1 UNTIL i > 3
                   ( VALUE #( col1 = i col2 = i ** 2 ) ) ).
    out->write( name = 'itab' data = itab ).
    jtab = VALUE #( FOR i = 1 UNTIL i > 3
                   ( VALUE #( col1 = i col2 = i ** 3 ) ) ).
    out->write( name = 'jtab' data = jtab ).

    "Insert a single line into an index table
    itab1 = itab.
    INSERT VALUE #( col1 = 11 col2 = 22 ) INTO itab1 INDEX 2.
    INSERT INITIAL LINE INTO itab1 INDEX 1.
    out->write( name = 'itab1' data = itab1 ).

    "Insert lines into an index table with LOOP
    itab1 = itab.
    LOOP AT itab1 ASSIGNING FIELD-SYMBOL(<line>).
      INSERT VALUE #( col1 = 3 * sy-tabix col2 = 5 * sy-tabix )
             INTO itab1.
    ENDLOOP.
    out->write( name = 'itab1' data = itab1 ).

    "Insert lines into an index table
    itab1 = itab.
    jtab1 = jtab.
    INSERT LINES OF itab1 INTO jtab1 INDEX 1.
    out->write( name = 'jtab1' data = jtab1 ).

    "Insert lines into a sorted table
    itab2 = itab.
    jtab2 = jtab.
    INSERT LINES OF itab2 INTO TABLE jtab2.
    out->write( name = 'jtab2' data = jtab2 ).
  ENDMETHOD.

ENDCLASS.
