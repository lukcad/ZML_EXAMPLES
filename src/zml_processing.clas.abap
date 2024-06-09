CLASS zml_processing DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    METHODS zml_test_ranges.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zml_processing IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    me->zml_test_ranges( ).

  ENDMETHOD.

  METHOD zml_test_ranges.
    DATA text TYPE c LENGTH 8 VALUE '12345678'.

    FIELD-SYMBOLS: <fs1> TYPE any,
                   <fs2> TYPE any.

    ASSIGN text+3(3) TO <fs1>.

    DO 8 TIMES.
      ASSIGN <fs1>(sy-index) TO <fs2>.
      IF <fs2> IS ASSIGNED.
        cl_demo_output=>write_text( |{ <fs2> }| ).
      ENDIF.
    ENDDO.
    cl_demo_output=>display( 'test' ).
  ENDMETHOD.

ENDCLASS.
