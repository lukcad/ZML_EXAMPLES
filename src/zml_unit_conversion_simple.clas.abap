CLASS zml_unit_conversion_simple DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
    METHODS find_dimension
      IMPORTING
        !iv_langu     TYPE sy-langu
        !iv_unit      TYPE t006-msehi
      EXPORTING
        !ev_dimension TYPE t006-dimid
        !ev_text      TYPE t006t-txdim
        !ev_subrc     TYPE sy-subrc.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zml_unit_conversion_simple IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    DATA: lv_msehi_in  TYPE msehi,
          lv_msehi_out TYPE msehi.

    DATA: lv_input  TYPE p LENGTH 10 DECIMALS 3,
          lv_output TYPE p LENGTH 10 DECIMALS 3.

    me->find_dimension( EXPORTING
    iv_langu = sy-langu iv_unit = 'KG'
    IMPORTING
    ev_dimension = DATA(lv_dimension) ).

    out->write( lv_dimension ).

    lv_input = '128000.500'.

    lv_msehi_in  = 'KG'.
    lv_msehi_out = 'TO'.

    DATA(rv_result) = abap_true.
* Function module to check if the given units are convertible or not
    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        input                = lv_input
        no_type_check        = 'X'
        round_sign           = ' '
        unit_in              = lv_msehi_in
        unit_out             = lv_msehi_out
      IMPORTING
*       ADD_CONST            =
*       DECIMALS             =
*       DENOMINATOR          =
*       NUMERATOR            =
        output               = lv_output
      EXCEPTIONS
        conversion_not_found = 1
        division_by_zero     = 2
        input_invalid        = 3
        output_invalid       = 4
        overflow             = 5
        type_invalid         = 6
        units_missing        = 7
        unit_in_not_found    = 8
        unit_out_not_found   = 9.
    IF sy-subrc EQ 0.
      "rv_result = abap_false.
      out->write( lv_output ).
    ENDIF.


  ENDMETHOD.



  METHOD find_dimension.

    ev_subrc = 0.
    CALL FUNCTION 'DIMENSION_GET_FOR_UNIT'
      EXPORTING
        language  = iv_langu
        unit      = iv_unit
      IMPORTING
        dimension = ev_dimension
        text      = ev_text.
    IF sy-subrc <> 0.
      ev_subrc = sy-subcs.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
