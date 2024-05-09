CLASS zml_unit_conversion_simple DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
TYPES: zdec_tt TYPE p LENGTH 10 DECIMALS 3.

    INTERFACES if_oo_adt_classrun .
    METHODS find_dimension
      IMPORTING
        !iv_langu     TYPE sy-langu
        !iv_unit      TYPE t006-msehi
      EXPORTING
        !ev_dimension TYPE t006-dimid
        !ev_text      TYPE t006t-txdim
        !ev_subrc     TYPE sy-subrc.

    METHODS unit_convertor
      IMPORTING
        !iv_msehi_in  TYPE msehi
        !iv_msehi_out TYPE msehi
        !iv_input TYPE zdec_tt
      EXPORTING
        !ev_subrc TYPE sy-subrc
        !ev_output TYPE zdec_tt.



  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zml_unit_conversion_simple IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    " using `find_dimensions`
    data lv_unit type msehi.
    lv_unit = 'KG'.
    me->find_dimension( EXPORTING
    iv_langu = sy-langu iv_unit = lv_unit
    IMPORTING
    ev_dimension = DATA(lv_dimension) ).
    out->write( |For unit `{ lv_unit }` dimension is: { lv_dimension }|  ).


    " using `unit_convertor`
    DATA: lv_msehi_in  TYPE msehi,
          lv_msehi_out TYPE msehi.
    DATA: lv_input  TYPE p LENGTH 10 DECIMALS 3.
    lv_input = '128000.500'.
    lv_msehi_in  = 'KG'.
    lv_msehi_out = 'TO'.
    me->unit_convertor( EXPORTING iv_input = lv_input iv_msehi_in = lv_msehi_in iv_msehi_out = lv_msehi_out IMPORTING ev_output = data(lv_output) ).
    out->write( name = |Convernted value `{ lv_input }` from `{ lv_msehi_in }` TO `{ lv_msehi_out }`:| data = lv_output ).

  ENDMETHOD.



  METHOD find_dimension.
    CALL FUNCTION 'DIMENSION_GET_FOR_UNIT'
      EXPORTING
        language  = iv_langu
        unit      = iv_unit
      IMPORTING
        dimension = ev_dimension
        text      = ev_text.
      ev_subrc = sy-subcs.
  ENDMETHOD.

  METHOD unit_convertor.
    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        input                = iv_input
        no_type_check        = 'X'
        round_sign           = ' '
        unit_in              = iv_msehi_in
        unit_out             = iv_msehi_out
      IMPORTING
        output               = ev_output
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
      ev_subrc = sy-subcs.
  ENDMETHOD.

ENDCLASS.
