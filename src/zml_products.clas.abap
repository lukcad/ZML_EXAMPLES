CLASS zml_products DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .
    METHODS read_all_products
      RETURNING VALUE(et_result) TYPE /bobf/t_epm_product_root .
    METHODS read_bykeys_products
      IMPORTING
                !iv_filter_key   TYPE /bobf/t_frw_key
      RETURNING VALUE(et_result) TYPE /bobf/t_epm_product_root .
    METHODS read_fltbyparams_products
      IMPORTING
                !iv_arr_param    TYPE /bobf/t_frw_query_selparam
      RETURNING VALUE(et_result) TYPE /bobf/t_epm_product_root .
    METHODS read_byqueryopt_products
      IMPORTING
                !iv_query_options TYPE /bobf/s_frw_query_options
      RETURNING VALUE(et_result)  TYPE /bobf/t_epm_product_root .
    METHODS read_bysql_products
      IMPORTING
                !iv_type_code    TYPE snwd_product_type_code
                !iv_dimension    TYPE t006-dimid
      RETURNING VALUE(et_result) TYPE /bobf/t_epm_product_root .

    CLASS-METHODS get_report_pbt
      IMPORTING
                !it_pbt       TYPE zml_tt_rep_pbt
      RETURNING VALUE(et_pbt) TYPE zml_tt_rep_pbt .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zml_products IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    " preparation of selection array based on dimension which was found by UOM.
    DATA: zml_arr_uom_sel TYPE /bobf/t_frw_query_selparam.
    DATA(xml_ucs) = NEW  zml_unit_conversion_simple(  ).
    DATA required_unit TYPE msehi.
    required_unit = 'TO'. "this unit belongs to dimension `MASS`
    xml_ucs->find_dimension( EXPORTING iv_langu = sy-langu iv_unit = required_unit IMPORTING ev_dimension = DATA(lv_dimension) ev_subrc = DATA(lv_subrc) ).
    IF ( lv_subrc = 0 AND lv_dimension IS NOT INITIAL ).
      SELECT t006~msehi FROM t006 WHERE dimid = @lv_dimension INTO TABLE @DATA(it_dim).
      FIELD-SYMBOLS: <it_dim_rec> LIKE LINE OF it_dim.
      LOOP AT it_dim ASSIGNING <it_dim_rec>.
        zml_arr_uom_sel = VALUE #( BASE zml_arr_uom_sel ( sign = 'I' attribute_name = 'MEASURE_UNIT' option = 'EQ' low = <it_dim_rec>-msehi  ) ).
      ENDLOOP.
    ENDIF.

    " Example of using `read_all_products` method
    out->write( read_all_products(  ) ).

    " Example of using `read_bykeys_products` method
    DATA: lv_filter_key  TYPE /bobf/t_frw_key.
    out->write( read_bykeys_products( iv_filter_key = lv_filter_key ) ).

    " Example of using `read_byqueryopt_products` method
    DATA: lv_query_options  TYPE /bobf/s_frw_query_options.
    lv_query_options-maximum_rows = 1.
    out->write( read_byqueryopt_products( iv_query_options = lv_query_options ) ).

    " Example of using `filtering parameters` method:
    "   sign can be "I" - include or "E" - exclude
    "   option(operator) can be: "EQ", "NE", "GE", "GT", "LE", "LT", "CP", "NP" "BT" "NB"
    "       "I" - include can be only as relational , not suitable for text content
    out->write( read_fltbyparams_products( iv_arr_param = VALUE #( BASE zml_arr_uom_sel "this array will add uom list selections by our required_unit
    ( sign = 'I' attribute_name = 'TYPE_CODE' option = 'EQ' low = 'AD' )
    ) ) ).

    " Example of using `read_bysql_products` method (notice: bobf keys will be empty):
    out->write( read_bysql_products( iv_dimension = lv_dimension iv_type_code = 'PR'  ) ).

    " Get report of total weight from products by product type in required UOM
    DATA zipb TYPE zml_tt_rep_pbt.
    zipb = VALUE #(
      ( type_code = 'PR' measure_unit = 'KG' weight_measure = '000.00')
      ( type_code = 'AD' measure_unit = 'G' weight_measure = '000.00')
    ).
    out->write( name = 'Total weight of Products per type:' data = get_report_pbt( it_pbt = zipb ) ).

  ENDMETHOD.

  METHOD read_all_products.
    DATA:
      lo_srv_mgr  TYPE REF TO /bobf/if_tra_service_manager,
      lo_tran_mgr TYPE REF TO /bobf/if_tra_transaction_mgr.

    lo_srv_mgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key = /bobf/if_epm_product_c=>sc_bo_key ).
    lo_tran_mgr = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

    lo_srv_mgr->query(
      EXPORTING
        iv_query_key            = /bobf/if_epm_product_c=>sc_query-root-select_all
        iv_fill_data            = abap_true
      IMPORTING
        et_data                 =  et_result
    ).
  ENDMETHOD.

  METHOD read_bykeys_products.
    DATA:
      lo_srv_mgr  TYPE REF TO /bobf/if_tra_service_manager,
      lo_tran_mgr TYPE REF TO /bobf/if_tra_transaction_mgr.

    lo_srv_mgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key = /bobf/if_epm_product_c=>sc_bo_key ).
    lo_tran_mgr = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

    lo_srv_mgr->query(
      EXPORTING
        iv_query_key            = /bobf/if_epm_product_c=>sc_query-root-select_by_elements
        it_filter_key           = iv_filter_key
        iv_fill_data            = abap_true
      IMPORTING
        et_data                 =  et_result
    ).
  ENDMETHOD.

  METHOD read_byqueryopt_products.
    DATA:
      lo_srv_mgr  TYPE REF TO /bobf/if_tra_service_manager,
      lo_tran_mgr TYPE REF TO /bobf/if_tra_transaction_mgr.

    lo_srv_mgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key = /bobf/if_epm_product_c=>sc_bo_key ).
    lo_tran_mgr = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

    lo_srv_mgr->query(
      EXPORTING
        iv_query_key            = /bobf/if_epm_product_c=>sc_query-root-select_by_elements
        is_query_options        = iv_query_options
        iv_fill_data            = abap_true
      IMPORTING
        et_data                 =  et_result
    ).
  ENDMETHOD.

  METHOD read_fltbyparams_products.
    DATA:
      lo_srv_mgr  TYPE REF TO /bobf/if_tra_service_manager,
      lo_tran_mgr TYPE REF TO /bobf/if_tra_transaction_mgr.

    lo_srv_mgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key = /bobf/if_epm_product_c=>sc_bo_key ).
    lo_tran_mgr = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

    lo_srv_mgr->query(
      EXPORTING
        iv_query_key            = /bobf/if_epm_product_c=>sc_query-root-select_by_elements
        it_selection_parameters = iv_arr_param
        iv_fill_data            = abap_true
      IMPORTING
        et_data                 =  et_result
    ).
  ENDMETHOD.


  METHOD read_bysql_products.

    DATA:
    lt_data TYPE /bobf/t_epm_product_root.
    SELECT b~msehi FROM t006 AS b WHERE b~dimid = @iv_dimension INTO TABLE @DATA(lt_msehi).
    SELECT * FROM /bobf/d_pr_root AS a WHERE a~type_code = @iv_type_code AND a~measure_unit IN ( SELECT b~msehi FROM @lt_msehi AS b ) INTO TABLE @DATA(lv_result).
    IF sy-subrc = 0.
      MOVE-CORRESPONDING lv_result TO et_result.
    ENDIF.


  ENDMETHOD.

  METHOD get_report_pbt.

    TYPES: BEGIN OF st_pbt_dim,
             dimension TYPE dimid.
             INCLUDE TYPE zml_rep_pbt_t.
    TYPES: END OF st_pbt_dim.

    DATA: lt_pbt_dim TYPE STANDARD TABLE OF st_pbt_dim.
    DATA: lt_pbt_vrm TYPE STANDARD TABLE OF st_pbt_dim.
    DATA: lt_result TYPE zml_tt_rep_pbt.
    DATA: lt_int_result TYPE zml_tt_rep_pbt.
    DATA: required_unit TYPE msehi.
    DATA: zweight_in TYPE p LENGTH 10 DECIMALS 3.
    DATA: zweight_out TYPE p LENGTH 10 DECIMALS 3.

    DATA(zml_ucs) = NEW  zml_unit_conversion_simple(  ).
    lt_pbt_dim = CORRESPONDING #( it_pbt ).
    LOOP AT lt_pbt_dim ASSIGNING FIELD-SYMBOL(<ls_pbt_dim>).
      required_unit = <ls_pbt_dim>-measure_unit.
      zml_ucs->find_dimension( EXPORTING iv_langu = sy-langu iv_unit = required_unit IMPORTING ev_dimension = DATA(lv_dimension_dim) ev_subrc = DATA(lv_subrc_dim) ).
      IF ( lv_subrc_dim = 0 ).
        <ls_pbt_dim>-dimension = lv_dimension_dim.
      ENDIF.
    ENDLOOP.

    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE @lt_pbt_vrm
    FROM /bobf/d_pr_root AS pr_root
    FOR ALL ENTRIES IN @lt_pbt_dim
    WHERE pr_root~type_code = @lt_pbt_dim-type_code AND measure_unit IN ( SELECT b~msehi FROM t006 AS b WHERE b~dimid = @lt_pbt_dim-dimension ).

    LOOP AT lt_pbt_dim ASSIGNING FIELD-SYMBOL(<ls_pbtdim>).
      required_unit = <ls_pbtdim>-measure_unit.
      LOOP AT lt_pbt_vrm ASSIGNING FIELD-SYMBOL(<ls_pbt_vrm>) where dimension = <ls_pbtdim>-dimension.
        zweight_in = <ls_pbt_vrm>-weight_measure.
        zml_ucs->unit_convertor(
          EXPORTING
            iv_msehi_in  = <ls_pbt_vrm>-measure_unit
            iv_msehi_out = required_unit
            iv_input     = zweight_in
          IMPORTING
            ev_subrc     = DATA(ev_subrc)
            ev_output    = zweight_out
        ).
        IF ( ev_subrc = 0 ).
          <ls_pbt_vrm>-measure_unit = required_unit.
          <ls_pbt_vrm>-weight_measure = zweight_out.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    SELECT
        a~type_code,
        a~measure_unit,
        SUM( a~weight_measure ) AS weight_measure
      FROM @lt_pbt_vrm AS a
      GROUP BY a~type_code, a~measure_unit
      INTO TABLE @DATA(lv_pbt).

    et_pbt = CORRESPONDING #( lv_pbt ).

  ENDMETHOD.

ENDCLASS.
