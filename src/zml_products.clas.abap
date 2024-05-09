CLASS zml_products DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF tt_ZML_BP_stru,
                     iv_value TYPE string,
           END OF tt_ZML_BP_stru.
    TYPES: zml_arr_values TYPE STANDARD TABLE OF tt_ZML_BP_stru WITH EMPTY KEY.
    INTERFACES if_oo_adt_classrun .
    METHODS read_all_products
      RETURNING VALUE(et_result) TYPE /bobf/t_epm_product_root .
    METHODS read_bykeys_products
      RETURNING VALUE(et_result) TYPE /bobf/t_epm_product_root .
    METHODS read_fltbyparams_products
      IMPORTING
                !iv_arr_param    TYPE /bobf/t_frw_query_selparam
      RETURNING VALUE(et_result) TYPE /bobf/t_epm_product_root .

    METHODS read_byqueryopt_products
      RETURNING VALUE(et_result) TYPE /bobf/t_epm_product_root .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zml_products IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    DATA: zml_arr_uom_sel TYPE /bobf/t_frw_query_selparam.
    DATA(xml_ucs) = NEW  zml_unit_conversion_simple(  ).

    DATA required_unit type msehi.
    required_unit = 'TO'. "this unit belongs to dimension `MASS`

    xml_ucs->find_dimension( EXPORTING iv_langu = sy-langu iv_unit = required_unit IMPORTING ev_dimension = data(lv_dimension) ev_subrc = data(lv_subrc) ).
    if ( lv_subrc = 0 and lv_dimension is not INITIAL ).

      select t006~msehi from t006 where dimid = @lv_dimension into table @data(it_dim).
      field-SYMBOLS: <it_dim_rec> like line of it_dim.
      loop at it_dim assigning <it_dim_rec>.
        zml_arr_uom_sel = VALUE #( base zml_arr_uom_sel ( sign = 'I' attribute_name = 'MEASURE_UNIT' option = 'EQ' low = <it_dim_rec>-msehi  ) ).
      endloop.

    ENDIF.


    out->write( read_all_products(  ) ).

    out->write( read_bykeys_products(  ) ).

    out->write( read_byqueryopt_products(  ) ).


    DATA: zml_arr TYPE zml_arr_values.
    zml_arr  = VALUE #( ( iv_value = 'KG' ) ).
    " Example of using of filtering parameters:
    "   sign can be "I" - include or "E" - exclude
    "   option(operator) can be: "EQ", "NE", "GE", "GT", "LE", "LT", "CP", "NP" "BT" "NB"
    "       "I" - iclude can be only as relational , not suitable for text content
    out->write( read_fltbyparams_products( iv_arr_param = VALUE #( base zml_arr_uom_sel "this array will add uom list selections by our required_unit
    ( sign = 'I' attribute_name = 'TYPE_CODE' option = 'EQ' low = '1' )
    ) ) ).

  ENDMETHOD.

  METHOD read_all_products.
    DATA:
      lo_srv_mgr  TYPE REF TO /bobf/if_tra_service_manager,
      lo_tran_mgr TYPE REF TO /bobf/if_tra_transaction_mgr.
    DATA:
      lt_data TYPE /bobf/t_epm_product_root.


    lo_srv_mgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key = /bobf/if_epm_product_c=>sc_bo_key ).
    lo_tran_mgr = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

    lo_srv_mgr->query(
      EXPORTING
        iv_query_key            = /bobf/if_epm_product_c=>sc_query-root-select_all
*        it_filter_key           =
*        it_selection_parameters =
*        is_query_options        =
        iv_fill_data            = abap_true
*        it_requested_attributes =
      IMPORTING
*        eo_message              =
*        es_query_info           =
        et_data                 =  et_result
*        et_key                  = data(lv_key)
    ).
  ENDMETHOD.

  METHOD read_bykeys_products.
    DATA:
      lo_srv_mgr  TYPE REF TO /bobf/if_tra_service_manager,
      lo_tran_mgr TYPE REF TO /bobf/if_tra_transaction_mgr.
    DATA:
      lt_data TYPE /bobf/t_epm_product_root.

    DATA:
       lv_filter_key  TYPE /bobf/t_frw_key.




    lo_srv_mgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key = /bobf/if_epm_product_c=>sc_bo_key ).
    lo_tran_mgr = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

    lo_srv_mgr->query(
      EXPORTING
        iv_query_key            = /bobf/if_epm_product_c=>sc_query-root-select_by_elements
        it_filter_key           = lv_filter_key
*        it_selection_parameters =
*        is_query_options        =
        iv_fill_data            = abap_true
*        it_requested_attributes =
      IMPORTING
*        eo_message              =
*        es_query_info           =
        et_data                 =  et_result
*        et_key                  = data(lv_key)
    ).
  ENDMETHOD.

  METHOD read_byqueryopt_products.
    DATA:
      lo_srv_mgr  TYPE REF TO /bobf/if_tra_service_manager,
      lo_tran_mgr TYPE REF TO /bobf/if_tra_transaction_mgr.
    DATA:
      lt_data TYPE /bobf/t_epm_product_root.

    DATA:
        lv_query_options  TYPE /bobf/s_frw_query_options .

    lv_query_options-maximum_rows = 1.


    lo_srv_mgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key = /bobf/if_epm_product_c=>sc_bo_key ).
    lo_tran_mgr = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

    lo_srv_mgr->query(
      EXPORTING
        iv_query_key            = /bobf/if_epm_product_c=>sc_query-root-select_by_elements
*        it_filter_key           = lv_filter_key
*        it_selection_parameters =
        is_query_options        = lv_query_options
        iv_fill_data            = abap_true
*        it_requested_attributes =
      IMPORTING
*        eo_message              =
*        es_query_info           =
        et_data                 =  et_result
*        et_key                  = data(lv_key)
    ).
  ENDMETHOD.

  METHOD read_fltbyparams_products.
    DATA:
      lo_srv_mgr  TYPE REF TO /bobf/if_tra_service_manager,
      lo_tran_mgr TYPE REF TO /bobf/if_tra_transaction_mgr.
    DATA:
      lt_data TYPE /bobf/t_epm_product_root.

    DATA:
      lv_selection_parameters  TYPE /bobf/t_frw_query_selparam.
    FIELD-SYMBOLS:
      <iv_param> LIKE LINE OF iv_arr_param.

    LOOP AT iv_arr_param ASSIGNING <iv_param>.
      lv_selection_parameters = VALUE #( BASE lv_selection_parameters ( <iv_param>  ) ).

    ENDLOOP.


    lo_srv_mgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key = /bobf/if_epm_product_c=>sc_bo_key ).
    lo_tran_mgr = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

    lo_srv_mgr->query(
      EXPORTING
        iv_query_key            = /bobf/if_epm_product_c=>sc_query-root-select_by_elements
*        it_filter_key           =
        it_selection_parameters = lv_selection_parameters
*        is_query_options        =
        iv_fill_data            = abap_true
*        it_requested_attributes =
      IMPORTING
*        eo_message              =
*        es_query_info           =
        et_data                 =  et_result
*        et_key                  = data(lv_key)
    ).
  ENDMETHOD.

ENDCLASS.
