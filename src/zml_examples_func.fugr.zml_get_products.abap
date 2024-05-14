FUNCTION zml_get_products.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_REP_PBT) TYPE  ZML_TT_REP_PBT
*"  EXPORTING
*"     REFERENCE(ET_REP_PBT) TYPE  ZML_TT_REP_PBT
*"----------------------------------------------------------------------
  et_rep_pbt = zml_products=>get_report_pbt( it_pbt = it_rep_pbt ).


ENDFUNCTION.
