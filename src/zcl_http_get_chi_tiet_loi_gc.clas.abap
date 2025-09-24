CLASS zcl_http_get_chi_tiet_loi_gc DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_range_option,
             sign   TYPE c LENGTH 1,
             option TYPE c LENGTH 2,
             low    TYPE string,
             high   TYPE string,
           END OF ty_range_option,
           tt_ranges TYPE TABLE OF ty_range_option.
    CLASS-METHODS: handle_get_chitietloi CHANGING cv_message TYPE string,
      handle_unknown_case CHANGING cv_message TYPE string,
      handle_get_supplieraddress CHANGING cv_message TYPE string.

    METHODS:
      handle_clear.

    CONSTANTS: c_header_content TYPE string VALUE 'content-type',
               c_content_type   TYPE string VALUE 'application/json, charset=utf-8'.

    CLASS-DATA: g_hdrid       TYPE string,
                g_json_string TYPE string,
                g_supplier    TYPE string.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HTTP_GET_CHI_TIET_LOI_GC IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.
    DATA: lt_parameters TYPE abap_parmbind_tab.
    DATA: ls_line LIKE LINE OF lt_parameters.
    FIELD-SYMBOLS: <lv_value> TYPE any.

    me->handle_clear( ).

    DATA: lt_parts TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    DATA(lv_req_body) = request->get_text( ).

    DATA(lv_method) = request->get_header_field( '~request_method' ).

    DATA(lv_uri) = request->get_header_field( '~request_uri' ).

    SPLIT lv_uri AT '?' INTO DATA(lv_path) DATA(lv_query_string).

    SPLIT lv_query_string AT '&' INTO TABLE lt_parts.

    LOOP AT lt_parts INTO DATA(lv_pair).
      SPLIT lv_pair AT '=' INTO DATA(lv_key) DATA(lv_val).

      CASE lv_key.
        WHEN 'name'.
          DATA(lv_name) = lv_val.
        WHEN 'hdrid'.
          g_hdrid = lv_val.
        WHEN 'supplier'.
          g_supplier = lv_val.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    IF lv_method = 'post' OR lv_method = 'POST'.

      ls_line-name  = 'UV_MESSAGE' .
      ls_line-kind  = cl_abap_objectdescr=>exporting .
      ls_line-value = REF #( lv_req_body ).
      INSERT ls_line INTO TABLE lt_parameters .
    ENDIF.

    DATA(lv_dyn_method) = |handle_{ to_lower( lv_method ) }_{ to_lower( lv_name ) }|.
    TRANSLATE lv_dyn_method TO UPPER CASE.


    ls_line-name  = 'CV_MESSAGE' .
    ls_line-kind  = cl_abap_objectdescr=>changing .
    ls_line-value = REF #( g_json_string ).
    INSERT ls_line INTO TABLE lt_parameters .

    DATA(lo_self) = NEW zcl_http_get_chi_tiet_loi_gc( ).
*** Call Methods:
    TRY.
        CALL METHOD lo_self->(lv_dyn_method)
          PARAMETER-TABLE lt_parameters.
      CATCH cx_sy_dyn_call_illegal_method INTO DATA(lx_dyn).
        " Trường hợp method không tồn tại
        CALL METHOD lo_self->('HANDLE_UNKNOWN_CASE')
          PARAMETER-TABLE lt_parameters.
    ENDTRY.

*** Response
    response->set_status( '200' ).

*** Setup -> Response content-type json
    response->set_header_field( i_name = c_header_content
      i_value = c_content_type ).

    response->set_text( g_json_string ).

  ENDMETHOD.


  METHOD handle_get_chitietloi.
    DATA: et_chitietloi TYPE TABLE OF zc_tbgc_loi.
    DATA: ir_hdrid TYPE tt_ranges.
    TRANSLATE g_hdrid TO UPPER CASE.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = g_hdrid ) TO ir_hdrid.
    DATA(o_jp_get_chitietloi) = NEW zcl_jp_get_chi_tiet_loi( ).
    o_jp_get_chitietloi->get_chitietloi(
        EXPORTING
           ir_hdr_id  = ir_hdrid
        IMPORTING
            e_chitietloi = et_chitietloi ).
    cv_message = xco_cp_json=>data->from_abap( et_chitietloi )->apply( VALUE #(
    ( xco_cp_json=>transformation->underscore_to_pascal_case )
        ) )->to_string( ).
  ENDMETHOD.


  METHOD handle_clear.
    CLEAR g_hdrid.
  ENDMETHOD.


  METHOD handle_unknown_case.
    " Xử lý khi method không tồn tại
    DATA(lv_message) =  'Invalid method or parameter'.

    cv_message = xco_cp_json=>data->from_abap( lv_message )->apply( VALUE #(
        ( xco_cp_json=>transformation->underscore_to_pascal_case )
        ) )->to_string( ).
  ENDMETHOD.


  METHOD handle_get_supplieraddress.
    DATA: lv_supplier TYPE i_supplier-supplier.

    lv_supplier = g_supplier.

    SELECT SINGLE bpaddrstreetname, bpaddrcityname, supplier, addressid, AddressSearchTerm1
      FROM i_supplier
      WITH PRIVILEGED ACCESS
      WHERE supplier = @lv_supplier
      INTO @DATA(ls_address).

    IF sy-subrc = 0.
      " Address found, process ls_address
    ELSE.
      " Supplier not found, handle accordingly
    ENDIF.
    cv_message = xco_cp_json=>data->from_abap( ls_address )->apply( VALUE #(
    ( xco_cp_json=>transformation->underscore_to_pascal_case )
        ) )->to_string( ).
  ENDMETHOD.
ENDCLASS.
