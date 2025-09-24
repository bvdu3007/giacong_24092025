CLASS zcl_jp_get_chi_tiet_loi DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_range_option,
             sign   TYPE c LENGTH 1,
             option TYPE c LENGTH 2,
             low    TYPE string,
             high   TYPE string,
           END OF ty_range_option,

           tt_ranges     TYPE TABLE OF ty_range_option,

           tt_returns    TYPE TABLE OF bapiret2,

           tt_chitietloi TYPE TABLE OF zc_tbgc_loi.
    TYPES: BEGIN OF ty_supplier_address,
             bpaddrstreetname TYPE string,
             bpaddrcityname   TYPE string,
           END OF ty_supplier_address,

           tt_supplier_address TYPE TABLE OF ty_supplier_address.


    CLASS-METHODS: get_chitietloi IMPORTING ir_hdr_id    TYPE tt_ranges
                                  EXPORTING e_chitietloi TYPE tt_chitietloi
                                            e_return     TYPE tt_returns .
    CLASS-METHODS: get_supplier_address IMPORTING ir_supplier        TYPE tt_ranges
                                        EXPORTING e_supplier_address TYPE tt_supplier_address
                                                  e_return           TYPE tt_returns .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_JP_GET_CHI_TIET_LOI IMPLEMENTATION.


  METHOD get_chitietloi.
    CLEAR e_return.

    SELECT dtlid,
           hdrid,
           loaihang,
           loailoi,
           loailoidesc,
           errorcode,
           errordesc,
           slloi,
           tile,
           bangi,
           checkbangi,
           bangii,
           checkbangii,
           ghichu,
           createdby,
           createdat,
           lastchangedby,
           lastchangedat
      FROM zr_tbgc_loi
      WHERE hdrid IN @ir_hdr_id
      INTO CORRESPONDING FIELDS OF TABLE @e_chitietloi.

  ENDMETHOD.


  METHOD get_supplier_address.
    SELECT bpaddrstreetname, bpaddrcityname, supplier FROM i_supplier
      WHERE supplier IN @ir_supplier
      INTO TABLE @DATA(lt_address).
  ENDMETHOD.
ENDCLASS.
