CLASS zcl_data_bc_xnt DEFINITION
  PUBLIC
  INHERITING FROM cx_rap_query_provider
*  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "Custom Entities
    INTERFACES if_rap_query_provider.

    TYPES: BEGIN OF ty_range_option,
             sign   TYPE c LENGTH 1,
             option TYPE c LENGTH 2,
             low    TYPE string,
             high   TYPE string,
           END OF ty_range_option,

           tt_ranges  TYPE TABLE OF ty_range_option,

           tt_rp_data TYPE TABLE OF zc_bc_xnt.

    "Variable
    CLASS-DATA: gt_data TYPE TABLE OF zc_bc_xnt.

    CLASS-DATA: gr_companycode TYPE tt_ranges,
                gr_fiscalyear  TYPE tt_ranges,
                mo_instance    TYPE REF TO zcl_data_bc_xnt.

    CLASS-METHODS:
      "Contructor
      get_Instance RETURNING VALUE(ro_instance) TYPE REF TO zcl_data_bc_xnt.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DATA_BC_XNT IMPLEMENTATION.


  METHOD get_instance.
    mo_instance = ro_instance = COND #( WHEN mo_instance IS BOUND
                                           THEN mo_instance
                                           ELSE NEW #( ) ).
  ENDMETHOD.


  METHOD if_rap_query_provider~select.

**--- Custom Entities ---**
    DATA: ls_page_info TYPE zcl_jp_common_core=>st_page_info,
          ir_material  TYPE tt_ranges,
          ir_plant     TYPE tt_ranges,
          ir_supplier  TYPE tt_ranges,
          ir_orderid   TYPE tt_ranges
          .

    DATA: lt_data TYPE tt_rp_data.
    FREE: lt_data.

    TRY.
        DATA(lo_so_cttgnh)  = zcl_data_bc_xnt=>get_instance( ).

        DATA(lo_common_app) = zcl_jp_common_core=>get_instance( ).

        lo_common_app->get_fillter_app(
            EXPORTING
                io_request  = io_request
                io_response = io_response
            IMPORTING
                wa_page_info          = ls_page_info
        ).

        TRY.
            DATA(lr_ranges) = ls_page_info-ro_filter->get_as_ranges( ).
          CATCH cx_rap_query_filter_no_range.
            "handle exception
        ENDTRY.

*        READ TABLE lr_ranges WITH KEY  name = 'DATEFR' INTO DATA(ls_ranges).
*        IF sy-subrc IS INITIAL.
*          READ TABLE ls_ranges-range INDEX 1 INTO DATA(ls_DateFR).
*        ENDIF.
*
*        READ TABLE lr_ranges WITH KEY  name = 'DATETO' INTO ls_ranges.
*        IF sy-subrc IS INITIAL.
*          READ TABLE ls_ranges-range INDEX 1 INTO DATA(ls_DateTo).
*        ENDIF.

        READ TABLE lr_ranges WITH KEY  name = 'ZPER' INTO DATA(ls_ranges).
        IF sy-subrc IS INITIAL.
          READ TABLE ls_ranges-range INDEX 1 INTO DATA(ls_ZPER).
        ENDIF.

        READ TABLE lr_ranges WITH KEY  name = 'MATERIAL' INTO ls_ranges.
        IF sy-subrc IS INITIAL.
          MOVE-CORRESPONDING ls_ranges-range TO ir_material.
        ENDIF.

        READ TABLE lr_ranges WITH KEY  name = 'PLANT' INTO ls_ranges.
        IF sy-subrc IS INITIAL.
          MOVE-CORRESPONDING ls_ranges-range TO ir_plant.
        ENDIF.

        READ TABLE lr_ranges WITH KEY  name = 'SUPPLIER' INTO ls_ranges.
        IF sy-subrc IS INITIAL.
          MOVE-CORRESPONDING ls_ranges-range TO ir_supplier.
        ENDIF.
        READ TABLE lr_ranges WITH KEY  name = 'ORDERID' INTO ls_ranges.

        IF sy-subrc IS INITIAL.
          MOVE-CORRESPONDING ls_ranges-range TO ir_orderid.
        ENDIF.

        DATA: lw_Date_fr TYPE d,
              lw_Date_to TYPE d.

        select SINGLE * from ztb_period
            WHERE ZPER = @ls_ZPER-low
             INTO @DATA(ls_period).

        lw_Date_fr = ls_period-zdatefr .
        lw_Date_to = ls_period-zdateto .

        CALL METHOD zcl_xnt=>get_xnt
          EXPORTING
            i_datefr    = lw_Date_fr
            i_dateto    = lw_Date_to
            ir_material = ir_material
            ir_plant    = ir_plant
            ir_supplier = ir_supplier
            ir_orderid  = ir_orderid
          IMPORTING
            e_nxt       = gt_data.

        LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
          SELECT SINGLE ProductDescription FROM I_ProductDescription
              WHERE Product = @<fs_data>-material
               INTO @<fs_data>-ProductDescription .
          SELECT SINGLE SearchTerm1 FROM I_BusinessPartner
              WHERE BusinessPartner = @<fs_data>-supplier
               INTO @<fs_data>-SupplierName .
          SELECT SINGLE SearchTerm1 FROM I_BusinessPartner
             WHERE BusinessPartner = @<fs_data>-supplier
              INTO @<fs_data>-SupplierName .

          SELECT SINGLE  Product FROM i_productionorderitem
          WHERE ProductionOrder = @<fs_data>-orderid
           INTO @<fs_data>-BTPSauMay .


          SELECT SINGLE ProductName FROM i_producttext
            WHERE Product = @<fs_data>-BTPSauMay
             INTO @<fs_data>-TenBTPSauMay .

        ENDLOOP.

        DATA(lv_total_records) = lines( gt_data ).
    DATA(lt_result) = gt_data.
    DATA(lo_paging) = io_request->get_paging( ).
    IF lo_paging IS BOUND.
      DATA(top) = lo_paging->get_page_size( ).
      IF top < 0. " -1 means all records
        top = lv_total_records.
      ENDIF.
      DATA(skip) = lo_paging->get_offset( ).

      IF skip >= lv_total_records.
        CLEAR lt_result. " Offset is beyond the total number of records
      ELSEIF top = 0.
        CLEAR lt_result. " No records requested
      ELSE.
        " Calculate the actual range to keep
        DATA(lv_start_index) = skip + 1. " ABAP uses 1-based indexing
        DATA(lv_end_index) = skip + top.

        " Ensure end index doesn't exceed table size
        IF lv_end_index > lv_total_records.
          lv_end_index = lv_total_records.
        ENDIF.

        " Create a new table with only the required records
        DATA: lt_paged_result LIKE lt_result.
        CLEAR lt_paged_result.

        " Copy only the required records
        DATA(lv_index) = lv_start_index.
        WHILE lv_index <= lv_end_index.
          APPEND lt_result[ lv_index ] TO lt_paged_result.
          lv_index = lv_index + 1.
        ENDWHILE.

        lt_result = lt_paged_result.
      ENDIF.
    ENDIF.

        IF io_request->is_total_numb_of_rec_requested( ).
          io_response->set_total_number_of_records( lines( lt_result ) ).
        ENDIF.

        IF io_request->is_data_requested( ).
          io_response->set_data( lt_result ).
        ENDIF.

      CATCH cx_root INTO DATA(exception).

        DATA(exception_message) = cl_message_helper=>get_latest_t100_exception( exception )->if_message~get_longtext( ).

        DATA(exception_t100_key) = cl_message_helper=>get_latest_t100_exception( exception )->t100key.

        RAISE EXCEPTION TYPE zcl_data_bc_xnt
          EXPORTING
            textid   = VALUE scx_t100key(
            msgid = exception_t100_key-msgid
            msgno = exception_t100_key-msgno
            attr1 = exception_t100_key-attr1
            attr2 = exception_t100_key-attr2
            attr3 = exception_t100_key-attr3
            attr4 = exception_t100_key-attr4 )
            previous = exception.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
