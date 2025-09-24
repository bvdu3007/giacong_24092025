CLASS zcl_productionorder_cf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_PRODUCTIONORDER_CF IMPLEMENTATION.


  METHOD if_rap_query_provider~select.

    DATA business_data TYPE TABLE OF ZC_PRODUCTIONORDER_CF .

    DATA business_data_line TYPE ZC_PRODUCTIONORDER_CF .

    DATA(top)     = io_request->get_paging( )->get_page_size( ).
    DATA(skip)    = io_request->get_paging( )->get_offset( ).

    DATA(requested_fields)  = io_request->get_requested_elements( ).

    DATA(sort_order)    = io_request->get_sort_elements( ).

            IF top IS NOT INITIAL.
          DATA(max_index) = top + skip.
        ELSE.
          max_index = 0.
        ENDIF.

    TRY.
        DATA(filter_condition_string) = io_request->get_filter( )->get_as_sql_string( ).
*        DATA(filter_condition_ranges) = io_request->get_filter( )->get_as_ranges(  ).

*        READ TABLE filter_condition_ranges WITH KEY name = 'PRODUCTIONORDER'
*               INTO DATA(lt_PRODUCTIONORDER).
*
*        READ TABLE filter_condition_ranges WITH KEY name = 'PRODUCTIONORDERTEXT'
*               INTO DATA(lt_PRODUCTIONORDERTEXT).

        SELECT ProductionOrder
        FROM I_ProductionOrder
        WHERE (filter_condition_string)
*        ProductionOrder in @lt_PRODUCTIONORDER-range
*        AND ProductionOrderText in @lt_PRODUCTIONORDERTEXT-range
        AND ( ProductionOrderType = '1014' OR ProductionOrderType = '2012' )
        into TABLE @business_data
        UP TO @max_index ROWS.

        IF skip IS NOT INITIAL.
          DELETE business_data TO skip.
        ENDIF.

        io_response->set_total_number_of_records( lines( business_data ) ).
        io_response->set_data( business_data ).

      CATCH cx_root INTO DATA(exception).
        DATA(exception_message) = cl_message_helper=>get_latest_t100_exception( exception )->if_message~get_longtext( ).

        DATA(exception_t100_key) = cl_message_helper=>get_latest_t100_exception( exception )->t100key.


    ENDTRY.
  ENDMETHOD.
ENDCLASS.
