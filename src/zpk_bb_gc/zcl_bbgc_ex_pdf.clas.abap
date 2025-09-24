CLASS zcl_bbgc_ex_pdf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      keys_bbgc     TYPE TABLE FOR ACTION IMPORT zr_TBBB_gc~btnprintpdf,
      result_bbgc   TYPE TABLE FOR ACTION RESULT zr_TBBB_gc~btnprintpdf,
      mapped_bbgc   TYPE RESPONSE FOR MAPPED EARLY zr_TBBB_gc,
      failed_bbgc   TYPE RESPONSE FOR FAILED EARLY zr_TBBB_gc,
      reported_bbgc TYPE RESPONSE FOR REPORTED EARLY zr_TBBB_gc.

    CLASS-METHODS:
      btnprintpdf_pkt
        IMPORTING keys     TYPE keys_bbgc
        EXPORTING o_pdf    TYPE string
        CHANGING  result   TYPE result_bbgc
                  mapped   TYPE mapped_bbgc
                  failed   TYPE failed_bbgc
                  reported TYPE reported_bbgc.

ENDCLASS.



CLASS zcl_bbgc_ex_pdf IMPLEMENTATION.


  METHOD btnprintpdf_pkt.


    "1 Đọc key từ RAP Action
    READ TABLE keys INDEX 1 INTO DATA(k).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    "2 Lấy dữ liệu header & detail từ CDS
    SELECT SINGLE * FROM zr_tbbb_gc
      WHERE hdrid = @k-%key-hdrid
      INTO @DATA(ls_bbgc).

    SELECT * FROM zr_tbgc_loi
      WHERE hdrid = @k-%key-hdrid
      INTO TABLE @DATA(lt_loi).


      SELECT SINGLE * FROM I_PRODUCTIONORDERITEM
      WHERE ProductionOrder = @ls_bbgc-OrderID
      INTO @DATA(ls_product).

    DATA(lv_dd)   = ls_bbgc-ngaynhaphang+6(2).
    DATA(lv_mm)   = ls_bbgc-ngaynhaphang+4(2).
    DATA(lv_yyyy) = ls_bbgc-ngaynhaphang(4).

    DATA(lv_ngaynhaphang) = |{ lv_dd }/{ lv_mm }/{ lv_yyyy }|.


    DATA(lv_dd2)   = ls_bbgc-ngaynhapkho+6(2).
    DATA(lv_mm2)   = ls_bbgc-ngaynhapkho+4(2).
    DATA(lv_yyyy2) = ls_bbgc-ngaynhapkho(4).

    DATA(lv_ngaynhapkho) = |{ lv_dd2 }/{ lv_mm2 }/{ lv_yyyy2 }|.

    DATA(lv_dd3)   = ls_bbgc-NgayTraBb+6(2).
    DATA(lv_mm3)   = ls_bbgc-NgayTraBb+4(2).
    DATA(lv_yyyy3) = ls_bbgc-NgayTraBb(4).

    DATA(lv_NgayTraBb) = |{ lv_dd3 }/{ lv_mm3 }/{ lv_yyyy3 }|.



    " Build XML

    DATA: headerxml   TYPE string,
          table1xml   TYPE string,
          section2xml TYPE string,
          table2xml   TYPE string,
          table3xml   TYPE string,
          table4xml   TYPE string,
          table5xml   TYPE string,
          table6xml   TYPE string,
          xml         TYPE string.

    SELECT SINGLE Supp~Supplier AS BPnumber,            "#EC CI_NOFIELD
                          Supp~addressid,
                          Supp~VATRegistration,
                          Supp~isonetimeaccount,
                          Supp~createdbyuser,
                          Supp~creationdate,
                          bp~creationtime
            FROM I_Supplier AS Supp
            INNER JOIN i_businesspartner AS bp ON Supp~Supplier = bp~BusinessPartner
            WHERE Supp~Supplier = @ls_bbgc-Supplier
            INTO @DATA(ls_I_Supplier)
            .

    "Customer Address
    zcl_jp_common_core=>get_address_id_details(
      EXPORTING
        addressid          = ls_I_Supplier-addressid
      IMPORTING
        o_addressiddetails = DATA(ls_addressid)
    ).

    DATA: lv_material TYPE string.
    lv_material = ls_bbgc-Material.
    SHIFT lv_material LEFT DELETING LEADING '0'.
    DATA: lv_orderID TYPE string.
    lv_orderID = ls_bbgc-OrderID.
    SHIFT lv_orderID LEFT DELETING LEADING '0'.
    DATA: lv_salesOrderItem TYPE string.
    lv_salesOrderItem = ls_product-SalesOrderItem.
    SHIFT lv_salesOrderItem LEFT DELETING LEADING '0'.
    DATA: lv_salesOrder TYPE string.
    lv_salesOrder = ls_bbgc-SalesOrder.
    SHIFT lv_salesOrder LEFT DELETING LEADING '0'.

    " Header
    headerxml =
      |<Header>| &&
      |<Title>{ ls_bbgc-LoaiHangDesc }</Title>| &&

        |<SoPo>{ ls_bbgc-sopo }</SoPo>| &&
        |<Supplier>{ ls_bbgc-supplier }</Supplier>| &&
        |<SuppilerName>{ ls_bbgc-suppliername1 }</SuppilerName>| &&
        |<SuppilerAddress>{ ls_addressid-address  }</SuppilerAddress>| &&
        |<OrderID>{ ls_bbgc-orderid }</OrderID>| &&
        |<NgayNhapHang>{ lv_ngaynhaphang }</NgayNhapHang>| &&
        |<NgayTraBb>{ lv_NgayTraBb }</NgayTraBb>| &&
        |<SalesOrder>{ ls_bbgc-SalesOrder }</SalesOrder>| &&
        |<SoBb>{ ls_bbgc-sobb }</SoBb>| &&
      |</Header>|.


    "Table1 (Nguyên liệu)
    table1xml = |<Table1>| &&
                  |<Row3>| &&
                  |<stt>1</stt>| &&
                    |<soPO>{ ls_bbgc-sopo }</soPO>| &&
                    |<lsxGiaCong>{ lv_orderID }</lsxGiaCong>| &&
                    |<soSO>{ lv_salesOrder }/{ lv_salesOrderItem }</soSO>| &&
                    |<material>{ lv_material }</material>| &&
                    |<ProductDescription>{ ls_bbgc-productdescription }</ProductDescription>| &&
                    |<tongCaiCt12>{ ls_bbgc-ct12 }</tongCaiCt12>| &&
                  |</Row3>| &&
                |</Table1>|.

    section2xml  = |<section2>| &&
|<NgayNhapKho>{ lv_ngaynhapkho }</NgayNhapKho>| &&
|</section2>|.

    "Table2 (Kiểm tra chất lượng)
    table2xml = |<Table2>| &&
                  |<Row1>| &&
                    |<Ct27>{ ls_bbgc-ct27 }</Ct27>| &&
                    |<Ct28>{ ls_bbgc-ct28 }</Ct28>| &&
                  |</Row1>| &&
                  |<Row2>| &&
                  |<soluongkiemCt13>{ ls_bbgc-ct13 }</soluongkiemCt13>| &&
                    |<tylekiemCt14>{ ls_bbgc-ct14 }</tylekiemCt14>| &&
                    |<soluongkclCt25>{ ls_bbgc-ct25 }</soluongkclCt25>| &&
                    |<tyleKCLCt26>{ ls_bbgc-ct26 }</tyleKCLCt26>| &&
                    |<Ct29>{ ls_bbgc-ct29 }</Ct29>| &&
                    |<Ct30>{ ls_bbgc-ct30 }</Ct30>| &&
                  |</Row2>| &&
                  |<Row3>| &&
                    |<Ct31>{ ls_bbgc-ct31 }</Ct31>| &&
                    |<Ct32>{ ls_bbgc-ct32 }</Ct32>| &&
                  |</Row3>| &&
                |</Table2>|.

    "Table3 (Thông tin khác)
    table3xml = |<Table3>| &&
                  |<Row1>| &&
                    |<Ct18>{ ls_bbgc-ct18 }</Ct18>| &&
                    |<Ct19>{ ls_bbgc-ct19 }</Ct19>| &&
                    |<Ct20>{ ls_bbgc-ct20 }</Ct20>| &&
                    |<Ct21>{ ls_bbgc-ct21 }</Ct21>| &&
                    |<Ct22>{ ls_bbgc-ct22 }</Ct22>| &&
                    |<Ct23>{ ls_bbgc-ct23 }</Ct23>| &&
                  |</Row1>| &&
                |</Table3>|.
    DATA: xml_row TYPE string.
    DATA: loai_loi      TYPE c LENGTH 4,
          loai_loi_desc TYPE c LENGTH 100,
          stt           TYPE string,
          last_loai_loi TYPE c LENGTH 4,
          flag          TYPE string VALUE 'X'.


    "Table4 (Ghi chú)
*    DATA: lt_loi_temp LIKE lt_loi,
*          ls_loi_temp LIKE LINE OF lt_loi,
*          lv_loai_loi TYPE zr_tbgc_loi-LoaiLoi VALUE ''.
*    LOOP AT lt_loi INTO DATA(ls_loi).
*      MOVE-CORRESPONDING ls_loi TO ls_loi_temp.
*
*      IF lv_loai_loi NE ls_loi-LoaiLoi.
*
*        APPEND ls_loi_temp TO lt_loi_temp.
*
*      ENDIF.
*
*      lv_loai_loi = ls_loi-LoaiLoi.
*
*      APPEND ls_loi_temp TO lt_loi_temp.
*
*    ENDLOOP.
*
*    CLEAR ls_loi.
*
*    LOOP AT lt_loi_temp INTO ls_loi.
*      IF ls_loi-loailoi <> last_loai_loi.
*        last_loai_loi = ls_loi-LoaiLoi.
*
*        loai_loi = ls_loi-LoaiLoi.
*        loai_loi_desc = ls_loi-LoaiLoiDesc.
*        ls_loi-GhiChu = flag.
*        CLEAR stt.
*      ELSE.
*        stt = stt + 1.
*        loai_loi = stt.
*        loai_loi_desc = ls_loi-Errordesc.
*      ENDIF.
*
*
*
*        xml_row = xml_row &&
*        |<Row1>| &&
*                    |<tt>{ loai_loi }</tt>| &&
*                    |<loaiLoi>{ loai_loi_desc }</loaiLoi>| &&
*                    |<Slloi>{ ls_loi-SlLoi }</Slloi>| &&
*                    |<Tile>{ ls_loi-tile }</Tile>| &&
*                    |<Bangi>&lt;= { ls_loi-bangi }%</Bangi>| &&
*                    |<Checkbangi>{ ls_loi-CheckBangi }</Checkbangi>| &&
*                    |<Bangii>&lt;= { ls_loi-Bangii }%</Bangii>| &&
*                    |<Checkbangii>{ ls_loi-CheckBangii }</Checkbangii>| &&
*                    |<Ghichu>{ ls_loi-GhiChu }</Ghichu>| &&
*                  |</Row1>| .
*
*        CLEAR ls_loi.
*
*      ENDLOOP.
*      table4xml = |<Table4>| &&
*      xml_row &&
*
*                  |</Table4>|.

    DATA: lt_loi_temp LIKE lt_loi,
          ls_loi_temp LIKE LINE OF lt_loi,
          lv_loai_loi TYPE zr_tbgc_loi-LoaiLoi VALUE ''.

    LOOP AT lt_loi INTO DATA(ls_loi).
      MOVE-CORRESPONDING ls_loi TO ls_loi_temp.

      IF lv_loai_loi NE ls_loi-LoaiLoi.
        APPEND ls_loi_temp TO lt_loi_temp.
      ENDIF.

      lv_loai_loi = ls_loi-LoaiLoi.
      APPEND ls_loi_temp TO lt_loi_temp.
    ENDLOOP.

    CLEAR ls_loi.

    LOOP AT lt_loi_temp INTO ls_loi.
      IF ls_loi-loailoi <> last_loai_loi.
        last_loai_loi = ls_loi-LoaiLoi.

        loai_loi = ls_loi-LoaiLoi.
        loai_loi_desc = ls_loi-LoaiLoiDesc.
        ls_loi-GhiChu = flag.
        CLEAR stt.


        xml_row = xml_row &&
        |<Row1>| &&
        |<tt>{ loai_loi }</tt>| &&
        |<loaiLoi>{ loai_loi_desc }</loaiLoi>| &&
        |<Slloi></Slloi>| &&
        |<Tile></Tile>| &&
        |<Bangi></Bangi>| &&
        |<Checkbangi></Checkbangi>| &&
        |<Bangii></Bangii>| &&
        |<Checkbangii></Checkbangii>| &&
        |<Ghichu>{ ls_loi-GhiChu }</Ghichu>| &&
        |</Row1>| .

      ELSE.
        stt = stt + 1.
        loai_loi = stt.
        loai_loi_desc = ls_loi-Errordesc.


        xml_row = xml_row &&
        |<Row1>| &&
        |<tt>{ loai_loi }</tt>| &&
        |<loaiLoi>{ loai_loi_desc }</loaiLoi>| &&
        |<Slloi>{ ls_loi-SlLoi }</Slloi>| &&
        |<Tile>{ ls_loi-tile }</Tile>| &&
        |<Bangi>&lt;= { ls_loi-bangi }%</Bangi>| &&
        |<Checkbangi>{ ls_loi-CheckBangi }</Checkbangi>| &&
        |<Bangii>&lt;= { ls_loi-Bangii }%</Bangii>| &&
        |<Checkbangii>{ ls_loi-CheckBangii }</Checkbangii>| &&
        |<Ghichu>{ ls_loi-GhiChu }</Ghichu>| &&
        |</Row1>| .
      ENDIF.

      CLEAR ls_loi.
    ENDLOOP.

    table4xml = |<Table4>| &&
          xml_row &&

                      |</Table4>|.

    "Table5 (Các lỗi chính)
    table5xml = |<Table5>| &&
                  |<Row1>| &&
                    |<loimanhCt33>{ ls_bbgc-ct33 }</loimanhCt33>| &&
                    |<loiInCt34>{ ls_bbgc-ct34 }</loiInCt34>| &&
                    |<bongKeoCt35>{ ls_bbgc-ct35 }</bongKeoCt35>| &&
                    |<banManhCt36>{ ls_bbgc-ct36 }</banManhCt36>| &&
                    |<loiConTrungCt37>{ ls_bbgc-ct37 }</loiConTrungCt37>| &&
                    |<pheThuTaiCt38>{ ls_bbgc-ct38 }</pheThuTaiCt38>| &&
                    |<loiKhacCt39>{ ls_bbgc-ct39 }</loiKhacCt39>| &&
                    |<tongLoiCtyCt40>{ ls_bbgc-ct40 }</tongLoiCtyCt40>| &&
                  |</Row1>| &&
                |</Table5>|.

    "Table6 (Các lỗi khác)
    table6xml = |<Table6>| &&
                  |<Row1>| &&
                    |<banCt41>{ ls_bbgc-ct41 }</banCt41>| &&
                    |<rachCt42>{ ls_bbgc-ct42 }</rachCt42>| &&
                    |<saiQuyCachCt43>{ ls_bbgc-ct43 }</saiQuyCachCt43>| &&
                    |<mayXauCt44>{ ls_bbgc-ct44 }</mayXauCt44>| &&
                    |<loChanKimCt45>{ ls_bbgc-ct45 }</loChanKimCt45>| &&
                    |<nhauNatCt46>{ ls_bbgc-ct46 }</nhauNatCt46>| &&
                    |<tongLoiCt47>{ ls_bbgc-ct47 }</tongLoiCt47>| &&
                  |</Row1>| &&
                |</Table6>|.

    "4. Gộp XML
    xml = |<?xml version="1.0" encoding="UTF-8"?>| &&
          |<form1>| &&
            |<Main>| &&
              |{ headerxml }| &&
              |{ table1xml }| &&
              |{ section2xml }| &&
              |{ table2xml }| &&
              |{ table3xml }| &&
              |{ table4xml }| &&
              |{ table5xml }| &&
              |{ table6xml }| &&
            |</Main>| &&
          |</form1>|.

    "5. Gọi Adobe Form
    DATA(ls_request) = VALUE zcl_gen_adobe=>ts_request( id = 'zbbgc' ).
    APPEND xml TO ls_request-data.

    DATA(o_gen_adobe) = NEW zcl_gen_adobe( ).
    DATA(lv_pdf) = o_gen_adobe->call_data( EXPORTING i_request = ls_request ).

    o_pdf = lv_pdf.

    "6. Trả file về cho RAP
    result = VALUE #( FOR key IN keys (
      %tky   = key-%tky
      %param = VALUE #(
                  filecontent   = lv_pdf
                  filename      = |BBGC_{ ls_bbgc-sobb }|
                  fileextension = 'pdf'
                  mimetype      = 'application/pdf' )
    ) ).

    " Append vào sub-table mapped-zr_tbbb_gc
    APPEND VALUE #( %tky = k-%tky ) TO mapped-zrtbbbgc.



  ENDMETHOD.
ENDCLASS.
