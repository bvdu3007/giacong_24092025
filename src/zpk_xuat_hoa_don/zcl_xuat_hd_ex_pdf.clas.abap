CLASS zcl_xuat_hd_ex_pdf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      keys_xhd     TYPE TABLE FOR ACTION IMPORT zr_tbxuat_hd~btnPrintPDF,
      result_xhd   TYPE TABLE FOR ACTION RESULT zr_tbxuat_hd~btnprintpdf,
      mapped_xhd   TYPE RESPONSE FOR MAPPED EARLY zr_tbxuat_hd,
      failed_xhd   TYPE RESPONSE FOR FAILED EARLY zr_tbxuat_hd,
      reported_xhd TYPE RESPONSE FOR REPORTED EARLY zr_tbxuat_hd.

    CLASS-METHODS:
      btnprintpdf_pkt
        IMPORTING keys     TYPE keys_xhd
        EXPORTING o_pdf    TYPE string
        CHANGING  result   TYPE result_xhd
                  mapped   TYPE mapped_xhd
                  failed   TYPE failed_xhd
                  reported TYPE reported_xhd.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_xuat_hd_ex_pdf IMPLEMENTATION.
  METHOD btnprintpdf_pkt.
    "1 Đọc key từ RAP Action
    READ TABLE keys INDEX 1 INTO DATA(k).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

*    select du lieu
    SELECT SINGLE * FROM zr_tbpb_hd
    WHERE HdrID = @k-%key-HdrID
    INTO @DATA(ls_tbpb).

    SELECT SINGLE * FROM zr_tbxuat_hd
    WHERE HdrID = @k-%key-hdrID
    INTO @DATA(ls_tbxuat).

    " Lấy thông tin company code, trong đó có addressid
    SELECT SINGLE companycode,
                  companycodename,
                  addressid,
                  country
      FROM I_CompanyCode
      WHERE companycode = @ls_tbxuat-Bukrs
      INTO @DATA(ls_company).

    IF sy-subrc = 0.

      " Lấy địa chỉ chi tiết từ addressid
      zcl_jp_common_core=>get_address_id_details(
        EXPORTING
          addressid          = ls_company-addressid
        IMPORTING
          o_addressiddetails = DATA(ls_addressid)
      ).
    ENDIF.
    SELECT SINGLE * FROM I_address_2
    WHERE AddressID = @ls_company-AddressID
    INTO @DATA(ls_address).

    SELECT SINGLE * FROM I_CoCodeCountryVATRegistration
    WHERE CompanyCode = @ls_tbxuat-Bukrs
    INTO @DATA(ls_cocode).

    select * from ztb_pb_hd
    where hdr_id = @ls_tbxuat-HdrID
    into table @data(lt_pbhd).


    "--- Lấy thông tin Business Partner (Customer / Supplier)
    zcl_jp_common_core=>get_businesspartner_details(
      EXPORTING
        i_document = VALUE #(
                      supplier       = ls_tbxuat-supplier
                       )
      IMPORTING
        o_BPdetails = DATA(ls_bpdetails)
    ).

    select single * from I_BuPaIdentification
    where BusinessPartner = @ls_tbxuat-Supplier
    into @data(ls_bpvat).

*replace các ký tự gây lỗi
DATA(lv_bpname) = ls_bpdetails-bpname.
REPLACE ALL OCCURRENCES OF '&' IN lv_bpname WITH '&amp;'.
REPLACE ALL OCCURRENCES OF '<' IN lv_bpname WITH '&lt;'.
REPLACE ALL OCCURRENCES OF '>' IN lv_bpname WITH '&gt;'.


    " Build XML

    DATA: headerxml TYPE string,
          table1xml TYPE string,
          xml       TYPE string.




    " Header
    headerxml =
      |<Header>| &&
      |<SoPhieu>{ ls_tbxuat-Searchterm1 } - { ls_tbxuat-Zper } - { ls_tbxuat-mahd }</SoPhieu>| &&
      |<DonViGGC>{ ls_company-CompanyCodeName }</DonViGGC>| &&
      |<DiaChiGGC>{ ls_addressid-address }, { ls_addressid-addressname }, { ls_company-Country }</DiaChiGGC>| &&
      |<MstGGC>{ ls_cocode-VATRegistration }</MstGGC>| &&
      |<DonViNGC>{ lv_bpname }</DonViNGC>| &&
      |<DiaChiNGC> { ls_bpdetails-bpaddress }</DiaChiNGC>| &&
      |<MstNGC>{ ls_bpvat-BPIdentificationNumber }</MstNGC>| &&
|</Header>|.

LOOP AT lt_pbhd into data(ls_pbhd).
    table1xml = table1xml &&
    |<Section1>| &&
        |<stt> { sy-tabix } </stt>| &&
        |<tenHangHoa> { ls_pbhd-productdescription+6 } </tenHangHoa>| &&
**        |<dvt> {  } </dvt>| &&
**        |<soLuong> {  } </soLuong>| &&
**        |<donGia> {  } </donGia>| &&
**        |<thanhTien> {  } </thanhTien>| &&
**        |<congTienHang> {  } </congTienHang>| &&
**        |<thueGTGT> {  } </thueGTGT>| &&
**        |<tongThanhToan> {  } </tongThanhToan>| &&

     |</Section1>|.
     ENDLOOP.

    "4. Gộp XML
    xml = |<?xml version="1.0" encoding="UTF-8"?>| &&
          |<form1>| &&
            |<Main>| &&
              |{ headerxml }| &&
              |{ table1xml }| &&
            |</Main>| &&
          |</form1>|.

    "5. Gọi Adobe Form
    DATA(ls_request) = VALUE zcl_gen_adobe=>ts_request( id = 'zxuathoadon' ).
    APPEND xml TO ls_request-data.

    DATA(o_gen_adobe) = NEW zcl_gen_adobe( ).
    DATA(lv_pdf) = o_gen_adobe->call_data( EXPORTING i_request = ls_request ).

    o_pdf = lv_pdf.

    "6. Trả file về cho RAP
    result = VALUE #( FOR key IN keys (
      %tky   = key-%tky
      %param = VALUE #(
                  filecontent   = lv_pdf
                  filename      = |xuathoadon|
                  fileextension = 'pdf'
                  mimetype      = 'application/pdf' )
    ) ).

    " Append vào sub-table mapped-zr_tbbb_gc
    APPEND VALUE #( %tky = k-%tky ) TO mapped-ZrTbxuatHd.

  ENDMETHOD.

ENDCLASS.
