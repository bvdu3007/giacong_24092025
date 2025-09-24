@EndUserText.label: 'BC chi tiết, TH BBGC'
@Metadata.allowExtensions: true
@ObjectModel.query.implementedBy: 'ABAP:ZCL_CTTHBBGC'
define custom entity ZC_CTTHBBGC
{
  key hdr_id                     : sysuuid_x16; // UUID
  key line_num                   : abap.int4; // Số TT

      lsx_gc                     : aufnr; // LSX gia công
      po_Subcontracting          : lifnr; // PO gia công
      company_code               : bukrs; // Company code
      so_number                  : ebeln; // Số SO
      item_type                  : abap.char(10); // Kiểu hàng
      item_id                    : matnr; // Mã hàng hóa
      item_desc                  : abap.char(255); // Mô tả mã hàng
      gc_id                      : lifnr; // Mã nhà GC
      gc_name                    : name1_gp; // Tên nhà GC
      report_no                  : zde_so_bb_gc; // Số biên bản
      import_date                : abap.dats; // Ngày nhập hàng
      entry_date                 : abap.dats; // Ngày nhập kho
      amount_on_paper            : abap.dec(10,0); // SL nhập theo chứng từ
      amount_check               : abap.dec(10,0); // Số lượng kiểm

      insect                     : abap.dec(10,0); // Lỗi côn trùng
      dirty                      : abap.dec(10,0); // Bẩn
      no_strap                   : abap.dec(10,0); // Không may quai
      bottom_edge_defect         : abap.dec(10,0); // Sụt đáy/viền
      strap_defect               : abap.dec(10,0); // Lỗi quai (làm tụt quai khi thử tải 30kg)
      seam_failure               : abap.dec(10,0); // May vỡ mặt manh
      fabric_defects             : abap.dec(10,0); // Máy cào xước màng, rách manh, rách góc
      light_stain                : abap.dec(10,0); // Bẩn nhẹ
      hem_fold_miss_4mm          : abap.dec(10,0); // Gấp miệng sai 4mm
      bottom_seam_miss_4mm       : abap.dec(10,0); // Chắp đáy sai 4mm
      bottom_edge_not_meet_requi : abap.dec(10,0); // Lỗi đáy/viền không đạt
      bottom_miss_center         : abap.dec(10,0); // Lỗi may lệch tâm đáy
      strap_off_seam             : abap.dec(10,0); // May trượt mí quai
      strap_defects              : abap.dec(10,0); // Lệch quai, vặn quai
      strap_unenven_10mm         : abap.dec(10,0); // May quai không bằng đầu 10mm
      strap_out_of_spec          : abap.dec(10,0); // May quai sai thông số
      strap_thread_break         : abap.dec(10,0); // Đứt chỉ quai
      improper_trimming          : abap.dec(10,0); // Hàng cắt chỉ không đạt
      fold_misalign_un_1_5cm     : abap.dec(10,0); // Gấp lệch 1-1.5cm
      fold_misalign_ov_1_5cm     : abap.dec(10,0); // Gấp lệch >1.5cm
      wrinkled                   : abap.dec(10,0); // Nhăn nhàu
      defect_stitch_opening      : abap.dec(10,0); // Sùi chỉ miệng, đứt chỉ miệng
      incorrect_stitch_pitch_1mm : abap.dec(10,0); // May sai bước chỉ > 1mm
      body_side_misalign_4mm     : abap.dec(10,0); // Lệch thân và hông > 4mm
      twisted_body_binding       : abap.dec(10,0); // Viền vặn thân
      incorrect_label            : abap.dec(10,0); // Thiếu tem, sai tem, thẻ bài
      short_quan_bundle          : abap.dec(10,0); // Thiếu số lượng trong bó
      excess_quan_bundle         : abap.dec(10,0); // Thừa số lượng trong bó
      zipper_defect              : abap.dec(10,0); // Bật viền, bật đầu khóa
      broken_zipper              : abap.dec(10,0); // Khóa hỏng

      ko_tem_may                 : abap.dec(10,0); // Số lượng hàng nhập không tem may
      ko_tem_dan                 : abap.dec(10,0); // Số lượng hàng nhập không tem dán
      ko_tem_treo                : abap.dec(10,0); // Số lượng hàng nhập không tem treo
      ko_tem_ban                 : abap.dec(10,0); // Số lượng hàng nhập không tem bắn
      thieu_tam_bia              : abap.dec(10,0); // Số lượng hàng nhập thiếu tấm bìa

      contrung_divat             : zde_check; // Biên bảng hiện trường: côn trùng, dị vật:
      chi_nhung_dau              : zde_check; // Biên bản hiện trường: chỉ nhúng dầu!:

      low_quality                : abap.dec(10,0); // Lỗi kém CL
      not_met_table_1            : abap.dec(10,0); // Không đạt bảng I
      not_met_table_2_mid        : abap.dec(10,0); // Không đạt bảng II (Lỗi nghiêm trọng)
      not_met_table_2_high       : abap.dec(10,0); // Không đạt bảng II (ĐBNT)
      not_met_table_2_total      : abap.dec(10,0); // Tổng số lượng hàng ko đạt bảng II
      defect_rate_exceeds_10     : abap.dec(10,0); // Tổng số lượng hàng kém chất lượng trên 10%
      Total_quality_passed       : abap.dec(10,0); // Tổng số lượng hàng đạt
      total                      : abap.dec(10,0); // Tổng Cộng
      note                       : abap.char(255); // Ghi chú

}
