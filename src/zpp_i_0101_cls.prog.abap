*___Class Definition_________________________________________________*

*___Class Main ______________________________________________________*
CLASS lcl_main DEFINITION .
  PUBLIC SECTION .
    TYPES: BEGIN OF ty_main,
             matnr TYPE matnr,
             werks TYPE werks_d,
             matkl TYPE matkl,
             dispo TYPE dispo,
           END OF ty_main.

    TYPES :
      tt_mrp_report TYPE STANDARD TABLE OF zpp_s_mrp_report WITH DEFAULT KEY,
      tt_main       TYPE STANDARD TABLE OF ty_main WITH DEFAULT KEY.

    DATA :
      mt_data_ozet  TYPE tt_mrp_report,
      mt_data_detay TYPE tt_mrp_report,
      mt_data       TYPE tt_mrp_report.

    METHODS :
      run,
      get_data,
      get_data_detay,
      get_data_ozet,
      filter_data.

    CLASS-METHODS:
      date_get_week
        IMPORTING
          i_date        TYPE  scdatum
        RETURNING
          VALUE(r_week) TYPE kweek
        EXCEPTIONS
          date_invalid.

  PRIVATE SECTION.
    CONSTANTS : mc_red(4)    VALUE '@0A@',
                mc_yellow(4) VALUE '@09@',
                mc_green(4)  VALUE '@08@'.
    METHODS:
      get_data_from_mard
        IMPORTING
          it_mara        TYPE tt_main
        RETURNING
          VALUE(rt_mard) TYPE mard_tt,
      get_data_from_makt
        IMPORTING
          it_mara        TYPE tt_main
        RETURNING
          VALUE(rt_makt) TYPE makt_itab,
      modify_data
        IMPORTING
          it_mard TYPE mard_tt
        CHANGING
          ct_data TYPE lcl_main=>tt_mrp_report,
      get_data_from_material
        RETURNING
          VALUE(rt_mara) TYPE tt_main.


ENDCLASS.

*__ Class Event _____________________________________________________*
CLASS lcl_event_handler DEFINITION .
  PUBLIC SECTION .
    DATA : mv_event(3).
    "SEL : selection screen
    "HDR : Header



    METHODS:
      constructor
        IMPORTING
          iv_event TYPE char03,

      handle_print_top_of_list FOR EVENT print_top_of_list OF cl_gui_alv_grid ,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,
      after_refresh FOR EVENT after_refresh OF cl_gui_alv_grid
        IMPORTING sender .

  PRIVATE SECTION.
    DATA :
      _mv_sw_sum TYPE flag,
      _mv_sw_det TYPE flag VALUE 'X'.

    METHODS:
      _change_switch
        IMPORTING
          i_sw_sum TYPE flag
          i_sw_det TYPE flag.

ENDCLASS.


*__ Class View ______________________________________________________*
CLASS lcl_report_view DEFINITION .
  PUBLIC SECTION .
    CONSTANTS: c_str_h TYPE dd02l-tabname VALUE 'ZPP_S_MRP_REPORT'.

    CLASS-DATA : mo_view TYPE REF TO lcl_report_view.

    CLASS-METHODS :
      get_instance
        RETURNING VALUE(ro_view) TYPE REF TO lcl_report_view.

    " ALV DATA
    DATA: gt_fcat              TYPE lvc_t_fcat,
          gr_grid              TYPE REF TO cl_gui_alv_grid,
          gr_cont              TYPE REF TO cl_gui_custom_container,
          gs_toolbar_excluding TYPE ui_functions,
          gt_sort              TYPE lvc_t_sort.

    METHODS:
      display_data,
      refresh_alv
        IMPORTING
          ir_grid TYPE REF TO cl_gui_alv_grid,
      build_fcat IMPORTING i_str  TYPE dd02l-tabname
                 CHANGING  t_fcat TYPE lvc_t_fcat,
      change_subtotals.

  PRIVATE SECTION.

    METHODS:
      display_alv,
      exclude_functions,
      _set_sort.

ENDCLASS.

*___Class Implementation_____________________________________________*

*___Class Main ______________________________________________________*
CLASS lcl_main IMPLEMENTATION.
  METHOD run.
    me->get_data( ).
    CASE abap_true.
      WHEN p_ozet.
        me->get_data_ozet( ).
      WHEN p_detay.
        me->get_data_detay( ).
    ENDCASE.
*    me->filter_data( ).
  ENDMETHOD.

  METHOD get_data_from_material.
    "werks ve dat00 için burada ilk aldığımız veriler kısıtlanmalı mı??
    SELECT DISTINCT mara~matnr, marc~werks, mara~matkl, marc~dispo
        FROM mara
        INNER JOIN marc ON marc~matnr EQ mara~matnr
        WHERE mara~matnr IN @s_matnr
          AND mara~matkl IN @s_matkl
          AND marc~werks IN @s_werks
          AND marc~dispo IN @s_dispo
          AND marc~lvorm EQ ''
        INTO TABLE @rt_mara.

  ENDMETHOD.


  METHOD get_data_from_mard.
    CHECK it_mara IS NOT INITIAL.

    SELECT mard~matnr,
           mard~werks,
           SUM( mard~speme ) AS speme
        FROM mard
        INNER JOIN @it_mara AS im ON im~matnr EQ mard~matnr AND
                                     im~werks EQ mard~werks
        GROUP BY mard~matnr, mard~werks
        INTO CORRESPONDING FIELDS OF TABLE @rt_mard.

  ENDMETHOD.


  METHOD get_data_from_makt.
    CHECK it_mara IS NOT INITIAL.

    SELECT *
        FROM makt
        FOR ALL ENTRIES IN @it_mara
        WHERE matnr EQ @it_mara-matnr
          AND spras EQ @sy-langu
        INTO TABLE @rt_makt.
  ENDMETHOD.


  METHOD modify_data.

    LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<lfs_data>).

      <lfs_data>-toplam_ihtiyac_mik = <lfs_data>-musteri_sip_mik
                                     + <lfs_data>-rezervasyon_mik
                                     + <lfs_data>-sat_cagri_mik
                                     + <lfs_data>-sas_cagri_mik
                                     + <lfs_data>-tp_cagri_mik
                                     + <lfs_data>-teslimat_mik
                                     + <lfs_data>-emniyet_stok
                                     + <lfs_data>-birincil_ihtiyac.

      <lfs_data>-toplam_giris = <lfs_data>-sat_mik
                             + <lfs_data>-sas_mik
                             + <lfs_data>-planli_sip_mik
                             + <lfs_data>-uretim_sip_mik
                             + <lfs_data>-stok_mik.

      <lfs_data>-fark = <lfs_data>-toplam_giris
                        - <lfs_data>-toplam_ihtiyac_mik.

      <lfs_data>-status = COND #( WHEN <lfs_data>-fark GE 0
                                    THEN mc_green
                                  ELSE mc_red ).

    ENDLOOP.

    SORT ct_data BY matnr werks dat00.

  ENDMETHOD.

  METHOD filter_data.
    DELETE mt_data WHERE toplam_giris EQ 0.
  ENDMETHOD.

  METHOD get_data_detay.
    mt_data = mt_data_detay.
  ENDMETHOD.

  METHOD get_data_ozet.
    mt_data = mt_data_ozet.
  ENDMETHOD.

  METHOD get_data.
    DATA : ls_data LIKE LINE OF mt_data.

    DATA : lt_mdpsx TYPE STANDARD TABLE OF mdps,
           lt_mdezx TYPE STANDARD TABLE OF mdez.
    DATA : ls_mt61d TYPE mt61d.

    DATA : lv_matnr_tmp TYPE matnr.

    DATA(lt_mara) = me->get_data_from_material( ).
    DATA(lt_mard) = me->get_data_from_mard( lt_mara ).
*    DATA(lt_makt) = me->get_data_from_makt( lt_mara ).

    CHECK lt_mara IS NOT INITIAL.

    LOOP AT lt_mara INTO DATA(ls_mara).

      CALL FUNCTION 'MD_STOCK_REQUIREMENTS_LIST_API'
        EXPORTING
          matnr                    = ls_mara-matnr
          werks                    = ls_mara-werks
          berid                    = p_berid2
*         ergbz                    = 'SAP00003'
*         afibz                    = 'SAP00002'
        IMPORTING
          e_mt61d                  = ls_mt61d
        TABLES
          mdpsx                    = lt_mdpsx
          mdezx                    = lt_mdezx
        EXCEPTIONS
          material_plant_not_found = 1
          plant_not_found          = 2
          OTHERS                   = 3.

      LOOP AT lt_mdpsx INTO DATA(ls_mdpsx).

        IF NOT s_dat00[] IS INITIAL.
          IF ls_mdpsx-delkz NE 'WB'.
            CHECK ls_mdpsx-dat00 IN s_dat00.
          ENDIF.
        ENDIF.

        ls_data-matnr = ls_mara-matnr.
        ls_data-matkl = ls_mt61d-matkl.
        ls_data-dispo = ls_mt61d-dispo.
        ls_data-meins = ls_mt61d-meins.
        ls_data-maktx = ls_mt61d-maktx.
        ls_data-werks = ls_mt61d-werks.

        IF ls_mdpsx-dat00 IS NOT INITIAL.
        ls_data-dat00 = ls_mdpsx-dat00.
        ELSE.
        ls_data-dat00 = VALUE #( lt_mdezx[ delkz = 'WB' ]-dat00 OPTIONAL ).
        ENDIF.
        ls_data-week   = lcl_main=>date_get_week( ls_data-dat00 ).

        CASE ls_mdpsx-delkz.
          WHEN 'VC'.
            ls_data-musteri_sip_mik = ls_mdpsx-mng01.
          WHEN 'MR' OR 'AR' OR 'SB'.
            ls_data-rezervasyon_mik = ls_mdpsx-mng01.
          WHEN 'U2'.
            ls_data-sat_cagri_mik = ls_mdpsx-mng01.
          WHEN 'U1'.
            ls_data-sas_cagri_mik = ls_mdpsx-mng01.
          WHEN 'U4'.
            ls_data-tp_cagri_mik = ls_mdpsx-mng01.
          WHEN 'VJ'.
            ls_data-teslimat_mik = ls_mdpsx-mng01.
*          WHEN 'SH'.
*            ls_data-emniyet_stok = ls_mdpsx-mng01.
          WHEN 'PP'.
            ls_data-birincil_ihtiyac = ls_mdpsx-mng01.

          WHEN 'BA'.
            ls_data-sat_mik = ls_mdpsx-mng01.
          WHEN 'BE' OR 'LA'.
            ls_data-sas_mik = ls_mdpsx-mng01.
          WHEN 'PA'.
            ls_data-planli_sip_mik = ls_mdpsx-mng01.
          WHEN 'FE'.
            ls_data-uretim_sip_mik = ls_mdpsx-mng01.
*          WHEN 'WB'.
*            ls_data-stok_mik = ls_mdpsx-mng01.
        ENDCASE.

        ls_data-emniyet_stok = abs( VALUE #( lt_mdezx[ delkz = 'SH' ]-mng01 OPTIONAL ) ).
        ls_data-stok_mik = VALUE #( lt_mdezx[ delkz = 'WB' ]-mng01 OPTIONAL ).

        IF lv_matnr_tmp NE ls_data-matnr.
        ELSE.
          ls_data-emniyet_stok = 0.
          ls_data-stok_mik = 0.
        ENDIF.


        lv_matnr_tmp = ls_data-matnr.


        APPEND ls_data TO mt_data_detay.

        CLEAR : ls_data-dat00 , ls_data-week,ls_data-status.
        COLLECT ls_data INTO mt_data_ozet.

        CLEAR : ls_data.
      ENDLOOP.
      CLEAR : ls_mt61d, lt_mdpsx, lt_mdezx.
    ENDLOOP.

    me->modify_data( EXPORTING it_mard = lt_mard
                     CHANGING  ct_data = mt_data_ozet ).

    me->modify_data( EXPORTING it_mard = lt_mard
                     CHANGING  ct_data = mt_data_detay ).

  ENDMETHOD.

  METHOD date_get_week.
    CHECK i_date IS NOT INITIAL.

    CALL FUNCTION 'DATE_GET_WEEK'
      EXPORTING
        date         = i_date
      IMPORTING
        week         = r_week
      EXCEPTIONS
        date_invalid = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        RAISING date_invalid.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


*__ Class Event _____________________________________________________*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD : constructor.
    mv_event = iv_event.
  ENDMETHOD.

  METHOD handle_print_top_of_list .
  ENDMETHOD .

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN '&RNT'.
      WHEN '&F03' OR '&F12' OR '&F15' .
        LEAVE TO SCREEN 0.
      WHEN 'SUM'.
        _change_switch( i_sw_det = '' i_sw_sum = 'X' ).
        gr_main->get_data_ozet( ).

        gr_report_view->build_fcat( EXPORTING i_str = lcl_report_view=>c_str_h
                     CHANGING t_fcat = gr_report_view->gt_fcat ).

        gr_report_view->gr_grid->set_frontend_fieldcatalog( it_fieldcatalog =  gr_report_view->gt_fcat ).

        gr_report_view->refresh_alv( ir_grid = gr_report_view->gr_grid ).

        CALL METHOD cl_gui_cfw=>flush.
      WHEN 'DET'.
        _change_switch( i_sw_det = 'X' i_sw_sum = '' ).
        gr_main->get_data_detay( ).

        gr_report_view->build_fcat( EXPORTING i_str = lcl_report_view=>c_str_h
                     CHANGING t_fcat = gr_report_view->gt_fcat ).

        gr_report_view->gr_grid->set_frontend_fieldcatalog( it_fieldcatalog =  gr_report_view->gt_fcat ).

        gr_report_view->refresh_alv( ir_grid = gr_report_view->gr_grid ).

        CALL METHOD cl_gui_cfw=>flush.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_toolbar.


    APPEND LINES OF
           VALUE ttb_button(
                    ( butn_type = 3 )  " separator
                    ( function = 'SUM' icon = icon_summarize
                      text = TEXT-b01 quickinfo = TEXT-b02
                      disabled = p_ozet )
                    ( function = 'DET' icon = icon_detail
                      text = TEXT-b03 quickinfo = TEXT-b04
                      disabled = p_detay )
           )
         TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD after_refresh.
    gr_report_view->change_subtotals( ).
  ENDMETHOD.

  METHOD _change_switch.
    p_ozet = i_sw_sum.
    p_detay = i_sw_det.

  ENDMETHOD.

ENDCLASS.

*__ Class View ______________________________________________________*
CLASS lcl_report_view IMPLEMENTATION.
  METHOD get_instance.
    IF mo_view IS NOT BOUND.
      CREATE OBJECT mo_view.
    ENDIF.
    ro_view = mo_view.
  ENDMETHOD.

  METHOD display_data.
    IF gr_grid IS NOT BOUND.
      me->build_fcat( EXPORTING i_str = c_str_h
                      CHANGING t_fcat = gt_fcat ).
      me->_set_sort( ).
      me->exclude_functions( ).
      me->display_alv( ).

    ELSE.
      me->refresh_alv( gr_grid ).
    ENDIF.
  ENDMETHOD.

  METHOD refresh_alv.
    ir_grid->refresh_table_display( is_stable = VALUE #( row = 'X' col = '' )
                                    i_soft_refresh = 'X' ).
  ENDMETHOD.

  METHOD display_alv.
    gr_cont = NEW #( container_name = 'MAIN' ).
    gr_grid = NEW #( i_parent = gr_cont ).


    gr_event_handler = NEW #( iv_event = 'HDR' ).
    SET HANDLER gr_event_handler->handle_print_top_of_list FOR gr_grid.
    SET HANDLER gr_event_handler->handle_user_command FOR gr_grid.
    SET HANDLER gr_event_handler->handle_toolbar FOR gr_grid.

    gr_grid->set_table_for_first_display(
      EXPORTING
        is_variant                    = VALUE #( report = sy-repid
                                                 username = sy-uname
                                                 handle = 'MAIN' )
        i_save                        = 'A'
        is_layout                     = VALUE #( zebra = 'X'
                                                 cwidth_opt = 'A'
                                                 sel_mode = 'A'
                                                )
        it_toolbar_excluding          = gs_toolbar_excluding
      CHANGING
        it_outtab                     = gr_main->mt_data
        it_fieldcatalog               = me->gt_fcat
        it_sort                       = me->gt_sort
*        it_filter                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD .

  METHOD exclude_functions.
    gs_toolbar_excluding = VALUE #( ( cl_gui_alv_grid=>mc_fc_print )
                                    ( cl_gui_alv_grid=>mc_fc_loc_append_row )
                                    ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
                                    ( cl_gui_alv_grid=>mc_fc_loc_copy )
                                    ( cl_gui_alv_grid=>mc_fc_loc_cut )
                                    ( cl_gui_alv_grid=>mc_fc_loc_paste )
                                    ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
                                    ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
                                    ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
                                    ( cl_gui_alv_grid=>mc_fc_loc_undo )
                                    ( cl_gui_alv_grid=>mc_fc_url_copy_to_clipboard )
*                                    ( cl_gui_alv_grid=>mc_fc_sort_dsc )
*                                    ( cl_gui_alv_grid=>mc_fc_refresh )
*                                    ( cl_gui_alv_grid=>mc_fc_check )
*                                    ( cl_gui_alv_grid=>mc_fc_loc_move_row )
*                                    ( cl_gui_alv_grid=>mc_mb_sum )
*                                    ( cl_gui_alv_grid=>mc_mb_subtot )
*                                    ( cl_gui_alv_grid=>mc_fc_graph )
*                                    ( cl_gui_alv_grid=>mc_fc_info )
*                                    ( cl_gui_alv_grid=>mc_fc_print_back )
*                                    ( cl_gui_alv_grid=>mc_fc_filter )
*                                    ( cl_gui_alv_grid=>mc_fc_find_more )
*                                    ( cl_gui_alv_grid=>mc_fc_find )
*                                    ( cl_gui_alv_grid=>mc_mb_export )
*                                    ( cl_gui_alv_grid=>mc_mb_variant )
*                                    ( cl_gui_alv_grid=>mc_fc_detail )
*                                    ( cl_gui_alv_grid=>mc_mb_view )
                                   ).
  ENDMETHOD.


  METHOD build_fcat.
    DEFINE m_set_coltext.
      &1-coltext   = &2.
      &1-scrtext_s = &2.
      &1-scrtext_m = &2.
      &1-scrtext_l = &2.
    END-OF-DEFINITION.

    REFRESH t_fcat.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = i_str
        i_bypassing_buffer     = abap_true
      CHANGING
        ct_fieldcat            = t_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    LOOP AT t_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
      CASE <fs_fcat>-domname.
        WHEN 'MENGE'.
*          <fs_fcat>-no_zero   = 'X'.
*          <fs_fcat>-decimals_o = '0'.
      ENDCASE.

      CASE <fs_fcat>-fieldname.
        WHEN 'DAT00' OR 'WEEK'.
          IF p_ozet EQ 'X'.
            <fs_fcat>-no_out = 'X'.
          ELSE.
            <fs_fcat>-no_out = ''.
          ENDIF.
        WHEN 'STATUS'.
          m_set_coltext <fs_fcat> TEXT-s01.
          <fs_fcat>-outputlen = 10.
          IF p_ozet EQ 'X'.
            <fs_fcat>-no_out = ''.
          ELSE.
            <fs_fcat>-no_out = 'X'.
          ENDIF.
        WHEN 'BLOKE_STOK' OR 'TP_CAGRI_MIK'.
          <fs_fcat>-tech = 'X'.
          <fs_fcat>-no_out = 'X'.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.        "build_fcat


  METHOD change_subtotals.

  ENDMETHOD.


  METHOD _set_sort.
    DATA : ls_sort LIKE LINE OF gt_sort.

    CLEAR : ls_sort, gt_sort.
    ls_sort-fieldname = 'MATNR'.
    ls_sort-up = 'X'.
    ls_sort-spos = '1'.
    ls_sort-subtot = 'X'.
    APPEND ls_sort TO gt_sort.
*
  ENDMETHOD.

ENDCLASS.
