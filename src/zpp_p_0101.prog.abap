*&---------------------------------------------------------------------*
*& Report    : Toplu MRP Raporu
*&---------------------------------------------------------------------*
*& Firma     : İmprova
*& Abap      : Onur Akalan
*& Modül     : Gökçe Işıldar
*& Tarih     : 19.01.2022
*&---------------------------------------------------------------------*
REPORT zpp_p_0101.

INCLUDE :
  zpp_i_0101_top,
  zpp_i_0101_scr,
  zpp_i_0101_cls,
  zpp_i_0101_mdl.


INITIALIZATION.
  gr_main = NEW #( ).

START-OF-SELECTION.
  gr_main->run( ).

END-OF-SELECTION.
  IF gr_main->mt_data IS INITIAL.
    MESSAGE s100(zpp) DISPLAY LIKE 'E' RAISING no_data.
    EXIT.
  ENDIF.

  CALL SCREEN 100.
