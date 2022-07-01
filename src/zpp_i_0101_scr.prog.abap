TABLES: mdtb, marc,mara, mdlv.

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME.
  SELECT-OPTIONS : s_matnr FOR mara-matnr,
                   s_werks FOR marc-werks OBLIGATORY NO INTERVALS NO-EXTENSION,
                   s_matkl FOR mara-matkl,
                   s_dispo FOR marc-dispo,
                   s_dat00 FOR mdtb-dat00 OBLIGATORY.
  PARAMETERS:   p_berid2 TYPE mdlv-berid.


SELECTION-SCREEN : END OF BLOCK b1.
SELECTION-SCREEN : BEGIN OF BLOCK b2 WITH FRAME.
  PARAMETERS :
    p_detay RADIOBUTTON GROUP r1 DEFAULT 'X',
    p_ozet  RADIOBUTTON GROUP r1.
SELECTION-SCREEN : END OF BLOCK b2.
