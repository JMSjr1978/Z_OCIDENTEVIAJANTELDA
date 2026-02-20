*&---------------------------------------------------------------------*
*& Report Z_TABELA_MULTIPLICACAO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_TABELA_MULTIPLICACAO NO STANDARD PAGE HEADING.

DATA: lv_numero     TYPE i,
      lv_contador   TYPE i,
      lv_resultado  TYPE i.

START-OF-SELECTION.

DO 10 TIMES.

  lv_numero  = sy-index.

  WRITE: / 'Tabela de multiplicação do', lv_numero.
  ULINE.

  DO 10 TIMES.
    lv_contador   = sy-index.
    lv_resultado  = lv_numero * lv_contador.

    WRITE: / lv_numero, 'X', lv_contador, '=', lv_resultado.
  ENDDO.
  SKIP.
ENDDO.
