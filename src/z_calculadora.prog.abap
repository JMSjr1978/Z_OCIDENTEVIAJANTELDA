*&---------------------------------------------------------------------*
*& Report Z_CALCULADORA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_CALCULADORA NO STANDARD PAGE HEADING.

DATA: lv_num1   TYPE i VALUE 8,
      lv_num2   TYPE i VALUE 7,
      lv_soma   TYPE i.

START-OF-SELECTION.

lv_soma = lv_num1 + lv_num2.

WRITE: / 'Número 1: ', lv_num1 RIGHT-JUSTIFIED.
WRITE: /.
WRITE: / 'Número 2: ', lv_num2 RIGHT-JUSTIFIED.
WRITE: /.
WRITE: / 'Soma:     ', lv_soma RIGHT-JUSTIFIED.
