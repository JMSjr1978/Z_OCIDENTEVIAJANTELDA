*&---------------------------------------------------------------------*
*& Report Z_NOME_USUARIO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_NOME_USUARIO NO STANDARD PAGE HEADING.

DATA: lv_user TYPE sy-uname.

START-OF-SELECTION.

lv_user = sy-uname.

WRITE: / 'Usuário:    ', lv_user.
ULINE.
WRITE: / 'Sistema:    ', sy-sysid.
ULINE.
WRITE: / 'Data Atual: ',sy-datum.
ULINE.
WRITE: / 'Hora Atual: ', sy-uzeit.
ULINE.
WRITE: / 'Idioma:     ', sy-langu.
