FORMAT COLOR COL_HEADING INTENSIFIED.       "AZUL FORTE
  WRITE: / '================================================'.
  WRITE: / '        INFORMAÇÕES DO SISTEMA SAP              '.
  WRITE: / '================================================'.
  FORMAT COLOR OFF.
  SKIP.

* INFORMAÇÕES DO SISTEMA

  FORMAT COLOR COL_KEY.
  WRITE: 'SISTEMA: '.
  FORMAT COLOR COL_NORMAL INTENSIFIED.
  WRITE: lv_sistema.
  ULINE.

  FORMAT COLOR COL_KEY.
  WRITE: 'MANDANTE: '.
  FORMAT COLOR COL_NORMAL INTENSIFIED.
  WRITE: lv_mandante.
  ULINE.

  FORMAT COLOR COL_KEY.
  WRITE: 'USUARIO: '.
  FORMAT COLOR COL_NORMAL INTENSIFIED.
  WRITE: lv_usuario.
  ULINE.

* DATA E HORA

  FORMAT COLOR COL_POSITIVE INTENSIFIED.
  WRITE: / 'DATA ATUAL: ', lv_data DD/MM/YYYY.
  WRITE: / 'HORA ATUAL: ', lv_hora USING EDIT MASK '__:__:__'.
  FORMAT COLOR OFF.
  ULINE.

* PROGRAMA E IDIOMA

  FORMAT COLOR COL_GROUP.
  WRITE: / 'PROGRAMA: ', lv_programa.
  WRITE: / 'IDIOMA: ', lv_idioma.

  IF lv_transacao IS NOT INITIAL.
    WRITE: / 'TRANSACAO: ', lv_transacao.
  ELSE.
    WRITE: / ' TRANSACAO: (Execucao Direta)'.
  ENDIF.
  FORMAT COLOR OFF.
  ULINE.

*RODAPÉ
  FORMAT COLOR COL_TOTAL INTENSIFIED.
  WRITE: / '========================================================'.
  WRITE: / '     [OK] Desenvolvido por: Maurício Júnior - S/4HANA         '.
  WRITE: / '========================================================'.
  FORMAT COLOR OFF.
