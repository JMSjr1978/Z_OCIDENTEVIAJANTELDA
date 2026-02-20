*& Report Z_TABELA_MULTIPLICACAO
*&---------------------------------------------------------------------*
*& Programa de Tabuada com Tela de Seleção e Gravação em Tabela
*&---------------------------------------------------------------------*
REPORT Z_TAB_MULTIPLICACAO_COMPLETA.

TABLES: ZTAB_TABUADA.

DATA: lt_tabuada TYPE TABLE OF ZTAB_TABUADA,
      ls_tabuada TYPE ztab_tabuada,
      lv_resultado TYPE i.

* Tela de Seleção
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_numini TYPE i DEFAULT 1 OBLIGATORY,    "Número inicial
            p_numfim TYPE i DEFAULT 10 OBLIGATORY.   "Número final
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_mulini TYPE i DEFAULT 1 OBLIGATORY,    "Multiplicador inicial
            p_mulfim TYPE i DEFAULT 10 OBLIGATORY.   "Multiplicador final
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
PARAMETERS: p_exibir RADIOBUTTON GROUP grp1 DEFAULT 'X',  "Apenas exibir
            p_gravar RADIOBUTTON GROUP grp1,              "Gravar na tabela
            p_limpar RADIOBUTTON GROUP grp1.              "Limpar tabela
SELECTION-SCREEN END OF BLOCK b3.

* Validação dos parâmetros
AT SELECTION-SCREEN.
  IF p_numini > p_numfim.
    MESSAGE 'Número inicial deve ser menor ou igual ao final!' TYPE 'E'.
  ENDIF.

  IF p_mulini > p_mulfim.
    MESSAGE 'Multiplicador inicial deve ser menor ou igual ao final!' TYPE 'E'.
  ENDIF.

START-OF-SELECTION.

  CASE 'X'.
    " Opção: Limpar tabela
    WHEN p_limpar.
      PERFORM limpar_tabela.

    " Opção: Gravar na tabela
    WHEN p_gravar.
      PERFORM gerar_tabuada.
      PERFORM gravar_tabela.

    " Opção: Apenas exibir
    WHEN p_exibir.
      PERFORM gerar_tabuada.
      PERFORM exibir_tabuada.
  ENDCASE.

*&---------------------------------------------------------------------*
*& Form gerar_tabuada
*&---------------------------------------------------------------------*
FORM gerar_tabuada.
  DATA: lv_numero TYPE i,
        lv_mult   TYPE i.

  REFRESH lt_tabuada.

  DO.
    lv_numero = p_numini + sy-index - 1.
    IF lv_numero > p_numfim.
      EXIT.
    ENDIF.

    DO.
      lv_mult = p_mulini + sy-index - 1.
      IF lv_mult > p_mulfim.
        EXIT.
      ENDIF.

      CLEAR ls_tabuada.
      ls_tabuada-numero          = lv_numero.
      ls_tabuada-multiplicador   = lv_mult.
      ls_tabuada-resultado       = lv_numero * lv_mult.
      ls_tabuada-data_criacao    = sy-datum.
      ls_tabuada-hora_criacao    = sy-uzeit.
      ls_tabuada-usuario_criacao = sy-uname.

      APPEND ls_tabuada TO lt_tabuada.
    ENDDO.
  ENDDO.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form exibir_tabuada
*&---------------------------------------------------------------------*
FORM exibir_tabuada.
  DATA: lv_numero_ant TYPE i VALUE -1.

  LOOP AT lt_tabuada INTO ls_tabuada.
    IF ls_tabuada-numero <> lv_numero_ant.
      IF sy-tabix > 1.
        SKIP 1.
      ENDIF.
      WRITE: / 'Tabela de multiplicação do', ls_tabuada-numero.
      ULINE.
      lv_numero_ant = ls_tabuada-numero.
    ENDIF.

    WRITE: / ls_tabuada-numero, 'x', ls_tabuada-multiplicador,
             '=', ls_tabuada-resultado.
  ENDLOOP.

  SKIP 2.
  WRITE: / 'Total de registros:', sy-tfill.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form gravar_tabela
*&---------------------------------------------------------------------*
FORM gravar_tabela.
  IF lt_tabuada IS NOT INITIAL.
    " Limpar registros anteriores do mesmo intervalo
    DELETE FROM ztab_tabuada WHERE numero BETWEEN p_numini AND p_numfim.

    " Inserir novos registros
    INSERT ztab_tabuada FROM TABLE lt_tabuada.

    IF sy-subrc = 0.
      COMMIT WORK.
      MESSAGE TEXT-010 TYPE 'S'.  "Dados gravados com sucesso!
      WRITE: / 'Dados gravados com sucesso na tabela ZTAB_TABUADA!'.
      WRITE: / 'Total de registros gravados:', sy-dbcnt.
    ELSE.
      ROLLBACK WORK.
      MESSAGE 'Erro ao gravar dados!' TYPE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form limpar_tabela
*&---------------------------------------------------------------------*
FORM limpar_tabela.
  DELETE FROM ztab_tabuada WHERE numero BETWEEN p_numini AND p_numfim.

  IF sy-subrc = 0.
    COMMIT WORK.
    WRITE: / 'Registros removidos com sucesso!'.
    WRITE: / 'Total de registros removidos:', sy-dbcnt.
  ELSE.
    WRITE: / 'Nenhum registro encontrado para remover.'.
  ENDIF.
ENDFORM.
