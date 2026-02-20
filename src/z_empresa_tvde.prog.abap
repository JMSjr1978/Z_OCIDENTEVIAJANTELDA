*&---------------------------------------------------------------------*
*& Report Z_EMPRESA_TVDE
*&---------------------------------------------------------------------*
*& GESTÃO DE VEÍCULOS
*& CRIADO POR: JMSJ
*& CRIADO EM: 15/12/2025
*&
*&---------------------------------------------------------------------*
REPORT Z_EMPRESA_TVDE.

* DEFINIÇÃO DE TIPOS

"Criação de estruturas de dados customizadas definidas pelo usuário
TYPES: BEGIN OF ty_veiculos,
         id         TYPE i,
         marca      TYPE char20,
         modelo     TYPE char30,
         cor        TYPE char15,
         ano        TYPE numc4,
         data_reg   TYPE sy-datum,
         usuario    TYPE sy-uname,
       END OF ty_veiculos.

*TABELAS INTERNAS E VARIÁVEIS
"DECLARA VARIÁVEIS E TABELAS INTERNAS NA MEMÓRIA DO PROGRAMA
DATA: gt_veiculos TYPE TABLE OF ty_veiculos,
      gs_veiculos TYPE ty_veiculos,
      gv_opcao    TYPE char1.

*TELA DE SELEÇÃO
" O SELECTION-SCREEN CRIA A INTERFACE GRÁFICA DE ENTRADA
" O RADIOBUTTON CRIA BOTÕES DE OPÇÃO MUTUALMENTE EXCLUSIVOS, ONDE APENAS 1 PODE SER SELECIONADO
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_list    RADIOBUTTON GROUP grp1 DEFAULT 'X' USER-COMMAND cmd,
              p_add     RADIOBUTTON GROUP grp1,
              p_edit    RADIOBUTTON GROUP grp1,
              p_del     RADIOBUTTON GROUP grp1,
              p_clear   RADIOBUTTON GROUP grp1.
SELECTION-SCREEN END OF BLOCK b1.

*TELA DE SELEÇÃO 2 - CAMPO PARA O ID
" MODIF ID - IDENTIFICA CAMPOS QUE  PODEM SER MODIFICADOS DINAMICAMENTE
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_id TYPE i MODIF ID ed.
SELECTION-SCREEN END OF BLOCK b2.

*TEXTO DA TELA DE SELEÇÃO
" EVENTO EXECUTADO ANTES DA TELA SER EXIBIDA, PERMITE INICIALIZAR VALORES E TEXTOS
INITIALIZATION.
"TEXT-001 - 'Opções do Sistema de Veículos
"TEXT-002 - ID do Veículo (Editar/Deletar)

*VALIDAÇÃO DA TELA DE SELEÇÃO - EVENTO QUE MODIFICA A TELA DINAMICAMENTE
"EXECUTADO TODA VEZ QUE A TELA É EXIBIDA
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.                        " É UMA TABELA INTERNA QUE CONTÉM TODOS OS ELEMENTOS VISUAIS DA TELA
    IF screen-group1 = 'ED'.             " VERIFICA SE O CAMPO PERTENCE AO GRUPO 'ED'
      IF p_edit = 'X' OR p_del = 'X'.    " MOSTRA/ESCONDE O CAMPO ID BASEADO NA OPÇÃO SELECIONADA
        screen-active = 1.               " CAMPO VISÍVEL E ATIVO
      ELSE.
        screen-active = 0.               " CAMPO INVISÍVEL
      ENDIF.
      MODIFY SCREEN.                     " APLICA A MODIFICAÇÃO VISUAL NO ELEMENTO ATUAL
    ENDIF.
  ENDLOOP.

*INÍCIO DO PROCESSAMENTO
START-OF-SELECTION.                      " EVENTO PRINCIPAL ONDE O PROCESSAMENTO COMEÇA

  IF p_list      = 'X'.
    PERFORM f_listar_veiculos.           " CHAMA A SUB-ROTINA LISTAR VEÍCULOS
  ELSEIF p_add   = 'X'.
    PERFORM f_adicionar_veiculo.         " CHAMA A SUB-ROTINA ADICIONAR VEÍCULO
  ELSEIF p_edit  = 'X'.
    PERFORM f_editar_veiculo.            " CHAMA A SUB-ROTINA EDITAR VEÍCULO
  ELSEIF p_del   = 'X'.
    PERFORM f_deletar_veiculo.           " CHAMA A SUB-ROTINA DELETAR VEÍCULO
  ELSEIF p_clear = 'X'.
    PERFORM f_limpar_tabela.            " CHAMA A SUB-ROTINA LIMPAR VEÍCULO
  ENDIF.

*===================================================================================================================
* SUB-ROTINA: LISTAR VEÍCULOS
*===================================================================================================================

FORM f_listar_veiculos.                  "FORM / ENDFORM - DEFINEM UMA SUB-ROTINA - PROCEDIMENTO REUTILIZÁVEL

  DATA: lv_count TYPE i.                 " VARIÁVEL LOCAL PARA CONTAR REGISTROS

  SELECT id marca modelo cor ano data_reg usuario             " COMANDO SQL PARA BUSCAR DADOS DO BANCO
    FROM zveiculos                                            " TABELA CUSTOMIZADA DO BANCO DE DADOS
    INTO TABLE gt_veiculos                                    " ARMAZENA RESULTADO EM TABELA INTERNA
    ORDER BY id.                                              " ORDENA POR ID

  IF sy-subrc = 0.                                            " CÓDIGO DE RETORNO (0 = SUCESSO, 4 = NÃO ENCONTRADO)
    DESCRIBE TABLE gt_veiculos LINES lv_count.                " CONTA LINHAS DA TABELA

    WRITE: /.
    WRITE: / '========== LISTA DE VEÍCULOS CADASTRADOS =========='.
    WRITE: / 'Total de veículos: ', lv_count.
    WRITE: /.
    WRITE: / 'ID', 15 'MARCA', 35 'MODELO', 70 'COR', 90 'ANO'.   " CABEÇALHO COM SUAS DEVIDAS POSIÇÕES

    LOOP AT gt_veiculos INTO gs_veiculos.

      WRITE: / gs_veiculos-id LEFT-JUSTIFIED,
             15 gs_veiculos-marca,
             35 gs_veiculos-modelo,
             70 gs_veiculos-cor,
             90 gs_veiculos-ano NO-GROUPING.
    ENDLOOP.
    SKIP 2.

    READ TABLE gt_veiculos INTO gs_veiculos INDEX 1.
    WRITE: / 'Último registro cadastrado em: ', gs_veiculos-data_reg DD/MM/YYYY,
             'pelo usuário: ', gs_veiculos-usuario.
  ELSE.
    MESSAGE 'Nenhum veículo cadastrado no sistema.' TYPE 'I'.
  ENDIF.

ENDFORM.


*===================================================================================================================
* SUB-ROTINA: ADICIONAR NOVO VEÍCULO
*===================================================================================================================

FORM f_adicionar_veiculo.

  DATA: lv_id_novo    TYPE i,             " ID DO NOVO VEÍCULO
        lv_resposta   TYPE char1,         " RESPOSTA DO POPUP
        ls_veiculo_db TYPE zveiculos.     " ESTRUTURA COMPATÍVEL COM TABELA DB

  SELECT MAX( id )                        " BUSCA O MAIOR ID EXISTENTE
    FROM zveiculos                        " DA TABELA ZVEÍCULOS
    INTO @lv_id_novo.                     " @ - NOTAÇÃO PARA HOST VARIABLES - SINTAXE MODERNA

    lv_id_novo = lv_id_novo + 1.          " INCREMENTA PARA O PRÓXIMO ID
      CLEAR gs_veiculos.                  " LIMPA A ESTRUTURA (ZERA TODOS OS CAMPOS)
      gs_veiculos-id = lv_id_novo.

    PERFORM f_popup_entrada_dados CHANGING gs_veiculos      " CHAMA SUB-ROTINA DE POPUP, PASSANDO PARÂMETROS POR REFERÊNCIA
                                           lv_resposta.     " CHANGING - PARÂMETROS QUE PODEM SER MODIFICADOS PELA SUB-ROTINA

  IF lv_resposta = 'S'.                   " SE USUÁRIO CONFIRMOU
    gs_veiculos-data_reg = sy-datum.      " SY-DATUM - DATA DO SISTEMA ATUAL
    gs_veiculos-usuario  = sy-uname.      " SY-UNAME - USUÁRIO LOGADO NO SISTEMA

    ls_veiculo_db = CORRESPONDING #( gs_veiculos ).         " CORRESPONDING #() - COPIA CAMPOS COM NOMES CORRESPONDENTES

    INSERT zveiculos FROM ls_veiculo_db.                    " INSERT - INSERE REGISTRO NO BANCO DE DADOS

    IF sy-subrc = 0.
      COMMIT WORK.                                          " CONFIRMA A TRANSAÇÃO
      MESSAGE 'Veículo adicionado com sucesso!' TYPE 'S'.

      WRITE: / '========== VEÍCULO ADICIONADO COM SUCESSO! =========='.
      WRITE: /.
      WRITE: / 'ID:           ', gs_veiculos-id LEFT-JUSTIFIED.
      WRITE: / 'MARCA:        ', gs_veiculos-marca.
      WRITE: / 'MODELO:       ', gs_veiculos-modelo.
      WRITE: / 'COR:          ', gs_veiculos-cor.
      WRITE: / 'ANO:          ', gs_veiculos-ano.
      WRITE: /.
    ELSE.
      ROLLBACK WORK.                                        " DESFAZ TRANSAÇÃO EM CASO DE ERRO
      MESSAGE 'Erro ao cadastrar veículo!' TYPE 'E'.
    ENDIF.
  ELSE.
    MESSAGE 'Cadastro cancelado.' TYPE 'I'.
  ENDIF.
ENDFORM.

*===================================================================================================================
* SUB-ROTINA: EDITAR VEÍCULO EXISTENTE
*===================================================================================================================

FORM f_editar_veiculo.

  DATA: lv_resposta  TYPE char1,                       " LV_RESPOSTA - GUARDA S ou N, DEPENDE DA RESPOSTA DO USUÁRIO
        ls_zveiculo   TYPE zveiculos.                  " LS_ZVEICULO - ESTRUTURA COMPATÍVEL COM A TABELA ZVEICULOS

  IF p_id IS INITIAL.                                           " IS INITIAL - VERIFICA SE VARIÁVEL ESTÁ VAZIA / NULA
    MESSAGE 'Por favor, informe o ID do veículo.' TYPE 'E'.     " MENSAGEM DE ERRO
    RETURN.                                                     " SAI DA SUB-ROTINA
  ENDIF.

  SELECT SINGLE *                                         " BUSCA APENAS UM REGISTRO
    FROM zveiculos
    INTO @ls_zveiculo
    WHERE id = @p_id.                                     " WHERE - CONDIÇÃO DE FILTRO SQL

  IF sy-subrc = 0.
    gs_veiculos = CORRESPONDING #( ls_zveiculo ).

    PERFORM f_popup_entrada_dados CHANGING gs_veiculos
                                           lv_resposta.

    IF lv_resposta = 'S'.
      gs_veiculos-data_reg = sy-datum.
      gs_veiculos-usuario  = sy-uname.

      UPDATE zveiculos FROM ls_zveiculo.                  " ATUALIZA REGISTRO NO BANCO

      IF sy-subrc = 0.
        COMMIT WORK.
        MESSAGE 'Veículo atualizado com sucesso!' TYPE 'S'.

        WRITE: / '========== VEÍCULO ATUALIZADO COM SUCESSO! =========='.
        WRITE: /.
        WRITE: / 'ID:           ', gs_veiculos-id.
        WRITE: / 'MARCA:        ', gs_veiculos-marca.
        WRITE: / 'MODELO:       ', gs_veiculos-modelo.
        WRITE: / 'COR:          ', gs_veiculos-cor.
        WRITE: / 'ANO:          ', gs_veiculos-ano.
      ELSE.
        ROLLBACK WORK.
        MESSAGE 'Erro ao atualizar veículo!' TYPE 'E'.
      ENDIF.
    ELSE.
      MESSAGE 'Edição cancelada.' TYPE 'I'.
    ENDIF.
  ELSE.
    MESSAGE 'Veículo não encontrado!' TYPE 'E'.
  ENDIF.
ENDFORM.


*===================================================================================================================
* SUB-ROTINA: DELETAR VEÍCULO
*===================================================================================================================

FORM f_deletar_veiculo.

  DATA: lv_resposta   TYPE char1,
        ls_zveiculos  TYPE zveiculos.

  IF p_id IS INITIAL.
    MESSAGE 'Por favor, informe o ID do veículo.' TYPE 'E'.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM zveiculos
    INTO @ls_zveiculos
    WHERE id = @p_id.

  IF sy-subrc = 0.
    gs_veiculos = CORRESPONDING #( ls_zveiculos ).

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar        = 'Confirmar Exclusão'
        text_question   = |Deseja realmente excluir o veículo { gs_veiculos-marca }{ gs_veiculos-modelo }?|
        text_button_1   = 'SIM'
        text_button_2   = 'NÃO'
        default_button  = '2'
      IMPORTING
        answer          = lv_Resposta
      EXCEPTIONS
        text_not_found  = 1
        OTHERS          = 2.

    IF lv_resposta = '1'.
      DELETE FROM zveiculos WHERE id = @p_id.

      IF sy-subrc = 0.
        COMMIT WORK.
        MESSAGE 'Veículo excluído com sucesso!' TYPE 'S'.
        ULINE.
        WRITE: / 'VEÍCULO EXCLUÍDO COM SUCESSO!'.
        WRITE: / 'ID:           ', gs_veiculos-id.
        WRITE: / 'MARCA:        ', gs_veiculos-marca.
        WRITE: / 'MODELO:       ', gs_veiculos-modelo.
      ELSE.
        ROLLBACK WORK.
        MESSAGE 'Erro ao excluir veículo!' TYPE 'E'.
      ENDIF.
    ELSE.
      MESSAGE 'Exclusão cancelada.' TYPE 'I'.
    ENDIF.
  ELSE.
    MESSAGE 'Veículo não encontrado.' TYPE 'E'.
  ENDIF.
ENDFORM.

*===================================================================================================================
* SUB-ROTINA: LIMPAR TODA A TABELA
*===================================================================================================================
FORM f_limpar_tabela.

  DATA: lv_resposta   TYPE char1,
        lv_count      TYPE i.

  SELECT COUNT(*)
    FROM zveiculos
    INTO @lv_count.

  IF lv_count = 0.
    MESSAGE 'A tabela está vazia.' TYPE 'I'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar       = 'ATENÇÃO - Limpar Tabela'
      text_question  = |Deseja realmente DELETAR TODOS os { lv_count } veículos da tabela?|
      text_button_1  = 'Sim, deletar tudo'
      text_button_2  = 'Não'
      default_button = '2'
    IMPORTING
      answer         = lv_resposta
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  IF lv_resposta = '1'.
    DELETE FROM zveiculos WHERE id > 0.

    IF sy-subrc = 0.
      COMMIT WORK.
      MESSAGE |{ lv_count } veículos foram deletados com sucesso!| TYPE 'S'.
      ULINE.
      WRITE: / 'TABELA LIMPA COM SUCESSO'.
      ULINE.
      WRITE: / 'Total de veículos deletados: ', lv_count.
      SKIP.
      WRITE: / 'A tabela ZVEICULOS agora está vazia'.
      SKIP.
    ELSE.
      ROLLBACK WORK.
      MESSAGE 'Erro ao limpar tabela|' TYPE 'E'.
    ENDIF.
  ELSE.
    MESSAGE 'Operação cancelada. Nenhum dado foi deletado.' TYPE 'I'.
  ENDIF.
ENDFORM.

*===================================================================================================================
* SUB-ROTINA: POPUP PARA ENTRADA/EDIÇÃO DE DADOS
*===================================================================================================================

FORM f_popup_entrada_dados CHANGING ps_veiculo  TYPE ty_veiculos
                                    pv_resposta TYPE char1.

  DATA: lt_fields       TYPE TABLE OF sval,
        ls_field        TYPE sval,
        lv_returncode   TYPE char1.

  CLEAR ls_field.
        ls_field-tabname    = 'ZVEICULOS'.
        ls_field-fieldname  = 'MARCA'.
        ls_field-value      = ps_veiculo-marca.
        APPEND ls_field TO lt_fields.

  CLEAR ls_field.
        ls_field-tabname    = 'ZVEICULOS'.
        ls_field-fieldname  = 'MODELO'.
        ls_field-value      = ps_veiculo-modelo.
        APPEND ls_field TO lt_fields.

  CLEAR ls_field.
        ls_field-tabname    = 'ZVEICULOS'.
        ls_field-fieldname  = 'COR'.
        ls_field-value      = ps_veiculo-cor.
        APPEND ls_field TO lt_fields.

  CLEAR ls_field.
        ls_field-tabname    = 'ZVEICULOS'.
        ls_field-fieldname  = 'ANO'.
        ls_field-value      = ps_veiculo-ano.
        APPEND ls_field TO lt_fields.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title = 'Dados do veículo'
    IMPORTING
      returncode  = lv_returncode
    TABLES
      fields      = lt_fields
    EXCEPTIONS
      OTHERS      = 1.

  IF lv_returncode  = 'A'.
     pv_resposta    = 'N'.
  ELSE.
    LOOP AT lt_fields INTO ls_field.
      CASE ls_field-fieldname.
        WHEN 'MARCA'.
          ps_veiculo-marca    = ls_field-value.
        WHEN 'MODELO'.
          ps_veiculo-modelo   = ls_field-value.
        WHEN 'COR'.
          ps_veiculo-cor      = ls_field-value.
        WHEN 'ANO'.
          ps_veiculo-ano      = ls_field-value.
      ENDCASE.
    ENDLOOP.
    pv_resposta = 'S'.
  ENDIF.
ENDFORM.
