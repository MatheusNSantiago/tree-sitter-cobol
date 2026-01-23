      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0147
      * ANALISTA..: F2054109 - Cintia Iglesias
      * AUTOR.....: F2804421 - Eliana Cristina Ramos da Silva
      * DATA......: 28/04/2017
      * OBJETIVO..: Negocial - Lista ações permitidas para a operação
      *             estruturada de mercado de capitais.
      * COMPILACAO: 52 - PSOSE600 - Cobol 4.2 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS002 12/07/2019 F2804421 - 2016/145195:
      *               - Inibir opção 'Alterar dado complementares', para
      *                 qualquer tipo de versão (original ou não).
      *
      *        30/04/2019 F2804421 - 2016/145195:
      *               - Excluir opção 'Devolver para edição'.
      *               - Alterar opção 'Liberar versão para aprovação'
      *                 para 'Editar versão'.
      *               - Alterar opção 'Aprovar versão' para 'Confirmar
      *                 versão'.
      *        25/02/2019 F2804421 - 2016/145195:
      *               - Incluir opção 'Atribuir ISIN' para estados da
      *                 da operaço 1_motivo 999 até estado 5.
      *               - Inluir acesso a tab.TIT_OPR_ETRD_MCP(SGTST002).
      *               - Incluir opção 'Atribuir Título' para os esta-
      *                 dos da operação 1 e motivo 999 até motivo 4.
      *
      *        26/12/2018 F2804421 - Permitir 'Editar' somente para
      *                              estado operação = 1 e motivo = 0.
      *        22/11/2018 F2804421 - Incluir opção 'Cancelar Autorização
      *                              de Compra' no menu.
      *        06/11/2018 F2804421 - Incluir opção 'Conferir Cadastro'
      *                              no menu, completando a opção ante-
      *                              rior 'Finalizar Cadastro'.
      *
      * VRS001 28/04/2017 F2804421 DE 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------

      *----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS0147.
      *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
      *

       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       77  CTE-INICIO-SS                 PIC  X(040)  VALUE
                   '*** S.S. COMECA AQUI ***'.
       77  CTE-PROG                      PIC  X(018)  VALUE
                   '*** SGTS0147 ***'.
       77  CTE-VERS                      PIC  X(006)  VALUE 'VRS002'.
      * Módulos acionados
       77  SGTST002                       PIC  X(008) VALUE 'SGTST002'.
       77  SGTST005                       PIC  X(008) VALUE 'SGTST005'.
       77  SGTST009                       PIC  X(008) VALUE 'SGTST009'.
       77  SGTST012                       PIC  X(008) VALUE 'SGTST012'.
       77  SGTSC069                       PIC  X(008) VALUE 'SGTSC069'.
       77  SGTSPE03                       PIC  X(008) VALUE 'SGTSPE03'.
       77  SGTSPE05                       PIC  X(008) VALUE 'SGTSPE05'.
      *----------------------------------------------------------------*
      *                     Containers utilizados
      *----------------------------------------------------------------*
      * Container de erro
       77 PRM-ERRO                 PIC X(16) VALUE 'PRM-ERRO'.

      * Container do usuário logado
       77 CTNR-PRM-INF-OPRD        PIC X(16) VALUE 'PRM-INF-OPRD'.
      * ---------------------------------------------------------------
      * Container da requisição do serviço IIB acionado
       77 PRM-DADOS-RQSC           PIC X(16) VALUE 'PRM-DADOS-RQSC'.

      * Container da resposta do serviço IIB acionado
       77 PRM-DADOS-RPST           PIC X(16) VALUE 'PRM-DADOS-RPST'.

      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*

      * Areas de trabalho

      *   Opções previstas para menu.
      *
       01 IN-ACOES                    PIC 9(02) VALUE 0.
          88 IN-EDITAR                VALUE 1.
          88 IN-DETALHAR              VALUE 2.
          88 IN-FINALIZAR-CADASTRO    VALUE 3.
          88 IN-EXCLUIR-CADASTRO      VALUE 4.
          88 IN-CANCELAR-CADASTRO     VALUE 5.
          88 IN-EXCLUIR-VERSAO        VALUE 6.
          88 IN-ENCERRAR-OPERACAO     VALUE 7.
          88 IN-EDITAR-VERSAO         VALUE 8.
          88 IN-APROVAR-COMPRA        VALUE 9.
          88 IN-CONFIRMAR-VERSAO      VALUE 10.
          88 IN-COMPRAR               VALUE 11.
          88 IN-ALTERAR-DADOS-COMPL   VALUE 12.
          88 IN-DEV-P-CADASTRAMENTO   VALUE 13.
          88 IN-DEV-P-AUTORIZACAO-CPR VALUE 14.
          88 IN-GERAR-VERSAO          VALUE 15.
          88 IN-REABRIR               VALUE 16.
          88 IN-TRANSF-OTR-CTRA       VALUE 17.
          88 IN-VINCULAR-PAGAMENTO    VALUE 18.
          88 IN-CONFERIR-CADASTRO     VALUE 19.
          88 IN-CANCELAR-AUT-COMPRA   VALUE 20.
          88 IN-ATRIBUIR-ISIN         VALUE 21.
          88 IN-ATRIBUIR-TITULO       VALUE 22.
      *----------------------------------------------------------------*
      * Book dos Containers e das subrotinas chamadas
      **---------------------------------------------------------------*
      * Book das variáveis auxiliares do tratam ento de erro via MSG
-INC SGTKERR1
      * Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999
      * Book para captura de erro via MSG1
-INC MSGKWMG1

      * Book do container de requisição e resposta deste modulo
       01 SGTS0147-DADOS.
-INC SGTK0147

      * Consultar dados do ISIN contido na tabela de títulos.
      * via commarea
       01  SGTST002-DADOS.
-INC SGTKT002

      * Consultar último estado ativo da operação EST_OPR_ETRD.
      * via commarea
       01  SGTST009-DADOS.
-INC SGTKT009

      * Consultar dados do tipo de versão da operação referente ao
      * número da versão da Operação da entrada. OPR_ETRD_MCP
      * via commarea
       01  SGTST005-DADOS.
-INC SGTKT005

      * Consultar dados da carteira da operação. CTRA_TIT_VL_MNTR
      * via commarea
       01  SGTST012-DADOS.
-INC SGTKT012

      * Consultar último estado da versão da operação. EST_VRS_OPR_ETRD
      * via container.
       01  SGTSC069-DADOS.
-INC SGTKC069

      * Consultar a existência de cód.tipo estado da versão > 2
      * (A avaliar)  na tabela EST_VRS_OPR_ETRD.
      * via container.
       01  SGTSPE03-DADOS.
-INC SGTKPE03

      * Consultar o estado e o tipo da última versão da operação nas
      * tabelas OPR_ETRD_MCP e EST_VRS_OPR_ETRD.
      * via container.
       01  SGTSPE05-DADOS.
-INC SGTKPE05

      * Book do Container de Erro
-INC IIBKERRO

       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*
           PERFORM 100000-INICIAR

           PERFORM 200000-PROCESSAR

           PERFORM 980000-GRAVAR-CONTAINER-RPST

           PERFORM 990000-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       100000-INICIAR                  SECTION.
      *----------------------------------------------------------------*
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *    Ativa a captura automatica de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999
           PERFORM 970000-LER-CANAL

      *    Define o nome do programa p/ fins manipulacao container
           MOVE 'SGTS0147'             TO NM-PGM

      *    Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       110000-VALIDAR-REQUISICAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *    Validação tamanho esperado da requisicao X tamanho recebido
      *    na section de leitura de container de requisicao 971000
      *
      *    Valida campo a campo da requisicao

           IF  SGTS0147-CD-CTRA-TIT-VL    <  ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'inválido.'        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0147-CD-CTRA-TIT-VL    EQUAL  ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0002               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'    TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0147-CD-OPR-ETRD-MCP   NOT NUMERIC
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0147-CD-OPR-ETRD-MCP   < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0004               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais inv
      -             'válido.'          TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0147-CD-OPR-ETRD-MCP   = ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0005               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             'informado.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0147-NR-VRS-OPR-ETRD   <  ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0006               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0147-NR-VRS-OPR-ETRD   =  ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0007               TO MSG1-CD-ERRO-TS
               MOVE 'Número versão operação pagador não informado.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       110000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-VER-ULT-ESTADO-OPRD

           PERFORM 220000-VER-ULT-ESTADO-VRS
      *
      * Consulta dados da operação selecionada no cadastro OPR_ETRD_MCP.
           PERFORM 262000-VER-OPERACAO-ATU

           PERFORM 280000-CONSULTA-CARTEIRA

      *   Para carteira de Recuperação Judicial, só deve existir na ba
      *   se operações com estado=5 encarteirada e estado da versão = 3
      *   aprovada
           IF SGTST012-IN-RCPO-JDCL EQUAL 'S'
      *           Estado da versão : 3 - Aprovada

              IF SGTSC069-CD-TIP-EST-VRS EQUAL 3
                 PERFORM 290000-VRS-APVD-CTRA-RCPO-JDCL
              ELSE
                 SET ERRO-PROGRAMADO     TO TRUE
                 MOVE 118                TO MSG1-CD-ERRO-TS
                 MOVE 'Código do tipo de estado da versão inválido.'
                                                     TO MSG1-TX-ERRO-TS
                 PERFORM 969000-MONITORAR-VARIAVEIS
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-IF

           ELSE

              PERFORM 230000-VER-MAIOR-ESTADO-VRS
      *
              MOVE 0003                   TO MSG1-CD-SEQL-ERRO

              EVALUATE SGTSC069-CD-TIP-EST-VRS
      *           Estado da versão : 1 - Em edição
                  WHEN 1
                      PERFORM 240000-VRS-EM-EDICAO-CTRA-CMUM
      *           Estado da versão : 2 - A avaliar
                  WHEN 2
                      PERFORM 250000-VRS-A-AVALIAR-CTRA-CMUM
      *           Estado da versão : 3 - Aprovada
                  WHEN 3
                      PERFORM 260000-VRS-APROVADA-CTRA-CMUM
                  WHEN OTHER
                     SET ERRO-PROGRAMADO     TO TRUE
                     MOVE 613                TO MSG1-CD-ERRO-TS
                     MOVE 'Cód.tipo de estado da versão é diferente de 1
      -              ',2, 3 e 4.'                TO MSG1-TX-ERRO-TS
                     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-EVALUATE
           END-IF

           .
       200000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       210000-VER-ULT-ESTADO-OPRD    SECTION.
      *----------------------------------------------------------------*
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTST009-DADOS

      *    Movimentar os campos de entrada

           MOVE SGTST009               TO NM-PGM

           SET SGTST009-CONSULTAR-EST-ATIVO    TO TRUE

           MOVE SGTS0147-CD-CTRA-TIT-VL
                                       TO SGTST009-CD-CTRA-TIT-VL
           MOVE SGTS0147-CD-OPR-ETRD-MCP
                                       TO SGTST009-CD-OPR-ETRD-MCP
           MOVE SGTS0147-NR-VRS-OPR-ETRD
                                       TO SGTST009-NR-VRS-OPR-ETRD

           PERFORM 920000-CHAMAR-SGTST009

           .
       210000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       220000-VER-ULT-ESTADO-VRS     SECTION.
      *----------------------------------------------------------------*
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSC069-DADOS

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSC069               TO NM-PGM

      *    Movimentar os campos de entrada

           MOVE SGTS0147-CD-CTRA-TIT-VL TO SGTSC069-CD-CTRA-TIT-VL
           MOVE SGTS0147-CD-OPR-ETRD-MCP
                                        TO SGTSC069-CD-OPR-ETRD-MCP
           MOVE SGTS0147-NR-VRS-OPR-ETRD
                                        TO SGTSC069-NR-VRS-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO

           .
       220000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       230000-VER-MAIOR-ESTADO-VRS   SECTION.
      *----------------------------------------------------------------*
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSPE03-DADOS

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSPE03               TO NM-PGM

      *    Movimentar os campos de entrada

           MOVE SGTS0147-CD-CTRA-TIT-VL TO SGTSPE03-CD-CTRA-TIT-VL
           MOVE SGTS0147-CD-OPR-ETRD-MCP
                                        TO SGTSPE03-CD-OPR-ETRD-MCP
           MOVE SGTS0147-NR-VRS-OPR-ETRD
                                        TO SGTSPE03-NR-VRS-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO

           .
       230000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       240000-VRS-EM-EDICAO-CTRA-CMUM         SECTION.
      *----------------------------------------------------------------*
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

      *   Estado da versão : 1 - Em edição

           EVALUATE SGTST009-CD-TIP-EST-OPR
      *   Estado da operação = 1 - Em edição e 5 - Encarteirado
               WHEN 1
                  SET  IN-DETALHAR     TO  TRUE
                  PERFORM 300000-CARREGA-ACAO

      *   Verifica motivo de alteração do Estado da Operação:
      *   Se igual a zeros = Ainda está em fase de edição e justifi-
      *      cativas da fase 'Finalizar Cadastro'.
      *   Se igual a 999   = Já finalizou a fase edição e justificati-
      *      vas, e passa para próxima fase - Conferir cadastro.

                  IF  SGTST009-CD-MTV-ALT  = 999
                      SET  IN-CONFERIR-CADASTRO  TO TRUE
                      PERFORM 300000-CARREGA-ACAO

      *  Verifica se titulo não informado e se ISIN não informado.
                      IF  SGTST005-CD-TIT-OPR-ETRD  EQUAL SPACES
                          SET  IN-ATRIBUIR-TITULO     TO  TRUE
                          PERFORM 300000-CARREGA-ACAO
                      ELSE
                          PERFORM 291000-CONSULTAR-TIT-OPR-ETRD
                          IF  SGTST002-CD-ISIN-TIT-OPR EQUAL SPACES
                              SET  IN-ATRIBUIR-ISIN     TO  TRUE
                              PERFORM 300000-CARREGA-ACAO
                          END-IF
                      END-IF
                  ELSE
                      SET  IN-EDITAR       TO  TRUE
                      PERFORM 300000-CARREGA-ACAO

                      SET  IN-FINALIZAR-CADASTRO  TO TRUE
                      PERFORM 300000-CARREGA-ACAO
                  END-IF

      *   Verifica se Estado da Versão já passou alguma vez pelo estado
      *   igual a 2 - A avaliar.

                  IF  SGTSPE03-IN-EST-VRS-02   =  'S'
                      SET  IN-CANCELAR-CADASTRO   TO  TRUE
                      PERFORM 300000-CARREGA-ACAO
                  ELSE
                      SET  IN-EXCLUIR-CADASTRO   TO TRUE
                      PERFORM 300000-CARREGA-ACAO
                  END-IF

               WHEN 5
                  SET  IN-EDITAR       TO  TRUE
                  PERFORM 300000-CARREGA-ACAO

                  SET  IN-DETALHAR     TO  TRUE
                  PERFORM 300000-CARREGA-ACAO

                  SET  IN-EXCLUIR-VERSAO  TO TRUE
                  PERFORM 300000-CARREGA-ACAO

                  SET  IN-EDITAR-VERSAO  TO TRUE
                  PERFORM 300000-CARREGA-ACAO

               WHEN OTHER
                  SET ERRO-PROGRAMADO  TO TRUE
                  MOVE 614             TO MSG1-CD-ERRO-TS
                  MOVE 'Estado da operação incompativel com estado da
      -                'versão da operação.'
                                       TO MSG1-TX-ERRO-TS
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           .
       240000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       250000-VRS-A-AVALIAR-CTRA-CMUM         SECTION.
      *----------------------------------------------------------------*
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

      *        Estado da versão : 2 - A avaliar

           EVALUATE SGTST009-CD-TIP-EST-OPR
      *       Estado da operação = 2 - Autorizar compra
               WHEN 2
                  SET  IN-DETALHAR        TO  TRUE
                  PERFORM 300000-CARREGA-ACAO
                  SET  IN-APROVAR-COMPRA  TO  TRUE
                  PERFORM 300000-CARREGA-ACAO
                  SET  IN-DEV-P-CADASTRAMENTO    TO  TRUE
                  PERFORM 300000-CARREGA-ACAO

      *  Verifica se titulo não informado e se ISIN não informado.
                  IF  SGTST005-CD-TIT-OPR-ETRD  EQUAL SPACES
                      SET  IN-ATRIBUIR-TITULO     TO  TRUE
                      PERFORM 300000-CARREGA-ACAO
                  ELSE
                      PERFORM 291000-CONSULTAR-TIT-OPR-ETRD
                      IF  SGTST002-CD-ISIN-TIT-OPR EQUAL SPACES
                          SET  IN-ATRIBUIR-ISIN     TO  TRUE
                          PERFORM 300000-CARREGA-ACAO
                      END-IF
                  END-IF

      *  Verifica se estado da versão já passou do 2 - A avaliar .
                  IF  SGTSPE03-IN-EST-VRS-02   =   'S'
                      SET  IN-CANCELAR-CADASTRO   TO  TRUE
                      PERFORM 300000-CARREGA-ACAO
                  ELSE
                      SET  IN-EXCLUIR-CADASTRO   TO  TRUE
                      PERFORM 300000-CARREGA-ACAO
                  END-IF

      *   Estado da operação = 5 - Encarteirado
               WHEN 5
                  SET  IN-DETALHAR       TO  TRUE
                  PERFORM 300000-CARREGA-ACAO

                  SET  IN-CONFIRMAR-VERSAO  TO  TRUE
                  PERFORM 300000-CARREGA-ACAO

                  SET  IN-EXCLUIR-VERSAO  TO  TRUE
                  PERFORM 300000-CARREGA-ACAO
      *
               WHEN OTHER
                  MOVE 614             TO MSG1-CD-ERRO-TS
                  SET ERRO-PROGRAMADO  TO TRUE
                  MOVE 'Estado da operação incompativel com estado da
      -                'versão da operação.'
                                       TO MSG1-TX-ERRO-TS
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .
       250000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       260000-VRS-APROVADA-CTRA-CMUM         SECTION.
      *----------------------------------------------------------------*
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

      *        Estado da versão : 3 - Aprovada

           EVALUATE SGTST009-CD-TIP-EST-OPR
      *   Estado da operação = 3 - Disparar Ordem de Compra
               WHEN 3
                  SET  IN-COMPRAR               TO TRUE
                  PERFORM 300000-CARREGA-ACAO
                  SET  IN-ALTERAR-DADOS-COMPL   TO TRUE
                  PERFORM 300000-CARREGA-ACAO
                  SET  IN-DETALHAR              TO TRUE
                  PERFORM 300000-CARREGA-ACAO
                  SET  IN-DEV-P-AUTORIZACAO-CPR TO TRUE
                  PERFORM 300000-CARREGA-ACAO
                  SET  IN-CANCELAR-CADASTRO     TO TRUE
                  PERFORM 300000-CARREGA-ACAO

      *  Verifica se titulo não informado e se ISIN não informado.
                  IF  SGTST005-CD-TIT-OPR-ETRD  EQUAL SPACES
                      SET  IN-ATRIBUIR-TITULO     TO  TRUE
                      PERFORM 300000-CARREGA-ACAO
                  ELSE
                      PERFORM 291000-CONSULTAR-TIT-OPR-ETRD
                      IF  SGTST002-CD-ISIN-TIT-OPR EQUAL SPACES
                          SET  IN-ATRIBUIR-ISIN     TO  TRUE
                          PERFORM 300000-CARREGA-ACAO
                      END-IF
                  END-IF

      *   Estado da operação = 4 - A Encarteirar
               WHEN 4
                  SET  IN-ALTERAR-DADOS-COMPL   TO TRUE
                  PERFORM 300000-CARREGA-ACAO
                  SET  IN-DETALHAR              TO TRUE
                  PERFORM 300000-CARREGA-ACAO
                  SET  IN-CANCELAR-CADASTRO     TO TRUE
                  PERFORM 300000-CARREGA-ACAO
                  SET  IN-CANCELAR-AUT-COMPRA   TO TRUE
                  PERFORM 300000-CARREGA-ACAO

      *  Verifica se titulo não informado e se ISIN não informado.
                  IF  SGTST005-CD-TIT-OPR-ETRD  EQUAL SPACES
                      SET  IN-ATRIBUIR-TITULO     TO  TRUE
                      PERFORM 300000-CARREGA-ACAO
                  ELSE
                      PERFORM 291000-CONSULTAR-TIT-OPR-ETRD
                      IF  SGTST002-CD-ISIN-TIT-OPR EQUAL SPACES
                          SET  IN-ATRIBUIR-ISIN     TO  TRUE
                          PERFORM 300000-CARREGA-ACAO
                      END-IF
                  END-IF

      *   Estado da operação = 5 - Encarteirado
               WHEN 5
                  PERFORM 261000-VER-EST-ULT-VERSAO

                  IF  SGTSPE05-CD-TIP-EST-VRS   >  2
                      SET  IN-GERAR-VERSAO        TO TRUE
                      PERFORM 300000-CARREGA-ACAO

                      SET  IN-ALTERAR-DADOS-COMPL TO TRUE
                      PERFORM 300000-CARREGA-ACAO

                      SET  IN-DETALHAR            TO TRUE
                      PERFORM 300000-CARREGA-ACAO

      *   Verifica se ISIN não foi informado.
                      PERFORM 291000-CONSULTAR-TIT-OPR-ETRD
                      IF  SGTST002-CD-ISIN-TIT-OPR EQUAL SPACES
                          SET  IN-ATRIBUIR-ISIN     TO  TRUE
                          PERFORM 300000-CARREGA-ACAO
                      END-IF


      *   As ações de TRANSFERIR PARA OUTRA CARTEIRA
      *   ,  ENCERRAR OPERAÇÃO
      *   e  VINCULAR PAGAMENTO só devem ser
      *   permitidas se a versão que o usuário selecionou na tela for
      *   a ultima e estiver aprovada (não pode existir nenhuma outra
      *   versão pendente)
                      IF  SGTSPE05-CD-TIP-EST-VRS EQUAL 3
                          AND SGTS0147-NR-VRS-OPR-ETRD EQUAL
                                         SGTSPE05-NR-VRS-OPR-ETRD
                          SET  IN-ENCERRAR-OPERACAO   TO TRUE
                          PERFORM 300000-CARREGA-ACAO
                          SET  IN-TRANSF-OTR-CTRA     TO TRUE
                          PERFORM 300000-CARREGA-ACAO
                          SET  IN-VINCULAR-PAGAMENTO  TO TRUE
                          PERFORM 300000-CARREGA-ACAO
                      END-IF

                  ELSE

      **************  PERFORM 262000-VER-EST-VERSAO-ATU

      *               Verificar
      *               Se tipo da versão da operação selecionada
      *                   pelo usuário = 1 - Original
      *               E
      *               Se Existe novo versionamento pendente cujo tipo
      *                   da versão também seja = 1 - Original
      *              NÃO DEVE PERMITIR ALTERAÇÃO ALGUMA NA OPERAÇÃO
      *              SELECIONADA PELO USU?RIO

                      IF  SGTST005-CD-TIP-VRS-OPR   =  1    AND
                          SGTSPE05-CD-TIP-VRS-OPR   =  1
                          SET  IN-DETALHAR            TO TRUE
                          PERFORM 300000-CARREGA-ACAO
                      ELSE

      *--> 12/07/2019 inibir alteração dados complementares
      **                    SET  IN-ALTERAR-DADOS-COMPL TO TRUE
      **                    PERFORM 300000-CARREGA-ACAO

                          SET  IN-DETALHAR            TO TRUE
                          PERFORM 300000-CARREGA-ACAO

                      END-IF
                  END-IF
      *   Estado da operação = 6 - Encerrar
               WHEN 6
                  SET  IN-DETALHAR    TO TRUE
                  PERFORM 300000-CARREGA-ACAO
                  SET  IN-REABRIR     TO TRUE
                  PERFORM 300000-CARREGA-ACAO

      *   Estado da operação = 10 - Transferida
               WHEN 10
                  SET  IN-DETALHAR    TO TRUE
                  PERFORM 300000-CARREGA-ACAO

               WHEN OTHER
                  SET ERRO-PROGRAMADO  TO TRUE
                  MOVE 614             TO MSG1-CD-ERRO-TS
                  MOVE 'Estado da operação incompativel com estado da
      -                'versão da operação.'
                                       TO MSG1-TX-ERRO-TS
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           .
       260000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       261000-VER-EST-ULT-VERSAO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

      *    Consulta o estado e tipo da última versão da operação.
      *    Movimentar o nome do programa a ser executado
           INITIALIZE                  SGTSPE05-DADOS

           MOVE SGTSPE05               TO NM-PGM

      *    Movimentar os campos de entrada

           MOVE SGTS0147-CD-CTRA-TIT-VL TO SGTSPE05-CD-CTRA-TIT-VL
           MOVE SGTS0147-CD-OPR-ETRD-MCP
                                        TO SGTSPE05-CD-OPR-ETRD-MCP

           PERFORM 900000-EXECUTAR-MODULO

           .
       261000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       262000-VER-OPERACAO-ATU          SECTION.
      *----------------------------------------------------------------*
      * Verifica situação atual da operação selecionada no cadastro de *
      * Operações. OPR_ETRD_MCP.                                       *
      *----------------------------------------------------------------*

           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTST005-DADOS

      *    Movimentar os campos de entrada
           SET SGTST005-CONSULTAR       TO TRUE
           MOVE SGTS0147-CD-CTRA-TIT-VL TO SGTST005-CD-CTRA-TIT-VL
           MOVE SGTS0147-CD-OPR-ETRD-MCP
                                        TO SGTST005-CD-OPR-ETRD-MCP
           MOVE SGTS0147-NR-VRS-OPR-ETRD
                                        TO SGTST005-NR-VRS-OPR-ETRD

           PERFORM 920001-CHAMAR-SGTST005

           .
       262000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       280000-CONSULTA-CARTEIRA  SECTION.
      *----------------------------------------------------------------*
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTST012-DADOS

      *    Movimentar os campos de entrada
           SET SGTST012-CONSULTAR       TO TRUE
           MOVE SGTS0147-CD-CTRA-TIT-VL TO SGTST012-CD-CTRA-TIT-VL

           PERFORM 920002-CHAMAR-SGTST012

           .
       280000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       290000-VRS-APVD-CTRA-RCPO-JDCL         SECTION.
      *----------------------------------------------------------------*
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

      *        Estado da versão : 3 - Aprovada

           EVALUATE SGTST009-CD-TIP-EST-OPR
      *   Estado da operação = 5 - Encarteirado
               WHEN 5
                  SET  IN-DETALHAR            TO TRUE
                  PERFORM 300000-CARREGA-ACAO
                  SET  IN-ENCERRAR-OPERACAO TO TRUE
                  PERFORM 300000-CARREGA-ACAO
                  SET  IN-TRANSF-OTR-CTRA     TO TRUE
                  PERFORM 300000-CARREGA-ACAO

      *   Estado da operação = 10 - Transferida
               WHEN 10
                  SET  IN-DETALHAR    TO TRUE
                  PERFORM 300000-CARREGA-ACAO

               WHEN OTHER
                  SET ERRO-PROGRAMADO  TO TRUE
                  MOVE 614             TO MSG1-CD-ERRO-TS
                  MOVE 'Estado da operação incompativel com estado da
      -                'versão da operação.'
                                       TO MSG1-TX-ERRO-TS
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           .
       290000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       291000-CONSULTAR-TIT-OPR-ETRD   SECTION.
      *----------------------------------------------------------------*
           MOVE 0014                   TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTST002-DADOS

           SET   SGTST002-CONSULTAR    TO  TRUE
           MOVE  SGTST005-CD-TIT-OPR-ETRD
                                       TO  SGTST002-CD-TIT-OPR-ETRD
           MOVE  SGTST005-NR-DVS-TIT   TO  SGTST002-NR-DVS-TIT

           PERFORM 920003-CHAMA-SGTST002

           .
       291000-SAI.
           EXIT.


      *----------------------------------------------------------------*
       300000-CARREGA-ACAO             SECTION.
      *----------------------------------------------------------------*
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

           ADD 1           TO SGTS0147-QT-OCR
               ON SIZE ERROR
               SET ERRO-PROGRAMADO   TO TRUE
               MOVE 607                TO MSG1-CD-ERRO-TS
               MOVE 'Erro soma do índice da tabela ações do menu.'
                                     TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-ADD

           EVALUATE TRUE
               WHEN IN-EDITAR
                    MOVE 'EDITAR'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
               WHEN IN-DETALHAR
                    MOVE 'DETALHAR'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
               WHEN IN-FINALIZAR-CADASTRO
                    MOVE 'FINALIZAR-CADASTRO'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
               WHEN IN-EXCLUIR-CADASTRO
                    MOVE 'EXCLUIR-CADASTRO'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
               WHEN IN-CANCELAR-CADASTRO
                    MOVE 'CANCELAR-CADASTRO'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
               WHEN IN-EXCLUIR-VERSAO
                    MOVE 'EXCLUIR-VERSAO'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
               WHEN IN-ENCERRAR-OPERACAO
                    MOVE 'ENCERRAR-OPERACAO'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
               WHEN IN-EDITAR-VERSAO
                    MOVE 'EDITAR-VERSAO'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
               WHEN IN-APROVAR-COMPRA
                    MOVE 'APROVAR-COMPRA'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
               WHEN IN-CONFIRMAR-VERSAO
                    MOVE 'CONFIRMAR-VERSAO'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
               WHEN IN-COMPRAR
                    MOVE 'COMPRAR'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
               WHEN IN-ALTERAR-DADOS-COMPL
                    MOVE 'ALTERAR-DADOS-COMPLEMENTARES'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
               WHEN IN-DEV-P-CADASTRAMENTO
                    MOVE 'DEVOLVER-PARA-CADASTRAMENTO'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
               WHEN IN-DEV-P-AUTORIZACAO-CPR
                    MOVE 'DEVOLVER-PARA-AUTORIZACAO-DE-COMPRA'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
               WHEN IN-GERAR-VERSAO
                    MOVE 'GERAR-VERSAO'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
               WHEN IN-REABRIR
                    MOVE 'REABRIR'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
               WHEN IN-TRANSF-OTR-CTRA
                    MOVE 'TRANSFERIR-PARA-OUTRA-CARTEIRA'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
               WHEN IN-VINCULAR-PAGAMENTO
                    MOVE 'VINCULAR-PAGAMENTO'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
               WHEN IN-CONFERIR-CADASTRO
                    MOVE 'CONFERIR-CADASTRO'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
               WHEN IN-CANCELAR-AUT-COMPRA
                    MOVE 'CANCELAR-AUT-COMPRA'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
               WHEN IN-ATRIBUIR-TITULO
                    MOVE 'ATRIBUIR-TITULO'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
               WHEN IN-ATRIBUIR-ISIN
                    MOVE 'ATRIBUIR-ISIN'
                                 TO SGTS0147-NM-ACAO (SGTS0147-QT-OCR)
           END-EVALUATE

           MOVE    ZEROS               TO    IN-ACOES

           .
       300000-SAI.
           EXIT.
      *                                                                *
      *----------------------------------------------------------------*
       900000-EXECUTAR-MODULO          SECTION.
      *----------------------------------------------------------------*
      *    Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           INITIALIZE MSG1-RESP1 MSG1-RESP2 EIBRESP EIBRESP2

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM

               WHEN SGTSC069
                  EXEC CICS LINK
                            PROGRAM (SGTSC069  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSPE03
                  EXEC CICS LINK
                            PROGRAM (SGTSPE03  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSPE05
                  EXEC CICS LINK
                            PROGRAM (SGTSPE05  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN OTHER
                   SET   ERRO-PROGRAMADO     TO TRUE
                   MOVE  9006          TO  MSG1-CD-ERRO-TS
                   STRING
                   'Não definida a execução do módulo ' NM-PGM
                   DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
      *
           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE

               WHEN OTHER
                    SET ERRO-CICS      TO TRUE
                    MOVE 3004          TO    MSG1-CD-ERRO-TS
                    STRING
                    'Erro na execução do módulo ' NM-PGM ', '
                    'RESP  ='   ' ' MSG1-RESP1  ', '
                    'RESP2 =' ' ' MSG1-RESP2
                    DELIMITED BY SIZE  INTO  MSG1-TX-ERRO-TS
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *    Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       920000-CHAMAR-SGTST009          SECTION.
      *----------------------------------------------------------------*
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

      *    Movimento nome do módulo para monitoração de variáveis

           MOVE SGTST009              TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST009)
                COMMAREA (SGTST009-DADOS)
           END-EXEC

           IF  SGTST009-CD-RTN-PGM     NOT EQUAL ZEROS AND 100
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE SGTST009-CD-RTN-PGM
                                       TO MSG1-CD-ERRO-TS
                                          MSG1-CD-RTN-PGM-ACND
               MOVE SGTST009-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       920000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       920001-CHAMAR-SGTST005          SECTION.
      *----------------------------------------------------------------*
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

      *    Movimento nome do módulo para monitoração de variáveis
           MOVE SGTST005               TO NM-PGM

           EXEC CICS LINK
                PROGRAM  (SGTST005)
                COMMAREA (SGTST005-DADOS)
           END-EXEC

           IF  SGTST005-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE SGTST005-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS
               MOVE SGTST005-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       920001-SAI.
           EXIT.
      *----------------------------------------------------------------*
       920002-CHAMAR-SGTST012          SECTION.
      *----------------------------------------------------------------*
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

      *    Movimento nome do módulo para monitoração de variáveis
           MOVE SGTST012               TO NM-PGM

           EXEC CICS LINK
                PROGRAM  (SGTST012)
                COMMAREA (SGTST012-DADOS)
           END-EXEC

           IF  SGTST012-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE SGTST012-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS
               MOVE SGTST012-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       920002-SAI.
           EXIT.
      *-----------------------------------------------------------------
       920003-CHAMA-SGTST002           SECTION.
      *-----------------------------------------------------------------
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE  SGTST002              TO  NM-PGM
           EXEC CICS LINK
                     PROGRAM  ( SGTST002       )
                     COMMAREA ( SGTST002-DADOS )
           END-EXEC

           IF  SGTST002-CD-RTN-PGM         NOT EQUAL ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST002-CD-RTN-PGM
                                       TO  MSG1-CD-ERRO-TS
                                           MSG1-CD-RTN-PGM-ACND
               MOVE  SGTST002-MSG-ERRO
                                       TO  MSG1-TX-ERRO-TS
                                           MSG1-TX-RTN-PGM-ACND
               MOVE  SGTST002-PSC-ERRO-PGM
                                       TO  MSG1-CD-SEQL-PGM-ACND
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       920003-SAI.
           EXIT.

      *
      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*
           EVALUATE NM-PGM

               WHEN SGTST002
                  MOVE 'SGTST002-CD-TIT-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST002-CD-TIT-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTST002-NR-DVS-TIT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST002-NR-DVS-TIT
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT


               WHEN SGTST005
                  MOVE 'SGTST005-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST005-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTST005-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST005-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTST005-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST005-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTST009
                  MOVE 'SGTST009-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST009-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTST009-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST009-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTST009-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST009-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTST012
                  MOVE 'SGTST012-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST012-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSC069
                  MOVE 'SGTSC069-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSC069-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSC069-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSC069-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSC069-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSC069-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSPE03
                  MOVE 'SGTSPE03-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSPE03-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSPE03-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSPE03-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSPE03-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSPE03-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSPE05
                  MOVE 'SGTSPE05-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSPE05-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSPE05-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSPE05-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSPE05-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSPE05-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN OTHER
                    CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      * SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
      *                                                                *
      *----------------------------------------------------------------*
       970000-LER-CANAL                SECTION.
      *    Obtem o canal corrente
      *----------------------------------------------------------------*
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       970000-FIM.
           EXIT.
      *
      *----------------------------------------------------------------*
       971000-LER-REQUISICAO           SECTION.
      *----------------------------------------------------------------*
           MOVE 0020                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisicao
           MOVE CTE-PROG(5:8)          TO NM-PGM
           MOVE 'SGTS0147-RQSC'        TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      * -->> Início

      *    Obtem o tamanho esperado da requisicao
           MOVE LENGTH OF SGTS0147-RQSC TO QT-CCT-EPRO

      *    Valida tamanho esperado da requisicao X tamanho recebido

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA   TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
      * -->> Fim

           MOVE 0021                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0147-RQSC)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       971000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *----------------------------------------------------------------*
      *    A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *----------------------------------------------------------------*

      *    Le o container de erro dos modulos chamados
           SET GET-CONTAINER           TO TRUE
           MOVE 'PRM-ERRO'             TO NM-CTNR
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS GET
                     CONTAINER ( NM-CTNR                     )
                     INTO      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC

           EVALUATE TRUE
      *        Caso deseje personalizar ou não interromper o
      *        processamento, alterar para não dar erro aqui SOMENTE
      *        PARA O MODULO CHAMADO e tratar na section chamadora
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                    MOVE  NM-PGM       TO  MSG1-NM-PGM-ACND

                    IF NM-PGM(1:3) EQUAL 'SGT'
                       MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                                           MSG1-CD-ERRO-TS
                       MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS

                    ELSE
                       MOVE 3004          TO MSG1-CD-ERRO-TS
                       STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS

                       MOVE IIBKERRO-TX-ERRO TO MSG1-TX-RTN-PGM-ACND
                       MOVE IIBKERRO-CD-ERRO TO MSG1-CD-RTN-PGM-ACND
                    END-IF

      *             Monitorar variáveis do módulo chamado
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA

                  WHEN OTHER
                    SET ERRO-CONTAINER  TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       973000-LER-RESPOSTA             SECTION.
      *----------------------------------------------------------------*
      *    Recupera o container com os dados de resposta (saída).
      *                        NOME DO CONTAINER RESPOSTA
      *                        --------------------------
      *    Modulo CONTAINER  -     NOME_PGM-RPST
      *    Modulo IIB (FIXO) -     PRM-DADOS-RPST
      *---------------------------------------------------------------*
      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

      *    Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

      *Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Le a RESPOSTA de acordo com o programa que a gravou

           EVALUATE NM-PGM
      *
      *        Calcular a quantidade de caracteres esperado e limpar
      *        a area de dados antes de executar o GET
      *        Para matrizes com dados variaveis (clausula depending
      *        on) nao ha como utilizar o comando INITIALIZE

               WHEN SGTSC069
                      EXEC CICS GET
                              CONTAINER (NM-CTNR      )
                              INTO      (SGTSC069-RPST)
                              FLENGTH   (QT-CCT-LIDO  )
                    END-EXEC

               WHEN SGTSPE03
                     EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSPE03-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSPE05
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSPE05-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC
               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9003         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na leitura da resposta do módulo '
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *Ao chegar aqui, h certeza de que no houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       973100-LER-CONTAINER-NODATA     SECTION.
      *----------------------------------------------------------------*

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *    Obtem a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR    )
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973100-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *----------------------------------------------------------------*
           MOVE 0022                   TO MSG1-CD-SEQL-ERRO

      *    EXCLUI EVENTUAL CONTAINER DE ERRO DE OUTROS PROGRAMAS
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

      *    Monta o nome do container de resposta
           MOVE 'SGTS0147-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                )
                     FROM      (SGTS0147-RPST          )
                     FLENGTH   (LENGTH OF SGTS0147-RPST)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *----------------------------------------------------------------*
      *    Postar container com dados de requisição (entrada)
      *
      *                        NOME DO CONTAINER REQUISICAO
      *                        ----------------------------
      *
      *    Modulo CONTAINER  -     NOME_PGM-RQSC
      *    Modulo IIB (FIXO) -     PRM-DADOS-RQSC para NM-CTNR
      *
      *----------------------------------------------------------------*

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RQSC'                TO NM-CTNR(9:5)

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisicao do programa a executar
           EVALUATE NM-PGM
               WHEN SGTSC069
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                )
                              FROM      (SGTSC069-RQSC          )
                              FLENGTH   (LENGTH OF SGTSC069-RQSC)
                    END-EXEC

               WHEN SGTSPE03
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                )
                              FROM      (SGTSPE03-RQSC          )
                              FLENGTH   (LENGTH OF SGTSPE03-RQSC)
                    END-EXEC

               WHEN SGTSPE05
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                )
                              FROM      (SGTSPE05-RQSC          )
                              FLENGTH   (LENGTH OF SGTSPE05-RQSC)
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9004         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na gravação da requisição do módulo '
                            NM-PGM '.' DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       982000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           MOVE NM-PGM          TO NM-CTNR(1:8)
           MOVE '-RPST'         TO NM-CTNR(9:5)

           SET DEL-CONTAINER           TO TRUE

      *    Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER (NM-CTNR)
                     RESP      (MSG1-RESP1)
                     RESP2     (MSG1-RESP2)
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *    Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *----------------------------------------------------------------*

      *    Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE 'PRM-ERRO'             TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER (NM-CTNR   )
                     RESP      (MSG1-RESP1)
                     RESP2     (MSG1-RESP2)
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *----------------------------------------------------------------*
      *    Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS PUT
                     CONTAINER (PRM-ERRO                   )
                     FROM      (IIBKERRO-PRM-ERRO          )
                     FLENGTH   (LENGTH OF IIBKERRO-PRM-ERRO)
                     RESP      (MSG1-RESP1                 )
                     RESP2     (MSG1-RESP2                 )
           END-EXEC
           .
       984000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       990000-ENCERRAR                 SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                RETURN
           END-EXEC
           .
       990000-ENCERRAR-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *     As sections abaixo representam os tratamentos de erro
      *    utilizando o sistema MSG de mensageria (rotina MSGS0001)
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF MSG1-CD-ERRO-TS EQUAL ZEROS
              MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF

           IF MSG1-TX-ERRO-TS EQUAL SPACES
              MOVE 'Erro não especificado no módulo.'
                                       TO MSG1-TX-ERRO-TS
           END-IF

      *Verifica se o tipo de erro gerado possui tratamento interno
           EVALUATE TRUE
               WHEN ERRO-CONTAINER
                    PERFORM 999004-TRATA-ERRO-CTNR
               WHEN ERRO-TAMANHO-AREA
                    PERFORM 999005-TRATA-ERRO-AREA-DADOS
               WHEN ERRO-CANAL
                    PERFORM 999006-TRATA-ERRO-CANAL
               WHEN OTHER
                    CONTINUE
           END-EVALUATE

      *Adiciona as variáveis de entrada para monitoração

           MOVE 'SGTS0147-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
           MOVE SGTS0147-CD-CTRA-TIT-VL   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0147-CD-OPR-ETRD-MCP'  TO MSGKW999-NM-VRV-MNT
           IF   SGTS0147-CD-OPR-ETRD-MCP IS NUMERIC
               MOVE SGTS0147-CD-OPR-ETRD-MCP    TO MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Código não numérico.'      TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0147-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTS0147-NR-VRS-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT


      *    Grava o erro gerado no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG

           MOVE MSG1-CD-ERRO-TS          TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO

      *    Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

      *    Encerra a transacao
           PERFORM 990000-ENCERRAR
           .
       999000-SAI.
           EXIT.
      * --------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * --------------------------------------*
-INC HLPKP999
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999
      * --------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
      * --------------------------------------*
-INC MSGKPMG1