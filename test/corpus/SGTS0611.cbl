      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0611
      * ANALISTA..: F7023235 - MATHEUS SANTIAGO
      * AUTOR.....: F7023235 - MATHEUS SANTIAGO
      * DATA......: 01/04/2025
      * SERVICO...: 000000
      * OPERACAO..: 12597713.1
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      * OBJETIVO..: Consulta informações detalhadas da versão atual
      *...........: de um evento que ocorreu fora da B3.
      *================================================================*
      *   VRS002  | 20/05/2025 F7023235 - Usar 0 no NR-SEQL-EVT-EXCD
      *                                   para pegar o último evento
      *                                   adicionado ao título
      *   VRS001  | 01/04/2025 F7023235 - IMPLANTAÇÃO
      *================================================================*

      *************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. SGTS0611.
        AUTHOR. F7023235.
      *************************
      *
      **********************
       ENVIRONMENT DIVISION.
      **********************
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
      ***************
       DATA DIVISION.
      ***************
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------

       77  CTE-PROG                    PIC  X(18)  VALUE
                                       '*** SGTS0611 ***'.
       77  CTE-VERS                    PIC  X(06)  VALUE 'VRS002'.

      *--- Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTST138                PIC  X(08) VALUE 'SGTST138'.
           03  SGTST139                PIC  X(08) VALUE 'SGTST139'.
           03  SGTST140                PIC  X(08) VALUE 'SGTST140'.
      *
      *------------------------ G U A R D A S  ------------------------*
      *
       77  GDA-NR-SEQL-EVT-EXCD        PIC  S9(4) COMP-5.

      *--- Definição dos Variaveis do Cobolito
       COPY ALMKCBL1.

      *--- Book do programa de ambiente - CICS5000
      *--- Definido na work para preservar as informacoes
       COPY HLPKE01B.

      *--- Container de erro
       01  CTNR-PRM-ERRO               PIC X(16) VALUE 'PRM-ERRO'.
      *--- Container do usuario logado
       01  CTNR-PRM-INF-OPRD           PIC X(16) VALUE 'PRM-INF-OPRD'.

      *--- Areas de Controle de Programa Chamado (CONTAINER)
       01  CTL-PGM-CHMD.
           03  NM-PGM-CHMD             PIC  X(08)         VALUE ' '.
           03  FILLER                  PIC  X(01)         VALUE ' '.
           03  MSG-PGM-CHMD            PIC  X(111)        VALUE ' '.

       01  WS-ERRO-CICS.
           03  WS-RESP1                PIC  9(008).
           03  WS-RESP2                PIC  9(008).

      *--- Máscaras edição de variáveis p/ monitoração mensagens de
       01  MCR-S9-04       PIC +9999.
       01  MCR-S9-07       PIC +9999999.

      *--- Constantes para Ações dos Subprogramas
       77 CTE-SGTST138-ACAO-CONSULTA       PIC 9(01) VALUE 4.
       77 CTE-SGTST138-ACAO-CONSULTA-ATU   PIC 9(01) VALUE 5.
       77 CTE-SGTST139-ACAO-CONSULTA       PIC 9(01) VALUE 5.
       77 CTE-SGTST140-ACAO-CONSULTA       PIC 9(01) VALUE 5.

      *
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      *
      *-------------------------- B O O K S ---------------------------*
      *
      *----------------------------------------------------------------*
      * SGTK9611 - BOOK DE ENTRADA DO PROGRAMA.                         *
      *----------------------------------------------------------------*
      *
       01  SGTS0611-DADOS.
       COPY SGTK0611.
      *
      *----------------------------------------------------------------*
      * SGTST138 - Book do program que faz CRUD na CRNG-EVT-TIT-EXCD
      *----------------------------------------------------------------*
      *
       01  SGTST138-DADOS.
       COPY SGTKT138.
      *
      *----------------------------------------------------------------*
      * SGTST139 - Book do program que faz CRUD na EST_EVT_TIT_EXCD
      *----------------------------------------------------------------*
      *
       01  SGTST139-DADOS.
       COPY SGTKT139.
      *
      *----------------------------------------------------------------*
      * SGTST140 - Book do program que faz CRUD na PRCT_CRNG_TIT_EXCD
      *----------------------------------------------------------------*
      *
       01  SGTST140-DADOS.
       COPY SGTKT140.
      *
      *----------------------------------------------------------------*
      *                            BOOKS IIB                           *
      *----------------------------------------------------------------*
      *
       COPY IIBKSRVC. *> Interface com o sistema IIB
      *
       COPY IIBKOPRD. *> Book contendo o codigo do usuario logado
      *
       COPY IIBKERRO. *> Variaveis para container de erro do IIB
      *
       COPY MSGKWMG1. *> Variaveis para captura de erro via MSG1
      *
       COPY SGTKERR1. *> Variaveis para tratamento de CONTAINER
      *
       COPY HLPKW999. *> Variaveis para tratamento de Abend e Condition

      *
      ********************
       PROCEDURE DIVISION.
      ********************
      *
      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL                                  SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM  100000-INICIALIZA.
           PERFORM  200000-PROCESSA.
           PERFORM  300000-ENCERRAR.
      *
       000099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       100000-INICIALIZA                                        SECTION.
      *----------------------------------------------------------------*
      *
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO.
           INITIALIZE SGTS0611-RPST WS-RESP1.

       COPY ALMKCBL2. *> Habilita Testes e Debug de Operacoes IIB
       COPY HLPKE999. *> Habilita captura automatica de abend do CICS

           MOVE CTE-PROG(5:8)          TO NM-PGM.
      * ----------------------------------------------------------------

           PERFORM 110000-INICIA-VARIAVEIS.
           PERFORM 120000-OBTER-NOME-DO-CANAL.
           PERFORM 130000-LER-REQUISICAO-RECEBIDA.
      *    PERFORM 130000-LER-DADOS-OPRD.
           PERFORM 140000-VALIDAR-REQUISICAO.
      *
       100099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       110000-INICIA-VARIAVEIS                                  SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE SGTS0611-RQSC.

           MOVE 'N' TO SGTS0611-S-IN-EVT-CTBD
                       SGTS0611-S-IN-ISN-IR-EVT
                       SGTS0611-S-IN-RTC-IR-EMT.
      *
       110099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       120000-OBTER-NOME-DO-CANAL                               SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO.

      *--- Por causa da captura automática de erro CICS, deve ser
      *--- previsto o erro de container
           SET  ERRO-CANAL             TO  TRUE.

           EXEC CICS ASSIGN
                CHANNEL ( NM-CNL )
           END-EXEC.

      *--- Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE.
      *
       120099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       130000-LER-REQUISICAO-RECEBIDA                           SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 0003                    TO MSG1-CD-SEQL-ERRO.

           MOVE LENGTH OF SGTS0611-RQSC TO QT-CCT-EPRO.

      *--- Configura dados para leitura do container de requisicao
           SET  GET-CONTAINER           TO TRUE.

      *--- Por causa da captura automatica de erro CICS, deve ser
      *--- previsto o erro de container
           SET  ERRO-CONTAINER          TO TRUE.

      *--- Obtem a quantidade de caracteres no container
           MOVE ZEROS                   TO QT-CCT-LIDO.

           EXEC CICS GET
              CONTAINER ( 'SGTS0611-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              NODATA
              FLENGTH   ( QT-CCT-LIDO             )
           END-EXEC.

           IF  QT-CCT-LIDO               NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               MOVE 3001                 TO MSG1-CD-ERRO-TS
               MOVE 'Tamanho container requisicao invalido'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.

           EXEC CICS GET
              CONTAINER ( 'SGTS0611-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTS0611-RQSC )
              INTO      ( SGTS0611-RQSC           )
           END-EXEC.

           SET  SEM-ERRO                 TO TRUE.

       130099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       130000-LER-DADOS-OPRD                                    SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO.

      *--- CONFIGURA DADOS PARA LEITURA DO CONTAINER DE REQUISICAO
           SET  GET-CONTAINER          TO TRUE.
           MOVE CTNR-PRM-INF-OPRD      TO NM-CTNR.

           MOVE 0041                   TO MSG1-CD-SEQL-ERRO.
      *--- POR CAUSA DA CAPTURA AUTOMATICA DE ERRO CICS, DEVE SER
      *--- PREVISTO ERRO DE CONTAINER
           SET  ERRO-CONTAINER         TO TRUE.

           MOVE 0042                   TO MSG1-CD-SEQL-ERRO.
           INITIALIZE PRM-INF-OPRD.
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.
           MOVE 0043                   TO MSG1-CD-SEQL-ERRO.

      *--- AO CHEGAR AQUI, HA CERTEZA DE QUE NAO HOUVE ERRO ALGUM
           SET  SEM-ERRO               TO TRUE.

       130000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       140000-VALIDAR-REQUISICAO                                SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 0005                     TO MSG1-CD-SEQL-ERRO.

           IF  SGTS0611-E-CD-ITMT-FNCO EQUAL SPACES
               MOVE  061101              TO MSG1-CD-ERRO-TS
               MOVE 'Codigo instrumento financeiro invalido'
                                         TO MSG1-TX-ERRO-TS
               SET MNT-VRV               TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.

           IF  SGTS0611-E-CD-GR-EVT-FDAR EQUAL ZEROS
               MOVE  061102              TO MSG1-CD-ERRO-TS
               MOVE  0000                TO MSG1-CD-ERRO-TS
               MOVE 'Codigo grupo evento fiduciario invalido'
                                         TO MSG1-TX-ERRO-TS
               SET MNT-VRV               TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.

           IF  SGTS0611-E-NR-SEQL-EVT-EXCD < 0
               MOVE  061103              TO MSG1-CD-ERRO-TS
               MOVE 'Numero sequential evento excluido invalido'
                                         TO MSG1-TX-ERRO-TS
               SET MNT-VRV               TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.

      *
       140099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       200000-PROCESSA                                          SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO.

           IF SGTS0611-E-NR-SEQL-EVT-EXCD EQUAL ZERO
               PERFORM 210000-BUSCA-INFO-EVT-ATU
           ELSE
               PERFORM 220000-BUSCA-INFO-EVT
           END-IF.

           PERFORM 230000-BUSCA-EST-EVT.
           PERFORM 240000-BUSCA-EST-PRCT-EVT.
      *
       200099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       210000-BUSCA-INFO-EVT-ATU                                SECTION.
      *----------------------------------------------------------------*
      *> Pega informações mais recentes do evento (CRNG_EVT_TIT_EXCD)
      *> via SGTST138
      *----------------------------------------------------------------*


           SET SGTST138-CONSULTAR-ULT-SEQL TO TRUE.
           PERFORM 230000-CHAMA-SGTST138.
      *
       210099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       220000-BUSCA-INFO-EVT                                    SECTION.
      *----------------------------------------------------------------*
      *
           SET SGTST138-CONSULTAR TO TRUE.
           MOVE SGTS0611-E-NR-SEQL-EVT-EXCD TO
                                              SGTST138-NR-SEQL-EVT-EXCD.

           PERFORM 230000-CHAMA-SGTST138.
      *
       220099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       230000-CHAMA-SGTST138                                    SECTION.
      *----------------------------------------------------------------*
      *> Consulta informações do evento (CRNG_EVT_TIT_EXCD) via SGTST13
      *----------------------------------------------------------------*
      *
           MOVE 0011                       TO MSG1-CD-SEQL-ERRO.
      *-----------------------------------------------------------------
           INITIALIZE SGTST138-DADOS.
      * ----------------------------------------------------------------
           MOVE SGTS0611-E-CD-ITMT-FNCO    TO SGTST138-CD-ITMT-FNCO.
           MOVE SGTS0611-E-CD-GR-EVT-FDAR  TO SGTST138-CD-GR-EVT-FDAR.

           MOVE PRM-INF-OPRD               TO SGTST138-CD-CHV-USU.
      *-----------------------------------------------------------------
           EXEC CICS LINK
                PROGRAM ( SGTST138 )
                COMMAREA( SGTST138-DADOS )
                LENGTH  ( LENGTH OF SGTST138-DADOS )
                RESP    ( WS-RESP1 )
                RESP2   ( WS-RESP2 )
           END-EXEC.
      *-----------------------------------------------------------------
      *--- Verificacao da Resposta CICS
      *-----------------------------------------------------------------
           IF WS-RESP1 NOT EQUAL DFHRESP(NORMAL)
              SET ERRO-PROGRAMADO     TO TRUE
              MOVE 3004               TO MSG1-CD-ERRO-TS

              STRING 'Erro ao acionar o módulo SGTST138.'
                      'RESP  = '    WS-RESP1  ', '
                      'RESP2 = '    WS-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
              END-STRING

              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.
      *-----------------------------------------------------------------
      *--- Verifica Codigo de Retorno da Aplicacao
      *-----------------------------------------------------------------
           IF SGTST138-CD-RTN-PGM NOT EQUAL 0
              SET   ERRO-PROGRAMA-ACIONADO TO TRUE
              MOVE SGTST138              TO MSG1-NM-PGM-ACND
                                            NM-PGM-CHMD
              MOVE SGTST138-CD-RTN-PGM   TO MSG1-CD-ERRO-TS
                                            MSG1-CD-RTN-PGM-ACND
              MOVE SGTST138-MSG-ERRO     TO MSG1-TX-ERRO-TS
                                            MSG1-TX-RTN-PGM-ACND
              SET MNT-VRV                TO TRUE *> Habilita log variavei
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.
      * ----------------------------------------------------------------
           MOVE SGTST138-CD-PRD            TO SGTS0611-S-CD-PRD.
           MOVE SGTST138-CD-MDLD           TO SGTS0611-S-CD-MDLD.
           MOVE SGTST138-NR-CT-LQDC        TO SGTS0611-S-NR-CT-LQDC.
           MOVE SGTST138-CD-MDLD-LQDC      TO SGTS0611-S-CD-MDLD-LQDC.
           MOVE SGTST138-VL-EVT            TO SGTS0611-S-VL-EVT.
           MOVE SGTST138-VL-PU-EVT         TO SGTS0611-S-VL-PU-EVT.
           MOVE SGTST138-QT-PAPL-EVT       TO SGTS0611-S-QT-PAPL-EVT.
           MOVE SGTST138-IN-EVT-CTBD       TO SGTS0611-S-IN-EVT-CTBD.
           MOVE SGTST138-IN-ISN-IR-EVT     TO SGTS0611-S-IN-ISN-IR-EVT.
           MOVE SGTST138-IN-RTC-IR-EMT     TO SGTS0611-S-IN-RTC-IR-EMT.
           MOVE SGTST138-DT-EVT            TO SGTS0611-S-DT-EVT.
           MOVE SGTST138-CD-USU-RSP-ATL    TO SGTS0611-S-CD-USU-RSP-ATL.
           MOVE SGTST138-TS-ULT-ATL        TO SGTS0611-S-TS-ULT-ATL.
           MOVE SGTST138-CD-CTRA-TIT-VL    TO SGTS0611-S-CD-CTRA-TIT-VL.
           MOVE SGTST138-VL-IR-EMT         TO SGTS0611-S-VL-IR-EMT.
           MOVE SGTST138-NR-SEQL-EVT-EXCD  TO
                                            SGTS0611-S-NR-SEQL-EVT-EXCD
                                            GDA-NR-SEQL-EVT-EXCD.
      *
       230099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       230000-BUSCA-EST-EVT                                     SECTION.
      *----------------------------------------------------------------*
      *> Consulta estado atual do evento (EST_EVT_TIT_EXCD) via SGTST139
      *----------------------------------------------------------------*
           MOVE 0012                      TO MSG1-CD-SEQL-ERRO.
      *-----------------------------------------------------------------
           INITIALIZE SGTST139-DADOS.
      *-----------------------------------------------------------------
           MOVE CTE-SGTST139-ACAO-CONSULTA TO SGTST139-ACAO.
           MOVE PRM-INF-OPRD               TO SGTST139-CD-CHV-USU.

           MOVE SGTS0611-E-CD-ITMT-FNCO    TO SGTST139-CD-ITMT-FNCO.
           MOVE SGTS0611-E-CD-GR-EVT-FDAR  TO SGTST139-CD-GR-EVT-FDAR.
           MOVE GDA-NR-SEQL-EVT-EXCD       TO SGTST139-NR-SEQL-EVT-EXCD.
      *-----------------------------------------------------------------
           EXEC CICS LINK
                PROGRAM ( SGTST139 )
                COMMAREA( SGTST139-DADOS )
                LENGTH  ( LENGTH OF SGTST139-DADOS )
                RESP    ( WS-RESP1 )
                RESP2   ( WS-RESP2 )
           END-EXEC.
      *-----------------------------------------------------------------
      *--- Verificacao da Resposta CICS
      *-----------------------------------------------------------------
           IF WS-RESP2 NOT EQUAL DFHRESP(NORMAL)
              SET ERRO-PROGRAMADO     TO TRUE
              MOVE 3004               TO MSG1-CD-ERRO-TS
              STRING 'Erro ao acionar o módulo SGTST139.'
                      'RESP  = '    WS-RESP1  ', '
                      'RESP2 = '    WS-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING

              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.
      *-----------------------------------------------------------------
      *--- Verifica Codigo de Retorno da Aplicacao
      *-----------------------------------------------------------------
           IF SGTST139-CD-RTN-PGM NOT EQUAL 0
              SET ERRO-PROGRAMA-ACIONADO TO TRUE
              MOVE SGTST139              TO MSG1-NM-PGM-ACND
                                            NM-PGM-CHMD
              MOVE SGTST139-CD-RTN-PGM   TO MSG1-CD-ERRO-TS
                                            MSG1-CD-RTN-PGM-ACND
              MOVE SGTST139-MSG-ERRO     TO MSG1-TX-ERRO-TS
                                            MSG1-TX-RTN-PGM-ACND
              SET MNT-VRV                TO TRUE
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.
      *-----------------------------------------------------------------
           MOVE SGTST139-CD-USU-ULT-ATL  TO  SGTS0611-S-CD-USU-ULT-ATL.
           MOVE SGTST139-CD-EST-EVT-EXCD TO  SGTS0611-S-CD-EST-EVT-EXCD.
           MOVE SGTST139-TX-JST-EVT-EXCD TO  SGTS0611-S-TX-JST-EVT-EXCD.
      *
       230099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       240000-BUSCA-EST-PRCT-EVT                                SECTION.
      *----------------------------------------------------------------*
      *> Consulta estado atual do evento (EST_EVT_TIT_EXCD) via SGTST139
      *----------------------------------------------------------------*
           MOVE 0013                       TO MSG1-CD-SEQL-ERRO.
      *-----------------------------------------------------------------
           INITIALIZE SGTST140-DADOS.
      *-----------------------------------------------------------------
           MOVE CTE-SGTST140-ACAO-CONSULTA TO SGTST140-ACAO.
           MOVE PRM-INF-OPRD               TO SGTST140-CD-CHV-USU.

           MOVE SGTS0611-E-CD-ITMT-FNCO    TO SGTST140-CD-ITMT-FNCO.
           MOVE SGTS0611-E-CD-GR-EVT-FDAR  TO SGTST140-CD-GR-EVT-FDAR.
           MOVE GDA-NR-SEQL-EVT-EXCD       TO SGTST140-NR-SEQL-EVT-EXCD.
      *-----------------------------------------------------------------
           EXEC CICS LINK
                PROGRAM ( SGTST140 )
                COMMAREA( SGTST140-DADOS )
                LENGTH  ( LENGTH OF SGTST140-DADOS )
                RESP    ( WS-RESP1 )
                RESP2   ( WS-RESP2 )
           END-EXEC.
      *-----------------------------------------------------------------
      *--- Verificacao da Resposta CICS
      *-----------------------------------------------------------------
           IF WS-RESP2 NOT EQUAL DFHRESP(NORMAL)
              SET ERRO-PROGRAMADO     TO TRUE
              MOVE 3004               TO MSG1-CD-ERRO-TS
              STRING 'Erro ao acionar o módulo SGTST140.'
                      'RESP  = '    WS-RESP1  ', '
                      'RESP2 = '    WS-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING

              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.
      *-----------------------------------------------------------------
      *--- Verifica Codigo de Retorno da Aplicacao
      *-----------------------------------------------------------------
           IF SGTST140-CD-RTN-PGM NOT EQUAL 0
              SET ERRO-PROGRAMA-ACIONADO TO TRUE
              MOVE SGTST140              TO MSG1-NM-PGM-ACND
                                            NM-PGM-CHMD
              MOVE SGTST140-CD-RTN-PGM   TO MSG1-CD-ERRO-TS
                                            MSG1-CD-RTN-PGM-ACND
              MOVE SGTST140-MSG-ERRO     TO MSG1-TX-ERRO-TS
                                            MSG1-TX-RTN-PGM-ACND
              SET MNT-VRV                TO TRUE
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.
      *-----------------------------------------------------------------
           MOVE SGTST140-CD-EST-PRCT-CRNG TO
                                            SGTS0611-S-CD-EST-PRCT-CRNG.
           MOVE SGTST140-TS-PRCT-RTN-EVT TO  SGTS0611-S-TS-PRCT-RTN-EVT.
           MOVE SGTST140-TX-PRCT-EVT     TO  SGTS0611-S-TX-PRCT-EVT.

      *
       240099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       300000-ENCERRAR                                          SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 0020                     TO MSG1-CD-SEQL-ERRO.

           PERFORM 980000-GRAVAR-CONTAINER-RPST.
           PERFORM 999999-ENCERRAR.
      *
       300099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       980000-GRAVAR-CONTAINER-RPST                             SECTION.
      *----------------------------------------------------------------*
           MOVE  0021                  TO  MSG1-CD-SEQL-ERRO.

      *--- Exclui eventual container de erro de outros programas
           PERFORM  981000-EXCLUIR-CONTAINER-ERRO.

      *--- Monta o nome do container de resposta
           SET   PUT-CONTAINER         TO  TRUE.

      *--- Por causa da captura automática de erro CICS, deve ser
      *--- previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE.

           EXEC CICS  PUT
                      CONTAINER ( 'SGTS0611-RPST'         )
                      CHANNEL   ( NM-CNL )
                      FROM      ( SGTS0611-RPST           )
                      FLENGTH   ( LENGTH OF SGTS0611-RPST )
           END-EXEC.

      *--- AO CHEGAR AQUI, HÁ CERTEZA DE QUE NÃO HOUVE ERRO ALGUM
           SET   SEM-ERRO              TO  TRUE.
      *
       980000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       981000-EXCLUIR-CONTAINER-ERRO                            SECTION.
      *----------------------------------------------------------------*
      * Exclui o container de erro para evitar lixo em caso de reuso
      * da mesma sessão CICS ou execuções encadeadas.
      *-----------------------------------------------------------------
           MOVE  0022                  TO  MSG1-CD-SEQL-ERRO.

      *--- Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO.

           MOVE CTNR-PRM-ERRO          TO NM-CTNR.
           SET DEL-CONTAINER           TO TRUE.

      *--- Exclui o container de resposta
      *--- Usa RESP/RESP2 pois HLPKE999 pode não estar ativo aqui
      *--- ou queremos tratar CONTAINERERR silenciosamente.
           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR    )
                      RESP      ( MSG1-RESP1 )
                      RESP2     ( MSG1-RESP2 )
           END-EXEC.

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE.

       981099-SAI.
           EXIT.

      *-----------------------------------------------------------------
      * Rotinas de Tratamento de Erro
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------
      * Ponto central de tratamento de erro. Chama rotinas específicas.
      *-----------------------------------------------------------------
           IF  ERRO-PROGRAMA-ACIONADO
               PERFORM 999100-MONITORAR-VRV-PGM-CHMD
           END-IF.

           PERFORM 999200-MONITORAR-VRV-PGM-E-ENC.
      *
       999000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999100-MONITORAR-VRV-PGM-CHMD   SECTION.
      *-----------------------------------------------------------------
      * Monitora as variáveis de ENTRADA do módulo chamado que falhou.
      *-----------------------------------------------------------------

           EVALUATE NM-PGM-CHMD

               WHEN SGTST138
                   MOVE 'SGTST138-ACAO'           TO MSGKW999-NM-VRV-MNT
                   MOVE SGTST138-ACAO             TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT

                   MOVE 'SGTST138-CD-ITMT-FNCO'   TO MSGKW999-NM-VRV-MNT
                   MOVE SGTST138-CD-ITMT-FNCO     TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT

                   MOVE 'SGTST138-CD-GR-EVT-FDAR' TO MSGKW999-NM-VRV-MNT
                   MOVE SGTST138-CD-GR-EVT-FDAR   TO MCR-S9-07 *> Exemplo
                   MOVE MCR-S9-07                 TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT

                   MOVE 'SGTST138-NR-SEQL-EVT-EXCD' TO
                                                     MSGKW999-NM-VRV-MNT
                   MOVE SGTST138-NR-SEQL-EVT-EXCD TO MCR-S9-07
                   MOVE MCR-S9-07                 TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTST139
                    MOVE 'SGTST139-ACAO'          TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST139-ACAO            TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST139-CD-ITMT-FNCO'  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST139-CD-ITMT-FNCO    TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST139-CD-GR-EVT-FDAR'TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST139-CD-GR-EVT-FDAR  TO MCR-S9-07
                    MOVE MCR-S9-07                TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST139-NR-SEQL-EVT-EXCD' TO
                                            MSGKW999-NM-VRV-MNT
                    MOVE SGTST139-NR-SEQL-EVT-EXCD TO MCR-S9-07
                    MOVE MCR-S9-07                TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTST140
                    MOVE 'SGTST140-CD-CHV-USU'    TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST140-CD-CHV-USU      TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST140-ACAO'          TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST140-ACAO            TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST140-CD-ITMT-FNCO'  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST140-CD-ITMT-FNCO    TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST140-CD-GR-EVT-FDAR'TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST140-CD-GR-EVT-FDAR  TO MCR-S9-07
                    MOVE MCR-S9-07                TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST140-NR-SEQL-EVT-EXCD' TO
                                            MSGKW999-NM-VRV-MNT
                    MOVE SGTST140-NR-SEQL-EVT-EXCD TO MCR-S9-07
                    MOVE MCR-S9-07               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN OTHER
                    CONTINUE

           END-EVALUATE.
      *
       999100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999200-MONITORAR-VRV-PGM-E-ENC SECTION.
      *-----------------------------------------------------------------
      * Define msg erro padrão, faz rollback, trata erros CICS comuns,
      * monitora variaveis de ENTRADA do programa principal (se MNT-VRV)
      * notifica MSG1 e grava container de erro antes de encerrar.
      *-----------------------------------------------------------------

      *--- Garante que há um código e texto de erro
           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF.
           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE 'Erro nao especificado no modulo.'
                                        TO MSG1-TX-ERRO-TS
           END-IF.

      *--- Desfaz alterações DB2/etc
           EXEC CICS SYNCPOINT ROLLBACK END-EXEC.

      *--- Tratamento específico para erros CICS comuns
           EVALUATE TRUE
               WHEN ERRO-CONTAINER
                    PERFORM 999004-TRATA-ERRO-CTNR
               WHEN ERRO-TAMANHO-AREA
                    PERFORM 999005-TRATA-ERRO-AREA-DADOS
               WHEN ERRO-CANAL
                    PERFORM 999006-TRATA-ERRO-CANAL
               WHEN OTHER
                    CONTINUE
           END-EVALUATE.

      *--- Adiciona variáveis de ENTRADA do SGTS0611 para monitoração
      *--- (Apenas se MNT-VRV foi ativado na validação/erro específico
           IF MNT-VRV
               MOVE 'SGTS0611-E-CD-ITMT-FNCO'   TO MSGKW999-NM-VRV-MNT
               MOVE SGTS0611-E-CD-ITMT-FNCO     TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0611-E-CD-GR-EVT-FDAR' TO MSGKW999-NM-VRV-MNT
               MOVE SGTS0611-E-CD-GR-EVT-FDAR   TO MCR-S9-07 *> Exemplo
               MOVE MCR-S9-07                TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0611-E-NR-SEQL-EVT-EXCD' TO MSGKW999-NM-VRV-MNT
               MOVE SGTS0611-E-NR-SEQL-EVT-EXCD TO MCR-S9-07 *> Exemplo
               MOVE MCR-S9-07                TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT
           END-IF.

      *--- Prepara dados para container de erro e chama MSG1
           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO.
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO.
           PERFORM MSG1-NTF-MSG. *> Notifica sistema MSG1

      *--- Grava Container de Erro para o chamador (IIB/Frontend)
           EXEC CICS PUT
                      CONTAINER ( CTNR-PRM-ERRO )
                      CHANNEL   ( NM-CNL )
                      FROM      ( IIBKERRO-PRM-ERRO )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      RESP      ( MSG1-RESP1 )
                      RESP2     ( MSG1-RESP2 )
           END-EXEC.
      *--- Encerra a transacao CICS
           PERFORM 999999-ENCERRAR.
      *
       999200-SAI.
           EXIT.

      *----------------------------------------
      * Includes Padrão de Tratamento de Erro e MSG1
      * (Assumindo que são os mesmos usados no SGTS250A)
      *----------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION. (Se existir/necessário)
      *----------------------------------------
       COPY HLPKP999.
      *----------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
       COPY SGTKP999.
      *----------------------------------------
      * Rotinas internas do MSG1 para montar containers
      *----------------------------------------
       COPY MSGKPMG1.

      *---------------------------------------*
       999999-ENCERRAR                 SECTION.
      *---------------------------------------*
           MOVE  9999                  TO  MSG1-CD-SEQL-ERRO. *> Final
           EXEC CICS RETURN END-EXEC.
      *
       999999-SAI.
           EXIT.
