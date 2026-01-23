      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0297
      * ANALISTA..: F8785286 - Roseli Akemi Kawahata
      * AUTOR.....: F0950413 - António Francisco Mendonça Silva
      * DATA......: 06/11/2019
      * OBJETIVO..: Atualizar Estado da Ordem de Negociação na Opção
      *             Concluir Venda (7 para 5).
      * COMPILACAO: 52 - PSOSE600 - Cobol 4.2 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS001 16.10.2019 - F0950413 - 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0297.

      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-WS               PIC X(40) VALUE
                                       '*** WSS COMECA AQUI ***'.
       77  CTE-PROG                    PIC X(18) VALUE
                                       '*** SGTS0297 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS001'.

      *--> CONTAINER do usuário logado
       77  CTNR-PRM-INF-OPRD           PIC X(16) VALUE 'PRM-INF-OPRD'.

      *-----------------------------------------------------------------
      *                     CONTAINERS UTILIZADOS
      *-----------------------------------------------------------------
      *--> CONTAINER DE ERRO
       77  CTNR-PRM-ERRO               PIC  X(16)     VALUE 'PRM-ERRO'.

      *--> Texto que destaca as mensagens "1069" e "1079" deste pgm
       77  TX-DTQ-MSG                  PIC  X(09)     VALUE 'conclusão'.

      *--> Módulos a serem acionados via 'CONTAINER'
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTSL069                PIC X(08) VALUE 'SGTSL069'.
           03  SGTSI078                PIC X(08) VALUE 'SGTSI078'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B

       01  CTE-FIM-WS                  PIC X(32)  VALUE
                                             '*** WSS TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  CTE-INICIO-LS               PIC  X(32)  VALUE
                                             '*** LS COMECA AQUI ***'.
      *--> Areas de trabalho
       01  AUX-CD-TIP-EST-NEGO         PIC  9999.

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC   ---------------9.

      *--> Variaveis utilizados para leitura
       01  REG-WS.
           03 CD-TIP-EST-NEGO          PIC S9(09) COMP-5 VALUE 0.
           03 TS-EST-ORD-NEGO          PIC  X(26) VALUE ' '.
           03 CD-USU-ULT-ATL           PIC  X(08) VALUE ' '.
           03 NR-VRS-OPR-ETRD          PIC S9(04) COMP-5 VALUE 0.
           03 NR-SEQL-ORD-NEGO         PIC S9(09) COMP-5 VALUE 0.

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book do container de erro
-INC IIBKERRO
      *--> Book para captura de erro via MSG1
-INC MSGKWMG1
      *--> Variaveis para tratamento de Abend e Condition
-INC HLPKW999
      *--> Variaveis para tratamento de CONTAINER
-INC SGTKERR1
      *--> Book dos dados do usuário
-INC IIBKOPRD

      *-----------------------------------------------------------------
      *    Book das Subrotinas Acionadas
      *-----------------------------------------------------------------
      *--> Inclusão na tabela 'DB2SGT.EST_ORD_NEGO_OPR'
       01  SGTSI078-DADOS.
-INC SGTKI078

      *--> Lista versão aprovada - DB2SGT.EST_VRS_OPR_ETRD
       01  SGTSL069-DADOS.
-INC SGTKL069

      *--> Book do programa
       01  SGTS0297-DADOS.
-INC SGTK0297

      *--> Area de trabalho SQL/ DB2.
           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

       01  CTE-FIM-LS                  PIC  X(32)  VALUE
                                             '*** LS TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
-INC ALMKCBL2

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997

      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999
           MOVE CTE-PROG(5:8)          TO NM-PGM

           PERFORM 100000-OBTER-NOME-DO-CANAL
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
           PERFORM 970000-LER-REQUISICAO-RECEBIDA

           PERFORM 120000-VALIDAR-REQUISICAO
           PERFORM 974000-LER-DADOS-OPRD
           PERFORM 130000-CONSISTE-EST-OPR
           PERFORM 140000-CONSISTE-ESTADO-ORD

           PERFORM 200000-DEVOLUCAO-CONCLUSAO

           PERFORM 999999-ENCERRAR
           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-OBTER-NOME-DO-CANAL      SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
                CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-VALIDAR-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           IF  SGTS0297-CD-CTRA-TVM    NOT > ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de títulos e valores mobiliários
      -             ' inválido.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0297-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
            IF SGTS0297-CD-OPR-ETRD-MCP = 0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0005               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' informado.'      TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
            END-IF
           END-IF

           IF  SGTS0297-NR-VRS-OPR-ETRD NOT > ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0006               TO MSG1-CD-ERRO-TS
               MOVE 'Numero da versão da operação estruturada inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0297-NR-SEQL-ORD-NEGO NOT > ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 1084               TO MSG1-CD-ERRO-TS
               MOVE 'Numero sequencial da ordem de negociação inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0297-TX-JST-EST-ORD =  ' '
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 1105               TO MSG1-CD-ERRO-TS
               MOVE 'Texto da justificativa do estado da ordem não infor
      -             'mado.'            TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-CONSISTE-EST-OPR         SECTION.
      *-----------------------------------------------------------------
      *--> Consulta estado atual da ordem de operação.
      *--> Caso o estado não seja encarteirado/aprovado, não pode vincu-
      *--> lar ordem de venda á oper. estruturada.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           SET  SGTSL069-LISTA-OPR     TO TRUE
           MOVE SGTS0297-CD-CTRA-TVM   TO SGTSL069-CD-CTRA-TIT-VL
           MOVE SGTS0297-CD-OPR-ETRD-MCP
                                       TO SGTSL069-CD-OPR-ETRD-MCP

           MOVE SGTSL069               TO NM-PGM
           PERFORM 900000-PROCEDIMENTOS-CONTAINER

           IF SGTSL069-CD-TIP-EST-OPR(1)    NOT EQUAL 5    OR
              SGTSL069-CD-TIP-EST-VRS(1)    NOT EQUAL 3
              SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
              MOVE  1069               TO  MSG1-CD-ERRO-TS

              STRING 'Código do estado da operação não permite '
                      DELIMITED BY SIZE
                      TX-DTQ-MSG
                      DELIMITED BY SPACES
                      ' de cadastramento de ordem de venda.'
                      DELIMITED BY SIZE
                      INTO                MSG1-TX-ERRO-TS
              END-STRING
              PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF SGTSL069-NR-VRS-OPR-ETRD-S(1)      NOT EQUAL
              SGTS0297-NR-VRS-OPR-ETRD
               SET   ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE  0514              TO  MSG1-CD-ERRO-TS
               MOVE  'Foi encontrada versão mais atualizada da operação
      -              'vinculada.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-CONSISTE-ESTADO-ORD      SECTION.
      *-----------------------------------------------------------------
      *--> Consulta estado atual da ordem de Negociação.
      *--> Caso o estado não seja = 1 (Em edição), não pode efetuar a
      *--> Finalização do Cadastro
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                SELECT A.CD_TIP_EST_NEGO
                     , A.TS_EST_ORD_NEGO
                     , A.CD_USU_ULT_ATL
                INTO  :CD-TIP-EST-NEGO
                   ,  :TS-EST-ORD-NEGO
                   ,  :CD-USU-ULT-ATL
                FROM DB2SGT.EST_ORD_NEGO_OPR A
                WHERE A.CD_CTRA_TIT_VL  = :SGTS0297-CD-CTRA-TVM
                 AND A.CD_OPR_ETRD_MCP  = :SGTS0297-CD-OPR-ETRD-MCP
                 AND A.NR_VRS_OPR_ETRD  = :SGTS0297-NR-VRS-OPR-ETRD
                 AND A.NR_SEQL_ORD_NEGO = :SGTS0297-NR-SEQL-ORD-NEGO
                 AND A.TS_EST_ORD_NEGO  =
                        (SELECT MAX (B.TS_EST_ORD_NEGO)
                         FROM DB2SGT.EST_ORD_NEGO_OPR B
                         WHERE B.CD_CTRA_TIT_VL   = A.CD_CTRA_TIT_VL
                           AND B.CD_OPR_ETRD_MCP  = A.CD_OPR_ETRD_MCP
                           AND B.NR_VRS_OPR_ETRD  = A.NR_VRS_OPR_ETRD
                          AND B.NR_SEQL_ORD_NEGO = A.NR_SEQL_ORD_NEGO)

                 FETCH FIRST 1 ROW ONLY
           END-EXEC

      *--> Consistência do Operador Logado
           IF  CD-USU-ULT-ATL          =  KOPRD-CD-OPRD
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 1080               TO MSG1-CD-ERRO-TS
               MOVE 'Operador logado deve ser diferente do operador que
      -             'efetuou a edição da ordem de negociação.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Consistência da permissão para confirmação de Ordem de Negoc.
      *--> CD-TIP-EST-NEGO = 7 (Não Encerrado)
           IF  CD-TIP-EST-NEGO    NOT =  7
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 1079               TO MSG1-CD-ERRO-TS
               MOVE CD-TIP-EST-NEGO    TO AUX-CD-TIP-EST-NEGO

               STRING 'Estado da ordem não permite '
                       DELIMITED BY SIZE
                       TX-DTQ-MSG
                       DELIMITED BY SPACE
                      ' de cadastramento de ordem de venda. Estado atua'
                      'l = '
                       DELIMITED BY SIZE
                       AUX-CD-TIP-EST-NEGO
                       DELIMITED BY SIZE
                       INTO MSG1-TX-ERRO-TS
               END-STRING

               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-DEVOLUCAO-CONCLUSAO      SECTION.
      *  Inclusão de estado "Concluída"  para  Ordem de Negociação Ven-
      *  da informada (Estado 5).
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTS0297-CD-CTRA-TVM
                                       TO  SGTSI078-CD-CTRA-TIT-VL
           MOVE SGTS0297-CD-OPR-ETRD-MCP
                                       TO  SGTSI078-CD-OPR-ETRD-MCP
           MOVE SGTS0297-NR-VRS-OPR-ETRD
                                       TO  SGTSI078-NR-VRS-OPR-ETRD
           MOVE SGTS0297-NR-SEQL-ORD-NEGO
                                       TO  SGTSI078-NR-SEQL-ORD-NEGO
           MOVE SGTS0297-TX-JST-EST-ORD
                                       TO  SGTSI078-TX-JST-EST-ORD
           MOVE 'CURRENT'              TO  SGTSI078-TS-EST-ORD-NEGO
           MOVE KOPRD-CD-OPRD          TO  SGTSI078-CD-USU-ULT-ATL

      *--> SGTSI078-CD-TIP-EST-NEGO = 5 (Concluída)
           MOVE 5                      TO  SGTSI078-CD-TIP-EST-NEGO
           MOVE SGTSI078               TO  NM-PGM
           PERFORM 900000-PROCEDIMENTOS-CONTAINER
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       900000-PROCEDIMENTOS-CONTAINER  SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

      *--> Grava o container de requisição do módulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *--> Executa módulos com container
           PERFORM 910000-EXECUTA-MODULOS

      *--> Lê o container de erro do módulo executado
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *--> Lê o container de resposta do módulo executado
           PERFORM 973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       910000-EXECUTA-MODULOS          SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

      *--> Executar Módulos com container, sem Serviço
           EVALUATE NM-PGM

      *--> Executar Módulo SGTSI078
               WHEN SGTSI078
                    EXEC CICS LINK
                              PROGRAM ( SGTSI078   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

      *--> Executar Módulo SGTSL069
               WHEN SGTSL069
                    EXEC CICS LINK
                              PROGRAM ( SGTSL069   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

      *--> Execução de Módulo não prevista
               WHEN OTHER
                    SET ERRO-PROGRAMADO     TO TRUE
                    MOVE 9006               TO MSG1-CD-ERRO-TS
                    STRING 'Não definida a execução do módulo '
                            NM-PGM      '.'
                            DELIMITED BY SIZE
                            INTO          MSG1-TX-ERRO-TS
                    END-STRING
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    PERFORM 911000-VER-ERRO-MODULO-CHAMADO
           END-EVALUATE
           .
       910000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       911000-VER-ERRO-MODULO-CHAMADO  SECTION.
      *----------------------------------------------------------------*
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

      *--> Análise do retorno das chamadas dos programas
           SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
           MOVE  3004                  TO  MSG1-CD-ERRO-TS
           STRING  'Erro ao acionar o módulo ' NM-PGM      '.'
                   'RESP  = '    MSG1-RESP1  ', '
                   'RESP2 = '    MSG1-RESP2
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
           PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           .
       911000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    Section da Faixa de 970000 a 979900 para Leitura de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       970000-LER-REQUISICAO-RECEBIDA  SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao
           MOVE  'SGTS0297-RQSC'       TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM  973100-LER-CONTAINER-NODATA

      *--> Obtem o tamanho esperado da requisicao
           MOVE  LENGTH OF SGTS0297-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisicao X tamanho recebido
           IF  QT-CCT-LIDO                 NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA
                                       TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0297-RQSC)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       970000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *--> A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

      *--> Le o container de erro dos modulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE CTNR-PRM-ERRO          TO  NM-CTNR
           INITIALIZE                  IIBKERRO-PRM-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                     )
                      INTO      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      RESP      ( MSG1-RESP1                  )
                      RESP2     ( MSG1-RESP2                  )
           END-EXEC

      *==> Observação <=================================================
      *        Caso deseje personalizar ou não interromper o
      *        processamento, alterar para não dar erro aqui SOMENTE
      *        PARA O MÓDULO CHAMADO e tratar na section chamadora
      *=================================================================
           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL DFHRESP(CONTAINERERR)
                     CONTINUE
               WHEN  MSG1-RESP1            EQUAL DFHRESP(NORMAL)
                     SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                     MOVE  NM-PGM      TO  MSG1-NM-PGM-ACND
                     IF  NM-PGM(1:3)       EQUAL 'SGT'
                         MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                                           MSG1-CD-ERRO-TS
                         MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
                     ELSE
                         MOVE  3004    TO  MSG1-CD-ERRO-TS
                         STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                                 DELIMITED BY SIZE
                                 INTO      MSG1-TX-ERRO-TS
                         MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                         MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                     END-IF
                     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *-----------------------------------------------------------------
      *    Recupera o container com os dados de resposta (saída).
      *-----------------------------------------------------------------
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

      *--> Trata área de resposta do programa acionado (NODATA)
           PERFORM 973100-LER-CONTAINER-NODATA

      *--> Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *--> Por causa da captura automática erro CICS, deve ser previsto
      *--> o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Recupera o container de resposta do programa que foi acionado
           EVALUATE NM-PGM

      *--> Lê container de resposta do SGTSL069
               WHEN SGTSL069
                    MOVE LENGTH OF SGTSL069-RPST      TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSL069-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

      *--> Lê container de resposta do SGTSI078
               WHEN SGTSI078
                    MOVE LENGTH OF SGTSI078-RPST      TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSI078-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

      *--> Módulo não previsto para leitura do container
               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9003         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na leitura da resposta do módulo '
                             NM-PGM      '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                    END-STRING
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      *--> Recupera o tamanho previsto e o tamanho recebido da resposta
      *--> do programa que foi acionado.

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Obtem a quantidade de caracteres no container
           MOVE  ZEROS                 TO  QT-CCT-LIDO

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR     )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       974000-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           MOVE CTNR-PRM-INF-OPRD      TO NM-CTNR

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto erro de container
           SET  ERRO-CONTAINER         TO TRUE

           INITIALIZE PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       974000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *   Section da Faixa de 980000 a 989900 para Gravação de Dados
      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *-----------------------------------------------------------------
      *    Postar container com dados de requisição (entrada)
      *-----------------------------------------------------------------
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para gravação do container de requisição
           SET  PUT-CONTAINER          TO TRUE

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *--> nome_modulo -RQSC
           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RQSC'                TO NM-CTNR(9:5)

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Grava container da requisição do serviço IIB a ser acionado
      *--> Em caso de erro, a captura automática do CICS trata o erro
           EVALUATE NM-PGM

      *--> Grava container de requisição do programa SGTSL069
               WHEN SGTSL069
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSL069-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSL069-RQSC )
                    END-EXEC

      *--> Grava container de requisição do programa SGTSI078
               WHEN SGTSI078
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSI078-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSI078-RQSC )
                    END-EXEC

      *--> Módulo não previsto para gravar container de requisição
               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9004         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na gravação da requisição do módulo.'
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                    END-STRING
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       982000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *-----------------------------------------------------------------
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *--> nome_modulo -RPST

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

           SET   DEL-CONTAINER         TO  TRUE

      *--> Exclui o container de resposta
      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR    )
                      RESP      ( MSG1-RESP1 )
                      RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1   EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1   EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *--> Excluir o container de erro
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE                  IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE CTNR-PRM-ERRO          TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR    )
                      RESP      ( MSG1-RESP1 )
                      RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1   EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1   EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    As sections abaixo iniciadas com 999000 representam os
      *    tratamentos de erro através do Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  ERRO-PROGRAMA-ACIONADO
               PERFORM 999100-MONITORAR-VRV-PGM-CHMD
           END-IF

           PERFORM 999200-MONITORAR-VRV-PGM-E-ENC
           .
       999000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999100-MONITORAR-VRV-PGM-CHMD   SECTION.
      *-----------------------------------------------------------------
      *--> Monitora as variáveis dos módulos chamados

           EVALUATE  NM-PGM

      *-->>SGTSL069<<
               WHEN  SGTSL069
                     MOVE 'SGTSL069-CD-TIP-SRVC' TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL069-CD-TIP-SRVC  TO  MCR-S9-15
                     MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL069-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL069-CD-CTRA-TIT-VL
                                                 TO  MCR-S9-15
                     MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL069-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSL069-CD-OPR-ETRD-MCP    NUMERIC
                         MOVE  SGTSL069-CD-OPR-ETRD-MCP
                                                 TO MCR-S9-15
                         MOVE  MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF

                     MOVE 'SGTSL069-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL069-NR-VRS-OPR-ETRD
                                                 TO  MCR-S9-15
                     MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL069-TS-EST-VRS-PAG'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL069-TS-EST-VRS-PAG
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTSI078<<
               WHEN  SGTSI078
                     MOVE 'SGTSI078-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-CD-CTRA-TIT-VL
                                                 TO  MCR-S9-15
                     MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI078-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSI078-CD-OPR-ETRD-MCP    NUMERIC
                         MOVE  SGTSI078-CD-OPR-ETRD-MCP
                                                 TO MCR-S9-15
                         MOVE  MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI078-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-NR-VRS-OPR-ETRD
                                                 TO  MCR-S9-15
                     MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI078-NR-SEQL-ORD-NEGO'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-NR-SEQL-ORD-NEGO
                                                 TO  MCR-S9-15
                     MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI078-TS-EST-ORD-NEGO'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-TS-EST-ORD-NEGO
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI078-CD-USU-ULT-ATL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-CD-USU-ULT-ATL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI078-CD-TIP-EST-NEGO'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-CD-TIP-EST-NEGO
                                                 TO  MCR-S9-15
                     MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                     CONTINUE
           END-EVALUATE
           .
       999100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999200-MONITORAR-VRV-PGM-E-ENC  SECTION.
      *-----------------------------------------------------------------
      *--> Monitora variàveis do pgm, grava o erro e encerra

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Em caso de erro, desfaz as atualizações em banco de dados
           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC

      *--> Verifica se o tipo de erro gerado possui tratamento interno
           EVALUATE  TRUE
               WHEN  ERRO-CONTAINER
                     PERFORM  999004-TRATA-ERRO-CTNR
               WHEN  ERRO-TAMANHO-AREA
                     PERFORM  999005-TRATA-ERRO-AREA-DADOS
               WHEN  ERRO-CANAL
                     PERFORM  999006-TRATA-ERRO-CANAL
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE 'SGTS0297-CD-CTRA-TVM'           TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0297-CD-CTRA-TVM            TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0297-CD-OPR-ETRD-MCP'     TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0297-CD-OPR-ETRD-MCP    NUMERIC
               MOVE  SGTS0297-CD-OPR-ETRD-MCP  TO  MCR-S9-15
               MOVE  MCR-S9-15                 TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'      TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0297-NR-VRS-OPR-ETRD'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0297-NR-VRS-OPR-ETRD        TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0297-NR-SEQL-ORD-NEGO'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0297-NR-SEQL-ORD-NEGO       TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0297-TX-JST-EST-ORD'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0297-TX-JST-EST-ORD         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'TX-DTQ-MSG'                     TO  MSGKW999-NM-VRV-MNT
           MOVE  TX-DTQ-MSG                      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'AUX-CD-TIP-EST-NEGO'            TO  MSGKW999-NM-VRV-MNT
           MOVE  AUX-CD-TIP-EST-NEGO             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS               TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS               TO  IIBKERRO-TX-ERRO

           EXEC CICS PUT
              CONTAINER ( CTNR-PRM-ERRO               )
              CHANNEL   ( NM-CNL                      )
              FROM      ( IIBKERRO-PRM-ERRO           )
              FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
              NOHANDLE
           END-EXEC

      *--> Encerra a transacao
           PERFORM  999999-ENCERRAR
           .
       999200-SAI.
           EXIT.

      *---------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      *----------------------------------------
-INC HLPKP999

      *----------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
-INC SGTKP999

      *----------------------------------------
      * 999010-TRATA-ERRO-DB2          SECTION.
      *----------------------------------------
-INC HLPKP997

      *----------------------------------------
      * MSG1-NTF-MSG                   SECTION.
      * MSG1-PCE-INC                   SECTION.
      * MSG1-PUT-CTNR-PRM              SECTION.
      * MSG1-PUT-CNTRS-OPC             SECTION.
      * MSG1-PUT-CTNR-PGM-ACND         SECTION.
      * MSG1-PUT-CTNR-OPR-ACND         SECTION.
      * MSG1-PUT-CTNR-SQL              SECTION.
      * MSG1-PUT-CTNR-CICS             SECTION.
      * MSG1-PUT-CTNR-VRV              SECTION.
      * MSG1-EXEC-MSG1                 SECTION.
      * MSG1-GET-CTNR-ERRO             SECTION.
      *----------------------------------------
-INC MSGKPMG1

      *-----------------------------------------------------------------
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTS0297 -----------------------------