       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0295
      * ANALISTA..: F2804421 - Eliana Cristina Ramos da Silva
      * AUTOR.....: F2804421 - Eliana Cristina Ramos da Silva
      * DATA......: 04/10/2019
      * OBJETIVO..: Negócio  - Efetua exclusão física da Ordem de Venda.
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 28/10/2019 F2804421  DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
        IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0295 .
      *
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
      *
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------

       77  CTE-INICIO-SS               PIC  X(40)  VALUE
                                            '*** S.S. COMEÇA AQUI ***'.
       77  CTE-PROG                    PIC  X(18)  VALUE
                                            '*** SGTS0295 ***'.
       77  CTE-VERS                    PIC  X(06)  VALUE 'VRS001'.

      * Módulos acionados
       77  SGTSE075                    PIC  X(08) VALUE 'SGTSE075'.
       77  SGTSL078                    PIC  X(08) VALUE 'SGTSL078'.
       77  SGTSE078                    PIC  X(08) VALUE 'SGTSE078'.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      * Areas de trabalho
       01 AX-ESTMAIOR1                 PIC  X(01) VALUE 'N'.

      *-----------------------------------------------------------------
      * Books dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------
      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      * Book para captura de abend cics do bloco código HLPKW999
-INC HLPKW999

      * Book para captura de erro via MSG1
-INC MSGKWMG1

      * Book do container de requisição e resposta deste módulo
       01  SGTS0295-DADOS.
-INC SGTK0295

      * Exclusão Ordem de Venda - tabela ORD_NEGO_OPR
       01  SGTSE075-DADOS.
-INC SGTKE075

      * Exclusão Estado da Ordem de Venda - tabela EST_ORD_NEGO_OPR
       01  SGTSE078-DADOS.
-INC SGTKE078

      * Listar estados da ordem de venda tabela - EST_ORD_NEGO_OPR
       01  SGTSL078-DADOS.
-INC SGTKL078

      * Book do Container de Erro
-INC IIBKERRO

       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

           PERFORM 100000-INICIAR

           PERFORM 200000-PROCESSAR

           PERFORM 999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE 001                    TO MSG1-CD-SEQL-ERRO

      *    Ativa a captura automática de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM 970000-LER-CANAL

      *    Define o nome do programa p/ fins manipulação container
           MOVE CTE-PROG(5:8)          TO NM-PGM

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

           PERFORM 110000-VALIDAR-REQUISICAO

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 002                    TO MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada
           IF  SGTS0295-CD-CTRA-TVM    LESS ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0295-CD-CTRA-TVM    EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0295-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0295-CD-OPR-ETRD-MCP    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0295-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'    TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0295-NR-VRS-OPR-ETRD    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número versão operação estruturada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0295-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0295-NR-SEQL-ORD-NEGO   EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  542               TO  MSG1-CD-ERRO-TS
               MOVE  'Número sequencial da ordem de negociação não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
      *
      *-- Carrega chave de chamada da lista de estados da ordem venda.
           PERFORM 205000-CARREGA-CHV-LISTA

           MOVE 003                    TO MSG1-CD-SEQL-ERRO
      *-- Verifica se existe estado da ordem diferente de 1 -Em edição

           PERFORM UNTIL SGTSL078-IN-CNTO-PSQ  EQUAL 'N'
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM VARYING SGTSL078-IC-TAB FROM 1 BY 1
                     UNTIL SGTSL078-IC-TAB > SGTSL078-QT-OCR-TAB

                 IF SGTSL078-CD-TIP-EST-NEGO (SGTSL078-IC-TAB) > 1
                    MOVE 'S'                 TO  AX-ESTMAIOR1
                    MOVE SGTSL078-QT-OCR-TAB TO  SGTSL078-IC-TAB
                 END-IF

             END-PERFORM
      *-- Rechamada da lista
              IF SGTSL078-IN-CNTO-PSQ  EQUAL 'S'
                 MOVE SGTSL078-TS-EST-ORD-NEGO (SGTSL078-QT-OCR-TAB)
                                      TO SGTSL078-TS-EST-ULT
              END-IF
           END-PERFORM

      *-- Verifica se foi encontrado algum estado da ordem (deve existir
      *-- pelo menos 1 estado.
           IF  SGTSL078-QT-OCR-TAB  = ZEROS
               SET   ERRO-PROGRAMADO    TO  TRUE
               MOVE  1095               TO  MSG1-CD-ERRO-TS
               MOVE  'Não existe(m) estado(s) para esta ordem de negocia
      -              'ção.'             TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *-- Se não foi localizado nenhum estado diferente de 1 -Em edição
      *-- efetua a exclusão dos estados da ordem e da ordem de venda
           IF  AX-ESTMAIOR1             EQUAL 'S'
               SET   ERRO-PROGRAMADO    TO  TRUE
               MOVE  1094               TO  MSG1-CD-ERRO-TS
               MOVE  'Estado da ordem de negociação não permite efetuar
      -              'a exclusão do cadastro.'
                                        TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           ELSE
               PERFORM 210000-EXCLUIR-EST-ORD-NEGO

               PERFORM 220000-EXCLUIR-ORD-NEGO

           END-IF

           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       205000-CARREGA-CHV-LISTA        SECTION.
      *-----------------------------------------------------------------
           MOVE 004                    TO MSG1-CD-SEQL-ERRO

      *-- Carrega chave de chamada lista estados da ordem de negociação.
           MOVE 2                      TO  SGTSL078-CD-TIP-SRVC
           MOVE SPACES                 TO  SGTSL078-IN-CNTO-PSQ
           MOVE ZEROS                  TO  SGTSL078-QT-OCR-TAB

           MOVE SGTS0295-CD-CTRA-TVM
                                       TO  SGTSL078-CD-CTRA-TIT-VL
           MOVE SGTS0295-CD-OPR-ETRD-MCP
                                       TO  SGTSL078-CD-OPR-ETRD-MCP
           MOVE SGTS0295-NR-VRS-OPR-ETRD
                                       TO  SGTSL078-NR-VRS-OPR-ETRD
           MOVE SGTS0295-NR-SEQL-ORD-NEGO
                                       TO  SGTSL078-NR-SEQL-ORD-ULT
           MOVE SPACES                 TO  SGTSL078-TS-EST-ULT

           MOVE SGTSL078               TO NM-PGM

           .
       205000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       210000-EXCLUIR-EST-ORD-NEGO     SECTION.
      *-----------------------------------------------------------------
           MOVE 005                    TO MSG1-CD-SEQL-ERRO

      *-- Carrega chave de chamada da lista de estados da ordem venda.
      *-- Excluir todos estados da lista estados da ordem de negociação.

           PERFORM 205000-CARREGA-CHV-LISTA

           PERFORM UNTIL SGTSL078-IN-CNTO-PSQ  EQUAL 'N'
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 215000-EXCLUIR-ESTADOS
                     VARYING SGTSL078-IC-TAB FROM 1 BY 1
                     UNTIL SGTSL078-IC-TAB > SGTSL078-QT-OCR-TAB
      *-- Rechamada da lista
              IF SGTSL078-IN-CNTO-PSQ  EQUAL 'S'
                  MOVE SGTSL078-TS-EST-ORD-NEGO (SGTSL078-QT-OCR-TAB)
                                      TO SGTSL078-TS-EST-ULT
              END-IF
           END-PERFORM

           .
       210000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       215000-EXCLUIR-ESTADOS          SECTION.
      *----------------------------------------------------------------*
           MOVE 006                    TO MSG1-CD-SEQL-ERRO

      *-- Exclusão EST_ORD_NEGO_OPR
           MOVE SGTSE078               TO NM-PGM

           MOVE SGTSL078-CD-CTRA-TIT-VL
                                       TO SGTSE078-CD-CTRA-TIT-VL
           MOVE SGTSL078-CD-OPR-ETRD-MCP
                                       TO SGTSE078-CD-OPR-ETRD-MCP
           MOVE SGTSL078-NR-VRS-OPR-ETRD
                                       TO SGTSE078-NR-VRS-OPR-ETRD
           MOVE SGTSL078-NR-SEQL-ORD-ULT
                                       TO SGTSE078-NR-SEQL-ORD-NEGO
           MOVE SGTSL078-TS-EST-ORD-NEGO (SGTSL078-IC-TAB)
                                       TO SGTSE078-TS-EST-ORD-NEGO
           PERFORM 900000-EXECUTAR-MODULO

           .
       215000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       220000-EXCLUIR-ORD-NEGO     SECTION.
      *-----------------------------------------------------------------
           MOVE 007                    TO MSG1-CD-SEQL-ERRO

      *-- Exclusão ORD_NEGO_OPR
           MOVE SGTSE075               TO NM-PGM

           MOVE SGTS0295-CD-CTRA-TVM
                                       TO SGTSE075-CD-CTRA-TIT-VL
           MOVE SGTS0295-CD-OPR-ETRD-MCP
                                       TO SGTSE075-CD-OPR-ETRD-MCP
           MOVE SGTS0295-NR-VRS-OPR-ETRD
                                       TO SGTSE075-NR-VRS-OPR-ETRD
           MOVE SGTS0295-NR-SEQL-ORD-NEGO
                                       TO SGTSE075-NR-SEQL-ORD-NEGO

           PERFORM 900000-EXECUTAR-MODULO

           .
       220000-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *-----------------------------------------------------------------
      * Executa um módulo implementado com CONTAINER                   *
      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *-----------------------------------------------------------------

      *    Grava o container de requisição do módulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM

               WHEN SGTSE075
                    EXEC CICS LINK
                              PROGRAM ( SGTSE075   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTSE078
                    EXEC CICS LINK
                              PROGRAM ( SGTSE078   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTSL078
                    EXEC CICS LINK
                              PROGRAM ( SGTSL078   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN OTHER
                    MOVE 9006          TO MSG1-CD-ERRO-TS
                    MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
      *
           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    MOVE 3004          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro ao acionar o módulo.'
                                       TO MSG1-TX-ERRO-TS
                    SET ERRO-CICS      TO TRUE
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *    Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *-----------------------------------------------------------------
      * Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------
      * Variáveis exclusivas de cada programa acionado
           EVALUATE NM-PGM
             WHEN SGTSE075
               MOVE 'SGTSE075-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSE075-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSE075-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
               MOVE SGTSE075-CD-OPR-ETRD-MCP   TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSE075-NR-VRS-OPR-ETRD' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSE075-NR-VRS-OPR-ETRD  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSE075-NR-SEQL-ORD-NEGO' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSE075-NR-SEQL-ORD-NEGO  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

             WHEN SGTSE078
               MOVE 'SGTSE078-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSE078-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSE078-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
               MOVE SGTSE078-CD-OPR-ETRD-MCP   TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSE078-NR-VRS-OPR-ETRD' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSE078-NR-VRS-OPR-ETRD  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSE078-NR-SEQL-ORD-NEGO' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSE078-NR-SEQL-ORD-NEGO  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSE078-TS-EST-ORD-NEGO' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSE078-TS-EST-ORD-NEGO  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

             WHEN SGTSL078
               MOVE 'SGTSL078-CD-TIP-SRVC' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSL078-CD-TIP-SRVC  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSL078-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSL078-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSL078-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
               MOVE SGTSL078-CD-OPR-ETRD-MCP   TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSL078-NR-VRS-OPR-ETRD' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSL078-NR-VRS-OPR-ETRD  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSL078-NR-SEQL-ORD-ULT' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSL078-NR-SEQL-ORD-ULT  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSL078-TS-EST-ULT'      TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSL078-TS-EST-ULT       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

              WHEN OTHER
                  CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *                                                                *
      * SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
      *                                                                *
      *-----------------------------------------------------------------
       970000-LER-CANAL                SECTION.
      *    Obtem o canal corrente
      *-----------------------------------------------------------------
           MOVE 008                    TO MSG1-CD-SEQL-ERRO

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

      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------
           MOVE 009                    TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisição
           MOVE CTE-PROG(5:8)          TO NM-PGM
           MOVE 'SGTS0295-RQSC'        TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      *--> Obtem o tamanho esperado da requisição
           MOVE  LENGTH OF  SGTS0295-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisição X tamanho recebido
           IF  QT-CCT-LIDO             NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE 010                    TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER ( NM-CTNR       )
                     FLENGTH   ( QT-CCT-LIDO   )
                     INTO      (  SGTS0295-RQSC )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *-----------------------------------------------------------------
      *    A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------

      *    Lê o container de erro dos módulos chamados
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
      *        PARA O módulo CHAMADO e tratar na section chamadora
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    SET ERRO-PROGRAMA-ACIONADO TO TRUE
                    MOVE  NM-PGM       TO  MSG1-NM-PGM-ACND

                 IF NM-PGM(1:3) EQUAL 'SGT'
                    EVALUATE NM-PGM
                        WHEN 'SGTSE075'
                           IF IIBKERRO-CD-ERRO EQUAL 100
                              MOVE 0650        TO  MSG1-CD-RTN-PGM-ACND
                                                   MSG1-CD-ERRO-TS
                              MOVE 'Ordem de negociação da operação inex
      -                            'istente.'  TO  MSG1-TX-RTN-PGM-ACND
                                                   MSG1-TX-ERRO-TS
                           END-IF
                        WHEN 'SGTSE78'
                           IF IIBKERRO-CD-ERRO EQUAL 100
                              MOVE 886       TO  MSG1-CD-RTN-PGM-ACND
                                                 MSG1-CD-ERRO-TS
                              MOVE 'Estado do tipo de negociação inexist
      -                            'ente.'   TO  MSG1-TX-RTN-PGM-ACND
                                                 MSG1-TX-ERRO-TS
                           END-IF
                        WHEN OTHER
                           MOVE IIBKERRO-TX-ERRO TO MSG1-TX-RTN-PGM-ACND
                                                    MSG1-TX-ERRO-TS
                           MOVE IIBKERRO-CD-ERRO TO MSG1-CD-RTN-PGM-ACND
                                                    MSG1-CD-ERRO-TS
                    END-EVALUATE
                 ELSE
                    MOVE 3004          TO MSG1-CD-ERRO-TS
                    STRING  'Erro ao acionar o módulo ' NM-PGM
                            '.'
                    DELIMITED BY SIZE
                    INTO           MSG1-TX-ERRO-TS

                    MOVE IIBKERRO-TX-ERRO TO MSG1-TX-RTN-PGM-ACND
                    MOVE IIBKERRO-CD-ERRO TO MSG1-CD-RTN-PGM-ACND
                 END-IF

      *         Monitorar variáveis do módulo chamado
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *-----------------------------------------------------------------
      *    Recupera o container com os dados de resposta (saída).
      *-----------------------------------------------------------------

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

      *    Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

      *Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Lê a RESPOSTA de acordo com o programa que a gravou


           EVALUATE NM-PGM

      *    Calcula a quantidade de caracteres esperado e limpa a área de
      *    dados antes de executar o GET (somentepara matrizes com dados
      *    não variáveis)

               WHEN SGTSE075
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSE075-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE078
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSE078-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL078
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSL078-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN OTHER
                    MOVE 9003          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro na leitura de resposta do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Obtém a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER ( NM-CTNR     )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973100-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
      *                                                                *
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *-----------------------------------------------------------------
      *    Postar container com dados de requisição (entrada)
      *-----------------------------------------------------------------

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_modulo -RQSC

           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RQSC'       TO NM-CTNR(9:5)


      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisição do programa a executar
           EVALUATE NM-PGM
               WHEN SGTSE075
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSE075-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSE075-RQSC )
                    END-EXEC

               WHEN SGTSE078
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSE078-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSE078-RQSC )
                    END-EXEC

               WHEN SGTSL078
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSL078-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSL078-RQSC )
                    END-EXEC

               WHEN OTHER
                    MOVE 9004          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro na gravação da requisição do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       982000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *-----------------------------------------------------------------
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *-----------------------------------------------------------------


      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RPST'       TO NM-CTNR(9:5)


           SET DEL-CONTAINER           TO TRUE

      *    Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR    )
                     RESP      ( MSG1-RESP1 )
                     RESP2     ( MSG1-RESP2 )
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

      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------

      *    Limpar variáveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE 011                    TO MSG1-CD-SEQL-ERRO
           MOVE 'PRM-ERRO'             TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR    )
                     RESP      ( MSG1-RESP1 )
                     RESP2     ( MSG1-RESP2 )
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

      *-----------------------------------------------------------------
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *-----------------------------------------------------------------
           MOVE 012                    TO MSG1-CD-SEQL-ERRO

      *    Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           MOVE 'PRM-ERRO'             TO NM-CTNR
           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                     )
                     FROM      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC
           .
       984000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    As sections abaixo iniciadas com 999000 representam os
      *    tratamentos de erro utilizando o Sistema MSG
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
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

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE 'SGTS0295-CD-CTRA-TVM'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE   SGTS0295-CD-CTRA-TVM
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE ' SGTS0295-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
           IF   SGTS0295-CD-OPR-ETRD-MCP    NUMERIC
               MOVE   SGTS0295-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE ' SGTS0295-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE   SGTS0295-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *--> Grava a Ocorrência de Erro no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava a Ocorrência de Erro no Container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

      *--> Encerra a transacao
           PERFORM 999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      *----------------------------------------
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
      *---------------------- Fim  SGTS0295 ---------------------------*