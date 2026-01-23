      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0183
      * ANALISTA..: F2054109 CINTIA IGLESIAS
      * AUTOR.....: F2054109 CINTIA IGLESIAS
      * DATA......: 31/08/2017
      * OBJETIVO..: Consulta Lista de Precificação da Operação
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 31/01/2019-  F0950413 Inclusão da sigla 'SGT' na section
      *                     972000-LER-CONTAINER-DE-ERRO
      *        31/08/2017 - F2054109 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS0183.
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
                    '*** SGTS0183 ***'.
       77  CTE-VERS                      PIC  X(006)  VALUE 'VRS001'.

      * Modulos acionados
       77  SGTSL068                      PIC  X(008) VALUE 'SGTSL068'.
       77  SGTS0136                      PIC  X(008) VALUE 'SGTS0136'.
      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*

       01  CD-OPR-ETRD-MCP-DISPLAY       PIC  9(011) VALUE 0.
       01  NRO-DEC-AUX                   PIC  9(004)V9(010) VALUE 0.
       01  NRO-DEC-AUX-R                 REDEFINES NRO-DEC-AUX.
           03  NRO-DEC-ALFA              PIC  9(014).

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      * Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999
      * Book para captura de erro via MSG1
-INC MSGKWMG1

      * Book do container de requisicao e resposta deste modulo
       01 SGTS0183-DADOS.
-INC SGTK0183
      * Consulta da tab PREF_TAXA_OPR
-INC SGTKL068
      * Formatar descrição da taxa
       01 SGTS0136-DADOS.
-INC SGTK0136
      * Book do Container de Erro
-INC IIBKERRO
      * Book do container de Servicos (para acionamento de servicos)
-INC IIBKSRVC

       01  CTE-FINAL-SS                  PIC X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIAR

           PERFORM 200000-PROCESSAR

           PERFORM 990000-ENCERRAR

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
           MOVE 'SGTS0183'             TO NM-PGM

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

      *    Valida campo a campo da requisicao

           IF  SGTS0183-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0002                TO MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'  TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0183-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0003                TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -        ' numérico.'             TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0183-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0005                TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -        ' informado.'            TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0183-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0007                TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       110000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           PERFORM 210000-LISTAR-PRECIFICACOES

           PERFORM 220000-TRATAR-LISTA-PREF
                   VARYING SGTSL068-IC-TAB FROM 1 BY 1
                   UNTIL SGTSL068-IC-TAB > SGTSL068-QT-OCR-TAB

           MOVE SGTSL068-QT-OCR-TAB    TO SGTS0183-QT-OCR-TAB
           MOVE SGTSL068-IN-CNTO-PSQ   TO SGTS0183-IN-CNTO-PSQ

           PERFORM 980000-GRAVAR-CONTAINER-RPST

           .
       200000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       210000-LISTAR-PRECIFICACOES       SECTION.
      *----------------------------------------------------------------*

           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSL068               TO NM-PGM

           MOVE SGTS0183-CD-CTRA-TIT-VL
                                 TO SGTSL068-CD-CTRA-TIT-VL
           MOVE SGTS0183-CD-OPR-ETRD-MCP
                                 TO SGTSL068-CD-OPR-ETRD-MCP

           MOVE SGTS0183-NR-VRS-OPR-ETRD
                                 TO SGTSL068-NR-VRS-OPR-ETRD
           MOVE SGTS0183-CD-TIP-TAXA
                                 TO SGTSL068-CD-TIP-TAXA
           MOVE SGTS0183-CD-ULT-TIP-TAXA
                                 TO SGTSL068-CD-ULT-TIP-TAXA
           MOVE SGTS0183-DT-ULT-INC-VLD
                                 TO SGTSL068-DT-ULT-INC-VLD

           PERFORM 900000-EXECUTAR-MODULO

           .
       210000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       220000-TRATAR-LISTA-PREF       SECTION.
      *----------------------------------------------------------------*

           MOVE SGTSL068-CD-TIP-TAXA-S(SGTSL068-IC-TAB)    TO
                                 SGTS0183-CD-TIP-TAXA-S(SGTSL068-IC-TAB)
           MOVE SGTSL068-DT-FIM-CND-TAXA-R(SGTSL068-IC-TAB)      TO
                             SGTS0183-DT-FIM-CND-TAXA-R(SGTSL068-IC-TAB)
           MOVE SGTSL068-DT-INC-CND-TAXA-R(SGTSL068-IC-TAB)      TO
                             SGTS0183-DT-INC-CND-TAXA-R(SGTSL068-IC-TAB)
           MOVE SGTSL068-PC-MIN-TAXA(SGTSL068-IC-TAB)  TO
                                   SGTS0183-PC-MIN-TAXA(SGTSL068-IC-TAB)
           MOVE SGTSL068-CD-TIP-APL-PREF(SGTSL068-IC-TAB) TO
                               SGTS0183-CD-TIP-APL-PREF(SGTSL068-IC-TAB)
           MOVE SGTSL068-CD-PERC-APL-PREF(SGTSL068-IC-TAB) TO
                              SGTS0183-CD-PERC-APL-PREF(SGTSL068-IC-TAB)
           MOVE SGTSL068-CD-IEC-PREF(SGTSL068-IC-TAB) TO
                                   SGTS0183-CD-IEC-PREF(SGTSL068-IC-TAB)

           IF  SGTSL068-CD-TIP-TAXA-S(SGTSL068-IC-TAB) = 1
               PERFORM 221000-FORMATAR-DESCR-TAXA
               MOVE  SGTS0136-TX-DCR-RMNC-OPR  TO
                               SGTS0183-TX-DCR-TAXA-OPR(SGTSL068-IC-TAB)
           END-IF

           .
       220000-SAI.
           EXIT.


      *----------------------------------------------------------------*
       221000-FORMATAR-DESCR-TAXA       SECTION.
      *----------------------------------------------------------------*

           MOVE SGTS0136               TO NM-PGM

           INITIALIZE   SGTS0136-DADOS

           MOVE SGTSL068-PC-MIN-TAXA(SGTSL068-IC-TAB)  TO
                                   SGTS0136-PC-TAXA-RMNC-OPR
           MOVE SGTSL068-CD-TIP-APL-PREF(SGTSL068-IC-TAB) TO
                               SGTS0136-CD-TIP-FMA-APL
           MOVE SGTSL068-CD-IEC-PREF(SGTSL068-IC-TAB) TO
                                   SGTS0136-CD-IEC-RMNC

           PERFORM 900000-EXECUTAR-MODULO

           .
       221000-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *----------------------------------------------------------------*
      * Executa um modulo implementado com CONTAINER                   *
      * Nao incluir nessa section os modulos chamados via COMMAREA     *
      *                                                                *
      *----------------------------------------------------------------*
       900000-EXECUTAR-MODULO          SECTION.
      *----------------------------------------------------------------*

      *    Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *    Nao ha captura automatica de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM
               WHEN SGTSL068
                   EXEC CICS LINK
                             PROGRAM ( SGTSL068  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTS0136
                   EXEC CICS LINK
                             PROGRAM ( SGTS0136  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN OTHER
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 9006             TO MSG1-CD-ERRO-TS
                 MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    MOVE 3004          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro ao acionar o módulo.'
                                       TO MSG1-TX-ERRO-TS
                    SET ERRO-CICS      TO TRUE
      *        Monitorar variáveis do módulo chamado                     VRS001
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
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variaveis dos modulos chamados
      *----------------------------------------------------------------*

           EVALUATE NM-PGM
           WHEN SGTSL068
                MOVE 'SGTSL068-CD-CTRA-TIT-VL'
                                   TO MSGKW999-NM-VRV-MNT
                MOVE SGTSL068-CD-CTRA-TIT-VL
                                   TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT
                MOVE 'SGTSL068-CD-OPR-ETRD-MCP'
                                   TO MSGKW999-NM-VRV-MNT

                IF  SGTSL068-CD-OPR-ETRD-MCP IS NUMERIC
                    MOVE SGTSL068-CD-OPR-ETRD-MCP
                                   TO CD-OPR-ETRD-MCP-DISPLAY
                    MOVE CD-OPR-ETRD-MCP-DISPLAY
                                   TO MSGKW999-VL-VRV-MNT
                ELSE
                    MOVE 'Código de Operação Estruturada deve ser número
      -                  '.'       TO MSGKW999-VL-VRV-MNT
                END-IF
                PERFORM 999003-ADICIONA-VRV-MNT
                MOVE 'SGTSL068-NR-VRS-OPR-ETRD'
                                   TO MSGKW999-NM-VRV-MNT
                MOVE SGTSL068-NR-VRS-OPR-ETRD
                                   TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT
                MOVE 'SGTSL068-CD-TIP-TAXA'
                                   TO MSGKW999-NM-VRV-MNT
                MOVE SGTSL068-CD-TIP-TAXA
                                   TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSL068-CD-ULT-TIP-TAXA'
                                   TO MSGKW999-NM-VRV-MNT
                MOVE SGTSL068-CD-ULT-TIP-TAXA
                                   TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSL068-DT-ULT-INC-VLD'
                                   TO MSGKW999-NM-VRV-MNT
                MOVE SGTSL068-DT-ULT-INC-VLD
                                   TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

           WHEN SGTS0136
                MOVE 'SGTS0136-PC-TAXA-RMNC-OPR'
                                   TO MSGKW999-NM-VRV-MNT
                IF  SGTS0136-PC-TAXA-RMNC-OPR IS NUMERIC
                    MOVE SGTS0136-PC-TAXA-RMNC-OPR
                                   TO NRO-DEC-AUX
                    MOVE NRO-DEC-ALFA  TO MSGKW999-VL-VRV-MNT
                ELSE
                    MOVE 'Percentual da Taxa de Remuneração deve ser núm
      -                  'ero.'        TO MSGKW999-NM-VRV-MNT
                END-IF

                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTS0136-CD-TIP-FMA-APL'
                                   TO MSGKW999-NM-VRV-MNT
                MOVE SGTS0136-CD-TIP-FMA-APL
                                   TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT
                MOVE 'SGTS0136-CD-IEC-RMNC'
                                   TO MSGKW999-NM-VRV-MNT
                MOVE SGTS0136-CD-IEC-RMNC
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
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       970000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       971000-LER-REQUISICAO           SECTION.
      *----------------------------------------------------------------*

           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisicao
           MOVE 'SGTS0183'             TO NM-PGM
           MOVE 'SGTS0183-RQSC'        TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      *    Obtem o tamanho esperado da requisicao
           MOVE LENGTH OF SGTS0183-RQSC TO QT-CCT-EPRO

      *    Valida tamanho esperado da requisicao X tamanho recebido

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA   TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisicao
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0183-RQSC)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE

           .
       971000-SAI.
           EXIT.

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

      *    Nao ha captura automatica de erro CICS por usar RESP e RESP2
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
      *--> Trata o erro de programa acionado por ser módulo interno
                SET ERRO-PROGRAMA-ACIONADO   TO  TRUE
                MOVE NM-PGM                  TO  MSG1-NM-PGM-ACND
                IF NM-PGM(1:3) EQUAL 'SGT'
                    MOVE IIBKERRO-CD-ERRO    TO  MSG1-CD-RTN-PGM-ACND
                                                 MSG1-CD-ERRO-TS
                    MOVE IIBKERRO-TX-ERRO    TO  MSG1-TX-RTN-PGM-ACND
                                                 MSG1-TX-ERRO-TS
                ELSE
                    MOVE  3004               TO  MSG1-CD-ERRO-TS
                    STRING  'Erro ao acionar o módulo ' NM-PGM      '.'
                            DELIMITED BY SIZE
                            INTO        MSG1-TX-ERRO-TS
                    END-STRING
                    MOVE IIBKERRO-CD-ERRO     TO  MSG1-CD-RTN-PGM-ACND
                    MOVE IIBKERRO-TX-ERRO     TO  MSG1-TX-RTN-PGM-ACND
                END-IF
      *-->      Monitorar variáveis do módulo chamado
                PERFORM 969000-MONITORAR-VARIAVEIS
                PERFORM 998900-ERRO

               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       973000-LER-RESPOSTA             SECTION.
      *----------------------------------------------------------------*

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR

           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RPST'       TO NM-CTNR(9:5)

      *    Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

      *Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE


      *    Le a RESPOSTA de acordo com o programa que a gravou

           EVALUATE NM-PGM

      *        Calcular a quantidade de caracteres esperado e limpar
      *        a area de dados antes de executar o GET
      *        Para matrizes com dados variaveis (clausula depending
      *        on) nao ha como utilizar o comando INITIALIZE

               WHEN SGTSL068
                    MOVE LENGTH OF SGTSL068-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL068-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTS0136
                    MOVE LENGTH OF SGTS0136-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTS0136-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN OTHER
                     SET ERRO-PROGRAMADO
                                       TO TRUE
                     MOVE 9006         TO MSG1-CD-ERRO-TS
                     MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE

           .
       973000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       973100-LER-CONTAINER-NODATA     SECTION.
      *----------------------------------------------------------------*

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Obtem a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973100-SAI.
           EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
      *                                                                *
      *----------------------------------------------------------------*
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *----------------------------------------------------------------*

      *    Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de resposta
           MOVE 'SGTS0183-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                )
                     CHANNEL   (NM-CNL                 )
                     FROM      (SGTS0183-RPST          )
                     FLENGTH   (LENGTH OF SGTS0183-RPST)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *----------------------------------------------------------------*

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RQSC'       TO NM-CTNR(9:5)


      *Por causa da captura automatica de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisicao do programa a executar
           EVALUATE NM-PGM
               WHEN SGTSL068
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSL068-RQSC )
                              FLENGTH   (LENGTH OF SGTSL068-RQSC )
                   END-EXEC
               WHEN SGTS0136
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTS0136-RQSC )
                              FLENGTH   (LENGTH OF SGTS0136-RQSC )
                   END-EXEC
               WHEN OTHER
                     SET ERRO-PROGRAMADO
                                       TO TRUE
                     MOVE 9006         TO MSG1-CD-ERRO-TS
                     MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *Ao chegar aqui, ha certeza de que nao houve erro algum
           SET  SEM-ERRO               TO TRUE

           .
       982000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*

           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RPST'       TO NM-CTNR(9:5)

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
           .
       983100-SAI.
           EXIT.
      *----------------------------------------------------------------*
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *----------------------------------------------------------------*

      *    Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS PUT
                     CONTAINER ( 'PRM-ERRO'                  )
                     FROM      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      (MSG1-RESP1                   )
                     RESP2     (MSG1-RESP2                   )
           END-EXEC

           .
       984000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       990000-ENCERRAR                 SECTION.
      *----------------------------------------------------------------*

           GOBACK
           .
       990000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
      *                                                                *
      * As sections abaixo iniciadas com 999000 representam os tratamen*
      * tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *
      *                                                                *
      *----------------------------------------------------------------*
       998900-ERRO                     SECTION.
      *----------------------------------------------------------------*

           IF MSG1-CD-ERRO-TS EQUAL ZEROS
              MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF

           IF MSG1-TX-ERRO-TS EQUAL SPACES
              MOVE 'Erro não específicado no módulo' TO MSG1-TX-ERRO-TS
           END-IF

           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       998900-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *     As sections abaixo representam os tratamentos de erro
      *    utilizando o sistema MSG de mensageria (rotina MSGS0001)
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *    Em caso de erro, desfaz as atualizações em banco de dados

           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC

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
           MOVE 'SGTS0183-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0183-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTS0183-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
           IF  SGTS0183-CD-OPR-ETRD-MCP IS NUMERIC
               MOVE  SGTS0183-CD-OPR-ETRD-MCP
                                      TO  CD-OPR-ETRD-MCP-DISPLAY
               MOVE  CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Código de Operação Estruturada deve ser número
      -                  '.'           TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0183-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0183-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTS0183-CD-TIP-TAXA'
                                       TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0183-CD-TIP-TAXA
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTS0183-CD-ULT-TIP-TAXA'
                                       TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0183-CD-ULT-TIP-TAXA
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTS0183-DT-ULT-INC-VLD'
                                       TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0183-DT-ULT-INC-VLD
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT


      *    Grava o erro gerado no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG

           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
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
      *---------------------- FIM SGTS0183 -----------------------------