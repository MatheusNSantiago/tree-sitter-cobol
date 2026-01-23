      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0092
      * ANALISTA..: F9507141 - Vicente de Paulo Tavares de Souza
      * AUTOR.....: F8658869 - Roberto Ioshimoto
      * DATA......: 06/01/2017
      * OBJETIVO..: Consultar tarifas.
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 06/01/2017 - F8658869 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *--------------------------------
       PROGRAM-ID. SGTS0092.
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
                    '*** SGTS0092 ***'.
       77  CTE-VERS                      PIC  X(006)  VALUE 'VRS001'.

      * Módulos acionados
       77  SGTSC063                      PIC  X(008) VALUE 'SGTSC063'.

      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*

      * Areas de trabalho
      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

       01  REG-WS.
           03  PC-TARF-OPR-DIST   PIC  S9(04)V9(10)  VALUE  ZEROS.
           03  PC-TARF-OPR-ETRO   PIC  S9(04)V9(10)  VALUE  ZEROS.
           03  PC-TARF-OPR-GRT    PIC  S9(04)V9(10)  VALUE  ZEROS.
           03  IN-ADC-IMPO        PIC   X(01)        VALUE  SPACES.
           03  CD-TIP-TARF        PIC  S9(9) COMP-5 VALUE 0.
           03  CD-USU-ULT-ATL     PIC   X(08) VALUE SPACES.
           03  TS-ULT-ATL         PIC   X(26) VALUE SPACES.
           03  PC-TARF-OPR        PIC  S9(04)V9(10) COMP-3 VALUE  ZEROS.

      *----------------------------------------------------------------*
      * Book dos Containers e das subrotinas chamadas
      *----------------------------------------------------------------*

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      * Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999

      * Book para captura de erro via MSG1
-INC MSGKWMG1

      * Book do container de requisição e resposta deste modulo
       01 SGTS0092-DADOS.
-INC SGTK0092

      * Consulta indicador de operação estruturada (IN_OPR_ETRD)
-INC SGTKC063

      * Book do Container de Erro
-INC IIBKERRO

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

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

           PERFORM 999999-ENCERRAR

           GOBACK

           .
       000000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       100000-INICIAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      * Habilita tratamento automático de Erros DB2
-INC HLPKE997
      *    Ativa a captura automatica de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM 970000-LER-CANAL

      *    Define o nome do programa p/ fins manipulacao container
           MOVE 'SGTS0092'             TO NM-PGM

      *    Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA

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

      *    Valida campo a campo da requisição

           IF  SGTS0092-CD-CTRA-TIT-VL  <= ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'inválido.'         TO MSG1-TX-ERRO-TS
               PERFORM 998900-ERRO
           END-IF

           IF  SGTS0092-CD-OPR-ETRD-MCP IS NOT NUMERIC
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'       TO MSG1-TX-ERRO-TS
               PERFORM 998900-ERRO
           END-IF

           IF  SGTS0092-CD-OPR-ETRD-MCP <= ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0004               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais inv
      -             'álido.'        TO MSG1-TX-ERRO-TS
               PERFORM 998900-ERRO
           END-IF


           IF  SGTS0092-NR-VRS-OPR-ETRD <= ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0006               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 998900-ERRO
           END-IF
           .
       110000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 201000-TRATA-DISTRIBUICAO

           PERFORM 202000-TRATA-ESTRUTURACAO

           PERFORM 203000-TRATA-GARANTIA-FIRME

           PERFORM 204000-CARTEIRA-ATIVA

      *    Movimenta dados de saída e grava container de resposta
           MOVE  'SGTS0092'  TO  NM-PGM
           PERFORM 980000-GRAVAR-CONTAINER-RPST
           .
       200000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       201000-TRATA-DISTRIBUICAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

      *    Distribuição
           MOVE 1                        TO CD-TIP-TARF

           PERFORM 910000-CONSULTA-TARIFA

           IF  SQLCODE  EQUAL  ZEROS
               MOVE  PC-TARF-OPR  TO  PC-TARF-OPR-DIST
           ELSE
               IF  SQLCODE  EQUAL  +100
                   MOVE  ZEROS  TO  PC-TARF-OPR-DIST
               ELSE
                   SET  ERRO-PROGRAMADO     TO TRUE
                   MOVE 201                 TO  MSG1-CD-ERRO-TS
                   MOVE 'Erro na leitura da tarifa distribuição.'
                                            TO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           END-IF
           .
       201000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       202000-TRATA-ESTRUTURACAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

      *    Estruturação
           MOVE 2                        TO CD-TIP-TARF

           PERFORM 910000-CONSULTA-TARIFA

           IF  SQLCODE  EQUAL  ZEROS
               MOVE  PC-TARF-OPR  TO  PC-TARF-OPR-ETRO
           ELSE
               IF  SQLCODE  EQUAL  +100
                   MOVE  ZEROS  TO  PC-TARF-OPR-ETRO
               ELSE
                   SET  ERRO-PROGRAMADO     TO TRUE
                   MOVE 202                 TO  MSG1-CD-ERRO-TS
                   MOVE 'Erro na leitura da tarifa estruturação.'
                                            TO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           END-IF
           .
       202000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       203000-TRATA-GARANTIA-FIRME     SECTION.
      *----------------------------------------------------------------*
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

      *    Garantia firme
           MOVE 3                        TO CD-TIP-TARF

           PERFORM 910000-CONSULTA-TARIFA

           IF  SQLCODE  EQUAL  ZEROS
               MOVE  PC-TARF-OPR  TO  PC-TARF-OPR-GRT
           ELSE
               IF  SQLCODE  EQUAL  +100
                   MOVE  ZEROS  TO  PC-TARF-OPR-GRT
               ELSE
                   SET  ERRO-PROGRAMADO     TO TRUE
                   MOVE 203                 TO  MSG1-CD-ERRO-TS
                   MOVE 'Erro na leitura da tarifa garantia firme.'
                                            TO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           END-IF
           .
       203000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       204000-CARTEIRA-ATIVA           SECTION.
      *----------------------------------------------------------------*
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSC063-DADOS.

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSC063               TO NM-PGM

      *    Movimentar os campos de entrada

           MOVE SGTS0092-CD-CTRA-TIT-VL  TO SGTSC063-CD-CTRA-TIT-VL
           MOVE SGTS0092-CD-OPR-ETRD-MCP TO SGTSC063-CD-OPR-ETRD-MCP
           MOVE SGTS0092-NR-VRS-OPR-ETRD TO SGTSC063-NR-VRS-OPR-ETRD
      *    Cross UP
           MOVE 18                       TO SGTSC063-CD-TIP-IN

           PERFORM 900000-EXECUTAR-MODULO

           PERFORM 973000-LER-RESPOSTA

           MOVE SGTSC063-IN-EXT-TIP  TO  IN-ADC-IMPO
           .
       204000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       900000-EXECUTAR-MODULO          SECTION.
      *----------------------------------------------------------------*

           MOVE 0007                   TO MSG1-CD-SEQL-ERRO
      *    Grava o container de requisição do módulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM
               WHEN SGTSC063
                   EXEC CICS LINK
                             PROGRAM ( SGTSC063  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN OTHER
                   SET ERRO-PROGRAMADO TO TRUE
                   MOVE 9006          TO MSG1-CD-ERRO-TS
                   STRING
                      'Não definida a execução do módulo ' NM-PGM '.'
                      DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                   END-STRING
                   PERFORM 998900-ERRO
           END-EVALUATE
      *
           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CICS      TO TRUE
                    MOVE 3004          TO MSG1-CD-ERRO-TS
                    STRING
                      'Erro  ao acionar o módulo ' NM-PGM ', '
                      'RESP  ='   ' ' MSG1-RESP1  ', '
                      'RESP2 =' ' ' MSG1-RESP2
                      DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    END-STRING


                    PERFORM 998900-ERRO
           END-EVALUATE
      *
      *    EFETUA A LEITURA DO CONTAINER DE ERRO
           PERFORM 972000-LER-CONTAINER-DE-ERRO
      *
           IF  IIBKERRO-CD-ERRO  EQUAL  ZEROS
      *        EFETUA A LEITURA DO CONTAINER DE RESPOSTA
               PERFORM 973000-LER-RESPOSTA
           END-IF
           .
       900000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       910000-CONSULTA-TARIFA      SECTION.
      *----------------------------------------------------------------*

            EXEC SQL
                 SELECT CD_USU_ULT_ATL
                      , TS_ULT_ATL
                      , PC_TARF_OPR
                   INTO  :CD-USU-ULT-ATL
                       , :TS-ULT-ATL
                       , :PC-TARF-OPR
                       FROM DB2SGT.TARF_OPR_ETRD
                WHERE CD_CTRA_TIT_VL     = :SGTS0092-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP    = :SGTS0092-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD    = :SGTS0092-NR-VRS-OPR-ETRD
                  AND CD_TIP_TARF        = :CD-TIP-TARF
           END-EXEC
            .
       910000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*

           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM
               WHEN SGTSC063
                    MOVE 'SGTSC063-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSC063-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSC063-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF  SGTSC063-CD-OPR-ETRD-MCP IS NUMERIC
                        MOVE SGTSC063-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP
                        MOVE CD-OPR-ETRD-MCP-TX
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE 'Valor não numérico.'
                                       TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSC063-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSC063-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSC063-CD-TIP-IN'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSC063-CD-TIP-IN
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
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

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

           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisicao
           MOVE 'SGTS0092'             TO NM-PGM
           MOVE 'SGTS0092-RQSC'        TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA
      * -->> Início

      *    Obtem o tamanho esperado da requisicao
           MOVE LENGTH OF SGTS0092-RQSC TO QT-CCT-EPRO

      *    Valida tamanho esperado da requisicao X tamanho recebido

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA   TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
      * -->> Fim

           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER ( NM-CTNR       )
                     FLENGTH   ( QT-CCT-LIDO   )
                     INTO      ( SGTS0092-RQSC )
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

           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

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
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    IF  NM-PGM  EQUAL  'SGTSC084'  AND
                        IIBKERRO-CD-ERRO  EQUAL  5
                        CONTINUE
                    ELSE
                        SET ERRO-PROGRAMA-ACIONADO TO TRUE
                        MOVE  NM-PGM           TO MSG1-NM-PGM-ACND
                        MOVE IIBKERRO-TX-ERRO  TO MSG1-TX-RTN-PGM-ACND
                                                  MSG1-TX-ERRO-TS
                        MOVE IIBKERRO-CD-ERRO  TO MSG1-CD-RTN-PGM-ACND
                                                  MSG1-CD-ERRO-TS

      *             Monitorar variáveis do módulo chamado
                        PERFORM 969000-MONITORAR-VARIAVEIS
                        PERFORM 998900-ERRO
                    END-IF
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
      *---------------------------------------------------------------*

           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome dos container de acordo com módulo a executar
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

           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM

               WHEN SGTSC063
                    INITIALIZE SGTSC063-RPST
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSC063-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN OTHER
                    SET ERRO-PROGRAMADO TO TRUE
                    MOVE  9006       TO   MSG1-CD-ERRO-TS
                    MOVE  'Não definida a execução do módulo.'
                                     TO MSG1-TX-ERRO-TS
                    PERFORM 998900-ERRO
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       973100-LER-CONTAINER-NODATA     SECTION.
      *----------------------------------------------------------------*

           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Obtem a quantidade de caracteres no container
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

      *----------------------------------------------------------------*
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *----------------------------------------------------------------*

      *    Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           MOVE PC-TARF-OPR-DIST  TO SGTS0092-PC-TARF-OPR-DIST
           MOVE PC-TARF-OPR-ETRO  TO SGTS0092-PC-TARF-OPR-ETRO
           MOVE PC-TARF-OPR-GRT   TO SGTS0092-PC-TARF-OPR-GRT
           MOVE IN-ADC-IMPO       TO SGTS0092-IN-ADC-IMPO

      *    Monta o nome do container de resposta
           MOVE 'SGTS0092-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

           EXEC CICS PUT
               CONTAINER ( NM-CTNR                 )
                    FROM ( SGTS0092-RPST           )
                 FLENGTH ( LENGTH OF SGTS0092-RPST )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *----------------------------------------------------------------*
      *    Postar container com dados de requisição (entrada)
      *----------------------------------------------------------------*

           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome dos container de acordo com módulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RQSC'       TO NM-CTNR(9:5)

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisição do programa a executar
           EVALUATE NM-PGM
               WHEN SGTSC063
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSC063-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSC063-RQSC )
                    END-EXEC

               WHEN OTHER
                    SET ERRO-PROGRAMADO TO TRUE
                    MOVE  9006       TO   MSG1-CD-ERRO-TS
                    MOVE  'Não definida a execução do módulo.'
                                     TO MSG1-TX-ERRO-TS
                    PERFORM 998900-ERRO
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       982000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA         SECTION.
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*

           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

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
      *
      *----------------------------------------------------------------*
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *----------------------------------------------------------------*

      *    Limpar variáveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO
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

      *----------------------------------------------------------------*
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0020                   TO MSG1-CD-SEQL-ERRO

      *    Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS PUT
                     CONTAINER ( 'PRM-ERRO'                  )
                     FROM      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC
           .
       984000-SAI.
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

           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE 'Erro não especificado no módulo.' TO
                     MSG1-TX-ERRO-TS
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

           MOVE ' SGTS0092-CD-CTRA-TIT-VL '      TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0092-CD-CTRA-TIT-VL         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0092-CD-OPR-ETRD-MCP'       TO MSGKW999-NM-VRV-MNT
           IF  SGTS0092-CD-OPR-ETRD-MCP          IS NUMERIC
               MOVE SGTS0092-CD-OPR-ETRD-MCP     TO   CD-OPR-ETRD-MCP
               MOVE CD-OPR-ETRD-MCP-TX           TO MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Valor não numérico.'        TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0092-NR-VRS-OPR-ETRD'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTS0092-NR-VRS-OPR-ETRD         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *    Grava o erro gerado no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG

           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO

      *    Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

      *    Encerra a transacao
           PERFORM 999999-ENCERRAR
           .
       999000-SAI.
           EXIT.
      * --------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * MSG1-CD-ERRO-TS = 9999
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * MSG1-CD-ERRO-TS = 9998
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      * --------------------------------------*
-INC HLPKP999
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * MSG1-CD-ERRO-TS = 3000
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * MSG1-CD-ERRO-TS = 3001
      * 999006-TRATA-ERRO-CANAL        SECTION.
      * MSG1-CD-ERRO-TS = 3003
      *---------------------------------------*
-INC SGTKP999
      * --------------------------------------*
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
      * --------------------------------------*
-INC MSGKPMG1
      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997
      *----------------------------------------------------------------*
       999999-ENCERRAR                 SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTS0092 -----------------------------