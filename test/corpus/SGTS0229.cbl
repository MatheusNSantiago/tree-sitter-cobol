       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0229
      * ANALISTA..: F8785286 - Roseli Akemi Kawahata
      * AUTOR.....: F0950413 - António Francisco Mendonça Silva
      * DATA......: 17/10/2018
      * OBJETIVO..: Negócio - Listar dados finalização cadastro da Ope-
      *             ração Estruturada de Mercado de Capitais.
      * COMPILACAO: COBOL/CICS (On-line)
      *-----------------------------------------------------------------
      * VRS001 17.10.2018 - F0950413 - 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0229.

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
               '*** SGTS0229 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS001'.

      *--> Módulos a serem acionados via 'CONTAINER'
       77  SGTSL009                    PIC X(08) VALUE 'SGTSL009'.
       77  SGTSL083                    PIC X(08) VALUE 'SGTSL083'.
       77  SGTSL095                    PIC X(08) VALUE 'SGTSL095'.
       77  SGTSL117                    PIC X(08) VALUE 'SGTSL117'.

       01  CTE-FIM-WS                  PIC X(32)  VALUE
                                             '*** WSS TERMINA AQUI ***'.
      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  CTE-INICIO-LS               PIC  X(32)  VALUE
                                             '*** LS COMECA AQUI ***'.
      *--> Variáveis de trabalho
       01  IDX-TB-RGR                  PIC S9(04)     COMP-5  VALUE   0.
       01  IC-TAB                      PIC S9(04)     COMP-5  VALUE   0.
       01  CNT-OCR-OPR                 PIC S9(04)     COMP-5  VALUE   0.

      *--> Book do container de erro
-INC IIBKERRO
      *--> Book para captura de erro via MSG1
-INC MSGKWMG1
      *--> Variaveis para tratamento de Abend e Condition
-INC HLPKW999
      *--> Variaveis para tratamento de CONTAINER
-INC SGTKERR1

      *--> Container de erro
       01  CTNR-PRM-ERRO               PIC X(16) VALUE 'PRM-ERRO'.

      *--> Areas de Controle de Programa Chamado (CONTAINER)
       01  CTL-PGM-CHMD.
           03  NM-PGM-CHMD             PIC  X(08)         VALUE ' '.

       01  WS-ERRO-CICS.
           03  WS-RESP1                PIC  9(008).
           03  WS-RESP2                PIC  9(008).

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      ---------------9.
       01  MCR-S9-15-V9-04             PIC  ----.---.---.---.--9,9999.

      *--> Timestamp de trabalho
       01  AUX-TS.
           03  ANO                     PIC  X(04)     VALUE '0001'.
           03  FILLER                  PIC  X(01)     VALUE '-'.
           03  MES                     PIC  X(02)     VALUE '01'.
           03  FILLER                  PIC  X(01)     VALUE '-'.
           03  DIA                     PIC  X(02)     VALUE '01'.
           03  FILLER                  PIC  X(01)     VALUE '-'.
           03  HORA                    PIC  X(02)     VALUE '00'.
           03  FILLER                  PIC  X(01)     VALUE '.'.
           03  MINUTO                  PIC  X(02)     VALUE '00'.
           03  FILLER                  PIC  X(01)     VALUE '.'.
           03  SEGUNDO                 PIC  X(02)     VALUE '00'.
           03  FILLER                  PIC  X(01)     VALUE '.'.
           03  MICRO-SEGUNDO           PIC  X(06)     VALUE '000000'.

       01  TB-SEARCH.
           03  TB-RGR                  OCCURS 50 TIMES.
               05 TB-CD-RGR-VLDC       PIC S9(09) COMP-5.
               05 TB-NR-VRS-RGR-VLDC   PIC S9(09) COMP-5.

      *-----------------------------------------------------------------
      *--> Consulta da tabela 'DB2SGT.EST_OPR_ETRD'
       01  SGTSL009-DADOS.
-INC SGTKL009

      *--> Consulta da tabela 'DB2SGT.EXCO_PRFL_SELD'
-INC SGTKL083

      *--> Consulta da tabela 'DB2SGT.ITEM_EXCO_OPR'
-INC SGTKL095

      *--> Consulta das tabelas 'TIP_JST_VLDC_OPR / JST_VLDC_OPR_ETRD'
       01  SGTSL117-DADOS.
-INC SGTKL117

      *--> Book do programa
       01  SGTS0229-DADOS.
-INC SGTK0229

       01  CTE-FIM-LS                  PIC  X(32)  VALUE
                                             '*** LS TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999
           MOVE CTE-PROG(5:8)          TO NM-PGM
                                          NM-PGM-CHMD

           PERFORM 100000-OBTER-NOME-DO-CANAL
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
           PERFORM 970000-LER-REQUISICAO-RECEBIDA

           PERFORM 140000-VALIDAR-PARAMETROS
           PERFORM 150000-VALOR-INICIAL-VARIAVEIS

           PERFORM 200000-PROCESSA-OPERACAO
           PERFORM 300000-PROCESSA-JUSTIFICATIVA
           PERFORM 400000-PROCESSA-EXCECAO

           PERFORM 980000-GRAVAR-CTNR-RESPOSTA
           PERFORM 999999-ENCERRAR
           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-OBTER-NOME-DO-CANAL      SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
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
       140000-VALIDAR-PARAMETROS       SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           IF  SGTS0229-CD-CTRA-TIT-VL  NOT > ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de títulos e valores mobiliários
      -             ' inválido.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0229-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
            IF SGTS0229-CD-OPR-ETRD-MCP = 0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0005               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' informado.'      TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
            END-IF
           END-IF

           IF  SGTS0229-NR-VRS-OPR-ETRD NOT > ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0006               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALOR-INICIAL-VARIAVEIS  SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           SET SGTSL083-LISTA-EXCO-ATV           TO TRUE
           MOVE 3                                TO SGTSL095-CD-TIP-SRVC

           SET SGTSL117-LS-JST-VLDC-OPR-ETRD     TO TRUE
           SET SGTSL009-LISTA-POR-OPR            TO TRUE

           MOVE SGTS0229-CD-CTRA-TIT-VL
                                       TO SGTSL009-CD-CTRA-TIT-VL
                                          SGTSL083-CD-CTRA-TIT-VL
                                          SGTSL095-CD-CTRA-TIT-VL
                                          SGTSL117-CD-CTRA-TIT-VL
           MOVE SGTS0229-CD-OPR-ETRD-MCP
                                       TO SGTSL009-CD-OPR-ETRD-MCP
                                          SGTSL083-CD-OPR-ETRD-MCP
                                          SGTSL095-CD-OPR-ETRD-MCP
                                          SGTSL117-CD-OPR-ETRD-MCP
           MOVE SGTS0229-NR-VRS-OPR-ETRD
                                       TO SGTSL009-NR-VRS-OPR-ETRD
                                          SGTSL083-NR-VRS-OPR-ETRD
                                          SGTSL095-NR-VRS-OPR-ETRD
                                          SGTSL117-NR-VRS-OPR-ETRD

           MOVE AUX-TS                 TO SGTSL009-TS-EST-OPR-ETRD-ULT

           MOVE 3                      TO SGTS0229-QT-OCR-OPR
           MOVE 0                      TO SGTSL083-E-NR-SEQL-EXCO-PRFL
                                          SGTSL083-CD-RGR-VLDC
                                          SGTSL083-NR-VRS-RGR-VLDC
                                          SGTSL095-E-NR-SEQL-ITEM-EXCO
                                          SGTSL117-ULT-CD-TIP-VLDC-OPR
                                          SGTS0229-CD-TIP-EST-OPR(1)
                                          SGTS0229-CD-TIP-EST-OPR(2)
                                          SGTS0229-CD-TIP-EST-OPR(3)
           .
       150000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-OPERACAO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSL009               TO NM-PGM-CHMD
           PERFORM UNTIL SGTSL009-IN-CNTO-PSQ  EQUAL 'N'
               PERFORM 900000-PROCEDIMENTOS-CONTAINER

               PERFORM VARYING SGTSL009-IC-TAB FROM 1 BY 1
                     UNTIL SGTSL009-IC-TAB > SGTSL009-QT-OCR-TAB
                  IF SGTS0229-CD-TIP-EST-OPR(1)  EQUAL 0  AND
                     SGTSL009-CD-TIP-EST-OPR (SGTSL009-IC-TAB) = 1
                     MOVE SGTSL009-CD-TIP-EST-OPR (SGTSL009-IC-TAB)
                                       TO SGTS0229-CD-TIP-EST-OPR  (1)
                     MOVE SGTSL009-CD-USU-RSP-EST (SGTSL009-IC-TAB)
                                       TO SGTS0229-CD-MTC-RSP-OPR  (1)
                     MOVE SGTSL009-TS-EST-OPR-ETRD (SGTSL009-IC-TAB)
                                       TO SGTS0229-TS-EST-OPR      (1)
                     IF SGTS0229-CD-TIP-EST-OPR(2) EQUAL 0 AND
                        SGTS0229-CD-TIP-EST-OPR(3) EQUAL 0
                        MOVE 1            TO SGTS0229-QT-OCR-OPR
                        MOVE SGTSL009-QT-OCR-TAB  TO  SGTSL009-IC-TAB
                     END-IF
                  END-IF

                  IF SGTS0229-CD-TIP-EST-OPR(2)  EQUAL 0  AND
                     SGTSL009-CD-TIP-EST-OPR (SGTSL009-IC-TAB) = 2
                     MOVE SGTSL009-CD-TIP-EST-OPR (SGTSL009-IC-TAB)
                                       TO SGTS0229-CD-TIP-EST-OPR  (2)
                     MOVE SGTSL009-CD-USU-RSP-EST (SGTSL009-IC-TAB)
                                       TO SGTS0229-CD-MTC-RSP-OPR  (2)
                     MOVE SGTSL009-TS-EST-OPR-ETRD (SGTSL009-IC-TAB)
                                       TO SGTS0229-TS-EST-OPR      (2)
                     IF SGTS0229-CD-TIP-EST-OPR(3) EQUAL 0
                        MOVE 2             TO SGTS0229-QT-OCR-OPR
                        IF SGTS0229-CD-TIP-EST-OPR(1) NOT EQUAL 0
                           MOVE SGTSL009-QT-OCR-TAB  TO  SGTSL009-IC-TAB
                        END-IF
                     END-IF
                  END-IF

                  IF SGTS0229-CD-TIP-EST-OPR(3)  EQUAL 0  AND
                     SGTSL009-CD-TIP-EST-OPR (SGTSL009-IC-TAB) = 3
                     MOVE 3            TO SGTS0229-QT-OCR-OPR
                     MOVE SGTSL009-CD-TIP-EST-OPR (SGTSL009-IC-TAB)
                                       TO SGTS0229-CD-TIP-EST-OPR  (3)
                     MOVE SGTSL009-CD-USU-RSP-EST (SGTSL009-IC-TAB)
                                       TO SGTS0229-CD-MTC-RSP-OPR  (3)
                     MOVE SGTSL009-TS-EST-OPR-ETRD (SGTSL009-IC-TAB)
                                       TO SGTS0229-TS-EST-OPR      (3)
                  END-IF

                  IF SGTS0229-CD-TIP-EST-OPR(1)  NOT EQUAL 0 AND
                     SGTS0229-CD-TIP-EST-OPR(2)  NOT EQUAL 0 AND
                     SGTS0229-CD-TIP-EST-OPR(3)  NOT EQUAL 0
                     MOVE SGTSL009-QT-OCR-TAB  TO  SGTSL009-IC-TAB
                  END-IF
               END-PERFORM

               MOVE SGTSL009-TS-EST-OPR-ETRD (SGTSL009-QT-OCR-TAB)
                                       TO SGTSL009-TS-EST-OPR-ETRD-ULT
           END-PERFORM
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-FORMATA-OCORR-1-2-OPR    SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           PERFORM VARYING IC-TAB FROM 1 BY 1
                     UNTIL IC-TAB > SGTSL009-QT-OCR-TAB
                   IF SGTSL009-CD-TIP-EST-OPR (IC-TAB) = 1
                      ADD 1            TO CNT-OCR-OPR
                      MOVE SGTSL009-CD-TIP-EST-OPR (IC-TAB)
                                       TO SGTS0229-CD-TIP-EST-OPR  (2)
                      MOVE SGTSL009-CD-USU-RSP-EST (IC-TAB)
                                       TO SGTS0229-CD-MTC-RSP-OPR  (2)
                      MOVE SGTSL009-TS-EST-OPR-ETRD (IC-TAB)
                                       TO SGTS0229-TS-EST-OPR      (2)
                      IF SGTSL009-CD-USU-RSP-EST (IC-TAB) NOT EQUAL
                         SGTS0229-CD-MTC-RSP-OPR (2)
                         ADD 1            TO CNT-OCR-OPR
                         MOVE SGTSL009-CD-TIP-EST-OPR (IC-TAB)
                                       TO SGTS0229-CD-TIP-EST-OPR  (1)
                         MOVE SGTSL009-CD-USU-RSP-EST (IC-TAB)
                                       TO SGTS0229-CD-MTC-RSP-OPR  (1)
                         MOVE SGTSL009-TS-EST-OPR-ETRD (IC-TAB)
                                       TO SGTS0229-TS-EST-OPR      (1)
                      ELSE
                         MOVE 3        TO CNT-OCR-OPR
                      END-IF
                      MOVE SGTSL009-QT-OCR-TAB   TO  IC-TAB
                   END-IF
           END-PERFORM

           MOVE SGTSL009-TS-EST-OPR-ETRD (SGTSL009-QT-OCR-TAB)
                                       TO SGTSL009-TS-EST-OPR-ETRD-ULT
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-PROCESSA-JUSTIFICATIVA   SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSL117               TO NM-PGM-CHMD
           PERFORM 900000-PROCEDIMENTOS-CONTAINER

           PERFORM VARYING IC-TAB FROM 1 BY 1
                     UNTIL IC-TAB > SGTSL117-QT-OCR-TAB
                        OR IC-TAB > 50
                   MOVE IC-TAB           TO SGTS0229-QT-OCR-JST
                   MOVE SGTSL117-CD-TIP-VLDC-OPR    (IC-TAB)    TO
                        SGTS0229-CD-TIP-VLDC-JST    (IC-TAB)
                   MOVE SGTSL117-TX-JST-VLDC-OPR    (IC-TAB)    TO
                        SGTS0229-TX-JST             (IC-TAB)
                   MOVE SGTSL117-TX-TIP-VLDC-OPR    (IC-TAB)    TO
                        SGTS0229-TX-TIP-JST         (IC-TAB)
                   MOVE SGTSL117-CD-USU-ULT-ATL     (IC-TAB)    TO
                        SGTS0229-CD-USU-ULT-ATL-JST (IC-TAB)
                   MOVE SGTSL117-TS-ULT-ATL         (IC-TAB)    TO
                        SGTS0229-TS-ULT-ATL-JST     (IC-TAB)
           END-PERFORM
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       400000-PROCESSA-EXCECAO         SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSL083               TO NM-PGM-CHMD
           PERFORM 900000-PROCEDIMENTOS-CONTAINER

           PERFORM VARYING SGTSL083-IC-TAB FROM 1 BY 1
                     UNTIL SGTSL083-IC-TAB > SGTSL083-QT-OCR-TAB
             MOVE SGTSL083-NR-SEQL-EXCO-PRFL (SGTSL083-IC-TAB)
                                       TO SGTSL095-E-NR-SEQL-EXCO-PRFL

             MOVE SGTSL095               TO NM-PGM-CHMD
             PERFORM 900000-PROCEDIMENTOS-CONTAINER

             PERFORM VARYING IC-TAB  FROM 1 BY 1
                     UNTIL IC-TAB > SGTSL095-QT-OCR-TAB
               ADD  1              TO SGTS0229-QT-OCR-EXCO
               MOVE SGTSL095-NR-SEQL-EXCO-PRFL    (IC-TAB)  TO
                    SGTS0229-NR-SEQL-EXCO         (SGTS0229-QT-OCR-EXCO)
               MOVE SGTSL095-NR-SEQL-ITEM-EXCO    (IC-TAB)  TO
                    SGTS0229-NR-SEQL-ITEM-EXCO    (SGTS0229-QT-OCR-EXCO)
               MOVE SGTSL095-CD-RGR-VLDC          (IC-TAB)  TO
                    SGTS0229-CD-RGR-VLDC-EXCO     (SGTS0229-QT-OCR-EXCO)
               MOVE SGTSL095-NR-VRS-RGR-VLDC      (IC-TAB)  TO
                    SGTS0229-NR-VRS-RGR-VLDC-EXCO (SGTS0229-QT-OCR-EXCO)
               MOVE SGTSL095-NR-SEQL-ITC          (IC-TAB)  TO
                    SGTS0229-NR-SEQL-ITC-EXCO     (SGTS0229-QT-OCR-EXCO)
               MOVE SGTSL095-IN-ITEM-CRIO-EXCO    (IC-TAB)  TO
                    SGTS0229-IN-ITEM-CRIC-EXCO    (SGTS0229-QT-OCR-EXCO)
               MOVE SGTSL095-VL-RLZD-ITEM-EXCO    (IC-TAB)  TO
                    SGTS0229-VL-RLZD-ITEM-EXCO    (SGTS0229-QT-OCR-EXCO)
               MOVE SGTSL095-TX-RLZD-ITEM-EXCO    (IC-TAB)  TO
                    SGTS0229-TX-RLZD-ITEM-EXCO    (SGTS0229-QT-OCR-EXCO)
               MOVE SGTSL095-NR-RLZD-ITEM-EXCO    (IC-TAB)  TO
                    SGTS0229-NR-RLZD-ITEM-EXCO    (SGTS0229-QT-OCR-EXCO)
               IF ((IC-TAB               EQUAL   1)      OR
                  (SGTSL095-CD-RGR-VLDC(IC-TAB)          NOT EQUAL
                   SGTSL095-CD-RGR-VLDC(IC-TAB - 1))     OR
                  (SGTSL095-NR-VRS-RGR-VLDC(IC-TAB)      NOT EQUAL
                   SGTSL095-NR-VRS-RGR-VLDC(IC-TAB - 1)))
                  ADD  1               TO IDX-TB-RGR
                  MOVE SGTSL095-CD-RGR-VLDC(IC-TAB)
                                       TO TB-CD-RGR-VLDC (IDX-TB-RGR)
                  MOVE SGTSL095-NR-VRS-RGR-VLDC(IC-TAB)
                                       TO TB-NR-VRS-RGR-VLDC(IDX-TB-RGR)
               END-IF
             END-PERFORM
           END-PERFORM

           PERFORM VARYING IC-TAB  FROM 1 BY 1
               UNTIL IC-TAB  >  IDX-TB-RGR
               MOVE TB-CD-RGR-VLDC (IC-TAB)
                                   TO SGTS0229-CD-RGR-VLDC(IC-TAB)
               MOVE TB-NR-VRS-RGR-VLDC(IC-TAB)
                                   TO SGTS0229-NR-VRS-RGR-VLDC(IC-TAB)
           END-PERFORM
           MOVE IDX-TB-RGR         TO SGTS0229-QT-OCR-RGR
           .
       400000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       900000-PROCEDIMENTOS-CONTAINER  SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

      *--> Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVA-REQUISICAO

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *--> Executa a operação acionada via IIB
           PERFORM 910000-EXECUTA-MODULOS

      *--> Lê o container de erro da operação acionada
           PERFORM 971000-LER-ERRO

      *--> Lê o container de resposta da operação acionada
           PERFORM 972000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       910000-EXECUTA-MODULOS          SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM-CHMD
      *--> Executar Módulo SGTSL009, container sem Serviço
               WHEN SGTSL009
                 EXEC CICS LINK
                           PROGRAM ( SGTSL009  )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC

      *--> Executar Módulo SGTSL083, container sem Serviço
               WHEN SGTSL083
                 EXEC CICS LINK
                           PROGRAM ( SGTSL083  )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC

      *--> Executar Módulo SGTSL095, container sem Serviço
               WHEN SGTSL095
                 EXEC CICS LINK
                           PROGRAM ( SGTSL095  )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC

      *--> Executar Módulo SGTSL117, container sem Serviço
               WHEN SGTSL117
                 EXEC CICS LINK
                           PROGRAM ( SGTSL117  )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC

      *--> Execução de Módulo não prevista
               WHEN OTHER
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 9006             TO MSG1-CD-ERRO-TS
                 STRING 'Não definida a execução do módulo '
                         NM-PGM-CHMD '.'
                         DELIMITED BY SIZE
                         INTO         MSG1-TX-ERRO-TS
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
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

      *--> Análise do retorno das chamadas dos programas
           SET ERRO-PROGRAMA-ACIONADO  TO  TRUE
           MOVE 3004          TO MSG1-CD-ERRO-TS
           STRING
              'Erro ao acionar o módulo ' NM-PGM-CHMD '.'
              'RESP  ='   ' ' MSG1-RESP1  ', '
              'RESP2 =' ' ' MSG1-RESP2
               DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
           END-STRING
           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       911000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970000-LER-REQUISICAO-RECEBIDA  SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisição
           MOVE 'SGTS0229'             TO NM-PGM
           MOVE 'SGTS0229-RQSC'        TO NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM 970100-LER-CONTAINER-NODATA

      *--> Obtem o tamanho esperado da requisição
           MOVE LENGTH OF SGTS0229-RQSC TO QT-CCT-EPRO

      *--> Valida tamanho esperado da requisição X tamanho recebido

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTS0229-RQSC' )
              CHANNEL   ( NM-CNL          )
              FLENGTH   ( QT-CCT-LIDO     )
              INTO      ( SGTS0229-RQSC   )
           END-EXEC
           .
       970000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       970100-LER-CONTAINER-NODATA     SECTION.
      *----------------------------------------------------------------*
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Obtem a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR    )
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       970100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       971000-LER-ERRO                 SECTION.
      *----------------------------------------------------------------*
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

      *----------------------------------------------------------------*
      *    A presença do container PRM-ERRO indica que houve erro de   *
      *    integração, a ausência do container indica sucesso.         *
      *----------------------------------------------------------------*

      *--> Configura dados para leitura de container de erro
           MOVE CTNR-PRM-ERRO          TO NM-CTNR
           SET  GET-CONTAINER          TO TRUE
           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Leitura do container de erro
      *--> Não há captura automática de erro CICS
           EXEC CICS GET
                     CONTAINER ( NM-CTNR                     )
                     INTO      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     NOHANDLE
           END-EXEC

           EVALUATE TRUE
              WHEN EIBRESP             EQUAL DFHRESP(CONTAINERERR)
                   CONTINUE
              WHEN EIBRESP             EQUAL DFHRESP(NORMAL)
      *--> Trata o erro de programa acionado por ser módulo interno
               SET ERRO-PROGRAMA-ACIONADO   TO  TRUE
               IF NM-PGM-CHMD(1:3) EQUAL 'SGT'
                  MOVE IIBKERRO-CD-ERRO     TO  MSG1-CD-RTN-PGM-ACND
                                                MSG1-CD-ERRO-TS
                  MOVE IIBKERRO-TX-ERRO     TO  MSG1-TX-RTN-PGM-ACND
                                                MSG1-TX-ERRO-TS
               ELSE
                  MOVE  3004                TO  MSG1-CD-ERRO-TS
                  STRING  'Erro ao acionar o módulo ' NM-PGM-CHMD '.'
                            DELIMITED BY SIZE
                            INTO        MSG1-TX-ERRO-TS
                  END-STRING
                  MOVE NM-PGM-CHMD          TO  MSG1-NM-PGM-ACND
                  MOVE IIBKERRO-CD-ERRO     TO  MSG1-CD-RTN-PGM-ACND
                  MOVE IIBKERRO-TX-ERRO     TO  MSG1-TX-RTN-PGM-ACND
               END-IF
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              WHEN OTHER
                   SET ERRO-CONTAINER       TO  TRUE
                   PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-RESPOSTA             SECTION.
      *-----------------------------------------------------------------
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

      *--> Inicializa quantidade de caracteres lidos na área de resposta
           MOVE ZEROS                  TO QT-CCT-LIDO

           MOVE NM-PGM-CHMD            TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

      *--> Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *--> Por causa da captura automática erro CICS, deve ser previsto
      *--> o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Trata área de resposta do programa acionado (NODATA)
           PERFORM 972100-TRATA-AREA-RESPOSTA

      *--> Recupera o container de resposta do programa que foi acionado
           EVALUATE NM-PGM-CHMD

      *--> Lê container de resposta do SGTSL009
               WHEN SGTSL009
                 MOVE LENGTH OF SGTSL009-RPST TO QT-CCT-EPRO
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSL009-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC

      *--> Lê container de resposta do SGTSL083
               WHEN SGTSL083
                 MOVE LENGTH OF SGTSL083-RPST TO QT-CCT-EPRO
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSL083-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC

      *--> Lê container de resposta do SGTSL095
               WHEN SGTSL095
                 MOVE LENGTH OF SGTSL095-RPST TO QT-CCT-EPRO
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSL095-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC

      *--> Lê container de resposta do SGTSL117
               WHEN SGTSL117
                 MOVE LENGTH OF SGTSL117-RPST TO QT-CCT-EPRO
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSL117-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC

      *--> Módulo não previsto para leitura do container
               WHEN OTHER
                 SET SEM-ERRO        TO TRUE
                 SET ERRO-PROGRAMADO TO TRUE
                 MOVE 9003           TO MSG1-CD-ERRO-TS
                 STRING
                    'Erro na leitura da resposta do módulo '
                     NM-PGM-CHMD '.'
                     DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                 END-STRING
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972100-TRATA-AREA-RESPOSTA      SECTION.
      *-----------------------------------------------------------------
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

      *--> Recupera o tamanho previsto e o tamanho recebido da resposta
      *--> do programa que foi acionado.

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Obtém a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO

           EXEC CICS GET
                CONTAINER (NM-CTNR    )
                NODATA
                FLENGTH   (QT-CCT-LIDO)
           END-EXEC
           .
       972100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980000-GRAVAR-CTNR-RESPOSTA     SECTION.
      *-----------------------------------------------------------------
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

      *--> Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

      *--> Monta o nome do container de resposta
           MOVE 'SGTS0229-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                CONTAINER ( NM-CTNR                 )
                CHANNEL   ( NM-CNL                  )
                FLENGTH   ( LENGTH OF SGTS0229-RPST )
                FROM      ( SGTS0229-RPST           )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       982000-GRAVA-REQUISICAO         SECTION.
      *-----------------------------------------------------------------
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para gravação do container de requisição
           SET  PUT-CONTAINER          TO TRUE

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *--> nome_modulo -RQSC

           MOVE NM-PGM-CHMD            TO NM-CTNR(1:8)
           MOVE '-RQSC'                TO NM-CTNR(9:5)

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Grava container da requisição do serviço IIB a ser acionado
      *--> Em caso de erro, a captura automática do CICS trata o erro

           EVALUATE NM-PGM-CHMD
      *--> Grava container de requisição do programa SGTSL009
               WHEN SGTSL009
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTSL009-RQSC           )
                            FLENGTH   ( LENGTH OF SGTSL009-RQSC )
                  END-EXEC

      *--> Grava container de requisição do programa SGTSL083
               WHEN SGTSL083
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTSL083-RQSC           )
                            FLENGTH   ( LENGTH OF SGTSL083-RQSC )
                  END-EXEC

      *--> Grava container de requisição do programa SGTSL095
               WHEN SGTSL095
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTSL095-RQSC           )
                            FLENGTH   ( LENGTH OF SGTSL095-RQSC )
                  END-EXEC

      *--> Grava container de requisição do programa SGTSL117
               WHEN SGTSL117
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTSL117-RQSC           )
                            FLENGTH   ( LENGTH OF SGTSL117-RQSC )
                  END-EXEC

      *--> Módulo não previsto para gravar container de requisição
               WHEN OTHER
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 9004             TO MSG1-CD-ERRO-TS
                 STRING
                    'Erro na gravação da requisição do módulo.'
                    NM-PGM-CHMD '.'
                     DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                 END-STRING
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       982000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *--> nome_modulo -RPST

           MOVE NM-PGM-CHMD            TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

           SET DEL-CONTAINER           TO TRUE

      *--> Exclui o container de resposta
      *--> Não há captura automática de erro CICS por usar RESP e RESP2
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

      *--> Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *----------------------------------------------------------------*
           MOVE 0020                   TO MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE CTNR-PRM-ERRO          TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
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

           EVALUATE  NM-PGM-CHMD

      *-->>SGTSL009<<
               WHEN SGTSL009
                    MOVE  'SGTSL009-CD-CTRA-TIT-VL'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL009-CD-CTRA-TIT-VL
                                               TO MCR-S9-15
                    MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL009-CD-OPR-ETRD-MCP'
                                               TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                    IF  SGTSL009-CD-OPR-ETRD-MCP
                                               IS NUMERIC
                        MOVE  SGTSL009-CD-OPR-ETRD-MCP
                                               TO MCR-S9-15
                        MOVE  MCR-S9-15        TO MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE  'Valor não numérico'
                                               TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL009-NR-VRS-OPR-ETRD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL009-NR-VRS-OPR-ETRD
                                               TO MCR-S9-15
                    MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL009-TS-EST-OPR-ETRD-ULT'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL009-TS-EST-OPR-ETRD-ULT
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTSL083<<
               WHEN SGTSL083
                    MOVE  'SGTSL083-E-CD-TIP-SRVC'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL083-E-CD-TIP-SRVC      TO MCR-S9-15
                    MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL083-CD-CTRA-TIT-VL'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL083-CD-CTRA-TIT-VL
                                               TO MCR-S9-15
                    MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL083-CD-OPR-ETRD-MCP'
                                               TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                    IF  SGTSL083-CD-OPR-ETRD-MCP
                                               IS NUMERIC
                        MOVE  SGTSL083-CD-OPR-ETRD-MCP
                                               TO MCR-S9-15
                        MOVE  MCR-S9-15        TO MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE  'Valor não numérico'
                                               TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL083-NR-VRS-OPR-ETRD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL083-NR-VRS-OPR-ETRD
                                               TO MCR-S9-15
                    MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL083-E-NR-SEQL-EXCO-PRFL'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL083-E-NR-SEQL-EXCO-PRFL
                                               TO MCR-S9-15
                    MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL083-CD-RGR-VLDC'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL083-CD-RGR-VLDC
                                               TO MCR-S9-15
                    MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL083-NR-VRS-RGR-VLDC'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL083-NR-VRS-RGR-VLDC
                                               TO MCR-S9-15
                    MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTSL095<<
               WHEN SGTSL095
                    MOVE  'SGTSL095-CD-TIP-SRVC'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL095-CD-TIP-SRVC        TO MCR-S9-15
                    MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL095-CD-CTRA-TIT-VL'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL095-CD-CTRA-TIT-VL
                                               TO MCR-S9-15
                    MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL095-CD-OPR-ETRD-MCP'
                                               TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                    IF  SGTSL095-CD-OPR-ETRD-MCP
                                               IS NUMERIC
                        MOVE  SGTSL095-CD-OPR-ETRD-MCP
                                               TO MCR-S9-15
                        MOVE  MCR-S9-15        TO MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE  'Valor não numérico'
                                               TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL095-NR-VRS-OPR-ETRD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL095-NR-VRS-OPR-ETRD
                                               TO MCR-S9-15
                    MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL095-E-NR-SEQL-EXCO-PRFL'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL095-E-NR-SEQL-EXCO-PRFL
                                               TO MCR-S9-15
                    MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL095-E-NR-SEQL-ITEM-EXCO'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL095-E-NR-SEQL-ITEM-EXCO
                                               TO MCR-S9-15
                    MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTSL117<<
               WHEN SGTSL117
                    MOVE  'SGTSL117-CD-TIP-SRVC'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL117-CD-TIP-SRVC TO MCR-S9-15
                    MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL117-CD-CTRA-TIT-VL'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL117-CD-CTRA-TIT-VL
                                               TO MCR-S9-15
                    MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL117-CD-OPR-ETRD-MCP'
                                               TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                    IF  SGTSL117-CD-OPR-ETRD-MCP
                                               IS NUMERIC
                        MOVE  SGTSL117-CD-OPR-ETRD-MCP
                                               TO MCR-S9-15
                        MOVE  MCR-S9-15        TO MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE  'Valor não numérico'
                                               TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL117-NR-VRS-OPR-ETRD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL117-NR-VRS-OPR-ETRD
                                               TO MCR-S9-15
                    MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
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
      *--> Monitorar variàveis do pgm, grava e encerra processamento

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

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

      *--> Adiciona as variáveis para monitoração
           MOVE 'SGTS0229-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0229-CD-CTRA-TIT-VL     TO MCR-S9-15
           MOVE  MCR-S9-15                   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTS0229-CD-OPR-ETRD-MCP'  TO MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF SGTS0229-CD-OPR-ETRD-MCP  NUMERIC
              MOVE SGTS0229-CD-OPR-ETRD-MCP  TO MCR-S9-15
              MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
           ELSE
              MOVE  'Valor não numérico'     TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0229-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0229-NR-VRS-OPR-ETRD    TO MCR-S9-15
           MOVE  MCR-S9-15                   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *--> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO

           EXEC CICS PUT
              CONTAINER ( CTNR-PRM-ERRO               )
              CHANNEL   ( NM-CNL                      )
              FROM      ( IIBKERRO-PRM-ERRO           )
              FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
              NOHANDLE
           END-EXEC

           PERFORM 999999-ENCERRAR
           .
       999200-SAI.
           EXIT.

      *---------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      *---------------------------------------*
-INC HLPKP999

      *---------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999

      *---------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
      *---------------------------------------*
-INC MSGKPMG1

      *---------------------------------------*
       999999-ENCERRAR                 SECTION.
      *---------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTS0229 ----------------------------