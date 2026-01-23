      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0072
      * ANALISTA..: F2054109 - Cintia Ap Soares Iglesias
      * AUTOR.....: F4535118 - Ivone de Melo Costa
      * DATA......: 03/10/2016
      * OBJETIVO..: Copiar Dados Vinculados da Operação Estruturada
      * Tipo de comunicação: Container
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS003 03/03/2023 F9158937 - Inclusão campo CD-FMA-APRC nos
      *        books da Condição Negociada SGTKI079 e SGTKL079
      * VRS002 26/07/2019 F2804421 DE 2016/145195 - Alterar valor do
      *                campo data de pesquisa na chamada da lista  de
      *                remuneração(SGTSL011) de 01.01.1900 p/01.01.0001.
      *        03/06/2019 F2804421 DE 2016/145195 - Inclusão GOBACK.
      *        11/04/2019 F2804421 DE 2016/145195 - Inclusão de movimen
      *                tação para campo SGTSL063-CD-ACAO (ação 1).
      *                - Acerto no 'initialize' do SGTSI063-DADOS.
000100*        14/02/2019 F8785286 DE 2016/145195 - Recompilado alterado
000100*              Book SGTSL006
      * VRS001 25/08/2017 F9188847  DE 2016/145195 - Recompilado por
      *                     alteração nos books: SGTKL068, SGTKI068,
      *                                          SGTKL079, SGTKI079
      *        26/07/2017 F8785286  DE 2016/145195 - Recompilado por
      *                    alteração no book SGTKL067.
      *        03/10/2016 F4535118  DE 2015/137233 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0072.
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

      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------

       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-SS               PIC  X(040)  VALUE
                                            '*** S.S. COMEÇA AQUI ***'.

       77  CTE-PROG                    PIC  X(018)  VALUE
                                            '*** SGTS0072 ***'.
       77  CTE-VERS                    PIC  X(006) VALUE 'VRS003'.

      *--> Modulos acionados
       77  SGTST006                    PIC  X(08) VALUE 'SGTST006'.
       77  SGTST011                    PIC  X(08) VALUE 'SGTST011'.
       77  SGTSI035                    PIC  X(08) VALUE 'SGTSI035'.
       77  SGTSI060                    PIC  X(08) VALUE 'SGTSI060'.
       77  SGTSI061                    PIC  X(08) VALUE 'SGTSI061'.
       77  SGTSI064                    PIC  X(08) VALUE 'SGTSI064'.
       77  SGTSL006                    PIC  X(08) VALUE 'SGTSL006'.
       77  SGTSL011                    PIC  X(08) VALUE 'SGTSL011'.
       77  SGTSLA35                    PIC  X(08) VALUE 'SGTSLA35'.
       77  SGTSL060                    PIC  X(08) VALUE 'SGTSL060'.
       77  SGTSL061                    PIC  X(08) VALUE 'SGTSL061'.
       77  SGTSL064                    PIC  X(08) VALUE 'SGTSL064'.
       77  SGTSL067                    PIC  X(08) VALUE 'SGTSL067'.
       77  SGTSI067                    PIC  X(08) VALUE 'SGTSI067'.
       77  SGTSL079                    PIC  X(08) VALUE 'SGTSL079'.
       77  SGTSI079                    PIC  X(08) VALUE 'SGTSI079'.
       77  SGTSL068                    PIC  X(08) VALUE 'SGTSL068'.
       77  SGTSI068                    PIC  X(08) VALUE 'SGTSI068'.
       77  SGTSL084                    PIC  X(08) VALUE 'SGTSL084'.
       77  SGTSI084                    PIC  X(08) VALUE 'SGTSI084'.
       77  SGTSL081                    PIC  X(08) VALUE 'SGTSL081'.
       77  SGTSI081                    PIC  X(08) VALUE 'SGTSI081'.
       77  SGTSL070                    PIC  X(08) VALUE 'SGTSL070'.
       77  SGTSI070                    PIC  X(08) VALUE 'SGTSI070'.
       77  SGTSL063                    PIC  X(08) VALUE 'SGTSL063'.
       77  SGTSI063                    PIC  X(08) VALUE 'SGTSI063'.
       77  SGTSL082                    PIC  X(08) VALUE 'SGTSL082'.
       77  SGTSI082                    PIC  X(08) VALUE 'SGTSI082'.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Areas de trabalho

       01  IC                          PIC S9(04) VALUE 0.
       01  IC-2                        PIC S9(04) VALUE 0.

       01  CD-CTRA-TIT                 PIC S9(09) COMP-5 VALUE 300000.

       01  AX-VL-4V4                   PIC --.--9,9999.

       01  AX-VL-4V10                  PIC --.--9,9999999999.

       01  AX-VL-15V2                  PIC ----.---.---.---.--9,99.

       01  AX-VL-13V4                  PIC --.---.---.---.--9,9999.

       01  AX-CD-TIP-CND-NEGD          PIC S9(04)  COMP-5 VALUE 0.
       01  AX-CD-TIP-TAXA              PIC S9(04)  COMP-5 VALUE 0.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de abend cics da bloco codigo HLPKP99999
-INC HLPKW999

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book do container de requisição e resposta deste modulo
       01 SGTS0072-DADOS.
-INC SGTK0072

      *--> CRUD CLI_OPR_ETRD
       01 SGTST006-DADOS.
-INC SGTKT006

      *--> CRUD RMNC_OPR_ETRD
       01 SGTST011-DADOS.
-INC SGTKT011

      *--> IN_OPR_ETRD
-INC SGTKL063
-INC SGTKI063

      *--> NOTA_TCN_VCLD_OPR
-INC SGTKL067
-INC SGTKI067
      *--> PREF_TAXA_OPR
-INC SGTKL068
-INC SGTKI068

-INC SGTKL084
-INC SGTKI084

-INC SGTKL081
-INC SGTKI081

-INC SGTKL082
-INC SGTKI082

      *--> CND_NEGD_OPR
-INC SGTKL079

       01 SGTSI079-DADOS.
-INC SGTKI079

      *-->
       01 SGTSL070-DADOS.
-INC SGTKL070

       01 SGTSI070-DADOS.
-INC SGTKI070

      *--> CRUD CTT_EXNO_OPR_ETRD
-INC SGTKI035

      *--> INCLUSÃO  - ANX_OPR_ETRD
-INC SGTKI060

      *--> INCLUSÃO  - RPAC_PGMD
-INC SGTKI061

      * INCLUSÃO  - BCO_PCT_OPR
-INC SGTKI064

      * LISTAR - CLI_OPR_ETRD
       01 SGTSL006-DADOS.
-INC SGTKL006

      * LISTAR - RMNC_OPR_ETRD
       01 SGTSL011-DADOS.
-INC SGTKL011

      * LISTAR - CTT_EXN0_OPR_ETRD
-INC SGTKLA35

      * LISTAR - ANX_OPR_ETRD
-INC SGTKL060

      * LISTAR - RPAC-PGMD
-INC SGTKL061

      * LISTAR - BCO-PCT-OPR
-INC SGTKL064

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book dos dados do usuário
-INC IIBKOPRD

       77  CTE-FINAL-SS                  PIC X(40)      VALUE
                    '          *** S.S. TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

           PERFORM 100000-BUSCA-REQUISICAO

           PERFORM 200000-PROCESSAR

           PERFORM 999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-BUSCA-REQUISICAO         SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

      *--> Ativa a captura automática de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

      *--> Define o nome do programa p/ fins manipulação container
           MOVE  CTE-PROG(5:8)         TO  NM-PGM
           MOVE  'SGTS0072-RPST'       TO  NM-CTNR

           PERFORM 970000-LER-CANAL

      *--> Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

           PERFORM 974000-LER-DADOS-OPRD

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *--> Valida campo a campo da requisicao
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTS0072-CD-OPR-ETRD-OGM NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0052              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação origem não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               SET   MNT-VRV           TO  TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0072-CD-OPR-ETRD-OGM <= ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0053              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação origem inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET   MNT-VRV           TO  TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0072-NR-VRS-OPR-OGM  <= ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0054              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação origem inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET   MNT-VRV           TO  TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0072-CD-OPR-ETRD-DST NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0055              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação destino não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               SET   MNT-VRV           TO  TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0072-CD-OPR-ETRD-DST <= ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0056              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação destino inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET   MNT-VRV           TO  TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0072-NR-VRS-OPR-DST  <= ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0057              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação destino inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET   MNT-VRV           TO  TRUE
               PERFORM 999000-ERRO
           END-IF
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------

           PERFORM 210000-LISTAR-REMUNERACAO

           PERFORM 220000-LISTAR-CLIENTE

           PERFORM 230000-LISTAR-CONTATO

           PERFORM 240000-LISTAR-REPACTUACAO

           PERFORM 250000-LISTAR-ANEXOS

           PERFORM 260000-LISTAR-BANCO

           PERFORM 270000-LISTAR-NOTA-TCN-OPR

           MOVE  0002                  TO  AX-CD-TIP-CND-NEGD
           PERFORM 280000-LISTAR-CND-NEGD-OPR

           MOVE  0003                  TO  AX-CD-TIP-CND-NEGD
           PERFORM 280000-LISTAR-CND-NEGD-OPR

           MOVE  0001                  TO  AX-CD-TIP-TAXA
           PERFORM 290000-LISTAR-PREF-TAXA

           MOVE  0002                  TO  AX-CD-TIP-TAXA
           PERFORM 290000-LISTAR-PREF-TAXA

           PERFORM 300000-LISTAR-TARF-OPR

           PERFORM 310000-LISTAR-PRV-CMPS-GRT

           PERFORM 320000-LISTAR-RCBT-PRVT

           PERFORM 330000-LISTAR-IND-OPR

           PERFORM 980000-GRAVAR-CONTAINER-RPST
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-LISTAR-REMUNERACAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL011-RQSC.

      *    Movimentar o nome do programa a ser executado
           MOVE  SGTSL011              TO  NM-PGM

      *    Movimentar os campos de entrada

           MOVE  SGTS0072-CD-CTRA-OGM  TO  SGTSL011-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-OGM
                                       TO  SGTSL011-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-OGM
                                       TO  SGTSL011-NR-VRS-OPR-ETRD

           MOVE  '01010001'            TO  SGTSL011-DT-INC-TAXA-ULT

           PERFORM  900000-EXECUTAR-MODULO

           PERFORM  510000-GRAVAR-REMUNERACAO
                    VARYING IC FROM 1 BY 1
                    UNTIL   IC GREATER SGTSL011-QT-OCR-TAB

           IF SGTSL011-TEM-MAIS-REGISTRO
              PERFORM UNTIL NOT SGTSL011-TEM-MAIS-REGISTRO
                 MOVE  SGTSL011-DT-INC-TAXA-RMNC(SGTSL011-QT-OCR-TAB)
                                       TO  SGTSL011-DT-INC-TAXA-ULT
                 MOVE  SGTSL011        TO  NM-PGM
                 PERFORM  900000-EXECUTAR-MODULO

                 PERFORM  510000-GRAVAR-REMUNERACAO
                               VARYING IC FROM 1 BY 1
                               UNTIL   IC GREATER SGTSL011-QT-OCR-TAB
              END-PERFORM
           END-IF
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-LISTAR-CLIENTE       SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL006-DADOS.

      *    Movimentar o nome do programa a ser executado
           MOVE  SGTSL006              TO  NM-PGM

      *    Movimentar os campos de entrada
      *    Listar por cliente
           MOVE  0003                  TO  SGTSL006-ACAO
           MOVE  SGTS0072-CD-CTRA-OGM  TO  SGTSL006-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-OGM
                                       TO  SGTSL006-ULT-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-OGM
                                       TO  SGTSL006-ULT-VRS-OPR-ETRD
      *--> 6 - Contato Interno
           MOVE  0006                  TO  SGTSL006-CD-TIP-FUC-CLI-E

           MOVE  ZEROS                 TO  SGTSL006-ULT-CD-CLI
                                           SGTSL006-ULT-TIP-FUC-CLI

           PERFORM  920000-CHAMAR-SGTSL006

           PERFORM  520000-GRAVAR-CLIENTE
                    VARYING IC FROM 1 BY 1
                    UNTIL   IC GREATER SGTSL006-QT-OCR-TAB

           IF SGTSL006-TEM-MAIS-REGISTRO
              PERFORM UNTIL NOT SGTSL006-TEM-MAIS-REGISTRO
                 MOVE SGTSL006-CD-TIP-FUC-CLI(SGTSL006-QT-OCR-TAB)
                                       TO  SGTSL006-ULT-TIP-FUC-CLI
                 MOVE SGTSL006-CD-CLI(SGTSL006-QT-OCR-TAB)
                                       TO  SGTSL006-ULT-CD-CLI

                 MOVE  SGTSL006        TO  NM-PGM
                 PERFORM  920000-CHAMAR-SGTSL006

                 PERFORM  520000-GRAVAR-CLIENTE
                               VARYING IC FROM 1 BY 1
                               UNTIL   IC GREATER SGTSL006-QT-OCR-TAB
              END-PERFORM
           END-IF


           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-LISTAR-CONTATO      SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSLA35-RQSC.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSLA35              TO  NM-PGM

      *--> Movimentar os campos de entrada

           MOVE  SGTS0072-CD-CTRA-OGM  TO  SGTSLA35-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-OGM
                                       TO  SGTSLA35-CD-OPR-ETRD-MC
           MOVE  SGTS0072-NR-VRS-OPR-OGM
                                       TO  SGTSLA35-NR-VRS-OPR-ETRD
           MOVE  0                     TO  SGTSLA35-CD-PSS-CTT-ULT

           PERFORM  900000-EXECUTAR-MODULO

           PERFORM  530000-GRAVAR-CONTATO
                    VARYING IC  FROM   1 BY 1
                    UNTIL   IC  GREATER SGTSLA35-QT-OCR-TAB

           IF SGTSLA35-TEM-MAIS-REGISTRO
              PERFORM UNTIL NOT SGTSLA35-TEM-MAIS-REGISTRO
                MOVE  SGTSLA35-CD-PSS-CTT-EXNO(SGTSLA35-QT-OCR-TAB)
                                       TO  SGTSLA35-CD-PSS-CTT-ULT

                MOVE  SGTSLA35         TO  NM-PGM
                PERFORM  900000-EXECUTAR-MODULO

                PERFORM  530000-GRAVAR-CONTATO
                    VARYING IC  FROM   1 BY 1
                    UNTIL   IC  GREATER SGTSLA35-QT-OCR-TAB
              END-PERFORM
           END-IF
           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       240000-LISTAR-REPACTUACAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL061-RQSC.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSL061              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0072-CD-CTRA-OGM  TO  SGTSL061-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-OGM
                                       TO  SGTSL061-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-OGM
                                       TO  SGTSL061-NR-VRS-OPR-ETRD
           MOVE  '01011900'            TO  SGTSL061-DT-RPAC-ULT

           PERFORM  900000-EXECUTAR-MODULO

           PERFORM  540000-GRAVAR-REPACTUACAO
                    VARYING IC             FROM 0001 BY 0001
                    UNTIL   IC             GREATER SGTSL061-QT-OCR-TAB

           IF SGTSL061-TEM-MAIS-REGISTRO
              PERFORM UNTIL NOT SGTSL061-TEM-MAIS-REGISTRO
                MOVE  CORR SGTSL061-DT-RPAC-PGMD-R(SGTSL061-QT-OCR-TAB)
                                       TO  SGTSL061-DT-RPAC-ULT-R
                MOVE  SGTSL061         TO  NM-PGM
                PERFORM  900000-EXECUTAR-MODULO

                PERFORM  540000-GRAVAR-REPACTUACAO
                            VARYING IC     FROM 0001 BY 0001
                            UNTIL   IC     GREATER SGTSL061-QT-OCR-TAB
              END-PERFORM
           END-IF
           .
       240000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       250000-LISTAR-ANEXOS            SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL060-RQSC.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSL060              TO  NM-PGM

           MOVE  SGTS0072-CD-CTRA-OGM  TO  SGTSL060-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-OGM
                                       TO  SGTSL060-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-OGM
                                       TO  SGTSL060-NR-VRS-OPR-ETRD
           MOVE  ZEROS                 TO  SGTSL060-CD-ARQ-OPR-ULT

           PERFORM  900000-EXECUTAR-MODULO

           PERFORM  550000-GRAVAR-ANEXOS
                    VARYING IC             FROM 0001 BY 0001
                    UNTIL   IC             GREATER SGTSL060-QT-OCR-TAB

           IF SGTSL060-TEM-MAIS-REGISTRO
              PERFORM UNTIL NOT SGTSL060-TEM-MAIS-REGISTRO
                 MOVE SGTSL060-CD-ARQ-OPR-ETRD (SGTSL060-QT-OCR-TAB)
                                       TO  SGTSL060-CD-ARQ-OPR-ULT

                 MOVE SGTSL060         TO  NM-PGM
                 PERFORM  900000-EXECUTAR-MODULO

                 PERFORM  550000-GRAVAR-ANEXOS
                           VARYING IC      FROM 0001 BY 0001
                           UNTIL   IC      GREATER SGTSL060-QT-OCR-TAB
              END-PERFORM
           END-IF
           .
       250000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       260000-LISTAR-BANCO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL064-RQSC.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSL064              TO  NM-PGM

           MOVE  SGTS0072-CD-CTRA-OGM  TO  SGTSL064-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-OGM
                                       TO  SGTSL064-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-OGM
                                       TO  SGTSL064-NR-VRS-OPR-ETRD
           MOVE  0001                  TO  SGTSL064-CD-TIP-PRTC
           MOVE  ZEROS                 TO  SGTSL064-CD-ULT-BCO-PCT

           PERFORM  900000-EXECUTAR-MODULO

           PERFORM  560000-GRAVAR-BCO
                    VARYING IC FROM 1 BY 1
                    UNTIL   IC GREATER SGTSL064-QT-OCR-TAB

           IF  SGTSL064-TEM-MAIS-REGISTRO
               PERFORM UNTIL NOT SGTSL064-TEM-MAIS-REGISTRO
                  MOVE  SGTSL064-CD-BCO-PCT(SGTSL064-QT-OCR-TAB)
                                       TO  SGTSL064-CD-ULT-BCO-PCT
                  MOVE  SGTSL064       TO  NM-PGM
                  PERFORM  900000-EXECUTAR-MODULO

                  PERFORM  560000-GRAVAR-BCO
                           VARYING IC      FROM 0001 BY 0001
                           UNTIL   IC      GREATER SGTSL064-QT-OCR-TAB
              END-PERFORM
           END-IF
           .
       260000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       270000-LISTAR-NOTA-TCN-OPR        SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL067-RQSC.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSL067              TO  NM-PGM

      *--> Toda nota técnica vinculada à operação
           MOVE  0002                  TO  SGTSL067-CD-TIP-SRVC

           MOVE  SGTS0072-CD-CTRA-OGM  TO
                                           SGTSL067-E-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-OGM
                                       TO  SGTSL067-E-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-OGM
                                       TO  SGTSL067-E-NR-VRS-OPR-ETRD

           MOVE  ZEROS                 TO  SGTSL067-E-NR-NOTA-TCN-OPR

           PERFORM  900000-EXECUTAR-MODULO

           PERFORM  570000-GRAVAR-NOTA-TCN-OPR
                    VARYING IC             FROM 0001 BY 0001
                    UNTIL   IC             GREATER SGTSL067-QT-OCR-TAB

           IF  SGTSL067-TEM-MAIS-REGISTRO
               PERFORM UNTIL NOT SGTSL067-TEM-MAIS-REGISTRO

                  MOVE  SGTSL067-NR-NOTA-TCN-OPR(SGTSL067-QT-OCR-TAB)
                                       TO  SGTSL067-E-NR-NOTA-TCN-OPR

                  MOVE SGTSL067        TO  NM-PGM
                  PERFORM  900000-EXECUTAR-MODULO

                  PERFORM  570000-GRAVAR-NOTA-TCN-OPR
                           VARYING IC      FROM 0001 BY 0001
                           UNTIL   IC      GREATER SGTSL067-QT-OCR-TAB
              END-PERFORM
           END-IF
           .
       270000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       280000-LISTAR-CND-NEGD-OPR        SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL079-RQSC.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSL079              TO  NM-PGM

           MOVE  SGTS0072-CD-CTRA-OGM  TO  SGTSL079-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-OGM
                                       TO  SGTSL079-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-OGM
                                       TO  SGTSL079-NR-VRS-OPR-ETRD

           MOVE   AX-CD-TIP-CND-NEGD   TO  SGTSL079-CD-TIP-CND-NEGD
                                           SGTSL079-CD-ULT-TIP-CND-NEGD

           MOVE  '01011900'            TO  SGTSL079-DT-INC-VLD-CND

           PERFORM  900000-EXECUTAR-MODULO

           PERFORM  580000-CND-NEGD-OPR
                    VARYING IC FROM 1 BY 1
                    UNTIL   IC GREATER SGTSL079-QT-OCR-TAB

           IF  SGTSL079-TEM-MAIS-REGISTRO
               PERFORM UNTIL NOT SGTSL079-TEM-MAIS-REGISTRO
                   MOVE  CORR
                     SGTSL079-DT-INC-VLD-CND-S-R(SGTSL079-QT-OCR-TAB)
                                       TO  SGTSL079-DT-INC-VLD-CND-R

                   MOVE  SGTSL079      TO  NM-PGM
                   PERFORM  900000-EXECUTAR-MODULO

                   PERFORM  580000-CND-NEGD-OPR
                            VARYING IC     FROM 0001 BY 0001
                            UNTIL   IC     GREATER SGTSL079-QT-OCR-TAB
               END-PERFORM
           END-IF
           .
       280000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       290000-LISTAR-PREF-TAXA       SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL068-RQSC.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSL068              TO  NM-PGM

           MOVE  SGTS0072-CD-CTRA-OGM  TO  SGTSL068-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-OGM
                                       TO  SGTSL068-CD-OPR-ETRD-MCP
           MOVE SGTS0072-NR-VRS-OPR-OGM
                                       TO  SGTSL068-NR-VRS-OPR-ETRD
           MOVE  AX-CD-TIP-TAXA        TO  SGTSL068-CD-TIP-TAXA
           MOVE  '01011900'            TO  SGTSL068-DT-ULT-INC-VLD

           PERFORM  900000-EXECUTAR-MODULO

           PERFORM  590000-PREF-TAXA
                    VARYING IC FROM 1 BY 1
                    UNTIL   IC GREATER SGTSL068-QT-OCR-TAB

           IF  SGTSL068-TEM-MAIS-REGISTRO
               PERFORM UNTIL NOT SGTSL068-TEM-MAIS-REGISTRO
                  MOVE CORR
                     SGTSL068-DT-INC-CND-TAXA-R(SGTSL068-QT-OCR-TAB)
                                       TO  SGTSL068-DT-ULT-INC-VLD-R

                  MOVE  SGTSL068       TO  NM-PGM
                  PERFORM  900000-EXECUTAR-MODULO

                  PERFORM  590000-PREF-TAXA
                           VARYING IC      FROM 0001 BY 0001
                           UNTIL   IC      GREATER SGTSL068-QT-OCR-TAB
              END-PERFORM
           END-IF
           .
       290000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-LISTAR-TARF-OPR       SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL084-RQSC.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSL084              TO  NM-PGM

           MOVE  SGTS0072-CD-CTRA-OGM  TO  SGTSL084-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-OGM
                                       TO  SGTSL084-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-OGM
                                       TO  SGTSL084-NR-VRS-OPR-ETRD
           MOVE  0                     TO  SGTSL084-CD-TIP-TARF-E

           PERFORM  900000-EXECUTAR-MODULO

           PERFORM  600000-TARF-OPR
                    VARYING IC             FROM 0001 BY 0001
                    UNTIL   IC             GREATER SGTSL084-QT-OCR-TAB

           IF  SGTSL084-TEM-MAIS-REGISTRO
               PERFORM UNTIL NOT SGTSL084-TEM-MAIS-REGISTRO
                  MOVE SGTSL084-CD-TIP-TARF(SGTSL084-QT-OCR-TAB)
                                       TO  SGTSL084-CD-TIP-TARF-E

                  MOVE  SGTSL084       TO  NM-PGM
                  PERFORM  900000-EXECUTAR-MODULO

                  PERFORM  600000-TARF-OPR
                           VARYING IC      FROM 0001 BY 0001
                           UNTIL   IC      GREATER SGTSL084-QT-OCR-TAB
               END-PERFORM
           END-IF
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       310000-LISTAR-PRV-CMPS-GRT       SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL081-RQSC.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSL081              TO  NM-PGM

           MOVE SGTS0072-CD-CTRA-OGM   TO
                                            SGTSL081-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-OGM
                                       TO  SGTSL081-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-OGM
                                       TO  SGTSL081-NR-VRS-OPR-ETRD
           MOVE  ZEROS                 TO  SGTSL081-NR-ULT-PRV-CMPS-GRT

           PERFORM  900000-EXECUTAR-MODULO

           PERFORM  311000-PRV-CMPS-GRT
                    VARYING IC FROM 1 BY 1
                    UNTIL   IC GREATER SGTSL081-QT-OCR-TAB

           IF  SGTSL081-TEM-MAIS-REGISTRO
               PERFORM UNTIL NOT SGTSL081-TEM-MAIS-REGISTRO
                  MOVE SGTS0072-CD-CTRA-OGM   TO
                                            SGTSL081-CD-CTRA-TIT-VL
                  MOVE  SGTS0072-CD-OPR-ETRD-OGM
                                       TO  SGTSL081-CD-OPR-ETRD-MCP
                  MOVE  SGTS0072-NR-VRS-OPR-OGM
                                       TO  SGTSL081-NR-VRS-OPR-ETRD
                  MOVE  SGTSL081-NR-PRV-CMPS-GRT(SGTSL081-QT-OCR-TAB)
                                       TO  SGTSL081-NR-ULT-PRV-CMPS-GRT

                  MOVE  SGTSL081       TO  NM-PGM
                  PERFORM  900000-EXECUTAR-MODULO

                  PERFORM  311000-PRV-CMPS-GRT
                           VARYING IC FROM 1 BY 1
                           UNTIL   IC GREATER SGTSL081-QT-OCR-TAB
               END-PERFORM
           END-IF
           .
       310000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       311000-PRV-CMPS-GRT         SECTION.
      *-----------------------------------------------------------------
           MOVE  0041                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                   SGTSI081-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSI081              TO  NM-PGM

           MOVE  SGTS0072-CD-CTRA-DST  TO  SGTSI081-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-DST
                                       TO  SGTSI081-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-DST
                                       TO  SGTSI081-NR-VRS-OPR-ETRD
           MOVE  SGTSL081-CD-NTZ-GRT(IC)
                                       TO  SGTSI081-CD-NTZ-GRT
           MOVE  SGTSL081-CD-FMA-VCLC(IC)
                                       TO  SGTSI081-CD-FMA-VCLC
           MOVE  SGTSL081-DT-LIM-GRT(IC)
                                       TO  SGTSI081-DT-LIM-GRT
           MOVE  SGTSL081-PC-GRT-CMPS-PRVT(IC)
                                       TO  SGTSI081-PC-GRT-CMPS-PRVT
           MOVE  SGTSL081-VL-GRT-CMPS-PRVT(IC)
                                       TO  SGTSI081-VL-GRT-CMPS-PRVT

           MOVE  KOPRD-CD-OPRD         TO  SGTSI081-CD-USU-ULT-ATL
           MOVE  'CURRENT'             TO  SGTSI081-TS-ULT-ATL

           PERFORM 900000-EXECUTAR-MODULO

           PERFORM 311100-LISTAR-CLI-GRTR
           .
       311000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       311100-LISTAR-CLI-GRTR       SECTION.
      *-----------------------------------------------------------------
           MOVE  0042                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL081-RQSC.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSL082              TO  NM-PGM

           SET SGTSL082-LISTA-TODOS  TO TRUE

           MOVE SGTS0072-CD-CTRA-OGM   TO
                                            SGTSL082-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-OGM
                                       TO  SGTSL082-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-OGM
                                       TO  SGTSL082-NR-VRS-OPR-ETRD
           MOVE  SGTSL081-NR-PRV-CMPS-GRT(IC) TO
                                             SGTSL082-NR-PRV-CMPS-GRT
           MOVE  ZEROS                 TO  SGTSL082-CD-ULT-CLI

           PERFORM  900000-EXECUTAR-MODULO

           PERFORM  311110-CLI-GRTR
                    VARYING IC-2 FROM 1 BY 1
                    UNTIL   IC-2 GREATER SGTSL082-QT-OCR-TAB

           IF  SGTSL082-TEM-MAIS-REGISTRO
               PERFORM UNTIL NOT SGTSL082-TEM-MAIS-REGISTRO

                  MOVE SGTSL082-CD-CLI(SGTSL082-QT-OCR-TAB)
                                       TO  SGTSL082-CD-ULT-CLI
                  MOVE  SGTSL082       TO  NM-PGM
                  PERFORM  900000-EXECUTAR-MODULO

                  PERFORM  311110-CLI-GRTR
                           VARYING IC-2 FROM 1 BY 1
                           UNTIL   IC-2 GREATER SGTSL082-QT-OCR-TAB
               END-PERFORM
           END-IF
           .
       311100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       311110-CLI-GRTR        SECTION.
      *-----------------------------------------------------------------
           MOVE  0043                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                   SGTSI082-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSI082              TO  NM-PGM

           MOVE  SGTS0072-CD-CTRA-DST  TO  SGTSI082-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-DST
                                       TO  SGTSI082-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-DST
                                       TO  SGTSI082-NR-VRS-OPR-ETRD
           MOVE  SGTSI081-NR-PRV-CMPS-GRT
                                       TO  SGTSI082-NR-PRV-CMPS-GRT
           MOVE  SGTSL082-CD-CLI(IC-2)
                                       TO  SGTSI082-CD-CLI

           PERFORM 900000-EXECUTAR-MODULO
           .
       311110-SAI.
           EXIT.

      *-----------------------------------------------------------------
       320000-LISTAR-RCBT-PRVT       SECTION.
      *-----------------------------------------------------------------
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL070-RQSC.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSL070              TO  NM-PGM

           SET   SGTSL070-LISTA-ULT-NR-SEQL
                                       TO  TRUE

           MOVE  SGTS0072-CD-CTRA-OGM  TO  SGTSL070-E-CD-CTRA-TIT-VL

           MOVE  SGTS0072-CD-OPR-ETRD-OGM
                                       TO  SGTSL070-E-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-OGM
                                       TO  SGTSL070-E-NR-VRS-OPR-ETRD

           MOVE  0                     TO  SGTSL070-CD-TIP-RCBT-PRVT-ULT
           MOVE  '01011900'            TO  SGTSL070-DT-RCBT-PRVT-ULT

           PERFORM  900000-EXECUTAR-MODULO

           PERFORM  620000-RCBT-PRVT
                    VARYING IC             FROM 0001 BY 0001
                    UNTIL   IC             GREATER  SGTSL070-QT-OCR

           IF  SGTSL070-TEM-MAIS-REGISTRO
               PERFORM UNTIL NOT SGTSL070-TEM-MAIS-REGISTRO
                  MOVE  SGTSL070-CD-TIP-RCBT-PRVT(SGTSL070-QT-OCR)
                                       TO  SGTSL070-CD-TIP-RCBT-PRVT-ULT
                  MOVE  SGTSL070-DT-RCBT-PRVT(SGTSL070-QT-OCR)
                                       TO  SGTSL070-DT-RCBT-PRVT-ULT

                  MOVE  SGTSL070       TO  NM-PGM
                  PERFORM  900000-EXECUTAR-MODULO

                  PERFORM  620000-RCBT-PRVT
                           VARYING IC FROM  0001 BY 0001
                           UNTIL   IC GREATER SGTSL070-QT-OCR
               END-PERFORM
           END-IF
           .
       320000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       330000-LISTAR-IND-OPR      SECTION.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL063-RQSC.

      *--> Movimentar o nome do programa a ser executado
      *    Ação = 2 lista todos os indicadores vigentes.

           MOVE  SGTSL063               TO  NM-PGM

           MOVE 1                      TO  SGTSL063-CD-ACAO

           MOVE  SGTS0072-CD-CTRA-OGM  TO  SGTSL063-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-OGM
                                       TO  SGTSL063-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-OGM
                                       TO  SGTSL063-NR-VRS-OPR-ETRD
           MOVE  0                     TO  SGTSL063-CD-TIP-IN-E

           PERFORM  900000-EXECUTAR-MODULO

           PERFORM  630000-IND-OPR
                    VARYING IC             FROM 0001 BY 0001
                    UNTIL   IC             GREATER  SGTSL063-QT-OCR-TAB

           IF  SGTSL063-TEM-MAIS-REGISTRO
               PERFORM UNTIL NOT SGTSL063-TEM-MAIS-REGISTRO
                    MOVE  SGTSL063-CD-TIP-IN(SGTSL063-QT-OCR-TAB)
                                       TO  SGTSI063-CD-TIP-IN

                    MOVE SGTSL063      TO  NM-PGM
                    PERFORM  900000-EXECUTAR-MODULO

                    PERFORM  630000-IND-OPR
                             VARYING IC    FROM 0001 BY 0001
                             UNTIL   IC    GREATER SGTSL063-QT-OCR-TAB
              END-PERFORM
           END-IF
           .
       330000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       510000-GRAVAR-REMUNERACAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTST011-AREA-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTST011              TO  NM-PGM

           SET   SGTST011-INCLUIR      TO  TRUE

           MOVE  SGTS0072-CD-CTRA-DST  TO  SGTST011-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-DST
                                       TO  SGTST011-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-DST
                                       TO  SGTST011-NR-VRS-OPR-ETRD
           MOVE  '  .  .    '          TO  SGTST011-DT-INC-TAXA-RMNC
           MOVE  CORR SGTSL011-DT-INC-TAXA-RMNC-R (IC)
                                       TO  SGTST011-DT-INC-TAXA-RMNC-R
           MOVE  'CURRENT'             TO  SGTST011-TS-ULT-ATL
           MOVE  KOPRD-CD-OPRD         TO  SGTST011-CD-USU-RSP-ATL
           MOVE  SGTSL011-PC-TAXA-RMNC-OPR (IC)
                                       TO  SGTST011-PC-TAXA-RMNC-OPR
           MOVE  '  .  .    '          TO  SGTST011-DT-FIM-TAXA-RMNC
           MOVE  CORR SGTSL011-DT-FIM-TAXA-RMNC-R (IC)
                                       TO  SGTST011-DT-FIM-TAXA-RMNC-R

           PERFORM 910000-CHAMAR-SGTST011
           .
       510000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       520000-GRAVAR-CLIENTE           SECTION.
      *-----------------------------------------------------------------
           MOVE  0017                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTST006-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTST006              TO  NM-PGM

           SET   SGTST006-INCLUIR      TO  TRUE

           MOVE  SGTS0072-CD-CTRA-DST  TO  SGTST006-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-DST
                                       TO  SGTST006-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-DST
                                       TO  SGTST006-NR-VRS-OPR-ETRD
           MOVE  SGTSL006-CD-CLI(IC)   TO  SGTST006-CD-CLI
      *--> Alterar cliente
           MOVE  0006                  TO  SGTST006-CD-TIP-FUC-CLI
           MOVE  KOPRD-CD-OPRD         TO  SGTST006-CD-USU-RSP-ATL
           MOVE  'CURRENT'             TO  SGTST006-TS-ULT-ATL

           PERFORM 921000-CHAMAR-SGTST006
           .
       520000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       530000-GRAVAR-CONTATO           SECTION.
      *-----------------------------------------------------------------
           MOVE  0018                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSI035-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSI035              TO  NM-PGM

           MOVE  SGTS0072-CD-CTRA-DST  TO  SGTSI035-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-DST
                                       TO  SGTSI035-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-DST
                                       TO  SGTSI035-NR-VRS-OPR-ETRD
           MOVE  SGTSLA35-CD-PSS-CTT-EXNO(IC)
                                       TO  SGTSI035-CD-PSS-CTT-EXNO
           MOVE  KOPRD-CD-OPRD         TO  SGTSI035-CD-USU-RSP-ATL
           MOVE  'CURRENT'             TO  SGTSI035-TS-ATL-CTT-EXNO

           PERFORM 900000-EXECUTAR-MODULO
           .
       530000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       550000-GRAVAR-ANEXOS            SECTION.
      *-----------------------------------------------------------------
           MOVE  0019                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSI060-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSI060              TO  NM-PGM

           MOVE  SGTS0072-CD-CTRA-DST  TO  SGTSI060-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-DST
                                       TO  SGTSI060-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-DST
                                       TO  SGTSI060-NR-VRS-OPR-ETRD
           MOVE  SGTSL060-CD-ARQ-OPR-ETRD (IC)
                                       TO  SGTSI060-CD-ARQ-OPR-ETRD
           MOVE  KOPRD-CD-OPRD         TO  SGTSI060-CD-USU-ULT-ATL
           MOVE  'CURRENT'             TO  SGTSI060-TS-ULT-ATL
           MOVE  SGTSL060-TX-DCR-ANX(IC)
                                       TO  SGTSI060-TX-DCR-ANX

           PERFORM 900000-EXECUTAR-MODULO
           .
       550000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       540000-GRAVAR-REPACTUACAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0020                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSI061-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSI061              TO  NM-PGM

           MOVE  SGTS0072-CD-CTRA-DST  TO  SGTSI061-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-DST
                                       TO  SGTSI061-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-DST
                                       TO  SGTSI061-NR-VRS-OPR-ETRD
           MOVE  '  .  .    '          TO  SGTSI061-DT-RPAC-PGMD
           MOVE  CORR SGTSL061-DT-RPAC-PGMD-R(IC)
                                       TO  SGTSI061-DT-RPAC-PGMD-R
           MOVE  KOPRD-CD-OPRD         TO  SGTSI061-CD-USU-ULT-ATL
           MOVE  'CURRENT'             TO  SGTSI061-TS-ULT-ATL
           MOVE  SGTSL061-TX-MTV-RPAC-PGMD(IC)
                                       TO  SGTSI061-TX-MTV-RPAC-PGMD

           PERFORM 900000-EXECUTAR-MODULO
           .
       540000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       560000-GRAVAR-BCO               SECTION.
      *-----------------------------------------------------------------
           MOVE  0021                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                   SGTSI064-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSI064              TO  NM-PGM

           MOVE  SGTS0072-CD-CTRA-DST  TO  SGTSI064-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-DST
                                       TO  SGTSI064-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-DST
                                       TO  SGTSI064-NR-VRS-OPR-ETRD
           MOVE  SGTSL064-CD-BCO-PCT(IC)
                                       TO  SGTSI064-CD-BCO-PCT
           MOVE  0001                  TO  SGTSI064-CD-TIP-PRTC
           MOVE  SGTSL064-IN-BCO-LDR(IC)
                                       TO  SGTSI064-IN-BCO-LDR
           MOVE  SGTSL064-PC-PRTC-BCO(IC)
                                       TO  SGTSI064-PC-PRTC-BCO
           MOVE  KOPRD-CD-OPRD         TO  SGTSI064-CD-USU-ULT-ATL
           MOVE  'CURRENT'             TO  SGTSI064-TS-ULT-ATL

           PERFORM 900000-EXECUTAR-MODULO
           .
       560000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       570000-GRAVAR-NOTA-TCN-OPR               SECTION.
      *-----------------------------------------------------------------
           MOVE  0022                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                   SGTSI067-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSI067              TO  NM-PGM

           MOVE  SGTS0072-CD-CTRA-DST  TO  SGTSI067-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-DST
                                       TO  SGTSI067-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-DST
                                       TO  SGTSI067-NR-VRS-OPR-ETRD
           MOVE  SGTSL067-NR-NOTA-TCN-OPR(IC)
                                       TO  SGTSI067-NR-NOTA-TCN-OPR

           MOVE  KOPRD-CD-OPRD         TO  SGTSI067-CD-USU-ULT-ATL
           MOVE  'CURRENT'             TO  SGTSI067-TS-INCL-NOTA-TCN

           PERFORM 900000-EXECUTAR-MODULO
           .
       570000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       580000-CND-NEGD-OPR         SECTION.
      *-----------------------------------------------------------------
           MOVE  0023                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                   SGTSI079-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSI079              TO  NM-PGM

           MOVE  SGTS0072-CD-CTRA-DST  TO  SGTSI079-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-DST
                                       TO  SGTSI079-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-DST
                                       TO  SGTSI079-NR-VRS-OPR-ETRD
           MOVE  SGTSL079-CD-TIP-CND-NEGD-S(IC)
                                       TO  SGTSI079-CD-TIP-CND-NEGD
           MOVE  SGTSL079-CD-TIP-CBT-S(IC)
                                       TO  SGTSI079-CD-TIP-CBT
           MOVE  SGTSL079-IN-OBGD-CND-S(IC)
                                       TO  SGTSI079-IN-OBGD-CND
           MOVE  SGTSL079-PC-TAXA-QBRA-FNDO-S(IC)
                                       TO  SGTSI079-PC-TAXA-QBRA-FNDO
           MOVE  SGTSL079-DT-INC-VLD-CND-S(IC)
                                       TO  SGTSI079-DT-INC-VLD-CND
           MOVE  SGTSL079-DT-FIM-VLD-CND-S(IC)
                                       TO  SGTSI079-DT-FIM-VLD-CND
           MOVE  SGTSL079-CD-FMA-APRC-S(IC)
                                       TO  SGTSI079-CD-FMA-APRC

           PERFORM 900000-EXECUTAR-MODULO
           .
       580000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       590000-PREF-TAXA         SECTION.
      *-----------------------------------------------------------------
           MOVE  0024                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                   SGTSI068-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSI068              TO  NM-PGM

           MOVE  SGTS0072-CD-CTRA-DST  TO  SGTSI068-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-DST
                                       TO  SGTSI068-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-DST
                                       TO  SGTSI068-NR-VRS-OPR-ETRD
           MOVE  AX-CD-TIP-TAXA        TO  SGTSI068-CD-TIP-TAXA
           MOVE  '  .  .    '          TO  SGTSI068-DT-INC-CND-TAXA-R
           MOVE  CORR SGTSL068-DT-INC-CND-TAXA-R(IC)
                                       TO  SGTSI068-DT-INC-CND-TAXA-R
           MOVE  '  .  .    '          TO  SGTSI068-DT-FIM-CND-TAXA-R
           MOVE  CORR SGTSL068-DT-FIM-CND-TAXA-R(IC)
                                       TO  SGTSI068-DT-FIM-CND-TAXA-R
           MOVE  SGTSL068-PC-MIN-TAXA(IC)
                                       TO  SGTSI068-PC-MIN-TAXA

           MOVE  KOPRD-CD-OPRD         TO  SGTSI068-CD-USU-ULT-ATL
           MOVE  'CURRENT'             TO  SGTSI068-TS-ULT-ATL
           MOVE  SGTSL068-CD-TIP-APL-PREF(IC)
                                       TO  SGTSI068-CD-TIP-APL-PREF
           MOVE  SGTSL068-CD-PERC-APL-PREF(IC)
                                       TO  SGTSI068-CD-PERC-APL-PREF
           MOVE  SGTSL068-CD-IEC-PREF(IC)
                                       TO  SGTSI068-CD-IEC-PREF

           PERFORM 900000-EXECUTAR-MODULO
           .
       590000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       600000-TARF-OPR         SECTION.
      *-----------------------------------------------------------------
           MOVE  0025                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                   SGTSI084-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSI084              TO  NM-PGM

           MOVE  SGTS0072-CD-CTRA-DST  TO  SGTSI084-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-DST
                                       TO  SGTSI084-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-DST
                                       TO  SGTSI084-NR-VRS-OPR-ETRD
           MOVE  SGTSL084-CD-TIP-TARF(IC)
                                       TO  SGTSI084-CD-TIP-TARF
           MOVE SGTSL084-PC-TARF-OPR(IC)
                                       TO  SGTSI084-PC-TARF-OPR

           MOVE  KOPRD-CD-OPRD         TO  SGTSI084-CD-USU-ULT-ATL
           MOVE  'CURRENT'             TO  SGTSI084-TS-ULT-ATL

           PERFORM 900000-EXECUTAR-MODULO
           .
       600000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       620000-RCBT-PRVT         SECTION.
      *-----------------------------------------------------------------
           MOVE  0027                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                   SGTSI070-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSI070              TO  NM-PGM

           MOVE  SGTS0072-CD-CTRA-DST  TO  SGTSI070-CD-CTRA-TIT-VL
           MOVE  SGTS0072-CD-OPR-ETRD-DST
                                       TO  SGTSI070-CD-OPR-ETRD-MCP
           MOVE  SGTS0072-NR-VRS-OPR-DST
                                       TO  SGTSI070-NR-VRS-OPR-ETRD

           MOVE  SGTSL070-NR-SEQL-RCBT-PRVT(IC)
                                       TO  SGTSI070-NR-SEQL-RCBT-PRVT
           MOVE  SGTSL070-CD-TIP-RCBT-PRVT(IC)
                                       TO  SGTSI070-CD-TIP-RCBT-PRVT
           MOVE  SGTSL070-DT-RCBT-PRVT(IC)
                                       TO  SGTSI070-DT-RCBT-PRVT
           MOVE  SGTSL070-PC-RCBT-PRVT(IC)
                                       TO  SGTSI070-PC-RCBT-PRVT
           MOVE  SGTSL070-VL-RCBT-PRVT(IC)
                                       TO  SGTSI070-VL-RCBT-PRVT

           MOVE  'CURRENT'             TO  SGTSI070-TS-ULT-ATL

           MOVE  SGTSL070-TX-OBS-RCBT-PRVT(IC)
                                       TO  SGTSI070-TX-OBS-RCBT-PRVT

           PERFORM 900000-EXECUTAR-MODULO
           .
       620000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       630000-IND-OPR         SECTION.
      *-----------------------------------------------------------------
           MOVE  0028                  TO  MSG1-CD-SEQL-ERRO

      *--> Os Tipos:
      *    0001 até 0005 - São tratados no modulo SGTS0088
      *                       Inclusão de Operação Estruturada

      * Copiar apenas os indicadores:
      * 14 - Operação Estruturada vinculada a Sindicato (NÃO EST? SENDO
      *                                  GRAVADO)
      * 16 - Indicador de Remuneração variável na precificação
      * 17 - Indicador de Facility na precificação
      * 18 - Indicador de Acrescimo de  Imposto (Gross UP)


           IF  SGTSL063-CD-TIP-IN(IC)  EQUAL 14 OR 16 OR 17 OR 18
              INITIALIZE                   SGTSI063-DADOS

      *--> Movimentar o nome do programa a ser executado
              MOVE  SGTSI063           TO  NM-PGM

              MOVE  SGTS0072-CD-CTRA-DST  TO  SGTSI063-CD-CTRA-TIT-VL
              MOVE  SGTS0072-CD-OPR-ETRD-DST
                                       TO  SGTSI063-CD-OPR-ETRD-MCP
              MOVE  SGTS0072-NR-VRS-OPR-DST
                                       TO  SGTSI063-NR-VRS-OPR-ETRD

              MOVE  SGTSL063-CD-TIP-IN(IC)
                                       TO  SGTSI063-CD-TIP-IN

              PERFORM 900000-EXECUTAR-MODULO
           END-IF
           .
       630000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *-----------------------------------------------------------------

      *--> Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM
               WHEN SGTSL064
                    EXEC CICS LINK
                              PROGRAM ( SGTSL064  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSI035
                    EXEC CICS LINK
                              PROGRAM ( SGTSI035  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSI060
                    EXEC CICS LINK
                              PROGRAM ( SGTSI060  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSI061
                    EXEC CICS LINK
                              PROGRAM ( SGTSI061  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSI064
                    EXEC CICS LINK
                              PROGRAM ( SGTSI064  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSL011
                    EXEC CICS LINK
                              PROGRAM ( SGTSL011  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSLA35
                    EXEC CICS LINK
                              PROGRAM ( SGTSLA35  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSL060
                    EXEC CICS LINK
                              PROGRAM ( SGTSL060  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSL061
                    EXEC CICS LINK
                              PROGRAM ( SGTSL061  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSI064
                    EXEC CICS LINK
                              PROGRAM ( SGTSI064  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSL067
                    EXEC CICS LINK
                              PROGRAM ( SGTSL067  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSI067
                    EXEC CICS LINK
                              PROGRAM ( SGTSI067  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSL079
                    EXEC CICS LINK
                              PROGRAM ( SGTSL079  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSI079
                    EXEC CICS LINK
                              PROGRAM ( SGTSI079  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSL068
                    EXEC CICS LINK
                              PROGRAM ( SGTSL068  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSI068
                    EXEC CICS LINK
                              PROGRAM ( SGTSI068  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSL084
                    EXEC CICS LINK
                              PROGRAM ( SGTSL084  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSI084
                    EXEC CICS LINK
                              PROGRAM ( SGTSI084  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSL081
                    EXEC CICS LINK
                              PROGRAM ( SGTSL081  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSI081
                    EXEC CICS LINK
                              PROGRAM ( SGTSI081  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSL070
                    EXEC CICS LINK
                              PROGRAM ( SGTSL070  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSI070
                    EXEC CICS LINK
                              PROGRAM ( SGTSI070  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSL063
                    EXEC CICS LINK
                              PROGRAM ( SGTSL063  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSI063
                    EXEC CICS LINK
                              PROGRAM ( SGTSI063  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSL082
                    EXEC CICS LINK
                              PROGRAM ( SGTSL082  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSI082
                    EXEC CICS LINK
                              PROGRAM ( SGTSI082  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9006          TO  MSG1-CD-ERRO-TS
                    STRING  'Não definida a execução do módulo'
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO            MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE

           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET   ERRO-CICS    TO  TRUE
                    MOVE  3004         TO  MSG1-CD-ERRO-TS
                    STRING
                    'Erro  ao acionar o módulo  ' NM-PGM  ', '
                    'RESP  = '  MSG1-RESP1  ', '
                    'RESP2 = '  MSG1-RESP2
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       910000-CHAMAR-SGTST011          SECTION.
      *-----------------------------------------------------------------
           MOVE  0029                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTST011              TO  NM-PGM
           EXEC CICS LINK
                PROGRAM    ( SGTST011                 )
                COMMAREA   ( SGTST011-DADOS           )
                LENGTH     ( LENGTH OF SGTST011-DADOS )
           END-EXEC

           IF  SGTST011-CD-RTN-PGM         NOT EQUAL  ZEROS  AND  +100
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST011-MSG-ERRO-PGM
                                       TO  MSG1-TX-ERRO-TS
                                           MSG1-TX-RTN-PGM-ACND
               MOVE  SGTST011-CD-RTN-PGM
                                       TO  MSG1-CD-ERRO-TS
                                           MSG1-CD-RTN-PGM-ACND
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       910000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       920000-CHAMAR-SGTSL006          SECTION.
      *-----------------------------------------------------------------
           MOVE  0030                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTSL006              TO  NM-PGM
           EXEC CICS LINK
                PROGRAM    ( SGTSL006                 )
                COMMAREA   ( SGTSL006-DADOS           )
                LENGTH     ( LENGTH OF SGTSL006-DADOS )
           END-EXEC

           IF  SGTSL006-CD-RTN-PGM         NOT EQUAL  ZEROS  AND  +100
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTSL006-MSG-ERRO-PGM
                                       TO  MSG1-TX-ERRO-TS
                                           MSG1-TX-RTN-PGM-ACND
               MOVE  SGTSL006-CD-RTN-PGM
                                       TO  MSG1-CD-ERRO-TS
                                           MSG1-CD-RTN-PGM-ACND
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       920000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       921000-CHAMAR-SGTST006          SECTION.
      *-----------------------------------------------------------------
           MOVE  0031                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTST006              TO  NM-PGM
           EXEC CICS LINK
                PROGRAM    ( SGTST006                 )
                COMMAREA   ( SGTST006-DADOS           )
                LENGTH     ( LENGTH OF SGTST006-DADOS )
           END-EXEC

           IF  SGTST006-CD-RTN-PGM           NOT EQUAL  ZEROS  AND  +100
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST006-MSG-ERRO
                                       TO  MSG1-TX-ERRO-TS
                                           MSG1-TX-RTN-PGM-ACND
               MOVE  SGTST006-CD-RTN-PGM
                                       TO  MSG1-CD-ERRO-TS
                                           MSG1-CD-RTN-PGM-ACND
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       921000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE NM-PGM
               WHEN SGTSL011
                    MOVE  'SGTSL011-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL011-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL011-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL011-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL011-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL011-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL011-DT-INC-TAXA-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL011-DT-INC-TAXA-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTST011
                    PERFORM 969100-ACAO-REMUNERACAO

               WHEN SGTSL006
                    MOVE  'SGTSL006-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL006-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL006-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL006-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL006-ULT-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL006-ULT-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL006-ULT-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL006-ULT-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL006-ULT-CD-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL006-ULT-CD-CLI
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL006-ULT-TIP-FUC-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL006-ULT-TIP-FUC-CLI
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL006-DT-EMS-TIT-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL006-DT-EMS-TIT-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL006-CD-TIP-FUC-CLI-E'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL006-CD-TIP-FUC-CLI-E
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTST006
                    PERFORM 969200-ACAO-CLIENTE

               WHEN SGTSLA35
                    MOVE  'SGTSLA35-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLA35-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLA35-CD-OPR-ETRD-MC'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLA35-CD-OPR-ETRD-MC
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLA35-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLA35-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLA35-CD-PSS-CTT-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLA35-CD-PSS-CTT-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI035
                    MOVE  'SGTSI035-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI035-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI035-CD-OPR-ETRD-MC'
                                       TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
                    IF  SGTSI035-CD-OPR-ETRD-MCP    NUMERIC
                        MOVE  SGTSI035-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                        MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM  999003-ADICIONA-VRV-MNT


                    MOVE  'SGTSI035-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI035-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI035-CD-PSS-CTT-EXNO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI035-CD-PSS-CTT-EXNO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI035-CD-USU-RSP-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI035-CD-USU-RSP-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI035-TS-ATL-CTT-EXNO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI035-TS-ATL-CTT-EXNO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL061
                    MOVE  'SGTSL061-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL061-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL061-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL061-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL061-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL061-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL061-DT-RPAC-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL061-DT-RPAC-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI061
                    MOVE  'SGTSI061-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI061-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI061-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI061-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI061-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI061-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI061-DT-RPAC-PGMD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI061-DT-RPAC-PGMD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI061-CD-USU-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI061-CD-USU-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI061-TS-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI061-TS-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI061-TX-MTV-RPAC-PGMD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI061-TX-MTV-RPAC-PGMD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL060
                    MOVE  'SGTSL060-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL060-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL060-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL060-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL060-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL060-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL060-CD-ARQ-OPR-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL060-CD-ARQ-OPR-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI060
                    MOVE  'SGTSI060-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI060-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI060-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI060-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI060-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI060-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI060-CD-ARQ-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI060-CD-ARQ-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI060-CD-USU-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI060-CD-USU-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI060-TS-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI060-TS-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI060-TX-DCR-ANX'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI060-TX-DCR-ANX
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL064
                    MOVE  'SGTSL064-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL064-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL064-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL064-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL064-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL064-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL064-CD-TIP-PRTC'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL064-CD-TIP-PRTC
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL064-CD-ULT-TIP-PRTC'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL064-CD-ULT-TIP-PRTC
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL064-CD-ULT-BCO-PCT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL064-CD-ULT-BCO-PCT
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI064
                    MOVE  'SGTSI064-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI064-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI064-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI064-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI064-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI064-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI064-CD-BCO-PCT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI064-CD-BCO-PCT
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI064-CD-TIP-PRTC'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI064-CD-TIP-PRTC
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI064-IN-BCO-LDR'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI064-IN-BCO-LDR
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI064-PC-PRTC-BCO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI064-PC-PRTC-BCO
                                       TO  AX-VL-4V4
                    MOVE  AX-VL-4V4    TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI064-CD-USU-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI064-CD-USU-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI064-TS-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI064-TS-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL067
                    MOVE  'SGTSL067-E-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL067-E-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL067-E-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL067-E-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL067-E-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL067-E-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL067-CD-TIP-SRVC'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL067-CD-TIP-SRVC
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL067-E-NR-NOTA-TCN-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL067-E-NR-NOTA-TCN-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL067-E-CD-TIP-DOC-TCN'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL067-E-CD-TIP-DOC-TCN
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI067
                    MOVE  'SGTSI067-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI067-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI067-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI067-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI067-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI067-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI067-NR-NOTA-TCN-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI067-NR-NOTA-TCN-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI067-CD-USU-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI067-CD-USU-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI067-TS-INCL-NOTA-TCN'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI067-TS-INCL-NOTA-TCN
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL079
                    MOVE  'SGTSL079-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL079-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL079-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL079-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL079-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL079-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL079-CD-TIP-CND-NEGD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL079-CD-TIP-CND-NEGD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL079-CD-ULT-TIP-CND-NEGD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL079-CD-ULT-TIP-CND-NEGD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL079-DT-INC-VLD-CND'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL079-DT-INC-VLD-CND
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI079
                    MOVE  'SGTSI079-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI079-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI079-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI079-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI079-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI079-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI079-CD-TIP-CND-NEGD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI079-CD-TIP-CND-NEGD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI079-DT-INC-VLD-CND'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI079-DT-INC-VLD-CND
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI079-CD-TIP-CBT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI079-CD-TIP-CBT
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI079-DT-FIM-VLD-CND'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI079-DT-FIM-VLD-CND
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI079-IN-OBGD-CND'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI079-IN-OBGD-CND
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI079-PC-TAXA-QBRA-FNDO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI079-PC-TAXA-QBRA-FNDO
                                       TO  AX-VL-4V10
                    MOVE  AX-VL-4V10   TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI079-CD-FMA-APRC'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI079-CD-FMA-APRC
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL068
                    MOVE  'SGTSL068-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL068-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL068-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL068-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL068-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL068-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL068-CD-TIP-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL068-CD-TIP-TAXA
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL068-CD-ULT-TIP-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL068-CD-ULT-TIP-TAXA
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL068-DT-ULT-INC-VLD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL068-DT-ULT-INC-VLD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI068
                    MOVE  'SGTSI068-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI068-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI068-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI068-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI068-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI068-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI068-CD-TIP-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI068-CD-TIP-TAXA
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI068-DT-INC-CND-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI068-DT-INC-CND-TAXA
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI068-CD-USU-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI068-CD-USU-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI068-TS-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI068-TS-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI068-DT-FIM-CND-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI068-DT-FIM-CND-TAXA
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI068-PC-MIN-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI068-PC-MIN-TAXA
                                       TO AX-VL-4V10
                    MOVE  AX-VL-4V10   TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI068-CD-TIP-APL-PREF'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI068-CD-TIP-APL-PREF
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI068-CD-PERC-APL-PREF'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI068-CD-PERC-APL-PREF
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI068-CD-IEC-PREF'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI068-CD-IEC-PREF
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL084
                    MOVE  'SGTSL084-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL084-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL084-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL084-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL084-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL084-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL084-CD-TIP-TARF-E'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL084-CD-TIP-TARF-E
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI084
                    MOVE  'SGTSI084-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI084-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI084-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI084-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI084-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI084-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI084-CD-TIP-TARF'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI084-CD-TIP-TARF
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI084-CD-USU-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI084-CD-USU-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI084-TS-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI084-TS-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI084-PC-TARF-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI084-PC-TARF-OPR
                                       TO  AX-VL-4V10
                    MOVE  AX-VL-4V10   TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL081
                    MOVE  'SGTSL081-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL081-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL081-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL081-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL081-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL081-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL081-NR-ULT-PRV-CMPS-GRT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL081-NR-ULT-PRV-CMPS-GRT
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI081
                    MOVE  'SGTSI081-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI081-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI081-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI081-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI081-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI081-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI081-CD-USU-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI081-CD-USU-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI081-TS-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI081-TS-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI081-CD-NTZ-GRT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI081-CD-NTZ-GRT
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI081-CD-FMA-VCLC'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI081-CD-FMA-VCLC
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI081-DT-LIM-GRT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI081-DT-LIM-GRT
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI081-PC-GRT-CMPS-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI081-PC-GRT-CMPS-PRVT
                                       TO  AX-VL-4V10
                    MOVE  AX-VL-4V10   TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI081-VL-GRT-CMPS-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI081-VL-GRT-CMPS-PRVT
                                       TO  AX-VL-15V2
                    MOVE  AX-VL-15V2   TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL070
                    MOVE  'SGTSL070-E-CD-TIP-SRVC'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL070-E-CD-TIP-SRVC
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL070-E-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL070-E-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL070-E-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL070-E-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL070-E-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL070-E-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL070-E-NR-SEQL-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL070-E-NR-SEQL-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL070-E-CD-TIP-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL070-E-CD-TIP-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL070-NR-SEQL-RCBT-PRVT-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL070-NR-SEQL-RCBT-PRVT-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL070-CD-TIP-RCBT-PRVT-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL070-CD-TIP-RCBT-PRVT-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL070-DT-RCBT-PRVT-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL070-DT-RCBT-PRVT-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI070
                    MOVE  'SGTSI070-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI070-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI070-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI070-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI070-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI070-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI070-NR-SEQL-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI070-NR-SEQL-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI070-CD-TIP-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI070-CD-TIP-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI070-DT-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI070-DT-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI070-TS-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI070-TS-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI070-TX-OBS-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI070-TX-OBS-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI070-VL-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI070-VL-RCBT-PRVT
                                       TO  AX-VL-13V4
                    MOVE  AX-VL-13V4   TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI070-PC-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI070-PC-RCBT-PRVT
                                       TO  AX-VL-4V4
                    MOVE  AX-VL-4V4    TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL063
                    MOVE  'SGTSL063-CD-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL063-CD-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL063-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL063-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL063-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL063-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL063-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL063-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL063-CD-TIP-IN-E'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL063-CD-TIP-IN-E
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI063
                    MOVE  'SGTSI063-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI063-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI063-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI063-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI063-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI063-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI063-CD-TIP-IN'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI063-CD-TIP-IN
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL082
                    MOVE  'SGTSL082-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL082-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL082-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL082-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL082-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL082-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL082-NR-PRV-CMPS-GRT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL082-NR-PRV-CMPS-GRT
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL082-CD-ULT-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL082-CD-ULT-CLI
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL082-CD-TIP-SRVC'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL082-CD-TIP-SRVC
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI082

                    MOVE  'SGTSI082-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI082-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI082-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI082-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI082-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI082-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI082-NR-PRV-CMPS-GRT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI082-NR-PRV-CMPS-GRT
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI082-CD-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI082-CD-CLI
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN OTHER
                    CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969100-ACAO-REMUNERACAO                SECTION.
      *--> Obtem a monitoração de acordo com a ação da remuneração
      *-----------------------------------------------------------------

           EVALUATE SGTST011-ACAO
      *--> Inclusão
             WHEN 0001
      *--> Alteração
             WHEN 0002
               MOVE  'SGTST011-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST011-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST011-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST011-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST011-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST011-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST011-DT-INC-TAXA-RMNC'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST011-DT-INC-TAXA-RMNC
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST011-TS-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST011-TS-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST011-CD-USU-RSP-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST011-CD-USU-RSP-ATL
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST011-PC-TAXA-RMNC-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST011-PC-TAXA-RMNC-OPR
                                       TO  AX-VL-4V4
               MOVE  AX-VL-4V4
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST011-DT-FIM-TAXA-RMNC'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST011-DT-FIM-TAXA-RMNC
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST011-CD-TIP-FMA-APL'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST011-CD-TIP-FMA-APL
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST011-CD-IEC-RMNC'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST011-CD-IEC-RMNC
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST011-CD-PERC-CPTZ-JUR'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST011-CD-PERC-CPTZ-JUR
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

      *--> Exclusão
              WHEN 0003
      *--> Consultar
              WHEN 0004
      *--> Consultar taxa da remuneração
              WHEN 0006
      *--> Consultar taxa de remuneração com data informada
              WHEN 0007
               MOVE  'SGTST011-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST011-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST011-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST011-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST011-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST011-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST011-DT-INC-TAXA-RMNC'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST011-DT-INC-TAXA-RMNC
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

      *--> Consultar taxa ativa
              WHEN 5
               MOVE  'SGTST011-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST011-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST011-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST011-CD-OPR-ETRD-MCP

                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST011-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST011-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT
           END-EVALUATE
           .
       969100-FIM.
           EXIT.

      *-----------------------------------------------------------------
       969200-ACAO-CLIENTE                SECTION.
      *    Obtem a monitoração de acordo com a ação do cliente
      *-----------------------------------------------------------------
            EVALUATE SGTST006-ACAO
      *--> Inclusão
             WHEN 0001
      *--> Alteração
             WHEN 0002
               MOVE  'SGTST006-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST006-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST006-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST006-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST006-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST006-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST006-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST006-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST006-CD-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST006-CD-CLI
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST006-CD-TIP-FUC-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST006-CD-TIP-FUC-CLI
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST006-CD-USU-RSP-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST006-CD-USU-RSP-ATL
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST006-TS-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST006-TS-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

      *--> Exclusão
             WHEN 0003
      *--> Consultar
             WHEN 0004
               MOVE  'SGTST006-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST006-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST006-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST006-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST006-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST006-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST006-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST006-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST006-CD-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST006-CD-CLI
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST006-CD-TIP-FUC-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST006-CD-TIP-FUC-CLI
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

      *--> Consulta cliente
             WHEN 0005
      *--> Alterar cliente
             WHEN 0006
               MOVE  'SGTST006-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST006-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST006-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST006-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST006-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST006-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST006-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST006-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTST006-CD-TIP-FUC-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTST006-CD-TIP-FUC-CLI
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT
            END-EVALUATE
            .
       969200-FIM.
           EXIT.

      *-----------------------------------------------------------------
      *                                                                *
      * SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
      *                                                                *
      *-----------------------------------------------------------------
       970000-LER-CANAL                SECTION.
      *--> Obtem o canal corrente
      *-----------------------------------------------------------------
           MOVE  0032
                                       TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL
                                       TO  TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO
                                       TO  TRUE
           .
       970000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------

           MOVE  0033
                                       TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao
           MOVE  'SGTS0072'
                                       TO  NM-PGM
           MOVE  'SGTS0072-RQSC'
                                       TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      *--> Caso a requisição seja variável (cláusula DEPENDING ON), o
      * tratamento abaixo não deve ser efetuado, senão, ocasionará erro
      *--> Início

      *--> Obtem o tamanho esperado da requisicao
           MOVE LENGTH OF SGTS0072-RQSC TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisicao X tamanho recebido

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM 999000-ERRO
           END-IF
      *--> Fim

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0072-RQSC)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *-----------------------------------------------------------------
      *--> A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------

           MOVE  0034                  TO  MSG1-CD-SEQL-ERRO

      *--> Le o container de erro dos módulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS GET
                     CONTAINER ( NM-CTNR                     )
                     INTO      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC

           EVALUATE TRUE
      *        Caso deseje personalizar ou nao interromper o
      *        processamento, alterar para nao dar erro aqui SOMENTE
      *        Para o Modulo Chamado e tratar na section chamadora
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    SET   ERRO-PROGRAMA-ACIONADO TO TRUE
                    MOVE  IIBKERRO-CD-ERRO       TO MSG1-CD-ERRO-TS
                    MOVE  IIBKERRO-TX-ERRO       TO MSG1-TX-ERRO-TS
                    MOVE  NM-PGM                 TO MSG1-NM-PGM-ACND
                    MOVE  IIBKERRO-TX-ERRO       TO MSG1-TX-RTN-PGM-ACND
                    MOVE  IIBKERRO-CD-ERRO       TO MSG1-CD-RTN-PGM-ACND

      *-----------> Monitorar variaveis do modulo chamado
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-ERRO

               WHEN OTHER
                    SET   ERRO-CONTAINER  TO  TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *-----------------------------------------------------------------
      *     Recupera o container com os dados de resposta (sada).

      *                        NOME DO CONTAINER RESPOSTA
      *                        --------------------------

      *    Módulo CONTAINER  -     NOME_PGM-RPST
      *    Módulo IIB (FIXO) -     PRM-DADOS-RPST

      *----------------------------------------------------------------
           MOVE  0035                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com módulo a executar
           MOVE  SPACES                TO  NM-CTNR

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_módulo -RPST

           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)


      *--> Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

      *--> Configura dados para leitura do container de resposta
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automáica de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Le a RESPOSTA de acordo com o programa que a gravou


           EVALUATE NM-PGM
      *        Calcular a quantidade de caracteres esperado e limpar
      *        a área de dados antes de executar o GET
      *        Para matrizes com dados variáveis (clausula depending
      *        on) não há como utilizar o comando INITIALIZE
               WHEN SGTSI035
                    INITIALIZE SGTSI035-RPST
                    MOVE LENGTH OF SGTSI035-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI035-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSI060
                    INITIALIZE SGTSI060-RPST
                    MOVE LENGTH OF SGTSI060-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI060-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSI061
                    INITIALIZE SGTSI061-RPST
                    MOVE LENGTH OF SGTSI061-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI061-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSI064
                    INITIALIZE SGTSI064-RPST
                    MOVE LENGTH OF SGTSI064-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI064-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSL011
                    MOVE LENGTH OF SGTSL011-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL011-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSLA35
                    MOVE LENGTH OF SGTSLA35-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSLA35-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSL060
                    MOVE LENGTH OF SGTSL060-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL060-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSL061
                    MOVE LENGTH OF SGTSL061-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL061-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSL064
                    MOVE LENGTH OF SGTSL064-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL064-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSL067
                    MOVE LENGTH OF SGTSL067-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL067-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSI067
                    MOVE LENGTH OF SGTSI067-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI067-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSL079
                    MOVE LENGTH OF SGTSL079-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL079-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSI079
                    MOVE LENGTH OF SGTSI079-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI079-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSL068
                    MOVE LENGTH OF SGTSL068-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL068-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSI068
                    MOVE LENGTH OF SGTSI068-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI068-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSL084
                    MOVE LENGTH OF SGTSL084-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL084-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSI084
                    MOVE LENGTH OF SGTSI084-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI084-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSL081
                    MOVE LENGTH OF SGTSL081-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL081-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSI081
                    MOVE LENGTH OF SGTSI081-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI081-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSL070
                    MOVE LENGTH OF SGTSL070-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL070-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSI070
                    MOVE LENGTH OF SGTSI070-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI070-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSL063
                    MOVE LENGTH OF SGTSL063-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL063-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSI063
                    MOVE LENGTH OF SGTSI063-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI063-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSL082
                    MOVE LENGTH OF SGTSL082-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL082-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSI082
                    MOVE LENGTH OF SGTSI082-RPST
                                       TO  QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI082-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9003         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na leitura da resposta do módulo '
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Obtem a quantidade de caracteres no container
           MOVE  ZEROS                 TO  QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       974000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE  0036                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-INF-OPRD'        TO  NM-CTNR

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           INITIALIZE PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       974000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
      *                                                                *
      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------
           MOVE  0037                  TO  MSG1-CD-SEQL-ERRO

      *--> Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

      *--> Monta o nome do container de resposta
           MOVE  'SGTS0072-RPST'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                    )
                     FROM      (SGTS0072-RPST          )
                     FLENGTH   (LENGTH OF SGTS0072-RPST)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       980000-SAI.
           EXIT.

      *----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *-----------------------------------------------------------------
      *--> Postar container com dados de requisição (entrada)
      *    Módulo CONTAINER  -     NOME_PGM-RQSC
      *    Módulo IIB (FIXO) -     PRM-DADOS-RQSC
      *-----------------------------------------------------------------
           MOVE  0038                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com módulo a executar
           MOVE  SPACES                TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_módulo -RQSC
           EVALUATE NM-PGM

               WHEN SGTSI035
                    MOVE  'SGTSI035-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSI060
                    MOVE  'SGTSI060-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSI061
                    MOVE  'SGTSI061-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSI064
                    MOVE  'SGTSI064-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSL011
                    MOVE  'SGTSL011-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSLA35
                    MOVE  'SGTSLA35-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSL060
                    MOVE  'SGTSL060-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSL061
                    MOVE  'SGTSL061-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSL064
                    MOVE  'SGTSL064-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSL067
                    MOVE  'SGTSL067-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSI067
                    MOVE  'SGTSI067-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSL079
                    MOVE  'SGTSL079-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSI079
                    MOVE  'SGTSI079-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSL068
                    MOVE  'SGTSL068-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSI068
                    MOVE  'SGTSI068-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSL084
                    MOVE  'SGTSL084-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSI084
                    MOVE  'SGTSI084-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSL081
                    MOVE  'SGTSL081-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSI081
                    MOVE  'SGTSI081-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSL070
                    MOVE  'SGTSL070-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSI070
                    MOVE  'SGTSI070-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSL063
                    MOVE  'SGTSL063-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSI063
                    MOVE  'SGTSI063-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSL082
                    MOVE  'SGTSL082-RQSC'
                                       TO  NM-CTNR
               WHEN SGTSI082
                    MOVE  'SGTSI082-RQSC'
                                       TO  NM-CTNR
               WHEN OTHER
                    MOVE  NM-PGM       TO  NM-CTNR(1:8)
                    MOVE  '-RQSC'      TO  NM-CTNR(9:5)
           END-EVALUATE

      *--> Por causa da captura automtica de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Faz PUT da requisicao do programa a executar
           EVALUATE NM-PGM

                WHEN SGTSI035
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSI035-RQSC  )
                              FLENGTH   (LENGTH OF SGTSI035-RQSC  )
                    END-EXEC
                WHEN SGTSI060
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSI060-RQSC  )
                              FLENGTH   (LENGTH OF SGTSI060-RQSC  )
                    END-EXEC
               WHEN SGTSI061
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSI061-RQSC  )
                              FLENGTH   (LENGTH OF SGTSI061-RQSC  )
                    END-EXEC
                WHEN SGTSI064
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSI064-RQSC  )
                              FLENGTH   (LENGTH OF SGTSI064-RQSC  )
                    END-EXEC
                WHEN SGTSL011
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSL011-RQSC  )
                              FLENGTH   (LENGTH OF SGTSL011-RQSC  )
                    END-EXEC
                WHEN SGTSLA35
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSLA35-RQSC  )
                              FLENGTH   (LENGTH OF SGTSLA35-RQSC  )
                    END-EXEC
                WHEN SGTSL060
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSL060-RQSC  )
                              FLENGTH   (LENGTH OF SGTSL060-RQSC  )
                    END-EXEC
                WHEN SGTSL061
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSL061-RQSC  )
                              FLENGTH   (LENGTH OF SGTSL061-RQSC  )
                    END-EXEC
                WHEN SGTSL064
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSL064-RQSC  )
                              FLENGTH   (LENGTH OF SGTSL064-RQSC  )
                    END-EXEC
                WHEN SGTSL067
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSL067-RQSC  )
                              FLENGTH   (LENGTH OF SGTSL067-RQSC  )
                    END-EXEC
                WHEN SGTSI067
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSI067-RQSC  )
                              FLENGTH   (LENGTH OF SGTSI067-RQSC  )
                    END-EXEC
                WHEN SGTSL079
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSL079-RQSC  )
                              FLENGTH   (LENGTH OF SGTSL079-RQSC  )
                    END-EXEC
                WHEN SGTSI079
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSI079-RQSC  )
                              FLENGTH   (LENGTH OF SGTSI079-RQSC  )
                    END-EXEC
                WHEN SGTSL068
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSL068-RQSC  )
                              FLENGTH   (LENGTH OF SGTSL068-RQSC  )
                    END-EXEC
                WHEN SGTSI068
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSI068-RQSC  )
                              FLENGTH   (LENGTH OF SGTSI068-RQSC  )
                    END-EXEC
                WHEN SGTSL084
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSL084-RQSC  )
                              FLENGTH   (LENGTH OF SGTSL084-RQSC  )
                    END-EXEC
                WHEN SGTSI084
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSI084-RQSC  )
                              FLENGTH   (LENGTH OF SGTSI084-RQSC  )
                    END-EXEC
                WHEN SGTSL081
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSL081-RQSC  )
                              FLENGTH   (LENGTH OF SGTSL081-RQSC  )
                    END-EXEC
                WHEN SGTSI081
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSI081-RQSC  )
                              FLENGTH   (LENGTH OF SGTSI081-RQSC  )
                    END-EXEC
                WHEN SGTSL070
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSL070-RQSC  )
                              FLENGTH   (LENGTH OF SGTSL070-RQSC  )
                    END-EXEC
                WHEN SGTSI070
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSI070-RQSC  )
                              FLENGTH   (LENGTH OF SGTSI070-RQSC  )
                    END-EXEC
                WHEN SGTSL063
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSL063-RQSC  )
                              FLENGTH   (LENGTH OF SGTSL063-RQSC  )
                    END-EXEC
                WHEN SGTSI063
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSI063-RQSC  )
                              FLENGTH   (LENGTH OF SGTSI063-RQSC  )
                    END-EXEC
                WHEN SGTSL082
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSL082-RQSC  )
                              FLENGTH   (LENGTH OF SGTSL082-RQSC  )
                    END-EXEC
                WHEN SGTSI082
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSI082-RQSC  )
                              FLENGTH   (LENGTH OF SGTSI082-RQSC  )
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9004         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na gravação da requisição do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       982000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *-----------------------------------------------------------------
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessão do*
      * CICS. Equivale ao initialize na área de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *-----------------------------------------------------------------

           MOVE  0039                  TO  MSG1-CD-SEQL-ERRO

           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)

           SET   DEL-CONTAINER         TO  TRUE

      *--> Exclui o container de resposta
      *--> Não há captura automática de erro CICS por usar RESP e
      *    RESP2   VRS001
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
                    SET   ERRO-CONTAINER
                                       TO  TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *--> Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------

      *--> Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           SET   DEL-CONTAINER         TO  TRUE

      *--> Não há captura automática de erro CICS por usar RESP e
      *    RESP2   VRS001
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
                    SET  ERRO-CONTAINER
                                       TO  TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *-----------------------------------------------------------------
           MOVE  0040                  TO  MSG1-CD-SEQL-ERRO

      *--> Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e
      *    RESP2   VRS001
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

      *-----------------------------------------------------------------
      *    As sections abaixo iniciadas com 999000 representam os
      *    tratamentos de erro utilizando o Sistema MSG
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-ERRO                     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF
           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-ERRO-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *     As sections abaixo representam os tratamentos de erro
      *    utilizando o sistema MSG de mensageria (rotina MSGS0001)
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

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
           IF  MNT-VRV
               MOVE  ' SGTS0072-CD-OPR-ETRD-OGM'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE   SGTS0072-CD-OPR-ETRD-OGM
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTS0072-NR-VRS-OPR-OGM'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS0072-NR-VRS-OPR-OGM
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTS0072-CD-OPR-ETRD-DST'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS0072-CD-OPR-ETRD-DST
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTS0072-NR-VRS-OPR-DST'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS0072-NR-VRS-OPR-DST
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

           END-IF

      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Em caso de abend da MSGS001, adiciona o nome da rotina,
      *    código do erro, retornos cics e nivel execução
           IF MSG1-CD-ERRO NOT EQUAL ZEROS
              STRING
                 '(MSG1/' MSG1-CD-ERRO '/' MSG1-RET-RESP '/'
                 MSG1-RET-RESP2  '/' MSG1-NVL-EXEC ') '
                 MSG1-TX-ERRO-TS
                         DELIMITED BY SIZE INTO IIBKERRO-TX-ERRO
           END-IF

      *--> Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

      *--> Encerra a transação
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
       999999-ENCERRAR-SAI.
           EXIT.
      *---------------------- FIM SGTS0072 -----------------------------