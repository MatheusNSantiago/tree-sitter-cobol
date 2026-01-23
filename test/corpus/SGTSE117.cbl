      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSE117
      * ANALISTA..: F2804421 - ELIANA CRISTINA RAMOS DA SILVA
      * AUTOR.....: F2804421 - ELIANA CRISTINA RAMOS DA SILVA
      * OBJETIVO..: EXCLUSAO DA TABELA JST_VLDC_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 12.11.2018 - F2804421 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSE117.

      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-SS           PIC  X(40) VALUE
               '*** Storage Section começa aqui ***'.

       77  CTE-PROG                PIC  X(17) VALUE '*** SGTSE117 ***'.
       77  CTE-VERS                PIC  X(06) VALUE 'VRS001'.
       77  SGTS014G                PIC  X(08) VALUE 'SGTS014G'.

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * Book de para acionamento do MSG1
-INC IIBKERRO
-INC MSGKWMG1
-INC HLPKW999

      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG            PIC  9(10) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG           PIC  9(11) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG           PIC  9(5) VALUE 0.
           03  CD-TIP-VLDC-OPR-LOG           PIC  9(5) VALUE 0.

      * Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

      * Book dos dados do usuário
-INC IIBKOPRD

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      * Book deste programa
-INC SGTKE117

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       77  CTE-FINAL-SS                PIC  X(40) VALUE
               '*** Storage Section termina aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO
      * Habilita tratamento automatico de Erros DB2
-INC HLPKE997
      * Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDA-PARAMETRO

           PERFORM 200000-EXCLUIR

           PERFORM 300000-POSTAR-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           SET  ERRO-CANAL             TO  TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   ( NM-CNL      )
              NOHANDLE
           END-EXEC
           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
               EIBRESP EQUAL DFHRESP(CONTAINERERR) )
               PERFORM 999002-TRATA-CONDITION-CICS
           END-IF
           .
       120000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSE117-RQSC      TO QT-CCT-EPRO

           SET  GET-CONTAINER          TO  TRUE
           SET  ERRO-CONTAINER         TO  TRUE
           MOVE 'SGTSE117-RQSC'        TO  NM-CTNR

           EXEC CICS GET
              CONTAINER ( NM-CTNR )
              CHANNEL   ( NM-CNL  )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( NM-CTNR )
              CHANNEL   ( NM-CNL  )
              FLENGTH ( LENGTH OF SGTSE117-RQSC )
              INTO    ( SGTSE117-RQSC )
           END-EXEC

           SET  SEM-ERRO               TO  TRUE
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDA-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           IF  SGTSE117-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 002                 TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'    TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE117-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 003                 TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE117-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 005                 TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' informado.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE117-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 007                 TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE117-CD-TIP-VLDC-OPR EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 861                 TO MSG1-CD-ERRO-TS
               MOVE 'Código do parâmetro da justificativa não informado.
      -             ''                  TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-EXCLUIR SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE DADOS-LOG
           MOVE 'E'                    TO SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG

           EXEC SQL
                DELETE FROM DB2SGT.JST_VLDC_OPR_ETRD
                WHERE CD_CTRA_TIT_VL     = :SGTSE117-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP    = :SGTSE117-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD    = :SGTSE117-NR-VRS-OPR-ETRD
                  AND CD_TIP_VLDC_OPR    = :SGTSE117-CD-TIP-VLDC-OPR
           END-EXEC

           IF  SQLCODE EQUAL +100
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 100                 TO  MSG1-CD-ERRO-TS
               MOVE 'Registro inexistente.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO  MSG1-CD-SEQL-ERRO

           MOVE  'Exclusão efetuada com sucesso.'
                                       TO  SGTSE117-TX-RTN-PRCT
           MOVE 'SGTSE117-RPST'        TO  NM-CTNR

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                )
                      CHANNEL   ( NM-CNL                 )
                      FLENGTH   ( LENGTH OF SGTSE117-RPST)
                      FROM      ( SGTSE117-RPST          )
           END-EXEC

           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       980102-GRAVA-LOG SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE 'JST_VLDC_OPR_ETRD '   TO SGTS014G-NM-ENT

           PERFORM 980103-LER-DADOS-OPRD
           MOVE  KOPRD-CD-OPRD     TO  SGTS014G-CD-CHV-USU

           MOVE 'SGTSE117'                  TO SGTS014G-NM-PGM
           MOVE SGTSE117-CD-CTRA-TIT-VL     TO CD-CTRA-TIT-VL-LOG
           MOVE SGTSE117-CD-OPR-ETRD-MCP    TO CD-OPR-ETRD-MCP-LOG
           MOVE SGTSE117-NR-VRS-OPR-ETRD    TO NR-VRS-OPR-ETRD-LOG
           MOVE SGTSE117-CD-TIP-VLDC-OPR    TO CD-TIP-VLDC-OPR-LOG

           STRING CD-CTRA-TIT-VL-LOG
           ';'    CD-OPR-ETRD-MCP-LOG
           ';'    NR-VRS-OPR-ETRD-LOG
           ';'    CD-TIP-VLDC-OPR-LOG
               DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR

           MOVE LENGTH OF DADOS-LOG    TO EIBCALEN
           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM  NOT EQUAL ZEROS
               SET  ERRO-PROGRAMA-ACIONADO     TO TRUE
               MOVE 112                        TO MSG1-CD-ERRO-TS
               MOVE 'Erro na gravação das informações do Log.'
                                               TO MSG1-TX-ERRO-TS
               MOVE SGTS014G                   TO MSG1-NM-PGM-ACND
               MOVE SGTS014G-CD-RTN-PGM        TO MSG1-CD-RTN-PGM-ACND
               MOVE SGTS014G-MSG-ERRO-PGM      TO MSG1-TX-RTN-PGM-ACND
               PERFORM 999100-MONITORAR-VRV-PGM-CHMD
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       980102-SAI.
           EXIT.
      *----------------------------------------------------------------*
       980103-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE 0009                   TO  MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO  TRUE
           MOVE 'PRM-INF-OPRD'         TO  NM-CTNR

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO  TRUE

           INITIALIZE PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
           END-EXEC.

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       980103-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------
           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                     TO MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                             TO MSG1-TX-ERRO-TS
           END-IF

      * Verifica se o tipo de erro gerado possui tratamento interno
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

           MOVE 'SGTSE117-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSE117-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE117-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
           IF  SGTSE117-CD-OPR-ETRD-MCP      IS NUMERIC
               MOVE SGTSE117-CD-OPR-ETRD-MCP TO MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Valor não numérico'     TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE117-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSE117-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE117-CD-TIP-VLDC-OPR'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSE117-CD-TIP-VLDC-OPR     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO

           EXEC CICS PUT
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   ( NM-CNL      )
              FROM      ( IIBKERRO-PRM-ERRO)
              FLENGTH ( LENGTH OF IIBKERRO-PRM-ERRO )
              NOHANDLE
           END-EXEC

           PERFORM 999999-ENCERRAR
           .
       999000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999100-MONITORAR-VRV-PGM-CHMD   SECTION.
      *-----------------------------------------------------------------
      *--> Monitora as variáveis dos módulos chamados

           MOVE  'SGTS014G-CD-TIP-ACAO' TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS014G-CD-TIP-ACAO   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTS014G-NM-ENT'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS014G-NM-ENT        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTS014G-CD-CHV-USU'  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS014G-CD-CHV-USU    TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTS014G-NM-PGM'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS014G-NM-PGM        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTS014G-CHV-PRMR'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS014G-CHV-PRMR      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           .
       999100-SAI.
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
      * 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997
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
-INC MSGKPMG1
      * --------------------------------------*
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC.









