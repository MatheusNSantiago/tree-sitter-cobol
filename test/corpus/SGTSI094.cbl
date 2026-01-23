      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSI094
      * ANALISTA..: F8785286 - ROSELI AKEMI KAWAHATA
      * AUTOR.....: F8785286 - ROSELI AKEMI KAWAHATA
      * OBJETIVO..: Inclusão da tabela IFLC_MNL_LIM_EXCO
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 06.11.2018 - F8785286 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSI094.

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
       77  CTE-INICIO-SS               PIC  X(40) VALUE
               '*** Storage Section começa aqui ***'.
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSI094 ***'.
       77  CTE-VERS     PIC  X(06) VALUE 'VRS001'.
       77  SGTS014G                    PIC  X(08) VALUE 'SGTS014G'.

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      *--> Variaveis para Gravacao do Log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG      PIC  9(10) VALUE ZEROS.
           03  CD-TIT-OPR-ETRD         PIC X(12)  VALUE ' '.
           03  NR-DVS-TIT              PIC  9(5)  VALUE ZEROS.
           03  TS-ATL-IFLC-MNL         PIC  X(26) VALUE ' '.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

       01  NM-PGM-CHMD                 PIC  X(08)            VALUE ' '.
      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book dos dados do Log
       01  DADOS-LOG.
-INC SGTK014G

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

       01  QT-REG-SELD                      PIC S9(4) COMP-5 VALUE 0.

-INC HLPKTIME
       01  TS-SQL-ATU                       PIC  X(26) VALUE ' '.

      * Indicadores de nulidades

       01  QT-REG-SELD-NULO                 PIC S9(4) COMP-5 VALUE 0.
       01  MSG1-NM-CMP                      PIC X(50) VALUE SPACES.

-INC SGTKI094

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

           PERFORM 200000-INCLUIR

           PERFORM 300000-POSTAR-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS  ASSIGN
                      CHANNEL ( NM-CNL                                 )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           SET   DEL-CONTAINER         TO  TRUE

           EXEC CICS DELETE
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   ( NM-CNL     )
              RESP      ( MSG1-RESP1 )
              RESP2     ( MSG1-RESP2 )
           END-EXEC

           IF  NOT (MSG1-RESP1             EQUAL DFHRESP(NORMAL)  OR
                    MSG1-RESP1             EQUAL DFHRESP(CONTAINERERR))
               SET   ERRO-CONTAINER    TO  TRUE
               PERFORM  999002-TRATA-CONDITION-CICS
           END-IF
           .
       120000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                        TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSI094-RQSC      TO QT-CCT-EPRO

           EXEC CICS GET
              CONTAINER ( 'SGTSI094-RQSC' )
              CHANNEL   ( NM-CNL      )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSI094-RQSC' )
              CHANNEL   ( NM-CNL      )
              FLENGTH ( LENGTH OF SGTSI094-RQSC )
              INTO    ( SGTSI094-RQSC )
           END-EXEC
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDA-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           PERFORM 150000-VALIDA-CAMPOS

           PERFORM 170001-VALIDA-TIT-OPR-ETRD-MCP
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       150000-VALIDA-CAMPOS SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           PERFORM 151000-BUSCA-TS-DT

           MOVE HLPKE001-TS-FMTD       TO TS-SQL-ATU

           IF  SGTSI094-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 002                TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'   TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI094-CD-TIT-OPR-ETRD EQUAL SPACES
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 231                 TO  MSG1-CD-ERRO-TS
               MOVE 'Código do título da operação estruturada não inform
      -             'ado.'              TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI094-NR-DVS-TIT      EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 232                 TO  MSG1-CD-ERRO-TS
               MOVE 'Número da divisão do título da operação não informa
      -             'do.'               TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI094-TS-ATL-IFLC-MNL EQUAL SPACES
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0867                TO  MSG1-CD-ERRO-TS
               MOVE 'Timestamp atualização influência manual inválido.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI094-IN-IFLC-MNL-EXCO EQUAL SPACES
               SET  ERRO-PROGRAMADO      TO TRUE
               MOVE 0869                 TO  MSG1-CD-ERRO-TS
               MOVE 'Indicador influência manual exceção não informado.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI094-IN-IFLC-MNL-EXCO NOT EQUAL 'S'  AND
               SGTSI094-IN-IFLC-MNL-EXCO NOT EQUAL 'N'
               SET  ERRO-PROGRAMADO      TO TRUE
               MOVE 0868                 TO  MSG1-CD-ERRO-TS
               MOVE 'Indicador influência manual exceção inválido.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI094-CD-USU-RSP-ULT-ATL    EQUAL SPACES
               SET  ERRO-PROGRAMADO           TO TRUE
               MOVE 278                       TO  MSG1-CD-ERRO-TS
               MOVE 'Código do usuário responsável pela atualização não
      -        'informado.'                   TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI094-TS-ATL-IFLC-MNL    EQUAL 'CURRENT'
               MOVE TS-SQL-ATU             TO SGTSI094-TS-ATL-IFLC-MNL
           END-IF
           .
       150000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       151000-BUSCA-TS-DT SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           CALL SBTIMEDT            USING  HLPKE001-CD-FUC
                                           HLPKE001-AREA-TRB
                                           HLPKE001-FMT-TS
                                           HLPKE001-TAM-TS
                                           HLPKE001-TS-FMTD
           IF  RETURN-CODE      NOT EQUAL  ZEROS
               SET  ERRO-PROGRAMADO     TO  TRUE
               MOVE 050                 TO  MSG1-CD-ERRO-TS
               MOVE 'Erro na chamada da SBTIMEDT' TO MSG1-TX-ERRO-TS
               PERFORM 999100-MONITORAR-VRV-PGM-CHMD
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       151000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       170001-VALIDA-TIT-OPR-ETRD-MCP SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO QT-REG-SELD

           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.TIT_OPR_ETRD_MCP
               WHERE CD_TIT_OPR_ETRD    = :SGTSI094-CD-TIT-OPR-ETRD
                 AND NR_DVS_TIT         = :SGTSI094-NR-DVS-TIT
           END-EXEC

           IF  QT-REG-SELD = 0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 234                TO  MSG1-CD-ERRO-TS
               MOVE 'Título da operação estruturada inexistente.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       170001-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-INCLUIR SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO
      *
      * Desligar erro automatico do DB2
      *
           EXEC SQL WHENEVER SQLERROR
                    CONTINUE
           END-EXEC
      *
           EXEC SQL
                INSERT INTO DB2SGT.IFLC_MNL_LIM_EXCO
                ( CD_CTRA_TIT_VL
                , CD_TIT_OPR_ETRD
                , NR_DVS_TIT
                , TS_ATL_IFLC_MNL
                , IN_IFLC_MNL_EXCO
                , CD_USU_RSP_ULT_ATL )
                VALUES
                ( :SGTSI094-CD-CTRA-TIT-VL
                , :SGTSI094-CD-TIT-OPR-ETRD
                , :SGTSI094-NR-DVS-TIT
                , :SGTSI094-TS-ATL-IFLC-MNL
                , :SGTSI094-IN-IFLC-MNL-EXCO
                , :SGTSI094-CD-USU-RSP-ULT-ATL )
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN -803
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 803                TO  MSG1-CD-ERRO-TS
               MOVE 'Inclusao nao efetuada, registro existente'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA

               WHEN 0
                    CONTINUE

               WHEN OTHER
      * Aciona o tratamento padrao de erro padrao para os demais casos
                    GO TO 999010-TRATA-ERRO-DB2
           END-EVALUATE

           INITIALIZE                      DADOS-LOG
           SET   SGTS014G-INCLUSAO     TO  TRUE

           PERFORM  981000-GRAVAR-LOG
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO  MSG1-CD-SEQL-ERRO

           MOVE  'Inclusão efetuada com sucesso.'
                                       TO  SGTSI094-TX-RTN-PRCT

           EXEC CICS  PUT
                      CONTAINER ( 'SGTSI094-RPST'        )
                      CHANNEL   ( NM-CNL                 )
                      FLENGTH   ( LENGTH OF SGTSI094-RPST)
                      FROM      ( SGTSI094-RPST          )
           END-EXEC

           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       981000-GRAVAR-LOG                SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO  MSG1-CD-SEQL-ERRO

           MOVE  'IFLC_MNL_LIM_EXCO'   TO  SGTS014G-NM-ENT
           MOVE  SGTSI094-CD-USU-RSP-ULT-ATL
                                       TO  SGTS014G-CD-CHV-USU
           MOVE  'SGTSI094'            TO  SGTS014G-NM-PGM

           MOVE  SGTSI094-CD-CTRA-TIT-VL
                                       TO  CD-CTRA-TIT-VL-LOG
           MOVE  SGTSI094-CD-TIT-OPR-ETRD
                                       TO  CD-TIT-OPR-ETRD
           MOVE  SGTSI094-NR-DVS-TIT
                                       TO  NR-DVS-TIT
           MOVE  SGTSI094-TS-ATL-IFLC-MNL
                                       TO  TS-ATL-IFLC-MNL

           STRING  CD-CTRA-TIT-VL-LOG      ';'
                   CD-TIT-OPR-ETRD     ';'
                   NR-DVS-TIT     ';'
                   TS-ATL-IFLC-MNL
                   DELIMITED BY SIZE
                   INTO                    SGTS014G-CHV-PRMR

           MOVE  LENGTH OF DADOS-LOG   TO  EIBCALEN

           CALL  SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM         NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  112               TO  MSG1-CD-ERRO-TS
               MOVE 'Erro na gravação das informações do Log.'
                                       TO                MSG1-TX-ERRO-TS
               MOVE  SGTS014G          TO  MSG1-NM-PGM-ACND
               MOVE  SGTS014G-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTS014G-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM 999100-MONITORAR-VRV-PGM-CHMD
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       981000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> VERIFICA SE O TIPO DE ERRO GERADO POSSUI TRATAMENTO INTERNO
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

           MOVE  'SGTSI094-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI094-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI094-CD-TIT-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI094-CD-TIT-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI094-NR-DVS-TIT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI094-NR-DVS-TIT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI094-TS-ATL-IFLC-MNL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI094-TS-ATL-IFLC-MNL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI094-IN-IFLC-MNL-EXCO'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI094-IN-IFLC-MNL-EXCO
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI094-CD-USU-RSP-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI094-CD-USU-RSP-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava o Erro no Sistema MSG
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                           )
                      CHANNEL   ( NM-CNL                               )
                      FROM      ( IIBKERRO-PRM-ERRO                    )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO          )
                      NOHANDLE
           END-EXEC

           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999100-MONITORAR-VRV-PGM-CHMD   SECTION.
      *-----------------------------------------------------------------
      *--> Monitora as variáveis dos módulos chamados

           EVALUATE  NM-PGM-CHMD

               WHEN  SGTS014G
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


               WHEN  SBTIMEDT
                    MOVE  'HLPKE001-CD-FUC'      TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-CD-FUC        TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'HLPKE001-AREA-TRB'    TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-AREA-TRB      TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'HLPKE001-FMT-TS'      TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-FMT-TS        TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'HLPKE001-TAM-TS'      TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-TAM-TS        TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'HLPKE001-TS-FMTD'     TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-TS-FMTD       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT


               WHEN  OTHER
                    CONTINUE
           END-EVALUATE
           .
       999100-SAI.
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

      *----------------------------------------
       999999-ENCERRAR                 SECTION.
      *----------------------------------------
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.

      *---------------------- FIM SGTSI094 -----------------------------