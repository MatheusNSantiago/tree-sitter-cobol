      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSI117
      * ANALISTA..: F2804421 - ELIANA CRISTINA RAMOS DA SILVA
      * AUTOR.....: F2804421 - ELIANA CRISTINA RAMOS DA SILVA
      * OBJETIVO..: INCLUSAO DA TABELA JST_VLDC_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 12.11.2018 - F2804421 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSI117.
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

       77  CTE-PROG                PIC  X(17) VALUE '*** SGTSI117 ***'.
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

-INC HLPKTIME
       01  TS-SQL-ATU                       PIC  X(26) VALUE ' '.

      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG            PIC  9(10) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG           PIC  9(11) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG           PIC  9(5) VALUE 0.
           03  CD-TIP-VLDC-OPR-LOG           PIC  9(5) VALUE 0.

      * Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      * Book deste programa
-INC SGTKI117
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
              CHANNEL   ( NM-CNL     )
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

           MOVE ZEROS                  TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSI117-RQSC
                                       TO QT-CCT-EPRO

           SET  GET-CONTAINER          TO  TRUE
           SET  ERRO-CONTAINER         TO  TRUE
           MOVE 'SGTSI117-RQSC'        TO  NM-CTNR

           EXEC CICS GET
              CONTAINER ( NM-CTNR     )
              CHANNEL   ( NM-CNL      )
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
              FLENGTH ( LENGTH OF SGTSI117-RQSC )
              INTO    ( SGTSI117-RQSC )
           END-EXEC


      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDA-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           PERFORM 141000-BUSCA-TS-DT
           MOVE HLPKE001-TS-FMTD       TO TS-SQL-ATU

           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           IF  SGTSI117-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 002                 TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'    TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI117-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 003                 TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI117-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 005                 TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' informado.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI117-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 007                 TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI117-CD-TIP-VLDC-OPR EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 861                 TO MSG1-CD-ERRO-TS
               MOVE 'Código do parâmetro da justificativa não informado.
      -             ''                  TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI117-CD-USU-ULT-ATL EQUAL SPACES
               SET  ERRO-PROGRAMADO           TO TRUE
               MOVE 278                       TO  MSG1-CD-ERRO-TS
               MOVE 'Código do usuário responsável pela atualização não
      -        'informado.'                   TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI117-TX-JST-VLDC-OPR EQUAL SPACES
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 861                 TO MSG1-CD-ERRO-TS
               MOVE 'Código do parâmetro da justificativa não informado.
      -             ''                  TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI117-TS-ULT-ATL EQUAL SPACES
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 106                 TO MSG1-CD-ERRO-TS
               MOVE 'Timestamp última atualização não informado.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           IF  SGTSI117-TS-ULT-ATL EQUAL 'CURRENT'
               MOVE TS-SQL-ATU         TO SGTSI117-TS-ULT-ATL
           END-IF
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       141000-BUSCA-TS-DT SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO
           CALL SBTIMEDT            USING HLPKE001-CD-FUC
                                          HLPKE001-AREA-TRB
                                          HLPKE001-FMT-TS
                                          HLPKE001-TAM-TS
                                          HLPKE001-TS-FMTD
           IF  RETURN-CODE      NOT EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO  TRUE
               MOVE 050                 TO  MSG1-CD-ERRO-TS
               MOVE 'Erro ao obter data e hora atual.'
                                        TO MSG1-TX-ERRO-TS
               MOVE SBTIMEDT            TO MSG1-NM-PGM-ACND
               PERFORM 999100-MONITORAR-VRV-PGM-CHMD
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       141000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-INCLUIR SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO
      *
      * Desligar erro automatico do DB2
      *
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC
      *
           EXEC SQL
                INSERT INTO DB2SGT.JST_VLDC_OPR_ETRD
                ( CD_CTRA_TIT_VL
                , CD_OPR_ETRD_MCP
                , NR_VRS_OPR_ETRD
                , CD_TIP_VLDC_OPR
                , CD_USU_ULT_ATL
                , TX_JST_VLDC_OPR
                , TS_ULT_ATL )
                VALUES
                ( :SGTSI117-CD-CTRA-TIT-VL
                , :SGTSI117-CD-OPR-ETRD-MCP
                , :SGTSI117-NR-VRS-OPR-ETRD
                , :SGTSI117-CD-TIP-VLDC-OPR
                , :SGTSI117-CD-USU-ULT-ATL
                , :SGTSI117-TX-JST-VLDC-OPR
                , :SGTSI117-TS-ULT-ATL )
           END-EXEC
      *
      * Ligar erro automatico do DB2
      *
           EXEC SQL WHENEVER SQLERROR
                GO TO 999010-TRATA-ERRO-DB2
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN -803
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 803                 TO MSG1-CD-ERRO-TS
               MOVE 'Inclusao nao efetuada, registro existente'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               WHEN 0
                    CONTINUE
               WHEN OTHER
      * Aciona o tratamento padrao de erro padrao para os demais casos
                    GO TO 999010-TRATA-ERRO-DB2
           END-EVALUATE

           INITIALIZE DADOS-LOG
           MOVE 'I'                    TO SGTS014G-CD-TIP-ACAO

           PERFORM 980102-GRAVA-LOG
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           MOVE  'Inclusão efetuada com sucesso.'
                                       TO  SGTSI117-TX-RTN-PRCT
           SET  PUT-CONTAINER          TO  TRUE
           SET  ERRO-CONTAINER         TO  TRUE

           MOVE 'SGTSI117-RPST'        TO  NM-CTNR

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                )
                      CHANNEL   ( NM-CNL                 )
                      FLENGTH   ( LENGTH OF SGTSI117-RPST)
                      FROM      ( SGTSI117-RPST          )
           END-EXEC

           SET  SEM-ERRO               TO  TRUE

           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980102-GRAVA-LOG SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO
           MOVE 'JST_VLDC_OPR_ETRD '   TO SGTS014G-NM-ENT

           MOVE SGTSI117-CD-USU-ULT-ATL
                                       TO SGTS014G-CD-CHV-USU

           MOVE 'SGTSI117'             TO SGTS014G-NM-PGM
           MOVE SGTSI117-CD-CTRA-TIT-VL
                                       TO CD-CTRA-TIT-VL-LOG
           MOVE SGTSI117-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-LOG
           MOVE SGTSI117-NR-VRS-OPR-ETRD
                                       TO NR-VRS-OPR-ETRD-LOG
           MOVE SGTSI117-CD-TIP-VLDC-OPR
                                       TO CD-TIP-VLDC-OPR-LOG
           STRING CD-CTRA-TIT-VL-LOG
           ';'    CD-OPR-ETRD-MCP-LOG
           ';'    NR-VRS-OPR-ETRD-LOG
           ';'    CD-TIP-VLDC-OPR-LOG
               DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR

           MOVE LENGTH OF DADOS-LOG    TO EIBCALEN
           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM  NOT EQUAL ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  112               TO  MSG1-CD-ERRO-TS
               MOVE 'Erro na gravação das informações do Log.'
                                       TO  MSG1-TX-ERRO-TS
               MOVE  SGTS014G          TO  MSG1-NM-PGM-ACND
               MOVE  SGTS014G-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTS014G-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM 999100-MONITORAR-VRV-PGM-CHMD
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       980102-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
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

           MOVE 'SGTSI117-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI117-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI117-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI117-CD-OPR-ETRD-MCP     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI117-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI117-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI117-CD-TIP-VLDC-OPR'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI117-CD-TIP-VLDC-OPR     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI117-CD-USU-ULT-ATL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI117-CD-USU-ULT-ATL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI117-TX-JST-VLDC-OPR'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI117-TX-JST-VLDC-OPR     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI117-TS-ULT-ATL'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI117-TS-ULT-ATL          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO
           EXEC CICS PUT
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   ( NM-CNL     )
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

           EVALUATE  MSG1-NM-PGM-ACND

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

      * --------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * --------------------------------------*
-INC HLPKP999
      *----------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
-INC SGTKP999
      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997
      * --------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
-INC MSGKPMG1
      * --------------------------------------*
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC.