      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSI077
      * ANALISTA..: F8658869 - Roberto Ioshimoto
      * AUTOR.....: F8658869 - Roberto Ioshimoto
      * OBJETIVO..: Inclusão na Tabela AUTZ_NEGO_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 23.06.2017 - F8658869 - DE 2016/145195 - IMPLANTAÇãO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSI077.

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
       77  CTE-INICIO-SS               PIC  X(040)  VALUE
                        '*** S.S. COMEÇA AQUI ***'.
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSI077 ***'.
       77  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
       COPY HLPKE01B.
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------

       77  SGTS014G                         PIC X(8) VALUE 'SGTS014G'.

      * BOOK TRATA MENSAGENS DE ERRO
       COPY IIBKERRO.
       COPY MSGKWMG1.
       COPY HLPKW999.

      * BOOK TRATA DADOS DO OPERADOR
       COPY IIBKOPRD.

      * Campos utilizados no container
       COPY SGTKERR1.

      * Area de trabalho
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

       01  QT-REG-SELD                      PIC S9(4) COMP-5 VALUE 0.
       COPY HLPKTIME.
       01  TS-SQL-ATU                       PIC  X(26) VALUE ' '.

      * Indicadores de nulidades

       01  TX-JST-FLZ-EXCO-N                PIC S9(4) COMP-5 VALUE 0.

      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG            PIC  9(10) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG           PIC  9(11) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG           PIC  9(5) VALUE 0.
           03  NR-SEQL-ORD-NEGO-LOG          PIC  9(10) VALUE 0.
           03  TS-AUTZ-NEGO-LOG              PIC  X(26) VALUE ' '.

      * Book dos dados do log
       01  DADOS-LOG.
       COPY SGTK014G.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  SGTSI077-DADOS.
       COPY SGTKI077.

       01  CTE-FINAL-SS                     PIC X(40) VALUE
               '*** S.S. TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      * Habilita tratamento automatico de Erros DB2
       COPY HLPKE997.
      * Habilita captura automatica de Abend e Erros CICS
       COPY HLPKE999.

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDA-CAMPOS
           PERFORM 150000-VALIDA-ORD-NEGO-OPR

           PERFORM 980000-INCLUIR-BANCO

           PERFORM 981000-GRAVA-LOG

           PERFORM 300000-GRAVAR-RESPOSTA

           GOBACK
           .

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL )
           END-EXEC

      * Se chegou até aqui, não há erro
           SET   SEM-ERRO              TO  TRUE
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

           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSI077-RQSC     TO QT-CCT-EPRO
           MOVE 'SGTSI077-RQSC'             TO NM-CTNR

           SET  GET-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS GET
              CONTAINER ( NM-CTNR       )
              CHANNEL   ( NM-CNL        )
              NODATA
              FLENGTH   ( QT-CCT-LIDO   )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA         TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSI077-RQSC )
              INTO      ( SGTSI077-RQSC           )
           END-EXEC

           SET  SEM-ERRO               TO TRUE
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDA-CAMPOS SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           IF  SGTSI077-TS-AUTZ-NEGO EQUAL 'CURRENT'
               PERFORM 140100-BUSCA-DATA-ATUAL
               MOVE HLPKE001-TS-FMTD   TO TS-SQL-ATU
               MOVE TS-SQL-ATU         TO SGTSI077-TS-AUTZ-NEGO
           END-IF

           IF  SGTSI077-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 002                 TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'    TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI077-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 003                 TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI077-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 005                 TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' informado.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI077-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 007                 TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI077-NR-SEQL-ORD-NEGO EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 542                 TO MSG1-CD-ERRO-TS
               MOVE 'Número sequencial ordem negociação não informado.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI077-TS-AUTZ-NEGO EQUAL SPACES OR
               SGTSI077-TS-AUTZ-NEGO EQUAL '0000-00-00-00.00.00.000000'
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 640                 TO MSG1-CD-ERRO-TS
               MOVE 'Data e hora atual da autorização negociação não inf
      -             'ormado.'           TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI077-CD-USU-RSP-AUTZ EQUAL SPACES
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 653                 TO MSG1-CD-ERRO-TS
               MOVE 'Código usuário responsavel autorização não informad
      -             'o.'                TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI077-IN-FLZ-EXCO-OPR EQUAL SPACES
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 654                 TO MSG1-CD-ERRO-TS
               MOVE 'Indicador flexibilização exceção operação não infor
      -             'mado.'             TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
      *    Trata nulidade
           IF  SGTSI077-TX-JST-FLZ-EXCO EQUAL SPACES
               MOVE -1                 TO TX-JST-FLZ-EXCO-N
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140100-BUSCA-DATA-ATUAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           CALL SBTIMEDT            USING HLPKE001-CD-FUC
                                          HLPKE001-AREA-TRB
                                          HLPKE001-FMT-TS
                                          HLPKE001-TAM-TS
                                          HLPKE001-TS-FMTD
           IF  RETURN-CODE      NOT EQUAL ZEROS
               SET  ERRO-PROGRAMA-ACIONADO     TO TRUE
               MOVE 050                 TO MSG1-CD-ERRO-TS
               MOVE 'Erro ao obter data e hora atual.'
                                        TO MSG1-TX-ERRO-TS
                                           MSG1-TX-RTN-PGM-ACND
               MOVE SBTIMEDT            TO MSG1-NM-PGM-ACND
               MOVE RETURN-CODE         TO MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALIDA-ORD-NEGO-OPR     SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO QT-REG-SELD
           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.ORD_NEGO_OPR
               WHERE CD_CTRA_TIT_VL     = :SGTSI077-CD-CTRA-TIT-VL
                 AND CD_OPR_ETRD_MCP    = :SGTSI077-CD-OPR-ETRD-MCP
                 AND NR_VRS_OPR_ETRD    = :SGTSI077-NR-VRS-OPR-ETRD
                 AND NR_SEQL_ORD_NEGO   = :SGTSI077-NR-SEQL-ORD-NEGO
           END-EXEC

           IF  QT-REG-SELD = 0
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 650                 TO MSG1-CD-ERRO-TS
               MOVE 'Ordem negociação operação inexistente.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       150000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-GRAVAR-RESPOSTA  SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           SET  PUT-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE
           MOVE 'SGTSI077-RPST'        TO NM-CTNR

           EXEC CICS PUT
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSI077-RPST )
              FROM      ( SGTSI077-RPST           )
           END-EXEC

           SET SEM-ERRO                TO TRUE
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980000-INCLUIR-BANCO SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO
      *
      * Desligar erro automatico do DB2
      *
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC
      *
           EXEC SQL
                INSERT INTO DB2SGT.AUTZ_NEGO_OPR_ETRD
                ( CD_CTRA_TIT_VL
                , CD_OPR_ETRD_MCP
                , NR_VRS_OPR_ETRD
                , NR_SEQL_ORD_NEGO
                , TS_AUTZ_NEGO
                , CD_USU_RSP_AUTZ
                , IN_FLZ_EXCO_OPR
                , TX_JST_FLZ_EXCO )
                VALUES
                ( :SGTSI077-CD-CTRA-TIT-VL
                , :SGTSI077-CD-OPR-ETRD-MCP
                , :SGTSI077-NR-VRS-OPR-ETRD
                , :SGTSI077-NR-SEQL-ORD-NEGO
                , :SGTSI077-TS-AUTZ-NEGO
                , :SGTSI077-CD-USU-RSP-AUTZ
                , :SGTSI077-IN-FLZ-EXCO-OPR
                , :SGTSI077-TX-JST-FLZ-EXCO
                  :TX-JST-FLZ-EXCO-N )
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
                   MOVE 'Inclusão não efetuada, registro existente.'
                                        TO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               WHEN 0
                    MOVE 'Inclusão efetuada com sucesso.'
                                        TO SGTSI077-TX-MSG-RTN
               WHEN OTHER
      * Aciona o tratamento padrão de erro para os demais casos
                    GO TO 999010-TRATA-ERRO-DB2
           END-EVALUATE
           .
       980000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       981000-GRAVA-LOG SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE DADOS-LOG
           MOVE 'I'                         TO SGTS014G-CD-TIP-ACAO
           MOVE 'AUTZ_NEGO_OPR_ETRD'        TO SGTS014G-NM-ENT
           IF  EXECUCAO-ONLINE
               PERFORM 981100-LE-DADOS-OPERADOR
               MOVE  KOPRD-CD-OPRD          TO SGTS014G-CD-CHV-USU
           ELSE
               MOVE 'SYSTEM'                TO SGTS014G-CD-CHV-USU
           END-IF
           MOVE 'SGTSI077'                  TO SGTS014G-NM-PGM
           MOVE SGTSI077-CD-CTRA-TIT-VL     TO CD-CTRA-TIT-VL-LOG
           MOVE SGTSI077-CD-OPR-ETRD-MCP    TO CD-OPR-ETRD-MCP-LOG
           MOVE SGTSI077-NR-VRS-OPR-ETRD    TO NR-VRS-OPR-ETRD-LOG
           MOVE SGTSI077-NR-SEQL-ORD-NEGO   TO NR-SEQL-ORD-NEGO-LOG
           MOVE SGTSI077-TS-AUTZ-NEGO       TO TS-AUTZ-NEGO-LOG
           STRING CD-CTRA-TIT-VL-LOG
           ';'    CD-OPR-ETRD-MCP-LOG
           ';'    NR-VRS-OPR-ETRD-LOG
           ';'    NR-SEQL-ORD-NEGO-LOG
           ';'    TS-AUTZ-NEGO-LOG
               DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR

           MOVE LENGTH OF DADOS-LOG    TO EIBCALEN
           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM  NOT EQUAL ZEROS
               SET   ERRO-PROGRAMA-ACIONADO TO  TRUE
               MOVE  112                    TO  MSG1-CD-ERRO-TS
               MOVE 'Erro na gravação das informações do Log.'
                                            TO  MSG1-TX-ERRO-TS
               MOVE  SGTS014G               TO  MSG1-NM-PGM-ACND
               MOVE  SGTS014G-MSG-ERRO-PGM  TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTS014G-CD-RTN-PGM    TO  MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       981000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       981100-LE-DADOS-OPERADOR        SECTION.
      *----------------------------------------------------------------*
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Lê o container com dados do usuário
      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  GET-CONTAINER          TO  TRUE
           MOVE 'PRM-INF-OPRD'         TO  NM-CTNR

           SET  ERRO-CONTAINER         TO  TRUE

           EXEC CICS GET
                     CONTAINER ( NM-CTNR                )
                     INTO      ( PRM-INF-OPRD           )
                     FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO              TO  TRUE

           .
       981100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------
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

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                        TO MSG1-TX-ERRO-TS
           END-IF

           MOVE 'SGTSI077-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI077-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI077-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
           IF  SGTSI077-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE  SGTSI077-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX         TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI077-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI077-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI077-NR-SEQL-ORD-NEGO'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI077-NR-SEQL-ORD-NEGO    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI077-TS-AUTZ-NEGO'      TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI077-TS-AUTZ-NEGO        TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI077-CD-USU-RSP-AUTZ'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI077-CD-USU-RSP-AUTZ     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI077-IN-FLZ-EXCO-OPR'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI077-IN-FLZ-EXCO-OPR     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI077-TX-JST-FLZ-EXCO'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI077-TX-JST-FLZ-EXCO     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS              TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS              TO IIBKERRO-TX-ERRO

           EXEC CICS PUT
              CONTAINER ( 'PRM-ERRO'                  )
              CHANNEL   ( NM-CNL                      )
              FROM      ( IIBKERRO-PRM-ERRO           )
              FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
              NOHANDLE
           END-EXEC

           PERFORM 999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      * --------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      * --------------------------------------*
       COPY HLPKP999.

      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      * --------------------------------------*
       COPY SGTKP999.

      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
       COPY HLPKP997.

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
       COPY MSGKPMG1.
      * --------------------------------------*
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
