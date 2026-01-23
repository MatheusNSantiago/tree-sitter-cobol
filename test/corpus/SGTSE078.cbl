      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSE078
      * ANALISTA..: F8658869 - Roberto Ioshimoto
      * AUTOR.....: F8658869 - Roberto Ioshimoto
      * OBJETIVO..: Exclusão na Tabela EST_ORD_NEGO_OPR
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 26.06.2017 - F8658869 - 2016/D145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSE078.

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
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSE078 ***'.
       77  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------

       77  SGTS014G                          PIC X(8) VALUE 'SGTS014G'.

      * Book trata mensagens de erro
-INC IIBKERRO
-INC MSGKWMG1
-INC HLPKW999

      * Book trata dados do container
-INC SGTKERR1

      * Book trata dados do operador
-INC IIBKOPRD

      * Area de trabalho
      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).
      * Indicadores de nulidades

      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG            PIC  9(10) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG           PIC  9(11) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG           PIC  9(5) VALUE 0.
           03  NR-SEQL-ORD-NEGO-LOG          PIC  9(10) VALUE 0.
           03  TS-EST-ORD-NEGO-LOG           PIC  X(26) VALUE ' '.

      * Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  SGTSE078-DADOS.
-INC SGTKE078

       01  CTE-FINAL-SS                     PIC X(40) VALUE
               '*** S.S. TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      * Habilita tratamento automático de Erros DB2
-INC HLPKE997
      * Habilita captura automática de Abend e Erros CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDA-PARAMETRO

           PERFORM 980000-GRAVA-LOG

           PERFORM 981000-EXCLUIR-BANCO

           PERFORM 300000-GRAVAR-RESPOSTA

           GOBACK
           .

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           SET ERRO-CANAL TO TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL )
           END-EXEC

      * Se chegou até aqui, não há erro
           SET  SEM-ERRO  TO  TRUE
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
           MOVE LENGTH OF SGTSE078-RQSC     TO QT-CCT-EPRO

           SET  GET-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE
           MOVE 'SGTSE078-RQSC'        TO NM-CTNR

           EXEC CICS GET
              CONTAINER ( NM-CTNR         )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA         TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSE078-RQSC )
              INTO      ( SGTSE078-RQSC           )
           END-EXEC

      ** Este ponto indica que não houve erro de cics.
           SET SEM-ERRO        TO TRUE
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDA-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           IF  SGTSE078-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 002                 TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'    TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE078-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 003                 TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturado mercado de capitais não
      -        ' numérico.'             TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE078-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 005                 TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturado mercado de capitais não
      -        ' informado.'            TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE078-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 007                 TO MSG1-CD-ERRO-TS
               MOVE 'Número versao operação estruturado não informado.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE078-NR-SEQL-ORD-NEGO EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 542                 TO MSG1-CD-ERRO-TS
               MOVE 'Número sequencial ordem negociação não informado.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE078-TS-EST-ORD-NEGO EQUAL SPACES OR
               SGTSE078-TS-EST-ORD-NEGO
                       EQUAL  '0000-00-00-00.00.00.000000'
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 543                 TO MSG1-CD-ERRO-TS
               MOVE 'Data e hora do estado ordem negociação não informad
      -             'o.'                TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-GRAVAR-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           SET  PUT-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE
           MOVE 'SGTSE078-RPST'        TO NM-CTNR

           EXEC CICS PUT
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSE078-RPST )
              FROM      ( SGTSE078-RPST           )
           END-EXEC

           SET SEM-ERRO                TO TRUE
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980000-GRAVA-LOG SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE DADOS-LOG
           MOVE 'E'                         TO SGTS014G-CD-TIP-ACAO
           MOVE 'EST_ORD_NEGO_OPR  '        TO SGTS014G-NM-ENT
           IF  EXECUCAO-ONLINE
               PERFORM 980100-LE-DADOS-OPERADOR
               MOVE  KOPRD-CD-OPRD          TO SGTS014G-CD-CHV-USU
           ELSE
               MOVE 'SYSTEM'                TO SGTS014G-CD-CHV-USU
           END-IF
           MOVE 'SGTSE078'                  TO SGTS014G-NM-PGM
           MOVE SGTSE078-CD-CTRA-TIT-VL     TO CD-CTRA-TIT-VL-LOG
           MOVE SGTSE078-CD-OPR-ETRD-MCP    TO CD-OPR-ETRD-MCP-LOG
           MOVE SGTSE078-NR-VRS-OPR-ETRD    TO NR-VRS-OPR-ETRD-LOG
           MOVE SGTSE078-NR-SEQL-ORD-NEGO   TO NR-SEQL-ORD-NEGO-LOG
           MOVE SGTSE078-TS-EST-ORD-NEGO    TO TS-EST-ORD-NEGO-LOG
           STRING CD-CTRA-TIT-VL-LOG
           ';'    CD-OPR-ETRD-MCP-LOG
           ';'    NR-VRS-OPR-ETRD-LOG
           ';'    NR-SEQL-ORD-NEGO-LOG
           ';'    TS-EST-ORD-NEGO-LOG
               DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR
           END-STRING

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
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       980000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       980100-LE-DADOS-OPERADOR        SECTION.
      *----------------------------------------------------------------*
           MOVE 0008                   TO  MSG1-CD-SEQL-ERRO

           INITIALIZE IIBKERRO-PRM-ERRO

           SET  GET-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE
           MOVE 'PRM-INF-OPRD'         TO NM-CTNR

      *--> Lê o container com dados do usuário
      *--> não há captura automática de erro CICS
           EXEC CICS GET
                     CONTAINER ( NM-CTNR                )
                     INTO      ( PRM-INF-OPRD           )
                     FLENGTH   ( LENGTH OF PRM-INF-OPRD )
                     NOHANDLE
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       980100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       981000-EXCLUIR-BANCO SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                DELETE FROM DB2SGT.EST_ORD_NEGO_OPR
                WHERE CD_CTRA_TIT_VL     = :SGTSE078-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP    = :SGTSE078-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD    = :SGTSE078-NR-VRS-OPR-ETRD
                  AND NR_SEQL_ORD_NEGO   = :SGTSE078-NR-SEQL-ORD-NEGO
                  AND TS_EST_ORD_NEGO    = :SGTSE078-TS-EST-ORD-NEGO
           END-EXEC

           IF  SQLCODE EQUAL +100
               SET  ERRO-PROGRAMADO          TO TRUE
               MOVE 100                      TO MSG1-CD-ERRO-TS
               MOVE 'Registro inexistente.'  TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
               IF  SQLCODE EQUAL  ZEROS
                   MOVE 'Exclusão efetuada com sucesso.'
                                             TO SGTSE078-TX-MSG-RTN
               ELSE
                   SET  ERRO-PROGRAMADO      TO TRUE
                   MOVE 541                  TO MSG1-CD-ERRO-TS
                   MOVE 'Erro na rotina de exclusão.'
                                             TO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           END-IF
           .
       981000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                        TO MSG1-TX-ERRO-TS
           END-IF

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

           MOVE 'SGTSE078-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSE078-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE078-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSE078-CD-OPR-ETRD-MCP        IS NUMERIC
               MOVE  SGTSE078-CD-OPR-ETRD-MCP  TO CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX        TO MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'      TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE078-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSE078-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE078-NR-SEQL-ORD-NEGO'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSE078-NR-SEQL-ORD-NEGO    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE078-TS-EST-ORD-NEGO'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSE078-TS-EST-ORD-NEGO     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO

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
      * --------------------------------------*
-INC MSGKPMG1
      * --------------------------------------*
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.