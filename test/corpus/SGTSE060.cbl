      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSE060
      * ANALISTA..: F4535118 - IVONE DE MELO COSTA RIBEIRO
      * AUTOR.....: F4535118 - IVONE DE MELO COSTA RIBEIRO
      * OBJETIVO..: EXCLUIR CONTAINER DA TABELA ANX_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 31/01/2017 F8785286 DE 2016/145195 - Inclusão leitura
      *              container operador para gravação no LOG.
      *        31.10.2016 - F4535118 DE 2015/137233 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSE060.

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
       77  CTE-INICIO-SS                 PIC  X(040)  VALUE
                   '*** S.S. COMEÇA AQUI ***'.
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSE060 ***'.
       77  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informações
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP              PIC  9(11).

      * Book de para acionamento do MSG1
-INC IIBKERRO

-INC MSGKWMG1

-INC HLPKW999

      * Área de trabalho

      * Indicadores de nulidades

      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG            PIC  9(10) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG           PIC  9(11) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG           PIC  9(5) VALUE 0.
           03  CD-ARQ-OPR-ETRD-LOG           PIC  9(10) VALUE 0.

      * Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

       01  SGTS014G VALUE 'SGTS014G'         PIC X(8).

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

-INC SGTKE060

001400* Book dos dados do usuário
-INC IIBKOPRD

000600* Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
           IF NOT EXECUCAO-ONLINE AND NOT  EXECUCAO-BATCH
              CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

           MOVE 0001                   TO  MSG1-CD-SEQL-ERRO
      * Habilita tratamento automatico de Erros DB2
-INC HLPKE997
      * Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDA-PARAMETRO

           PERFORM 200000-EXCLUIR
           PERFORM 300000-POSTA-RESPOSTA

           GOBACK
           .
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO  MSG1-CD-SEQL-ERRO

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
           MOVE 0004                   TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           MOVE ZEROS                      TO  QT-CCT-LIDO
           MOVE LENGTH OF SGTSE060-RQSC    TO  QT-CCT-EPRO

           EXEC CICS GET
              CONTAINER ( 'SGTSE060-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO   NOT   EQUAL   QT-CCT-EPRO
               SET  ERRO-PROGRAMADO    TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE

           EXEC CICS GET
              CONTAINER ( 'SGTSE060-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSE060-RQSC )
              INTO      ( SGTSE060-RQSC           )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDA-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO  MSG1-CD-SEQL-ERRO

           IF  SGTSE060-CD-CTRA-TIT-VL LESS ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 001                TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'inválido.'        TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE060-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 002                TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'   TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE060-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 003                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturado mercado de capitais não
      -        ' numérico.'                TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE060-CD-OPR-ETRD-MCP    LESS ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 004                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturado mercado de capitais inv
      -        'álido.'                    TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE060-CD-OPR-ETRD-MCP    EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 005                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturado mercado de capitais não
      -        ' informado.'               TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE060-NR-VRS-OPR-ETRD    LESS ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 006                    TO  MSG1-CD-ERRO-TS
               MOVE 'Número de versão da operação estruturado inválido.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE060-NR-VRS-OPR-ETRD    EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 007                    TO  MSG1-CD-ERRO-TS
               MOVE 'Número de versão da operação estruturado não inform
      -             'ado.'                 TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

            IF  SGTSE060-CD-ARQ-OPR-ETRD    EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 149                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código arquivo operação estruturado não informado.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE060-CD-ARQ-OPR-ETRD    LESS ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 150                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código arquivo operação estruturado inválido.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-EXCLUIR SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO  MSG1-CD-SEQL-ERRO

           INITIALIZE DADOS-LOG
           MOVE 'E'                    TO  SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG

           EXEC SQL
                DELETE FROM DB2SGT.ANX_OPR_ETRD
                WHERE CD_CTRA_TIT_VL    =  :SGTSE060-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP   =  :SGTSE060-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD   =  :SGTSE060-NR-VRS-OPR-ETRD
                  AND CD_ARQ_OPR_ETRD   =  :SGTSE060-CD-ARQ-OPR-ETRD
           END-EXEC

           IF  SQLCODE EQUAL +100
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 100                    TO  MSG1-CD-ERRO-TS
               MOVE 'Registro inexistente.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'Exclusão efetuada com sucesso.'
                                       TO  SGTSE060-TX-RTN-PRCT

           EXEC CICS PUT
              CONTAINER ( 'SGTSE060-RPST'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSE060-RPST )
              FROM      ( SGTSE060-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980102-GRAVA-LOG SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'ANX_OPR_ETRD      '   TO  SGTS014G-NM-ENT

           IF  EXECUCAO-ONLINE
               PERFORM 980103-LER-DADOS-OPRD
               MOVE  KOPRD-CD-OPRD         TO  SGTS014G-CD-CHV-USU
           ELSE
               MOVE 'SYSTEM'           TO  SGTS014G-CD-CHV-USU
           END-IF

           MOVE 'SGTSE060'                 TO  SGTS014G-NM-PGM
           MOVE SGTSE060-CD-CTRA-TIT-VL    TO  CD-CTRA-TIT-VL-LOG
           MOVE SGTSE060-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD-MCP-LOG
           MOVE SGTSE060-NR-VRS-OPR-ETRD   TO  NR-VRS-OPR-ETRD-LOG
           MOVE SGTSE060-CD-ARQ-OPR-ETRD   TO  CD-ARQ-OPR-ETRD-LOG

           STRING    CD-CTRA-TIT-VL-LOG
              ';'    CD-OPR-ETRD-MCP-LOG
              ';'    NR-VRS-OPR-ETRD-LOG
              ';'    CD-ARQ-OPR-ETRD-LOG
               DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR
           END-STRING

           MOVE LENGTH OF DADOS-LOG    TO  EIBCALEN
           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM    NOT  EQUAL ZEROS
               SET  ERRO-PROGRAMA-ACIONADO TO  TRUE
               MOVE  NM-PGM                TO  MSG1-NM-PGM-ACND
               MOVE 112                    TO  MSG1-CD-ERRO-TS
               MOVE 'Erro na gravação das informações do Log.'
                                           TO  MSG1-TX-ERRO-TS
               MOVE SGTS014G-CD-RTN-PGM    TO  MSG1-CD-RTN-PGM-ACND
               MOVE SGTS014G-MSG-ERRO-PGM  TO  MSG1-TX-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       980102-SAI.
           EXIT.

008160*----------------------------------------------------------------*
008170 980103-LER-DADOS-OPRD           SECTION.
008180*----------------------------------------------------------------*
008190     MOVE 0009                   TO  MSG1-CD-SEQL-ERRO
008200
008210*Configura dados para leitura do container de requisição
008220     SET  GET-CONTAINER          TO  TRUE
008230     MOVE 'PRM-INF-OPRD'         TO  NM-CTNR
008240
008250*Por causa da captura automática de erro CICS, deve ser previsto
008260*o erro de container
008270     SET  ERRO-CONTAINER         TO  TRUE
008280
008290     INITIALIZE PRM-INF-OPRD
008300     EXEC CICS GET
008310          CONTAINER ( NM-CTNR                )
008320          INTO      ( PRM-INF-OPRD           )
008330          FLENGTH   ( LENGTH OF PRM-INF-OPRD )
008340     END-EXEC.
008350
008360*    Ao chegar aqui, há certeza de que não houve erro algum
008370     SET  SEM-ERRO               TO  TRUE
008380     .
008390 980103-SAI.
008400     EXIT.

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

           MOVE 'SGTSE060-CD-CTRA-TIT-VL'    TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSE060-CD-CTRA-TIT-VL      TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE060-CD-OPR-ETRD-MCP'   TO  MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSE060-CD-OPR-ETRD-MCP      IS NUMERIC
               MOVE  SGTSE060-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX         TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE060-NR-VRS-OPR-ETRD'   TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSE060-NR-VRS-OPR-ETRD     TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE060-CD-ARQ-OPR-ETRD'   TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSE060-CD-ARQ-OPR-ETRD     TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS              TO  IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS              TO  IIBKERRO-TX-ERRO

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