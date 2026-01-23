      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSI080
      * ANALISTA..: F0175702 - ADRIANO SEIJI NAKAMURA
      * AUTOR.....: F0175702 - ADRIANO SEIJI NAKAMURA
      * OBJETIVO..: INCLUSAO DA TABELA ISLC_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 16.03.2017 - F0175702 DE 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSI080.

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
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSI080 ***'.
       77  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

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

      * Area de trabalho
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

       01  QT-REG-SELD                 PIC S9(4) COMP-5 VALUE 0.
-INC HLPKTIME

       01  DT-VNCT-OPR                 PIC  X(10).
       01  DT-VNCT-OPR-R               REDEFINES DT-VNCT-OPR.
           03  DIA                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  MES                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  ANO                     PIC  9(04).
       01  DT-CPR-OPR                  PIC  X(10).
       01  DT-CPR-OPR-R                REDEFINES DT-CPR-OPR.
           03  DIA                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  MES                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  ANO                     PIC  9(04).
       01  CD-CTRA-TIT-VL              PIC S9(9) COMP-5.
       01  NR-VRS-OPR-ETRD             PIC S9(4) COMP-5.
       01  DT-INC-ISLC                 PIC  X(10) VALUE SPACES.
       01  DT-FIM-ISLC                 PIC  X(10) VALUE SPACES.
       01  DT-SQL-ATU                  PIC  X(10)
                                       VALUE '  .  .    '.
       01  DT-SQL-ATU-R REDEFINES DT-SQL-ATU.
           03  DIA                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  MES                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03 ANO                      PIC  9(04).
       01  TS-SQL-ATU                  PIC  X(26) VALUE ' '.
       01  DT-AUX-DMA                  PIC  X(10) VALUE ' '.
       01  DT-AUX-DMA-R                REDEFINES DT-AUX-DMA.
           02  DIA                     PIC  X(2).
           02  PTO1                    PIC  X(1).
           02  MES                     PIC  X(2).
           02  PTO2                    PIC  X(1).
           02  ANO                     PIC  X(4).
       01  AUX-DT-08                   PIC  X(08) VALUE SPACES.
       01  AUX-DT-08-R                 REDEFINES AUX-DT-08.
           03  DIA                     PIC  9(2).
           03  MES                     PIC  9(2).
           03  ANO                     PIC  9(4).
       01  DT-INC-ISLC-INV              PIC  X(08) VALUE SPACES.
       01  DT-INC-ISLC-INV-R            REDEFINES DT-INC-ISLC-INV.
           03  ANO                       PIC  9(4).
           03  MES                       PIC  9(2).
           03  DIA                       PIC  9(2).
       01  DT-VNCT-OPR-INV              PIC  X(08) VALUE SPACES.
       01  DT-VNCT-OPR-INV-R            REDEFINES DT-VNCT-OPR-INV.
           03  ANO                      PIC  9(4).
           03  MES                      PIC  9(2).
           03  DIA                      PIC  9(2).
       01  DT-CPR-OPR-INV               PIC  X(08) VALUE SPACES.
       01  DT-CPR-OPR-INV-R             REDEFINES DT-CPR-OPR-INV.
           03  ANO                      PIC  9(4).
           03  MES                      PIC  9(2).
           03  DIA                      PIC  9(2).
       01  DT-FIM-ISLC-INV              PIC  X(08) VALUE SPACES.
       01  DT-FIM-ISLC-INV-R            REDEFINES DT-FIM-ISLC-INV.
           03  ANO                      PIC  9(4).
           03  MES                      PIC  9(2).
           03  DIA                      PIC  9(2).

      * Indicadores de nulidades

       01  CD-ARQ-OPR-ETRD-N                PIC S9(4) COMP-5 VALUE 0.
       01  DT-FIM-ISLC-OPR-N                PIC S9(4) COMP-5 VALUE 0.
       01  TX-DCR-ISLC-OPR-N                PIC S9(4) COMP-5 VALUE 0.

      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG            PIC  9(10) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG           PIC  9(11) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG           PIC  9(5) VALUE 0.
           03  NR-ISLC-OPR-LOG               PIC  9(10) VALUE 0.

      * Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G
      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      * Book dos dados do usuário
-INC IIBKOPRD

       01  SGTS014G VALUE 'SGTS014G'         PIC X(8).

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

-INC SGTKI080

       01  CTE-FINAL-SS                     PIC X(40) VALUE
               '*** S.S. TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
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
           PERFORM 980103-LER-DADOS-OPRD

           PERFORM 200000-INCLUIR
           MOVE 'Inclusão efetuada com sucesso.'
             TO SGTSI080-TX-RTN-PRCT
           PERFORM 300000-POSTA-RESPOSTA

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
           MOVE LENGTH OF SGTSI080-RQSC     TO QT-CCT-EPRO

           SET  GET-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS GET
              CONTAINER ( 'SGTSI080-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           SET  SEM-ERRO               TO TRUE

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           SET  GET-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS GET
              CONTAINER ( 'SGTSI080-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSI080-RQSC )
              INTO      ( SGTSI080-RQSC           )
           END-EXEC

           SET  SEM-ERRO               TO TRUE
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDA-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           PERFORM 150000-VALIDA-CAMPOS

           PERFORM 160000-TRATA-NULIDADE
           IF  CD-ARQ-OPR-ETRD-N     EQUAL 0
               PERFORM 170001-VALIDA-ARQ-OPR-ETRD
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALIDA-CAMPOS SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           PERFORM 151000-BUSCA-TS-DT
           MOVE CORR HLPKE001-TS-FMTD  TO DT-SQL-ATU-R
           MOVE HLPKE001-TS-FMTD       TO TS-SQL-ATU

           IF  SGTSI080-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 002                 TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'    TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI080-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 003                 TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'        TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI080-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 005                 TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' informado.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI080-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 007                 TO  MSG1-CD-ERRO-TS
               MOVE 'Numero de versao da operacao não informado.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI080-IN-ISLC-OPR-ATI EQUAL SPACES
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 631                 TO  MSG1-CD-ERRO-TS
               MOVE 'Indicador insolvência operação ativa não informado.
      -             ''                  TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI080-DT-INC-ISLC-OPR EQUAL SPACES
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 632                 TO  MSG1-CD-ERRO-TS
               MOVE 'Data inicio insolvência operação não informado.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
               MOVE SGTSI080-DT-INC-ISLC-OPR TO AUX-DT-08
               MOVE CORR AUX-DT-08-R   TO DT-AUX-DMA-R
               MOVE '.'                TO PTO1 PTO2
               MOVE DT-AUX-DMA-R       TO DT-INC-ISLC
               MOVE CORR AUX-DT-08-R   TO DT-INC-ISLC-INV-R
           END-IF

           IF  SGTSI080-DT-FIM-ISLC-OPR NOT EQUAL SPACES
               MOVE SGTSI080-DT-FIM-ISLC-OPR TO AUX-DT-08
               MOVE CORR AUX-DT-08-R   TO DT-AUX-DMA-R
               MOVE '.'                TO PTO1 PTO2
               MOVE DT-AUX-DMA-R       TO DT-FIM-ISLC
               MOVE CORR AUX-DT-08-R   TO DT-FIM-ISLC-INV-R
           END-IF

           IF  SGTSI080-TS-ULT-ATL EQUAL 'CURRENT'
               MOVE TS-SQL-ATU         TO SGTSI080-TS-ULT-ATL
           END-IF
           IF  SGTSI080-DT-INC-ISLC-OPR EQUAL 'CURRENT'
               MOVE DT-SQL-ATU         TO SGTSI080-DT-INC-ISLC-OPR
           END-IF
           IF  SGTSI080-DT-FIM-ISLC-OPR EQUAL 'CURRENT'
               MOVE DT-SQL-ATU         TO SGTSI080-DT-FIM-ISLC-OPR
           END-IF
           .
       150000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       151000-BUSCA-TS-DT SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO
           CALL SBTIMEDT            USING HLPKE001-CD-FUC
                                          HLPKE001-AREA-TRB
                                          HLPKE001-FMT-TS
                                          HLPKE001-TAM-TS
                                          HLPKE001-TS-FMTD
           IF  RETURN-CODE      NOT EQUAL ZEROS
               SET  ERRO-PROGRAMA-ACIONADO     TO TRUE
               MOVE 050                        TO MSG1-CD-ERRO-TS
               MOVE 'Erro ao obter data e hora atual.'
                                               TO MSG1-TX-ERRO-TS
                                                  MSG1-TX-RTN-PGM-ACND
               MOVE SBTIMEDT                   TO MSG1-NM-PGM-ACND
               MOVE RETURN-CODE                TO MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       151000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       160000-TRATA-NULIDADE SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           IF  SGTSI080-CD-ARQ-OPR-ETRD EQUAL ZEROS
               MOVE -1                 TO CD-ARQ-OPR-ETRD-N
           END-IF

           IF  SGTSI080-DT-FIM-ISLC-OPR EQUAL SPACES
               MOVE -1                 TO DT-FIM-ISLC-OPR-N
           END-IF

           IF  SGTSI080-TX-DCR-ISLC-OPR EQUAL SPACES
               MOVE -1                 TO TX-DCR-ISLC-OPR-N
           END-IF
           .
       160000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170001-VALIDA-ARQ-OPR-ETRD     SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO QT-REG-SELD
           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.ARQ_OPR_ETRD
               WHERE CD_ARQ_OPR_ETRD    = :SGTSI080-CD-ARQ-OPR-ETRD
           END-EXEC

           IF  QT-REG-SELD = 0
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 151                 TO  MSG1-CD-ERRO-TS
               MOVE 'Arquivo operação estruturado inexistente.'
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

           PERFORM 430000-BUSCA-NR-SEQUENCIA
           EXEC SQL
                INSERT INTO DB2SGT.ISLC_OPR_ETRD
                ( CD_CTRA_TIT_VL
                , CD_OPR_ETRD_MCP
                , NR_VRS_OPR_ETRD
                , NR_ISLC_OPR
                , CD_USU_ULT_ATL
                , TS_ULT_ATL
                , CD_ARQ_OPR_ETRD
                , IN_ISLC_OPR_ATI
                , DT_INC_ISLC_OPR
                , DT_FIM_ISLC_OPR
                , TX_DCR_ISLC_OPR )
                VALUES
                ( :SGTSI080-CD-CTRA-TIT-VL
                , :SGTSI080-CD-OPR-ETRD-MCP
                , :SGTSI080-NR-VRS-OPR-ETRD
                , :SGTSI080-NR-ISLC-OPR
                , :KOPRD-CD-OPRD
                , :SGTSI080-TS-ULT-ATL
                , :SGTSI080-CD-ARQ-OPR-ETRD
                  :CD-ARQ-OPR-ETRD-N
                , :SGTSI080-IN-ISLC-OPR-ATI
                , :DT-INC-ISLC
                , :DT-FIM-ISLC
                  :DT-FIM-ISLC-OPR-N
                , :SGTSI080-TX-DCR-ISLC-OPR
                  :TX-DCR-ISLC-OPR-N )
           END-EXEC
           INITIALIZE DADOS-LOG
           MOVE 'I'                    TO SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
              CONTAINER ( 'SGTSI080-RPST'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSI080-RPST )
              FROM      ( SGTSI080-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       430000-BUSCA-NR-SEQUENCIA SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
               SELECT VALUE (MAX(NR_ISLC_OPR) , 0 )
                INTO :SGTSI080-NR-ISLC-OPR
                FROM DB2SGT.ISLC_OPR_ETRD
                WHERE CD_CTRA_TIT_VL     = :SGTSI080-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP    = :SGTSI080-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD    = :SGTSI080-NR-VRS-OPR-ETRD
           END-EXEC
           ADD 1                       TO SGTSI080-NR-ISLC-OPR
           .
       430000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       980102-GRAVA-LOG SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

           MOVE 'ISLC_OPR_ETRD     '        TO SGTS014G-NM-ENT
           IF  EXECUCAO-ONLINE
               MOVE KOPRD-CD-OPRD TO SGTS014G-CD-CHV-USU
           ELSE
               MOVE 'SYSTEM'                TO SGTS014G-CD-CHV-USU
           END-IF
           MOVE 'SGTSI080'                  TO SGTS014G-NM-PGM
           MOVE SGTSI080-CD-CTRA-TIT-VL     TO CD-CTRA-TIT-VL-LOG
           MOVE SGTSI080-CD-OPR-ETRD-MCP    TO CD-OPR-ETRD-MCP-LOG
           MOVE SGTSI080-NR-VRS-OPR-ETRD    TO NR-VRS-OPR-ETRD-LOG
           MOVE SGTSI080-NR-ISLC-OPR        TO NR-ISLC-OPR-LOG
           STRING CD-CTRA-TIT-VL-LOG
           ';'    CD-OPR-ETRD-MCP-LOG
           ';'    NR-VRS-OPR-ETRD-LOG
           ';'    NR-ISLC-OPR-LOG
               DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR
           MOVE LENGTH OF DADOS-LOG    TO EIBCALEN
           CALL SGTS014G USING DFHEIBLK DADOS-LOG
           IF  SGTS014G-CD-RTN-PGM  NOT EQUAL ZEROS
               SET   ERRO-PROGRAMA-ACIONADO TO  TRUE
               MOVE 112                     TO  MSG1-CD-ERRO-TS
               MOVE 'Erro na gravação das informações do Log.'
                                            TO  MSG1-TX-ERRO-TS
               MOVE  SGTS014G               TO  MSG1-NM-PGM-ACND
               MOVE  SGTS014G-MSG-ERRO-PGM  TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTS014G-CD-RTN-PGM    TO  MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
008390 980102-SAI.
008400     EXIT.

008160*----------------------------------------------------------------*
008170 980103-LER-DADOS-OPRD           SECTION.
008180*----------------------------------------------------------------*
008190     MOVE 0014                   TO MSG1-CD-SEQL-ERRO
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

           MOVE 'SGTSI080-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI080-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI080-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
           IF  SGTSI080-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE  SGTSI080-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX         TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI080-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI080-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI080-NR-ISLC-OPR'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI080-NR-ISLC-OPR         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI080-TS-ULT-ATL'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI080-TS-ULT-ATL          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI080-CD-ARQ-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI080-CD-ARQ-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI080-IN-ISLC-OPR-ATI'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI080-IN-ISLC-OPR-ATI     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI080-DT-INC-ISLC-OPR'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI080-DT-INC-ISLC-OPR     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI080-DT-FIM-ISLC-OPR'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI080-DT-FIM-ISLC-OPR     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI080-TX-DCR-ISLC-OPR'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI080-TX-DCR-ISLC-OPR     TO MSGKW999-VL-VRV-MNT
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
      * --------------------------------------*
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