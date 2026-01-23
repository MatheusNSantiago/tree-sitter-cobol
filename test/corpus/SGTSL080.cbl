      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL080
      * ANALISTA..: F0175702 ADRIANO SEIJI NAKAMURA
      * AUTOR.....: F0175702 ADRIANO SEIJI NAKAMURA
      * OBJETIVO..: CONSULTA TABELA DB2SGT.ISLC_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS002 08.01.2025 - F9158937 - Acerto na inclusão do campo
      *                     DT-FIM-ISLC-OPR quando NULO
      * VRS001 20.07.2018 - F2804421 DE 2016/145195 - Excluir do pgm a
      *        utilização de Sql ref pesquisa de tipo de serviço = 1/2;
      *        09.06.2017 - F8658869 DE 2016/145195 - Inclusão serviço 2
      *        13.03.2017 - F0175702 DE 2016/145195 - IMPLANTACAO

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL080.

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
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSL080 ***'.
       77  CTE-VERS     PIC  X(06) VALUE 'VRS002'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
      * book de para acionamento do msg1
-INC IIBKERRO
-INC MSGKWMG1
-INC HLPKW999
-INC SGTKERR1

      * Area de trabalho
      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP-NR         PIC  9(11).

       01 QT-REG-SELD                         PIC S9(4) COMP-5 VALUE 0.
       01 DT-AUX-DMA                   PIC  X(10) VALUE ' '.
       01 DT-AUX-DMA-R                REDEFINES DT-AUX-DMA.
          02 DIA                       PIC  X(2).
          02 PTO1                      PIC  X(1).
          02 MES                       PIC  X(2).
          02 PTO2                      PIC  X(1).
          02 ANO                       PIC  X(4).
       01 AUX-DT-08                    PIC  X(08) VALUE SPACES.
       01 AUX-DT-08-R                  REDEFINES AUX-DT-08.
          03 DIA                       PIC  9(2).
          03 MES                       PIC  9(2).
          03 ANO                       PIC  9(4).
       01 DT-INC-ISLC-INV              PIC  X(08) VALUE SPACES.
       01 DT-INC-ISLC-INV-R            REDEFINES DT-INC-ISLC-INV.
          03 ANO                       PIC  9(4).
          03 MES                       PIC  9(2).
          03 DIA                       PIC  9(2).
       01 DT-FIM-ISLC-INV              PIC  X(08) VALUE SPACES.
       01 DT-FIM-ISLC-INV-R            REDEFINES DT-FIM-ISLC-INV.
          03 ANO                       PIC  9(4).
          03 MES                       PIC  9(2).
          03 DIA                       PIC  9(2).

      * Campos utilizados para leitura

       01  REG-WS.
           03 CD-CTRA-TIT-VL                 PIC S9(9) COMP-5
              OCCURS 101 TIMES              VALUE 0.
           03 CD-OPR-ETRD-MCP                PIC S9(11) COMP-3
              OCCURS 101 TIMES              VALUE 0.
           03 NR-VRS-OPR-ETRD                PIC S9(4) COMP-5
              OCCURS 101 TIMES              VALUE 0.
           03 NR-ISLC-OPR                    PIC S9(9) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 CD-USU-ULT-ATL                 PIC X(8)
              OCCURS 101 TIMES               VALUE ' '.
           03 TS-ULT-ATL                     PIC X(26)
              OCCURS 101 TIMES               VALUE ' '.
           03 CD-ARQ-OPR-ETRD                PIC S9(9) COMP-5
              OCCURS 101 TIMES               VALUE ZEROS.
           03 CD-ARQ-OPR-ETRD-N              PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE ZEROS.
           03 IN-ISLC-OPR-ATI                PIC X(1)
              OCCURS 101 TIMES               VALUE ' '.
           03 DT-INC-ISLC-OPR                PIC X(10)
              OCCURS 101 TIMES               VALUE ' '.
           03 DT-FIM-ISLC-OPR                PIC X(10)
              OCCURS 101 TIMES               VALUE ' '.
           03 DT-FIM-ISLC-OPR-N              PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE ZEROS.
           03 TX-DCR-ISLC-OPR                PIC X(200)
              OCCURS 101 TIMES               VALUE ' '.
           03 TX-DCR-ISLC-OPR-N              PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.

      * Indicadores de nulidades

       01 WS-IN-ISLC-OPR-INC                PIC  X(1) VALUE 'S'.
       01 WS-IN-ISLC-OPR-FIM                PIC  X(1) VALUE 'N'.
       01 WS-DT-INC-ISLC                    PIC  X(10)
                                            VALUE '01.01.0001'.
       01 WS-DT-FIM-ISLC                    PIC  X(10)
                                            VALUE '31.12.9999'.

      * Variavel(is) de controle do cursor(es)

       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ            VALUE +100.
           03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO          VALUE 'S'.

      * Subrotinas utilizadas

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
-INC SGTKL080

      * Lista todas as Insolvências da Operação Estruturada ordenadas pe
      * lo Número Sequencial da Insolvência da Operação.
           EXEC SQL
             DECLARE CURSOR-PSQ-1    CURSOR
                     WITH ROWSET POSITIONING FOR
              SELECT  A.NR_ISLC_OPR
                  , A.CD_USU_ULT_ATL
                  , A.TS_ULT_ATL
                  , IFNULL (A.CD_ARQ_OPR_ETRD, 0)
                  , A.IN_ISLC_OPR_ATI
                  , A.DT_INC_ISLC_OPR
                  , A.DT_FIM_ISLC_OPR
                  , IFNULL (A.TX_DCR_ISLC_OPR, ' ')
                FROM DB2SGT.ISLC_OPR_ETRD  A
               WHERE CD_CTRA_TIT_VL   = :SGTSL080-E-CD-CTRA-TIT-VL
                 AND CD_OPR_ETRD_MCP  = :SGTSL080-E-CD-OPR-ETRD-MCP
                 AND NR_VRS_OPR_ETRD  = :SGTSL080-E-NR-VRS-OPR-ETRD
      *--> Paginacao
                 AND NR_ISLC_OPR      > :SGTSL080-E-NR-ISLC-OPR
                 ORDER BY  A.NR_ISLC_OPR
           END-EXEC.

       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
      * Habilita tratamento automatico de erros DB2
-INC HLPKE997
      * Habilita captura automatica de abend e erros CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 110000-EXCLUIR-CTNR-RPST
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDAR-PARAMETRO

           PERFORM 970101-ABRE-CURSOR-PSQ
           PERFORM 970102-LER-CURSOR-PSQ
           PERFORM 970103-FECHA-CURSOR-PSQ

           PERFORM 200000-PROCESSA-PESQUISA
                    VARYING SGTSL080-IC-TAB FROM 1 BY 1
                    UNTIL SGTSL080-IC-TAB > SGTSL080-QT-OCR-TAB

           PERFORM 300000-POSTA-RESPOSTA
           GOBACK
           .

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

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
       110000-EXCLUIR-CTNR-RPST SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSL080-RPST' )
              CHANNEL   ( NM-CNL          )
              NOHANDLE
           END-EXEC

           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
               EIBRESP EQUAL DFHRESP(CONTAINERERR) )
               PERFORM 999002-TRATA-CONDITION-CICS
           END-IF
           .
       110000-SAI.
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

           MOVE ZEROS                        TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL080-RQSC      TO QT-CCT-EPRO
      *
           SET  GET-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE
           MOVE 'SGTSL080-RQSC'        TO NM-CTNR

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
              FLENGTH   ( LENGTH OF SGTSL080-RQSC )
              INTO      ( SGTSL080-RQSC           )
           END-EXEC

           SET  SEM-ERRO               TO TRUE
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO SGTSL080-IN-CTNC-PSQ

           IF  SGTSL080-E-CD-CTRA-TIT-VL  LESS ZEROS
               SET ERRO-PROGRAMADO TO TRUE
               MOVE 0001           TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'inválido.'    TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL080-E-CD-CTRA-TIT-VL  EQUAL ZEROS
               SET ERRO-PROGRAMADO TO TRUE
               MOVE 0002           TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'
                                   TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL080-E-CD-OPR-ETRD-MCP NOT NUMERIC
               SET ERRO-PROGRAMADO TO TRUE
001400         MOVE 0003           TO MSG1-CD-ERRO-TS
001410         MOVE 'Código operação estruturada mercado capitais não nu
      -             'mérico.'      TO MSG1-TX-ERRO-TS
001440         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL080-E-CD-OPR-ETRD-MCP <= ZEROS
               SET ERRO-PROGRAMADO TO TRUE
               MOVE 0004           TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado capitais in
      -             'válido.'      TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL080-E-NR-VRS-OPR-ETRD <= ZEROS
               SET ERRO-PROGRAMADO TO TRUE
               MOVE 0006           TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inváli
      -             'do.'          TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE NR-ISLC-OPR(SGTSL080-IC-TAB)     TO
                     SGTSL080-NR-ISLC-OPR(SGTSL080-IC-TAB)
           MOVE CD-ARQ-OPR-ETRD(SGTSL080-IC-TAB) TO
                     SGTSL080-CD-ARQ-OPR-ETRD(SGTSL080-IC-TAB)
           MOVE CD-USU-ULT-ATL(SGTSL080-IC-TAB)  TO
                     SGTSL080-CD-USU-ULT-ATL(SGTSL080-IC-TAB)
           MOVE IN-ISLC-OPR-ATI(SGTSL080-IC-TAB) TO
                     SGTSL080-IN-ISLC-OPR-ATI(SGTSL080-IC-TAB)
           MOVE TX-DCR-ISLC-OPR(SGTSL080-IC-TAB) TO
                     SGTSL080-TX-DCR-ISLC-OPR(SGTSL080-IC-TAB)
           MOVE TS-ULT-ATL(SGTSL080-IC-TAB)      TO
                     SGTSL080-TS-ULT-ATL(SGTSL080-IC-TAB)
           MOVE DT-INC-ISLC-OPR(SGTSL080-IC-TAB) TO DT-AUX-DMA
           MOVE CORR DT-AUX-DMA-R                TO AUX-DT-08-R
           MOVE AUX-DT-08 TO SGTSL080-DT-INC-ISLC-OPR(SGTSL080-IC-TAB)
           IF DT-FIM-ISLC-OPR-N (SGTSL080-IC-TAB) EQUAL -1
              MOVE '01.01.0001' TO DT-FIM-ISLC-OPR (SGTSL080-IC-TAB)
           END-IF
           MOVE DT-FIM-ISLC-OPR(SGTSL080-IC-TAB) TO DT-AUX-DMA
           MOVE CORR DT-AUX-DMA-R                TO AUX-DT-08-R
           MOVE AUX-DT-08 TO SGTSL080-DT-FIM-ISLC-OPR(SGTSL080-IC-TAB)
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           SET  PUT-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE
           MOVE 'SGTSL080-RPST'        TO NM-CTNR

           EXEC CICS PUT
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL080-RPST )
              FROM      ( SGTSL080-RPST           )
           END-EXEC

           SET SEM-ERRO                TO TRUE
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                OPEN CURSOR-PSQ-1
           END-EXEC

           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS
                EXEC SQL
                     FETCH NEXT ROWSET
                           CURSOR-PSQ-1 FOR 0101 ROWS
                    INTO :NR-ISLC-OPR
                       , :CD-USU-ULT-ATL
                       , :TS-ULT-ATL
                       , :CD-ARQ-OPR-ETRD
                         :CD-ARQ-OPR-ETRD-N
                       , :IN-ISLC-OPR-ATI
                       , :DT-INC-ISLC-OPR
                       , :DT-FIM-ISLC-OPR
                         :DT-FIM-ISLC-OPR-N
                       , :TX-DCR-ISLC-OPR
                         :TX-DCR-ISLC-OPR-N
                END-EXEC

           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL080-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100                TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL080-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO

           EXEC SQL
                CLOSE CURSOR-PSQ-1
           END-EXEC
           .
       970103-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------
           IF  IN-CURSOR-PSQ-ABTO  EQUAL  'S'
               PERFORM 970103-FECHA-CURSOR-PSQ
           END-IF

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

           MOVE 'SGTSL080-E-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL080-E-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL080-E-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSL080-E-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE SGTSL080-E-CD-OPR-ETRD-MCP TO  CD-OPR-ETRD-MCP-NR
               MOVE CD-OPR-ETRD-MCP-TX       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Valor não numérico'     TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL080-E-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL080-E-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL080-E-NR-ISLC-OPR'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL080-E-NR-ISLC-OPR         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS                TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS                TO IIBKERRO-TX-ERRO

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