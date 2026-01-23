       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL095
      * ANALISTA..: F8785286 - ROSELI AKEMI KAWAHATA
      * AUTOR.....: F8785286 - ROSELI AKEMI KAWAHATA
      * OBJETIVO..: Consulta da Tabela ITEM_EXCO_OPR
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 15.05.2018 - F8785286 - DE 2015/137233 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL095.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-SS               PIC  X(40)  VALUE
                   '*** S.S. COMECA AQUI ***'.
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTSL095 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

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

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      * Area de trabalho
       01  QT-REG-SELD                      PIC S9(4) COMP-5 VALUE 0.

      * Indicadores de nulidades
       01  QT-REG-SELD-NULO                 PIC S9(4) COMP-5 VALUE 0.

      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG            PIC  9(10) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG           PIC  9(11) VALUE 0.
           03  VL-RLZD-ITEM-EXCO-LOG         PIC  9(19) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG           PIC  9(5) VALUE 0.
           03  NR-SEQL-EXCO-PRFL-LOG         PIC  9(10) VALUE 0.
           03  NR-SEQL-ITEM-EXCO-LOG         PIC  9(5) VALUE 0.

      * Campos utilizados para leitura
       01  REG-WS.
           03 NR-SEQL-EXCO-PRFL              PIC S9(9) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 NR-SEQL-ITEM-EXCO              PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 CD-RGR-VLDC                    PIC S9(9) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 NR-VRS-RGR-VLDC                PIC S9(9) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 NR-SEQL-ITC                    PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 IN-ITEM-CRIO-EXCO              PIC X(1)
              OCCURS 101 TIMES               VALUE ' '.
           03 VL-RLZD-ITEM-EXCO              PIC S9(15)V9(4) COMP-3
              OCCURS 101 TIMES               VALUE 0.
           03 TX-RLZD-ITEM-EXCO              PIC X(20)
              OCCURS 101 TIMES               VALUE ' '.
           03 NR-RLZD-ITEM-EXCO              PIC S9(9) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 TX-OBS-ITEM-EXCO               PIC X(250)
              OCCURS 101 TIMES               VALUE ' '.
           03 VL-RLZD-ITEM-EXCO-N            PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 TX-RLZD-ITEM-EXCO-N            PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 NR-RLZD-ITEM-EXCO-N            PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 TX-OBS-ITEM-EXCO-N             PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.

      * Variavel(is) de controle do cursor(es)

       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ            VALUE +100.
           03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO          VALUE 'S'.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

-INC SGTKL095
      *-----------------------------------------------------------------
      * Consulta um item de exceção específico (toda a chave)
      *-----------------------------------------------------------------
           EXEC SQL
             DECLARE CURSOR-PSQ-1 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_RGR_VLDC
                  , A.NR_VRS_RGR_VLDC
                  , A.NR_SEQL_ITC
                  , A.IN_ITEM_CRIO_EXCO
                  , A.VL_RLZD_ITEM_EXCO
                  , A.TX_RLZD_ITEM_EXCO
                  , A.NR_RLZD_ITEM_EXCO
                  , A.TX_OBS_ITEM_EXCO
               FROM DB2SGT.ITEM_EXCO_OPR   A
              WHERE A.CD_CTRA_TIT_VL    = :SGTSL095-CD-CTRA-TIT-VL
                AND A.CD_OPR_ETRD_MCP   = :SGTSL095-CD-OPR-ETRD-MCP
                AND A.NR_VRS_OPR_ETRD   = :SGTSL095-NR-VRS-OPR-ETRD
                AND A.NR_SEQL_EXCO_PRFL = :SGTSL095-E-NR-SEQL-EXCO-PRFL
                AND A.NR_SEQL_ITEM_EXCO = :SGTSL095-E-NR-SEQL-ITEM-EXCO
           END-EXEC.

      *-----------------------------------------------------------------
      * Consulta um item de exceção por Carteira / Operação / Versão
      *-----------------------------------------------------------------
           EXEC SQL
             DECLARE CURSOR-PSQ-2 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.NR_SEQL_EXCO_PRFL
                  , A.NR_SEQL_ITEM_EXCO
                  , A.CD_RGR_VLDC
                  , A.NR_VRS_RGR_VLDC
                  , A.NR_SEQL_ITC
                  , A.IN_ITEM_CRIO_EXCO
                  , A.VL_RLZD_ITEM_EXCO
                  , A.TX_RLZD_ITEM_EXCO
                  , A.NR_RLZD_ITEM_EXCO
                  , A.TX_OBS_ITEM_EXCO
               FROM DB2SGT.ITEM_EXCO_OPR   A
              WHERE A.CD_CTRA_TIT_VL    = :SGTSL095-CD-CTRA-TIT-VL
                AND A.CD_OPR_ETRD_MCP   = :SGTSL095-CD-OPR-ETRD-MCP
                AND A.NR_VRS_OPR_ETRD   = :SGTSL095-NR-VRS-OPR-ETRD
                AND (A.NR_SEQL_EXCO_PRFL > :SGTSL095-E-NR-SEQL-EXCO-PRFL
                OR (A.NR_SEQL_EXCO_PRFL = :SGTSL095-E-NR-SEQL-EXCO-PRFL
                AND A.NR_SEQL_ITEM_EXCO
                                       > :SGTSL095-E-NR-SEQL-ITEM-EXCO))
             ORDER BY A.NR_SEQL_EXCO_PRFL , A.NR_SEQL_ITEM_EXCO
           END-EXEC.
      *-----------------------------------------------------------------
      * Consulta um item de exceção específico (toda a chave)
      *-----------------------------------------------------------------
           EXEC SQL
             DECLARE CURSOR-PSQ-3 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.NR_SEQL_EXCO_PRFL
                  , A.NR_SEQL_ITEM_EXCO
                  , A.CD_RGR_VLDC
                  , A.NR_VRS_RGR_VLDC
                  , A.NR_SEQL_ITC
                  , A.IN_ITEM_CRIO_EXCO
                  , A.VL_RLZD_ITEM_EXCO
                  , A.TX_RLZD_ITEM_EXCO
                  , A.NR_RLZD_ITEM_EXCO
                  , A.TX_OBS_ITEM_EXCO
               FROM DB2SGT.ITEM_EXCO_OPR   A
              WHERE A.CD_CTRA_TIT_VL    = :SGTSL095-CD-CTRA-TIT-VL
                AND A.CD_OPR_ETRD_MCP   = :SGTSL095-CD-OPR-ETRD-MCP
                AND A.NR_VRS_OPR_ETRD   = :SGTSL095-NR-VRS-OPR-ETRD
                AND A.NR_SEQL_EXCO_PRFL = :SGTSL095-E-NR-SEQL-EXCO-PRFL
                AND A.NR_SEQL_ITEM_EXCO >= :SGTSL095-E-NR-SEQL-ITEM-EXCO
           END-EXEC.

        01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL          SECTION.

           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      * Habilita tratamento automatico de Erros DB2
-INC HLPKE997
      * Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 110000-EXCLUIR-CTNR-RPST
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDA-PARAMETRO

           PERFORM 970101-ABRE-CURSOR-PSQ
           PERFORM 970102-LER-CURSOR-PSQ
           PERFORM 970103-FECHA-CURSOR-PSQ
           PERFORM 200000-PROCESSA-PESQUISA
                    VARYING SGTSL095-IC-TAB FROM 1 BY 1
                    UNTIL SGTSL095-IC-TAB > SGTSL095-QT-OCR-TAB

           PERFORM 300000-POSTA-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
                CHANNEL ( NM-CNL )
           END-EXEC

           SET  SEM-ERRO               TO TRUE
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSL095-RPST' )
              CHANNEL   ( NM-CNL      )
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
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL095-RQSC      TO QT-CCT-EPRO

           SET GET-CONTAINER           TO TRUE
           SET ERRO-CONTAINER          TO TRUE
           MOVE 'SGTSL095-RQSC'        TO NM-CTNR

           EXEC CICS GET
              CONTAINER ( NM-CTNR         )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA     TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL095-RQSC )
              INTO      ( SGTSL095-RQSC           )
           END-EXEC

      ** Este ponto indica que não houve erro de cics.
           SET SEM-ERRO        TO TRUE

           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDA-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO SGTSL095-IN-CTNC-PSQ

           IF  SGTSL095-CD-TIP-SRVC    EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 107                TO  MSG1-CD-ERRO-TS
               MOVE 'Código tipo serviço inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL095-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 001                TO  MSG1-CD-ERRO-TS
               MOVE 'Código carteira título valor inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL095-CD-OPR-ETRD-MCP NOT NUMERIC
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -         ' numérico.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL095-CD-OPR-ETRD-MCP   EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 004                TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturado mercado de capitais inv
      -        'álido.'                TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL095-NR-VRS-OPR-ETRD   EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 006                TO  MSG1-CD-ERRO-TS
               MOVE 'Numero versão operação estruturado inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE NR-SEQL-EXCO-PRFL(SGTSL095-IC-TAB) TO
                     SGTSL095-NR-SEQL-EXCO-PRFL(SGTSL095-IC-TAB)
           MOVE NR-SEQL-ITEM-EXCO(SGTSL095-IC-TAB) TO
                     SGTSL095-NR-SEQL-ITEM-EXCO(SGTSL095-IC-TAB)
           MOVE CD-RGR-VLDC(SGTSL095-IC-TAB)       TO
                     SGTSL095-CD-RGR-VLDC(SGTSL095-IC-TAB)
           MOVE NR-VRS-RGR-VLDC(SGTSL095-IC-TAB)   TO
                     SGTSL095-NR-VRS-RGR-VLDC(SGTSL095-IC-TAB)
           MOVE NR-SEQL-ITC(SGTSL095-IC-TAB)        TO
                     SGTSL095-NR-SEQL-ITC(SGTSL095-IC-TAB)
           MOVE IN-ITEM-CRIO-EXCO(SGTSL095-IC-TAB) TO
                     SGTSL095-IN-ITEM-CRIO-EXCO(SGTSL095-IC-TAB)
           MOVE VL-RLZD-ITEM-EXCO(SGTSL095-IC-TAB) TO
                     SGTSL095-VL-RLZD-ITEM-EXCO(SGTSL095-IC-TAB)
           MOVE TX-RLZD-ITEM-EXCO(SGTSL095-IC-TAB) TO
                     SGTSL095-TX-RLZD-ITEM-EXCO(SGTSL095-IC-TAB)
           MOVE NR-RLZD-ITEM-EXCO(SGTSL095-IC-TAB) TO
                     SGTSL095-NR-RLZD-ITEM-EXCO(SGTSL095-IC-TAB)
           MOVE TX-OBS-ITEM-EXCO(SGTSL095-IC-TAB)  TO
                     SGTSL095-TX-OBS-ITEM-EXCO(SGTSL095-IC-TAB)
                      .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
              CONTAINER ( 'SGTSL095-RPST' )
              CHANNEL   ( NM-CNL      )
              FLENGTH ( LENGTH OF SGTSL095-RPST )
              FROM    ( SGTSL095-RPST )
           END-EXEC
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           EVALUATE SGTSL095-CD-TIP-SRVC
           WHEN 1
               EXEC SQL
                   OPEN CURSOR-PSQ-1
               END-EXEC

           WHEN 2
               EXEC SQL
                   OPEN CURSOR-PSQ-2
               END-EXEC

           WHEN 3
               EXEC SQL
                   OPEN CURSOR-PSQ-3
               END-EXEC
           END-EVALUATE

           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EVALUATE SGTSL095-CD-TIP-SRVC
           WHEN 1
               EXEC SQL
                    FETCH NEXT ROWSET
                          CURSOR-PSQ-1 FOR 0101 ROWS
                    INTO :CD-RGR-VLDC
                       , :NR-VRS-RGR-VLDC
                       , :NR-SEQL-ITC
                       , :IN-ITEM-CRIO-EXCO
                       , :VL-RLZD-ITEM-EXCO
                         :VL-RLZD-ITEM-EXCO-N
                       , :TX-RLZD-ITEM-EXCO
                         :TX-RLZD-ITEM-EXCO-N
                       , :NR-RLZD-ITEM-EXCO
                         :NR-RLZD-ITEM-EXCO-N
                       , :TX-OBS-ITEM-EXCO
                         :TX-OBS-ITEM-EXCO-N
               END-EXEC

           WHEN 2
               EXEC SQL
                    FETCH NEXT ROWSET
                          CURSOR-PSQ-2 FOR 0101 ROWS
                    INTO :NR-SEQL-EXCO-PRFL
                       , :NR-SEQL-ITEM-EXCO
                       , :CD-RGR-VLDC
                       , :NR-VRS-RGR-VLDC
                       , :NR-SEQL-ITC
                       , :IN-ITEM-CRIO-EXCO
                       , :VL-RLZD-ITEM-EXCO
                         :VL-RLZD-ITEM-EXCO-N
                       , :TX-RLZD-ITEM-EXCO
                         :TX-RLZD-ITEM-EXCO-N
                       , :NR-RLZD-ITEM-EXCO
                         :NR-RLZD-ITEM-EXCO-N
                       , :TX-OBS-ITEM-EXCO
                         :TX-OBS-ITEM-EXCO-N
               END-EXEC

           WHEN 3
               EXEC SQL
                    FETCH NEXT ROWSET
                          CURSOR-PSQ-3 FOR 0101 ROWS
                    INTO :NR-SEQL-EXCO-PRFL
                       , :NR-SEQL-ITEM-EXCO
                       , :CD-RGR-VLDC
                       , :NR-VRS-RGR-VLDC
                       , :NR-SEQL-ITC
                       , :IN-ITEM-CRIO-EXCO
                       , :VL-RLZD-ITEM-EXCO
                         :VL-RLZD-ITEM-EXCO-N
                       , :TX-RLZD-ITEM-EXCO
                         :TX-RLZD-ITEM-EXCO-N
                       , :NR-RLZD-ITEM-EXCO
                         :NR-RLZD-ITEM-EXCO-N
                       , :TX-OBS-ITEM-EXCO
                         :TX-OBS-ITEM-EXCO-N
               END-EXEC

           END-EVALUATE

           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL095-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100                TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL095-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
           EVALUATE SGTSL095-CD-TIP-SRVC
           WHEN 1
               EXEC SQL
                   CLOSE CURSOR-PSQ-1
               END-EXEC

           WHEN 2
               EXEC SQL
                   CLOSE CURSOR-PSQ-2
               END-EXEC

           WHEN 3
               EXEC SQL
                   CLOSE CURSOR-PSQ-3
               END-EXEC
           END-EVALUATE
           .
       970103-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------
           IF  IN-CURSOR-PSQ-ABTO  EQUAL  'S'
               PERFORM 970103-FECHA-CURSOR-PSQ
           END-IF

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888                    TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                             TO  MSG1-TX-ERRO-TS
           END-IF

      *Verifica se o tipo de erro gerado possui tratamento interno
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

           MOVE 'SGTSL095-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL095-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL095-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
           IF  SGTSL095-CD-OPR-ETRD-MCP      IS NUMERIC
               MOVE  SGTSL095-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX      TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                             TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL095-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL095-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL095-E-NR-SEQL-EXCO-PRFL'
                                             TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL095-E-NR-SEQL-EXCO-PRFL TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL095-E-NR-SEQL-ITEM-EXCO'
                                             TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL095-E-NR-SEQL-ITEM-EXCO TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS              TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS              TO IIBKERRO-TX-ERRO
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
      * --------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * --------------------------------------*
-INC HLPKP999
      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997
      * --------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
-INC MSGKPMG1
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999
      * --------------------------------------*
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC.