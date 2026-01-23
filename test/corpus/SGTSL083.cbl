      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL083
      * ANALISTA..: F8785286 Roseli Akemi Kawahata
      * AUTOR.....: F8785286 Roseli Akemi Kawahata
      * OBJETIVO..: Consulta Tabela EXCO_PRFL_SELD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 24/08/2017 F8785286 Inclusão cursores 3 e 4 e campos
      *              SGTSL083-CD-RGR-VLDC  e SGTSL083-NR-VRS-RGR-VLDC
      *        20/09/2018 - F8785286 DE 2016/145195 - Inclusão serv 3.
      *        04/08/2017 - F8785286 DE 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL083.
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
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTSL083 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.
      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * book de para acionamento do msg1
-INC IIBKERRO
-INC MSGKWMG1
-INC HLPKW999
-INC SGTKERR1

       01  GRP-CONTAINER.
           03  CANAL                   PIC X(16) VALUE SPACES.
      *    03  QT-CCT-LIDO             PIC S9(9) COMP-5 VALUE 0.
      *    03  QT-CCT-EPRO             PIC S9(9) COMP-5 VALUE 0.
      *    03  VL-EXBR-1               PIC 9(10)        VALUE 0.
      *    03  VL-EXBR-2               PIC 9(10)        VALUE 0.

      * Area de trabalho
       01 QT-REG-SELD                  PIC S9(4) COMP-5 VALUE 0.

      * Campos utilizados para leitura
       01  REG-WS.
           03 CD-CTRA-TIT-VL           PIC S9(9) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 CD-OPR-ETRD-MCP          PIC S9(11) COMP-3
              OCCURS 101 TIMES         VALUE 0.
           03 NR-VRS-OPR-ETRD          PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 CD-RGR-VLDC              PIC S9(9) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 NR-VRS-RGR-VLDC          PIC  S9(9) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 NR-SEQL-EXCO-PRFL        PIC  S9(9) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 CD-USU-ULT-ATL           PIC  X(8)
              OCCURS 101 TIMES         VALUE ' '.
           03 TS-ULT-ATL               PIC  X(26)
              OCCURS 101 TIMES         VALUE ' '.
           03 IN-EXCO-ATV              PIC  X(1)
              OCCURS 101 TIMES         VALUE ' '.
           03 TX-OBS-EXCO-PRFL         PIC  X(400)
              OCCURS 101 TIMES         VALUE ' '.

      * Indicadores de nulidades
       01 QT-REG-SELD-N                PIC S9(4) COMP-5 VALUE 0.

      * Variáveis auxiliares
       01 AUX-CD-TIP-EST-DCS           PIC S9(4) COMP-5 VALUE 2.

      * Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ      VALUE +100.
           03  IN-CURSOR-PSQ-ABTO      PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO     VALUE 'S'.

       01  DADOS-LOG-DISPLAY.
           03  CD-OPR-ETRD-MCP-DISP    PIC  9(11) VALUE 0.

      * Subrotinas utilizadas
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

-INC SGTKL083
      *-----------------------------------------------------------------
      *--> Pesquisa Registros de Exceção Ativa, IN_EXCO_ATV = 'S'
      *-----------------------------------------------------------------
           EXEC SQL
             DECLARE CURSOR-PSQ-1 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_CTRA_TIT_VL
                  , A.CD_OPR_ETRD_MCP
                  , A.NR_VRS_OPR_ETRD
                  , A.NR_SEQL_EXCO_PRFL
                  , A.CD_USU_ULT_ATL
                  , A.TS_ULT_ATL
                  , A.IN_EXCO_ATV
                  , A.TX_OBS_EXCO_PRFL
             FROM DB2SGT.EXCO_PRFL_SELD   A

             WHERE A.CD_CTRA_TIT_VL      = :SGTSL083-CD-CTRA-TIT-VL
               AND A.CD_OPR_ETRD_MCP     = :SGTSL083-CD-OPR-ETRD-MCP
               AND A.NR_VRS_OPR_ETRD     = :SGTSL083-NR-VRS-OPR-ETRD
               AND A.IN_EXCO_ATV         = 'S'
               AND A.NR_SEQL_EXCO_PRFL  >= :SGTSL083-E-NR-SEQL-EXCO-PRFL

             ORDER BY  A.NR_SEQL_EXCO_PRFL
           END-EXEC.
      *-----------------------------------------------------------------
      *    Lista Todas as Exceções de uma Operacao Ordenada por Código
      *    da Regra, Número Versão da Regra de Validação e Número Sequen
      *    cial da Exceção da Operação
      *-----------------------------------------------------------------
           EXEC SQL
             DECLARE CURSOR-PSQ-2 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.NR_SEQL_EXCO_PRFL
                  , A.CD_USU_ULT_ATL
                  , A.TS_ULT_ATL
                  , A.IN_EXCO_ATV
                  , A.TX_OBS_EXCO_PRFL
                  , A.NR_VRS_OPR_ETRD
             FROM DB2SGT.EXCO_PRFL_SELD   A

             WHERE A.CD_CTRA_TIT_VL    = :SGTSL083-CD-CTRA-TIT-VL
              AND A.CD_OPR_ETRD_MCP    = :SGTSL083-CD-OPR-ETRD-MCP
              AND A.NR_VRS_OPR_ETRD    = :SGTSL083-NR-VRS-OPR-ETRD
      * Paginação	
              AND A.NR_SEQL_EXCO_PRFL  > :SGTSL083-E-NR-SEQL-EXCO-PRFL
             ORDER BY A.NR_SEQL_EXCO_PRFL
           END-EXEC.
      *-----------------------------------------------------------------
      *--> Pesquisa Registros de Exceção Ativa, IN_EXCO_ATV = 'S'
      *-->   de qualquer versão da operação
      *-----------------------------------------------------------------
           EXEC SQL
             DECLARE CURSOR-PSQ-3 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_CTRA_TIT_VL
                  , A.CD_OPR_ETRD_MCP
                  , A.NR_VRS_OPR_ETRD
                  , A.NR_SEQL_EXCO_PRFL
                  , A.CD_USU_ULT_ATL
                  , A.TS_ULT_ATL
                  , A.IN_EXCO_ATV
                  , A.TX_OBS_EXCO_PRFL
             FROM DB2SGT.EXCO_PRFL_SELD   A

             WHERE A.CD_CTRA_TIT_VL      = :SGTSL083-CD-CTRA-TIT-VL
               AND A.CD_OPR_ETRD_MCP     = :SGTSL083-CD-OPR-ETRD-MCP
               AND A.IN_EXCO_ATV         = 'S'

               AND ((A.NR_VRS_OPR_ETRD  >= :SGTSL083-NR-VRS-OPR-ETRD)
               OR  (A.NR_VRS_OPR_ETRD    = :SGTSL083-NR-VRS-OPR-ETRD
               AND A.NR_SEQL_EXCO_PRFL  >=
                                        :SGTSL083-E-NR-SEQL-EXCO-PRFL))

             ORDER BY  A.NR_VRS_OPR_ETRD , A.NR_SEQL_EXCO_PRFL
           END-EXEC.
      *-----------------------------------------------------------------
      *--> Pesquisa Registros de Exceção Ativa, IN_EXCO_ATV = 'S'
      *-->   para a cart/oper/versão  e  regra/versão-regra
      *-----------------------------------------------------------------
           EXEC SQL
             DECLARE CURSOR-PSQ-4 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_CTRA_TIT_VL
                  , A.CD_OPR_ETRD_MCP
                  , A.NR_VRS_OPR_ETRD
                  , A.NR_SEQL_EXCO_PRFL
                  , A.CD_USU_ULT_ATL
                  , A.TS_ULT_ATL
                  , A.IN_EXCO_ATV
                  , A.TX_OBS_EXCO_PRFL

             FROM DB2SGT.EXCO_PRFL_SELD   A
             INNER JOIN DB2SGT.ITEM_EXCO_OPR   B

             ON  B.CD_CTRA_TIT_VL   =  A.CD_CTRA_TIT_VL
             AND B.CD_OPR_ETRD_MCP  =  A.CD_OPR_ETRD_MCP
             AND B.NR_VRS_OPR_ETRD  =  A.NR_VRS_OPR_ETRD

             WHERE A.CD_CTRA_TIT_VL      = :SGTSL083-CD-CTRA-TIT-VL
               AND A.CD_OPR_ETRD_MCP     = :SGTSL083-CD-OPR-ETRD-MCP
               AND A.NR_VRS_OPR_ETRD     = :SGTSL083-NR-VRS-OPR-ETRD
               AND A.IN_EXCO_ATV         = 'S'
               AND B.CD_RGR_VLDC         = :SGTSL083-CD-RGR-VLDC
               AND B.NR_VRS_RGR_VLDC     = :SGTSL083-NR-VRS-RGR-VLDC

               AND A.NR_SEQL_EXCO_PRFL  >= :SGTSL083-E-NR-SEQL-EXCO-PRFL

             ORDER BY  A.NR_SEQL_EXCO_PRFL
           END-EXEC.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*

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
                    VARYING SGTSL083-IC-TAB FROM 1 BY 1
                    UNTIL SGTSL083-IC-TAB > SGTSL083-QT-OCR-TAB

           PERFORM 300000-POSTA-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO  MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO  TRUE

           EXEC CICS ASSIGN
              CHANNEL ( CANAL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO  MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSL083-RPST' )
              CHANNEL   ( CANAL      )
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
       120000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   ( CANAL      )
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
       130000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO  QT-CCT-LIDO
           MOVE LENGTH OF SGTSL083-RQSC    TO  QT-CCT-EPRO

           SET GET-CONTAINER           TO TRUE
           SET ERRO-CONTAINER          TO TRUE
           MOVE 'SGTSL083-RQSC'        TO NM-CTNR

           EXEC CICS GET
              CONTAINER ( NM-CTNR    )
              CHANNEL   ( CANAL      )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( NM-CTNR    )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSL083-RQSC )
              INTO    ( SGTSL083-RQSC )
           END-EXEC

           SET SEM-ERRO                TO TRUE
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO  SGTSL083-IN-CTNC-PSQ

           IF  NOT  SGTSL083-LISTA-EXCO-ATV AND
               NOT  SGTSL083-LISTA-TODOS    AND
               NOT  SGTSL083-LISTA-TDS-VRS  AND
               NOT  SGTSL083-LISTA-OPR-RGR
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0107              TO  MSG1-CD-ERRO-TS
               MOVE  'Código tipo serviço inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL083-CD-CTRA-TIT-VL     EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 002                    TO  MSG1-CD-ERRO-TS
               MOVE 'Codigo Carteira TVM deve ser informado.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL083-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 003                    TO  MSG1-CD-ERRO-TS
               MOVE 'Codigo da Operação Estruturada inválido.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
               IF  SGTSL083-CD-OPR-ETRD-MCP    EQUAL ZEROS
                   SET  ERRO-PROGRAMADO        TO  TRUE
                   MOVE 005                    TO  MSG1-CD-ERRO-TS
                   MOVE 'Codigo da Operação Estruturada deve ser informa
      -                 'do.'                  TO  MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           END-IF

           IF NOT SGTSL083-LISTA-TDS-VRS
              IF  SGTSL083-NR-VRS-OPR-ETRD   EQUAL ZEROS
                  SET  ERRO-PROGRAMADO       TO  TRUE
                  MOVE 007                   TO  MSG1-CD-ERRO-TS
                  MOVE
                      'Número da Versão da Operação deve ser informado.'
                                             TO  MSG1-TX-ERRO-TS
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-IF
           END-IF
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA        SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO  MSG1-CD-SEQL-ERRO

           MOVE NR-VRS-OPR-ETRD(SGTSL083-IC-TAB) TO
                             SGTSL083-NR-VRS-OPR-ETRD-S(SGTSL083-IC-TAB)

           MOVE NR-SEQL-EXCO-PRFL(SGTSL083-IC-TAB) TO
                             SGTSL083-NR-SEQL-EXCO-PRFL(SGTSL083-IC-TAB)

           MOVE CD-USU-ULT-ATL(SGTSL083-IC-TAB)   TO
                             SGTSL083-CD-USU-ULT-ATL(SGTSL083-IC-TAB)

           MOVE TS-ULT-ATL(SGTSL083-IC-TAB)  TO
                             SGTSL083-TS-ULT-ATL(SGTSL083-IC-TAB)

           MOVE IN-EXCO-ATV(SGTSL083-IC-TAB)    TO
                             SGTSL083-IN-EXCO-ATV(SGTSL083-IC-TAB)

           MOVE TX-OBS-EXCO-PRFL(SGTSL083-IC-TAB)    TO
                             SGTSL083-TX-OBS-EXCO-PRFL(SGTSL083-IC-TAB)
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO  MSG1-CD-SEQL-ERRO

           SET PUT-CONTAINER           TO TRUE
           SET ERRO-CONTAINER          TO TRUE
           MOVE 'SGTSL083-RPST'        TO NM-CTNR

           EXEC CICS PUT
              CONTAINER ( NM-CTNR    )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSL083-RPST )
              FROM    ( SGTSL083-RPST )
           END-EXEC

           SET SEM-ERRO                TO TRUE
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO  MSG1-CD-SEQL-ERRO

           EVALUATE  TRUE
               WHEN  SGTSL083-LISTA-EXCO-ATV
                     EXEC SQL
                          OPEN CURSOR-PSQ-1
                     END-EXEC

               WHEN  SGTSL083-LISTA-TODOS
                     EXEC SQL
                          OPEN CURSOR-PSQ-2
                     END-EXEC

               WHEN  SGTSL083-LISTA-TDS-VRS
                     EXEC SQL
                          OPEN CURSOR-PSQ-3
                     END-EXEC

               WHEN  SGTSL083-LISTA-OPR-RGR
                     EXEC SQL
                          OPEN CURSOR-PSQ-4
                     END-EXEC
           END-EVALUATE

           MOVE 'S'                    TO  IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ           SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO  MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EVALUATE  TRUE

               WHEN  SGTSL083-LISTA-EXCO-ATV
                     EXEC SQL
                       FETCH NEXT ROWSET CURSOR-PSQ-1
                             FOR 0101 ROWS
                        INTO :CD-CTRA-TIT-VL
                           , :CD-OPR-ETRD-MCP
                           , :NR-VRS-OPR-ETRD
                           , :NR-SEQL-EXCO-PRFL
                           , :CD-USU-ULT-ATL
                           , :TS-ULT-ATL
                           , :IN-EXCO-ATV
                           , :TX-OBS-EXCO-PRFL
                     END-EXEC

               WHEN  SGTSL083-LISTA-TODOS
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-2
                                FOR 0101 ROWS
                           INTO :NR-SEQL-EXCO-PRFL
                              , :CD-USU-ULT-ATL
                              , :TS-ULT-ATL
                              , :IN-EXCO-ATV
                              , :TX-OBS-EXCO-PRFL
                              , :NR-VRS-OPR-ETRD
                     END-EXEC

               WHEN  SGTSL083-LISTA-TDS-VRS
                     EXEC SQL
                       FETCH NEXT ROWSET CURSOR-PSQ-3
                             FOR 0101 ROWS
                        INTO :CD-CTRA-TIT-VL
                           , :CD-OPR-ETRD-MCP
                           , :NR-VRS-OPR-ETRD
                           , :NR-SEQL-EXCO-PRFL
                           , :CD-USU-ULT-ATL
                           , :TS-ULT-ATL
                           , :IN-EXCO-ATV
                           , :TX-OBS-EXCO-PRFL
                     END-EXEC

               WHEN  SGTSL083-LISTA-OPR-RGR
                     EXEC SQL
                       FETCH NEXT ROWSET CURSOR-PSQ-4
                             FOR 0101 ROWS
                        INTO :CD-CTRA-TIT-VL
                           , :CD-OPR-ETRD-MCP
                           , :NR-VRS-OPR-ETRD
                           , :NR-SEQL-EXCO-PRFL
                           , :CD-USU-ULT-ATL
                           , :TS-ULT-ATL
                           , :IN-EXCO-ATV
                           , :TX-OBS-EXCO-PRFL
                     END-EXEC
           END-EVALUATE

           MOVE SQLERRD(3)             TO  QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL083-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO  QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO  SGTSL083-QT-OCR-TAB
           MOVE SQLCODE                TO  CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ         SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO  IN-CURSOR-PSQ-ABTO

           EVALUATE  TRUE
               WHEN  SGTSL083-LISTA-EXCO-ATV
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-1
                     END-EXEC

               WHEN  SGTSL083-LISTA-TODOS
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-2
                     END-EXEC

               WHEN  SGTSL083-LISTA-TDS-VRS
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-3
                     END-EXEC

               WHEN  SGTSL083-LISTA-OPR-RGR
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-4
                     END-EXEC
           END-EVALUATE
           .
       970103-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------
           IF MSG1-CD-ERRO-TS EQUAL ZEROS
              MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF

           IF MSG1-TX-ERRO-TS EQUAL SPACES
              MOVE 'Erro não especificado no módulo.' TO MSG1-TX-ERRO-TS
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

           MOVE  'SGTSL083-CD-TIP-SRVC'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL083-E-CD-TIP-SRVC        TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL083-CD-CTRA-TIT-VL'      TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSL083-CD-CTRA-TIT-VL        TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           IF  SGTSL083-CD-OPR-ETRD-MCP        IS  NUMERIC
               MOVE 'SGTSL083-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSL083-CD-OPR-ETRD-MCP  TO CD-OPR-ETRD-MCP-DISP
               MOVE  CD-OPR-ETRD-MCP-DISP      TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT
           END-IF

           MOVE 'SGTSL083-NR-VRS-OPR-ETRD'     TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSL083-NR-VRS-OPR-ETRD       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL083-E-NR-SEQL-EXCO-PRFL' TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSL083-E-NR-SEQL-EXCO-PRFL   TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS                TO  IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS                TO  IIBKERRO-TX-ERRO

           EXEC CICS PUT
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   ( CANAL      )
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
-INC MSGKPMG1
      * --------------------------------------*
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC.