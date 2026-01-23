      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL037
      * ANALISTA..: F8785286 Roseli Akemi Kawahata
      * AUTOR.....: F8785286 Roseli Akemi Kawahata
      * OBJETIVO..: Consulta Tabelas  DB2SGT.CRCT_CTRA_TVM e
      *               EST_CRCT_CTRA_TVM
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 26.07.2017 - F8785286 DE 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL037.
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
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTSL037 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.
      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
       LOCAL-STORAGE SECTION.
      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      ---------------9.
      *-----------------------------------------------------------------
      * book de para acionamento do msg1

-INC IIBKERRO
-INC MSGKWMG1
-INC HLPKW999
-INC SGTKERR1

       01  GRP-CONTAINER.
           03  CANAL                   PIC X(16) VALUE SPACES.

      * Area de trabalho
       01 QT-REG-SELD                  PIC S9(4) COMP-5 VALUE 0.

       01 CARTEIRA-300000              PIC S9(9)  COMP-5 VALUE 300000.

       01 DT-AUX-DMA                   PIC  X(10) VALUE ' '.
       01 DT-AUX-DMA-R                 REDEFINES DT-AUX-DMA.
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

      * Campos utilizados para leitura
       01  REG-WS.
           03 CD-CTRA-TVM              PIC S9(9) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 CD-TIP-ATB-CTRA          PIC S9(9) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 NR-ETRD-CRCT-CTRA        PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 CD-UOR-VCLD-CRC          PIC S9(9) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 CD-UOR-VCLD-CRC-N        PIC  S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 VL-VCLD-CRCT-CTRA        PIC S9(15)V99 COMP-3
              OCCURS 101 TIMES         VALUE 0.
           03 VL-VCLD-CRCT-CTRA-N      PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 PC-VCLD-CRCT-CTRA        PIC S9(4)V9999 COMP-3
              OCCURS 101 TIMES         VALUE 0.
           03 PC-VCLD-CRCT-CTRA-N      PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 NR-NOTA-TCN-OPR          PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 TS-EST-CRCT-CTRA         PIC  X(26)
              OCCURS 101 TIMES         VALUE ' '.
           03 CD-TIP-EST-DCS           PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 CD-USU-ATL-EST           PIC  X(8)
              OCCURS 101 TIMES         VALUE ' '.
           03 TX-VCLD-CRCT             PIC  X(20)
              OCCURS 101 TIMES         VALUE ' '.
           03 TX-VCLD-CRCT-N           PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 TX-OBS-EST-CRCT          PIC  X(60)
              OCCURS 101 TIMES         VALUE ' '.
           03 TX-OBS-EST-CRCT-N        PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 IN-CRCT-GLB              PIC X(1)
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

      * Subrotinas utilizadas
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
      *--> Pesquisa Característica da Carteira mais recente
      *-----------------------------------------------------------------
-INC SGTKL037
           EXEC SQL
             DECLARE CURSOR-PSQ-1 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_CTRA_TVM
                  , A.CD_TIP_ATB_CTRA
                  , A.NR_ETRD_CRCT_CTRA
                  , A.CD_UOR_VCLD_CRCT
                  , A.VL_VCLD_CRCT_CTRA
                  , A.TX_VCLD_CRCT
                  , A.PC_VCLD_CRCT_CTRA
                  , A.NR_NOTA_TCN_OPR
                  , B.TS_EST_CRCT_CTRA
                  , B.CD_TIP_EST_DCS
                  , B.CD_USU_ATL_EST
                  , B.TX_OBS_EST_CRCT
             FROM DB2SGT.CRCT_CTRA_TVM  A
             INNER JOIN DB2SGT.EST_CRCT_CTRA_TVM  B
                    ON  A.CD_CTRA_TVM       =  B.CD_CTRA_TVM
                    AND A.CD_TIP_ATB_CTRA   =  B.CD_TIP_ATB_CTRA
                    AND A.NR_ETRD_CRCT_CTRA =  B.NR_ETRD_CRCT_CTRA

             WHERE A.CD_CTRA_TVM         = :SGTSL037-E-CD-CTRA-TVM
               AND A.CD_TIP_ATB_CTRA     =
                                         :SGTSL037-E-CD-TIP-ATB-CTRA-01
               AND A.NR_ETRD_CRCT_CTRA   > 0
               AND B.TS_EST_CRCT_CTRA = (SELECT MAX(C.TS_EST_CRCT_CTRA)
                   FROM DB2SGT.EST_CRCT_CTRA_TVM C
                   WHERE C.CD_CTRA_TVM        = :SGTSL037-E-CD-CTRA-TVM
                     AND B.CD_CTRA_TVM        =  C.CD_CTRA_TVM
                     AND B.CD_TIP_ATB_CTRA    =  C.CD_TIP_ATB_CTRA
                     AND B.NR_ETRD_CRCT_CTRA  =  C.NR_ETRD_CRCT_CTRA
                     AND C.CD_TIP_EST_DCS     = :AUX-CD-TIP-EST-DCS
                     AND C.CD_TIP_ATB_CTRA    =
                                         :SGTSL037-E-CD-TIP-ATB-CTRA-01)
            ORDER BY  B.TS_EST_CRCT_CTRA   DESC
           END-EXEC.

      *-----------------------------------------------------------------
      *--> Pesquisa Característica da Carteira por Indic Caract Global
      *-----------------------------------------------------------------
           EXEC SQL
             DECLARE CURSOR-PSQ-2 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_CTRA_TVM
                  , A.CD_TIP_ATB_CTRA
                  , A.NR_ETRD_CRCT_CTRA
                  , A.CD_UOR_VCLD_CRCT
                  , A.VL_VCLD_CRCT_CTRA
                  , A.PC_VCLD_CRCT_CTRA
                  , A.NR_NOTA_TCN_OPR
                  , B.TS_EST_CRCT_CTRA
                  , B.CD_TIP_EST_DCS
                  , B.CD_USU_ATL_EST
                  , B.TX_OBS_EST_CRCT
                  , D.IN_CRCT_GLB
             FROM DB2SGT.CRCT_CTRA_TVM  A
             INNER JOIN DB2SGT.TIP_ATB_VCLD_CTRA  D
                    ON  A.CD_TIP_ATB_CTRA   =  D.CD_TIP_ATB_CTRA
             INNER JOIN DB2SGT.EST_CRCT_CTRA_TVM  B
                    ON  A.CD_CTRA_TVM       =  B.CD_CTRA_TVM
                    AND A.CD_TIP_ATB_CTRA   =  B.CD_TIP_ATB_CTRA
                    AND A.NR_ETRD_CRCT_CTRA =  B.NR_ETRD_CRCT_CTRA

             WHERE A.CD_CTRA_TVM         = :SGTSL037-E-CD-CTRA-TVM
               AND A.CD_TIP_ATB_CTRA     IN
                               (:SGTSL037-E-CD-TIP-ATB-CTRA-01 ,
                                :SGTSL037-E-CD-TIP-ATB-CTRA-02)
               AND A.NR_ETRD_CRCT_CTRA   > 0
               AND B.TS_EST_CRCT_CTRA = (SELECT MAX(C.TS_EST_CRCT_CTRA)
                   FROM DB2SGT.EST_CRCT_CTRA_TVM C
                   WHERE B.CD_CTRA_TVM         =  C.CD_CTRA_TVM
                     AND B.CD_TIP_ATB_CTRA     =  C.CD_TIP_ATB_CTRA
                     AND B.NR_ETRD_CRCT_CTRA   =  C.NR_ETRD_CRCT_CTRA
                     AND C.CD_TIP_EST_DCS      = :AUX-CD-TIP-EST-DCS
                     AND C.CD_TIP_ATB_CTRA     IN
                               (:SGTSL037-E-CD-TIP-ATB-CTRA-01 ,
                                :SGTSL037-E-CD-TIP-ATB-CTRA-02))
           ORDER BY A.CD_TIP_ATB_CTRA , B.TS_EST_CRCT_CTRA   DESC
           END-EXEC.

      *-----------------------------------------------------------------
      *--> Pesquisa Característica da Carteira
      *-----------------------------------------------------------------
           EXEC SQL
             DECLARE CURSOR-PSQ-3 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT  A.CD_CTRA_TVM
                  ,  A.CD_TIP_ATB_CTRA
                  ,  A.NR_ETRD_CRCT_CTRA
                  ,  A.CD_UOR_VCLD_CRCT
                  ,  A.VL_VCLD_CRCT_CTRA
                  ,  A.PC_VCLD_CRCT_CTRA
                  ,  A.NR_NOTA_TCN_OPR
                  ,  A.TX_VCLD_CRCT
             FROM DB2SGT.CRCT_CTRA_TVM A
             WHERE  A.CD_TIP_ATB_CTRA   = :SGTSL037-E-CD-TIP-ATB-CTRA-01
               AND  A.NR_ETRD_CRCT_CTRA = :SGTSL037-E-NR-ETRD-CRCT-CTRA
             ORDER BY A.CD_CTRA_TVM,
                      A.NR_ETRD_CRCT_CTRA
           END-EXEC.

      *-----------------------------------------------------------------
      *--> Pesquisa Característica das Carteiras
      *-----------------------------------------------------------------
           EXEC SQL
             DECLARE CURSOR-PSQ-4 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_CTRA_TVM
                  , A.CD_TIP_ATB_CTRA
                  , A.NR_ETRD_CRCT_CTRA
                  , A.CD_UOR_VCLD_CRCT
                  , A.VL_VCLD_CRCT_CTRA
                  , A.TX_VCLD_CRCT
                  , A.PC_VCLD_CRCT_CTRA
                  , A.NR_NOTA_TCN_OPR
                  , B.TS_EST_CRCT_CTRA
                  , B.CD_TIP_EST_DCS
                  , B.CD_USU_ATL_EST
                  , B.TX_OBS_EST_CRCT
             FROM DB2SGT.CRCT_CTRA_TVM  A
             INNER JOIN DB2SGT.EST_CRCT_CTRA_TVM  B
                    ON  A.CD_CTRA_TVM       =  B.CD_CTRA_TVM
                    AND A.CD_TIP_ATB_CTRA   =  B.CD_TIP_ATB_CTRA
                    AND A.NR_ETRD_CRCT_CTRA =  B.NR_ETRD_CRCT_CTRA

             WHERE A.CD_TIP_ATB_CTRA     =
                                         :SGTSL037-E-CD-TIP-ATB-CTRA-01
               AND B.TS_EST_CRCT_CTRA = (SELECT MAX(C.TS_EST_CRCT_CTRA)
                   FROM DB2SGT.EST_CRCT_CTRA_TVM C
                   WHERE B.CD_CTRA_TVM        =  C.CD_CTRA_TVM
                     AND B.CD_TIP_ATB_CTRA    =  C.CD_TIP_ATB_CTRA
                     AND C.CD_TIP_EST_DCS     =  :AUX-CD-TIP-EST-DCS
                     AND C.CD_TIP_ATB_CTRA    =
                                         :SGTSL037-E-CD-TIP-ATB-CTRA-01)
            ORDER BY  B.TS_EST_CRCT_CTRA   DESC
           END-EXEC.

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
                    VARYING SGTSL037-IC-TAB FROM 1 BY 1
                    UNTIL SGTSL037-IC-TAB > SGTSL037-QT-OCR-TAB

           PERFORM 300000-POSTA-RESPOSTA

           GOBACK
           .
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
              CONTAINER ( 'SGTSL037-RPST' )
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

           MOVE LENGTH OF SGTSL037-RQSC    TO  QT-CCT-EPRO
           EXEC CICS GET
              CONTAINER ( 'SGTSL037-RQSC' )
              CHANNEL   ( CANAL      )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSL037-RQSC' )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSL037-RQSC )
              INTO    ( SGTSL037-RQSC )
           END-EXEC
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO  SGTSL037-IN-CTNC-PSQ

           IF  SGTSL037-CD-TIP-SRVC    EQUAL ZEROS   OR
               SGTSL037-CD-TIP-SRVC    GREATER 4
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 107                TO  MSG1-CD-ERRO-TS
               MOVE 'Código do tipo de serviço inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validação obrigatória para pesquisas 1 e 2
           IF NOT SGTSL037-LISTA-CRCT-CTRA
              AND NOT SGTSL037-LISTA-POR-TIP-ATB
              IF SGTSL037-E-CD-CTRA-TVM     EQUAL ZEROS
                 SET  ERRO-PROGRAMADO  TO  TRUE
                 MOVE 002              TO  MSG1-CD-ERRO-TS
                 MOVE 'Codigo Carteira TVM deve ser informado.'
                                       TO  MSG1-TX-ERRO-TS
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-IF
           END-IF

      *--> Validação obrigatória para pesquisas 1 e 3
           IF NOT SGTSL037-IN-CRCT-GLB
            IF  SGTSL037-E-CD-TIP-ATB-CTRA-01  EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 602                TO  MSG1-CD-ERRO-TS
               MOVE 'Codigo Tipo Atritubo Carteira deve ser informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
            END-IF
           END-IF

      *--> Validação obrigatória só para pesquisa 3
           IF SGTSL037-LISTA-CRCT-CTRA
              IF SGTSL037-E-NR-ETRD-CRCT-CTRA EQUAL ZEROS
                 SET  ERRO-PROGRAMADO   TO  TRUE
                 MOVE 753               TO  MSG1-CD-ERRO-TS
                 MOVE 'Número estruturado da característica da carteira
      -               'inválido.'       TO  MSG1-TX-ERRO-TS
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-IF
           END-IF
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA        SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO  MSG1-CD-SEQL-ERRO

           MOVE CD-CTRA-TVM(SGTSL037-IC-TAB)       TO
                             SGTSL037-CD-CTRA-TVM(SGTSL037-IC-TAB)

           MOVE CD-TIP-ATB-CTRA(SGTSL037-IC-TAB)   TO
                             SGTSL037-CD-TIP-ATB-CTRA(SGTSL037-IC-TAB)

           MOVE NR-ETRD-CRCT-CTRA(SGTSL037-IC-TAB) TO
                             SGTSL037-NR-ETRD-CRCT-CTRA(SGTSL037-IC-TAB)

           MOVE CD-UOR-VCLD-CRC(SGTSL037-IC-TAB)   TO
                             SGTSL037-CD-UOR-VCLD-CRC(SGTSL037-IC-TAB)

           MOVE VL-VCLD-CRCT-CTRA(SGTSL037-IC-TAB) TO
                             SGTSL037-VL-VCLD-CRCT-CTRA(SGTSL037-IC-TAB)

           MOVE PC-VCLD-CRCT-CTRA(SGTSL037-IC-TAB) TO
                             SGTSL037-PC-VCLD-CRCT-CTRA(SGTSL037-IC-TAB)

           MOVE NR-NOTA-TCN-OPR(SGTSL037-IC-TAB)   TO
                             SGTSL037-NR-NOTA-TCN-OPR(SGTSL037-IC-TAB)

           MOVE TS-EST-CRCT-CTRA(SGTSL037-IC-TAB)  TO
                             SGTSL037-TS-EST-CRCT-CTRA(SGTSL037-IC-TAB)

           MOVE CD-TIP-EST-DCS(SGTSL037-IC-TAB)    TO
                             SGTSL037-CD-TIP-EST-DCS(SGTSL037-IC-TAB)

           MOVE CD-USU-ATL-EST(SGTSL037-IC-TAB)    TO
                             SGTSL037-CD-USU-ATL-EST(SGTSL037-IC-TAB)

           MOVE TX-OBS-EST-CRCT(SGTSL037-IC-TAB)   TO
                             SGTSL037-TX-OBS-EST-CRCT(SGTSL037-IC-TAB)

           MOVE IN-CRCT-GLB(SGTSL037-IC-TAB)       TO
                             SGTSL037-IN-CRCT-GLB-CTRA(SGTSL037-IC-TAB)

           MOVE TX-VCLD-CRCT(SGTSL037-IC-TAB)   TO
                             SGTSL037-TX-VCLD-CRCT(SGTSL037-IC-TAB)
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO  MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
              CONTAINER ( 'SGTSL037-RPST' )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSL037-RPST )
              FROM    ( SGTSL037-RPST )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO  MSG1-CD-SEQL-ERRO

           EVALUATE  TRUE
               WHEN  SGTSL037-LISTA-RECENTE
                     EXEC SQL
                          OPEN CURSOR-PSQ-1
                     END-EXEC

               WHEN  SGTSL037-IN-CRCT-GLB
                     EXEC SQL
                          OPEN CURSOR-PSQ-2
                     END-EXEC

               WHEN  SGTSL037-LISTA-CRCT-CTRA
                     EXEC SQL
                          OPEN CURSOR-PSQ-3
                     END-EXEC

               WHEN  SGTSL037-LISTA-POR-TIP-ATB
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
               WHEN  SGTSL037-LISTA-RECENTE
                  EXEC SQL
                    FETCH NEXT ROWSET
                          CURSOR-PSQ-1 FOR 0101 ROWS
                    INTO :CD-CTRA-TVM
                       , :CD-TIP-ATB-CTRA
                       , :NR-ETRD-CRCT-CTRA
                       , :CD-UOR-VCLD-CRC
                         :CD-UOR-VCLD-CRC-N
                       , :VL-VCLD-CRCT-CTRA
                         :VL-VCLD-CRCT-CTRA-N
                       , :TX-VCLD-CRCT
                         :TX-VCLD-CRCT-N
                       , :PC-VCLD-CRCT-CTRA
                         :PC-VCLD-CRCT-CTRA-N
                       , :NR-NOTA-TCN-OPR
                       , :TS-EST-CRCT-CTRA
                       , :CD-TIP-EST-DCS
                       , :CD-USU-ATL-EST
                       , :TX-OBS-EST-CRCT
                         :TX-OBS-EST-CRCT-N
                  END-EXEC

               WHEN  SGTSL037-IN-CRCT-GLB
                  EXEC SQL
                    FETCH NEXT ROWSET
                          CURSOR-PSQ-2 FOR 0101 ROWS
                    INTO :CD-CTRA-TVM
                       , :CD-TIP-ATB-CTRA
                       , :NR-ETRD-CRCT-CTRA
                       , :CD-UOR-VCLD-CRC
                         :CD-UOR-VCLD-CRC-N
                       , :VL-VCLD-CRCT-CTRA
                         :VL-VCLD-CRCT-CTRA-N
                       , :PC-VCLD-CRCT-CTRA
                         :PC-VCLD-CRCT-CTRA-N
                       , :NR-NOTA-TCN-OPR
                       , :TS-EST-CRCT-CTRA
                       , :CD-TIP-EST-DCS
                       , :CD-USU-ATL-EST
                       , :TX-OBS-EST-CRCT
                         :TX-OBS-EST-CRCT-N
                       , :IN-CRCT-GLB
                  END-EXEC

               WHEN  SGTSL037-LISTA-CRCT-CTRA
                  EXEC SQL
                    FETCH NEXT ROWSET
                          CURSOR-PSQ-3 FOR 0101 ROWS
                    INTO :CD-CTRA-TVM
                       , :CD-TIP-ATB-CTRA
                       , :NR-ETRD-CRCT-CTRA
                       , :CD-UOR-VCLD-CRC
                         :CD-UOR-VCLD-CRC-N
                       , :VL-VCLD-CRCT-CTRA
                         :VL-VCLD-CRCT-CTRA-N
                       , :PC-VCLD-CRCT-CTRA
                         :PC-VCLD-CRCT-CTRA-N
                       , :NR-NOTA-TCN-OPR
                       , :TX-VCLD-CRCT
                         :TX-VCLD-CRCT-N
                  END-EXEC

               WHEN  SGTSL037-LISTA-POR-TIP-ATB
                  EXEC SQL
                    FETCH NEXT ROWSET
                          CURSOR-PSQ-4 FOR 0101 ROWS
                    INTO :CD-CTRA-TVM
                       , :CD-TIP-ATB-CTRA
                       , :NR-ETRD-CRCT-CTRA
                       , :CD-UOR-VCLD-CRC
                         :CD-UOR-VCLD-CRC-N
                       , :VL-VCLD-CRCT-CTRA
                         :VL-VCLD-CRCT-CTRA-N
                       , :TX-VCLD-CRCT
                         :TX-VCLD-CRCT-N
                       , :PC-VCLD-CRCT-CTRA
                         :PC-VCLD-CRCT-CTRA-N
                       , :NR-NOTA-TCN-OPR
                       , :TS-EST-CRCT-CTRA
                       , :CD-TIP-EST-DCS
                       , :CD-USU-ATL-EST
                       , :TX-OBS-EST-CRCT
                         :TX-OBS-EST-CRCT-N
                  END-EXEC
           END-EVALUATE

           MOVE SQLERRD(3)             TO  QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL037-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO  QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO  SGTSL037-QT-OCR-TAB
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
               WHEN  SGTSL037-LISTA-RECENTE
                     EXEC SQL
                     CLOSE CURSOR-PSQ-1
                     END-EXEC

               WHEN  SGTSL037-IN-CRCT-GLB
                     EXEC SQL
                     CLOSE CURSOR-PSQ-2
                     END-EXEC

               WHEN  SGTSL037-LISTA-CRCT-CTRA
                     EXEC SQL
                     CLOSE CURSOR-PSQ-3
                     END-EXEC

               WHEN  SGTSL037-LISTA-POR-TIP-ATB
                     EXEC SQL
                     CLOSE CURSOR-PSQ-4
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

           MOVE 'SGTSL037-CD-TIP-SRVC'         TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSL037-CD-TIP-SRVC           TO  MCR-S9-15
           MOVE MCR-S9-15                      TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL037-E-CD-CTRA-TVM'       TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSL037-E-CD-CTRA-TVM         TO  MCR-S9-15
           MOVE MCR-S9-15                      TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL037-E-CD-TIP-ATB-CTRA-01'
                                               TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSL037-E-CD-TIP-ATB-CTRA-01  TO  MCR-S9-15
           MOVE MCR-S9-15                      TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL037-E-CD-TIP-ATB-CTRA-02'
                                               TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSL037-E-CD-TIP-ATB-CTRA-02  TO  MCR-S9-15
           MOVE MCR-S9-15                      TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL037-E-CD-TIP-ATB-CTRA-03'
                                               TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSL037-E-CD-TIP-ATB-CTRA-03  TO  MCR-S9-15
           MOVE MCR-S9-15                      TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL037-E-NR-ETRD-CRCT-CTRA' TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSL037-E-NR-ETRD-CRCT-CTRA   TO  MCR-S9-15
           MOVE MCR-S9-15                      TO  MSGKW999-VL-VRV-MNT
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