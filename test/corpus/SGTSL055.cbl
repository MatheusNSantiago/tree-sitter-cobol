      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL055
      * ANALISTA..: F0175702 ADRIANO SEIJI NAKAMURA
      * AUTOR.....: F0175702 ADRIANO SEIJI NAKAMURA
      * OBJETIVO..: Listar Nota Tecnica de Operacao Estruturada
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 18/01/2018 F8785286 DE 2016/145195 - Acerto dos códigos
      *          das mensagens para mensageria.
      *        16/11/2016 F0175702 DE 2016/145195 - IMPLANTACAO
      *----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL055.
      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-SS                 PIC  X(040)  VALUE
                   '*** S.S. COMEÇA AQUI ***'.
       77  CTE-PROG     PIC  X(17)     VALUE '*** SGTSL055 ***'.
       77  CTE-VERS     PIC  X(06)     VALUE 'VRS001'.
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

      * Area de trabalho

       01 QT-REG-SELD                  PIC S9(4) COMP-5 VALUE 0.
       01 WS-CD-ETR-INC                PIC S9(4) COMP-5 VALUE 0.
       01 WS-CD-ETR-FIM                PIC S9(4) COMP-5 VALUE 9999.
       01 WS-CD-TIP-DOC-INC            PIC S9(4) COMP-5 VALUE 0.
       01 WS-CD-TIP-DOC-FIM            PIC S9(4) COMP-5 VALUE 9999.
       01 WS-NR-NOTA-TCN-OPR-INC       PIC S9(4) COMP-5 VALUE 0.
       01 WS-NR-NOTA-TCN-OPR-FIM       PIC S9(4) COMP-5 VALUE 9999.

      * Campos utilizados para leitura
       01  REG-WS.
           03 NR-NOTA-TCN-OPR          PIC S9(4) COMP-5
           OCCURS 101 TIMES            VALUE 0.
           03 CD-ARQ-OPR-ETRD          PIC S9(9) COMP-5
           OCCURS 101 TIMES            VALUE 0.
           03 CD-UOR-DRTA-OGNL         PIC S9(9) COMP-5
           OCCURS 101 TIMES            VALUE 0.
           03 CD-REF-NOTA-TCN          PIC X(12)
           OCCURS 101 TIMES            VALUE ' '.
           03 DT-EMS-NOTA              PIC X(10)
           OCCURS 101 TIMES            VALUE ' '.
           03 DT-APVC-NOTA             PIC X(10)
           OCCURS 101 TIMES            VALUE ' '.
           03 CD-ETR-DCS               PIC S9(4) COMP-5
           OCCURS 101 TIMES            VALUE 0.
           03 CD-TIP-DOC-TCN           PIC S9(4) COMP-5
           OCCURS 101 TIMES            VALUE 0.
           03 CD-USU-RSP-ULT-ATL       PIC X(08)
           OCCURS 101 TIMES            VALUE ' '.
           03 TS-ULT-ATL               PIC X(26)
           OCCURS 101 TIMES            VALUE ' '.
           03 TX-TIP-DOC-TCN           PIC X(40)
           OCCURS 101 TIMES            VALUE ' '.
           03 CD-ETR-DCS-N             PIC S9(4) COMP-5
           OCCURS 101 TIMES            VALUE 0.
           03 CD-TIP-DOC-TCN-N         PIC S9(4) COMP-5
           OCCURS 101 TIMES            VALUE 0.
           03 CD-ARQ-OPR-ETRD-N        PIC S9(4) COMP-5
           OCCURS 101 TIMES            VALUE 0.

      * Indicadores de nulidades

      * Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ                        VALUE +100.
           03  IN-CURSOR-PSQ-ABTO      PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO                VALUE 'S'.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
-INC SGTKL055
      *--> Caso haja mais que 100 occ, baixa deve pegar ult-occ e chamar
           EXEC SQL
             DECLARE CURSOR-PSQ-1 CURSOR
             WITH ROWSET POSITIONING FOR
             SELECT
               A.NR_NOTA_TCN_OPR
             , A.CD_ARQ_OPR_ETRD
             , A.CD_UOR_DRTA_OGNL
             , A.CD_REF_NOTA_TCN
             , A.DT_EMS_NOTA
             , A.DT_APVC_NOTA
             , A.CD_ETR_DCS
             , A.CD_TIP_DOC_TCN
             , A.CD_USU_RSP_ULT_ATL
             , A.TS_ULT_ATL
             FROM DB2SGT.NOTA_TCN_OPR_ETRD  A
             WHERE
             A.NR_NOTA_TCN_OPR > :SGTSL055-E-NR-NOTA-TCN-OPR
             AND A.CD_REF_NOTA_TCN = :SGTSL055-E-CD-REF-NOTA-TCN
             ORDER BY A.NR_NOTA_TCN_OPR
           END-EXEC.

      *--> Pesquisa Todas as Notas Tcnicas cadastradas no SGT.
           EXEC SQL
             DECLARE CURSOR-PSQ-2 CURSOR
             WITH ROWSET POSITIONING FOR
             SELECT
               A.NR_NOTA_TCN_OPR
             , A.CD_ARQ_OPR_ETRD
             , A.CD_UOR_DRTA_OGNL
             , A.CD_REF_NOTA_TCN
             , A.DT_EMS_NOTA
             , A.DT_APVC_NOTA
             , A.CD_ETR_DCS
             , A.CD_TIP_DOC_TCN
             , B.TX_TIP_DOC_TCN
             FROM DB2SGT.NOTA_TCN_OPR_ETRD  A
             INNER JOIN DB2SGT.TIP_DOC_NOTA_TCN  B
             ON A.CD_TIP_DOC_TCN = B.CD_TIP_DOC_TCN
             WHERE
                 UPPER(COALESCE(A.CD_REF_NOTA_TCN, ' ')) LIKE
                                UPPER(:SGTSL055-E-CD-REF-NOTA-TCN)
             AND A.DT_EMS_NOTA >= :SGTSL055-E-DT-INC-EMS-NOTA
             AND A.DT_EMS_NOTA <= :SGTSL055-E-DT-FIM-EMS-NOTA
             AND A.CD_ETR_DCS >= :WS-CD-ETR-INC
             AND A.CD_ETR_DCS <= :WS-CD-ETR-FIM
             AND A.CD_TIP_DOC_TCN >= :WS-CD-TIP-DOC-INC
             AND A.CD_TIP_DOC_TCN <= :WS-CD-TIP-DOC-FIM
             AND A.NR_NOTA_TCN_OPR >= :WS-NR-NOTA-TCN-OPR-INC
             AND A.NR_NOTA_TCN_OPR <= :WS-NR-NOTA-TCN-OPR-FIM
             AND A.NR_NOTA_TCN_OPR > :SGTSL055-NR-ULT-NOTA-TCN-OPR

             ORDER BY A.NR_NOTA_TCN_OPR
           END-EXEC.

      *--> Pesquisa Nota Tcnica cadastradas pelo Nro da Nota.
           EXEC SQL
             DECLARE CURSOR-PSQ-3 CURSOR
             WITH ROWSET POSITIONING FOR
             SELECT
               A.CD_REF_NOTA_TCN
             , A.CD_ARQ_OPR_ETRD
             , A.CD_UOR_DRTA_OGNL
             , A.DT_EMS_NOTA
             , A.DT_APVC_NOTA
             , A.CD_ETR_DCS
             , A.CD_TIP_DOC_TCN
             , B.TX_TIP_DOC_TCN
             , A.CD_USU_RSP_ULT_ATL
             , A.TS_ULT_ATL
             FROM DB2SGT.NOTA_TCN_OPR_ETRD  A
             INNER JOIN DB2SGT.TIP_DOC_NOTA_TCN  B
             ON A.CD_TIP_DOC_TCN = B.CD_TIP_DOC_TCN
             WHERE
             A.NR_NOTA_TCN_OPR = :SGTSL055-E-NR-NOTA-TCN-OPR
           END-EXEC.

      *--> Pesquisa Nota TÉcnica cadastradas pelo Cod. Ref.
           EXEC SQL
             DECLARE CURSOR-PSQ-4 CURSOR
             WITH ROWSET POSITIONING FOR
             SELECT
               A.NR_NOTA_TCN_OPR
             , A.CD_ARQ_OPR_ETRD
             , A.CD_UOR_DRTA_OGNL
             , A.DT_EMS_NOTA
             , A.DT_APVC_NOTA
             , A.CD_ETR_DCS
             , A.CD_TIP_DOC_TCN
             , B.TX_TIP_DOC_TCN
             , A.CD_USU_RSP_ULT_ATL
             , A.TS_ULT_ATL
             FROM DB2SGT.NOTA_TCN_OPR_ETRD  A
             INNER JOIN DB2SGT.TIP_DOC_NOTA_TCN  B
             ON A.CD_TIP_DOC_TCN = B.CD_TIP_DOC_TCN
             WHERE
             A.CD_REF_NOTA_TCN = :SGTSL055-E-CD-REF-NOTA-TCN
           END-EXEC.

       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
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

           PERFORM 200000-PROCESSA-PESQUISA
             VARYING SGTSL055-IC-TAB FROM 1 BY 1
             UNTIL SGTSL055-IC-TAB GREATER SGTSL055-QT-OCR-TAB
           PERFORM 970103-FECHA-CURSOR-PSQ

           PERFORM 300000-POSTA-RESPOSTA
           GOBACK
           .
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO  TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       100000-SAI.
           EXIT.

      *---------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST        SECTION.
      *---------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSL055-RPST' )
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
       120000-EXCLUIR-CTNR-ERRO        SECTION.
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
       130000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                      TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL055-RQSC    TO QT-CCT-EPRO

           EXEC CICS GET
              CONTAINER ( 'SGTSL055-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM 999000-ERRO
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSL055-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL055-RQSC )
              INTO      ( SGTSL055-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *--------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *--------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO SGTSL055-IN-CTNC-PSQ

           EVALUATE SGTSL055-CD-TIP-SRVC
           WHEN 1
               IF  SGTSL055-E-CD-REF-NOTA-TCN EQUAL ZEROS
                   SET ERRO-PROGRAMADO TO TRUE
                   MOVE 0679           TO MSG1-CD-ERRO-TS
                   MOVE 'Código de referência da nota técnica não inform
      -                 'ado.'         TO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-IF

           WHEN 2
               INSPECT SGTSL055-E-CD-REF-NOTA-TCN
                             REPLACING ALL SPACES BY '%'

               IF  SGTSL055-E-CD-TIP-DOC-TCN > ZEROS
                   MOVE SGTSL055-E-CD-TIP-DOC-TCN TO WS-CD-TIP-DOC-INC
                                                     WS-CD-TIP-DOC-FIM
               END-IF

               IF  SGTSL055-E-CD-ETR-DCS > ZEROS
                   MOVE SGTSL055-E-CD-ETR-DCS TO WS-CD-ETR-INC
                                                 WS-CD-ETR-FIM
               END-IF

               IF  SGTSL055-E-DT-INC-EMS-NOTA EQUAL SPACES
                   MOVE '01.01.0001'   TO SGTSL055-E-DT-INC-EMS-NOTA
               END-IF

               IF  SGTSL055-E-DT-FIM-EMS-NOTA EQUAL SPACES
                   MOVE '31.12.9999'   TO SGTSL055-E-DT-FIM-EMS-NOTA
               END-IF

               IF  SGTSL055-E-NR-NOTA-TCN-OPR > ZEROS
                   MOVE SGTSL055-E-NR-NOTA-TCN-OPR  TO
                                            WS-NR-NOTA-TCN-OPR-INC
                                            WS-NR-NOTA-TCN-OPR-FIM
               END-IF

           WHEN 3
               IF  SGTSL055-E-NR-NOTA-TCN-OPR   <= ZEROS
                   SET ERRO-PROGRAMADO TO TRUE
                   MOVE 0182           TO MSG1-CD-ERRO-TS
                   MOVE 'Número nota técnica operação inválido.'
                                       TO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-IF

           WHEN 4
               IF  SGTSL055-E-CD-REF-NOTA-TCN   EQUAL SPACES
                   SET ERRO-PROGRAMADO TO TRUE
                   MOVE 0284           TO MSG1-CD-ERRO-TS
                   MOVE 'Código referência nota técnica inválido.'
                                       TO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-IF

           END-EVALUATE
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA        SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE NR-NOTA-TCN-OPR         (SGTSL055-IC-TAB)
                           TO SGTSL055-NR-NOTA-TCN-OPR(SGTSL055-IC-TAB)
           MOVE CD-ARQ-OPR-ETRD         (SGTSL055-IC-TAB)
                           TO SGTSL055-CD-ARQ-OPR-ETRD(SGTSL055-IC-TAB)
           MOVE CD-UOR-DRTA-OGNL        (SGTSL055-IC-TAB)
                           TO SGTSL055-CD-UOR-DRTA-OGNL(SGTSL055-IC-TAB)
           MOVE CD-REF-NOTA-TCN         (SGTSL055-IC-TAB)
                           TO SGTSL055-CD-REF-NOTA-TCN(SGTSL055-IC-TAB)
           MOVE DT-EMS-NOTA             (SGTSL055-IC-TAB)
                           TO SGTSL055-DT-EMS-NOTA    (SGTSL055-IC-TAB)
           MOVE DT-APVC-NOTA            (SGTSL055-IC-TAB)
                           TO SGTSL055-DT-APVC-NOTA   (SGTSL055-IC-TAB)
           MOVE CD-ETR-DCS              (SGTSL055-IC-TAB)
                           TO SGTSL055-CD-ETR-DCS     (SGTSL055-IC-TAB)
           MOVE CD-TIP-DOC-TCN          (SGTSL055-IC-TAB)
                           TO SGTSL055-CD-TIP-DOC-TCN (SGTSL055-IC-TAB)
           MOVE CD-USU-RSP-ULT-ATL      (SGTSL055-IC-TAB)
                         TO SGTSL055-CD-USU-RSP-ULT-ATL(SGTSL055-IC-TAB)
           MOVE TS-ULT-ATL              (SGTSL055-IC-TAB)
                           TO SGTSL055-TS-ULT-ATL     (SGTSL055-IC-TAB)
           MOVE TX-TIP-DOC-TCN          (SGTSL055-IC-TAB)
                           TO SGTSL055-TX-TIP-DOC-TCN (SGTSL055-IC-TAB)
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
              CONTAINER ( 'SGTSL055-RPST'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL055-RPST )
              FROM      ( SGTSL055-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           EVALUATE SGTSL055-CD-TIP-SRVC
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

           WHEN 4
                EXEC SQL
                     OPEN CURSOR-PSQ-4
                END-EXEC

           END-EVALUATE
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ           SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EVALUATE SGTSL055-CD-TIP-SRVC
           WHEN 1
               EXEC SQL
                    FETCH NEXT ROWSET
                    CURSOR-PSQ-1 FOR 101 ROWS
                    INTO :NR-NOTA-TCN-OPR
                       , :CD-ARQ-OPR-ETRD
                         :CD-ARQ-OPR-ETRD-N
                       , :CD-UOR-DRTA-OGNL
                       , :CD-REF-NOTA-TCN
                       , :DT-EMS-NOTA
                       , :DT-APVC-NOTA
                       , :CD-ETR-DCS
                         :CD-ETR-DCS-N
                       , :CD-TIP-DOC-TCN
                         :CD-TIP-DOC-TCN-N
                       , :CD-USU-RSP-ULT-ATL
                       , :TS-ULT-ATL
               END-EXEC

           WHEN 2
               EXEC SQL
                    FETCH NEXT ROWSET
                    CURSOR-PSQ-2 FOR 101 ROWS
                    INTO :NR-NOTA-TCN-OPR
                       , :CD-ARQ-OPR-ETRD
                         :CD-ARQ-OPR-ETRD-N
                       , :CD-UOR-DRTA-OGNL
                       , :CD-REF-NOTA-TCN
                       , :DT-EMS-NOTA
                       , :DT-APVC-NOTA
                       , :CD-ETR-DCS
                         :CD-ETR-DCS-N
                       , :CD-TIP-DOC-TCN
                         :CD-TIP-DOC-TCN-N
                       , :TX-TIP-DOC-TCN
               END-EXEC

           WHEN 3
               EXEC SQL
                    FETCH NEXT ROWSET
                    CURSOR-PSQ-3 FOR 101 ROWS
                    INTO :CD-REF-NOTA-TCN
                       , :CD-ARQ-OPR-ETRD
                         :CD-ARQ-OPR-ETRD-N
                       , :CD-UOR-DRTA-OGNL
                       , :DT-EMS-NOTA
                       , :DT-APVC-NOTA
                       , :CD-ETR-DCS
                         :CD-ETR-DCS-N
                       , :CD-TIP-DOC-TCN
                         :CD-TIP-DOC-TCN-N
                       , :TX-TIP-DOC-TCN
                       , :CD-USU-RSP-ULT-ATL
                       , :TS-ULT-ATL
               END-EXEC

           WHEN 4
               EXEC SQL
                    FETCH NEXT ROWSET
                    CURSOR-PSQ-4 FOR 101 ROWS
                    INTO :NR-NOTA-TCN-OPR
                       , :CD-ARQ-OPR-ETRD
                         :CD-ARQ-OPR-ETRD-N
                       , :CD-UOR-DRTA-OGNL
                       , :DT-EMS-NOTA
                       , :DT-APVC-NOTA
                       , :CD-ETR-DCS
                         :CD-ETR-DCS-N
                       , :CD-TIP-DOC-TCN
                         :CD-TIP-DOC-TCN-N
                       , :TX-TIP-DOC-TCN
                       , :CD-USU-RSP-ULT-ATL
                       , :TS-ULT-ATL
               END-EXEC

           END-EVALUATE

           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL055-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL055-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ         SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO

           EVALUATE SGTSL055-CD-TIP-SRVC
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

           WHEN 4
                EXEC SQL
                     CLOSE CURSOR-PSQ-4
                END-EXEC

           END-EVALUATE
           .
       970103-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *                                                                *
      * As sections abaixo iniciadas com 999000 representam os trata-  *
      * mentos de erro utilizando o Sistema MSG (rotina MSGS0001)      *
      *                                                                *
      *----------------------------------------------------------------*
       999000-ERRO                     SECTION.
      *----------------------------------------------------------------*

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

           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-ERRO-SAI.
           EXIT.

      *--------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *--------------------------------------------------------------

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

           MOVE 'SGTSL055-CD-TIP-SRVC'          TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL055-CD-TIP-SRVC            TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL055-E-NR-NOTA-TCN-OPR'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL055-E-NR-NOTA-TCN-OPR      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL055-E-CD-REF-NOTA-TCN'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL055-E-CD-REF-NOTA-TCN      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL055-E-DT-INC-EMS-NOTA'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL055-E-DT-INC-EMS-NOTA      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL055-E-DT-FIM-EMS-NOTA'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL055-E-DT-FIM-EMS-NOTA      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL055-E-CD-TIP-DOC-TCN '    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL055-E-CD-TIP-DOC-TCN       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL055-E-CD-ETR-DCS '        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL055-E-CD-ETR-DCS           TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL055-NR-ULT-NOTA-TCN-OPR'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL055-NR-ULT-NOTA-TCN-OPR    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS                 TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS                 TO IIBKERRO-TX-ERRO

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
       999999-ENCERRAR                 SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTSL055 -----------------------------