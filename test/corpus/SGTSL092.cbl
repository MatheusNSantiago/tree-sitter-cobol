      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL092
      * ANALISTA..: F2054109 - Cintia Iglesias
      * AUTOR.....: F9188847 Soila Akamatsu
      * OBJETIVO..: Listar regra de validação da característica
      *             e seus estados
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 13/12/2018 F9188847 Inclusão dos campos no cursor-01
      *        SGTSL092-CD-CTRA-TIT-VL-INI, SGTSL092-CD-CTRA-TIT-VL-FIM
      *        e SGTSL092-CD-CTRA-TIT-VL
      *        11.06.2018 F8785286 Inclusão do cursor-05 para listar
      *                     estado da versão informada.
      *        19.12.2017 F8785286 Inclusão do cursor-04 para listar
      *                            regras princ e secundárias.
      *        12.09.2017 F9188847 DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL092.
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
                   '*** S.S. COMEÇA AQUI ***'.
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSL092 ***'.
       77  CTE-VERS     PIC  X(06) VALUE 'VRS001'.
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
       01 QT-REG-SELD                         PIC S9(4) COMP-5 VALUE 0.

      *--> Data de trabalho
       01  AUX-DT-PSQ-INI              PIC X(10)   VALUE SPACES.
       01  AUX-DT-PSQ-INI-R            REDEFINES
           AUX-DT-PSQ-INI.
           03 ANO                      PIC X(04).
           03 FILLER                   PIC X(01).
           03 MES                      PIC X(02).
           03 FILLER                   PIC X(01).
           03 DIA                      PIC X(02).

       01  AUX-DT-PSQ-FIM              PIC X(10)   VALUE SPACES.
       01  AUX-DT-PSQ-FIM-R            REDEFINES
           AUX-DT-PSQ-FIM.
           03 ANO                      PIC X(04).
           03 FILLER                   PIC X(01).
           03 MES                      PIC X(02).
           03 FILLER                   PIC X(01).
           03 DIA                      PIC X(02).

       01  AUX-DT-PSQ                  PIC X(10)   VALUE SPACES.
       01  AUX-DT-PSQ-R                  REDEFINES
           AUX-DT-PSQ.
           03 ANO                      PIC X(04).
           03 FILLER                   PIC X(01).
           03 MES                      PIC X(02).
           03 FILLER                   PIC X(01).
           03 DIA                      PIC X(02).

       01 IND-PPL-1                    PIC X(01).
       01 IND-PPL-2                    PIC X(01).

      * Campos utilizados para leitura
       01  REG-WS.
           03 CD-RGR-VLDC                    PIC S9(9) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 NR-VRS-RGR-VLDC                PIC S9(9) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 CD-TIP-EST-DCS                 PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 CD-USU-RSP-ATL-EST             PIC X(8)
              OCCURS 101 TIMES               VALUE ' '.
           03 TS-EST-RGR-VLDC                PIC X(26)
              OCCURS 101 TIMES               VALUE ' '.
           03 TS-ESTADO                      PIC X(26)
              OCCURS 101 TIMES               VALUE ' '.
           03 NM-RGR-VLDC-CRCT               PIC X(250)
              OCCURS 101 TIMES               VALUE ' '.
           03 NM-RTL-RGR-VLDC                PIC X(60)
              OCCURS 101 TIMES               VALUE ' '.
           03 IN-RGR-PPL                     PIC X(1)
              OCCURS 101 TIMES               VALUE ' '.
           03 IN-RGR-VLDC-ATV                PIC X(1)
              OCCURS 101 TIMES               VALUE ' '.
           03 NR-NOTA-TCN-OPR                PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 NR-NOTA-TCN-OPR-N              PIC S9(04) COMP-5
              OCCURS 101 TIMES              VALUE 0.
           03 CD-CTRA-TIT-VL                 PIC S9(9) COMP-5
              OCCURS 101 TIMES               VALUE 0.

      * Campos utilizados para tratar campo nome para pesquisa.
       01  NM-PSQD                           PIC X(80) VALUE SPACES.
       01  TAM-PSQD                          PIC S9(4) COMP-5 VALUE 0.
       01  QT-CCT-DGTD                       PIC S9(4) COMP-5 VALUE 0.
       01  QT-CCT-N-DGTD                     PIC S9(4) COMP-5 VALUE 0.
       01  QT-CCT-PSQ                        PIC S9(4) COMP-5 VALUE 0.

      * Indicadores de nulidades
       01 QT-REG-SELD-N                       PIC S9(4) COMP-5 VALUE 0.

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
-INC SGTKL092

      * Cursor 01
      * Lista último Estado da Regra de Validação de Caracteristica

           EXEC SQL
             DECLARE CURSOR-PSQ-01 CURSOR
                     WITH ROWSET POSITIONING FOR
      *      SELECT DISTINCT C.CD_RGR_VLDC
      *           , C.NR_VRS_RGR_VLDC
      *           , C.TS_ESTADO
      *           , D.CD_TIP_EST_DCS
      *           , D.CD_USU_RSP_ATL_EST
      *           , B.NM_RGR_VLDC_CRCT
      *           , B.NM_RTL_RGR_VLDC
      *           , B.IN_RGR_PPL
      *           , B.IN_RGR_VLDC_ATV
      *           , IFNULL (B.NR_NOTA_TCN_OPR, 0)
      *       FROM
      *        (SELECT A.CD_RGR_VLDC
      *              , A.NR_VRS_RGR_VLDC
      *              , MAX(A.TS_EST_RGR_VLDC) AS TS_ESTADO
      *            FROM DB2SGT.EST_RGR_VLDC_CRCT  A
      *              WHERE A.CD_RGR_VLDC  >=
      *                   :SGTSL092-CD-RGR-VLDC-INI  AND
      *                   A.CD_RGR_VLDC  <=
      *                   :SGTSL092-CD-RGR-VLDC-FIM
      *              GROUP BY A.CD_RGR_VLDC ,
      *                       A.NR_VRS_RGR_VLDC)  C
      *       INNER JOIN
      *             DB2SGT.EST_RGR_VLDC_CRCT  D
      *       ON  C.CD_RGR_VLDC      =  D.CD_RGR_VLDC
      *       AND C.NR_VRS_RGR_VLDC  =  D.NR_VRS_RGR_VLDC
      *       AND C.TS_ESTADO        =  D.TS_EST_RGR_VLDC
      *       INNER JOIN
      *             DB2SGT.RGR_VLDC_CRCT_OPR B
      *       ON  C.CD_RGR_VLDC      =  B.CD_RGR_VLDC
      *       AND C.NR_VRS_RGR_VLDC  =  B.NR_VRS_RGR_VLDC
      *       INNER JOIN
      *             DB2SGT.MDLD_VCLD_RGR  E
      *       ON  C.CD_RGR_VLDC      =  E.CD_RGR_VLDC
      *       AND C.NR_VRS_RGR_VLDC  =  E.NR_VRS_RGR_VLDC
      *       WHERE UPPER(B.NM_RGR_VLDC_CRCT)
      *         LIKE UPPER(:NM-PSQD)
      *         AND (B.IN_RGR_PPL     =  :IND-PPL-1
      *         OR   B.IN_RGR_PPL     =  :IND-PPL-2)
      *         AND E.CD_PRD          >= :SGTSL092-CD-PRD-INI
      *         AND E.CD_PRD          <= :SGTSL092-CD-PRD-FIN
      *         AND E.CD_MDLD         >= :SGTSL092-CD-MDLD-INI
      *         AND E.CD_MDLD         <= :SGTSL092-CD-MDLD-FIN
      *         AND D.CD_TIP_EST_DCS >=  :SGTSL092-CD-TIP-EST-DCS-INI
      *         AND D.CD_TIP_EST_DCS <=  :SGTSL092-CD-TIP-EST-DCS-FIN
      *         AND C.TS_ESTADO      <   :SGTSL092-ULT-TS-EST
      *        ORDER BY C.TS_ESTADO DESC

              SELECT DISTINCT D.CD_RGR_VLDC
                  , D.NR_VRS_RGR_VLDC
                  , D.TS_EST_RGR_VLDC
                  , D.CD_TIP_EST_DCS
                  , D.CD_USU_RSP_ATL_EST
                  , B.NM_RGR_VLDC_CRCT
                  , B.NM_RTL_RGR_VLDC
                  , B.IN_RGR_PPL
                  , B.IN_RGR_VLDC_ATV
                  , IFNULL (B.NR_NOTA_TCN_OPR, 0)
                  , F.CD_CTRA_TIT_VL
              FROM DB2SGT.RGR_VLDC_CRCT_OPR B

              INNER JOIN
                    DB2SGT.EST_RGR_VLDC_CRCT  D
              ON  D.CD_RGR_VLDC      =  B.CD_RGR_VLDC
              AND D.NR_VRS_RGR_VLDC  =  B.NR_VRS_RGR_VLDC
              AND D.TS_EST_RGR_VLDC =
               (SELECT MAX(A.TS_EST_RGR_VLDC)
                   FROM DB2SGT.EST_RGR_VLDC_CRCT  A
                     WHERE A.CD_RGR_VLDC  =  B.CD_RGR_VLDC)

              INNER JOIN
                    DB2SGT.MDLD_VCLD_RGR  E
              ON  B.CD_RGR_VLDC      =  E.CD_RGR_VLDC
              AND B.NR_VRS_RGR_VLDC  =  E.NR_VRS_RGR_VLDC

              INNER JOIN
                    DB2SGT.CTRA_RGR_VLDC  F
              ON  B.CD_RGR_VLDC      =  F.CD_RGR_VLDC
              AND B.NR_VRS_RGR_VLDC  =  F.NR_VRS_RGR_VLDC

              WHERE UPPER(B.NM_RGR_VLDC_CRCT)
                LIKE UPPER(:NM-PSQD)
                AND (B.IN_RGR_PPL      = :IND-PPL-1
                OR   B.IN_RGR_PPL      = :IND-PPL-2)
                AND (B.CD_RGR_VLDC    >= :SGTSL092-CD-RGR-VLDC-INI
                AND  B.CD_RGR_VLDC    <= :SGTSL092-CD-RGR-VLDC-FIM)

                AND E.CD_PRD          >= :SGTSL092-CD-PRD-INI
                AND E.CD_PRD          <= :SGTSL092-CD-PRD-FIN
                AND E.CD_MDLD         >= :SGTSL092-CD-MDLD-INI
                AND E.CD_MDLD         <= :SGTSL092-CD-MDLD-FIN
                AND D.CD_TIP_EST_DCS  >= :SGTSL092-CD-TIP-EST-DCS-INI
                AND D.CD_TIP_EST_DCS  <= :SGTSL092-CD-TIP-EST-DCS-FIN
                AND D.TS_EST_RGR_VLDC <  :SGTSL092-ULT-TS-EST
                AND F.CD_CTRA_TIT_VL  >= :SGTSL092-CD-CTRA-TIT-VL-INI
                AND F.CD_CTRA_TIT_VL  <= :SGTSL092-CD-CTRA-TIT-VL-FIM
               ORDER BY D.TS_EST_RGR_VLDC DESC

           END-EXEC.
      *
      * Cursor 02
      * Lista Estados da Regra de Validação de Caracteristica em um
      * período

           EXEC SQL
             DECLARE CURSOR-PSQ-02 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT DISTINCT A.CD_RGR_VLDC
                  , A.NR_VRS_RGR_VLDC
                  , A.TS_EST_RGR_VLDC
                  , A.CD_TIP_EST_DCS
                  , B.NM_RGR_VLDC_CRCT
                  , B.NM_RTL_RGR_VLDC
                  , B.IN_RGR_PPL
                  , B.IN_RGR_VLDC_ATV
                  , IFNULL (B.NR_NOTA_TCN_OPR, 0)
             FROM DB2SGT.EST_RGR_VLDC_CRCT  A
             INNER JOIN
                    DB2SGT.RGR_VLDC_CRCT_OPR B
              ON  A.CD_RGR_VLDC      =  B.CD_RGR_VLDC
              AND A.NR_VRS_RGR_VLDC  =  B.NR_VRS_RGR_VLDC
             INNER JOIN
                    DB2SGT.MDLD_VCLD_RGR  C
              ON  A.CD_RGR_VLDC      =  C.CD_RGR_VLDC
              AND A.NR_VRS_RGR_VLDC  =  C.NR_VRS_RGR_VLDC
             WHERE UPPER(B.NM_RGR_VLDC_CRCT)
               LIKE UPPER(:NM-PSQD)
               AND (B.IN_RGR_PPL      =  :IND-PPL-1
                OR  B.IN_RGR_PPL      =  :IND-PPL-2)
               AND C.CD_PRD          >= :SGTSL092-CD-PRD-INI
               AND C.CD_PRD          <= :SGTSL092-CD-PRD-FIN
               AND C.CD_MDLD         >= :SGTSL092-CD-MDLD-INI
               AND C.CD_MDLD         <= :SGTSL092-CD-MDLD-FIN
               AND A.CD_TIP_EST_DCS   >= :SGTSL092-CD-TIP-EST-DCS-INI
               AND A.CD_TIP_EST_DCS   <= :SGTSL092-CD-TIP-EST-DCS-FIN
               AND A.CD_RGR_VLDC      >= :SGTSL092-CD-RGR-VLDC-INI
               AND A.CD_RGR_VLDC      <= :SGTSL092-CD-RGR-VLDC-FIM
               AND DATE(A.TS_EST_RGR_VLDC) >= :AUX-DT-PSQ-INI
               AND DATE(A.TS_EST_RGR_VLDC) <= :AUX-DT-PSQ-FIM
               AND A.TS_EST_RGR_VLDC  <   :SGTSL092-ULT-TS-EST
               ORDER BY A.TS_EST_RGR_VLDC  DESC
           END-EXEC.
      *
      * Cursor 03
      * Lista Estados da Regra de Validação de Caracteristica antes de
      * uma data
           EXEC SQL
             DECLARE CURSOR-PSQ-03 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT DISTINCT C.CD_RGR_VLDC
                  , C.NR_VRS_RGR_VLDC
                  , C.TS_ESTADO
                  , D.CD_TIP_EST_DCS
                  , B.NM_RGR_VLDC_CRCT
                  , B.NM_RTL_RGR_VLDC
                  , B.IN_RGR_PPL
                  , B.IN_RGR_VLDC_ATV
                  , IFNULL (B.NR_NOTA_TCN_OPR, 0)
             FROM
               (SELECT A.CD_RGR_VLDC
                     , A.NR_VRS_RGR_VLDC
                     , MAX(A.TS_EST_RGR_VLDC) AS TS_ESTADO
                FROM DB2SGT.EST_RGR_VLDC_CRCT  A
                WHERE A.CD_RGR_VLDC  >=
                     :SGTSL092-CD-RGR-VLDC-INI  AND
                     A.CD_RGR_VLDC  <=
                     :SGTSL092-CD-RGR-VLDC-FIM  AND
                     DATE(A.TS_EST_RGR_VLDC) <
                     :AUX-DT-PSQ
                GROUP BY A.CD_RGR_VLDC ,
                         A.NR_VRS_RGR_VLDC)  C
             INNER JOIN
                  DB2SGT.EST_RGR_VLDC_CRCT  D
             ON  C.CD_RGR_VLDC      =  D.CD_RGR_VLDC
             AND C.NR_VRS_RGR_VLDC  =  D.NR_VRS_RGR_VLDC
             AND C.TS_ESTADO        =  D.TS_EST_RGR_VLDC
             INNER JOIN
                    DB2SGT.RGR_VLDC_CRCT_OPR B
              ON  C.CD_RGR_VLDC      =  B.CD_RGR_VLDC
              AND C.NR_VRS_RGR_VLDC  =  B.NR_VRS_RGR_VLDC
             INNER JOIN
                    DB2SGT.MDLD_VCLD_RGR  E
              ON  C.CD_RGR_VLDC      =  E.CD_RGR_VLDC
              AND C.NR_VRS_RGR_VLDC  =  E.NR_VRS_RGR_VLDC
             WHERE  UPPER(B.NM_RGR_VLDC_CRCT)
               LIKE UPPER(:NM-PSQD)
               AND (B.IN_RGR_PPL     =  :IND-PPL-1
                OR  B.IN_RGR_PPL     =  :IND-PPL-2)
               AND E.CD_PRD          >= :SGTSL092-CD-PRD-INI
               AND E.CD_PRD          <= :SGTSL092-CD-PRD-FIN
               AND E.CD_MDLD         >= :SGTSL092-CD-MDLD-INI
               AND E.CD_MDLD         <= :SGTSL092-CD-MDLD-FIN
               AND D.CD_TIP_EST_DCS >=  :SGTSL092-CD-TIP-EST-DCS-INI
               AND D.CD_TIP_EST_DCS <=  :SGTSL092-CD-TIP-EST-DCS-FIN
               AND C.TS_ESTADO      <   :SGTSL092-ULT-TS-EST
             ORDER BY TS_ESTADO DESC
           END-EXEC.

      * Cursor 04
      * Lista Estados da Regra de Validação de Caracteristica independen
      * te de a regra ser principal ou secundária
           EXEC SQL
             DECLARE CURSOR-PSQ-04 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT   D.CD_RGR_VLDC
                  , D.NR_VRS_RGR_VLDC
                  , D.TS_EST_RGR_VLDC
                  , D.CD_TIP_EST_DCS
                  , D.CD_USU_RSP_ATL_EST
                  , B.NM_RGR_VLDC_CRCT
                  , B.NM_RTL_RGR_VLDC
                  , B.IN_RGR_PPL
                  , B.IN_RGR_VLDC_ATV
                  , IFNULL (B.NR_NOTA_TCN_OPR, 0)
              FROM DB2SGT.RGR_VLDC_CRCT_OPR B

              INNER JOIN
                    DB2SGT.EST_RGR_VLDC_CRCT  D
              ON  D.CD_RGR_VLDC      =  B.CD_RGR_VLDC
              AND D.NR_VRS_RGR_VLDC  =  B.NR_VRS_RGR_VLDC
              AND D.TS_EST_RGR_VLDC =
               (SELECT MAX(A.TS_EST_RGR_VLDC)
                   FROM DB2SGT.EST_RGR_VLDC_CRCT  A
                     WHERE A.CD_RGR_VLDC  =  D.CD_RGR_VLDC
                     AND A.CD_TIP_EST_DCS  =  D.CD_TIP_EST_DCS)

              WHERE UPPER(B.NM_RGR_VLDC_CRCT)
                LIKE UPPER(:NM-PSQD)
                AND (B.IN_RGR_PPL        = :IND-PPL-1
                OR   B.IN_RGR_PPL        = :IND-PPL-2)
                AND (B.CD_RGR_VLDC      >= :SGTSL092-CD-RGR-VLDC-INI
                AND  B.CD_RGR_VLDC      <= :SGTSL092-CD-RGR-VLDC-FIM)

                AND D.CD_TIP_EST_DCS    >= :SGTSL092-CD-TIP-EST-DCS-INI
                AND D.CD_TIP_EST_DCS    <= :SGTSL092-CD-TIP-EST-DCS-FIN
                AND D.TS_EST_RGR_VLDC   <  :SGTSL092-ULT-TS-EST

                AND ((D.CD_RGR_VLDC     >  :SGTSL092-CD-RGR-VLDC-INI) OR
                     (D.CD_RGR_VLDC      = :SGTSL092-CD-RGR-VLDC-INI AND
                      D.TS_EST_RGR_VLDC <  :SGTSL092-ULT-TS-EST))

               ORDER BY D.CD_RGR_VLDC
                      , D.TS_EST_RGR_VLDC DESC


           END-EXEC.

      * Cursor 05
      * Lista Estados da Versão da Regra de Validação de Caracteristica
           EXEC SQL
             DECLARE CURSOR-PSQ-05 CURSOR
                     WITH ROWSET POSITIONING FOR

             SELECT D.CD_RGR_VLDC
                  , D.NR_VRS_RGR_VLDC
                  , D.TS_EST_RGR_VLDC
                  , D.CD_TIP_EST_DCS
                  , D.CD_USU_RSP_ATL_EST
                  , B.NM_RGR_VLDC_CRCT
                  , B.NM_RTL_RGR_VLDC
                  , B.IN_RGR_PPL
                  , B.IN_RGR_VLDC_ATV

              FROM DB2SGT.RGR_VLDC_CRCT_OPR B

              INNER JOIN
                   DB2SGT.EST_RGR_VLDC_CRCT  D

              ON  D.CD_RGR_VLDC      =  B.CD_RGR_VLDC
              AND D.NR_VRS_RGR_VLDC  =  B.NR_VRS_RGR_VLDC
              AND D.TS_EST_RGR_VLDC  =
               (SELECT MAX(A.TS_EST_RGR_VLDC)
                   FROM DB2SGT.EST_RGR_VLDC_CRCT  A
                     WHERE A.CD_RGR_VLDC      = D.CD_RGR_VLDC
                       AND A.NR_VRS_RGR_VLDC  = D.NR_VRS_RGR_VLDC)


              WHERE  D.CD_RGR_VLDC      = :SGTSL092-CD-RGR-VLDC-INI
                AND  D.NR_VRS_RGR_VLDC  = :SGTSL092-NR-VRS-RGR-VLDC-INI

               ORDER BY D.CD_RGR_VLDC
                      , D.TS_EST_RGR_VLDC DESC
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
           PERFORM 141000-TRATAR-NOME

           PERFORM 200000-PROCESSA-PESQUISA

           PERFORM 300000-POSTA-RESPOSTA
           GOBACK
           .

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
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

      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSL092-RPST' )
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

           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL092-RQSC     TO QT-CCT-EPRO
           EXEC CICS GET
              CONTAINER ( 'SGTSL092-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA         TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS GET
              CONTAINER ( 'SGTSL092-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL092-RQSC )
              INTO      ( SGTSL092-RQSC           )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           IF  NOT  SGTSL092-LISTA-ULT-EST     AND
               NOT  SGTSL092-LISTA-POR-PERIODO AND
               NOT  SGTSL092-LISTA-ANTES-DATA  AND
               NOT  SGTSL092-LISTA-SEM-MDLD    AND
               NOT  SGTSL092-LISTA-VERSAO
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0107              TO  MSG1-CD-ERRO-TS
               MOVE  'Código tipo serviço inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL092-ULT-TS-EST  EQUAL  SPACES
               MOVE '2900-12-31-00.00.00.000000' TO
                                SGTSL092-ULT-TS-EST
           END-IF

           IF  SGTSL092-CD-CTRA-TIT-VL-FIM  EQUAL  ZEROS
               MOVE  99999999  TO  SGTSL092-CD-CTRA-TIT-VL-FIM
           END-IF

           IF  SGTSL092-CD-RGR-VLDC-FIM  EQUAL  ZEROS
               MOVE  9999  TO  SGTSL092-CD-RGR-VLDC-FIM
           END-IF

           IF  SGTSL092-CD-TIP-EST-DCS-FIN   EQUAL  ZEROS
               MOVE  9999  TO  SGTSL092-CD-TIP-EST-DCS-FIN
           END-IF

           IF  SGTSL092-DT-PSQ-INI  EQUAL  SPACES
               MOVE  '1900-01-01'      TO AUX-DT-PSQ-INI
           ELSE
               MOVE '    -  -  '       TO AUX-DT-PSQ-INI
               MOVE CORR SGTSL092-DT-PSQ-INI-R
                                       TO AUX-DT-PSQ-INI-R
           END-IF

           IF  SGTSL092-DT-PSQ-FIN  EQUAL  SPACES
               MOVE '2900-12-31'       TO AUX-DT-PSQ-FIM
           ELSE
               MOVE '    -  -  '       TO AUX-DT-PSQ-FIM
               MOVE CORR SGTSL092-DT-PSQ-FIN-R
                                       TO AUX-DT-PSQ-FIM-R
           END-IF

           MOVE '    -  -  '           TO AUX-DT-PSQ
           MOVE CORR SGTSL092-DT-PSQ-R
                                       TO AUX-DT-PSQ-R

           IF  SGTSL092-IN-RGR-PPL-E EQUAL SPACES
              MOVE 'S'              TO IND-PPL-1
              MOVE 'N'              TO IND-PPL-2
           ELSE
              MOVE SGTSL092-IN-RGR-PPL-E TO  IND-PPL-1
                                             IND-PPL-2
           END-IF

           IF  SGTSL092-CD-PRD-FIN  EQUAL  ZEROS
               MOVE  9999              TO  SGTSL092-CD-PRD-FIN
           END-IF

           IF  SGTSL092-CD-MDLD-FIN   EQUAL  ZEROS
               MOVE  9999              TO  SGTSL092-CD-MDLD-FIN
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       141000-TRATAR-NOME              SECTION.
      *--> Trocar brancos do final da string por %
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE SPACES                 TO NM-PSQD
           MOVE ZEROS                  TO QT-CCT-DGTD
                                          QT-CCT-N-DGTD
                                          QT-CCT-PSQ

      ***  Busca a quantidade de caracteres não digitados, ou seja,
      ***  a quantidade de caracteres em brancos à esquerda.

           INSPECT FUNCTION REVERSE(SGTSL092-NM-RGR-VLDC-CRCT-E)
               TALLYING QT-CCT-N-DGTD  FOR LEADING SPACES

      ***  Calcula a quantidade de caracteres digitados

           MOVE   LENGTH OF SGTSL092-NM-RGR-VLDC-CRCT-E TO TAM-PSQD
           SUBTRACT QT-CCT-N-DGTD FROM TAM-PSQD
               GIVING QT-CCT-DGTD
               ON SIZE ERROR
                  SET   ERRO-PROGRAMADO   TO  TRUE
                  MOVE 0143               TO MSG1-CD-ERRO-TS
                  MOVE 'Erro no cálculo do tamanho do texto.'
                                          TO MSG1-TX-ERRO-TS
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-SUBTRACT

      ***  Calcula a quantidade de caracteres para pesquisa, ou seja,
      ***  prevendo a delimitação por 1 caracter % no lado direito.

           ADD 1                       TO QT-CCT-DGTD GIVING QT-CCT-PSQ
               ON SIZE ERROR
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE 0143               TO MSG1-CD-ERRO-TS
               MOVE 'Erro no cálculo do tamanho do texto para pesquisa'
                                       TO MSG1-TX-ERRO-TS
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-ADD

      ***  Formata campo de pesquisa com delimitador %
      ***  e preenche até o final do campo com %

           MOVE ALL '%'                TO NM-PSQD

           IF   QT-CCT-DGTD  GREATER ZEROS
                MOVE SGTSL092-NM-RGR-VLDC-CRCT-E   (1:QT-CCT-DGTD)
                                       TO NM-PSQD (1:QT-CCT-DGTD)
           END-IF
           .
       141000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO SGTSL092-IN-CTNC-PSQ

           PERFORM 970101-ABRE-CURSOR-PSQ
           PERFORM 970102-LER-CURSOR-PSQ
           PERFORM 970103-FECHA-CURSOR-PSQ

           PERFORM  210000-MONTAR-TABELA-RESPOSTA
                     VARYING SGTSL092-IC-TAB FROM 1 BY 1
                    UNTIL SGTSL092-IC-TAB > SGTSL092-QT-OCR-TAB
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-MONTAR-TABELA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           EVALUATE  TRUE
            WHEN  SGTSL092-LISTA-ULT-EST
                 MOVE  CD-RGR-VLDC (SGTSL092-IC-TAB)           TO
                           SGTSL092-CD-RGR-VLDC(SGTSL092-IC-TAB)
                 MOVE  NR-VRS-RGR-VLDC (SGTSL092-IC-TAB)       TO
                           SGTSL092-NR-VRS-RGR-VLDC(SGTSL092-IC-TAB)
                 MOVE  TS-ESTADO (SGTSL092-IC-TAB)             TO
                           SGTSL092-TS-EST-RGR-VLDC(SGTSL092-IC-TAB)
                 MOVE  CD-TIP-EST-DCS (SGTSL092-IC-TAB)        TO
                           SGTSL092-CD-TIP-EST-DCS(SGTSL092-IC-TAB)
                 MOVE  CD-USU-RSP-ATL-EST (SGTSL092-IC-TAB)    TO
                           SGTSL092-CD-USU-RSP-ATL-EST(SGTSL092-IC-TAB)
                 MOVE  NM-RGR-VLDC-CRCT (SGTSL092-IC-TAB)      TO
                           SGTSL092-NM-RGR-VLDC-CRCT(SGTSL092-IC-TAB)
                 MOVE  NM-RTL-RGR-VLDC (SGTSL092-IC-TAB)       TO
                           SGTSL092-NM-RTL-RGR-VLDC(SGTSL092-IC-TAB)
                 MOVE  IN-RGR-PPL (SGTSL092-IC-TAB)            TO
                           SGTSL092-IN-RGR-PPL(SGTSL092-IC-TAB)
                 MOVE  IN-RGR-VLDC-ATV (SGTSL092-IC-TAB)       TO
                           SGTSL092-IN-RGR-VLDC-ATV(SGTSL092-IC-TAB)
                 MOVE  NR-NOTA-TCN-OPR (SGTSL092-IC-TAB)       TO
                          SGTSL092-NR-NOTA-TCN-OPR(SGTSL092-IC-TAB)
                 MOVE  CD-CTRA-TIT-VL (SGTSL092-IC-TAB)        TO
                          SGTSL092-CD-CTRA-TIT-VL(SGTSL092-IC-TAB)

           WHEN  SGTSL092-LISTA-POR-PERIODO
                 MOVE  CD-RGR-VLDC (SGTSL092-IC-TAB)           TO
                           SGTSL092-CD-RGR-VLDC(SGTSL092-IC-TAB)
                 MOVE  NR-VRS-RGR-VLDC (SGTSL092-IC-TAB)       TO
                           SGTSL092-NR-VRS-RGR-VLDC(SGTSL092-IC-TAB)
                 MOVE  TS-EST-RGR-VLDC(SGTSL092-IC-TAB)        TO
                           SGTSL092-TS-EST-RGR-VLDC(SGTSL092-IC-TAB)
                 MOVE  CD-TIP-EST-DCS(SGTSL092-IC-TAB)         TO
                           SGTSL092-CD-TIP-EST-DCS(SGTSL092-IC-TAB)
                 MOVE  NM-RGR-VLDC-CRCT (SGTSL092-IC-TAB)      TO
                           SGTSL092-NM-RGR-VLDC-CRCT(SGTSL092-IC-TAB)
                 MOVE  NM-RTL-RGR-VLDC (SGTSL092-IC-TAB)       TO
                           SGTSL092-NM-RTL-RGR-VLDC(SGTSL092-IC-TAB)
                 MOVE  IN-RGR-PPL (SGTSL092-IC-TAB)            TO
                           SGTSL092-IN-RGR-PPL(SGTSL092-IC-TAB)
                 MOVE  IN-RGR-VLDC-ATV (SGTSL092-IC-TAB)       TO
                           SGTSL092-IN-RGR-VLDC-ATV(SGTSL092-IC-TAB)
                 MOVE  NR-NOTA-TCN-OPR (SGTSL092-IC-TAB)       TO
                           SGTSL092-NR-NOTA-TCN-OPR(SGTSL092-IC-TAB)

           WHEN  SGTSL092-LISTA-ANTES-DATA
                 MOVE  CD-RGR-VLDC (SGTSL092-IC-TAB)           TO
                           SGTSL092-CD-RGR-VLDC(SGTSL092-IC-TAB)
                 MOVE  NR-VRS-RGR-VLDC(SGTSL092-IC-TAB)        TO
                           SGTSL092-NR-VRS-RGR-VLDC(SGTSL092-IC-TAB)
                 MOVE  TS-ESTADO(SGTSL092-IC-TAB)              TO
                           SGTSL092-TS-EST-RGR-VLDC(SGTSL092-IC-TAB)
                 MOVE  CD-TIP-EST-DCS(SGTSL092-IC-TAB)         TO
                           SGTSL092-CD-TIP-EST-DCS(SGTSL092-IC-TAB)
                 MOVE  NM-RGR-VLDC-CRCT (SGTSL092-IC-TAB)      TO
                           SGTSL092-NM-RGR-VLDC-CRCT(SGTSL092-IC-TAB)
                 MOVE  NM-RTL-RGR-VLDC (SGTSL092-IC-TAB)       TO
                           SGTSL092-NM-RTL-RGR-VLDC(SGTSL092-IC-TAB)
                 MOVE  IN-RGR-PPL (SGTSL092-IC-TAB)            TO
                           SGTSL092-IN-RGR-PPL(SGTSL092-IC-TAB)
                 MOVE  IN-RGR-VLDC-ATV (SGTSL092-IC-TAB)       TO
                           SGTSL092-IN-RGR-VLDC-ATV(SGTSL092-IC-TAB)
                 MOVE  NR-NOTA-TCN-OPR (SGTSL092-IC-TAB)       TO
                           SGTSL092-NR-NOTA-TCN-OPR(SGTSL092-IC-TAB)

           WHEN  SGTSL092-LISTA-SEM-MDLD
           WHEN  SGTSL092-LISTA-VERSAO
                 MOVE  CD-RGR-VLDC (SGTSL092-IC-TAB)           TO
                           SGTSL092-CD-RGR-VLDC(SGTSL092-IC-TAB)
                 MOVE  NR-VRS-RGR-VLDC (SGTSL092-IC-TAB)       TO
                           SGTSL092-NR-VRS-RGR-VLDC(SGTSL092-IC-TAB)
                 MOVE  TS-ESTADO (SGTSL092-IC-TAB)             TO
                           SGTSL092-TS-EST-RGR-VLDC(SGTSL092-IC-TAB)
                 MOVE  CD-TIP-EST-DCS (SGTSL092-IC-TAB)        TO
                           SGTSL092-CD-TIP-EST-DCS(SGTSL092-IC-TAB)
                 MOVE  CD-USU-RSP-ATL-EST (SGTSL092-IC-TAB)    TO
                           SGTSL092-CD-USU-RSP-ATL-EST(SGTSL092-IC-TAB)
                 MOVE  NM-RGR-VLDC-CRCT (SGTSL092-IC-TAB)      TO
                           SGTSL092-NM-RGR-VLDC-CRCT(SGTSL092-IC-TAB)
                 MOVE  NM-RTL-RGR-VLDC (SGTSL092-IC-TAB)       TO
                           SGTSL092-NM-RTL-RGR-VLDC(SGTSL092-IC-TAB)
                 MOVE  IN-RGR-PPL (SGTSL092-IC-TAB)            TO
                           SGTSL092-IN-RGR-PPL(SGTSL092-IC-TAB)
                 MOVE  IN-RGR-VLDC-ATV (SGTSL092-IC-TAB)       TO
                           SGTSL092-IN-RGR-VLDC-ATV(SGTSL092-IC-TAB)
                 MOVE  NR-NOTA-TCN-OPR (SGTSL092-IC-TAB)       TO
                          SGTSL092-NR-NOTA-TCN-OPR(SGTSL092-IC-TAB)
           END-EVALUATE
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
              CONTAINER ( 'SGTSL092-RPST'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL092-RPST )
              FROM      ( SGTSL092-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           EVALUATE  TRUE
               WHEN  SGTSL092-LISTA-ULT-EST
                     EXEC SQL
                          OPEN CURSOR-PSQ-01
                     END-EXEC

               WHEN  SGTSL092-LISTA-POR-PERIODO
                     EXEC SQL
                          OPEN CURSOR-PSQ-02
                     END-EXEC

               WHEN  SGTSL092-LISTA-ANTES-DATA
                     EXEC SQL
                          OPEN CURSOR-PSQ-03
                     END-EXEC

               WHEN  SGTSL092-LISTA-SEM-MDLD
                     EXEC SQL
                          OPEN CURSOR-PSQ-04
                     END-EXEC

               WHEN  SGTSL092-LISTA-VERSAO
                     EXEC SQL
                          OPEN CURSOR-PSQ-05
                     END-EXEC
           END-EVALUATE

           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EVALUATE  TRUE

               WHEN  SGTSL092-LISTA-ULT-EST
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-01
                                FOR 101 ROWS
                           INTO
                                :CD-RGR-VLDC
                              , :NR-VRS-RGR-VLDC
                              , :TS-ESTADO
                              , :CD-TIP-EST-DCS
                              , :CD-USU-RSP-ATL-EST
                              , :NM-RGR-VLDC-CRCT
                              , :NM-RTL-RGR-VLDC
                              , :IN-RGR-PPL
                              , :IN-RGR-VLDC-ATV
                              , :NR-NOTA-TCN-OPR
                                :NR-NOTA-TCN-OPR-N
                              , :CD-CTRA-TIT-VL
                     END-EXEC

               WHEN  SGTSL092-LISTA-POR-PERIODO
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-02
                                FOR 101 ROWS
                           INTO
                                :CD-RGR-VLDC
                              , :NR-VRS-RGR-VLDC
                              , :TS-EST-RGR-VLDC
                              , :CD-TIP-EST-DCS
                              , :NM-RGR-VLDC-CRCT
                              , :NM-RTL-RGR-VLDC
                              , :IN-RGR-PPL
                              , :IN-RGR-VLDC-ATV
                              , :NR-NOTA-TCN-OPR
                                :NR-NOTA-TCN-OPR-N
                     END-EXEC

               WHEN  SGTSL092-LISTA-ANTES-DATA
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-03
                                FOR 101 ROWS
                           INTO
                                :CD-RGR-VLDC
                              , :NR-VRS-RGR-VLDC
                              , :TS-ESTADO
                              , :CD-TIP-EST-DCS
                              , :NM-RGR-VLDC-CRCT
                              , :NM-RTL-RGR-VLDC
                              , :IN-RGR-PPL
                              , :IN-RGR-VLDC-ATV
                              , :NR-NOTA-TCN-OPR
                                :NR-NOTA-TCN-OPR-N
                     END-EXEC

               WHEN  SGTSL092-LISTA-SEM-MDLD
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-04
                                FOR 101 ROWS
                           INTO
                                :CD-RGR-VLDC
                              , :NR-VRS-RGR-VLDC
                              , :TS-ESTADO
                              , :CD-TIP-EST-DCS
                              , :CD-USU-RSP-ATL-EST
                              , :NM-RGR-VLDC-CRCT
                              , :NM-RTL-RGR-VLDC
                              , :IN-RGR-PPL
                              , :IN-RGR-VLDC-ATV
                              , :NR-NOTA-TCN-OPR
                                :NR-NOTA-TCN-OPR-N
                     END-EXEC

               WHEN  SGTSL092-LISTA-VERSAO
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-05
                                FOR 101 ROWS
                           INTO
                                :CD-RGR-VLDC
                              , :NR-VRS-RGR-VLDC
                              , :TS-ESTADO
                              , :CD-TIP-EST-DCS
                              , :CD-USU-RSP-ATL-EST
                              , :NM-RGR-VLDC-CRCT
                              , :NM-RTL-RGR-VLDC
                              , :IN-RGR-PPL
                              , :IN-RGR-VLDC-ATV
                     END-EXEC

           END-EVALUATE

           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL092-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL092-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO

           EVALUATE  TRUE
               WHEN  SGTSL092-LISTA-ULT-EST
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-01
                     END-EXEC

               WHEN  SGTSL092-LISTA-POR-PERIODO
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-02
                     END-EXEC

               WHEN  SGTSL092-LISTA-ANTES-DATA
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-03
                     END-EXEC

               WHEN  SGTSL092-LISTA-SEM-MDLD
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-04
                     END-EXEC

               WHEN  SGTSL092-LISTA-VERSAO
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-05
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

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                        TO MSG1-TX-ERRO-TS
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

           MOVE 'SGTSL092-CD-TIP-SRVC'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL092-CD-TIP-SRVC          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL092-CD-RGR-VLDC-INI'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL092-CD-RGR-VLDC-INI      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL092-CD-RGR-VLDC-FIM'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL092-CD-RGR-VLDC-FIM      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL092-CD-TIP-EST-DCS-INI' TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL092-CD-TIP-EST-DCS-INI   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL092-CD-TIP-EST-DCS-FIN' TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL092-CD-TIP-EST-DCS-FIN   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL092-DT-PSQ-INI'         TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL092-DT-PSQ-INI           TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL092-DT-PSQ-FIN'         TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL092-DT-PSQ-FIN           TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL092-DT-PSQ'             TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL092-DT-PSQ               TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL092-ULT-TS-EST'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL092-ULT-TS-EST          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL092-NM-RGR-VLDC-CRCT-E' TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL092-NM-RGR-VLDC-CRCT-E   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL092-IN-RGR-PPL-E'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL092-IN-RGR-PPL-E         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL092-NR-NOTA-TCN-OPR-E'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL092-NR-NOTA-TCN-OPR-E    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL092-CD-PRD-INI'         TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL092-CD-PRD-INI           TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL092-CD-PRD-FIN'         TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL092-CD-PRD-FIN           TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL092-CD-MDLD-INI'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL092-CD-MDLD-INI          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL092-CD-MDLD-FIN'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL092-CD-MDLD-FIN          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL092-CD-CTRA-TIT-VL-INI'
                                              TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL092-CD-CTRA-TIT-VL-INI
                                              TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL092-CD-CTRA-TIT-VL-FIM'
                                              TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL092-CD-CTRA-TIT-VL-FIM
                                              TO MSGKW999-VL-VRV-MNT
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
           END-EXEC
           .
       999999-SAI.
           EXIT.