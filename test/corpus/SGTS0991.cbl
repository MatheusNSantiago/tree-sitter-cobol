      *-----------------------------------------------------------------
      * Programa..: SGTS0991
      * Analista..: F4961858 João Pedro Soares Ferreira
      * Autor.....: F4961858 João Pedro Soares Ferreira
      * Data......: 22.08.2024
      * Objetivo..: Retornar categoria TVM a partir de data de compra e
      *             código do título da operação
      * Compilação: Cobol/CICS
      *-----------------------------------------------------------------
      * VRS001 27.01.2025 F4961858 - IMPLANTAÇÃO
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0991.

      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-PROG                    PIC  X(18) VALUE
                                       '*** SGTS0991 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------
      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      *--> Book para captura de erro via MSG1
-INC MSGKWMG1
      * Books da definicao de dados a serem usados na subrotina de erros
-INC HLPKE001
      *--> Book do Container de Erro
-INC IIBKERRO
      *-----------------------------------------------------------------
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTK0991

      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               MOVE  0001              TO  CD-RTN-PGM
               MOVE  'Área com tamanho inválido.'
                                       TO  MSG-ERRO
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      * WHENEVER DB2
-INC HLPKE002
      * WHENEVER CICS
-INC HLPKE003

           MOVE 'SGT'  TO SGTS0991-SG-SIS-MSG
           MOVE ZEROES TO SGTS0991-CD-RTN-PGM

           PERFORM 100000-VALIDAR-PARAMETRO

           PERFORM 200000-BUSCAR-CD-CTGR-TVM

           PERFORM 999999-ENCERRAR
           .
      *----------------------------------------------------------------*
       100000-VALIDAR-PARAMETRO        SECTION.
      *----------------------------------------------------------------*
           IF SGTS0991-CD-TIT-OPR-ETRD EQUAL SPACES
              MOVE 0001                TO CD-RTN-PGM
              MOVE 'Código do título da operação estruturada não informa
      -       'do.' TO  MSG-ERRO

              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF SGTS0991-DT-CPR-OPR EQUAL SPACES
              MOVE 0002                TO CD-RTN-PGM
              MOVE 'Data de compra da operação não informada.'
              TO  MSG-ERRO

              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .

       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-BUSCAR-CD-CTGR-TVM     SECTION.
      *-----------------------------------------------------------------
           EXEC SQL
                 SELECT COALESCE(MAX(A.CD_CTGR_TVM), 0)
                   INTO :SGTS0991-CD-CTGR-TVM
                        FROM DB2SGT.OPR_ETRD_MCP A
                 INNER JOIN
                      (SELECT CD_CTRA_TIT_VL,
                              CD_OPR_ETRD_MCP,
                              MAX(NR_VRS_OPR_ETRD) AS MAIOR_VERSAO,
                              MAX(TS_EST_VRS)      AS MAIOR_TS
                       FROM   DB2SGT.EST_VRS_OPR_ETRD
                       WHERE CD_CTRA_TIT_VL  > 0
                         AND CD_TIP_EST_VRS  = 3
                         AND TS_EST_VRS < '2024-12-31 23:59:59'
                       GROUP BY
                         CD_CTRA_TIT_VL,
                         CD_OPR_ETRD_MCP
                       ORDER BY
                         CD_CTRA_TIT_VL,
                         CD_OPR_ETRD_MCP)    B
                 ON
                       A.CD_CTRA_TIT_VL  = B.CD_CTRA_TIT_VL
                  AND  A.CD_OPR_ETRD_MCP = B.CD_OPR_ETRD_MCP
                  AND  A.NR_VRS_OPR_ETRD = MAIOR_VERSAO
                 WHERE A.CD_TIT_OPR_ETRD = :SGTS0991-CD-TIT-OPR-ETRD
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                 MOVE 0 TO CD-RTN-PGM SGTS0991-CD-RTN-PGM

                 IF SGTS0991-CD-CTGR-TVM EQUAL 21
                    MOVE 1 TO SGTS0991-CD-CTGR-TVM
                 END-IF

                 IF SGTS0991-CD-CTGR-TVM EQUAL 22
                    MOVE 2 TO SGTS0991-CD-CTGR-TVM
                 END-IF

                 IF SGTS0991-CD-CTGR-TVM EQUAL 23
                    MOVE 3 TO SGTS0991-CD-CTGR-TVM
                 END-IF

                IF SGTS0991-CD-CTGR-TVM EQUAL 0
                    MOVE 100 TO CD-RTN-PGM SGTS0991-CD-RTN-PGM
                    MOVE 0 TO SGTS0991-CD-CTGR-TVM

                    STRING  'Não existem registros ativos desse papel/op
      -             'eração no SGT.' DELIMITED BY SIZE
                                             INTO  MSG-ERRO
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
                 END-IF

                 CONTINUE
               WHEN +100
                 MOVE  SQLCODE            TO  CD-RTN-PGM
                 STRING  'Não existem registros ativos desse papel/opera
      -          'ção no SGT.' DELIMITED BY SIZE
                                          INTO  MSG-ERRO
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               WHEN OTHER
                 SET SGTS0991-ERRO-DE-DB2 TO TRUE

                 MOVE  SQLCODE            TO  CD-RTN-PGM
                 MOVE  SQLERRM            TO  MSG-ERRO

                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .

       200999-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA                     SECTION.
      *-----------------------------------------------------------------
           MOVE CD-RTN-PGM       TO SGTS0991-CD-RTN-PGM

           IF SGTS0991-MSG-ERRO EQUAL SPACES
              MOVE MSG-ERRO      TO SGTS0991-MSG-ERRO
           END-IF

           PERFORM 999001-ERRO-001
           .
       999000-SAI.
           EXIT.
      *--------------------
      * 999000-ERRO SECTION
      *--------------------
      *-----------------
      * 999001-ERRO-001.
      *-----------------
-INC HLPKE004
-INC HLPKE012

           GOBACK
           .
      *-----------------
      * 999002-ERRO-DB2
      *-----------------
-INC HLPKE005
      *-----------------
      * 999003-TRATA-ABEND-CICS
      * 999004-TRATA-CONDITION-CICS
      *-----------------
-INC HLPKE006
      *----------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
      *-INC SGTKP999
      * ---------------------------------------
      * MSG1-NTF-MSG                   SECTION.
      * ---------------------------------------
-INC MSGKPMG1
      * ---------------------------------------
       999999-ENCERRAR                 SECTION.
      * ---------------------------------------

           GOBACK.
      *------------------------- Fim SGTS0991 -------------------------*
