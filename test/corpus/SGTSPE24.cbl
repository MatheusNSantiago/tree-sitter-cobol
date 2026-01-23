      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSPE24
      * ANALISTA..: F9158937 - Silvia Aguiar de Oliveira
      * AUTOR.....: F9158937 - Silvia Aguiar de Oliveira
      * OBJETIVO..: Consultar estado e versão de operação ativa numa
      *             determinada data
      * COMPILACAO: COBOL/SQL - BATCH / ON-LINE
      *-----------------------------------------------------------------
      * VRS001 11/07/2019 - F9158937  - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSPE24.

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
       01  CTE-INICIO-SS               PIC  X(35) VALUE
                                 ' *** SGTSPE24 S.S. COMECA AQUI *** '.

       01  CTE-PROG                    PIC  X(17) VALUE
                                       '*** SGTSPE24 *** '.
       01  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

       77  SBVERSAO                    PIC  X(8)   VALUE 'SBVERSAO'.


       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
       01  CD-MTV-ALT-N                PIC S9(004) COMP-5.
       01  DT-INC-VGC-VRS-OPR-N        PIC S9(004) COMP-5.

       01  TS-EST-OPR.
           03 DT-EST-OPR               PIC X(10).
           03 DT-EST-OPR-R REDEFINES   DT-EST-OPR.
              05 ANO                   PIC  9(4).
              05 FILLER                PIC  X(1).
              05 MES                   PIC  9(2).
              05 FILLER                PIC  X(1).
              05 DIA                   PIC  9(2).
           03 HR-EST-OPR               PIC X(16).

       01  DT-REF                      PIC 9(08).
       01  DT-REF-R    REDEFINES       DT-REF.
           03  ANO                     PIC  9(4).
           03  MES                     PIC  9(2).
           03  DIA                     PIC  9(2).

       01  DT-EST                      PIC 9(08).
       01  DT-EST-R    REDEFINES       DT-EST.
           03  ANO                     PIC  9(4).
           03  MES                     PIC  9(2).
           03  DIA                     PIC  9(2).

      *--> Book definição dados a serem usados na subrotina de erros
-INC HLPKE001

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B

      * Area de trabalho

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       77  CTE-FINAL-SS                PIC X(40)      VALUE
               '  *** STORAGE SECTION TERMINA AQUI ***  '.
      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTKPE24

      *-----------------------------------------------------------------
       PROCEDURE DIVISION  USING DFHCOMMAREA.
      *-----------------------------------------------------------------
       000000-PRINCIPAL                SECTION.


      * TESTE OBRIGATORIO DO TAMANHO DA LINKAGE NAO RETIRAR
           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
              MOVE 0001               TO PSC-ERRO-PGM
              MOVE  0220              TO CD-RTN-PGM
              STRING 'Area com tamanho inválido. ' DELIMITED BY SIZE
                     INTO MSG-ERRO
              END-STRING
              PERFORM 999001-ERRO-001
           ELSE
              MOVE 0001               TO PSC-ERRO-PGM
           END-IF

           IF  NOT EXECUCAO-ONLINE     AND NOT EXECUCAO-BATCH
              CALL  CICS5000 USING CICS5000-COMMAREA
           END-IF

           INITIALIZE SQLCA
           INITIALIZE SGTSPE24-SAIDA

      * WHENEVER DB2
-INC HLPKE002
001530* WHENEVER CICS
-INC HLPKE003

           PERFORM 140000-VALIDAR-PARAMETRO

           PERFORM 200000-PROCESSA-PESQUISA

           MOVE RTN-PGM                      TO SGTSPE24-RTN-PGM

           GOBACK
           .

       000000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO PSC-ERRO-PGM

           IF  SGTSPE24-E-CD-CTRA-TIT-VL <= ZEROS
               MOVE  0001              TO  CD-RTN-PGM
               MOVE 'Código da carteira de título e valores mobiliários
      -        'inválido.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           IF  SGTSPE24-E-CD-TIT-OPR-ETRD   EQUAL SPACES
               MOVE  0231              TO  CD-RTN-PGM
               MOVE 'Código do título da operação estruturada não inform
      -        'ado.'                  TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           IF  SGTSPE24-E-NR-DVS-TIT  <= ZEROS
               MOVE  0232              TO  CD-RTN-PGM
               MOVE 'Número da divisão do título da operação não informa
      -        'do.'                   TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           IF  SGTSPE24-E-DT-REF     EQUAL ZEROS
              MOVE  0049             TO  CD-RTN-PGM
              MOVE 'Data informada inválida.'
                                     TO  MSG-ERRO
              PERFORM 999001-ERRO-001
           END-IF
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA        SECTION.
      *-----------------------------------------------------------------
      * Seleciona  Operações Aprovadas em uma determinada data a partir
      * de um título
           MOVE 0003                   TO PSC-ERRO-PGM

           INITIALIZE SGTSPE24-SAIDA

           EXEC SQL
               SELECT A.CD_CTRA_TIT_VL,
                      A.CD_OPR_ETRD_MCP,
                      A.NR_VRS_OPR_ETRD,
                      A.TS_EST_OPR_ETRD,
                      A.CD_TIP_EST_OPR,
                      A.CD_MTV_ALT,
                      IFNULL(C.DT_INC_VGC_VRS_OPR, '01.01.0001')
                                      AS X_DT_INI_VGC_VRS,
                      B.CD_TIP_EST_VRS
               INTO
                      :SGTSPE24-CD-CTRA-TIT-VL     ,
                      :SGTSPE24-CD-OPR-ETRD-MCP    ,
                      :SGTSPE24-NR-VRS-OPR-ETRD    ,
                      :SGTSPE24-TS-EST-OPR-ETRD    ,
                      :SGTSPE24-CD-TIP-EST-OPR     ,
                      :SGTSPE24-CD-MTV-ALT
                      :CD-MTV-ALT-N                ,
                      :SGTSPE24-DT-INC-VGC-VRS-OPR
                      :DT-INC-VGC-VRS-OPR-N        ,
                      :SGTSPE24-CD-TIP-EST-VRS
               FROM DB2SGT.EST_OPR_ETRD  A
               INNER JOIN DB2SGT.EST_VRS_OPR_ETRD  B
                     ON  B.CD_CTRA_TIT_VL   =  A.CD_CTRA_TIT_VL
                     AND B.CD_OPR_ETRD_MCP  =  A.CD_OPR_ETRD_MCP
                     AND B.NR_VRS_OPR_ETRD  =  A.NR_VRS_OPR_ETRD
               INNER JOIN  DB2SGT.OPR_ETRD_MCP  C
                     ON  C.CD_CTRA_TIT_VL   =  A.CD_CTRA_TIT_VL
                     AND C.CD_OPR_ETRD_MCP  =  A.CD_OPR_ETRD_MCP
                     AND C.NR_VRS_OPR_ETRD  =  A.NR_VRS_OPR_ETRD
               WHERE A.CD_CTRA_TIT_VL    =  :SGTSPE24-E-CD-CTRA-TIT-VL
                 AND C.CD_TIT_OPR_ETRD   =  :SGTSPE24-E-CD-TIT-OPR-ETRD
                 AND C.NR_DVS_TIT        =  :SGTSPE24-E-NR-DVS-TIT
                 AND B.CD_TIP_EST_VRS    =  3
                 AND  (C.DT_INC_VGC_VRS_OPR  <=  :SGTSPE24-E-DT-REF
                  OR   C.DT_INC_VGC_VRS_OPR  IS NULL)
               ORDER BY A.TS_EST_OPR_ETRD DESC
               FETCH FIRST 1 ROW ONLY
           END-EXEC

           EVALUATE SQLCODE
           WHEN ZEROS
               CONTINUE
           WHEN 100
               MOVE  0100              TO  CD-RTN-PGM
               STRING 'Registro inexistente. ' DELIMITED BY SIZE
                      'Titulo = '              DELIMITED BY SIZE
                      SGTSPE24-E-CD-TIT-OPR-ETRD  DELIMITED BY SIZE
                      INTO MSG-ERRO
               END-STRING
           WHEN OTHER
               PERFORM 999002-ERRO-DB2
           END-EVALUATE

           MOVE SGTSPE24-TS-EST-OPR-ETRD TO TS-EST-OPR
           MOVE CORR DT-EST-OPR-R        TO DT-EST-R
           MOVE CORR SGTSPE24-E-DT-REF-R TO DT-REF-R

           EVALUATE SGTSPE24-CD-TIP-EST-OPR
           WHEN  ZEROS
           WHEN  3
           WHEN  4
           WHEN  5
               CONTINUE
           WHEN  6
           WHEN 10
               IF  DT-EST  GREATER  DT-REF
                   PERFORM 210000-PROCESSA-ENCERRADAS
               END-IF
           END-EVALUATE

           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       210000-PROCESSA-ENCERRADAS      SECTION.
      *-----------------------------------------------------------------
      * Se a data de encerramento ou transferência de um título é maior
      * que a data pesquisada faz-se novamente a pesquisa ignorando os
      * estados Encerrada e Transferencia de Carteira.
           MOVE 0004                   TO PSC-ERRO-PGM

           INITIALIZE SGTSPE24-SAIDA

           EXEC SQL
               SELECT A.CD_CTRA_TIT_VL,
                      A.CD_OPR_ETRD_MCP,
                      A.NR_VRS_OPR_ETRD,
                      A.TS_EST_OPR_ETRD,
                      A.CD_TIP_EST_OPR,
                      A.CD_MTV_ALT,
                      IFNULL(C.DT_INC_VGC_VRS_OPR, '01.01.0001')
                                      AS X_DT_INI_VGC_VRS,
                      B.CD_TIP_EST_VRS
               INTO
                      :SGTSPE24-CD-CTRA-TIT-VL     ,
                      :SGTSPE24-CD-OPR-ETRD-MCP    ,
                      :SGTSPE24-NR-VRS-OPR-ETRD    ,
                      :SGTSPE24-TS-EST-OPR-ETRD    ,
                      :SGTSPE24-CD-TIP-EST-OPR     ,
                      :SGTSPE24-CD-MTV-ALT
                      :CD-MTV-ALT-N                ,
                      :SGTSPE24-DT-INC-VGC-VRS-OPR
                      :DT-INC-VGC-VRS-OPR-N        ,
                      :SGTSPE24-CD-TIP-EST-VRS
               FROM DB2SGT.EST_OPR_ETRD  A
               INNER JOIN DB2SGT.EST_VRS_OPR_ETRD  B
                     ON  B.CD_CTRA_TIT_VL   =  A.CD_CTRA_TIT_VL
                     AND B.CD_OPR_ETRD_MCP  =  A.CD_OPR_ETRD_MCP
                     AND B.NR_VRS_OPR_ETRD  =  A.NR_VRS_OPR_ETRD
               INNER JOIN  DB2SGT.OPR_ETRD_MCP  C
                     ON  C.CD_CTRA_TIT_VL   =  A.CD_CTRA_TIT_VL
                     AND C.CD_OPR_ETRD_MCP  =  A.CD_OPR_ETRD_MCP
                     AND C.NR_VRS_OPR_ETRD  =  A.NR_VRS_OPR_ETRD
               WHERE A.CD_CTRA_TIT_VL    =  :SGTSPE24-E-CD-CTRA-TIT-VL
                 AND C.CD_TIT_OPR_ETRD   =  :SGTSPE24-E-CD-TIT-OPR-ETRD
                 AND C.NR_DVS_TIT        =  :SGTSPE24-E-NR-DVS-TIT
                 AND B.CD_TIP_EST_VRS    =  3
                 AND  (C.DT_INC_VGC_VRS_OPR    <=  :SGTSPE24-E-DT-REF
                  OR   C.DT_INC_VGC_VRS_OPR  IS NULL)
                 AND  A.CD_TIP_EST_OPR NOT IN(6, 10)
               ORDER BY A.TS_EST_OPR_ETRD DESC
               FETCH FIRST 1 ROW ONLY
           END-EXEC

           EVALUATE SQLCODE
           WHEN ZEROS
               CONTINUE
           WHEN 100
               MOVE  0100              TO  CD-RTN-PGM
               STRING 'Registro inexistente. ' DELIMITED BY SIZE
                      'Titulo = '              DELIMITED BY SIZE
                      SGTSPE24-E-CD-TIT-OPR-ETRD  DELIMITED BY SIZE
                      INTO MSG-ERRO
               END-STRING
           WHEN OTHER
               PERFORM 999002-ERRO-DB2
           END-EVALUATE

           .
       210000-SAI.
           EXIT.
      *-----------------------------------------------------------------

001530*--------------------
001530* 999001-ERRO-001                    SECTION.
001530*--------------------

-INC HLPKE004
-INC HLPKE012

      * 1 - Erro de db2
           IF TIP-RTN-PGM   EQUAL 1
              IF CD-RTN-PGM NOT EQUAL 100 AND -803
                 MOVE 9997    TO   CD-RTN-PGM
              END-IF
           END-IF

      * 2 - erro cics
           IF TIP-RTN-PGM EQUAL 2
              IF CD-RTN-PGM  EQUAL ZEROS OR 333
                 MOVE 9999    TO   CD-RTN-PGM
              END-IF
           END-IF

001530     MOVE RTN-PGM                      TO SGTSPE24-RTN-PGM
001530     GOBACK
001530     .
      *-----------------
      * 999002-ERRO-DB2 SECTION.
      *-----------------
-INC HLPKE005
      *-----------------
      * 999003-TRATA-ABEND-CICS SECTION.
      * 999004-TRATA-CONDITION-CICS SECTION.
      * 999005-CMPT-ERRO-CICS SECTION.
      *-----------------
-INC HLPKE006

001530 999999-SAI.
001530     EXIT.

      *---------------- FIM SGTSPE24 ----------------------------------*