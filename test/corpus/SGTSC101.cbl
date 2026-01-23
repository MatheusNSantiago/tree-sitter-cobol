      CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * Programa..: SGTSC101.
      * Analista..: F2020988 Christina Celeste Guimarães
      * Autor.....: F2020988 Christina Celeste Guimarães
      * Data......: 17/09/2018
      * Objetivo..: Gerar relatorio sobre Projecao da Reserva Bancaria
      *             Consulta em Lista nas tabelas
      *             DB2SGT.ATI_FNCO_ANL_RSCO e DB2SGT.REG_EST_DRIA_ATI
      * Compilação: COBOL CICS
      *-----------------------------------------------------------------
      * VRS001 17/09/2018 - F2020988 - DE 2018/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSC101.

      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------

       01  CTE-PROG                    PIC  X(16)
                                       VALUE '*** SGTSC101 ***'.
       01  CTE-VERS                    PIC  X(06)
                                       VALUE 'VRS001'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  IIBP2000                PIC  X(08) VALUE 'IIBP2000'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------

      *--> Área de Trabalho
       01  I                           PIC  9(04)  VALUE ZEROS.
       01  U                           PIC  9(04)  VALUE ZEROS.
       01  IDX                         PIC  9(04)  VALUE ZEROS.
       01  IND                         PIC  9(04)  VALUE ZEROS.
       01  CNT-DET                     PIC  9(04)  VALUE ZEROS.
       01  CNT-USU                     PIC  9(04)  VALUE ZEROS.
       01  USU                         PIC  9(04)  VALUE ZEROS.
       01  GDA-NM-USU-INCL-CCL         PIC  X(50)  VALUE SPACES.
       01  GDA-NM-USU-CNFC-CCL         PIC  X(50)  VALUE SPACES.
       01  IND-FIM-CPR1                PIC  X(01)  VALUE 'N'.
       01  IND-FIM-CPR2                PIC  X(01)  VALUE 'N'.
       01  GDA-NM-ATI-FNCO-I           PIC  X(20)  VALUE SPACES.
       01  GDA-NM-ATI-FNCO-F           PIC  X(20)  VALUE
                                       '99999999999999999999'.
       01  GDA-CD-ISIN-ATI-I           PIC  X(20)  VALUE SPACES.
       01  GDA-CD-ISIN-ATI-F           PIC  X(20)  VALUE
                                       '99999999999999999999'.

      *--> Area DCLGEN
-INC SGTK099T
-INC SGTK101T

      *--> Book Usado na Subrotina de Ocorrências de Erro
-INC HLPKE001

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
       LINKAGE                         SECTION.
      *-----------------------------------------------------------------
       01 DFHCOMMAREA.
-INC SGTKC101

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

           IF  EIBCALEN  NOT EQUAL LENGTH OF  DFHCOMMAREA
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0001              TO  PSC-ERRO-PGM
               MOVE  0220              TO  CD-RTN-PGM
               MOVE  'Área com tamanho inválido - verifique book/local.'
                                       TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           MOVE  0001                  TO  PSC-ERRO-PGM

           IF  NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
               CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
-INC ALMKCBL2

      *--> Whenever CICS
-INC HLPKE003


           PERFORM 100000-VALIDAR-PARAMETRO

           PERFORM 200000-PROCESSAR-NEGOCIO

           PERFORM  999999-ENCERRAR

           GOBACK

           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  PSC-ERRO-PGM

           IF  SGTSC101-CD-CTRA-SIS-DRIV   NEGATIVE
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0001              TO  CD-RTN-PGM
               MOVE  'Código da carteira sistema drive inválido.'
                                       TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           IF  SGTSC101-CD-CTRA-SIS-DRIV   EQUAL  ZEROS
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0002              TO  CD-RTN-PGM
               MOVE  'Código da carteira sistema drive não informado.'
                                       TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF


           MOVE  ZEROS                 TO  I

           INITIALIZE                      SGTSC101-RTN-PGM
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR-NEGOCIO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  PSC-ERRO-PGM

           MOVE  'N'                   TO  SGTSC101-IN-CTND

           PERFORM  202000-BUSCA-POSICAO

           MOVE  CNT-DET               TO  SGTSC101-QT-REG-DET
           MOVE  IND                   TO  SGTSC101-NR-PSQ-FIM

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       202000-BUSCA-POSICAO            SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  PSC-ERRO-PGM

           MOVE  ZEROS                 TO  SQLCODE

           EXEC SQL
              DECLARE CURSOR-PR1 CURSOR FOR
               SELECT A.DT_EST_ATI
                    , B.NM_ATI_FNCO_ANL
                    , B.NR_SEQL_AQS_ATI
                    , A.NR_SEQL_REG_EST
                    , B.NR_SEQL_ATI
                    , A.CD_ISIN_ATI
                    , A.QT_PAPL_DT_PSC
                    , A.VL_PU_MC_DT_PSC
                    , A.VL_CTB_APL_MOEN
               FROM   DB2SGT.REG_EST_DRIA_ATI A
                   ,  DB2SGT.ATI_FNCO_ANL_RSCO B
               WHERE A.CD_CTRA_SIS_DRIV  = :SGTSC101-CD-CTRA-SIS-DRIV
                 AND B.NM_ATI_FNCO_ANL   BETWEEN :GDA-NM-ATI-FNCO-I
                 AND                             :GDA-NM-ATI-FNCO-F
                 AND A.CD_ISIN_ATI       BETWEEN :GDA-CD-ISIN-ATI-I
                 AND                             :GDA-CD-ISIN-ATI-F
                 AND A.CD_CTRA_SIS_DRIV  = B.CD_CTRA_SIS_DRIV
                 AND A.NR_SEQL_ATI       = B.NR_SEQL_ATI
                 AND A.CD_EST_REG_ATI    <> 9

                 AND A.DT_EST_ATI =
                  (
                 SELECT MAX( A.DT_EST_ATI)
                 FROM DB2SGT.REG_EST_DRIA_ATI A
                 WHERE  A.CD_CTRA_SIS_DRIV = :SGTSC101-CD-CTRA-SIS-DRIV
                  )
                 AND B.NR_SEQL_ATI > 0
                 ORDER BY B.NM_ATI_FNCO_ANL
                 ,        A.CD_ISIN_ATI
           END-EXEC

           IF SGTSC101-NM-ATI-FNCO-ANL-E NOT EQUAL SPACES
              MOVE SGTSC101-NM-ATI-FNCO-ANL-E TO GDA-NM-ATI-FNCO-I
              MOVE SGTSC101-NM-ATI-FNCO-ANL-E TO GDA-NM-ATI-FNCO-F
           END-IF

           IF SGTSC101-CD-ISIN-ATI-E     NOT EQUAL SPACES
              MOVE SGTSC101-CD-ISIN-ATI-E     TO GDA-CD-ISIN-ATI-I
              MOVE SGTSC101-CD-ISIN-ATI-E     TO GDA-CD-ISIN-ATI-F
           END-IF

           EXEC SQL
              OPEN CURSOR-PR1
           END-EXEC

           IF  SQLCODE                     NOT EQUAL ZEROS
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0003              TO  CD-RTN-PGM
               MOVE  'Erro ao iniciar leitura dos ativos.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           MOVE 'N'                     TO IND-FIM-CPR1

           PERFORM  210000-FETCH-CURSOR-PR1
                    UNTIL IND-FIM-CPR1     EQUAL  'S'

           EXEC SQL
              CLOSE CURSOR-PR1
           END-EXEC

           IF  SQLCODE                     NOT EQUAL ZEROS
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0004              TO  CD-RTN-PGM
               MOVE  'Erro ao finalizar leitura dos ativos.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           .
       200009-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-FETCH-CURSOR-PR1         SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO PSC-ERRO-PGM

           EXEC SQL
              FETCH CURSOR-PR1
               INTO :101T-DT-EST-ATI
                  , :099T-NM-ATI-FNCO-ANL
                  , :099T-NR-SEQL-AQS-ATI
                  , :101T-NR-SEQL-REG-EST
                  , :099T-NR-SEQL-ATI
                  , :101T-CD-ISIN-ATI
                  , :101T-QT-PAPL-DT-PSC
                  , :101T-VL-PU-MC-DT-PSC
                  , :101T-VL-SDO-TTL-ATZD
           END-EXEC

           EVALUATE  SQLCODE
               WHEN  ZEROS
                     ADD   0001        TO  IND
                     IF  SGTSC101-NR-PSQ-INC LESS IND
                         IF  I             >= 80
                             SUBTRACT 0001
                                      FROM IND
                             MOVE 'S'  TO  IND-FIM-CPR1
                             MOVE 'S'  TO  SGTSC101-IN-CTND
                         ELSE
                             PERFORM 211000-MONTAR-SAIDA
                         END-IF
                     END-IF
               WHEN  100
                     MOVE  'S'         TO  IND-FIM-CPR1
               WHEN  OTHER
                     SET   ERRO-CODIFICADO
                                       TO  TRUE
                     MOVE  0005        TO  CD-RTN-PGM
                     MOVE  'Erro ao consultar ativos.'
                                       TO MSG-ERRO
                     PERFORM 999001-ERRO-001
           END-EVALUATE

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       211000-MONTAR-SAIDA             SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  PSC-ERRO-PGM

           ADD   0001                  TO  CNT-DET
           ADD   0001                  TO  I

           MOVE  101T-DT-EST-ATI       TO  SGTSC101-DT-EST-ATI       (I)
           MOVE  099T-NM-ATI-FNCO-ANL  TO  SGTSC101-NM-ATI-FNCO-ANL  (I)
           MOVE  099T-NR-SEQL-AQS-ATI  TO  SGTSC101-NR-SEQL-AQS-ATI  (I)
           MOVE  101T-NR-SEQL-REG-EST  TO  SGTSC101-NR-SEQL-REG-EST  (I)
           MOVE  099T-NR-SEQL-ATI      TO  SGTSC101-NR-SEQL-ATI      (I)
           MOVE  101T-CD-ISIN-ATI      TO  SGTSC101-CD-ISIN-ATI      (I)
           MOVE  101T-QT-PAPL-DT-PSC   TO  SGTSC101-QT-PAPL-DT-PSC   (I)
           MOVE  101T-VL-PU-MC-DT-PSC  TO  SGTSC101-VL-PU-MC-DT-PSC  (I)
           MOVE  101T-VL-SDO-TTL-ATZD  TO  SGTSC101-VL-CTB-APL-MOEN  (I)

           .
       211000-SAI.
           EXIT.



      *-----------------------------------------------------------------
      * 999000-ERRO                    SECTION
      *-----------------------------------------------------------------
      *-----------------
      * 999001-ERRO-001.
      *-----------------
-INC HLPKE004
           MOVE RTN-PGM                TO SGTSC101-RTN-PGM

           PERFORM 999999-ENCERRAR
           .
       999001-ERRO.
           EXIT.

      *----------------------------
      * 999003-TRATA-ABEND-CICS
      * 999004-TRATA-CONDITION-CICS
      *----------------------------
-INC HLPKE006

      *-----------------------------------------------------------------
       999999-ENCERRAR                 SECTION.
      *--> Retorna para programa chamador
      *-----------------------------------------------------------------

           GOBACK

           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTSC101 -----------------------------