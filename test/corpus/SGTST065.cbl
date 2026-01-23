       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTST065
      * ANALISTA..: F2054109 - CINTIA AP SOARES IGLESIAS
      * AUTOR.....: F2054109 - CINTIA AP SOARES IGLESIAS
      * DATA......: 05.07.2016
      * OBJETIVO..: PERSISTENCIA DA TABELA IMP_MVTC_FNCR_TIT
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS004 14/11/2024 f4957150 recompilação erro bind
      * VRS003 15/07/2024 C1089208 INCLUSAO DE NOVOS CAMPOS:
      *                            SGTST065-CD-TIP-OGM-INF
      *                            SGTST065-CD-OPR-ITMT-FNCO
      *                            SGTST065-CD-TIP-CTRA-CTDA
      *                            SGTST065-NR-SEQL-PGTO-EVT
      *                            SGTST065-CD-GR-EVT-FDAR
      *                            SGTST065-CD-ITMT-FNCO-FDAR
      *                            SGTST065-CD-CLI-FDAR
      *                            SGTST065-CD-EVT-ITMT-CTIP
      *                            SGTST065-TX-EVT-ITMT-CTIP
      *                            SGTST065-NR-CT-CLI-CMRA
      *                            SGTST065-TX-GR-ITMT-FNCO
      *                            SGTST065-NR-UNCO-CTR-OPR
      *                            SGTST065-VL-PU-OPR-ITMT
      *                            SGTST065-VL-BRTO-OPR
      *                            SGTST065-CD-TIP-LCTO-PSC
      * VRS002 13/02/2019 F2634012 DE 2016/14195 - acerto no tamanho
      *                            do campo HR-IMP-MVTC-FNCR
      * VRS001 05.07.2016 F2054109 DE 2015/137233 - IMPLANTACAO-
      *                           INCLUSãO DE NOVAS FUNçOES
      *        21/02/2017 F8785286 DE 2016/145195 - Inclusão campo
      *                            CD-USU-RSP-ATL para chave log
      *                            (movimentar a partirdo container
      *                             operador).
      *        31/01/2018 F2634012 DE 2016/14195 - Inclusão dos campos
      *                            TX-DCR-TIP-CPTR E CD-TIP-LCTO-CTB E
      *                            CD-CTRA-TIT-VL
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTST065.

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
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTST065 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS004'.

      * Registro da tabela
       01  REG-CACHE.
           03  CHAVE-CACHE.
               05  DT-IMP-MVTC-FNCR          PIC  X(10) VALUE ' '.
               05  DT-IMP-MVTC-FNCR-R REDEFINES
                   DT-IMP-MVTC-FNCR.
                   07 DIA                    PIC  9(2).
                   07 FILLER                 PIC  X(1).
                   07 MES                    PIC  9(2).
                   07 FILLER                 PIC  X(1).
                   07 ANO                    PIC  9(4).
               05  HR-IMP-MVTC-FNCR          PIC  X(008) VALUE ' '.
               05  NR-IMP-MVTC-FNCR          PIC S9(4) COMP-5 VALUE 0.
               05  FILLER                    PIC  X(20) VALUE ' '.
           03  DADOS-CACHE.
               05  CD-TIT-OPR-ETRD           PIC  X(12) VALUE ' '.
               05  NR-DVS-TIT                PIC S9(4) COMP-5 VALUE 0.
               05  DT-EFT-MVTC               PIC  X(10) VALUE ' '.
               05  DT-EFT-MVTC-R REDEFINES
                   DT-EFT-MVTC.
                   07 DIA                    PIC  9(2).
                   07 FILLER                 PIC  X(1).
                   07 MES                    PIC  9(2).
                   07 FILLER                 PIC  X(1).
                   07 ANO                    PIC  9(4).
               05  VL-EFT-MVTC               PIC S9(13)V9(2) COMP-3
                                             VALUE 0.
               05  DT-CLCD-MVTC              PIC  X(10) VALUE ' '.
               05  DT-CLCD-MVTC-R REDEFINES
                   DT-CLCD-MVTC.
                   07 DIA                    PIC  9(2).
                   07 FILLER                 PIC  X(1).
                   07 MES                    PIC  9(2).
                   07 FILLER                 PIC  X(1).
                   07 ANO                    PIC  9(4).
               05  VL-CLCD-MVTC              PIC S9(13)V9(2) COMP-3
                                                               VALUE 0.
               05  CD-TIP-MVTC               PIC S9(4) COMP-5  VALUE 0.
               05  QT-EVT                    PIC S9(15)V9(14) COMP-3
                                                               VALUE 0.
               05  CD-EST-IMP                PIC S9(4) COMP-5  VALUE 0.
               05  TX-OBS-MVTC               PIC  X(40) VALUE ' '.
               05  TX-DCR-ERRO-IMP           PIC  X(200) VALUE ' '.
               05  CD-USU-RSP-ATL            PIC  X(08) VALUE ' '.
               05  TX-DCR-TIP-CPTR           PIC  X(30) VALUE ' ' .
               05  CD-TIP-LCTO-CTB           PIC  9(01) VALUE 0.
               05  CD-CTRA-TIT-VL            PIC S9(09) COMP-5 VALUE 0.
               05  CD-TIP-OGM-INF            PIC S9(04) COMP-5 VALUE 0.
               05  CD-OPR-ITMT-FNCO          PIC S9(09) COMP-5 VALUE 0.
               05  CD-TIP-CTRA-CTDA          PIC S9(04) COMP-5 VALUE 0.
               05  NR-SEQL-PGTO-EVT          PIC S9(04) COMP-5 VALUE 0.
               05  CD-GR-EVT-FDAR            PIC S9(04) COMP-5 VALUE 0.
               05  CD-ITMT-FNCO-FDAR         PIC S9(09) COMP-5 VALUE 0.
               05  CD-CLI-FDAR               PIC S9(09) COMP-5 VALUE 0.
               05  CD-EVT-ITMT-CTIP          PIC S9(09) COMP-5 VALUE 0.
               05  TX-EVT-ITMT-CTIP          PIC X(100) VALUE ' '.
               05  NR-CT-CLI-CMRA            PIC S9(09) COMP-5 VALUE 0.
               05  TX-GR-ITMT-FNCO           PIC X(100) VALUE ' '.
               05  NR-UNCO-CTR-OPR           PIC S9(17)V COMP-3 VALUE 0.
               05  VL-PU-OPR-ITMT            PIC S9(10)V9(8) COMP-3
                                                                VALUE 0.
               05  VL-BRTO-OPR               PIC S9(10)V9(8) COMP-3
                                                                VALUE 0.
               05  CD-TIP-LCTO-PSC           PIC S9(01)V     COMP-3
                                                                VALUE 0.

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * Area de trabalho

       01  QT-REG-SELD                      PIC S9(4) COMP-5 VALUE 0.
       01  DT-SQL-ATU                       PIC  X(10)
                                                 VALUE '  .  .    '.
       01  DT-SQL-ATU-R REDEFINES DT-SQL-ATU.
           03 DIA                           PIC  9(02).
           03 FILLER                        PIC  X(01).
           03 MES                           PIC  9(02).
           03 FILLER                        PIC  X(01).
           03 ANO                           PIC  9(04).

      * Indicadores de nulidades

       01  QT-REG-SELD-NULO                 PIC S9(4) COMP-5 VALUE 0.
       01  CD-TIT-OPR-ETRD-N                PIC S9(4) COMP-5 VALUE 0.
       01  NR-DVS-TIT-N                     PIC S9(4) COMP-5 VALUE 0.
       01  DT-EFT-MVTC-N                    PIC S9(4) COMP-5 VALUE 0.
       01  VL-EFT-MVTC-N                    PIC S9(4) COMP-5 VALUE 0.
       01  DT-CLCD-MVTC-N                   PIC S9(4) COMP-5 VALUE 0.
       01  VL-CLCD-MVTC-N                   PIC S9(4) COMP-5 VALUE 0.
       01  CD-TIP-MVTC-N                    PIC S9(4) COMP-5 VALUE 0.
       01  QT-EVT-N                         PIC S9(4) COMP-5 VALUE 0.
       01  TX-OBS-MVTC-N                    PIC S9(4) COMP-5 VALUE 0.
       01  TX-DCR-ERRO-IMP-N                PIC S9(4) COMP-5 VALUE 0.
       01  TX-DCR-TIP-CPTR-N                PIC S9(4) COMP-5 VALUE 0.
       01  CD-TIP-LCTO-CTB-N                PIC S9(4) COMP-5 VALUE 0.
       01  CD-CTRA-TIT-VL-N                 PIC S9(4) COMP-5 VALUE 0.
       01  CD-TIP-OGM-INF-N                 PIC S9(4) COMP-5 VALUE 0.
       01  CD-OPR-ITMT-FNCO-N               PIC S9(4) COMP-5 VALUE 0.
       01  CD-TIP-CTRA-CTDA-N               PIC S9(4) COMP-5 VALUE 0.
       01  NR-SEQL-PGTO-EVT-N               PIC S9(4) COMP-5 VALUE 0.
       01  CD-GR-EVT-FDAR-N                 PIC S9(4) COMP-5 VALUE 0.
       01  CD-ITMT-FNCO-FDAR-N              PIC S9(4) COMP-5 VALUE 0.
       01  CD-CLI-FDAR-N                    PIC S9(4) COMP-5 VALUE 0.
       01  CD-EVT-ITMT-CTIP-N               PIC S9(4) COMP-5 VALUE 0.
       01  TX-EVT-ITMT-CTIP-N               PIC S9(4) COMP-5 VALUE 0.
       01  NR-CT-CLI-CMRA-N                 PIC S9(4) COMP-5 VALUE 0.
       01  TX-GR-ITMT-FNCO-N                PIC S9(4) COMP-5 VALUE 0.
       01  NR-UNCO-CTR-OPR-N                PIC S9(4) COMP-5 VALUE 0.
       01  VL-PU-OPR-ITMT-N                 PIC S9(4) COMP-5 VALUE 0.
       01  VL-BRTO-OPR-N                    PIC S9(4) COMP-5 VALUE 0.
       01  CD-TIP-LCTO-PSC-N                PIC S9(4) COMP-5 VALUE 0.

       01 AUX-ERRO                       PIC X(30).
       01 AUX-ERRO-R  REDEFINES AUX-ERRO.
          03 AUX-CD-ERRO                  PIC S9(04) COMP.
      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  DT-IMP-MVTC-FNCR-LOG          PIC  X(10) VALUE ' '.
           03  HR-IMP-MVTC-FNCR-LOG          PIC  X(08) VALUE ' '.
           03  NR-IMP-MVTC-FNCR-LOG          PIC  9(5) VALUE 0.

      * Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

      * Subrotinas Utilizadas

       01  SGTS014G VALUE 'SGTS014G'         PIC X(8).
      * Books da definicao de dados a serem usados na subrotina de erros
-INC HLPKE001

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTKT065
      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------

           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               MOVE  0220              TO  CD-RTN-PGM
               MOVE 'Area com tamanho invalido.'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF
           MOVE 0001                   TO PSC-ERRO-PGM
      * WHENEVER DB2
-INC HLPKE002
      * WHENEVER CICS
-INC HLPKE003

           PERFORM 100000-VALIDA-PARAMETRO

           EVALUATE TRUE
              WHEN SGTST065-INCLUIR
                   PERFORM 400000-INCLUIR
              WHEN SGTST065-ALTERAR
                   PERFORM 500000-ALTERAR
              WHEN SGTST065-EXCLUIR
                   PERFORM 600000-EXCLUIR
              WHEN SGTST065-CONSULTAR
                   IF  CHAVE-CACHE = SGTST065-CHAVE
                       MOVE DADOS-CACHE TO SGTST065-AREA-DADOS
                   ELSE
                       PERFORM 700000-CONSULTA
                   END-IF
              WHEN SGTST065-CONSULTAR-ULT-IMP-TIT
                   PERFORM 800000-CONSULTAR-ULT-IMP-TIT
      *        WHEN SGTST065-VERIFICAR-TIT-ERRO
      *           PERFORM 900000-VERIFICAR-TIT-ERRO
           END-EVALUATE

           GOBACK
           .
      *-----------------------------------------------------------------
       100000-VALIDA-PARAMETRO         SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO PSC-ERRO-PGM
           INITIALIZE SGTST065-RTN-PGM

           IF  NOT SGTST065-INCLUIR
           AND NOT SGTST065-EXCLUIR
           AND NOT SGTST065-ALTERAR
           AND NOT SGTST065-CONSULTAR
           AND NOT SGTST065-CONSULTAR-ULT-IMP-TIT
           AND NOT SGTST065-VERIFICAR-TIT-ERRO
               MOVE  0116              TO  CD-RTN-PGM
               MOVE 'Ação inválida.' TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 130000-VALIDA-CAMPOS

           IF  SGTST065-INCLUIR
           OR  SGTST065-ALTERAR
               PERFORM 160000-TRATA-NULIDADE
           END-IF
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       130000-VALIDA-CAMPOS            SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO PSC-ERRO-PGM
           PERFORM 140000-BUSCA-TS-DT
           MOVE CORR HLPKE001-TS-FMTD  TO DT-SQL-ATU-R
           MOVE  SPACES                TO AUX-ERRO

           IF  SGTST065-DT-IMP-MVTC-FNCR EQUAL SPACES
           AND NOT SGTST065-CONSULTAR-ULT-IMP-TIT
               MOVE  0371              TO  CD-RTN-PGM
               MOVE 'Data da importação da movimentação financeira invál
      -        'ida.'                  TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTST065-HR-IMP-MVTC-FNCR EQUAL SPACES
           AND NOT SGTST065-CONSULTAR-ULT-IMP-TIT
               MOVE  0369              TO  CD-RTN-PGM
               MOVE 'Horário da importação da movimentação financeira n
      -        'ão informado.'         TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTST065-CONSULTAR-ULT-IMP-TIT
            OR SGTST065-VERIFICAR-TIT-ERRO
               IF  SGTST065-NR-DVS-TIT  LESS ZEROS
                  MOVE  0369              TO  CD-RTN-PGM
                  MOVE 'Numero divisão do titulo inválido.'
                                          TO MSG-ERRO
                  PERFORM 999001-ERRO-001
               END-IF
           END-IF

           IF  SGTST065-VERIFICAR-TIT-ERRO
               IF  SGTST065-DT-EFT-MVTC  EQUAL SPACES
                  MOVE  0336              TO  CD-RTN-PGM
                  MOVE 'Data efetiva da movimentação não informada.'
                                          TO MSG-ERRO
                  PERFORM 999001-ERRO-001
               END-IF
           END-IF

           IF  SGTST065-NR-IMP-MVTC-FNCR EQUAL ZEROS
           AND NOT SGTST065-INCLUIR
           AND NOT SGTST065-CONSULTAR-ULT-IMP-TIT
           AND NOT SGTST065-VERIFICAR-TIT-ERRO
               MOVE  0370              TO  CD-RTN-PGM
               MOVE 'Número da importação da movimentação financeira não
      -        ' informado.'           TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTST065-INCLUIR
           OR  SGTST065-ALTERAR
               IF  SGTST065-CD-USU-RSP-ATL EQUAL SPACES
                   MOVE  0278              TO  CD-RTN-PGM
                   MOVE 'Código do usuário responsável pela atualização
      -            'não informado.'        TO  MSG-ERRO
                  PERFORM 999001-ERRO-001
               END-IF
           END-IF

           IF  SGTST065-EXCLUIR
               IF  SGTST065-CD-USU-RSP-ATL EQUAL SPACES
                   MOVE  0278              TO  CD-RTN-PGM
                   MOVE 'Código do usuário responsável pela atualização
      -            'não informado.'        TO  MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF

           IF  SGTST065-DT-IMP-MVTC-FNCR EQUAL 'CURRENT'
               MOVE DT-SQL-ATU         TO SGTST065-DT-IMP-MVTC-FNCR
           END-IF

           IF  SGTST065-DT-EFT-MVTC EQUAL 'CURRENT'
               MOVE DT-SQL-ATU         TO SGTST065-DT-EFT-MVTC
           END-IF

           IF  SGTST065-DT-CLCD-MVTC EQUAL 'CURRENT'
               MOVE DT-SQL-ATU         TO SGTST065-DT-CLCD-MVTC
           END-IF
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-BUSCA-TS-DT              SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO PSC-ERRO-PGM
           CALL SBTIMEDT            USING HLPKE001-CD-FUC
                                          HLPKE001-AREA-TRB
                                          HLPKE001-FMT-TS
                                          HLPKE001-TAM-TS
                                          HLPKE001-TS-FMTD
           IF  RETURN-CODE      NOT EQUAL ZEROS
               MOVE  0050              TO  CD-RTN-PGM
               MOVE 'Erro ao obter data e hora atual.' TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           MOVE RETURN-CODE            TO AUX-CD-ERRO

           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       160000-TRATA-NULIDADE           SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO PSC-ERRO-PGM
           IF  SGTST065-CD-TIT-OPR-ETRD EQUAL SPACES
               MOVE -1                 TO CD-TIT-OPR-ETRD-N
           END-IF

           IF  SGTST065-NR-DVS-TIT EQUAL ZEROS
               MOVE -1                 TO NR-DVS-TIT-N
           END-IF

           IF  DIA OF SGTST065-DT-EFT-MVTC-R EQUAL SPACES
           OR  MES OF SGTST065-DT-EFT-MVTC-R EQUAL SPACES
           OR  ANO OF SGTST065-DT-EFT-MVTC-R EQUAL SPACES
               MOVE -1                 TO DT-EFT-MVTC-N
           END-IF

           IF  SGTST065-VL-EFT-MVTC EQUAL ZEROS
               MOVE -1                 TO VL-EFT-MVTC-N
           END-IF

           IF  DIA OF SGTST065-DT-CLCD-MVTC-R EQUAL SPACES
           OR  MES OF SGTST065-DT-CLCD-MVTC-R EQUAL SPACES
           OR  ANO OF SGTST065-DT-CLCD-MVTC-R EQUAL SPACES
               MOVE -1                 TO DT-CLCD-MVTC-N
           END-IF

           IF  SGTST065-VL-CLCD-MVTC EQUAL ZEROS
               MOVE -1                 TO VL-CLCD-MVTC-N
           END-IF

           IF  SGTST065-CD-TIP-MVTC EQUAL ZEROS
               MOVE -1                 TO CD-TIP-MVTC-N
           END-IF

           IF  SGTST065-QT-EVT EQUAL ZEROS
               MOVE -1                 TO QT-EVT-N
           END-IF

           IF  SGTST065-TX-OBS-MVTC EQUAL SPACES
               MOVE -1                 TO TX-OBS-MVTC-N
           END-IF

           IF  SGTST065-TX-DCR-ERRO-IMP EQUAL SPACES
               MOVE -1                 TO TX-DCR-ERRO-IMP-N
           END-IF
           IF  SGTST065-TX-DCR-TIP-CPTR EQUAL SPACES
               MOVE -1                 TO TX-DCR-TIP-CPTR-N
           END-IF
           IF  SGTST065-CD-TIP-LCTO-CTB EQUAL ZEROS
               MOVE -1                 TO CD-TIP-LCTO-CTB-N
           END-IF
           IF  SGTST065-CD-CTRA-TIT-VL  EQUAL ZEROS
               MOVE -1                 TO CD-CTRA-TIT-VL-N
           END-IF
           IF  SGTST065-CD-TIP-OGM-INF  EQUAL ZEROS
               MOVE -1                 TO CD-TIP-OGM-INF-N
           END-IF
      *     IF  SGTST065-CD-OPR-ITMT-FNCO  EQUAL ZEROS
      *         MOVE -1                 TO CD-OPR-ITMT-FNCO-N
      *     END-IF
      *     IF  SGTST065-CD-TIP-CTRA-CTDA  EQUAL ZEROS
      *         MOVE -1                 TO CD-TIP-CTRA-CTDA-N
      *     END-IF
      *     IF  SGTST065-NR-SEQL-PGTO-EVT  EQUAL ZEROS
      *         MOVE -1                 TO NR-SEQL-PGTO-EVT-N
      *     END-IF
      *     IF  SGTST065-CD-GR-EVT-FDAR  EQUAL ZEROS
      *         MOVE -1                 TO CD-GR-EVT-FDAR-N
      *     END-IF
      *     IF  SGTST065-CD-ITMT-FNCO-FDAR  EQUAL ZEROS
      *         MOVE -1                 TO CD-ITMT-FNCO-FDAR
      *     END-IF
      *     IF  SGTST065-CD-CLI-FDAR  EQUAL ZEROS
      *         MOVE -1                 TO CD-CLI-FDAR-N
      *     END-IF
           IF  SGTST065-CD-EVT-ITMT-CTIP  EQUAL ZEROS
               MOVE -1                 TO CD-EVT-ITMT-CTIP-N
           END-IF
           IF  SGTST065-TX-EVT-ITMT-CTIP  EQUAL SPACES
               MOVE -1                 TO TX-EVT-ITMT-CTIP-N
           END-IF
           IF  SGTST065-NR-CT-CLI-CMRA  EQUAL ZEROS
               MOVE -1                 TO NR-CT-CLI-CMRA-N
           END-IF
           IF  SGTST065-TX-GR-ITMT-FNCO  EQUAL SPACES
               MOVE -1                 TO TX-GR-ITMT-FNCO-N
           END-IF
           IF  SGTST065-NR-UNCO-CTR-OPR  EQUAL ZEROS
               MOVE -1                 TO NR-UNCO-CTR-OPR-N
           END-IF
           IF  SGTST065-VL-PU-OPR-ITMT  EQUAL ZEROS
               MOVE -1                 TO VL-PU-OPR-ITMT-N
           END-IF
           IF  SGTST065-VL-BRTO-OPR  EQUAL ZEROS
               MOVE -1                 TO VL-BRTO-OPR-N
           END-IF
           IF  SGTST065-CD-TIP-LCTO-PSC  EQUAL ZEROS
               MOVE -1                 TO CD-TIP-LCTO-PSC-N
           END-IF.

       160000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       400000-INCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO PSC-ERRO-PGM
           PERFORM 430000-BUSCA-NR-SEQUENCIA
           EXEC SQL
                INSERT INTO DB2SGT.IMP_MVTC_FNCR_TIT
                ( DT_IMP_MVTC_FNCR
                , HR_IMP_MVTC_FNCR
                , NR_IMP_MVTC_FNCR
                , CD_TIT_OPR_ETRD
                , NR_DVS_TIT
                , DT_EFT_MVTC
                , VL_EFT_MVTC
                , DT_CLCD_MVTC
                , VL_CLCD_MVTC
                , CD_TIP_MVTC
                , QT_EVT
                , CD_EST_IMP
                , TX_OBS_MVTC
                , TX_DCR_ERRO_IMP
                , TX_DCR_TIP_CPTR
                , CD_TIP_LCTO_CTB
                , CD_CTRA_TIT_VL
                , CD_TIP_OGM_INF
                , CD_OPR_ITMT_FNCO
                , CD_TIP_CTRA_CTDA
                , NR_SEQL_PGTO_EVT
                , CD_GR_EVT_FDAR
                , CD_ITMT_FNCO_FDAR
                , CD_CLI_FDAR
                , CD_EVT_ITMT_CTIP
                , TX_EVT_ITMT_CTIP
                , NR_CT_CLI_CMRA
                , TX_GR_ITMT_FNCO
                , NR_UNCO_CTR_OPR
                , VL_PU_OPR_ITMT
                , VL_BRTO_OPR
                , CD_TIP_LCTO_PSC )
                VALUES
                ( :SGTST065-DT-IMP-MVTC-FNCR
                , :SGTST065-HR-IMP-MVTC-FNCR
                , :SGTST065-NR-IMP-MVTC-FNCR
                , :SGTST065-CD-TIT-OPR-ETRD
                           :CD-TIT-OPR-ETRD-N
                , :SGTST065-NR-DVS-TIT
                           :NR-DVS-TIT-N
                , :SGTST065-DT-EFT-MVTC
                           :DT-EFT-MVTC-N
                , :SGTST065-VL-EFT-MVTC
                           :VL-EFT-MVTC-N
                , :SGTST065-DT-CLCD-MVTC
                           :DT-CLCD-MVTC-N
                , :SGTST065-VL-CLCD-MVTC
                           :VL-CLCD-MVTC-N
                , :SGTST065-CD-TIP-MVTC
                           :CD-TIP-MVTC-N
                , :SGTST065-QT-EVT
                           :QT-EVT-N
                , :SGTST065-CD-EST-IMP
                , :SGTST065-TX-OBS-MVTC
                           :TX-OBS-MVTC-N
                , :SGTST065-TX-DCR-ERRO-IMP
                           :TX-DCR-ERRO-IMP-N
                , :SGTST065-TX-DCR-TIP-CPTR
                           :TX-DCR-TIP-CPTR-N
                , :SGTST065-CD-TIP-LCTO-CTB
                           :CD-TIP-LCTO-CTB-N
                , :SGTST065-CD-CTRA-TIT-VL
                           :CD-CTRA-TIT-VL-N
                , :SGTST065-CD-TIP-OGM-INF
                           :CD-TIP-OGM-INF-N
                , :SGTST065-CD-OPR-ITMT-FNCO
                           :CD-OPR-ITMT-FNCO-N
                , :SGTST065-CD-TIP-CTRA-CTDA
                           :CD-TIP-CTRA-CTDA-N
                , :SGTST065-NR-SEQL-PGTO-EVT
                           :NR-SEQL-PGTO-EVT-N
                , :SGTST065-CD-GR-EVT-FDAR
                           :CD-GR-EVT-FDAR-N
                , :SGTST065-CD-ITMT-FNCO-FDAR
                           :CD-ITMT-FNCO-FDAR-N
                , :SGTST065-CD-CLI-FDAR
                           :CD-CLI-FDAR-N
                , :SGTST065-CD-EVT-ITMT-CTIP
                           :CD-EVT-ITMT-CTIP-N
                , :SGTST065-TX-EVT-ITMT-CTIP
                           :TX-EVT-ITMT-CTIP-N
                , :SGTST065-NR-CT-CLI-CMRA
                           :NR-CT-CLI-CMRA-N
                , :SGTST065-TX-GR-ITMT-FNCO
                           :TX-GR-ITMT-FNCO-N
                , :SGTST065-NR-UNCO-CTR-OPR
                           :NR-UNCO-CTR-OPR-N
                , :SGTST065-VL-PU-OPR-ITMT
                           :VL-PU-OPR-ITMT-N
                , :SGTST065-VL-BRTO-OPR
                           :VL-BRTO-OPR-N
                , :SGTST065-CD-TIP-LCTO-PSC
                           :CD-TIP-LCTO-PSC-N)
           END-EXEC
           PERFORM 980101-GUARDA-CACHE
           INITIALIZE DADOS-LOG
           MOVE 'I'                    TO SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG
           .
       400000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       430000-BUSCA-NR-SEQUENCIA       SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO PSC-ERRO-PGM

           EXEC SQL
               SELECT VALUE (MAX(NR_IMP_MVTC_FNCR) , 0 )
                INTO :SGTST065-NR-IMP-MVTC-FNCR
                FROM DB2SGT.IMP_MVTC_FNCR_TIT
                WHERE DT_IMP_MVTC_FNCR   = :SGTST065-DT-IMP-MVTC-FNCR
                  AND HR_IMP_MVTC_FNCR   = :SGTST065-HR-IMP-MVTC-FNCR
           END-EXEC
           ADD 1                       TO SGTST065-NR-IMP-MVTC-FNCR
           .
       430000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       500000-ALTERAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO PSC-ERRO-PGM
           EXEC SQL
             UPDATE DB2SGT.IMP_MVTC_FNCR_TIT
             SET CD_TIT_OPR_ETRD    = :SGTST065-CD-TIT-OPR-ETRD
                                      :CD-TIT-OPR-ETRD-N
               , NR_DVS_TIT         = :SGTST065-NR-DVS-TIT
                                      :NR-DVS-TIT-N
               , DT_EFT_MVTC        = :SGTST065-DT-EFT-MVTC
                                      :DT-EFT-MVTC-N
               , VL_EFT_MVTC        = :SGTST065-VL-EFT-MVTC
                                      :VL-EFT-MVTC-N
               , DT_CLCD_MVTC       = :SGTST065-DT-CLCD-MVTC
                                      :DT-CLCD-MVTC-N
               , VL_CLCD_MVTC       = :SGTST065-VL-CLCD-MVTC
                                      :VL-CLCD-MVTC-N
               , CD_TIP_MVTC        = :SGTST065-CD-TIP-MVTC
                                      :CD-TIP-MVTC-N
               , QT_EVT             = :SGTST065-QT-EVT
                                      :QT-EVT-N
               , CD_EST_IMP         = :SGTST065-CD-EST-IMP
               , TX_OBS_MVTC        = :SGTST065-TX-OBS-MVTC
                                      :TX-OBS-MVTC-N
               , TX_DCR_ERRO_IMP    = :SGTST065-TX-DCR-ERRO-IMP
                                      :TX-DCR-ERRO-IMP-N
               , TX_DCR_TIP_CPTR    = :SGTST065-TX-DCR-TIP-CPTR
                                      :TX-DCR-TIP-CPTR-N
               , CD_TIP_LCTO_CTB    = :SGTST065-CD-TIP-LCTO-CTB
                                      :CD-TIP-LCTO-CTB-N
               , CD_CTRA_TIT_VL    = :SGTST065-CD-CTRA-TIT-VL
                                      :CD-CTRA-TIT-VL-N
               , CD_TIP_OGM_INF    = :SGTST065-CD-TIP-OGM-INF
                                      :CD-TIP-OGM-INF-N
               , CD_OPR_ITMT_FNCO  = :SGTST065-CD-OPR-ITMT-FNCO
                                      :CD-OPR-ITMT-FNCO-N
               , CD_TIP_CTRA_CTDA  = :SGTST065-CD-TIP-CTRA-CTDA
                                      :CD-TIP-CTRA-CTDA-N
               , NR_SEQL_PGTO_EVT  = :SGTST065-NR-SEQL-PGTO-EVT
                                      :NR-SEQL-PGTO-EVT-N
               , CD_GR_EVT_FDAR    = :SGTST065-CD-GR-EVT-FDAR
                                      :CD-GR-EVT-FDAR-N
               , CD_ITMT_FNCO_FDAR = :SGTST065-CD-ITMT-FNCO-FDAR
                                      :CD-ITMT-FNCO-FDAR-N
               , CD_CLI_FDAR       = :SGTST065-CD-CLI-FDAR
                                      :CD-CLI-FDAR-N
               , CD_EVT_ITMT_CTIP  = :SGTST065-CD-EVT-ITMT-CTIP
                                      :CD-EVT-ITMT-CTIP-N
               , TX_EVT_ITMT_CTIP  = :SGTST065-TX-EVT-ITMT-CTIP
                                      :TX-EVT-ITMT-CTIP-N
               , NR_CT_CLI_CMRA    = :SGTST065-NR-CT-CLI-CMRA
                                      :NR-CT-CLI-CMRA-N
               , TX_GR_ITMT_FNCO   = :SGTST065-TX-GR-ITMT-FNCO
                                      :TX-GR-ITMT-FNCO-N
               , NR_UNCO_CTR_OPR   = :SGTST065-NR-UNCO-CTR-OPR
                                      :NR-UNCO-CTR-OPR-N
               , VL_PU_OPR_ITMT    = :SGTST065-VL-PU-OPR-ITMT
                                      :VL-PU-OPR-ITMT-N
               , VL_BRTO_OPR       = :SGTST065-VL-BRTO-OPR
                                      :VL-BRTO-OPR-N
               , CD_TIP_LCTO_PSC   = :SGTST065-CD-TIP-LCTO-PSC
                                      :CD-TIP-LCTO-PSC-N
                WHERE DT_IMP_MVTC_FNCR   = :SGTST065-DT-IMP-MVTC-FNCR
                  AND HR_IMP_MVTC_FNCR   = :SGTST065-HR-IMP-MVTC-FNCR
                  AND NR_IMP_MVTC_FNCR   = :SGTST065-NR-IMP-MVTC-FNCR
           END-EXEC
           IF  SQLCODE EQUAL +100
               INITIALIZE CHAVE-CACHE
               MOVE 'Alteracao nao efetuada, registro inexistente'
                                             TO MSG-ERRO
               MOVE SQLCODE                  TO CD-RTN-PGM
               PERFORM 999001-ERRO-001
           END-IF
           PERFORM 980101-GUARDA-CACHE
           INITIALIZE DADOS-LOG
           MOVE 'A'                    TO SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG
           .
       500000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       600000-EXCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO PSC-ERRO-PGM
           INITIALIZE DADOS-LOG
           MOVE 'E'                    TO SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG
           EXEC SQL
                DELETE FROM DB2SGT.IMP_MVTC_FNCR_TIT
                WHERE DT_IMP_MVTC_FNCR   = :SGTST065-DT-IMP-MVTC-FNCR
                  AND HR_IMP_MVTC_FNCR   = :SGTST065-HR-IMP-MVTC-FNCR
                  AND NR_IMP_MVTC_FNCR   = :SGTST065-NR-IMP-MVTC-FNCR
           END-EXEC
           INITIALIZE REG-CACHE
           IF  SQLCODE EQUAL +100
               MOVE 'Exclusao nao efetuada, registro inexistente'
                                             TO MSG-ERRO
               MOVE SQLCODE                  TO CD-RTN-PGM
               PERFORM 999001-ERRO-001
           END-IF
           .
       600000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       700000-CONSULTA                 SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO PSC-ERRO-PGM
      *    Campos nulos sao inicializados na consulta
           INITIALIZE SGTST065-CD-TIT-OPR-ETRD
                      SGTST065-NR-DVS-TIT
                      SGTST065-DT-EFT-MVTC
                      SGTST065-VL-EFT-MVTC
                      SGTST065-DT-CLCD-MVTC
                      SGTST065-VL-CLCD-MVTC
                      SGTST065-CD-TIP-MVTC
                      SGTST065-QT-EVT
                      SGTST065-CD-EST-IMP
                      SGTST065-TX-OBS-MVTC
                      SGTST065-TX-DCR-ERRO-IMP
                      SGTST065-TX-DCR-TIP-CPTR
                      SGTST065-CD-TIP-LCTO-CTB
                      SGTST065-CD-CTRA-TIT-VL
                      SGTST065-CD-TIP-OGM-INF
                      SGTST065-CD-OPR-ITMT-FNCO
                      SGTST065-CD-TIP-CTRA-CTDA
                      SGTST065-NR-SEQL-PGTO-EVT
                      SGTST065-CD-GR-EVT-FDAR
                      SGTST065-CD-ITMT-FNCO-FDAR
                      SGTST065-CD-CLI-FDAR
                      SGTST065-CD-EVT-ITMT-CTIP
                      SGTST065-TX-EVT-ITMT-CTIP
                      SGTST065-NR-CT-CLI-CMRA
                      SGTST065-TX-GR-ITMT-FNCO
                      SGTST065-NR-UNCO-CTR-OPR
                      SGTST065-VL-PU-OPR-ITMT
                      SGTST065-VL-BRTO-OPR
                      SGTST065-CD-TIP-LCTO-PSC
           EXEC SQL
                 SELECT CD_TIT_OPR_ETRD
                      , NR_DVS_TIT
                      , DT_EFT_MVTC
                      , VL_EFT_MVTC
                      , DT_CLCD_MVTC
                      , VL_CLCD_MVTC
                      , CD_TIP_MVTC
                      , QT_EVT
                      , CD_EST_IMP
                      , TX_OBS_MVTC
                      , TX_DCR_ERRO_IMP
                      , TX_DCR_TIP_CPTR
                      , CD_TIP_LCTO_CTB
                      , CD_CTRA_TIT_VL
                      , CD_TIP_OGM_INF
                      , CD_OPR_ITMT_FNCO
                      , CD_TIP_CTRA_CTDA
                      , NR_SEQL_PGTO_EVT
                      , CD_GR_EVT_FDAR
                      , CD_ITMT_FNCO_FDAR
                      , CD_CLI_FDAR
                      , CD_EVT_ITMT_CTIP
                      , TX_EVT_ITMT_CTIP
                      , NR_CT_CLI_CMRA
                      , TX_GR_ITMT_FNCO
                      , NR_UNCO_CTR_OPR
                      , VL_PU_OPR_ITMT
                      , VL_BRTO_OPR
                      , CD_TIP_LCTO_PSC
                   INTO  :SGTST065-CD-TIT-OPR-ETRD
                                  :CD-TIT-OPR-ETRD-N
                       , :SGTST065-NR-DVS-TIT
                                  :NR-DVS-TIT-N
                       , :SGTST065-DT-EFT-MVTC
                                  :DT-EFT-MVTC-N
                       , :SGTST065-VL-EFT-MVTC
                                  :VL-EFT-MVTC-N
                       , :SGTST065-DT-CLCD-MVTC
                                  :DT-CLCD-MVTC-N
                       , :SGTST065-VL-CLCD-MVTC
                                  :VL-CLCD-MVTC-N
                       , :SGTST065-CD-TIP-MVTC
                                  :CD-TIP-MVTC-N
                       , :SGTST065-QT-EVT
                                  :QT-EVT-N
                       , :SGTST065-CD-EST-IMP
                       , :SGTST065-TX-OBS-MVTC
                                  :TX-OBS-MVTC-N
                       , :SGTST065-TX-DCR-ERRO-IMP
                                  :TX-DCR-ERRO-IMP-N
                       , :SGTST065-TX-DCR-TIP-CPTR
                                  :TX-DCR-TIP-CPTR-N
                       , :SGTST065-CD-TIP-LCTO-CTB
                                  :CD-TIP-LCTO-CTB-N
                       , :SGTST065-CD-CTRA-TIT-VL
                                  :CD-CTRA-TIT-VL-N
                       , :SGTST065-CD-TIP-OGM-INF
                                  :CD-TIP-OGM-INF-N
                       , :SGTST065-CD-OPR-ITMT-FNCO
                                  :CD-OPR-ITMT-FNCO-N
                       , :SGTST065-CD-TIP-CTRA-CTDA
                                  :CD-TIP-CTRA-CTDA-N
                       , :SGTST065-NR-SEQL-PGTO-EVT
                                  :NR-SEQL-PGTO-EVT-N
                       , :SGTST065-CD-GR-EVT-FDAR
                                  :CD-GR-EVT-FDAR-N
                       , :SGTST065-CD-ITMT-FNCO-FDAR
                                  :CD-ITMT-FNCO-FDAR-N
                       , :SGTST065-CD-CLI-FDAR
                                  :CD-CLI-FDAR-N
                       , :SGTST065-CD-EVT-ITMT-CTIP
                                  :CD-EVT-ITMT-CTIP-N
                       , :SGTST065-TX-EVT-ITMT-CTIP
                                  :TX-EVT-ITMT-CTIP-N
                       , :SGTST065-NR-CT-CLI-CMRA
                                  :NR-CT-CLI-CMRA-N
                       , :SGTST065-TX-GR-ITMT-FNCO
                                  :TX-GR-ITMT-FNCO-N
                       , :SGTST065-NR-UNCO-CTR-OPR
                                  :NR-UNCO-CTR-OPR-N
                       , :SGTST065-VL-PU-OPR-ITMT
                                  :VL-PU-OPR-ITMT-N
                       , :SGTST065-VL-BRTO-OPR
                                  :VL-BRTO-OPR-N
                       , :SGTST065-CD-TIP-LCTO-PSC
                                  :CD-TIP-LCTO-PSC-N

                         FROM DB2SGT.IMP_MVTC_FNCR_TIT
                WHERE DT_IMP_MVTC_FNCR   = :SGTST065-DT-IMP-MVTC-FNCR
                  AND HR_IMP_MVTC_FNCR   = :SGTST065-HR-IMP-MVTC-FNCR
                  AND NR_IMP_MVTC_FNCR   = :SGTST065-NR-IMP-MVTC-FNCR
           END-EXEC
           IF  SQLCODE = +100
               INITIALIZE SGTST065-AREA-DADOS
               MOVE 'Importacao movimentacao financeira titulo inexisten
      -        'te' TO MSG-ERRO
               MOVE SQLCODE                   TO CD-RTN-PGM
               PERFORM 999001-ERRO-001
           END-IF
           PERFORM 980101-GUARDA-CACHE
           .
       700000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       800000-CONSULTAR-ULT-IMP-TIT    SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO PSC-ERRO-PGM
           EXEC SQL
                 SELECT DT_IMP_MVTC_FNCR
                      , HR_IMP_MVTC_FNCR
                   INTO  :SGTST065-DT-IMP-MVTC-FNCR
                       , :SGTST065-HR-IMP-MVTC-FNCR
                       FROM DB2SGT.IMP_MVTC_FNCR_TIT
                WHERE
                      CD_TIT_OPR_ETRD    = :SGTST065-CD-TIT-OPR-ETRD
                  AND NR_DVS_TIT         = :SGTST065-NR-DVS-TIT
                ORDER BY  DT_IMP_MVTC_FNCR  DESC
                       ,  HR_IMP_MVTC_FNCR  DESC
                FETCH FIRST 1 ROW ONLY
           END-EXEC
           IF  SQLCODE = +100
               INITIALIZE SGTST065-AREA-DADOS
               MOVE 'Importacao movimentacao financeira titulo inexisten
      -        'te' TO MSG-ERRO
               MOVE SQLCODE                   TO CD-RTN-PGM
               PERFORM 999001-ERRO-001
           END-IF
           .
       800000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      * 900000-VERIFICAR-TIT-ERRO       SECTION.
      *-----------------------------------------------------------------
      *     MOVE 0012                   TO PSC-ERRO-PGM
      *     MOVE  SPACES   TO   SGTST065-IN-IMP-MVTC-SCS
      *     EXEC SQL
      *           SELECT 'N'
      *             INTO  :SGTST065-IN-IMP-MVTC-SCS
      *                 FROM DB2SGT.IMP_MVTC_FNCR_TIT
      *          WHERE
      *                DT_IMP_MVTC_FNCR   = :SGTST065-DT-IMP-MVTC-FNCR
      *            AND HR_IMP_MVTC_FNCR   = :SGTST065-HR-IMP-MVTC-FNCR
      *            AND CD_TIT_OPR_ETRD    = :SGTST065-CD-TIT-OPR-ETRD
      *            AND NR_DVS_TIT         = :SGTST065-NR-DVS-TIT
      *            AND IN_IMP_MVTC_SCS    = 'N'
      *            AND DT_EFT_MVTC       >= :SGTST065-DT-EFT-MVTC
      *          FETCH FIRST 1 ROW ONLY
      *     END-EXEC
      *     IF  SQLCODE = +100
      *         INITIALIZE SGTST065-AREA-DADOS
      *         MOVE 'Importacao movimentacao financeira titulo inexisten
      * -        'te' TO MSG-ERRO
      *         MOVE SQLCODE                   TO CD-RTN-PGM
      *         PERFORM 999001-ERRO-001
      *     END-IF
      *     .
      * 900000-SAI.
      *     EXIT.
      *-----------------------------------------------------------------
       980101-GUARDA-CACHE             SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO PSC-ERRO-PGM

           MOVE SGTST065-CHAVE         TO CHAVE-CACHE
           MOVE SGTST065-AREA-DADOS         TO DADOS-CACHE
           .
       980101-SAI.
           EXIT.
      *-----------------------------------------------------------------
       980102-GRAVA-LOG                SECTION.
      *-----------------------------------------------------------------
           MOVE 0014                   TO PSC-ERRO-PGM
           MOVE 'IMP_MVTC_FNCR_TIT '        TO SGTS014G-NM-ENT
           IF  EXECUCAO-ONLINE
               MOVE SGTST065-CD-USU-RSP-ATL TO SGTS014G-CD-CHV-USU
      *        EXEC CICS
      *             ASSIGN USERID(SGTS014G-CD-CHV-USU)
      *        END-EXEC
           ELSE
               MOVE 'SYSTEM'                TO SGTS014G-CD-CHV-USU
           END-IF
           MOVE 'SGTST065'                  TO SGTS014G-NM-PGM
           MOVE SGTST065-DT-IMP-MVTC-FNCR      TO DT-IMP-MVTC-FNCR-LOG
           MOVE SGTST065-HR-IMP-MVTC-FNCR      TO HR-IMP-MVTC-FNCR-LOG
           MOVE SGTST065-NR-IMP-MVTC-FNCR      TO NR-IMP-MVTC-FNCR-LOG
           STRING DT-IMP-MVTC-FNCR-LOG
           ';'    HR-IMP-MVTC-FNCR-LOG
           ';'    NR-IMP-MVTC-FNCR-LOG
               DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR
           MOVE LENGTH OF DADOS-LOG    TO EIBCALEN
           CALL SGTS014G USING DFHEIBLK DADOS-LOG
           IF  SGTS014G-CD-RTN-PGM  NOT EQUAL ZEROS
               MOVE SGTS014G-MSG-ERRO-PGM       TO MSG-ERRO-PGM
               MOVE CD-RTN-PGM                  TO AUX-CD-ERRO
               MOVE 112                         TO CD-RTN-PGM
               MOVE  'Erro na gravação das informações do Log.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           .
      *--------------------
      * 999000-ERRO SECTION
      *--------------------
      *-----------------
      * 999001-ERRO-001.
      *-----------------
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

           MOVE RTN-PGM             TO SGTST065-RTN-PGM

      * quando for incluida a monitoração de erro das variáveis neste
      * pgm essa verificação poderá ser excluída. Essa condição está
      * aqui por causa da rotina sbdataf e do retorno do sgts014g por
      * retornarem um código de erro único e ser necessário o código
      * de erro origem
      *
           IF   AUX-ERRO            NOT EQUAL  SPACES
            IF  EXECUCAO-BATCH
                MOVE AUX-CD-ERRO    TO         CD-RTN-PGM
             ELSE
                MOVE AUX-ERRO       TO         TX-IDFC-ATNT-MSG-E
             END-IF
           END-IF

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

      *---------------------- FIM SGTST065 ----------------------------