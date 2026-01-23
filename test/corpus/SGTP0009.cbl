             CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTPBA72
      * ANALISTA..: F9458161 - Vanessa Tsukamoto
      * AUTOR.....: F8785286 - Roseli Akemi Kawahata
      * DATA......: 11.10.2024
      * OBJETIVO..: Atualiza informação das tranches a partir de dados
      *             dos sistemas CAC e GSF cart. 200000.
      *             (Projeto Res CMN 4966)
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * Subrotinas Utilizadas:
      * ----------------------
      *    SGTST002 - Persistencia da tabela TIT_OPR_ETRD_MCP
      *    SGTSX005 - Persistencia da tabela OPR_ETRD_MCP
      *    SGTST006 - Persistencia da tabela CLI_OPR_ETRD
      *    SGTST009 - Persistencia da tabela EST_OPR_ETRD
      *    SGTST011 - Persistencia da tabela RMNC_OPR_ETRD
      *    SGTS014G - Persistencia da tabela LOG_TAB_SIS
      *-----------------------------------------------------------------
      * VRS01B 23.10.2024 - F9812118 - Inclusão de contadores de display.
      * VRS001 11.10.2024 - F8785286 - IMPLANTACAO.
      *-----------------------------------------------------------------
      
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTPBA72.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       INPUT-OUTPUT                    SECTION.
      *
       FILE-CONTROL.
           SELECT SGTFB72E  ASSIGN  TO  SGTFB72E.
           SELECT SGTF136S  ASSIGN  TO  SGTF136S.
      
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE                            SECTION.
      *
      * Arquivo com informações CAC e GSF para criação de operações
      *   carteira 200000
       FD  SGTFB72E
           RECORDING F
           RECORD 102
           BLOCK 0.
       01  REG-FD-SGTFB72E        PIC X(102).
      
      * Arquivo ocorrências de erro
       FD  SGTF136S
           RECORDING F
           RECORD 349
           BLOCK 0.
       01  REG-FD-SGTF136S        PIC X(349).
      
       WORKING-STORAGE SECTION.
      
      *--------------------> Variaveis do Sistema <--------------------*
       77  CTE-INICIO-SS               PIC  X(35) VALUE
           '*** STORAGE SECTION COMECA AQUI ***'.
      
       77  CTE-PROG                    PIC  X(17) VALUE
           '*** SGTPBA72 ***'.
      
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.
      
      * CHAMADOS + MOCK
       01 TAB-PROGRAMAS-CHAMADOS.
          03 SGTST002                  PIC X(8) VALUE 'SGTST002'.
          03 SGTSX005                  PIC X(8) VALUE 'SGTSX005'.
          03 SGTST006                  PIC X(8) VALUE 'SGTST006'.
          03 SGTST009                  PIC X(8) VALUE 'SGTST009'.
          03 SGTST011                  PIC X(8) VALUE 'SGTST011'.
          03 SGTS014G                  PIC X(8) VALUE 'SGTS014G'.
          03 CICSB001                  PIC X(8) VALUE 'CICSB001'.
          03 HLPS0010                  PIC X(8) VALUE 'HLPS0010'.
          03 SBABEND                   PIC X(8) VALUE 'SBABEND'.
          03 SBCPU                     PIC X(8) VALUE 'SBCPU'.
          03 SBDATAF                   PIC X(8) VALUE 'SBDATAF'.
          03 SBSTR                     PIC X(8) VALUE 'SBSTR'.
          03 SBVERSAO                  PIC X(8) VALUE 'SBVERSAO'.
          03 SGTST136                  PIC X(8) VALUE 'SGTST136'.
          03 DB2S0001                  PIC X(8) VALUE 'DB2S0001'.
      *
       COPY DB2K0001.
      *
       COPY ALMKCBL1.
      
      *----------------------------------------------------------------*
      *           Book do programa de ambiente - CICS5000              *
      *       Definido na work, para preservar as informacoes          *
      *----------------------------------------------------------------*
       LOCAL-STORAGE SECTION.
       COPY HLPKE001.
      
      *--> HLPKDFHE - Definicao da area DFHEIBLK
       COPY HLPKDFHE.
      
       COPY HLPKE01B.
      
       01 IA-1                         PIC S9(004) COMP-5  VALUE 0.
       01 IA-2                         PIC S9(004) COMP-5  VALUE 0.
      
       01 GDA-MSG-ERRO                 PIC  X(200) VALUE SPACES.
       01 CNT-LIDOS                    PIC S9(009) COMP-5 VALUE 0.
       01 CNT-GRAVADOS                 PIC S9(009) COMP-5 VALUE 0.
       01 CNT-INCD                     PIC S9(009) COMP-5 VALUE 0.      VRS01B
       01 CNT-ALTD                     PIC S9(009) COMP-5 VALUE 0.      VRS01B
       01 CNT-ERRO                     PIC S9(009) COMP-5 VALUE 0.
       01 CNT-COMMIT                   PIC S9(009) COMP-5 VALUE 0.
       01 CNT-CHAVE                    PIC S9(009) COMP-5 VALUE 0.
       01 CARTEIRA                     PIC S9(009) COMP-5 VALUE 200000.
       01 QT-REG-SELD                  PIC S9(004) COMP-5 VALUE 0.
       01 NM-PGM                       PIC  X(008) VALUE ' '.
       01 AUX-CD-CHV-USU               PIC  X(008) VALUE 'SGTPBA72'.
       01 AUX-ERRO                     PIC  X(001) VALUE ' '.
       01 CD-TIP-EST-VRS               PIC S9(004) COMP-5 VALUE 3.
      
       01 GDA-CD-TIT-OPR-ETRD          PIC  X(012) VALUE ' '.
       01 GDA-VL-PU-CPR-TIT-OPR        PIC S9(9)V9(8) COMP-3 VALUE 0.
      *
      * Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ      VALUE +100.
           03  IN-CURSOR-PSQ-ABTO      PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO     VALUE 'S'.
      
      
       01  DIS-CODE                    PIC  --------9.
       01  DIS-SINAL-CODE              PIC X(1).
       01  DIS-VALOR                   PIC ZZZZZZZZZZ9,9999999.
       01  DIS-SMALL                   PIC ----9.
       01  DIS-INTEGER                 PIC ---------9.
      
      *01  CHAMADA-GSF                  PIC  X(01) VALUE SPACES.
      *    88 ERRO-GSF                  VALUE 'S'.
      
       01  REG-AUX.
           03  AUX-CD-ITMT-FNCO-FDAR    PIC  9(009) COMP-5.
           03  AUX-CD-GR-EVT-FDAR       PIC  9(004) COMP-5.
           03  AUX-DT-EVT-ITMT-FDAR     PIC   X(010).
           03  AUX-PC-TAXA-PGTO-EVT     PIC  S9(10)V9(7) COMP-3.
           03  AUX-CD-ITMT-FNCO         PIC   X(050).
           03  AUX-CD-IEC               PIC  S9(004) COMP-5.
               88  AUX-VNA              VALUE 915.
           03  AUX-VL-PREC-ITMT         PIC S9(10)V9(8) COMP-3.
           03  AUX-VL-SBR-ITMT          PIC S9(10)V9(8) COMP-3.
           03  AUX-VL-EVT-ITMT          PIC S9(10)V9(8) COMP-3.
      
       01  DT-ATU                       PIC X(10) VALUE ' '.
       01  DT-ATU-R REDEFINES DT-ATU.
           03 DIA                       PIC  9(02).
           03 FILLER                    PIC  X(01).
           03 MES                       PIC  9(02).
           03 FILLER                    PIC  X(01).
           03 ANO                       PIC  9(04).

      * Area de subrotina  SBTIMEDT
       01 DADOS-SBTIMEDT.
          03 CD-FUC                    PIC X(03)      VALUE 'F01'.
          03 AREA-TRB.
             05 AREA-TRB-TAM           PIC S9(9) COMP VALUE 1024.
             05 AREA-TRB-DADOS         PIC  X(1024)   VALUE ' '.
          03 FMT-TS                    PIC S9(4) COMP VALUE 1.
          03 TAM-TS                    PIC S9(4) COMP VALUE 30.
          03 TS-FMTD                   PIC X(30).
          03 TS-FMTD-R                 REDEFINES  TS-FMTD.
             05  ANO                   PIC  9(4).
             05  FILLER                PIC  X(1).
             05  MES                   PIC  9(2).
             05  FILLER                PIC  X(1).
             05  DIA                   PIC  9(2).
             05  FILLER                PIC  X(1).
             05  HORA                  PIC  9(2).
             05  FILLER                PIC  X(1).
             05  MINUTO                PIC  9(2).
             05  FILLER                PIC  X(1).
             05  SEGUNDO               PIC  9(2).
             05  FILLER                PIC  X(1).
             05  MICRO-SEGUNDO         PIC  9(6).
             05  FILLER                PIC  X(4).
      
      * Campos utilizados para leitura
       01 REG-WS.
          03 CD-CTRA-TIT-VL            PIC S9(009) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROES.
          03 CD-OPR-ETRD-MCP           PIC S9(011) COMP-3
                                       OCCURS 0101 TIMES  VALUE ZEROES.
          03 NR-VRS-OPR-ETRD           PIC S9(004) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROES.
          03 DT-CPR-OPR                PIC  X(010)
                                       OCCURS 0101 TIMES  VALUE SPACES.
          03 CD-TIT-OPR-ETRD           PIC  X(012)
                                       OCCURS 0101 TIMES  VALUE SPACES.
          03 NR-DVS-TIT                PIC S9(004) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROES.
          03 VL-PU-CPR-TIT-OPR         PIC S9(011)V9(8) COMP-3
                                       OCCURS 0101 TIMES  VALUE ZEROES.
      *
      * Indicadores de nulidades
          03 DT-CPR-OPR-N              PIC S9(4) COMP-5
                                       OCCURS 101 TIMES  VALUE 0.
          03 CD-TIT-OPR-ETRD-N         PIC S9(4) COMP-5
                                       OCCURS 101 TIMES  VALUE 0.
          03 NR-DVS-TIT-N              PIC S9(4) COMP-5
                                       OCCURS 101 TIMES  VALUE 0.
      
      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG      PIC  9(10) VALUE ZEROS.
           03  CD-OPR-ETRD-MCP-LOG     PIC  9(11) VALUE ZEROS.
           03  NR-VRS-OPR-ETRD-LOG     PIC  9(05) VALUE ZEROS.
           03  TS-EST-VRS-LOG          PIC  X(26) VALUE SPACES.
           03  CD-TIP-IN-LOG           PIC  9(05) VALUE ZEROS.
           03  NR-IN-OPR-ETRD-LOG      PIC  9(05) VALUE ZEROS.
      
      * ------- PROGRAMAS CHAMADOS -------------------------------------
      *01  GSFS038P-DADOS.
      *    -INC GSFK038P
      *
      *-----> Persistencia da tabela  TIT_OPR_ETRD_MCP
       01  SGTST002-DADOS.
       COPY SGTKT002.
      
      *-----> Persistencia da tabela  OPR_ETRD_MCP
       01  SGTSX005-DADOS.
       COPY SGTKX005.
      
      *--> Persistência da Tabela CLI_OPR_ETRD
       01  SGTST006-DADOS.
       COPY SGTKT006.
      
      *--> Persistência da Tabela EST_OPR_ETRD
       01  SGTST009-DADOS.
       COPY SGTKT009.
      
      *--> Persistência da Tabela RMNC_OPR_ETRD
       01  SGTST011-DADOS.
       COPY SGTKT011.
      
      * Book dos dados do log
       01  DADOS-LOG.
       COPY SGTK014G.
      
      *------- LAYOUT ARQUIVO DE ENTRADA
       01  SGTPB72E-DADOS.
       COPY SGTK072D.
      
      *------- LAYOUT ARQUIVO DE SAIDA
       01  SGTP136S-DADOS.
       COPY SGTK136E.
      
      * Controle de leitura do arquivo de entrada
       01  CTL-LET-ARQ.
           03  IN-FIM-ARQ       PIC X(01)          VALUE SPACES.
               88  FIM-ARQ                         VALUE 'S'.
      *
           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.
      *-----------------------------------------------------------------
      * Verifica se o título já está cadastrado em alguma operação
      *-----------------------------------------------------------------
           EXEC SQL
           DECLARE CURSOR-PSQ-1 CURSOR
                     WITH ROWSET POSITIONING FOR
           SELECT CD_CTRA_TIT_VL
                , CD_OPR_ETRD_MCP
                , NR_VRS_OPR_ETRD
                , DT_CPR_OPR
                , CD_TIT_OPR_ETRD
                , NR_DVS_TIT
                , VL_PU_CPR_TIT_OPR
           FROM DB2SGT.OPR_ETRD_MCP
           WHERE CD_CTRA_TIT_VL    = :CARTEIRA
             AND DT_CPR_OPR        = :K072D-DT-LQDC
             AND CD_TIT_OPR_ETRD   = :GDA-CD-TIT-OPR-ETRD
             AND VL_PU_CPR_TIT_OPR = :GDA-VL-PU-CPR-TIT-OPR
      *    GROUP BY CD_OPR_ETRD_MCP
      *           , DT_CPR_OPR
      *           , CD_TIT_OPR_ETRD
      *           , VL_PU_CPR_TIT_OPR
           END-EXEC.
      *------------------- Variaveis do Sistema -----------------------*
       01  CTE-SAI-SS                  PIC  X(40) VALUE
           '*** Storage Section Termina Aqui ***'.
      
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       000000-PRINCIPAL                SECTION.
      *----------------------------------------------------------------*
      * BOOK MOCK BATCH
       COPY ALMKCBL2.
      
      D    DISPLAY CTE-PROG ' 000000-PRINCIPAL '
      
           MOVE  0001                  TO  PSC-ERRO-PGM
      
           CALL  SBVERSAO  USING  CTE-PROG  CTE-VERS
      
           DISPLAY '------------------------------------------'
           DISPLAY '-- INICIO DE PROCESSAMENTO - SGTPBA72     '
           DISPLAY '-- Internalização do Cronograma 200.000   '
           DISPLAY '------------------------------------------'
      
           OPEN INPUT  SGTFB72E
           OPEN OUTPUT SGTF136S
      
           PERFORM 780000-BUSCA-TS-DT
           MOVE '  .  .    '           TO DT-ATU
           MOVE DIA OF TS-FMTD-R       TO DIA OF DT-ATU-R
           MOVE MES OF TS-FMTD-R       TO MES OF DT-ATU-R
           MOVE ANO OF TS-FMTD-R       TO ANO OF DT-ATU-R

           PERFORM UNTIL FIM-ARQ
               INITIALIZE SGTPB72E-DADOS
               MOVE 'N'                TO AUX-ERRO
               READ SGTFB72E INTO SGTPB72E-DADOS
                  AT END
                     SET FIM-ARQ       TO TRUE
                  NOT AT END
                     ADD +1            TO CNT-LIDOS
                     ON SIZE ERROR
                        MOVE 'ERRO EM OPERACAO ARITMETICA 1'
                                       TO MSG-ERRO
                        PERFORM 999000-ERRO
                     END-ADD
      *
                  IF AUX-ERRO NOT EQUAL 'S'
                     PERFORM 100000-CONS-TRANCHE
                     EXEC SQL
                     SELECT CD_CTRA_TIT_VL
                          , CD_OPR_ETRD_MCP
                          , NR_VRS_OPR_ETRD
                          , DT_CPR_OPR
                          , CD_TIT_OPR_ETRD
                          , NR_DVS_TIT
                          , VL_PU_CPR_TIT_OPR
                     INTO  :CD-CTRA-TIT-VL
                         , :CD-OPR-ETRD-MCP
                         , :NR-VRS-OPR-ETRD
                         , :DT-CPR-OPR
                           :DT-CPR-OPR-N
                         , :CD-TIT-OPR-ETRD
                           :CD-TIT-OPR-ETRD-N
                         , :NR-DVS-TIT
                           :NR-DVS-TIT-N
                         , :VL-PU-CPR-TIT-OPR
                     FROM DB2SGT.OPR_ETRD_MCP
                     WHERE CD_CTRA_TIT_VL    = :CARTEIRA
                       AND DT_CPR_OPR        = :K072D-DT-LQDC
                       AND CD_TIT_OPR_ETRD   = :GDA-CD-TIT-OPR-ETRD
                       AND VL_PU_CPR_TIT_OPR = :GDA-VL-PU-CPR-TIT-OPR
      *              FETCH FIRST 1 ROW ONLY

                     END-EXEC
      
                     PERFORM 200000-TRATA-TRANCHE
                  END-IF
      *
               END-READ
      
           END-PERFORM
           PERFORM 800000-FIM
           .
       000000-SAI.
           GOBACK.
      *----------------------------------------------------------------*
       100000-CONS-TRANCHE             SECTION.
      *----------------------------------------------------------------*
      D    DISPLAY CTE-PROG ' 100000-CONS-TRANCHE '
      
           MOVE 0002                   TO PSC-ERRO-PGM
      
           MOVE ZEROS                  TO QT-REG-SELD
      
           PERFORM 770101-ABRE-CURSOR-PSQ
           MOVE K072D-CD-ITMT-FNCO-CLRG  TO GDA-CD-TIT-OPR-ETRD
           MOVE K072D-VL-PU-ATI          TO GDA-VL-PU-CPR-TIT-OPR
           PERFORM 770102-LER-CURSOR-PSQ
           PERFORM 770103-FECHA-CURSOR-PSQ
           .
       100000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       200000-TRATA-TRANCHE            SECTION.
      *----------------------------------------------------------------*
      D    DISPLAY CTE-PROG ' 200000-TRATA-TRANCHE '
      
           MOVE  0003                  TO  PSC-ERRO-PGM
      
      *    EVALUATE  QT-REG-SELD
           EVALUATE  SQLCODE
           WHEN +100
              PERFORM 210000-INCL-TRANCHE
              IF AUX-ERRO NOT EQUAL 'S'
                 PERFORM 900900-CONTROLA-COMMIT
                 ADD   +1              TO CNT-INCD
                       ON SIZE ERROR
                          MOVE 'ERRO EM OPERACAO ARITMETICA 2'
                                       TO MSG-ERRO
                          PERFORM   999000-ERRO
                 END-ADD
              END-IF
           WHEN 0
              PERFORM 220000-ATLZ-TRANCHE
              IF AUX-ERRO NOT EQUAL 'S'
                 PERFORM 900900-CONTROLA-COMMIT
                 ADD   +1              TO CNT-ALTD                      VRS01B
                       ON SIZE ERROR
                          MOVE 'ERRO EM OPERACAO ARITMETICA 3'
                                       TO MSG-ERRO
                          PERFORM   999000-ERRO
                 END-ADD
              END-IF
           WHEN  OTHER
               MOVE K072D-VL-PU-ATI   TO DIS-VALOR
               STRING 'Existe mais de uma tranche para esse titulo '
                 K072D-CD-ITMT-FNCO-CLRG  ' ' K072D-DT-LQDC
                 ' ' DIS-VALOR   DELIMITED BY SIZE
                                       INTO MSG-ERRO
               PERFORM   300000-MONTA-SAIDA
               WRITE  REG-FD-SGTF136S  FROM SGTP136S-DADOS
                 ADD   +1              TO CNT-GRAVADOS
                    ON SIZE ERROR
                       MOVE 'ERRO EM OPERACAO ARITMETICA 4'             VRS01B
                                       TO MSG-ERRO
      *                PERFORM   999000-ERRO
                 END-ADD
           END-EVALUATE
      *
      *    EXEC SQL
      *         COMMIT
      *    END-EXEC
      
      *    PERFORM 900900-CONTROLA-COMMIT
           .
       200000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       210000-INCL-TRANCHE             SECTION.
      *----------------------------------------------------------------*
      D    DISPLAY CTE-PROG ' 210000-INCL-TRANCHE '
      
           MOVE  0004                  TO  PSC-ERRO-PGM
      
           IF AUX-ERRO NOT EQUAL 'S'
              PERFORM 211000-INCL-TIT
           END-IF
      
           IF AUX-ERRO NOT EQUAL 'S'
              PERFORM 212000-INCL-OPR-SGT
           END-IF
      
           IF AUX-ERRO NOT EQUAL 'S'
              PERFORM 222000-INCL-EST-OPR
           END-IF
      
           IF AUX-ERRO NOT EQUAL 'S'
              PERFORM 223000-INCL-EST-VRS-OPR
           END-IF
      
           IF AUX-ERRO NOT EQUAL 'S'
              PERFORM 215000-INCL-REMUN
           END-IF
      
           IF AUX-ERRO NOT EQUAL 'S'
              PERFORM 216000-INCL-CLIENTE
           END-IF
           .
       210000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       211000-INCL-TIT                 SECTION.
      *--> Inclui Titulo da Operação Estruturada
      *-----------------------------------------------------------------
      D    DISPLAY CTE-PROG ' 211000-INCL-TIT '
      
           MOVE 0005                       TO PSC-ERRO-PGM
      
           MOVE SGTST002                   TO NM-PGM
           SET  SGTST002-INCLUIR           TO TRUE
           MOVE 'SGTPBA72'                 TO SGTST002-CD-CHV-USU
                                              SGTST002-CD-USU-RSP-ATL
           MOVE 'CURRENT'                  TO SGTST002-TS-ULT-ATL
           MOVE K072D-CD-ITMT-FNCO-CLRG    TO SGTST002-CD-TIT-OPR-ETRD
           MOVE ZEROES                     TO SGTST002-NR-DVS-TIT
      
      
           MOVE LENGTH OF SGTST002-DADOS   TO EIBCALEN
           CALL SGTST002  USING  DFHEIBLK  SGTST002-DADOS
      
           IF  SGTST002-CD-RTN-PGM         NOT EQUAL ZEROS
               MOVE SGTST002-CD-RTN-PGM    TO  CD-RTN-PGM
               MOVE SGTST002-MSG-ERRO      TO  MSG-ERRO
               PERFORM 999000-ERRO
           END-IF
           .
       211000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       212000-INCL-OPR-SGT             SECTION.
      *--> Inclui Operação Estruturada
      *-----------------------------------------------------------------
      D    DISPLAY CTE-PROG ' 212000-INCL-OPR-SGT '
      
           MOVE 0006                   TO PSC-ERRO-PGM
      
           MOVE ZEROES                 TO SGTSX005-CD-OPR-ETRD-MCP
                                          SGTSX005-CD-UOR-DTTR-OPR
                                          SGTSX005-NR-CC-BB
                                          SGTSX005-QT-PAPL-TIT-EMS
                                          SGTSX005-QT-PAPL-TIT-CPR
                                          SGTSX005-CD-CTRA-TIT-VL
                                          SGTSX005-CD-OPR-ETRD-MCP
                                          SGTSX005-NR-VRS-OPR-ETRD
                                          SGTSX005-NR-DVS-TIT
                                          SGTSX005-NR-UNCO-CTR-OPR
                                          SGTSX005-CD-TIP-OPR-ETRD
                                          SGTSX005-VL-CPR-OPR
                                          SGTSX005-VL-PU-EMS-TIT-OPR
                                          SGTSX005-CD-CMCP
                                          SGTSX005-CD-GR-MCP
                                          SGTSX005-CD-LVRO-MC-CPTL
                                          SGTSX005-CD-SMCP
                                          SGTSX005-CD-ESG-NEGL
                                          SGTSX005-CD-CTGR-TVM
      
           MOVE SPACES                 TO SGTSX005-IN-RMNC-VRV
                                          SGTSX005-IN-OPR-ETRD-SPRO
                                          SGTSX005-IN-OPR-ETRD-VRJ
                                          SGTSX005-IN-OPR-ETRD-DSCP
                                          SGTSX005-IN-REG-OPR-BB
                                          SGTSX005-IN-OPR-N-NEGO
                                          SGTSX005-IN-EXPO-VCLD-GRT

      *--  Domínio CD_CLSC_ITMT_FNCO (modelo de dados):
      *--  1-Normal
      *--  2-Risco de crédito aumentado
      *--  3-Problema de recuperacao de credito
           MOVE 1                      TO SGTSX005-CD-CLSC-ITMT-FNCO

           MOVE SGTSX005               TO NM-PGM
           SET  SGTSX005-INCLUIR       TO TRUE
           MOVE K072D-CD-ITMT-FNCO-CLRG
                                       TO SGTSX005-CD-TIT-OPR-ETRD
           MOVE SGTST002-NR-DVS-TIT    TO SGTSX005-NR-DVS-TIT
           MOVE 200000                 TO SGTSX005-CD-CTRA-TIT-VL
      
      *    Código do tipo de versão da operação:
      *    1 - Original        2 - Renegociação
      *    3 - Repactuação     4 - Alteração
      *    MOVE 1                      TO SGTSX005-CD-TIP-VRS-OPR
      
           MOVE K072D-CD-UOR           TO SGTSX005-CD-UOR-BB
           MOVE 1                      TO SGTSX005-NR-VRS-OPR-ETRD
                                          SGTSX005-NR-CC-BB
           MOVE K072D-VL-PU-ATI
                                       TO SGTSX005-VL-PU-CPR-TIT-OPR
           MOVE 'SGTPBA72'             TO SGTSX005-CD-USU-ULT-ATL
           MOVE K072D-DT-LQDC          TO SGTSX005-DT-CPR-OPR
           MOVE K072D-CD-PRD           TO SGTSX005-CD-PRD
           MOVE K072D-CD-MDLD          TO SGTSX005-CD-MDLD
           MOVE K072D-QT-ATI           TO SGTSX005-QT-PAPL-TIT-CPR
           MOVE K072D-CD-GR-MC-CPTL    TO SGTSX005-CD-GR-MCP
           MOVE K072D-CD-LMCP          TO SGTSX005-CD-LVRO-MC-CPTL
           MOVE K072D-CD-SMCP          TO SGTSX005-CD-SMCP
           MOVE K072D-CD-TIP-CLSC-CTB  TO SGTSX005-CD-CTGR-TVM
           MOVE K072D-CD-TIP-VRS-OPR   TO SGTSX005-CD-TIP-VRS-OPR
           MOVE K072D-CD-ESG-NEG       TO SGTSX005-CD-ESG-NEGL
           MOVE 18910                  TO SGTSX005-CD-UOR-DTTR-OPR
           MOVE K072D-DT-VNCT-ITMT-FDAR
                                       TO SGTSX005-DT-VNCT-OPR
           MOVE 'CURRENT'              TO SGTSX005-TS-ULT-ATL
           MOVE DT-ATU                 TO SGTSX005-DT-EVT-VCLD-VRS

           COMPUTE SGTSX005-VL-CPR-OPR =
                   K072D-QT-ATI  * K072D-VL-PU-ATI
      
           MOVE LENGTH OF SGTSX005-DADOS   TO EIBCALEN
           CALL  SGTSX005  USING  DFHEIBLK  SGTSX005-DADOS
      
           IF  SGTSX005-CD-RTN-PGM        NOT EQUAL ZEROS
               MOVE SGTSX005-CD-RTN-PGM   TO  CD-RTN-PGM
               MOVE SGTSX005-MSG-ERRO     TO  MSG-ERRO
               PERFORM 999000-ERRO
           END-IF
      *
      *    MOVE SGTSX005-CD-CTRA-TIT-VL   TO  AUX-CD-CTRA-TIT-VL
      *    MOVE SGTSX005-CD-OPR-ETRD-MCP  TO  AUX-CD-OPR-ETRD-MCP
      *    MOVE SGTSX005-NR-VRS-OPR-ETRD  TO  AUX-NR-VRS-OPR-ETRD
           .
       212000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       215000-INCL-REMUN               SECTION.
      *--> Inclui Remuneração da Operação Estruturada
      *-----------------------------------------------------------------
      D    DISPLAY CTE-PROG ' 215000-INCL-REMUN '
      
           MOVE 0007                       TO PSC-ERRO-PGM
      
           MOVE SGTST011                   TO NM-PGM
           SET  SGTST011-INCLUIR           TO TRUE
           MOVE SGTSX005-CD-CTRA-TIT-VL    TO SGTST011-CD-CTRA-TIT-VL
           MOVE SGTSX005-CD-OPR-ETRD-MCP   TO SGTST011-CD-OPR-ETRD-MCP
           MOVE SGTSX005-NR-VRS-OPR-ETRD   TO SGTST011-NR-VRS-OPR-ETRD
           MOVE K072D-PC-IDX-ITMT          TO SGTST011-PC-TAXA-RMNC-OPR
           MOVE K072D-CD-IEC               TO SGTST011-CD-IEC-RMNC
           MOVE K072D-CD-TIP-PERC-EVT-JUR  TO SGTST011-CD-PERC-CPTZ-JUR
           MOVE SPACES                     TO SGTST011-DT-FIM-TAXA-RMNC
           MOVE 'SGTPBA72'                 TO SGTST011-CD-CHV-USU
                                              SGTST011-CD-USU-RSP-ATL
           MOVE 1                          TO SGTST011-CD-TIP-FMA-APL
           MOVE 'CURRENT'                  TO SGTST011-TS-ULT-ATL
           MOVE K072D-DT-LQDC              TO SGTST011-DT-INC-TAXA-RMNC
      *
           MOVE LENGTH OF SGTST011-DADOS   TO EIBCALEN
           CALL SGTST011  USING  DFHEIBLK  SGTST011-DADOS
      
           IF  SGTST011-CD-RTN-PGM         NOT EQUAL ZEROS
               MOVE SGTST011-CD-RTN-PGM    TO  CD-RTN-PGM
               MOVE SGTST011-MSG-ERRO      TO  MSG-ERRO
               PERFORM 999000-ERRO
           END-IF
           .
       215000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       216000-INCL-CLIENTE             SECTION.
      *--> Inclui Cliente da Operação Estruturada
      *-----------------------------------------------------------------
      D    DISPLAY CTE-PROG ' 216000-INCL-CLIENTE '
      
           MOVE 0008                       TO PSC-ERRO-PGM
      
           MOVE SGTST006                   TO NM-PGM
           SET  SGTST006-INCLUIR           TO TRUE
           MOVE SGTSX005-CD-CTRA-TIT-VL    TO SGTST006-CD-CTRA-TIT-VL
           MOVE SGTSX005-CD-OPR-ETRD-MCP   TO SGTST006-CD-OPR-ETRD-MCP
           MOVE SGTSX005-NR-VRS-OPR-ETRD   TO SGTST006-NR-VRS-OPR-ETRD
           MOVE K072D-CD-MCI-IFLC          TO SGTST006-CD-CLI
           MOVE 2                          TO SGTST006-CD-TIP-FUC-CLI
           MOVE SPACES                     TO SGTST006-DT-FIM-VLD-FUC
           MOVE 'SGTPBA72'                 TO SGTST006-CD-CHV-USU
                                              SGTST006-CD-USU-RSP-ATL
           MOVE 'CURRENT'                  TO SGTST006-TS-ULT-ATL
      
           MOVE LENGTH OF SGTST006-DADOS   TO EIBCALEN
           CALL SGTST006  USING  DFHEIBLK  SGTST006-DADOS
      
           IF  SGTST006-CD-RTN-PGM         NOT EQUAL ZEROS
               MOVE SGTST006-CD-RTN-PGM    TO  CD-RTN-PGM
               MOVE SGTST006-MSG-ERRO      TO  MSG-ERRO
               PERFORM 999000-ERRO
           END-IF
           .
       216000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       220000-ATLZ-TRANCHE             SECTION.
      *----------------------------------------------------------------*
      D    DISPLAY CTE-PROG ' 220000-ATLZ-TRANCHE '
      
           MOVE  0009                  TO  PSC-ERRO-PGM
      
           IF AUX-ERRO NOT EQUAL 'S'
              PERFORM 221000-ALTER-OPR-SGT
           END-IF
      
           IF AUX-ERRO NOT EQUAL 'S'
              PERFORM 222000-INCL-EST-OPR
           END-IF
      
           IF AUX-ERRO NOT EQUAL 'S'
              PERFORM 223000-INCL-EST-VRS-OPR
           END-IF
           .
       220000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       221000-ALTER-OPR-SGT            SECTION.
      *--> Alterar Operação Estruturada
      *-----------------------------------------------------------------
      D    DISPLAY CTE-PROG ' 221000-ALTER-OPR-SGT '
      
           MOVE 0010                       TO PSC-ERRO-PGM
      
           MOVE SGTSX005                   TO NM-PGM
           SET  SGTSX005-CONSULTAR-OPR-POR-TIT
                                           TO TRUE
           MOVE 200000                     TO SGTSX005-CD-CTRA-TIT-VL
           MOVE K072D-CD-ITMT-FNCO-CLRG    TO SGTSX005-CD-TIT-OPR-ETRD
           MOVE NR-DVS-TIT(1)              TO SGTSX005-NR-DVS-TIT
      *
           MOVE LENGTH OF SGTSX005-DADOS   TO EIBCALEN
           CALL SGTSX005  USING  DFHEIBLK  SGTSX005-DADOS
      
           IF  SGTSX005-CD-RTN-PGM         NOT EQUAL ZEROS
               MOVE SGTSX005-CD-RTN-PGM    TO  CD-RTN-PGM
               MOVE SGTSX005-MSG-ERRO      TO  MSG-ERRO
               PERFORM 999000-ERRO
           END-IF
      
           MOVE 0011                       TO PSC-ERRO-PGM
           SET  SGTSX005-ALTERAR           TO TRUE
           MOVE SGTSX005                   TO NM-PGM
      
           MOVE K072D-CD-UOR               TO SGTSX005-CD-UOR-BB
           MOVE 1                          TO SGTSX005-NR-VRS-OPR-ETRD
                                              SGTSX005-NR-CC-BB
           MOVE K072D-VL-PU-ATI
                                           TO SGTSX005-VL-PU-CPR-TIT-OPR
           MOVE 'SGTPBA72'                 TO SGTSX005-CD-USU-ULT-ATL
           MOVE K072D-DT-LQDC              TO SGTSX005-DT-CPR-OPR
           MOVE K072D-CD-PRD               TO SGTSX005-CD-PRD
           MOVE K072D-CD-MDLD              TO SGTSX005-CD-MDLD
           MOVE K072D-QT-ATI               TO SGTSX005-QT-PAPL-TIT-CPR
           MOVE K072D-CD-GR-MC-CPTL        TO SGTSX005-CD-GR-MCP
           MOVE K072D-CD-LMCP              TO SGTSX005-CD-LVRO-MC-CPTL
           MOVE K072D-CD-SMCP              TO SGTSX005-CD-SMCP
           MOVE K072D-CD-TIP-CLSC-CTB      TO SGTSX005-CD-CTGR-TVM
           MOVE K072D-CD-TIP-VRS-OPR       TO SGTSX005-CD-TIP-VRS-OPR
           MOVE K072D-CD-ESG-NEG           TO SGTSX005-CD-ESG-NEGL
           MOVE 18910                      TO SGTSX005-CD-UOR-DTTR-OPR
           MOVE K072D-DT-VNCT-ITMT-FDAR    TO SGTSX005-DT-VNCT-OPR
           MOVE 'CURRENT'                  TO SGTSX005-TS-ULT-ATL
           MOVE DT-ATU                     TO SGTSX005-DT-EVT-VCLD-VRS

           COMPUTE SGTSX005-VL-CPR-OPR =
                   K072D-QT-ATI  * K072D-VL-PU-ATI
      
           MOVE LENGTH OF SGTSX005-DADOS   TO EIBCALEN
           CALL  SGTSX005  USING  DFHEIBLK SGTSX005-DADOS
      
           IF  SGTSX005-CD-RTN-PGM         NOT EQUAL ZEROS
               MOVE SGTSX005-CD-RTN-PGM    TO  CD-RTN-PGM
               MOVE SGTSX005-MSG-ERRO      TO  MSG-ERRO
               PERFORM 999000-ERRO
           END-IF
           .
       221000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       222000-INCL-EST-OPR             SECTION.
      *--> Inclui Estado da Operação Estruturada
      *-----------------------------------------------------------------
      D    DISPLAY CTE-PROG ' 222000-INCL-EST-OPR '
      
           MOVE 0012                       TO PSC-ERRO-PGM
      
      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE SGTST009                   TO  NM-PGM
      
      *--> Movimentar os campos de entrada
      
           SET  SGTST009-INCLUIR           TO  TRUE
           MOVE SGTSX005-CD-CTRA-TIT-VL    TO  SGTST009-CD-CTRA-TIT-VL
           MOVE SGTSX005-CD-OPR-ETRD-MCP   TO  SGTST009-CD-OPR-ETRD-MCP
           MOVE SGTSX005-NR-VRS-OPR-ETRD   TO  SGTST009-NR-VRS-OPR-ETRD
      
      *    Estado 5 = encarteirada
           MOVE 5                          TO  SGTST009-CD-TIP-EST-OPR
           MOVE 'CURRENT'                  TO  SGTST009-TS-EST-OPR-ETRD
           MOVE 'SGTPBA72'                 TO  SGTST009-CD-USU-RSP-EST
                                               SGTST009-CD-USU-RSP-ATL
      *
      *
           MOVE LENGTH OF SGTST009-DADOS   TO EIBCALEN
           CALL SGTST009  USING  DFHEIBLK  SGTST009-DADOS
      
           IF  SGTST009-CD-RTN-PGM         NOT EQUAL ZEROS
               MOVE SGTST009-CD-RTN-PGM    TO  CD-RTN-PGM
               MOVE SGTST009-MSG-ERRO      TO  MSG-ERRO
               PERFORM 999000-ERRO
           END-IF
           .
       222000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       223000-INCL-EST-VRS-OPR         SECTION.
      *--> Inclui Estado da Versão da Operação Estruturada
      *-----------------------------------------------------------------
      D    DISPLAY CTE-PROG ' 223000-INCL-EST-VRS-OPR '
      
      *--> Desligar erro automatico do DB2
           EXEC SQL WHENEVER SQLERROR
                    CONTINUE
           END-EXEC
      
           EXEC SQL
                INSERT INTO  DB2SGT.EST_VRS_OPR_ETRD
                           ( CD_CTRA_TIT_VL
                           , CD_OPR_ETRD_MCP
                           , NR_VRS_OPR_ETRD
                           , TS_EST_VRS
                           , CD_USU_ULT_ATL
                           , CD_TIP_EST_VRS
      *                    , TX_OBS_EST_VRS
                           )
                VALUES
                           ( :SGTSX005-CD-CTRA-TIT-VL
                           , :SGTSX005-CD-OPR-ETRD-MCP
                           , :SGTSX005-NR-VRS-OPR-ETRD
                           , :TS-FMTD
                           , :AUX-CD-CHV-USU
                           , :CD-TIP-EST-VRS
                           )
           END-EXEC
      
      *--> Ligar erro automatico do DB2
      *    EXEC SQL WHENEVER SQLERROR
      *             GO TO 999002-ERRO-DB2
      *    END-EXEC
      
           EVALUATE  SQLCODE
               WHEN  -803
                    MOVE  803              TO  CD-RTN-PGM
                    MOVE  'Inclusão não efetuada, registro existente.'
                                           TO  MSG-ERRO
                    PERFORM 999000-ERRO
               WHEN  ZEROS
                     CONTINUE
      
               WHEN  OTHER
      *--> Aciona o Tratamento de Erro Padrao para os Demais Casos
                     CONTINUE
      *              PERFORM 999002-ERRO-DB2
      *              DISPLAY CTE-PROG ' DEU ERRO 223000'
           END-EVALUATE
      
           MOVE 0014                   TO PSC-ERRO-PGM
           INITIALIZE                  DADOS-LOG
           SET   SGTS014G-INCLUSAO     TO  TRUE
      
           MOVE  'EST_VRS_OPR_ETRD  '  TO  SGTS014G-NM-ENT
           MOVE  AUX-CD-CHV-USU        TO  SGTS014G-CD-CHV-USU
           MOVE  'SGTPBA72'            TO  SGTS014G-NM-PGM
      
           MOVE  200000                TO  CD-CTRA-TIT-VL-LOG
           MOVE  SGTSX005-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP-LOG
           MOVE  SGTSX005-NR-VRS-OPR-ETRD
                                       TO  NR-VRS-OPR-ETRD-LOG
           MOVE  TS-FMTD               TO  TS-EST-VRS-LOG
           STRING  CD-CTRA-TIT-VL-LOG      ';'
                   CD-OPR-ETRD-MCP-LOG     ';'
                   NR-VRS-OPR-ETRD-LOG     ';'
                   TS-EST-VRS-LOG
                   DELIMITED BY SIZE
                   INTO                    SGTS014G-CHV-PRMR
      
           PERFORM 781000-GRAVAR-LOG
           .
       223000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       300000-MONTA-SAIDA              SECTION.
      *----------------------------------------------------------------*
      D    DISPLAY CTE-PROG ' 300000-MONTA-SAIDA '
      
           MOVE  0015                  TO  PSC-ERRO-PGM
      
           MOVE TS-FMTD                TO  SGTPB136-DT-PRCT
           PERFORM 310000-BUSCA-NR-SEQUENCIA
           MOVE K072D-DT-LQDC          TO  SGTPB136-DT-CPR
           MOVE K072D-CD-ITMT-FNCO-CLRG
                                       TO  SGTPB136-CD-ITMT-FNCO
           MOVE K072D-VL-PU-ATI        TO  SGTPB136-VL-PU-CPR
           MOVE 1                      TO  SGTPB136-CD-TIP-PRCT
           MOVE 2                      TO  SGTPB136-CD-EST-PRCT
           MOVE TS-FMTD                TO  SGTPB136-TS-PRCT
           MOVE 'SGTPBA72'             TO  SGTPB136-CD-PGM
           MOVE CD-CTRA-TIT-VL  (1)    TO  SGTPB136-CD-CTRA-TIT-VL
           MOVE 0                      TO  SGTPB136-CD-CTRA-TIT-VL-N
           MOVE CD-OPR-ETRD-MCP (1)    TO  SGTPB136-CD-OPR-ETRD-MCP
           MOVE 0                      TO  SGTPB136-CD-OPR-ETRD-MCP-N
           MOVE NR-VRS-OPR-ETRD (1)    TO  SGTPB136-NR-VRS-OPR-ETRD
           MOVE 0                      TO  SGTPB136-NR-VRS-OPR-ETRD-N
           MOVE 'SYSTEM'               TO  SGTPB136-CD-USU
           MOVE 2                      TO  SGTPB136-CD-OGM-ERRO
           MOVE 0                      TO  SGTPB136-CD-OGM-ERRO-N
           MOVE MSG-ERRO               TO  SGTPB136-TX-ERRO-PRCT
           MOVE 0                      TO  SGTPB136-TX-ERRO-PRCT-N
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       310000-BUSCA-NR-SEQUENCIA       SECTION.
      *-----------------------------------------------------------------
      D    DISPLAY CTE-PROG ' 310000-BUSCA-NR-SEQUENCIA '
      
           MOVE 0016                   TO PSC-ERRO-PGM
      
           EXEC SQL
                SELECT VALUE (MAX(NR_SEQL_PRCT), 0 )
                  INTO :SGTPB136-NR-SEQL-PRCT
                  FROM DB2SGT.PRCT_CTRA_TSR
                 WHERE DT_PRCT = :SGTPB136-DT-PRCT
           END-EXEC
      
      * MOCK-POINT MCKV4
           EVALUATE SQLCODE
               WHEN 0
                    CONTINUE
               WHEN OTHER
                     CONTINUE
      *             PERFORM 999002-ERRO-DB2
      *              DISPLAY CTE-PROG ' DEU ERRO 310000 '
           END-EVALUATE
      
           ADD 1                       TO SGTPB136-NR-SEQL-PRCT
           .
       310000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       770101-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
      D    DISPLAY CTE-PROG ' 770101-ABRE-CURSOR-PSQ '
      *
           MOVE  0017                  TO  PSC-ERRO-PGM
      *
           EXEC SQL
                OPEN CURSOR-PSQ-1
           END-EXEC
      
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
           .
       770101-SAI.
           EXIT.
      *-----------------------------------------------------------------
       770102-LER-CURSOR-PSQ           SECTION.
      *-----------------------------------------------------------------
      D    DISPLAY CTE-PROG ' 770102-LER-CURSOR-PSQ '
      *
           MOVE  0018                  TO  PSC-ERRO-PGM
      *
            EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ-1  FOR 0101 ROWS
               INTO   :CD-CTRA-TIT-VL
                    , :CD-OPR-ETRD-MCP
                    , :NR-VRS-OPR-ETRD
                    , :DT-CPR-OPR
                      :DT-CPR-OPR-N
                    , :CD-TIT-OPR-ETRD
                      :CD-TIT-OPR-ETRD-N
                    , :NR-DVS-TIT
                      :NR-DVS-TIT-N
                    , :VL-PU-CPR-TIT-OPR
           END-EXEC
      
           MOVE SQLERRD(3)             TO QT-REG-SELD
      
           IF  QT-REG-SELD EQUAL 0101
               MOVE 0100               TO QT-REG-SELD
           END-IF
      
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       770102-SAI.
           EXIT.
      *-----------------------------------------------------------------
       770103-FECHA-CURSOR-PSQ         SECTION.
      *-----------------------------------------------------------------
      D    DISPLAY CTE-PROG ' 770103-FECHA-CURSOR-PSQ '
      *
           MOVE  0019                  TO  PSC-ERRO-PGM
      *
           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
      
           EXEC SQL
                CLOSE CURSOR-PSQ-1
           END-EXEC
           .
       770103-SAI.
           EXIT.
      *-----------------------------------------------------------------
       780000-BUSCA-TS-DT              SECTION.
      *-----------------------------------------------------------------
      D    DISPLAY CTE-PROG ' 780000-BUSCA-TS-DT '
      
           MOVE 0020                   TO PSC-ERRO-PGM
      
           CALL SBTIMEDT            USING HLPKE001-CD-FUC
                                          HLPKE001-AREA-TRB
                                          HLPKE001-FMT-TS
                                          HLPKE001-TAM-TS
                                          HLPKE001-TS-FMTD
      
           IF  RETURN-CODE      NOT EQUAL ZEROS
               SET   ERRO-DE-PGM       TO  TRUE
               MOVE  0050              TO  CD-RTN-PGM
               MOVE  'SBTIMEDT'        TO  PGM-ERRO
               MOVE 'Erro ao obter data e hora atual.' TO MSG-ERRO
               PERFORM 999000-ERRO
           END-IF
      
           MOVE HLPKE001-TS-FMTD       TO TS-FMTD
           .
       780000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       781000-GRAVAR-LOG               SECTION.
      *    Gravar LOG das  inclusões efetuadas nesse módulo
      *    Movimentações dos campos feitas  nas respectivas sections
      *-----------------------------------------------------------------
      D    DISPLAY CTE-PROG ' 781000-GRAVAR-LOG '
      
           MOVE  0021                  TO  PSC-ERRO-PGM
      
           MOVE  LENGTH OF DADOS-LOG   TO  EIBCALEN
           CALL  SGTS014G           USING  DFHEIBLK DADOS-LOG
      
           IF  SGTS014G-CD-RTN-PGM     NOT EQUAL  ZEROS
               MOVE  112               TO  CD-RTN-PGM
               MOVE  SGTS014G-MSG-ERRO-PGM
                                       TO  MSG-ERRO
               MOVE  SGTS014G-CD-RTN-PGM
                                       TO  CD-RTN-PGM
               PERFORM 999000-ERRO
           END-IF
           .
       781000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       800000-FIM                      SECTION.
      *----------------------------------------------------------------*
      D    DISPLAY CTE-PROG ' 800000-FIM '
      
           MOVE  0022                  TO  PSC-ERRO-PGM
      
           CLOSE SGTFB72E
                 SGTF136S
      
      *    PERFORM 900900-CONTROLA-COMMIT
      
           DISPLAY '*-------------------------------------------------*'
           DISPLAY '*- FIM DO PROCESSAMENTO                          -*'
           DISPLAY '*-------------------------------------------------*'
           DISPLAY ' '
           MOVE CNT-LIDOS              TO DIS-CODE
           DISPLAY ' QT REGISTROS LIDOS               : ' DIS-CODE
           MOVE CNT-CHAVE              TO DIS-CODE
           DISPLAY ' QT DE CD-ITMT-FNCO-FDAR LIDOS    : ' DIS-CODE
      *    MOVE CNT-GRAVADOS           TO DIS-CODE
      *    DISPLAY ' QT DE REGISTROS GRAVADOS         : ' DIS-CODE
           MOVE CNT-INCD               TO DIS-CODE                      VRS01B
           DISPLAY ' QT DE TRANCHES INCLUIDAS         : ' DIS-CODE      VRS01B
           MOVE CNT-ALTD               TO DIS-CODE                      VRS01B
           DISPLAY ' QT DE TRANCHES ALTERADAS         : ' DIS-CODE      VRS01B
           MOVE CNT-GRAVADOS           TO DIS-CODE
           DISPLAY ' QT DE REGISTROS COM ERRO         : ' DIS-CODE
           DISPLAY '888 ' CTE-PROG ' Código           : ' RETURN-CODE
           DISPLAY ' '
           DISPLAY '***************************************************'
           .
       800000-SAI.
           EXIT.
      *---------------------------------------*
       900900-CONTROLA-COMMIT          SECTION.
      *---------------------------------------*
      D    DISPLAY CTE-PROG ' 900900-CONTROLA-COMMIT '
      
           CALL DB2S0001 USING DB2S0001-LINK.
           IF  NOT DB2S0001-FIM-NORMAL
               PERFORM 900901-ERRO-COMMIT
           END-IF
           .
       900900-FIM.
           EXIT.
      *----------------------------------------------------------------*
       900901-ERRO-COMMIT              SECTION.
      *----------------------------------------------------------------*
      D    DISPLAY CTE-PROG ' 900901-ERRO-COMMIT '
      
           DISPLAY '888 ' CTE-PROG ' 006 - ERRO CHAMADA DB2S0001 --- '
                                            UPON SYSOUT.
           DISPLAY '888 ' CTE-PROG ' 006 - ERRO DB2S0001             '
                                            UPON SYSOUT.
           DISPLAY '888 ' CTE-PROG ' 006 - DB2S0001-RETORNO        = '
                   DB2S0001-RETORNO         UPON SYSOUT.
           DISPLAY '888 ' CTE-PROG ' 006 - DB2S0001-SQLCODE        = '
                   DB2S0001-SQLCODE         UPON SYSOUT.
           DISPLAY '888 ' CTE-PROG ' 006 - DB2S0001-MENSAGEM       = '
                   DB2S0001-MENSAGEM        UPON SYSOUT.
      
           CALL SBABEND
           .
       900901-SAI.
           EXIT.
      *--------------------
       999000-ERRO                     SECTION.
      *--------------------
      D    DISPLAY CTE-PROG ' 999000-ERRO '
      
           EXEC SQL
                ROLLBACK
           END-EXEC
      *--------------------
           MOVE 'S'                TO AUX-ERRO
           PERFORM   300000-MONTA-SAIDA
           WRITE  REG-FD-SGTF136S  FROM SGTP136S-DADOS
             ADD   +1              TO CNT-GRAVADOS
                ON SIZE ERROR
                   MOVE 'ERRO EM OPERACAO ARITMETICA 5'
                                   TO MSG-ERRO
      *            PERFORM   999000-ERRO
             END-ADD
      *--------------------
           .
       999000-ERRO-SAI.
           EXIT.
      *-----------------
      * 999000-ERRO.
      *-----------------
      *    -INC HLPKE004
      *
      *    IF  ERRO-CODIFICADO
      *        PERFORM 999082-RECUPERA-MSG-CODIFICADA
      *    END-IF
      *
      *    DISPLAY  '888'  CTE-PROG 'CANCELADO POR'
      *    DISPLAY  '888'  CTE-PROG  MSG-ERRO-PGM
      *    DISPLAY  '888'  CTE-PROG 'PROGRAMA:' PGM-ERRO
      *    DISPLAY HLPS0200-TX-MSG-ITNO
      *    DISPLAY  '888'  CTE-PROG 'CD-RTN :'  CD-RTN-PGM
      *    DISPLAY  '888'  CTE-PROG 'TIP-RTN :' TIP-RTN-PGM
      *    DISPLAY  '888'  CTE-PROG 'PSC-ERRO:' PSC-ERRO-PGM
      *    DISPLAY  '888'  CTE-PROG 'MAIS INFORMACOES VIDE DUMP FORMATA
      *        'DO.'
      *
      *    CALL SBABEND
      *    .
      *999009-SAI.
      *    EXIT.
      *-----------------
      * 999002-ERRO-DB2
      *-----------------
      *   -INC HLPKE005
      
      *------------------------ FIM SGTPBA72 --------------------------
