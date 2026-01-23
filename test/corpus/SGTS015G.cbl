      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS015G.
      * ANALISTA..: F2020988 - CHRISTINA C. G. R. RIBEIRO
      * AUTOR.....: F2020988 - CHRISTINA C. G. R. RIBEIRO
      * DATA......: 28.08.2018.
      * OBJETIVO..: GERAR LOG DO SISTEMA SGT - BASILEIA
      * COMPILACAO: 2 - PSOSE210 - Cobol 4.2 S/ OTIMIZACAO
      *
      *-----------------------------------------------------------------
      * TABELAS CONTEMPLADAS NESSE PROGRAMA:
      * ATI_FNCO_ANL_RSCO
      * CTRA_SIS_DRIV
      * REG_EST_DRIA_ATI
      * RGR_AGPT_ATI_FNCO
      * AGPT_ATI_FNCO
      * PNC_CFMD_ATI
      * GRT_ATI_FNCO
      * PNC_GRT_ATI_FNCO
      * STIT_ATI_FNCO
      * PNC_STIT_ATI_FNCO
      * CLI_STIT_ATI_FNCO
      * HST_AGPT_ATI_FNCO
      * HST_CTRA_SIS_DRIV
      * HST_GRT_ATI_FNCO
      * HST_RGR_AGPT_ATI
      * HST_STIT_ATI_FNCO
      *-----------------------------------------------------------------
      * PROGRAMA GERADO AUTOMATICAMENTE VERSAO GERADOR : V001
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
      * VRS001 28.08.2018 - F2020988 - DE 2016/145195 - IMPLANTACAO.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS015G.

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
       01  CTE-INICIAL  PIC  X(40) VALUE '*** W.S.S. COMECA AQUI ***'.
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTS015G ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * Area de trabalho

       01  QT-REG-SELD                      PIC S9(04) COMP-5 VALUE 0.

      * Indicadores de nulidades

       01  QT-REG-SELD-NULO                 PIC S9(04) COMP-5 VALUE 0.
      * Variaveis utilizadas no programa

       01  LEN                              PIC S9(04) COMP-5 VALUE 0.
       01  IND-CHAVE                        PIC  9(04)    VALUE 0.
       01  TX-DADO                          PIC  X(4000)  VALUE SPACES.
       01  TX-LOG                           PIC  X(1000)  VALUE SPACES.
       01  CHAVE01                          PIC  X(50)    VALUE SPACES.
       01  CHAVE02                          PIC  X(50)    VALUE SPACES.
       01  CHAVE03                          PIC  X(50)    VALUE SPACES.
       01  CHAVE04                          PIC  X(50)    VALUE SPACES.
       01  CHAVE05                          PIC  X(50)    VALUE SPACES.
       01  CHAVE06                          PIC  X(50)    VALUE SPACES.
       01  CHAVE07                          PIC  X(50)    VALUE SPACES.
       01  CHAVE08                          PIC  X(50)    VALUE SPACES.
       01  CHAVE09                          PIC  X(50)    VALUE SPACES.
       01  CHAVE10                          PIC  X(50)    VALUE SPACES.
       01  CHAVE11                          PIC  X(50)    VALUE SPACES.
       01  CHAVE12                          PIC  X(50)    VALUE SPACES.
       01  CHAVE13                          PIC  X(50)    VALUE SPACES.
       01  CHAVE14                          PIC  X(50)    VALUE SPACES.
       01  CHAVE15                          PIC  X(50)    VALUE SPACES.
       01  NM-TAB                           PIC  X(20)    VALUE SPACES.

      * Areas das tabelas utilizadas

      * area da tabela ==> ATI_FNCO_ANL_RSCO
       01  S1SGT099.
           03 S1SGT099-CD-CTRA-SIS-DRIV       PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT099-NR-SEQL-ATI            PIC S9(9) COMP-5 VALUE 0.
      * area da tabela ==> CTRA_SIS_DRIV
       01  S1SGT100.
           03 S1SGT100-CD-CTRA-SIS-DRIV       PIC S9(9) COMP-5 VALUE 0.
      * area da tabela ==> REG_EST_DRIA_ATI
       01  S1SGT101.
           03 S1SGT101-CD-CTRA-SIS-DRIV       PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT101-NR-SEQL-ATI            PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT101-DT-EST-ATI             PIC X(10) VALUE ' '.
           03 S1SGT101-NR-SEQL-REG-EST        PIC S9(4) COMP-5 VALUE 0.
      * area da tabela ==> RGR_AGPT_ATI_FNCO
       01  S1SGT102.
           03 S1SGT102-CD-AGPT-ATI-FNCO       PIC S9(9) COMP-5 VALUE 0.
      * area da tabela ==> AGPT_ATI_FNCO
       01  S1SGT103.
           03 S1SGT103-CD-CTRA-SIS-DRIV       PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT103-NR-SEQL-ATI            PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT103-CD-AGPT-ATI-FNCO       PIC S9(9) COMP-5 VALUE 0.
      * area da tabela ==> PNC_CFMD_ATI
       01  S1SGT104.
           03 S1SGT104-CD-CTRA-SIS-DRIV       PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT104-NR-SEQL-ATI            PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT104-TS-PNC-CFMD            PIC X(26) VALUE ' '.
      * area da tabela ==> GRT_ATI_FNCO
       01  S1SGT105.
           03 S1SGT105-CD-CTRA-SIS-DRIV       PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT105-NR-SEQL-ATI            PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT105-CD-STIT-ATI-FNCO       PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT105-NR-GRT-ATI             PIC S9(9) COMP-5 VALUE 0.
      * area da tabela ==> PNC_GRT_ATI_FNCO
       01  S1SGT106.
           03 S1SGT106-CD-CTRA-SIS-DRIV       PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT106-NR-SEQL-ATI            PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT106-TS-PNC-CFMD            PIC X(26) VALUE ' '.
      * area da tabela ==> STIT_ATI_FNCO
       01  S1SGT107.
           03 S1SGT107-CD-CTRA-SIS-DRIV       PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT107-NR-SEQL-ATI            PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT107-CD-STIT-ATI-FNCO       PIC S9(9) COMP-5 VALUE 0.
      * area da tabela ==> PNC_STIT_ATI_FNCO
       01  S1SGT108.
           03 S1SGT108-CD-CTRA-SIS-DRIV       PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT108-NR-SEQL-ATI            PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT108-TS-PNC-CFMD            PIC X(26) VALUE ' '.
      * area da tabela ==> CLI_STIT_ATI_FNCO
       01  S1SGT109.
           03 S1SGT109-CD-CTRA-SIS-DRIV       PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT109-NR-SEQL-ATI            PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT109-CD-STIT-ATI-FNCO       PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT109-CD-TIP-FUC-CLI         PIC S9(4) COMP-5 VALUE 0.
      * area da tabela ==> HST_AGPT_ATI_FNCO
       01  S1SGT110.
           03 S1SGT110-CD-CTRA-SIS-DRIV       PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT110-NR-SEQL-ATI            PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT110-CD-AGPT-ATI-FNCO       PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT110-TS-HST-AGPT-ATI        PIC X(26) VALUE ' '.
      * area da tabela ==> HST_CTRA_SIS_DRIV
       01  S1SGT111.
           03 S1SGT111-CD-CTRA-SIS-DRIV       PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT111-TS-HST-CTRA            PIC X(26) VALUE ' '.
      * area da tabela ==> HST_GRT_ATI_FNCO
       01  S1SGT112.
           03 S1SGT112-CD-CTRA-SIS-DRIV       PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT112-NR-SEQL-ATI            PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT112-CD-STIT-ATI-FNCO       PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT112-NR-GRT-ATI             PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT112-TS-HST-GRT-ATI         PIC X(26) VALUE ' '.
      * area da tabela ==> HST_RGR_AGPT_ATI_FNCO
       01  S1SGT113.
           03 S1SGT113-CD-AGPT-ATI-FNCO       PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT113-TS-HST-RGR-AGPT        PIC X(26) VALUE ' '.
      * area da tabela ==> HST_STIT_ATI_FNCO
       01  S1SGT114.
           03 S1SGT114-CD-CTRA-SIS-DRIV       PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT114-NR-SEQL-ATI            PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT114-CD-STIT-ATI-FNCO       PIC S9(9) COMP-5 VALUE 0.
           03 S1SGT114-TS-HST-STIT-ATI        PIC X(26) VALUE ' '.


      * Subrotinas Utilizadas

      * Books da definicao de dados a serem usados na subrotina de erros
-INC HLPKE001

-INC HLPKE011

       01  FILLER              PIC X(40) VALUE
           '*** SGTS015G *** TERMINA AQUI ***'.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTK015G
      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------
           IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               MOVE 3001               TO  CD-RTN-PGM
               MOVE 'Area com tamanho invalido - verifique book/local'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF
      * WHENEVER DB2
-INC HLPKE002

           PERFORM 100000-VALIDAR-PARAMETRO
           PERFORM 200000-CONSISTE-CHAVE
           PERFORM 300000-LEITURA-TABELAS

           GOBACK
           .
      *-----------------------------------------------------------------
       100000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO PSC-ERRO-PGM

           IF  SGTS015G-NM-ENT = SPACES
               MOVE 0285               TO  CD-RTN-PGM
               MOVE 'Nome tabela nao informada' TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTS015G-CD-TIP-ACAO = SPACES
               MOVE 0286               TO  CD-RTN-PGM
               MOVE 'Tipo da acao nao informada' TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTS015G-CHV-PRMR = SPACES
               MOVE 0287               TO  CD-RTN-PGM
               MOVE 'Chave primaria nao informada' TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTS015G-NM-PGM  = SPACES
               MOVE 0288               TO  CD-RTN-PGM
               MOVE 'Nome programa nao informada' TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTS015G-CD-TIP-ACAO NOT EQUAL 'A'
           AND SGTS015G-CD-TIP-ACAO NOT EQUAL 'E'
           AND SGTS015G-CD-TIP-ACAO NOT EQUAL 'I'
               MOVE 0289               TO  CD-RTN-PGM
               MOVE 'Informe a acao correta' TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           .
       100000-SAI-VALIDAR-PARAMETRO.
           EXIT.
      *-----------------------------------------------------------------
       200000-CONSISTE-CHAVE           SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO PSC-ERRO-PGM

           MOVE ZEROS                 TO IND-CHAVE
           INSPECT SGTS015G-CHV-PRMR TALLYING  IND-CHAVE FOR ALL ';'
           ADD 1                       TO IND-CHAVE
           UNSTRING SGTS015G-CHV-PRMR DELIMITED BY ';' INTO
           CHAVE01 CHAVE02 CHAVE03 CHAVE04 CHAVE05
           CHAVE06 CHAVE07 CHAVE08 CHAVE09 CHAVE10
           CHAVE11 CHAVE12 CHAVE13 CHAVE14 CHAVE15
           END-UNSTRING

           IF  SGTS015G-NM-ENT = 'RGR_AGPT_ATI_FNCO'
           OR  SGTS015G-NM-ENT = 'CTRA_SIS_DRIV'
               IF  IND-CHAVE NOT EQUAL 0001
                   MOVE 0290                     TO CD-RTN-PGM
                   MOVE 'Chave nao esta correta' TO MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF

           IF  SGTS015G-NM-ENT = 'HST_CTRA_SIS_DRIV'
           OR  SGTS015G-NM-ENT = 'HST_RGR_AGPT_ATI'
           OR  SGTS015G-NM-ENT = 'ATI_FNCO_ANL_RSCO'
               IF  IND-CHAVE NOT EQUAL 0002
                   MOVE 0290                     TO CD-RTN-PGM
                   MOVE 'Chave nao esta correta' TO MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF

           IF  SGTS015G-NM-ENT = 'AGPT_ATI_FNCO'
           OR  SGTS015G-NM-ENT = 'PNC_CFMD_ATI'
           OR  SGTS015G-NM-ENT = 'PNC_GRT_ATI_FNCO'
           OR  SGTS015G-NM-ENT = 'STIT_ATI_FNCO'
           OR  SGTS015G-NM-ENT = 'PNC_STIT_ATI_FNCO'
               IF  IND-CHAVE NOT EQUAL 0003
                   MOVE 0290                     TO CD-RTN-PGM
                   MOVE 'Chave nao esta correta' TO MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF

           IF  SGTS015G-NM-ENT = 'REG_EST_DRIA_ATI'
           OR  SGTS015G-NM-ENT = 'GRT_ATI_FNCO'
           OR  SGTS015G-NM-ENT = 'CLI_STIT_ATI_FNCO'
           OR  SGTS015G-NM-ENT = 'HST_AGPT_ATI_FNCO'
           OR  SGTS015G-NM-ENT = 'HST_STIT_ATI_FNCO'
               IF  IND-CHAVE NOT EQUAL 0004
                   MOVE 0290                     TO CD-RTN-PGM
                   MOVE 'Chave nao esta correta' TO MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF

           IF  SGTS015G-NM-ENT = 'HST_GRT_ATI_FNCO'
               IF  IND-CHAVE NOT EQUAL 0005
                   MOVE 0290                     TO CD-RTN-PGM
                   MOVE 'Chave nao esta correta' TO MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF

           .
       200000-SAI-CONSISTE-CHAVE.
           EXIT.
      *-----------------------------------------------------------------
       300000-LEITURA-TABELAS          SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO PSC-ERRO-PGM
           MOVE ZEROS                  TO IND-CHAVE
           MOVE  SGTS015G-NM-ENT       TO NM-TAB

           EVALUATE SGTS015G-NM-ENT
               WHEN 'REG_EST_DRIA_ATI'
                   PERFORM 401000-REG-EST-DRIA-ATI
               WHEN 'RGR_AGPT_ATI_FNCO'
                   PERFORM 402000-RGR-AGPT-ATI-FNCO
               WHEN 'AGPT_ATI_FNCO'
                   PERFORM 403000-AGPT-ATI-FNCO
               WHEN 'PNC_CFMD_ATI'
                   PERFORM 404000-PNC-CFMD-ATI
               WHEN 'GRT_ATI_FNCO'
                   PERFORM 405000-GRT-ATI-FNCO
               WHEN 'PNC_GRT_ATI_FNCO'
                   PERFORM 406000-PNC-GRT-ATI-FNCO
               WHEN 'STIT_ATI_FNCO'
                   PERFORM 407000-STIT-ATI-FNCO
               WHEN 'PNC_STIT_ATI_FNCO'
                   PERFORM 408000-PNC-STIT-ATI-FNCO
               WHEN 'CLI_STIT_OPR_ETRD'
                   PERFORM 409000-CLI-STIT-ATI-FNCO
               WHEN 'HST_AGPT_ATI_FNCO'
                   PERFORM 410000-HST-AGPT-ATI-FNCO
               WHEN 'HST_CTRA_SIS_DRIV'
                   PERFORM 411000-HST-CTRA-SIS-DRIV
               WHEN 'HST_GRT_ATI_FNCO'
                   PERFORM 412000-HST-GRT-ATI-FNCO
               WHEN 'HST_RGR_AGPT_ATI'
                   PERFORM 413000-HST-RGR-AGPT-ATI
               WHEN 'HST_STIT_ATI_FNCO'
                   PERFORM 414000-HST-STIT-ATI-FNCO
               WHEN 'ATI_FNCO_ANL_RSCO'
                   PERFORM 415000-ATI-FNCO-ANL-RSCO
               WHEN 'CTRA_SIS_DRIV'
                   PERFORM 416000-CTRA-SIS-DRIV

           END-EVALUATE
           .
       300000-SAI-CONSISTE-CHAVE.
           EXIT.
      *-----------------------------------------------------------------
       401000-REG-EST-DRIA-ATI         SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO PSC-ERRO-PGM

           MOVE ZEROS                  TO LEN
           INSPECT CHAVE01 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE01 (1:LEN)         TO
                          S1SGT101-CD-CTRA-SIS-DRIV
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE02 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE02 (1:LEN)         TO
                          S1SGT101-NR-SEQL-ATI
           MOVE ZEROS                   TO LEN
           INSPECT CHAVE03 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE

           MOVE CHAVE03 (1:LEN)                 TO
                          S1SGT101-DT-EST-ATI
           MOVE ZEROS                   TO LEN

           INSPECT CHAVE04 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE04 (1:LEN)         TO
                          S1SGT101-NR-SEQL-REG-EST

           EXEC SQL
              SELECT
            DIGITS(CD_CTRA_SIS_DRIV)
                      || ';' ||
            DIGITS(NR_SEQL_ATI)
                      || ';' ||
            CHAR(DT_EST_ATI)
                      || ';' ||
            DIGITS(NR_SEQL_REG_EST)
                      || ';' ||
            CHAR(CD_USU_ATL_REG)
                      || ';' ||
            CHAR(TS_ATL_REG)
                      || ';' ||
            DIGITS(CD_PRD)
                      || ';' ||
            DIGITS(CD_MDLD)
                      || ';' ||
            DIGITS(QT_PAPL_DT_PSC)
                      || ';' ||
            CHAR(DT_AQS_ATI)
                      || ';' ||
            CHAR(VALUE(DT_VNCT_ATI, '01.01.0001'))
                      || ';' ||
            DIGITS(CD_TIP_ATI)
                      || ';' ||
            CHAR(IN_ATI_SPRO)
                      || ';' ||
            CHAR(IN_ATI_VRJ)
                      || ';' ||
            CHAR(IN_ATI_DSCP)
                      || ';' ||
            CHAR(IN_ATI_REG_BB)
                      || ';' ||
            CHAR(IN_ATI_N_NEGO)
                      || ';' ||
            CHAR(IN_ATI_EXPO_VCLD)
                      || ';' ||
            DIGITS(VL_EMS_PPL)
                      || ';' ||
            CHAR(CD_ISIN_ATI)
                      || ';' ||
            CHAR(CD_ESPC_ATI)
                      || ';' ||
            DIGITS(VL_PU_MC_DT_PSC)
                      || ';' ||
            DIGITS(VL_SDO_TTL_ATZD)
                      || ';' ||
            DIGITS(VL_TAXA_IDX)
                      || ';' ||
            DIGITS(CD_TIP_ALTO)
                      || ';' ||
            DIGITS(VL_CTB_APL_MOEN)
                      || ';' ||
            DIGITS(VL_JUR_ULT_MM)
                      || ';' ||
            DIGITS(VL_AMTZ)
                      || ';' ||
            DIGITS(VL_PVS_APL_MOEN)
                      || ';' ||
            DIGITS(VALUE(CD_CLI_EMT, 0))
                      || ';' ||
            DIGITS(VALUE(CD_CLI_AGT_FDAR, 0))
                      || ';' ||
            DIGITS(CD_EST_REG_ATI)
                      || ';' ||
            DIGITS(VALUE(NR_IDFR_EMT, 0))
                      || ';' ||
            DIGITS(VALUE(NR_IDFR_AGT_FDAR, 0))
                      || ';' ||
            CHAR(VALUE(TX_DCR_TAXA_SIS,' '))
                      || ';' ||
            CHAR(TX_DCR_ATI)
              INTO   :TX-DADO
               FROM DB2SGT.REG_EST_DRIA_ATI
               WHERE CD_CTRA_SIS_DRIV               =
                     :S1SGT101-CD-CTRA-SIS-DRIV
                 AND NR_SEQL_ATI                    =
                     :S1SGT101-NR-SEQL-ATI
                 AND DT_EST_ATI                     =
                     :S1SGT101-DT-EST-ATI
                 AND NR_SEQL_REG_EST                =
                     :S1SGT101-NR-SEQL-REG-EST
           END-EXEC
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE 0291                         TO CD-RTN-PGM
               MOVE 'Dados da tabela: REG-EST_DRIA-ATI nao cadastrado'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GRAVA-LOG
           .
       401000-SAI-REG-EST-DRIA-ATI.
           EXIT.
      *-----------------------------------------------------------------
       402000-RGR-AGPT-ATI-FNCO         SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO PSC-ERRO-PGM

           MOVE ZEROS                  TO LEN
           INSPECT CHAVE01 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE01 (1:LEN)         TO
                          S1SGT102-CD-AGPT-ATI-FNCO

           EXEC SQL
              SELECT
            DIGITS(CD_AGPT_ATI_FNCO)
                      || ';' ||
            DIGITS(CD_RBC_CTB_PATL)
                      || ';' ||
            DIGITS(CD_RBC_MTM_PST)
                      || ';' ||
            DIGITS(CD_RBC_MTM_NGT)
                      || ';' ||
            CHAR(IN_EXPO_STZC)
                      || ';' ||
            CHAR(DT_INC_VGC)
                      || ';' ||
            CHAR(DT_FIM_VGC)
                      || ';' ||
            DIGITS(CD_EST_AGPT_ATI)
                      || ';' ||
            CHAR(CD_USU_MNTC_AGPT)
                      || ';' ||
            CHAR(VALUE(CD_USU_CNFC_AGPT,' '))
                      || ';' ||
            CHAR(CD_PGM_AGPT_ATI)
                      || ';' ||
            CHAR(NM_AGPT_ATI_FNCO)
              INTO   :TX-DADO
              FROM DB2SGT.RGR_AGPT_ATI_FNCO
               WHERE CD_AGPT_ATI_FNCO               =
                     :S1SGT102-CD-AGPT-ATI-FNCO
           END-EXEC
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE 0291                         TO CD-RTN-PGM
               MOVE 'Dados da tabela: RGR_AGPT_ATI_FNCO nao cadastrado'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GRAVA-LOG

           .
       402000-SAI-RGR-AGPT-ATI-FNCO.
           EXIT.
      *-----------------------------------------------------------------
       403000-AGPT-ATI-FNCO            SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO PSC-ERRO-PGM

           MOVE ZEROS                  TO LEN

           INSPECT CHAVE01 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE01 (1:LEN)        TO S1SGT103-CD-CTRA-SIS-DRIV

           MOVE ZEROS                  TO LEN
           INSPECT CHAVE02 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE02 (1:LEN)        TO S1SGT103-NR-SEQL-ATI

           MOVE ZEROS                  TO LEN
           INSPECT CHAVE03 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE03 (1:LEN)        TO S1SGT103-CD-AGPT-ATI-FNCO

           EXEC SQL
              SELECT
            DIGITS(CD_CTRA_SIS_DRIV)
                      || ';' ||
            DIGITS(NR_SEQL_ATI)
                      || ';' ||
            DIGITS(CD_AGPT_ATI_FNCO)
                      || ';' ||
            DIGITS(CD_EST_VCL_AGPT)
                      || ';' ||
            CHAR(CD_USU_MNTC_VCL)
                      || ';' ||
            CHAR(VALUE(CD_USU_CNFC_VCL,' '))
                      || ';' ||
            CHAR(CD_PGM_VCL_AGPT)
              INTO   :TX-DADO
              FROM DB2SGT.AGPT_ATI_FNCO
               WHERE CD_CTRA_SIS_DRIV                 =
                     :S1SGT103-CD-CTRA-SIS-DRIV
                 AND NR_SEQL_ATI                =
                     :S1SGT103-NR-SEQL-ATI
                 AND CD_AGPT_ATI_FNCO                =
                     :S1SGT103-CD-AGPT-ATI-FNCO
           END-EXEC

           IF  SQLCODE NOT EQUAL ZEROS
               MOVE 0291                         TO CD-RTN-PGM
               MOVE 'Dados da tabela: AGPT_ATI_FNCO nao cadastrado'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GRAVA-LOG
           .
       403000-SAI-AGPT-ATI-FNCO.
           EXIT.
      *-----------------------------------------------------------------
       404000-PNC-CFMD-ATI             SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO PSC-ERRO-PGM

           MOVE ZEROS                  TO LEN
           INSPECT CHAVE01 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE01 (1:LEN)         TO
                          S1SGT104-CD-CTRA-SIS-DRIV
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE02 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE02 (1:LEN)         TO
                          S1SGT104-NR-SEQL-ATI
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE03 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE03 (1:LEN)         TO
                          S1SGT104-TS-PNC-CFMD

           EXEC SQL
              SELECT
            DIGITS(CD_CTRA_SIS_DRIV)
                      || ';' ||
            DIGITS(NR_SEQL_ATI)
                      || ';' ||
            CHAR(TS_PNC_CFMD)
                      || ';' ||
            CHAR(NM_PNC_CFMD)
                      || ';' ||
            DIGITS(VL_PNC_CFMD)
                      || ';' ||
            DIGITS(CD_TIP_PNC_CFMD)
                      || ';' ||
            DIGITS(CD_EST_PNC_CFMD)
                      || ';' ||
            CHAR(VALUE(CD_USU_CNFC_PNC,' '))
                      || ';' ||
            CHAR(VALUE(TX_DCR_PNC_CFMD,' '))
              INTO   :TX-DADO
              FROM DB2SGT.PNC_CFMD_ATI
               WHERE CD_CTRA_SIS_DRIV               =
                     :S1SGT104-CD-CTRA-SIS-DRIV
                 AND NR_SEQL_ATI                    =
                     :S1SGT104-NR-SEQL-ATI
                 AND TS_PNC_CFMD                    =
                     :S1SGT104-TS-PNC-CFMD
           END-EXEC
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE 0291                         TO CD-RTN-PGM
               MOVE 'Dados da tabela: PNC-CFMD-ATI nao cadastrado'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GRAVA-LOG

           .
       404000-SAI-PNC-CFMD-ATI.
           EXIT.
      *-----------------------------------------------------------------
       405000-GRT-ATI-FNCO         SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO PSC-ERRO-PGM

           MOVE ZEROS                  TO LEN
           INSPECT CHAVE01 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE01 (1:LEN)         TO
                          S1SGT105-CD-CTRA-SIS-DRIV
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE02 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE02 (1:LEN)         TO
                          S1SGT105-NR-SEQL-ATI
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE03 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE03 (1:LEN)         TO
                          S1SGT105-CD-STIT-ATI-FNCO
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE04 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE04 (1:LEN)         TO
                          S1SGT105-NR-GRT-ATI

           EXEC SQL
              SELECT
            DIGITS(CD_CTRA_SIS_DRIV)
                      || ';' ||
            DIGITS(NR_SEQL_ATI)
                      || ';' ||
            DIGITS(CD_STIT_ATI_FNCO)
                      || ';' ||
            DIGITS(NR_GRT_ATI)
                      || ';' ||
            DIGITS(CD_ESPC_GRT)
                      || ';' ||
            DIGITS(CD_FMA_VCLC)
                      || ';' ||
            DIGITS(VL_ATZD_GRT_VCLD)
                      || ';' ||
            DIGITS(VL_OGNL_GRT_VCLD)
                      || ';' ||
            CHAR(DT_INC_GRT_ATI)
                      || ';' ||
            CHAR(DT_FIM_GRT_ATI)
                      || ';' ||
            DIGITS(CD_EST_GRT_ATI)
                      || ';' ||
            CHAR(CD_USU_MNTC_GRT)
                      || ';' ||
            CHAR(VALUE(CD_USU_CNFC_GRT,' '))
                      || ';' ||
            CHAR(CD_PGM_GRT_ATI)
              INTO   :TX-DADO
              FROM DB2SGT.GRT_ATI_FNCO
               WHERE CD_CTRA_SIS_DRIV               =
                     :S1SGT105-CD-CTRA-SIS-DRIV
                 AND NR_SEQL_ATI                    =
                     :S1SGT105-NR-SEQL-ATI
                 AND CD_STIT_ATI_FNCO               =
                     :S1SGT105-CD-STIT-ATI-FNCO
                 AND NR_GRT_ATI                     =
                     :S1SGT105-NR-GRT-ATI
           END-EXEC
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE 0291                         TO CD-RTN-PGM
               MOVE 'Dados da tabela: GRT_ATI_FNCO nao cadastrado'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GRAVA-LOG

           .
       405000-SAI-GRT-ATI-FNCO.
           EXIT.
      *-----------------------------------------------------------------
       406000-PNC-GRT-ATI-FNCO          SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO PSC-ERRO-PGM

           MOVE ZEROS                  TO LEN
           INSPECT CHAVE01 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE01 (1:LEN)         TO
                          S1SGT106-CD-CTRA-SIS-DRIV
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE02 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE02 (1:LEN)         TO
                          S1SGT106-NR-SEQL-ATI
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE03 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE03 (1:LEN)         TO
                          S1SGT106-TS-PNC-CFMD

           EXEC SQL
              SELECT
            DIGITS(CD_CTRA_SIS_DRIV)
                      || ';' ||
            DIGITS(NR_SEQL_ATI)
                      || ';' ||
            CHAR(TS_PNC_CFMD)
                      || ';' ||
            DIGITS(CD_STIT_ATI_FNCO)
                      || ';' ||
            DIGITS(NR_GRT_ATI)
              INTO   :TX-DADO
              FROM DB2SGT.PNC_GRT_ATI_FNCO
               WHERE CD_CTRA_SIS_DRIV               =
                     :S1SGT106-CD-CTRA-SIS-DRIV
                 AND NR_SEQL_ATI                    =
                     :S1SGT106-NR-SEQL-ATI
                 AND TS_PNC_CFMD                    =
                     :S1SGT106-TS-PNC-CFMD
           END-EXEC
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE 0291                         TO CD-RTN-PGM
               MOVE 'Dados da tabela: PNC_GRT_ATI_FNCO nao cadastrado'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GRAVA-LOG

           .
       406000-SAI-PNC-GRT-ATI-FNCO.
           EXIT.
      *-----------------------------------------------------------------
       407000-STIT-ATI-FNCO            SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO PSC-ERRO-PGM

           MOVE ZEROS                  TO LEN
           INSPECT CHAVE01 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE01 (1:LEN)         TO
                          S1SGT107-CD-CTRA-SIS-DRIV
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE02 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE02 (1:LEN)         TO
                          S1SGT107-NR-SEQL-ATI
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE03 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE03 (1:LEN)         TO
                          S1SGT107-CD-STIT-ATI-FNCO

           EXEC SQL
              SELECT
            DIGITS(CD_CTRA_SIS_DRIV)
                      || ';' ||
            DIGITS(NR_SEQL_ATI)
                      || ';' ||
            DIGITS(CD_STIT_ATI_FNCO)
                      || ';' ||
            CHAR(NM_STIT_ATI_FNCO)
                      || ';' ||
            DIGITS(CD_PRD)
                      || ';' ||
            DIGITS(CD_MDLD)
                      || ';' ||
            DIGITS(CD_TIP_ATI)
                      || ';' ||
            CHAR(DT_AQS_STIT_ATI)
                      || ';' ||
            CHAR(DT_EMS_STIT_ATI)
                      || ';' ||
            CHAR(DT_VNCT_STIT_ATI)
                      || ';' ||
            CHAR(DT_INC_VGC_STIT)
                      || ';' ||
            CHAR(DT_FIM_VGC_STIT)
                      || ';' ||
            DIGITS(VALUE(QT_STIT_ATI, 0))
                      || ';' ||
            DIGITS(VL_PU_MTM_STIT)
                      || ';' ||
            DIGITS(VALUE(VL_CTB_STIT_ATI, 0))
                      || ';' ||
            DIGITS(VALUE(VL_CTRD_STIT_ATI, 0))
                      || ';' ||
            DIGITS(VL_PVS_STIT_ATI)
                      || ';' ||
            CHAR(IN_ATI_STIT_DSCP)
                      || ';' ||
            CHAR(IN_REG_BB)
                      || ';' ||
            CHAR(CD_ISIN_STIT_ATI)
                      || ';' ||
            CHAR(CD_USU_MNTC_STIT)
                      || ';' ||
            CHAR(VALUE(CD_USU_CNFC_STIT,' '))
                      || ';' ||
            CHAR(CD_PGM_ATI_VCLD)
              INTO   :TX-DADO
              FROM DB2SGT.STIT_ATI_FNCO
               WHERE CD_CTRA_SIS_DRIV               =
                     :S1SGT107-CD-CTRA-SIS-DRIV
                 AND NR_SEQL_ATI                    =
                     :S1SGT107-NR-SEQL-ATI
                 AND CD_STIT_ATI_FNCO               =
                     :S1SGT107-CD-STIT-ATI-FNCO
           END-EXEC
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE 0291                         TO CD-RTN-PGM
               MOVE 'Dados da tabela: STIT-ATI-FNCO nao cadastrado'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GRAVA-LOG

           .
       407000-SAI-STIT-ATI-FNCO.
           EXIT.
      *-----------------------------------------------------------------
       408000-PNC-STIT-ATI-FNCO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO PSC-ERRO-PGM

           MOVE ZEROS                  TO LEN
           INSPECT CHAVE01 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE01 (1:LEN)         TO
                          S1SGT108-CD-CTRA-SIS-DRIV
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE02 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE02 (1:LEN)         TO
                          S1SGT108-NR-SEQL-ATI
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE03 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE03 (1:LEN)         TO
                          S1SGT108-TS-PNC-CFMD

           EXEC SQL
              SELECT
            DIGITS(CD_CTRA_SIS_DRIV)
                      || ';' ||
            DIGITS(NR_SEQL_ATI)
                      || ';' ||
            CHAR(TS_PNC_CFMD)
                      || ';' ||
            DIGITS(CD_STIT_ATI_FNCO)
              INTO   :TX-DADO
              FROM DB2SGT.PNC_STIT_ATI_FNCO
               WHERE CD_CTRA_SIS_DRIV               =
                     :S1SGT108-CD-CTRA-SIS-DRIV
                 AND NR_SEQL_ATI                    =
                     :S1SGT108-NR-SEQL-ATI
                 AND TS_PNC_CFMD                    =
                     :S1SGT108-TS-PNC-CFMD
           END-EXEC
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE 0291                         TO CD-RTN-PGM
               MOVE 'Dados da tabela: PNC-STIT-ATI-FNCO nao cadastrado'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GRAVA-LOG

           .
       408000-SAI-PNC-STIT-ATI-FNCO.
           EXIT.
      *-----------------------------------------------------------------
       409000-CLI-STIT-ATI-FNCO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO PSC-ERRO-PGM

           MOVE ZEROS                  TO LEN
           INSPECT CHAVE01 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE01 (1:LEN)         TO
                          S1SGT109-CD-CTRA-SIS-DRIV
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE02 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE02 (1:LEN)         TO
                          S1SGT109-NR-SEQL-ATI
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE03 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE03 (1:LEN)         TO
                          S1SGT109-CD-STIT-ATI-FNCO
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE04 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE04 (1:LEN)         TO
                          S1SGT109-CD-TIP-FUC-CLI

           EXEC SQL
              SELECT
            DIGITS(CD_CTRA_SIS_DRIV)
                      || ';' ||
            DIGITS(NR_SEQL_ATI)
                      || ';' ||
            DIGITS(CD_STIT_ATI_FNCO)
                      || ';' ||
            DIGITS(CD_TIP_FUC_CLI)
                      || ';' ||
            DIGITS(VALUE(NR_IDFR_CLI_EXNO, 0))
                      || ';' ||
            DIGITS(VALUE(CD_CLI, 0))
                      || ';' ||
            DIGITS(VALUE(CD_TIP_PSS, 0))
              INTO   :TX-DADO
              FROM DB2SGT.CLI_STIT_ATI_FNCO
               WHERE CD_CTRA_SIS_DRIV                =
                     :S1SGT109-CD-CTRA-SIS-DRIV
                 AND NR_SEQL_ATI                =
                     :S1SGT109-NR-SEQL-ATI
                 AND CD_STIT_ATI_FNCO               =
                     :S1SGT109-CD-STIT-ATI-FNCO
                 AND CD_TIP_FUC_CLI                 =
                     :S1SGT109-CD-TIP-FUC-CLI
           END-EXEC
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE 0291                         TO CD-RTN-PGM
               MOVE 'Dados da tabela: CLI-STIT-ATI-FNCO nao cadastrado'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GRAVA-LOG
           .
       409000-SAI-CLI-STIT-ATI-FNCO.
           EXIT.
      *-----------------------------------------------------------------
       410000-HST-AGPT-ATI-FNCO             SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO PSC-ERRO-PGM

           MOVE ZEROS                  TO LEN
           INSPECT CHAVE01 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE01 (1:LEN)         TO
                          S1SGT110-CD-CTRA-SIS-DRIV
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE02 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE02 (1:LEN)         TO
                          S1SGT110-NR-SEQL-ATI
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE03 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE03 (1:LEN)         TO
                          S1SGT110-CD-AGPT-ATI-FNCO
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE04 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE04 (1:LEN)         TO
                          S1SGT110-TS-HST-AGPT-ATI

           EXEC SQL
              SELECT
            DIGITS(CD_CTRA_SIS_DRIV)
                      || ';' ||
            DIGITS(NR_SEQL_ATI)
                      || ';' ||
            DIGITS(CD_AGPT_ATI_FNCO)
                      || ';' ||
            CHAR(TS_HST_AGPT_ATI)
                      || ';' ||
            DIGITS(CD_EST_VCL_AGPT)
                      || ';' ||
            CHAR(CD_USU_MNTC_VCL)
                      || ';' ||
            CHAR(VALUE(CD_USU_CNFC_VCL,' '))
                      || ';' ||
            CHAR(CD_PGM_VCL_AGPT)
              INTO   :TX-DADO
              FROM DB2SGT.HST_AGPT_ATI_FNCO
               WHERE CD_CTRA_SIS_DRIV               =
                     :S1SGT110-CD-CTRA-SIS-DRIV
                 AND NR_SEQL_ATI                    =
                     :S1SGT110-NR-SEQL-ATI
                 AND CD_AGPT_ATI_FNCO               =
                     :S1SGT110-CD-AGPT-ATI-FNCO
                 AND TS_HST_AGPT_ATI                =
                     :S1SGT110-TS-HST-AGPT-ATI
           END-EXEC
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE 0291                         TO CD-RTN-PGM
               MOVE 'Dados da tabela: HST_AGPT_ATI_FNCO nao cadastrado'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GRAVA-LOG

           .
       410000-SAI-HST-AGPT-ATI-FNCO.
           EXIT.
      *-----------------------------------------------------------------
       411000-HST-CTRA-SIS-DRIV          SECTION.
      *-----------------------------------------------------------------
           MOVE 0014                   TO PSC-ERRO-PGM

           MOVE ZEROS                  TO LEN
           INSPECT CHAVE01 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE01 (1:LEN)         TO
                          S1SGT111-CD-CTRA-SIS-DRIV
           MOVE CHAVE02         TO
                          S1SGT111-TS-HST-CTRA

           EXEC SQL
              SELECT
            DIGITS(CD_CTRA_SIS_DRIV)
                      || ';' ||
            CHAR(TS_HST_CTRA)
                      || ';' ||
            CHAR(NM_PGM_RSP_ATL)
                      || ';' ||
            CHAR(DT_INC_ENVO_ATI)
                      || ';' ||
            CHAR(VALUE(DT_FIM_ENVO_ATI, '01.01.0001'))
                      || ';' ||
            DIGITS(VALUE(CD_UOR_GST, 0))
                      || ';' ||
            DIGITS(VALUE(CD_UOR_OPR, 0))
                      || ';' ||
            DIGITS(CD_EST_CTRA_SIS)
                      || ';' ||
            CHAR(VALUE(CD_USU_MNTC_CTRA,''))
                      || ';' ||
            CHAR(VALUE(CD_USU_CNFC_CTRA,' '))
                      || ';' ||
            CHAR(NM_CTRA_SIS_DRIV)
              INTO   :TX-DADO
              FROM DB2SGT.HST_CTRA_SIS_DRIV
               WHERE CD_CTRA_SIS_DRIV             =
                     :S1SGT111-CD-CTRA-SIS-DRIV
                 AND TS_HST_CTRA                  =
                     :S1SGT111-TS-HST-CTRA
           END-EXEC
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE 0291                         TO CD-RTN-PGM
               MOVE 'Dados da tabela: HST_CTRA_SIS_DRIV nao cadastrado'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GRAVA-LOG

           .
       411000-SAI-HST-CTRA-SIS-DRIV.
           EXIT.
      *-----------------------------------------------------------------
       412000-HST-GRT-ATI-FNCO          SECTION.
      *-----------------------------------------------------------------
           MOVE 0015                   TO PSC-ERRO-PGM

           MOVE ZEROS                  TO LEN
           INSPECT CHAVE01 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE01 (1:LEN)         TO
                          S1SGT112-CD-CTRA-SIS-DRIV
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE02 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE02 (1:LEN)         TO
                          S1SGT112-NR-SEQL-ATI
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE03 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE03 (1:LEN)         TO
                          S1SGT112-CD-STIT-ATI-FNCO
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE04 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE04 (1:LEN)         TO
                          S1SGT112-NR-GRT-ATI
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE05 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE05 (1:LEN)         TO
                          S1SGT112-TS-HST-GRT-ATI

           EXEC SQL
              SELECT
            DIGITS(CD_CTRA_SIS_DRIV)
                      || ';' ||
            DIGITS(NR_SEQL_ATI)
                      || ';' ||
            DIGITS(CD_STIT_ATI_FNCO)
                      || ';' ||
            DIGITS(NR_GRT_ATI)
                      || ';' ||
            CHAR(TS_HST_GRT_ATI)
                      || ';' ||
            DIGITS(CD_ESPC_GRT)
                      || ';' ||
            DIGITS(CD_FMA_VCLC)
                      || ';' ||
            DIGITS(VL_ATZD_GRT_VCLD)
                      || ';' ||
            DIGITS(VL_OGNL_GRT_VCLD)
                      || ';' ||
            CHAR(DT_INC_GRT_ATI)
                      || ';' ||
            CHAR(DT_FIM_GRT_ATI)
                      || ';' ||
            DIGITS(CD_EST_GRT_ATI)
                      || ';' ||
            CHAR(CD_USU_MNTC_GRT)
                      || ';' ||
            CHAR(VALUE(CD_USU_CNFC_GRT,' '))
                      || ';' ||
            CHAR(CD_PGM_GRT_ATI)
              INTO   :TX-DADO
              FROM DB2SGT.HST_GRT_ATI_FNCO
               WHERE CD_CTRA_SIS_DRIV               =
                     :S1SGT112-CD-CTRA-SIS-DRIV
                 AND NR_SEQL_ATI                    =
                     :S1SGT112-NR-SEQL-ATI
                 AND CD_STIT_ATI_FNCO               =
                     :S1SGT112-CD-STIT-ATI-FNCO
                 AND NR_GRT_ATI                     =
                     :S1SGT112-NR-GRT-ATI
                 AND TS_HST_GRT_ATI                 =
                     :S1SGT112-TS-HST-GRT-ATI
           END-EXEC
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE 0291                         TO CD-RTN-PGM
               MOVE 'Dados da tabela: HST_GRT_ATI_FNCO nao cadastrado'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GRAVA-LOG

           .
       412000-SAI-HST-GRT-ATI-FNCO.
           EXIT.
      *-----------------------------------------------------------------
       413000-HST-RGR-AGPT-ATI         SECTION.
      *-----------------------------------------------------------------
           MOVE 0016                   TO PSC-ERRO-PGM

           MOVE ZEROS                  TO LEN
           INSPECT CHAVE01 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE01 (1:LEN)         TO
                          S1SGT113-CD-AGPT-ATI-FNCO
           MOVE CHAVE02         TO
                          S1SGT113-TS-HST-RGR-AGPT

           EXEC SQL
              SELECT
            DIGITS(CD_AGPT_ATI_FNCO)
                      || ';' ||
            CHAR(TS_HST_RGR_AGPT)
                      || ';' ||
            DIGITS(CD_RBC_CTB_PATL)
                      || ';' ||
            DIGITS(CD_RBC_MTM_PST)
                      || ';' ||
            DIGITS(CD_RBC_MTM_NGT)
                      || ';' ||
            CHAR(IN_EXPO_STZC)
                      || ';' ||
            CHAR(DT_INC_VGC)
                      || ';' ||
            CHAR(DT_FIM_VGC)
                      || ';' ||
            DIGITS(CD_EST_AGPT_ATI)
                      || ';' ||
            CHAR(CD_USU_MNTC_AGPT)
                      || ';' ||
            CHAR(VALUE(CD_USU_CNFC_AGPT,' '))
                      || ';' ||
            CHAR(CD_PGM_AGPT_ATI)
                      || ';' ||
            CHAR(NM_AGPT_ATI_FNCO)
              INTO   :TX-DADO
              FROM DB2SGT.HST_RGR_AGPT_ATI
               WHERE CD_AGPT_ATI_FNCO                =
                     :S1SGT113-CD-AGPT-ATI-FNCO
                 AND TS_HST_RGR_AGPT                 =
                     :S1SGT113-TS-HST-RGR-AGPT
           END-EXEC
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE 0291                         TO CD-RTN-PGM
               MOVE 'Dados da tabela: HST-RGR-AGPT-TIT nao cadastrado'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GRAVA-LOG

           .
       413000-SAI-HST-RGR-AGPT-TIT.
           EXIT.
      *-----------------------------------------------------------------
       414000-HST-STIT-ATI-FNCO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0017                   TO PSC-ERRO-PGM

           MOVE ZEROS                  TO LEN
           INSPECT CHAVE01 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE01 (1:LEN)         TO
                          S1SGT114-CD-CTRA-SIS-DRIV
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE02 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE02 (1:LEN)         TO
                          S1SGT114-NR-SEQL-ATI
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE03 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE03 (1:LEN)         TO
                          S1SGT114-CD-STIT-ATI-FNCO
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE04 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE04 (1:LEN)         TO
                          S1SGT114-TS-HST-STIT-ATI

           EXEC SQL
              SELECT
            DIGITS(CD_CTRA_SIS_DRIV)
                      || ';' ||
            DIGITS(NR_SEQL_ATI)
                      || ';' ||
            DIGITS(CD_STIT_ATI_FNCO)
                      || ';' ||
            CHAR(TS_HST_STIT_ATI)
                      || ';' ||
            CHAR(NM_STIT_ATI_FNCO)
                      || ';' ||
            DIGITS(CD_PRD)
                      || ';' ||
            DIGITS(CD_MDLD)
                      || ';' ||
            DIGITS(CD_TIP_ATI)
                      || ';' ||
            CHAR(DT_AQS_STIT_ATI)
                      || ';' ||
            CHAR(DT_EMS_STIT_ATI)
                      || ';' ||
            CHAR(DT_VNCT_STIT_ATI)
                      || ';' ||
            CHAR(DT_INC_VGC_STIT)
                      || ';' ||
            CHAR(DT_FIM_VGC_STIT)
                      || ';' ||
            DIGITS(VALUE(QT_STIT_ATI, 0))
                      || ';' ||
            DIGITS(VL_PU_MTM_STIT)
                      || ';' ||
            DIGITS(VALUE(VL_CTB_STIT_ATI, 0))
                      || ';' ||
            DIGITS(VALUE(VL_CTRD_STIT_ATI, 0))
                      || ';' ||
            DIGITS(VL_PVS_STIT_ATI)
                      || ';' ||
            CHAR(IN_ATI_STIT_DSCP)
                      || ';' ||
            CHAR(IN_REG_BB)
                      || ';' ||
            CHAR(CD_ISIN_STIT_ATI)
                      || ';' ||
            CHAR(CD_USU_MNTC_STIT)
                      || ';' ||
            CHAR(VALUE(CD_USU_CNFC_STIT,' '))
                      || ';' ||
            CHAR(CD_PGM_ATI_VCLD)
              INTO   :TX-DADO
              FROM DB2SGT.HST_STIT_ATI_FNCO
               WHERE CD_CTRA_SIS_DRIV               =
                     :S1SGT114-CD-CTRA-SIS-DRIV
                 AND NR_SEQL_ATI                    =
                     :S1SGT114-NR-SEQL-ATI
                 AND CD_STIT_ATI_FNCO               =
                     :S1SGT114-CD-STIT-ATI-FNCO
                 AND TS_HST_STIT_ATI                =
                     :S1SGT114-TS-HST-STIT-ATI
           END-EXEC
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE 0291                         TO CD-RTN-PGM
               MOVE 'Dados da tabela: HST_STIT_ATI_FNCO nao cadastrado'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GRAVA-LOG

           .
       414000-SAI-HST-STIT-ATI-FNCO.
           EXIT.
      *-----------------------------------------------------------------
       415000-ATI-FNCO-ANL-RSCO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0018                   TO PSC-ERRO-PGM

           MOVE ZEROS                  TO LEN
           INSPECT CHAVE01 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE01 (1:LEN)         TO
                          S1SGT099-CD-CTRA-SIS-DRIV
           MOVE ZEROS                  TO LEN
           INSPECT CHAVE02 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE02 (1:LEN)         TO
                          S1SGT099-NR-SEQL-ATI

           EXEC SQL
              SELECT
            DIGITS(CD_CTRA_SIS_DRIV)
                      || ';' ||
            DIGITS(NR_SEQL_ATI)
                      || ';' ||
            DIGITS(NR_SEQL_AQS_ATI)
                      || ';' ||
            CHAR(TS_ATL_ATI_FNCO)
                      || ';' ||
            CHAR(CD_USU_RSP_ATL)
                      || ';' ||
            CHAR(NM_ATI_FNCO_ANL)
                      || ';' ||
            DIGITS(CD_CLS_COTA_TIT)
              INTO   :TX-DADO
              FROM DB2SGT.ATI_FNCO_ANL_RSCO
               WHERE CD_CTRA_SIS_DRIV               =
                     :S1SGT099-CD-CTRA-SIS-DRIV
                 AND NR_SEQL_ATI                    =
                     :S1SGT099-NR-SEQL-ATI
           END-EXEC
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE 0291                         TO CD-RTN-PGM
               MOVE 'Dados da tabela: ATI_FNCO_ANL_RSCO nao cadastrado'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GRAVA-LOG

           .
       415000-SAI-ATI-FNCO-ANL-RSCO.
           EXIT.
      *-----------------------------------------------------------------
       416000-CTRA-SIS-DRIV        SECTION.
      *-----------------------------------------------------------------
           MOVE 0019                   TO PSC-ERRO-PGM

           MOVE ZEROS                  TO LEN
           INSPECT CHAVE01 TALLYING LEN FOR ALL ' '
           COMPUTE LEN = 50 - LEN
              ON SIZE ERROR
                 MOVE 0143                         TO CD-RTN-PGM
                 MOVE 'Erro ao calcular o tam var' TO MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-COMPUTE
           MOVE CHAVE01 (1:LEN)         TO
                          S1SGT100-CD-CTRA-SIS-DRIV
           EXEC SQL
              SELECT
            DIGITS(CD_CTRA_SIS_DRIV)
                      || ';' ||
            CHAR(NM_PGM_RSP_ATL)
                      || ';' ||
            CHAR(DT_INC_ENVO_ATI)
                      || ';' ||
            CHAR(VALUE(DT_FIM_ENVO_ATI, '01.01.0001'))
                      || ';' ||
            DIGITS(VALUE(CD_UOR_GST, 0))
                      || ';' ||
            DIGITS(VALUE(CD_UOR_OPR, 0))
                      || ';' ||
            DIGITS(CD_EST_CTRA_SIS)
                      || ';' ||
            CHAR(VALUE(CD_USU_MNTC_CTRA,' '))
                      || ';' ||
            CHAR(VALUE(CD_USU_CNFC_CTRA,' '))
                      || ';' ||
            CHAR(NM_CTRA_SIS_DRIV)
              INTO   :TX-DADO
              FROM DB2SGT.CTRA_SIS_DRIV
               WHERE CD_CTRA_SIS_DRIV               =
                     :S1SGT100-CD-CTRA-SIS-DRIV
           END-EXEC
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE 0291                         TO CD-RTN-PGM
               MOVE 'Dados da tabela: CTRA_SIS_DRIV nao cadastrado'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GRAVA-LOG

           .
       416000-SAI-CTRA-SIS-DRIV.
           EXIT.


      *-----------------------------------------------------------------
       980101-GRAVA-LOG                SECTION.
      *-----------------------------------------------------------------
           MOVE 0020                   TO PSC-ERRO-PGM

           MOVE TX-DADO                TO TX-LOG

           EXEC SQL
           INSERT INTO DB2SGT.LOG_TAB_SIS
                     ( TS_ATL_LOG
                      , NM_TAB_LOG
                      , CD_TIP_ATL_LOG
                      , CD_USU_RSP_ATL
                      , NM_PGM_ATL_LOG
                      , TX_CTU_LOG )
             VALUES ( CURRENT TIMESTAMP
                    , :SGTS015G-NM-ENT
                    , :SGTS015G-CD-TIP-ACAO
                    , :SGTS015G-CD-CHV-USU
                    , :SGTS015G-NM-PGM
                    , :TX-LOG )
           END-EXEC

           .
       980101-SAI-GRAVA-LOG            SECTION.
           EXIT.
      *--------------------
      * 999000-ERRO SECTION
      *--------------------
      *-----------------
      * 999001-ERRO-001.
      *-----------------
-INC HLPKE004
           PERFORM 999080-LISTA-EXCECAO-DSNTIAR
           PERFORM 999082-PREPARA-ERRO
           MOVE RTN-PGM                       TO SGTS015G-RTN-PGM
           GOBACK
           .
      *-----------------
      * 999002-ERRO-DB2
      *-----------------
-INC HLPKE005
      *-----------------
      * 999003-ERRO-CICS
      *-----------------
-INC HLPKE006
      *-----------------------------
      * 999080-LISTA-EXCECAO-DSNTIAR
      *-----------------------------
-INC HLPKE007
      *------------------------
      * 999081-ADICIONA-EXCECAO
      *------------------------
-INC HLPKE008
      *----------------------------
       999082-PREPARA-ERRO             SECTION.
      *----------------------------
           MOVE 'PSC-ERRO-PGM'                TO HLPKE001-EXCO-NM-VRV
           MOVE PSC-ERRO-PGM                  TO HLPKE001-EXCO-CTU-NUM
           PERFORM 999081-ADICIONA-EXCECAO

           MOVE 'CD-RTN-PGM'                  TO HLPKE001-EXCO-NM-VRV
           MOVE CD-RTN-PGM                    TO HLPKE001-EXCO-CTU-NUM
           PERFORM 999081-ADICIONA-EXCECAO

           MOVE 'MSG-ERRO-PGM'                TO HLPKE001-EXCO-NM-VRV
           MOVE MSG-ERRO-PGM                  TO HLPKE001-EXCO-CTU-VRV
           PERFORM 999081-ADICIONA-EXCECAO

           MOVE 'SGTS015G-NM-ENT'             TO HLPKE001-EXCO-NM-VRV
           MOVE SGTS015G-NM-ENT               TO HLPKE001-EXCO-CTU-VRV
           PERFORM 999081-ADICIONA-EXCECAO

           MOVE 'SGTS015G-CD-TIP-ACAO'        TO HLPKE001-EXCO-NM-VRV
           MOVE SGTS015G-CD-TIP-ACAO          TO HLPKE001-EXCO-CTU-VRV
           PERFORM 999081-ADICIONA-EXCECAO

           MOVE 'SGTS015G-CHV-PRMR'           TO HLPKE001-EXCO-NM-VRV
           MOVE SGTS015G-CHV-PRMR             TO HLPKE001-EXCO-CTU-VRV
           PERFORM 999081-ADICIONA-EXCECAO

           MOVE 'SGTS015G-CD-CHV-USU'         TO HLPKE001-EXCO-NM-VRV
           MOVE SGTS015G-CD-CHV-USU           TO HLPKE001-EXCO-CTU-VRV
           PERFORM 999081-ADICIONA-EXCECAO

           MOVE 'SGTS015G-NM-PGM'             TO HLPKE001-EXCO-NM-VRV
           MOVE SGTS015G-NM-PGM               TO HLPKE001-EXCO-CTU-VRV
           PERFORM 999081-ADICIONA-EXCECAO
      * EFETUA A CHAMADA DA SUBROTINA HLPS0001
-INC HLPKE009

           .
       999082-SAI-ERROS.
           EXIT.
      *---------------------- FIM SGTS015G ----------------------------