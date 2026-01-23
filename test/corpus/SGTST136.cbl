       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTST136
      * ANALISTA..: F9458161 - Vanessa Tsukamoto
      * AUTOR.....: F9458161 - Vanessa Tsukamoto
      * DATA......: 16.09.2024
      * OBJETIVO..: PERSISTENCIA DA TABELA PRCT_CTRA_TSR
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS001 16.09.2024 - F9458161 - Implantacao
      *-----------------------------------------------------------------
      * CD-OGM-ERRO: 1-CAC
      *              2-GSF
      *              3-PRD
      *              4-SGT
      *              5-RSM
      *              6-MST
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTST136.
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
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTST136 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTS014G                PIC  X(08) VALUE 'SGTS014G'.

      * Registro da tabela

       01  REG-CACHE.
           03  CHAVE-CACHE.
               05  DT-PRCT                   PIC  X(10) VALUE ' '.
               05  DT-PRCT-R REDEFINES
                   DT-PRCT.
                   07 DIA                    PIC  9(2).
                   07 FILLER                 PIC  X(1).
                   07 MES                    PIC  9(2).
                   07 FILLER                 PIC  X(1).
                   07 ANO                    PIC  9(4).
               05  NR-SEQL-PRCT              PIC S9(9) COMP-5 VALUE 0.
               05  FILLER                    PIC  X(20) VALUE ' '.
           03  DADOS-CACHE.
               05  DT-CPR                    PIC  X(10) VALUE ' '.
               05  DT-CPR-R REDEFINES
                   DT-CPR.
                   07 DIA                    PIC  9(2).
                   07 FILLER                 PIC  X(1).
                   07 MES                    PIC  9(2).
                   07 FILLER                 PIC  X(1).
                   07 ANO                    PIC  9(4).
               05  CD-ITMT-FNCO              PIC  X(50) VALUE ' '.
               05  VL-PU-CPR           PIC S9(11)V9(8) COMP-3 VALUE 0.
               05  CD-TIP-PRCT               PIC S9(4) COMP-5 VALUE 0.
               05  CD-EST-PRCT               PIC S9(4) COMP-5 VALUE 0.
               05  TS-PRCT                   PIC  X(026) VALUE ' '.
               05  CD-PGM                    PIC  X(8) VALUE ' '.
               05  CD-CTRA-TIT-VL            PIC S9(9) COMP-5 VALUE 0.
               05  CD-OPR-ETRD-MCP           PIC S9(11) COMP-3 VALUE 0.
               05  NR-VRS-OPR-ETRD           PIC S9(4) COMP-5 VALUE 0.
               05  CD-USU                    PIC  X(8) VALUE ' '.
               05  CD-OGM-ERRO               PIC S9(4) COMP-5 VALUE 0.
               05  TX-ERRO-PRCT              PIC  X(100) VALUE ' '.

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
       01  TS-SQL-ATU                       PIC  X(26) VALUE ' '.

      * Indicadores de nulidades
       01  CD-OGM-ERRO-N                    PIC S9(4) COMP-5 VALUE 0.
       01  TX-ERRO-PRCT-N                   PIC S9(4) COMP-5 VALUE 0.
       01  CD-CTRA-TIT-VL-N                 PIC S9(4) COMP-5 VALUE 0.
       01  CD-OPR-ETRD-MCP-N                PIC S9(4) COMP-5 VALUE 0.
       01  NR-VRS-OPR-ETRD-N                PIC S9(4) COMP-5 VALUE 0.

      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  LOG-DT-PRCT                  PIC  X(10) VALUE ' '.
           03  LOG-NR-SEQL-PRCT             PIC  9(09) VALUE 0.

       01  GDA-VALOR-DISPLAY                 PIC  X(21).
       01  GDA-VALOR-NUM REDEFINES GDA-VALOR-DISPLAY.
           03 GDA-VALOR-N                    PIC  9(11)V9(8).
       01  GDA-VALOR-MASK REDEFINES GDA-VALOR-DISPLAY.
           03 GDA-VALOR-M                    PIC -ZZZZZZZZZZZ,99999999.

      * Subrotinas Utilizadas

      * Books da definicao de dados a serem usados na subrotina de erros
-INC HLPKE001

      * Books auxiliares
-INC SGTKMSGV

      * Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

      * Book para mock
-INC ALMKCBL1

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTKT136
      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------

      * Book para mock
-INC ALMKCBL2

           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               SET  ERRO-CODIFICADO    TO TRUE
               MOVE 0220               TO CD-RTN-PGM
               MOVE 'Area com tamanho invalido - verifique book/local'
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
              WHEN SGTST136-INCLUIR
                   PERFORM 400000-INCLUIR
              WHEN SGTST136-ALTERAR
                   PERFORM 500000-ALTERAR
              WHEN SGTST136-CONSULTAR
                   IF  CHAVE-CACHE = SGTST136-CHAVE
                       MOVE DADOS-CACHE TO SGTST136-AREA-DADOS
                   ELSE
                       PERFORM 700000-CONSULTA
                   END-IF
           END-EVALUATE
           GOBACK
           .
      *-----------------------------------------------------------------
       100000-VALIDA-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO PSC-ERRO-PGM

           INITIALIZE SGTST136-RTN-PGM

           IF  NOT SGTST136-INCLUIR
           AND NOT SGTST136-ALTERAR
           AND NOT SGTST136-CONSULTAR
               SET  ERRO-CODIFICADO    TO TRUE
               MOVE 0116               TO CD-RTN-PGM
               MOVE 'Acao invalida.'   TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 130000-VALIDA-CAMPOS

           IF  SGTST136-INCLUIR
           OR  SGTST136-ALTERAR
               PERFORM 160000-TRATA-NULIDADE
           END-IF
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       130000-VALIDA-CAMPOS SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO PSC-ERRO-PGM

           PERFORM 140000-BUSCA-TS-DT

           MOVE CORR HLPKE001-TS-FMTD  TO DT-SQL-ATU-R
           MOVE HLPKE001-TS-FMTD       TO TS-SQL-ATU

           IF  SGTST136-DT-PRCT EQUAL SPACES
               SET  ERRO-CODIFICADO    TO TRUE
               MOVE 'Data do processamento nao informada.'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  NOT SGTST136-INCLUIR
           AND SGTST136-NR-SEQL-PRCT EQUAL ZEROS
               SET  ERRO-CODIFICADO    TO TRUE
               MOVE 'Numero sequencial do processamento nao informado.'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTST136-INCLUIR
           OR  SGTST136-ALTERAR
               IF  SGTST136-DT-CPR EQUAL SPACES
                   MOVE '01.01.0001'   TO SGTST136-DT-CPR
               END-IF
      *         IF  SGTST136-CD-ITMT-FNCO EQUAL SPACES
      *            SET  ERRO-CODIFICADO TO TRUE
      *            MOVE 'Instrumento financeiro nao informado'
      *                                 TO MSG-ERRO
      *            PERFORM 999001-ERRO-001
      *        END-IF
               IF  SGTST136-VL-PU-CPR < 0
                   SET  ERRO-CODIFICADO TO TRUE
                   MOVE 1136            TO CD-RTN-PGM
                   MOVE 'Valor preco unitario de compra inválido.'
                                        TO MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
               IF  SGTST136-CD-TIP-PRCT EQUAL ZEROS
                   SET  ERRO-CODIFICADO TO TRUE
                   MOVE 'Codigo do tipo de processamento nao informado.'
                                        TO MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
               IF  SGTST136-CD-EST-PRCT EQUAL ZEROS
                   SET  ERRO-CODIFICADO TO TRUE
                   MOVE 'Codigo estado do processamento nao informado.'
                                        TO MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
               IF  SGTST136-TS-PRCT EQUAL SPACES
                   MOVE 'CURRENT'      TO SGTST136-TS-PRCT
               END-IF
               IF  SGTST136-CD-PGM EQUAL SPACES
                   SET  ERRO-CODIFICADO TO TRUE
                   MOVE 'Codigo do programa responsavel pela atualizacao
      -            ' nao informado'
                                        TO MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
               IF  SGTST136-CD-USU EQUAL SPACES
                   SET  ERRO-CODIFICADO TO TRUE
                   MOVE 0226            TO CD-RTN-PGM
                   MOVE 'Codigo do usuario nao informado'
                                        TO MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF

           IF  SGTST136-DT-PRCT EQUAL 'CURRENT'
               MOVE DT-SQL-ATU         TO SGTST136-DT-PRCT
           END-IF

           IF  SGTST136-DT-CPR EQUAL 'CURRENT'
               MOVE DT-SQL-ATU         TO SGTST136-DT-CPR
           END-IF

           IF  SGTST136-TS-PRCT EQUAL 'CURRENT'
               MOVE TS-SQL-ATU         TO SGTST136-TS-PRCT
           END-IF

           IF  SGTST136-CD-CTRA-TIT-VL  NOT EQUAL ZEROS
           AND SGTST136-CD-OPR-ETRD-MCP NOT EQUAL ZEROS
           AND SGTST136-NR-VRS-OPR-ETRD NOT EQUAL ZEROS
               PERFORM 170001-VALIDA-OPR-ETRD-MCP
           END-IF
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-BUSCA-TS-DT SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO PSC-ERRO-PGM

           CALL SBTIMEDT            USING HLPKE001-CD-FUC
                                          HLPKE001-AREA-TRB
                                          HLPKE001-FMT-TS
                                          HLPKE001-TAM-TS
                                          HLPKE001-TS-FMTD

      * MOCK-POINT MCKV1
           IF  RETURN-CODE      NOT EQUAL ZEROS
               SET  ERRO-DE-PGM        TO TRUE
               MOVE SBTIMEDT           TO PGM-ERRO
               MOVE 0050               TO CD-RTN-PGM
               MOVE 'Erro ao obter data e hora atual.' TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       160000-TRATA-NULIDADE SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO PSC-ERRO-PGM

           IF  SGTST136-CD-CTRA-TIT-VL EQUAL ZEROS
               MOVE -1                 TO CD-CTRA-TIT-VL-N
           END-IF

           IF  SGTST136-CD-OPR-ETRD-MCP EQUAL ZEROS
               MOVE -1                 TO CD-OPR-ETRD-MCP-N
           END-IF

           IF  SGTST136-NR-VRS-OPR-ETRD EQUAL ZEROS
               MOVE -1                 TO NR-VRS-OPR-ETRD-N
           END-IF

           IF  SGTST136-CD-OGM-ERRO EQUAL ZEROS
               MOVE -1                 TO CD-OGM-ERRO-N
           END-IF

           IF  SGTST136-TX-ERRO-PRCT EQUAL SPACES
               MOVE -1                 TO TX-ERRO-PRCT-N
           END-IF
           .
       160000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       170001-VALIDA-OPR-ETRD-MCP     SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO PSC-ERRO-PGM

           MOVE ZEROS                  TO QT-REG-SELD

           EXEC SQL
                SELECT 1
                  INTO :QT-REG-SELD
                  FROM DB2SGT.OPR_ETRD_MCP
                 WHERE CD_CTRA_TIT_VL  = :SGTST136-CD-CTRA-TIT-VL
                   AND CD_OPR_ETRD_MCP = :SGTST136-CD-OPR-ETRD-MCP
                   AND NR_VRS_OPR_ETRD = :SGTST136-NR-VRS-OPR-ETRD
           END-EXEC

      * MOCK-POINT MCKV2
           EVALUATE SQLCODE
               WHEN 0
                    CONTINUE
               WHEN +100
                    SET  ERRO-CODIFICADO TO TRUE
                    MOVE 0050            TO CD-RTN-PGM
                    MOVE 'Operacao inexistente.' TO MSG-ERRO
                    PERFORM 999001-ERRO-001
               WHEN OTHER
                    PERFORM 999002-ERRO-DB2
           END-EVALUATE
           .
       170001-SAI.
           EXIT.
      *-----------------------------------------------------------------
       400000-INCLUIR SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO PSC-ERRO-PGM
      *
      * Desligar erro automatico do DB2
      *
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC
      *
           PERFORM 410000-BUSCA-NR-SEQUENCIA

           EXEC SQL
                INSERT INTO DB2SGT.PRCT_CTRA_TSR
                ( DT_PRCT
                , NR_SEQL_PRCT
                , DT_CPR
                , CD_ITMT_FNCO
                , VL_PU_CPR
                , CD_TIP_PRCT
                , CD_EST_PRCT
                , TS_PRCT
                , CD_PGM
                , CD_CTRA_TIT_VL
                , CD_OPR_ETRD_MCP
                , NR_VRS_OPR_ETRD
                , CD_USU
                , CD_OGM_ERRO
                , TX_ERRO_PRCT )
                VALUES
                ( :SGTST136-DT-PRCT
                , :SGTST136-NR-SEQL-PRCT
                , :SGTST136-DT-CPR
                , :SGTST136-CD-ITMT-FNCO
                , :SGTST136-VL-PU-CPR
                , :SGTST136-CD-TIP-PRCT
                , :SGTST136-CD-EST-PRCT
                , :SGTST136-TS-PRCT
                , :SGTST136-CD-PGM
                , :SGTST136-CD-CTRA-TIT-VL
                  :CD-CTRA-TIT-VL-N
                , :SGTST136-CD-OPR-ETRD-MCP
                  :CD-OPR-ETRD-MCP-N
                , :SGTST136-NR-VRS-OPR-ETRD
                  :NR-VRS-OPR-ETRD-N
                , :SGTST136-CD-USU
                , :SGTST136-CD-OGM-ERRO
                  :CD-OGM-ERRO-N
                , :SGTST136-TX-ERRO-PRCT
                  :TX-ERRO-PRCT-N )
           END-EXEC
      *
      * Ligar erro automatico do DB2
      *
           EXEC SQL WHENEVER SQLERROR   GO TO 999002-ERRO-DB2 END-EXEC
      *
      * MOCK-POINT MCKV3
           EVALUATE SQLCODE
               WHEN 0
                    CONTINUE
               WHEN -803
                    SET ERRO-CODIFICADO TO TRUE
                    MOVE 0803           TO CD-RTN-PGM
                    MOVE 'Inclusao nao efetuada, registro existente'
                                        TO MSG-ERRO
                    PERFORM 999001-ERRO-001
               WHEN OTHER
      * Aciona o tratamento padrao de erro padrao para os demais casos
                    PERFORM 999002-ERRO-DB2
           END-EVALUATE

           PERFORM 980101-GUARDA-CACHE
           PERFORM 980102-GRAVA-LOG
           .
       400000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       410000-BUSCA-NR-SEQUENCIA SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO PSC-ERRO-PGM

           EXEC SQL
                SELECT VALUE (MAX(NR_SEQL_PRCT), 0 )
                  INTO :SGTST136-NR-SEQL-PRCT
                  FROM DB2SGT.PRCT_CTRA_TSR
                 WHERE DT_PRCT = :SGTST136-DT-PRCT
           END-EXEC

      * MOCK-POINT MCKV4
           EVALUATE SQLCODE
               WHEN 0
                    CONTINUE
               WHEN OTHER
                    PERFORM 999002-ERRO-DB2
           END-EVALUATE

           ADD 1                       TO SGTST136-NR-SEQL-PRCT
           .
       410000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       500000-ALTERAR SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO PSC-ERRO-PGM

           EXEC SQL
                UPDATE DB2SGT.PRCT_CTRA_TSR
                   SET DT_CPR          = :SGTST136-DT-CPR
                     , CD_ITMT_FNCO    = :SGTST136-CD-ITMT-FNCO
                     , VL_PU_CPR       = :SGTST136-VL-PU-CPR
                     , CD_TIP_PRCT     = :SGTST136-CD-TIP-PRCT
                     , CD_EST_PRCT     = :SGTST136-CD-EST-PRCT
                     , TS_PRCT         = :SGTST136-TS-PRCT
                     , CD_PGM          = :SGTST136-CD-PGM
                     , CD_CTRA_TIT_VL  = :SGTST136-CD-CTRA-TIT-VL
                                         :CD-CTRA-TIT-VL-N
                     , CD_OPR_ETRD_MCP = :SGTST136-CD-OPR-ETRD-MCP
                                         :CD-OPR-ETRD-MCP-N
                     , NR_VRS_OPR_ETRD = :SGTST136-NR-VRS-OPR-ETRD
                                         :NR-VRS-OPR-ETRD-N
                     , CD_USU          = :SGTST136-CD-USU
                     , CD_OGM_ERRO     = :SGTST136-CD-OGM-ERRO
                                         :CD-OGM-ERRO-N
                     , TX_ERRO_PRCT    = :SGTST136-TX-ERRO-PRCT
                                         :TX-ERRO-PRCT-N
                 WHERE DT_PRCT         = :SGTST136-DT-PRCT
                   AND NR_SEQL_PRCT    = :SGTST136-NR-SEQL-PRCT
           END-EXEC

      * MOCK-POINT MCKV5
           EVALUATE SQLCODE
               WHEN 0
                    CONTINUE
               WHEN +100
                    INITIALIZE CHAVE-CACHE
                    SET ERRO-CODIFICADO TO TRUE
                    MOVE 0100           TO CD-RTN-PGM
                    MOVE 'Alteracao nao efetuada, registro inexistente'
                                        TO MSG-ERRO
                    PERFORM 999001-ERRO-001
               WHEN OTHER
                    PERFORM 999002-ERRO-DB2
           END-EVALUATE

           PERFORM 980101-GUARDA-CACHE
           PERFORM 980102-GRAVA-LOG
           .
       500000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       700000-CONSULTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO PSC-ERRO-PGM

      *    Campos nulos sao inicializados na consulta
           INITIALIZE SGTST136-CD-OGM-ERRO
                      SGTST136-TX-ERRO-PRCT

           EXEC SQL
                SELECT DT_CPR
                     , CD_ITMT_FNCO
                     , VL_PU_CPR
                     , CD_TIP_PRCT
                     , CD_EST_PRCT
                     , TS_PRCT
                     , CD_PGM
                     , CD_CTRA_TIT_VL
                     , CD_OPR_ETRD_MCP
                     , NR_VRS_OPR_ETRD
                     , CD_USU
                     , CD_OGM_ERRO
                     , TX_ERRO_PRCT
                  INTO  :SGTST136-DT-CPR
                      , :SGTST136-CD-ITMT-FNCO
                      , :SGTST136-VL-PU-CPR
                      , :SGTST136-CD-TIP-PRCT
                      , :SGTST136-CD-EST-PRCT
                      , :SGTST136-TS-PRCT
                      , :SGTST136-CD-PGM
                      , :SGTST136-CD-CTRA-TIT-VL
                        :CD-CTRA-TIT-VL-N
                      , :SGTST136-CD-OPR-ETRD-MCP
                        :CD-OPR-ETRD-MCP-N
                      , :SGTST136-NR-VRS-OPR-ETRD
                        :NR-VRS-OPR-ETRD-N
                      , :SGTST136-CD-USU
                      , :SGTST136-CD-OGM-ERRO
                        :CD-OGM-ERRO-N
                      , :SGTST136-TX-ERRO-PRCT
                        :TX-ERRO-PRCT-N
                   FROM DB2SGT.PRCT_CTRA_TSR
                  WHERE DT_PRCT      = :SGTST136-DT-PRCT
                    AND NR_SEQL_PRCT = :SGTST136-NR-SEQL-PRCT
           END-EXEC

      * MOCK-POINT MCKV6
           EVALUATE SQLCODE
               WHEN 0
                    CONTINUE
               WHEN +100
                    INITIALIZE SGTST136-AREA-DADOS
                    SET ERRO-CODIFICADO TO TRUE
                    MOVE 0100           TO CD-RTN-PGM
                    MOVE 'Registro inexistente.' TO MSG-ERRO
                    PERFORM 999001-ERRO-001
               WHEN OTHER
                    PERFORM 999002-ERRO-DB2
           END-EVALUATE

           PERFORM 980101-GUARDA-CACHE
           .
       700000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       980101-GUARDA-CACHE SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO PSC-ERRO-PGM

           MOVE SGTST136-CHAVE         TO CHAVE-CACHE
           MOVE SGTST136-AREA-DADOS    TO DADOS-CACHE
           .
       980101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980102-GRAVA-LOG                SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO PSC-ERRO-PGM

           INITIALIZE DADOS-LOG
           SET  SGTS014G-INCLUSAO           TO TRUE
           MOVE 'DB2SGT.PRCT_CTRA_TSR'      TO SGTS014G-NM-ENT

           IF  EXECUCAO-ONLINE
               MOVE SGTST136-CD-CHV-USU     TO SGTS014G-CD-CHV-USU
           ELSE
               MOVE 'SYSTEM'                TO SGTS014G-CD-CHV-USU
           END-IF

           MOVE 'SGTST136'                  TO SGTS014G-NM-PGM

           MOVE SGTST136-DT-PRCT            TO LOG-DT-PRCT
           MOVE SGTST136-NR-SEQL-PRCT       TO LOG-NR-SEQL-PRCT
           STRING LOG-DT-PRCT ';' LOG-NR-SEQL-PRCT
                  DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR
           END-STRING

           MOVE LENGTH OF DADOS-LOG    TO EIBCALEN
           CALL SGTS014G USING DFHEIBLK DADOS-LOG

      * MOCK-POINT MCKV7
           IF  SGTS014G-CD-RTN-PGM  NOT EQUAL ZEROS
               SET  ERRO-DE-PGM             TO TRUE
               MOVE SGTS014G                TO PGM-ERRO
               MOVE 0112                    TO CD-RTN-PGM
               MOVE 'Erro na gravacao das informacoes do Log.'
                                            TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           .
       980102-SAI.
           EXIT.
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

           IF  CD-RTN-PGM EQUAL ZEROS
               MOVE 8888      TO CD-RTN-PGM
           END-IF

           MOVE RTN-PGM                TO SGTST136-RTN-PGM
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

      *---------------------- FIM SGTST136 ----------------------------
