      *-----------------------------------------------------------------
      * PROGRAMA..: SGTST012
      * ANALISTA..: F2054109 - CINTIA IGLESIAS
      * AUTOR.....: F8186823 - PAULO SERGIO MIRANDA DUARTE
      * DATA......: 10.04.2015
      * OBJETIVO..: PERSISTENCIA DA TABELA CTRA_TIT_VL_MNTR
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS003 04.05.2018 - F2054109 - Inclusão do campo
      *                                 SGTST012-IN-RCPO-JDCL
      * VRS003 19.03.2018 - F9188847 - ALTERAÇÃO NO BOOK DO SGTS014G
      * VRS002 13.09.2015 - C1077250 - GRAVAR MESMO EST NO HST
      * VRS001 10.04.2015 - F8186823 - DE 2015/134565 - IMPLANTACAO
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTST012.

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
       77  CTE-INICIO-SS               PIC  X(40) VALUE
               '*** Storage Section comeca aqui ***'.
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTST012 ***'.
       77  CTE-VERS     PIC  X(06) VALUE 'VRS003'.

       01  SGTST019                 PIC  X(08) VALUE 'SGTST019'.

      * Registro da tabela

       01  REG-CACHE.
           03  CHAVE-CACHE.
               05  CD-CTRA-TIT-VL            PIC S9(9) COMP-5 VALUE 0.
               05  FILLER                    PIC  X(20) VALUE ' '.
           03  DADOS-CACHE.
               05  NM-CTRA-TIT-MNTR          PIC  X(50) VALUE ' '.
               05  DT-INC-VGC-CTRA           PIC  X(10) VALUE ' '.
               05  DT-INC-VGC-CTRA-R REDEFINES
                   DT-INC-VGC-CTRA.
                   07 DIA                    PIC  9(2).
                   07 FILLER                 PIC  X(1).
                   07 MES                    PIC  9(2).
                   07 FILLER                 PIC  X(1).
                   07 ANO                    PIC  9(4).
               05  DT-FIM-VGC-CTRA           PIC  X(10) VALUE ' '.
               05  DT-FIM-VGC-CTRA-R REDEFINES
                   DT-FIM-VGC-CTRA.
                   07 DIA                    PIC  9(2).
                   07 FILLER                 PIC  X(1).
                   07 MES                    PIC  9(2).
                   07 FILLER                 PIC  X(1).
                   07 ANO                    PIC  9(4).
               05  CD-UOR-GST                PIC S9(9) COMP-5 VALUE 0.
               05  CD-UOR-OPR                PIC S9(9) COMP-5 VALUE 0.
               05  CD-EST-CTRA-ATI           PIC S9(4) COMP-5 VALUE 0.
               05  CD-USU-MNTC-CTRA          PIC  X(8) VALUE ' '.
               05  CD-USU-CNFC-CTRA          PIC  X(8) VALUE ' '.
               05  CD-PGM-CTRA-MNTR          PIC  X(8) VALUE ' '.

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
       01  DT-FIM-VGC-CTRA-N                PIC S9(4) COMP-5 VALUE 0.
       01  CD-UOR-GST-N                     PIC S9(4) COMP-5 VALUE 0.
       01  CD-UOR-OPR-N                     PIC S9(4) COMP-5 VALUE 0.
       01  CD-USU-CNFC-CTRA-N               PIC S9(4) COMP-5 VALUE 0.

      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG            PIC  9(10) VALUE 0.

      *-- GRAVA HISTORICO EM HST_CTRA_TIT_VL
       01 SGTST019-DADOS.
-INC SGTKT019


      * Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

      * Subrotinas Utilizadas

       01  SGTS014G VALUE 'SGTS014G'         PIC X(8).

      *-----------------------------------------------------------------
      *    Áreas e Books Utilizados no Tratamento da Mensageria
      *-----------------------------------------------------------------
       01  IC-TAB                      PIC  9(02) VALUE ZEROS.


      * Books da definicao de dados a serem usados na subrotina de erros
-INC HLPKE001

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> HLPKW999
      *    Variaveis Auxiliares para Captura de Erro CICS
      *    e Adicionar Variaveis Monitoradas
       01  MSGKW999-VRV-ERRO-CICS.
           03  MSGKW999-RESP1-X8            PIC  X(08) VALUE SPACES.
           03  MSGKW999-RESP2-X8            PIC  X(08) VALUE SPACES.
       01  MSGKW999-VRV-ALINHA-NUMERO.
           03  MSGKW999-NUMERO-Z17          PIC  ZZZZZZZZZZZZZZZZ9.
           03  MSGKW999-NUMERO-X17          PIC  X(17) VALUE SPACES.
           03  MSGKW999-BRANCOS             PIC  9(08) VALUE ZEROS.
       01  MSGKW999-VRV-AUX.
           03  MSGKW999-NM-VRV-MNT     PIC  X(030) VALUE SPACES.
           03  MSGKW999-VL-VRV-MNT     PIC  X(100) VALUE SPACES.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC  X(40) VALUE
               '*** Storage Section termina aqui ***'.
      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTKT012
      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------

      * teste obrigatorio do tamanho da linkage nao retirar
           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               MOVE  0220              TO  CD-RTN-PGM
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
              WHEN SGTST012-INCLUIR
                   PERFORM 400000-INCLUIR
              WHEN SGTST012-ALTERAR
                   PERFORM 500000-ALTERAR
              WHEN SGTST012-EXCLUIR
                   PERFORM 600000-EXCLUIR
              WHEN SGTST012-CONSULTAR
                   IF  CHAVE-CACHE = SGTST012-CHAVE
                       MOVE DADOS-CACHE TO SGTST012-AREA-DADOS
                   ELSE
                       PERFORM 700000-CONSULTA
                   END-IF
              WHEN SGTST012-ALTERAR-EST
                   PERFORM 800000-ALTERAR-EST
           END-EVALUATE

           GOBACK
           .
      *-----------------------------------------------------------------
       100000-VALIDA-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO PSC-ERRO-PGM

           INITIALIZE SGTST012-RTN-PGM

           IF  NOT SGTST012-INCLUIR
           AND NOT SGTST012-EXCLUIR
           AND NOT SGTST012-ALTERAR
           AND NOT SGTST012-CONSULTAR
           AND NOT SGTST012-ALTERAR-EST
               MOVE  0116              TO  CD-RTN-PGM
               MOVE  'Ação inválida.'  TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 130000-VALIDA-CAMPOS

           IF  SGTST012-INCLUIR
           OR  SGTST012-ALTERAR
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

           IF  SGTST012-CD-CTRA-TIT-VL EQUAL ZEROS
                MOVE  0001             TO  CD-RTN-PGM
                MOVE 'Codigo carteira titulo valor invalido' TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTST012-INCLUIR
           OR  SGTST012-ALTERAR
               IF  SGTST012-NM-CTRA-TIT-MNTR EQUAL SPACES
                   MOVE  0822          TO  CD-RTN-PGM
                   MOVE 'Nome carteira titulo monetario invalido'
                                         TO MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF

               IF  SGTST012-DT-INC-VGC-CTRA EQUAL SPACES
                   MOVE  0823          TO  CD-RTN-PGM
                   MOVE 'Data inicio vigencia carteira invalido'
                                         TO MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF

               IF  SGTST012-CD-EST-CTRA-ATI EQUAL ZEROS
                   MOVE  0824          TO  CD-RTN-PGM
                   MOVE 'Codigo estado carteira ativo invalido'
                                         TO MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF

               IF  SGTST012-CD-USU-MNTC-CTRA EQUAL SPACES
                   MOVE  0825          TO  CD-RTN-PGM
                   MOVE 'Codigo usuario manutencao carteira invalido'
                                         TO MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF

               IF  SGTST012-CD-PGM-CTRA-MNTR EQUAL SPACES
                   MOVE  0826          TO  CD-RTN-PGM
                   MOVE 'Codigo programa carteira monetario invalido'
                                         TO MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF

               IF SGTST012-IN-RCPO-JDCL EQUAL SPACES
                  MOVE 'N'      TO    SGTST012-IN-RCPO-JDCL
               END-IF
           END-IF

           IF  SGTST012-EXCLUIR
               IF  SGTST012-CD-USU-MNTC-CTRA EQUAL SPACES
                   MOVE  0825          TO  CD-RTN-PGM
                   MOVE 'Codigo usuario manutencao carteira invalido'
                                         TO MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF

           IF  SGTST012-ALTERAR-EST
               IF  SGTST012-CD-EST-CTRA-ATI EQUAL ZEROS
                   MOVE  0824          TO  CD-RTN-PGM
                   MOVE 'Codigo estado carteira ativo invalido'
                                         TO MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF

               IF  SGTST012-CD-USU-CNFC-CTRA EQUAL SPACES
                   MOVE  0825          TO  CD-RTN-PGM
                   MOVE 'Codigo usuario manutencao carteira invalido'
                                         TO MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF

               IF  SGTST012-CD-PGM-CTRA-MNTR EQUAL SPACES
                   MOVE  0826          TO  CD-RTN-PGM
                   MOVE 'Codigo programa carteira monetario invalido'
                                         TO MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF

           IF  SGTST012-DT-INC-VGC-CTRA EQUAL 'CURRENT'
               MOVE DT-SQL-ATU         TO SGTST012-DT-INC-VGC-CTRA
           END-IF

           IF  SGTST012-DT-FIM-VGC-CTRA EQUAL 'CURRENT'
               MOVE DT-SQL-ATU         TO SGTST012-DT-FIM-VGC-CTRA
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

           IF  RETURN-CODE      NOT EQUAL ZEROS
               SET   ERRO-DE-PGM       TO  TRUE
               MOVE  0050              TO  CD-RTN-PGM
               MOVE  'Erro ao obter data e hora atual.'
                                         TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       160000-TRATA-NULIDADE SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO PSC-ERRO-PGM
           IF  SGTST012-DT-FIM-VGC-CTRA EQUAL SPACES
               MOVE -1                 TO DT-FIM-VGC-CTRA-N
           END-IF

           IF  SGTST012-CD-UOR-GST EQUAL ZEROS
               MOVE -1                 TO CD-UOR-GST-N
           END-IF

           IF  SGTST012-CD-UOR-OPR EQUAL ZEROS
               MOVE -1                 TO CD-UOR-OPR-N
           END-IF

           IF  SGTST012-CD-USU-CNFC-CTRA EQUAL SPACES
               MOVE -1                 TO CD-USU-CNFC-CTRA-N
           END-IF

           .
       160000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       400000-INCLUIR SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO PSC-ERRO-PGM
      *
      * Desligar erro automatico do DB2
      *
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC
      *
           EXEC SQL
                INSERT INTO DB2SGT.CTRA_TIT_VL_MNTR
                ( CD_CTRA_TIT_VL
                , NM_CTRA_TIT_MNTR
                , DT_INC_VGC_CTRA
                , DT_FIM_VGC_CTRA
                , CD_UOR_GST
                , CD_UOR_OPR
                , CD_EST_CTRA_ATI
                , CD_USU_MNTC_CTRA
                , CD_USU_CNFC_CTRA
                , CD_PGM_CTRA_MNTR
                , IN_RCPO_JDCL )
                VALUES
                ( :SGTST012-CD-CTRA-TIT-VL
                , :SGTST012-NM-CTRA-TIT-MNTR
                , :SGTST012-DT-INC-VGC-CTRA
                , :SGTST012-DT-FIM-VGC-CTRA
                  :DT-FIM-VGC-CTRA-N
                , :SGTST012-CD-UOR-GST
                  :CD-UOR-GST-N
                , :SGTST012-CD-UOR-OPR
                  :CD-UOR-OPR-N
                , :SGTST012-CD-EST-CTRA-ATI
                , :SGTST012-CD-USU-MNTC-CTRA
                , :SGTST012-CD-USU-CNFC-CTRA
                  :CD-USU-CNFC-CTRA-N
                , :SGTST012-CD-PGM-CTRA-MNTR
                , :SGTST012-IN-RCPO-JDCL)
           END-EXEC
      *
      * Ligar erro automatico do DB2
      *
           EXEC SQL WHENEVER SQLERROR   GO TO 999002-ERRO-DB2 END-EXEC
      *
           EVALUATE SQLCODE
               WHEN -803
                   MOVE 'Inclusao nao efetuada, registro existente'
                                    TO MSG-ERRO
                   MOVE SQLCODE                 TO CD-RTN-PGM
                   PERFORM 999001-ERRO-001
               WHEN 0
                    CONTINUE
               WHEN OTHER
      * Aciona o tratamento padrao de erro padrao para os demais casos
                    PERFORM 999002-ERRO-DB2
           END-EVALUATE

           PERFORM 980101-GUARDA-CACHE
           INITIALIZE DADOS-LOG
           MOVE 'I'                    TO SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG
           .
       400000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       500000-ALTERAR SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO PSC-ERRO-PGM

           EXEC SQL
             UPDATE DB2SGT.CTRA_TIT_VL_MNTR
             SET NM_CTRA_TIT_MNTR   = :SGTST012-NM-CTRA-TIT-MNTR
               , DT_INC_VGC_CTRA    = :SGTST012-DT-INC-VGC-CTRA
               , DT_FIM_VGC_CTRA    = :SGTST012-DT-FIM-VGC-CTRA
                                      :DT-FIM-VGC-CTRA-N
               , CD_UOR_GST         = :SGTST012-CD-UOR-GST
                                      :CD-UOR-GST-N
               , CD_UOR_OPR         = :SGTST012-CD-UOR-OPR
                                      :CD-UOR-OPR-N
               , CD_EST_CTRA_ATI    = :SGTST012-CD-EST-CTRA-ATI
               , CD_USU_MNTC_CTRA   = :SGTST012-CD-USU-MNTC-CTRA
               , CD_USU_CNFC_CTRA   = :SGTST012-CD-USU-CNFC-CTRA
                                      :CD-USU-CNFC-CTRA-N
               , CD_PGM_CTRA_MNTR   = :SGTST012-CD-PGM-CTRA-MNTR
               , IN_RCPO_JDCL       = :SGTST012-IN-RCPO-JDCL
                WHERE CD_CTRA_TIT_VL     = :SGTST012-CD-CTRA-TIT-VL
           END-EXEC

           IF  SQLCODE EQUAL +100
               INITIALIZE CHAVE-CACHE
               MOVE 'Alteracao nao efetuada, registro inexistente'
                                             TO MSG-ERRO
               MOVE SQLCODE                   TO CD-RTN-PGM
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
       600000-EXCLUIR SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO PSC-ERRO-PGM

           INITIALIZE DADOS-LOG
           MOVE 'E'                    TO SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG

           EXEC SQL
                DELETE FROM DB2SGT.CTRA_TIT_VL_MNTR
                WHERE CD_CTRA_TIT_VL     = :SGTST012-CD-CTRA-TIT-VL
           END-EXEC

           INITIALIZE REG-CACHE

           IF  SQLCODE EQUAL +100
               MOVE 'Exclusao nao efetuada, registro inexistente'
                                             TO MSG-ERRO
               MOVE SQLCODE                   TO CD-RTN-PGM
               PERFORM 999001-ERRO-001
           END-IF
           .
       600000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       700000-CONSULTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO PSC-ERRO-PGM

      *    Campos nulos sao inicializados na consulta
           INITIALIZE SGTST012-DT-FIM-VGC-CTRA
                      SGTST012-CD-UOR-GST
                      SGTST012-CD-UOR-OPR
                      SGTST012-CD-USU-CNFC-CTRA

           EXEC SQL
                 SELECT NM_CTRA_TIT_MNTR
                      , DT_INC_VGC_CTRA
                      , DT_FIM_VGC_CTRA
                      , CD_UOR_GST
                      , CD_UOR_OPR
                      , CD_EST_CTRA_ATI
                      , CD_USU_MNTC_CTRA
                      , CD_USU_CNFC_CTRA
                      , CD_PGM_CTRA_MNTR
                      , IN_RCPO_JDCL
                   INTO  :SGTST012-NM-CTRA-TIT-MNTR
                       , :SGTST012-DT-INC-VGC-CTRA
                       , :SGTST012-DT-FIM-VGC-CTRA
                         :DT-FIM-VGC-CTRA-N
                       , :SGTST012-CD-UOR-GST
                         :CD-UOR-GST-N
                       , :SGTST012-CD-UOR-OPR
                         :CD-UOR-OPR-N
                       , :SGTST012-CD-EST-CTRA-ATI
                       , :SGTST012-CD-USU-MNTC-CTRA
                       , :SGTST012-CD-USU-CNFC-CTRA
                         :CD-USU-CNFC-CTRA-N
                       , :SGTST012-CD-PGM-CTRA-MNTR
                       , :SGTST012-IN-RCPO-JDCL
                       FROM DB2SGT.CTRA_TIT_VL_MNTR
                WHERE CD_CTRA_TIT_VL     = :SGTST012-CD-CTRA-TIT-VL
           END-EXEC

           IF  SQLCODE = +100
               INITIALIZE SGTST012-AREA-DADOS
               MOVE 'Carteira titulo valor monetario inexistente'
                                         TO MSG-ERRO
               MOVE SQLCODE              TO CD-RTN-PGM
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GUARDA-CACHE
           .
       700000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       980101-GUARDA-CACHE SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO PSC-ERRO-PGM

           MOVE SGTST012-CHAVE         TO CHAVE-CACHE
           MOVE SGTST012-AREA-DADOS         TO DADOS-CACHE
           .
       980101-SAI.
           EXIT.
      *-----------------------------------------------------------------
       800000-ALTERAR-EST SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO PSC-ERRO-PGM

           EXEC SQL
             UPDATE DB2SGT.CTRA_TIT_VL_MNTR
             SET CD_EST_CTRA_ATI    = :SGTST012-CD-EST-CTRA-ATI
               , CD_USU_CNFC_CTRA   = :SGTST012-CD-USU-CNFC-CTRA
               , CD_PGM_CTRA_MNTR   = :SGTST012-CD-PGM-CTRA-MNTR
                WHERE CD_CTRA_TIT_VL     = :SGTST012-CD-CTRA-TIT-VL
           END-EXEC

           EVALUATE SQLCODE
           WHEN 0
               PERFORM 801000-GRAVA-HST
           WHEN +100
               INITIALIZE CHAVE-CACHE
               MOVE 'Alteracao nao efetuada, registro inexistente'
                                             TO MSG-ERRO
               MOVE SQLCODE                  TO CD-RTN-PGM
               PERFORM 999001-ERRO-001
           END-EVALUATE

           PERFORM 980101-GUARDA-CACHE
           INITIALIZE DADOS-LOG
           MOVE 'A'                    TO SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG
           .
       500000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       801000-GRAVA-HST SECTION.
      *-----------------------------------------------------------------
           PERFORM 700000-CONSULTA

           INITIALIZE SGTST019-DADOS
              REPLACING  ALPHANUMERIC BY SPACES
                         NUMERIC      BY ZEROS

           MOVE 1                         TO SGTST019-ACAO
           MOVE SGTST012-CD-CTRA-TIT-VL   TO SGTST019-CD-CTRA-TIT-VL
           MOVE 'CURRENT'                 TO SGTST019-TS-HST-CTRA-TIT
           MOVE SGTST012-NM-CTRA-TIT-MNTR TO SGTST019-NM-CTRA-TIT-MNTR
           MOVE SGTST012-DT-INC-VGC-CTRA  TO SGTST019-DT-INC-VGC-CTRA
           MOVE SGTST012-DT-FIM-VGC-CTRA  TO SGTST019-DT-FIM-VGC-CTRA
           MOVE SGTST012-CD-UOR-GST       TO SGTST019-CD-UOR-GST
           MOVE SGTST012-CD-UOR-OPR       TO SGTST019-CD-UOR-OPR
           MOVE SGTST012-CD-EST-CTRA-ATI  TO SGTST019-CD-EST-CTRA-ATI
           MOVE SGTST012-CD-USU-MNTC-CTRA TO SGTST019-CD-USU-MNTC-CTRA
           MOVE SGTST012-CD-USU-CNFC-CTRA TO SGTST019-CD-USU-CNFC-CTRA
           MOVE 'SGTST012'                TO SGTST019-CD-PGM-CTRA-MNTR

           MOVE LENGTH OF SGTST019-DADOS TO EIBCALEN
           CALL SGTST019 USING DFHEIBLK SGTST019-DADOS

           IF SGTST019-CD-RTN-PGM NOT EQUAL ZEROS
               INITIALIZE CHAVE-CACHE
               MOVE  0827          TO  CD-RTN-PGM
               MOVE 'Gravacao de histórico nao efetuada.'
                                             TO MSG-ERRO
               MOVE SGTST019-CD-RTN-PGM       TO CD-RTN-PGM
               PERFORM 999001-ERRO-001
           END-IF

           .
       500000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       980102-GRAVA-LOG SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO PSC-ERRO-PGM

           MOVE 'CTRA_TIT_VL_MNTR  '        TO SGTS014G-NM-ENT

           IF  EXECUCAO-ONLINE
               IF  SGTST012-INCLUIR
               OR  SGTST012-ALTERAR
               OR  SGTST012-EXCLUIR
                   MOVE  SGTST012-CD-USU-MNTC-CTRA
                                       TO  SGTS014G-CD-CHV-USU
               ELSE
                   IF  SGTST012-ALTERAR-EST
                       MOVE  SGTST012-CD-USU-CNFC-CTRA
                                       TO  SGTS014G-CD-CHV-USU
                   END-IF
               END-IF
           ELSE
               MOVE  'SISTEMA'         TO  SGTS014G-CD-CHV-USU
           END-IF

           MOVE 'SGTST012'                  TO SGTS014G-NM-PGM
           MOVE SGTST012-CD-CTRA-TIT-VL        TO CD-CTRA-TIT-VL-LOG

           STRING CD-CTRA-TIT-VL-LOG
               DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR

           MOVE LENGTH OF DADOS-LOG    TO EIBCALEN
           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM  NOT EQUAL ZEROS
               SET   ERRO-DE-PGM       TO  TRUE
               MOVE  0112              TO  CD-RTN-PGM
               MOVE  'Erro na gravação das informações do Log.'
                                       TO  MSG-ERRO-PGM
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
           MOVE RTN-PGM                      TO SGTST012-RTN-PGM
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

      *-----------------------------------------------------------------
      *    As Sections e Books abaixo fazem o tratamento de erro
      *    através do Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-TRATAR-ERRO-MSG          SECTION.
      *--> Envia a Ocorrência de Erro para o Sistema de Mensageria
      *-----------------------------------------------------------------

      *--> Alterar o MOVE conforme o Book do módulo
           MOVE  SGTST012-TIP-RTN-PGM  TO  TIP-RTN-PGM
      *--> Alterar os Campos de Entrada conforme o Book do módulo
           PERFORM  999100-MONITORAR-ENTRADA

      *--> Movimenta Campos Básicos da Ocorrência de Erro
           MOVE  CD-RTN-PGM            TO  MSG1-CD-ERRO-TS
           MOVE  PSC-ERRO-PGM          TO  MSG1-CD-SEQL-ERRO
           MOVE  MSG-ERRO              TO  MSG1-TX-ERRO-TS

      *--> Realiza a tradução do tipo de erro do COMMAREA
      *    para tipos de erro da Mensageria
           EVALUATE  TRUE
               WHEN  ERRO-DE-DB2
                     PERFORM  999200-MONITORAR-SQL
               WHEN  ERRO-DE-CICS
                     PERFORM  999300-MONITORAR-CICS
               WHEN  ERRO-DE-PGM
                     SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                     PERFORM  999400-MONITORAR-VARIAVEIS
               WHEN  ERRO-CODIFICADO
                     SET   ERRO-PROGRAMADO
                                       TO TRUE
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

      *--> Grava o erro no Sistema MSG
           PERFORM  MSG1-NTF-MSG
           .
       999000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999100-MONITORAR-ENTRADA        SECTION.
      *--> Monitora as variáveis de entrada desde módulo
      *    Alterar os Campos conforme o Book
      *-----------------------------------------------------------------

           MOVE 'SGTST012-CD-CHV-USU'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST012-CD-CHV-USU           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST012-ACAO'                TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST012-ACAO                 TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST012-CD-CTRA-TIT-VL'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST012-CD-CTRA-TIT-VL       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST012-NM-CTRA-TIT-MNTR'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST012-NM-CTRA-TIT-MNTR     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST012-DT-INC-VGC-CTRA'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST012-DT-INC-VGC-CTRA      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST012-DT-FIM-VGC-CTRA'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST012-DT-FIM-VGC-CTRA      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST012-CD-UOR-GST'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST012-CD-UOR-GST           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST012-CD-UOR-OPR'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST012-CD-UOR-OPR           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST012-CD-EST-CTRA-ATI'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST012-CD-EST-CTRA-ATI      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST012-CD-USU-MNTC-CTRA'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST012-CD-USU-MNTC-CTRA     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST012-CD-USU-CNFC-CTRA'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST012-CD-USU-CNFC-CTRA     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT
           .
           MOVE 'SGTST012-CD-PGM-CTRA-MNTR'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST012-CD-PGM-CTRA-MNTR     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST012-IN-RCPO-JDCL'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST012-IN-RCPO-JDCL         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT
           .
       999100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999200-MONITORAR-SQL            SECTION.
      *--> Monitora as variáveis do SQLCA (SQL Communication Area)
      *-----------------------------------------------------------------

      *--> Aciona a Monitoração das Variáveis de Erro SQL no MSGS0001
           SET   ERRO-SQL              TO  TRUE

           MOVE  SQLCA                 TO  MSG1-SQLCA

           MOVE 'SQLCODE'              TO  MSGKW999-NM-VRV-MNT
           MOVE  SQLCODE               TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SQLSTATE'             TO  MSGKW999-NM-VRV-MNT
           MOVE  SQLSTATE              TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           PERFORM  VARYING  IC-TAB
                    FROM  0001 BY 0001
                    UNTIL  IC-TAB          GREATER 0012
                    IF  HLPKE001-TAB-ERRO-TX(IC-TAB)
                                           NOT EQUAL SPACES
                        STRING  'DSNTIAR-TX-ERRO(' IC-TAB ')'
                                DELIMITED BY SIZE
                                INTO       MSGKW999-NM-VRV-MNT
                        END-STRING
                        MOVE  HLPKE001-TAB-ERRO-TX(IC-TAB)
                                       TO  MSGKW999-VL-VRV-MNT
                        PERFORM  999500-ADICIONA-VRV-MNT
                    END-IF
           END-PERFORM

           IF  SQLERRML                GREATER  ZEROS
               MOVE 'SQLERRML'         TO  MSGKW999-NM-VRV-MNT
               MOVE  SQLERRML          TO  MSGKW999-VL-VRV-MNT
               PERFORM  999500-ADICIONA-VRV-MNT
           END-IF

           IF  SQLWARN0                EQUAL 'W'
               MOVE 'SQLWARN'          TO  MSGKW999-NM-VRV-MNT
               MOVE  SQLWARN(2:7)      TO  MSGKW999-VL-VRV-MNT
               PERFORM  999500-ADICIONA-VRV-MNT
           END-IF
           .
       999200-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999300-MONITORAR-CICS           SECTION.
      *--> Monitora as variáveis ABEND ou CONDITION do CICS
      *-----------------------------------------------------------------

      *--> Aciona a Monitoração das Variáveis de Erro CICS no MSGS0001
           SET   ERRO-CICS             TO  TRUE

      *--> Tratar e Adicionar EIBRESP 01 e EIBRESP 02
           MOVE  EIBRESP               TO  MSG1-RESP1
           MOVE  EIBRESP2              TO  MSG1-RESP2

           MOVE  MSG1-RESP1            TO  MSGKW999-NUMERO-Z17
           PERFORM  999600-ALINHA-NUMERO-ESQUERDA
           MOVE  MSGKW999-NUMERO-X17   TO  MSGKW999-RESP1-X8

           MOVE  MSG1-RESP2            TO  MSGKW999-NUMERO-Z17
           PERFORM  999600-ALINHA-NUMERO-ESQUERDA
           MOVE  MSGKW999-NUMERO-X17    TO  MSGKW999-RESP2-X8

           MOVE 'EIBRESP 01'           TO  MSGKW999-NM-VRV-MNT
           MOVE  MSGKW999-RESP1-X8     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'EIBRESP 02'           TO  MSGKW999-NM-VRV-MNT
           MOVE  MSGKW999-RESP2-X8     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

      *--> Adicionar Outros Campos EIB (Exec Interface Block)
           MOVE 'EIBFN'                TO  MSGKW999-NM-VRV-MNT
           MOVE  EIBFN                 TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'EIBDS'                TO  MSGKW999-NM-VRV-MNT
           MOVE  EIBDS                 TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'EIBRSRCE'             TO  MSGKW999-NM-VRV-MNT
           MOVE  EIBRSRCE              TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'HLPS0201-TX-EIBFN'    TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPS0201-TX-EIBFN     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'HLPS0201-TX-EIBRESP'  TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPS0201-TX-EIBRESP   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'Nome CICS'            TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPKE001-NM-CICS      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'Usuario CICS'         TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPKE001-CHV-USU      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT
           .
       999300-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999400-MONITORAR-VARIAVEIS       SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  PGM-ERRO
               WHEN  SGTS014G
                     MOVE  'SGTS014G-CD-TIP-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-CD-TIP-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE  'SGTS014G-CD-CHV-USU'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-CD-CHV-USU
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE  'SGTS014G-NM-PGM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-NM-PGM
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE  'SGTS014G-CHV-PRMR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-CHV-PRMR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

               WHEN  OTHER
                    CONTINUE
           END-EVALUATE

           .
       999400-SAI.
           EXIT.


      *-----------------------------------------------------------------
       999500-ADICIONA-VRV-MNT         SECTION.
      *-----------------------------------------------------------------
           IF  MSGKW999-NM-VRV-MNT         EQUAL SPACES
               MOVE  SPACES            TO  MSGKW999-VL-VRV-MNT
           ELSE
               IF  MSG1-QT-VRV-MNT         LESS 100
                   ADD   0001          TO MSG1-QT-VRV-MNT
                   MOVE  MSGKW999-NM-VRV-MNT
                     TO  MSG1-NM-VRV-MON(MSG1-QT-VRV-MNT)
                   MOVE  MSGKW999-VL-VRV-MNT
                     TO  MSG1-VL-VRV-MON(MSG1-QT-VRV-MNT)
               END-IF
               MOVE SPACES             TO MSGKW999-NM-VRV-MNT
                                          MSGKW999-VL-VRV-MNT
           END-IF
           .
       999500-ADICIONA-VRV-MNT-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999600-ALINHA-NUMERO-ESQUERDA   SECTION.
      *-----------------------------------------------------------------
           MOVE  MSGKW999-NUMERO-Z17   TO  MSGKW999-NUMERO-X17

           UNSTRING  MSGKW999-NUMERO-X17
                     DELIMITED BY ALL ' '
                     INTO MSGKW999-BRANCOS MSGKW999-NUMERO-X17
           END-UNSTRING
           .
       999600-SAI.
           EXIT.

      *----------------------------------------
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
      *----------------------------------------
-INC MSGKPMG1

      *---------------------- FIM SGTST012 ----------------------------