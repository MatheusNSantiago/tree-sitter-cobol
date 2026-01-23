      *-----------------------------------------------------------------
      * PROGRAMA..: SGTST139
      * ANALISTA..: H464259  - NEWTON DE LIMA CARLINI
      * AUTOR.....: H464259  - NEWTON DE LIMA CARLINI
      * DATA......: 21.03.2025
      * OBJETIVO..: PERSISTENCIA DA TABELA EST_EVT_TIT_EXCD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 21.03.2025 - H464259  - IMPLANTACAO
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTST139.

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
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTST139 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

      * Registro da tabela

       01  REG-CACHE.
           03  CHAVE-CACHE.
               05  CD-ITMT-FNCO              PIC  X(50) VALUE ' '.
               05  CD-GR-EVT-FDAR            PIC S9(4) COMP-5 VALUE 0.
               05  NR-SEQL-EVT-EXCD          PIC S9(4) COMP-5 VALUE 0.
               05  TS-EST-EVT-EXCD           PIC  X(026) VALUE ' '.
               05  FILLER                    PIC  X(20) VALUE ' '.
           03  DADOS-CACHE.
               05  CD-USU-ULT-ATL            PIC  X(8) VALUE ' '.
               05  CD-EST-EVT-EXCD           PIC S9(4) COMP-5 VALUE 0.
               05  TX-JST-EVT-EXCD           PIC  X(200) VALUE ' '.
               05  TS-EST-EVT-EXCD-S         PIC  X(26) VALUE ' '.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTS014G                PIC  X(08) VALUE 'SGTS014G'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * Area de trabalho

       01  QT-REG-SELD                      PIC S9(4) COMP-5 VALUE 0.
       01  TS-SQL-ATU                       PIC  X(26) VALUE ' '.

      * Indicadores de nulidades

       01  QT-REG-SELD-NULO                 PIC S9(4) COMP-5 VALUE 0.
       01  TX-JST-EVT-EXCD-N                PIC S9(4) COMP-5 VALUE 0.

      *--> Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-ITMT-FNCO-LOG        PIC  X(50) VALUE ' '.
           03  CD-GR-EVT-FDAR-LOG      PIC  9(05) VALUE 0.
           03  NR-SEQL-EVT-EXCD-LOG    PIC  9(05) VALUE 0.
           03  TS-EST-EVT-EXCD-LOG     PIC  X(26) VALUE ' '.


      *--> Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

      * Books da definicao de dados a serem usados na subrotina de erros
-INC HLPKE001


           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTKT139
      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------
           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               MOVE  0220              TO  CD-RTN-PGM
               MOVE 'Area com tamanho invalido - verifique book/local'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           MOVE 0001                   TO PSC-ERRO-PGM

           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      *--> Habilita Testes no Cobolito
-INC ALMKCBL2
      * WHENEVER DB2
-INC HLPKE002
      * WHENEVER CICS
-INC HLPKE003

           PERFORM 100000-VALIDA-PARAMETRO

           EVALUATE TRUE
              WHEN SGTST139-INCLUIR
                   PERFORM 400000-INCLUIR
              WHEN SGTST139-ALTERAR
                   PERFORM 500000-ALTERAR
              WHEN SGTST139-EXCLUIR
                   PERFORM 600000-EXCLUIR
              WHEN SGTST139-CONSULTAR
                   IF  CHAVE-CACHE = SGTST139-CHAVE
                       MOVE DADOS-CACHE TO SGTST139-AREA-DADOS
                   ELSE
                       PERFORM 700000-CONSULTA
                   END-IF
              WHEN SGTST139-CONSULTAR-EST-ATIVO
                   PERFORM 800000-CONSULTA-EST-ATIVO
           END-EVALUATE

           GOBACK
           .
      *-----------------------------------------------------------------
       100000-VALIDA-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO PSC-ERRO-PGM
           INITIALIZE                  SGTST139-RTN-PGM

           IF  NOT SGTST139-INCLUIR
           AND NOT SGTST139-EXCLUIR
           AND NOT SGTST139-ALTERAR
           AND NOT SGTST139-CONSULTAR
           AND NOT SGTST139-CONSULTAR-EST-ATIVO
               MOVE  0116              TO  CD-RTN-PGM
               MOVE  'Ação inválida.'  TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 130000-VALIDA-CAMPOS

           IF  SGTST139-INCLUIR
           OR  SGTST139-ALTERAR
               PERFORM 160000-TRATA-NULIDADE
               PERFORM 170001-VALIDA-CRNG-EVT-EXCD
           END-IF
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       130000-VALIDA-CAMPOS SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO PSC-ERRO-PGM

           PERFORM 140000-BUSCA-TS-DT
           MOVE HLPKE001-TS-FMTD       TO TS-SQL-ATU


           IF  SGTST139-CD-ITMT-FNCO EQUAL SPACES
               MOVE  1160          TO  CD-RTN-PGM
               MOVE 'Código do Título do Evento não informado.'
                                   TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTST139-CD-GR-EVT-FDAR EQUAL ZEROS
              MOVE  1186          TO  CD-RTN-PGM
              MOVE 'Código do Tipo de Evento não informado.'
                                         TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTST139-CD-GR-EVT-FDAR <  ZEROS
              MOVE  1161          TO  CD-RTN-PGM
              MOVE 'Código do Tipo de Evento inválido.'
                                         TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTST139-NR-SEQL-EVT-EXCD EQUAL ZEROS
               MOVE  1179          TO  CD-RTN-PGM
               MOVE 'Numero do Evento SGT não informado.'
                                         TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTST139-NR-SEQL-EVT-EXCD < ZEROS
               MOVE  1180          TO  CD-RTN-PGM
               MOVE 'Numero do Evento SGT inválido.'
                                         TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF


           IF  SGTST139-INCLUIR
           OR  SGTST139-ALTERAR

             IF  SGTST139-TS-EST-EVT-EXCD EQUAL SPACES
                 MOVE  1178          TO  CD-RTN-PGM
                 MOVE 'Timestamp do Estado do Evento Excluído não inform
      -             'ado.'
                                         TO MSG-ERRO
               PERFORM 999001-ERRO-001
             END-IF

               IF  SGTST139-CD-USU-ULT-ATL EQUAL SPACES
                   MOVE  0651          TO  CD-RTN-PGM
                   MOVE 'Código do usuário responsável pela última atual
      -                 'ização não informado.'
                                         TO MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF

               IF  SGTST139-CD-EST-EVT-EXCD EQUAL ZEROS
                   MOVE  1176          TO  CD-RTN-PGM
                   MOVE 'Código do Estado do Evento do Títulos Excluído
      -                 'não informado.'
                                         TO MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF
           IF  SGTST139-TS-EST-EVT-EXCD EQUAL 'CURRENT'
               MOVE TS-SQL-ATU         TO SGTST139-TS-EST-EVT-EXCD
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
               MOVE RETURN-CODE        TO CD-RTN-PGM
               MOVE 'ERRO NA CHAMADA DA SBTIMEDT' TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       160000-TRATA-NULIDADE SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO PSC-ERRO-PGM
           IF  SGTST139-TX-JST-EVT-EXCD EQUAL SPACES
               MOVE -1                 TO TX-JST-EVT-EXCD-N
           END-IF

           .
       160000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170001-VALIDA-CRNG-EVT-EXCD      SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  PSC-ERRO-PGM

           MOVE  ZEROS                 TO  QT-REG-SELD

           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.CRNG_EVT_TIT_EXCD
               WHERE CD_ITMT_FNCO     =  :SGTST139-CD-ITMT-FNCO
                 AND CD_GR_EVT_FDAR   =  :SGTST139-CD-GR-EVT-FDAR
                 AND NR_SEQL_EVT_EXCD =  :SGTST139-NR-SEQL-EVT-EXCD
           END-EXEC

           IF  QT-REG-SELD                 EQUAL ZEROS
               MOVE  1177              TO  CD-RTN-PGM
               MOVE  'Evento de Título Excluído da Câmara de Liquidação
      -              'inexistente.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           .
       170001-SAI.
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
                INSERT INTO DB2SGT.EST_EVT_TIT_EXCD
                ( CD_ITMT_FNCO
                , CD_GR_EVT_FDAR
                , NR_SEQL_EVT_EXCD
                , TS_EST_EVT_EXCD
                , CD_USU_ULT_ATL
                , CD_EST_EVT_EXCD
                , TX_JST_EVT_EXCD )
                VALUES
                ( :SGTST139-CD-ITMT-FNCO
                , :SGTST139-CD-GR-EVT-FDAR
                , :SGTST139-NR-SEQL-EVT-EXCD
                , :SGTST139-TS-EST-EVT-EXCD
                , :SGTST139-CD-USU-ULT-ATL
                , :SGTST139-CD-EST-EVT-EXCD
                , :SGTST139-TX-JST-EVT-EXCD
                  :TX-JST-EVT-EXCD-N )
           END-EXEC
      *
      * Ligar erro automatico do DB2
      *
           EXEC SQL WHENEVER SQLERROR   GO TO 999002-ERRO-DB2 END-EXEC
      *
           EVALUATE SQLCODE
               WHEN -803
                   MOVE  0803        TO  CD-RTN-PGM
                   MOVE 'Inclusão não efetuada, registro existente.'
                                     TO MSG-ERRO

                   PERFORM 999001-ERRO-001
               WHEN 0
                    CONTINUE
               WHEN OTHER
      * Aciona o tratamento padrao de erro padrao para os demais casos
                    PERFORM 999002-ERRO-DB2
           END-EVALUATE
           PERFORM 980101-GUARDA-CACHE

           INITIALIZE                      DADOS-LOG
           MOVE  'I'                   TO  SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG
           .
       400000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       500000-ALTERAR SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO PSC-ERRO-PGM
           EXEC SQL
             UPDATE DB2SGT.EST_EVT_TIT_EXCD
             SET CD_USU_ULT_ATL     = :SGTST139-CD-USU-ULT-ATL
               , CD_EST_EVT_EXCD    = :SGTST139-CD-EST-EVT-EXCD
               , TX_JST_EVT_EXCD    = :SGTST139-TX-JST-EVT-EXCD
                                      :TX-JST-EVT-EXCD-N
                WHERE CD_ITMT_FNCO       = :SGTST139-CD-ITMT-FNCO
                  AND CD_GR_EVT_FDAR     = :SGTST139-CD-GR-EVT-FDAR
                  AND NR_SEQL_EVT_EXCD   = :SGTST139-NR-SEQL-EVT-EXCD
                  AND TS_EST_EVT_EXCD    = :SGTST139-TS-EST-EVT-EXCD
           END-EXEC
           IF  SQLCODE EQUAL +100
               INITIALIZE CHAVE-CACHE
               MOVE  0100                     TO  CD-RTN-PGM
               MOVE 'Registro inexistente.'
                                              TO MSG-ERRO

               PERFORM 999001-ERRO-001
           END-IF
           PERFORM 980101-GUARDA-CACHE

           INITIALIZE                      DADOS-LOG
           MOVE  'A'                   TO  SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG
           .
       500000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       600000-EXCLUIR SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO PSC-ERRO-PGM

           INITIALIZE                      DADOS-LOG
           MOVE  'E'                   TO  SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG

           EXEC SQL
                DELETE FROM DB2SGT.EST_EVT_TIT_EXCD
                WHERE CD_ITMT_FNCO       = :SGTST139-CD-ITMT-FNCO
                  AND CD_GR_EVT_FDAR     = :SGTST139-CD-GR-EVT-FDAR
                  AND NR_SEQL_EVT_EXCD   = :SGTST139-NR-SEQL-EVT-EXCD
                  AND TS_EST_EVT_EXCD    = :SGTST139-TS-EST-EVT-EXCD
           END-EXEC
           INITIALIZE REG-CACHE
           IF  SQLCODE EQUAL +100
               MOVE  0100              TO  CD-RTN-PGM
               MOVE 'Registro inexistente.'
                                       TO MSG-ERRO

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
           INITIALIZE SGTST139-TX-JST-EVT-EXCD
           EXEC SQL
                 SELECT CD_USU_ULT_ATL
                      , CD_EST_EVT_EXCD
                      , TX_JST_EVT_EXCD
                   INTO  :SGTST139-CD-USU-ULT-ATL
                       , :SGTST139-CD-EST-EVT-EXCD
                       , :SGTST139-TX-JST-EVT-EXCD
                         :TX-JST-EVT-EXCD-N
                       FROM DB2SGT.EST_EVT_TIT_EXCD
                WHERE CD_ITMT_FNCO       = :SGTST139-CD-ITMT-FNCO
                  AND CD_GR_EVT_FDAR     = :SGTST139-CD-GR-EVT-FDAR
                  AND NR_SEQL_EVT_EXCD   = :SGTST139-NR-SEQL-EVT-EXCD
                  AND TS_EST_EVT_EXCD    = :SGTST139-TS-EST-EVT-EXCD
           END-EXEC
           IF  SQLCODE = +100
               INITIALIZE SGTST139-AREA-DADOS
               MOVE  0100              TO  CD-RTN-PGM
               MOVE 'Registro inexistente.'
                                       TO MSG-ERRO

               PERFORM 999001-ERRO-001
           END-IF
           PERFORM 980101-GUARDA-CACHE
           .
       700000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       800000-CONSULTA-EST-ATIVO  SECTION.
      *--->    Executa a consulta sem informar Timestamp.
      *        Recupera, da chave informada, o registro com maior
      *        Timestamp.
      *-----------------------------------------------------------------
           MOVE 0010                   TO PSC-ERRO-PGM
      *    Campos nulos sao inicializados na consulta
           INITIALIZE SGTST139-TX-JST-EVT-EXCD
           EXEC SQL
                 SELECT CD_USU_ULT_ATL
                      , CD_EST_EVT_EXCD
                      , TX_JST_EVT_EXCD
                      , TS_EST_EVT_EXCD
                   INTO  :SGTST139-CD-USU-ULT-ATL
                       , :SGTST139-CD-EST-EVT-EXCD
                       , :SGTST139-TX-JST-EVT-EXCD
                         :TX-JST-EVT-EXCD-N
                       , :SGTST139-TS-EST-EVT-EXCD-S
                       FROM DB2SGT.EST_EVT_TIT_EXCD
                WHERE CD_ITMT_FNCO       = :SGTST139-CD-ITMT-FNCO
                  AND CD_GR_EVT_FDAR     = :SGTST139-CD-GR-EVT-FDAR
                  AND NR_SEQL_EVT_EXCD   = :SGTST139-NR-SEQL-EVT-EXCD
                  ORDER BY TS_EST_EVT_EXCD   DESC
                  FETCH FIRST 1 ROW ONLY
           END-EXEC
           IF  SQLCODE = +100
               INITIALIZE SGTST139-AREA-DADOS
               MOVE  0100              TO  CD-RTN-PGM
               MOVE 'Registro inexistente.'
                                       TO MSG-ERRO

               PERFORM 999001-ERRO-001
           END-IF
           PERFORM 980101-GUARDA-CACHE
           .
       800000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       980101-GUARDA-CACHE SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                        TO PSC-ERRO-PGM

           MOVE SGTST139-CHAVE              TO CHAVE-CACHE
           MOVE SGTST139-AREA-DADOS         TO DADOS-CACHE
           .
       980101-SAI.
           EXIT.
      *-----------------------------------------------------------------
       980102-GRAVA-LOG                SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                       TO  PSC-ERRO-PGM

           MOVE 'EST_EVT_TIT_EXCD'        TO SGTS014G-NM-ENT
           MOVE  'SGTST139'                 TO  SGTS014G-NM-PGM

           IF  EXECUCAO-ONLINE
               MOVE SGTST139-CD-USU-ULT-ATL TO SGTS014G-CD-CHV-USU
           ELSE
               MOVE 'SYSTEM'                TO SGTS014G-CD-CHV-USU
           END-IF

           MOVE 'SGTST139'                  TO SGTS014G-NM-PGM
           MOVE SGTST139-CD-ITMT-FNCO       TO CD-ITMT-FNCO-LOG
           MOVE SGTST139-CD-GR-EVT-FDAR     TO CD-GR-EVT-FDAR-LOG
           MOVE SGTST139-NR-SEQL-EVT-EXCD   TO NR-SEQL-EVT-EXCD-LOG
           MOVE SGTST139-TS-EST-EVT-EXCD    TO TS-EST-EVT-EXCD-LOG

           STRING CD-ITMT-FNCO-LOG
           ';'    CD-GR-EVT-FDAR-LOG
           ';'    NR-SEQL-EVT-EXCD-LOG
           ';'    TS-EST-EVT-EXCD-LOG
               DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR

           MOVE LENGTH OF DADOS-LOG    TO EIBCALEN
           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM  NOT EQUAL ZEROS
               MOVE  SGTS014G          TO  PGM-ERRO
               MOVE 0112              TO CD-RTN-PGM
               MOVE 'Erro na gravação das informações do Log.'
                                      TO MSG-ERRO-PGM
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

           MOVE RTN-PGM                      TO SGTST139-RTN-PGM
           GOBACK
           .
       999001-SAI.
           EXIT.
      *-----------------
      * 999002-ERRO-DB2
      *-----------------
-INC HLPKE005
      *-----------------
      * 999003-TRATA-ABEND-CICS
      * 999004-TRATA-CONDITION-CICS
      *-----------------
-INC HLPKE006

      *---------------------- FIM SGTST139 ----------------------------
