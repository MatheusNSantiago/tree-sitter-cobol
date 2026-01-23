      *-----------------------------------------------------------------
      * Programa..: SGTST029
      * Analista..: F2746899 - Eduardo Pio Ribeiro Junior
      * Autor.....: F2746899 - Eduardo Pio Ribeiro Junior
      * Data......: 20/07/2015
      * Objetivo..: CRUD da Tabela CTGR_BC_TVM
      * Compilacao: Cobol/CICS/DB2
      *-----------------------------------------------------------------
      * VRS003 02/05/2019 - F8650584 - DE 2016/145195
      *                     Inclusão de Mensageria e atualização no
      *                     Tratamento de Ocorrências de Erros
      * VRS002 09/12/2016 - F9188847 - DE 2016/145195- Inclusao Funcao 6
      * VRS001 20/07/2015 - F2746899 - DE 2016/145195- Implantacao
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTST029.

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
       77  CTE-INICIO-SS               PIC  X(40) VALUE
               '*** Storage Section começa aqui ***'.
       77  CTE-PROG                    PIC  X(17)
                                       VALUE '*** SGTST029 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS003'.

      *--> Registro da tabela
       01  REG-CACHE.
           03  CHAVE-CACHE.
               05  CD-CTGR-TVM         PIC S9(04) COMP-5 VALUE 0.
               05  FILLER              PIC  X(20) VALUE ' '.
           03  DADOS-CACHE.
               05  NM-CTGR-TVM         PIC  X(40) VALUE ' '.
               05  IN-CTGR-TVM         PIC  X(01) VALUE ' '.
               05  TS-ATL              PIC  X(26) VALUE ' '.
               05  CD-USU-ATL          PIC  X(08) VALUE ' '.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTS014G                PIC  X(08) VALUE 'SGTS014G'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      * Area de trabalho

       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE 0.
       01  TS-SQL-ATU                  PIC  X(26) VALUE ' '.

       01  AUX-NM-CTGR                 PIC  X(40) VALUE ' '.
       01  PSC-FIM                     PIC S9(04) COMP-5 VALUE 0.

      *--> Variáveis para gravação do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTGR-TVM-LOG         PIC  9(04) VALUE 0.

      *--> Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

      *--> Book das variáveis auxiliares do tratamento de erro
-INC HLPKE001

      *-----------------------------------------------------------------
      *    Areas e Books Utilizados no Tratamento da Mensageria
      *-----------------------------------------------------------------
       01  IC-TAB                      PIC  9(02) VALUE ZEROS.

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> HLPKW999
      *    Variaveis Auxiliares para Captura de Erro CICS
      *    e Adicionar Variaveis Monitoradas
       01  MSGKW999-VRV-ERRO-CICS.
           03  MSGKW999-RESP1-X8       PIC  X(08) VALUE SPACES.
           03  MSGKW999-RESP2-X8       PIC  X(08) VALUE SPACES.
       01  MSGKW999-VRV-ALINHA-NUMERO.
           03  MSGKW999-NUMERO-Z17     PIC  ZZZZZZZZZZZZZZZZ9.
           03  MSGKW999-NUMERO-X17     PIC  X(17) VALUE SPACES.
           03  MSGKW999-BRANCOS        PIC  9(08) VALUE ZEROS.
       01  MSGKW999-VRV-AUX.
           03  MSGKW999-NM-VRV-MNT     PIC  X(030) VALUE SPACES.
           03  MSGKW999-VL-VRV-MNT     PIC  X(100) VALUE SPACES.
      *--> Indicador de gravação de variáveis monitoradas
       01  MSGKW999-IN-GRV-VRV         PIC  X(01)  VALUE ' '.
           88 MSGKW999-MNT-VRV                     VALUE 'S'.
           88 MSGKW999-N-MNT-VRV                   VALUE 'N'.

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      ---------------9.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC  X(40) VALUE
               '*** Storage Section termina aqui ***'.

      *-----------------------------------------------------------------
       LINKAGE                         SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTKT029

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               MOVE  0001              TO  PSC-ERRO-PGM
               MOVE  0220              TO  CD-RTN-PGM
               MOVE  'Área com tamanho inválido - verifique book/local.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           MOVE  0001                  TO  PSC-ERRO-PGM

           IF  NOT EXECUCAO-ONLINE  AND
               NOT EXECUCAO-BATCH
               CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      *--> Habilita Testes no Cobolito
-INC ALMKCBL2

      *--> WHENEVER DB2
-INC HLPKE002

      *--> WHENEVER CICS
-INC HLPKE003

           PERFORM 100000-VALIDA-PARAMETRO

           EVALUATE TRUE
              WHEN SGTST029-INCLUIR
                   PERFORM 400000-INCLUIR
                   PERFORM 981000-LOG-INCLUSAO
                   MOVE  'Inclusão efetuada com sucesso.'
                                       TO  SGTST029-TX-MSG-RTN
              WHEN SGTST029-ALTERAR
                   PERFORM 500000-ALTERAR
                   PERFORM 982000-LOG-ALTERACAO
                   MOVE  'Alteração efetuada com sucesso.'
                                       TO  SGTST029-TX-MSG-RTN
              WHEN SGTST029-EXCLUIR
                   PERFORM 983000-LOG-EXCLUSAO
                   PERFORM 600000-EXCLUIR
                   MOVE  'Exclusão efetuada com sucesso.'
                                       TO  SGTST029-TX-MSG-RTN
              WHEN SGTST029-CONSULTAR-DESCRICAO
                   PERFORM 700000-CONSULTA-DESCRICAO
              WHEN SGTST029-CONSULTAR-COD-CTGR-BC
                   PERFORM 800000-CONSULTA-COD-CTGR-BC
              WHEN SGTST029-CONSULTAR-CTGR-ATI
                   PERFORM 900000-CONSULTA-CTGR-TVM-ATI

           END-EVALUATE

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-VALIDA-PARAMETRO         SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  PSC-ERRO-PGM

           INITIALIZE                      SGTST029-RTN-PGM

           IF  NOT SGTST029-INCLUIR
           AND NOT SGTST029-EXCLUIR
           AND NOT SGTST029-ALTERAR
           AND NOT SGTST029-CONSULTAR-DESCRICAO
           AND NOT SGTST029-CONSULTAR-COD-CTGR-BC
           AND NOT SGTST029-CONSULTAR-CTGR-ATI
               MOVE  0116              TO  CD-RTN-PGM
               MOVE  'Ação inválida.'  TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           PERFORM 130000-VALIDA-CAMPOS

      *--> Obtem Timestamp e Data e Trata CURRENT
           PERFORM  140000-BUSCA-TS-DT

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-VALIDA-CAMPOS            SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  PSC-ERRO-PGM

           IF  NOT SGTST029-INCLUIR  AND
               NOT SGTST029-CONSULTAR-DESCRICAO
               IF  SGTST029-CD-CTGR-TVM    EQUAL ZEROS
                   MOVE  0250          TO  CD-RTN-PGM
                   MOVE  'Código da categoria Bacen não informado.'
                                       TO  MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
               IF  SGTST029-CD-CTGR-TVM    NEGATIVE
                    MOVE  0251         TO  CD-RTN-PGM
                    MOVE  'Código da categoria Bacen inválido.'
                                       TO  MSG-ERRO
                    PERFORM 999001-ERRO-001
               END-IF
           END-IF


           IF  SGTST029-NM-CTGR-TVM        EQUAL SPACES  AND
              (SGTST029-CONSULTAR-DESCRICAO OR SGTST029-ALTERAR)
               MOVE  0249              TO  CD-RTN-PGM
               MOVE  'Nome da categoria Bacen não informada.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  NOT ( SGTST029-EXCLUIR  OR
                     SGTST029-CONSULTAR-COD-CTGR-BC  OR
                     SGTST029-CONSULTAR-CTGR-ATI)
              PERFORM 131000-RETIRA-ESPACO-INI-DESCR
           END-IF


           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       131000-RETIRA-ESPACO-INI-DESCR  SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  PSC-ERRO-PGM

           INSPECT  SGTST029-NM-CTGR-TVM
                    TALLYING PSC-FIM
                    FOR LEADING SPACES

           IF  PSC-FIM                     > ZEROS
              MOVE  SPACES             TO  AUX-NM-CTGR
              MOVE  SGTST029-NM-CTGR-TVM(PSC-FIM + 1:40 - PSC-FIM)
                                       TO  AUX-NM-CTGR

              MOVE  SPACES             TO  SGTST029-NM-CTGR-TVM
              MOVE  AUX-NM-CTGR        TO  SGTST029-NM-CTGR-TVM
           END-IF

           .
       131000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-BUSCA-TS-DT              SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  PSC-ERRO-PGM

           CALL SBTIMEDT            USING  HLPKE001-CD-FUC
                                           HLPKE001-AREA-TRB
                                           HLPKE001-FMT-TS
                                           HLPKE001-TAM-TS
                                           HLPKE001-TS-FMTD

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               SET   SGTST029-ERRO-DE-PGM
                                       TO  TRUE
               MOVE  SBTIMEDT          TO  PGM-ERRO
               MOVE  0050              TO  CD-RTN-PGM
               MOVE  'Erro ao obter data e hora atual.'
                                       TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           MOVE  HLPKE001-TS-FMTD      TO  TS-SQL-ATU

           IF  SGTST029-TS-ATL             EQUAL 'CURRENT'
               MOVE  TS-SQL-ATU        TO  SGTST029-TS-ATL
           END-IF

           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       400000-INCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  PSC-ERRO-PGM

           PERFORM 430000-BUSCA-NR-SEQUENCIA

           EXEC SQL
                INSERT INTO DB2SGT.CTGR_BC_TVM
                ( CD_CTGR_TVM
                , NM_CTGR_TVM
                , IN_CTGR_TVM
                , TS_ATL
                , CD_USU_ATL )
                VALUES
                ( :SGTST029-CD-CTGR-TVM
                , :SGTST029-NM-CTGR-TVM
                , :SGTST029-IN-CTGR-TVM
                , :SGTST029-TS-ATL
                , :SGTST029-CD-USU-ATL )
           END-EXEC

           PERFORM 980101-GUARDA-CACHE
           .
       400000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       430000-BUSCA-NR-SEQUENCIA       SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  PSC-ERRO-PGM

           EXEC SQL
               SELECT VALUE (MAX(CD_CTGR_TVM) , 0 )
                INTO :SGTST029-CD-CTGR-TVM
                FROM DB2SGT.CTGR_BC_TVM
           END-EXEC

           ADD   0001                  TO  SGTST029-CD-CTGR-TVM
                 ON SIZE ERROR
                 MOVE  0252            TO  CD-RTN-PGM
                 MOVE  'Erro no cálculo do número de sequencial.'
                                       TO  MSG-ERRO
                 PERFORM 999001-ERRO-001
           END-ADD
           .
       430000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       500000-ALTERAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO  PSC-ERRO-PGM

           EXEC SQL
             UPDATE DB2SGT.CTGR_BC_TVM
             SET NM_CTGR_TVM        = :SGTST029-NM-CTGR-TVM
               , IN_CTGR_TVM        = :SGTST029-IN-CTGR-TVM
               , TS_ATL             = :SGTST029-TS-ATL
               , CD_USU_ATL         = :SGTST029-CD-USU-ATL
                WHERE CD_CTGR_TVM        = :SGTST029-CD-CTGR-TVM
           END-EXEC

           IF  SQLCODE                     EQUAL +100
               INITIALIZE                  CHAVE-CACHE
               MOVE  0100              TO  CD-RTN-PGM
               MOVE 'Registro inexistente.'
                                       TO  MSG-ERRO
               PERFORM  999200-MONITORAR-SQL
               PERFORM  999001-ERRO-001
           END-IF

           PERFORM 980101-GUARDA-CACHE
           .
       500000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       600000-EXCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  PSC-ERRO-PGM

           EXEC SQL
                DELETE FROM DB2SGT.CTGR_BC_TVM
                WHERE CD_CTGR_TVM        = :SGTST029-CD-CTGR-TVM
           END-EXEC

           INITIALIZE                      REG-CACHE

           IF  SQLCODE                     EQUAL +100
               MOVE  0100              TO  CD-RTN-PGM
               MOVE 'Registro inexistente.'
                                       TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF
           .
       600000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       700000-CONSULTA-DESCRICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  PSC-ERRO-PGM

           INITIALIZE                      SGTST029-AREA-DADOS

           EXEC SQL
                 SELECT CD_CTGR_TVM
                      , NM_CTGR_TVM
                      , IN_CTGR_TVM
                      , TS_ATL
                      , CD_USU_ATL
                   INTO  :SGTST029-CD-CTGR-TVM
                       , :SGTST029-NM-CTGR-TVM
                       , :SGTST029-IN-CTGR-TVM
                       , :SGTST029-TS-ATL
                       , :SGTST029-CD-USU-ATL
                       FROM DB2SGT.CTGR_BC_TVM
                WHERE
                       NM_CTGR_TVM       = :SGTST029-NM-CTGR-TVM
                AND    CD_CTGR_TVM     <>  :SGTST029-CD-CTGR-TVM
           END-EXEC

           IF  SQLCODE                     EQUAL +100
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0254              TO  CD-RTN-PGM
               MOVE  ZEROS             TO  SGTST029-TIP-RTN-PGM
               MOVE  'Nome da categoria Bacen inexistente.'
                                       TO  MSG-ERRO
           END-IF

           PERFORM 980101-GUARDA-CACHE
           .
       700000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       800000-CONSULTA-COD-CTGR-BC     SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  PSC-ERRO-PGM

           INITIALIZE                      SGTST029-AREA-DADOS

           EXEC SQL
                 SELECT CD_CTGR_TVM
                      , NM_CTGR_TVM
                      , IN_CTGR_TVM
                      , TS_ATL
                      , CD_USU_ATL
                   INTO  :SGTST029-CD-CTGR-TVM
                       , :SGTST029-NM-CTGR-TVM
                       , :SGTST029-IN-CTGR-TVM
                       , :SGTST029-TS-ATL
                       , :SGTST029-CD-USU-ATL
                       FROM DB2SGT.CTGR_BC_TVM
                WHERE CD_CTGR_TVM  = :SGTST029-CD-CTGR-TVM
           END-EXEC

           IF  SQLCODE                     EQUAL +100
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0255              TO  CD-RTN-PGM
               MOVE  'Código da categoria Bacen inexistente.'
                                       TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           PERFORM 980101-GUARDA-CACHE
           .
       800000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       900000-CONSULTA-CTGR-TVM-ATI    SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  PSC-ERRO-PGM

           MOVE  ZEROS                 TO  QT-REG-SELD

           EXEC SQL
              SELECT 1
              INTO :QT-REG-SELD
              FROM DB2SGT.CTGR_BC_TVM
              WHERE CD_CTGR_TVM = :SGTST029-CD-CTGR-TVM
              AND IN_CTGR_TVM = 'S'
             FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG-SELD                 EQUAL ZEROS
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0256              TO  CD-RTN-PGM
               MOVE  'Categoria Bacen TVM ativa inexistente.'
                                       TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980101-GUARDA-CACHE             SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  PSC-ERRO-PGM

           MOVE  SGTST029-CHAVE        TO  CHAVE-CACHE
           MOVE  SGTST029-AREA-DADOS   TO  DADOS-CACHE

           .
       980101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       981000-LOG-INCLUSAO             SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  PSC-ERRO-PGM

           INITIALIZE                      DADOS-LOG

           SET   SGTS014G-INCLUSAO     TO  TRUE

           MOVE  'CTGR_BC_TVM'         TO  SGTS014G-NM-ENT
           IF  EXECUCAO-ONLINE
               MOVE SGTST029-CD-USU-ATL
                                       TO  SGTS014G-CD-CHV-USU
           ELSE
               MOVE  'SISTEMA'         TO  SGTS014G-CD-CHV-USU
           END-IF
           MOVE  CTE-PROG(5:8)         TO  SGTS014G-NM-PGM

           MOVE  SGTST029-CD-CTGR-TVM  TO  CD-CTGR-TVM-LOG

           STRING  CD-CTGR-TVM-LOG
                   DELIMITED BY SIZE
                   INTO                    SGTS014G-CHV-PRMR

           MOVE  LENGTH OF DADOS-LOG   TO  EIBCALEN

           CALL SGTS014G            USING  DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM         NOT EQUAL ZEROS
               SET   ERRO-DE-PGM       TO  TRUE
               MOVE  SGTS014G          TO  PGM-ERRO
               MOVE  0112              TO  CD-RTN-PGM
               MOVE  'Erro na gravação das informações do Log.'
                                       TO  MSG-ERRO
               MOVE SGTS014G           TO  MSG1-NM-PGM-ACND
               MOVE SGTS014G-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               MOVE SGTS014G-MSG-ERRO  TO  MSG1-TX-RTN-PGM-ACND
               PERFORM  999001-ERRO-001
           END-IF
           .
       981000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       982000-LOG-ALTERACAO            SECTION.
      *-----------------------------------------------------------------
           MOVE  0014                  TO  PSC-ERRO-PGM

           INITIALIZE                      DADOS-LOG

           SET   SGTS014G-ALTERACAO    TO  TRUE

           MOVE  'CTGR_BC_TVM'         TO  SGTS014G-NM-ENT

           MOVE  SGTST029-CD-CTGR-TVM  TO  CD-CTGR-TVM-LOG

           STRING  CD-CTGR-TVM-LOG
                   DELIMITED BY SIZE
                   INTO                    SGTS014G-CHV-PRMR

           MOVE  SGTST029-CD-USU-ATL   TO  SGTS014G-CD-CHV-USU

           MOVE  CTE-PROG(5:8)         TO  SGTS014G-NM-PGM

           MOVE  LENGTH OF DADOS-LOG   TO  EIBCALEN

           CALL SGTS014G            USING  DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM         NOT EQUAL ZEROS
               SET   ERRO-DE-PGM       TO  TRUE
               MOVE  SGTS014G          TO  PGM-ERRO
               MOVE  0112              TO  CD-RTN-PGM
               MOVE  'Erro na gravação das informações do Log.'
                                       TO  MSG-ERRO
               MOVE SGTS014G           TO  MSG1-NM-PGM-ACND
               MOVE SGTS014G-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               MOVE SGTS014G-MSG-ERRO  TO  MSG1-TX-RTN-PGM-ACND
               PERFORM  999001-ERRO-001
           END-IF
           .
       982000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983000-LOG-EXCLUSAO             SECTION.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  PSC-ERRO-PGM

           INITIALIZE                      DADOS-LOG

           SET   SGTS014G-EXCLUSAO     TO  TRUE

           MOVE  'CTGR_BC_TVM'         TO  SGTS014G-NM-ENT

           MOVE  SGTST029-CD-CTGR-TVM  TO  CD-CTGR-TVM-LOG

           STRING  CD-CTGR-TVM-LOG
                   DELIMITED BY SIZE
                   INTO                    SGTS014G-CHV-PRMR

           MOVE  SGTST029-CD-USU-ATL   TO  SGTS014G-CD-CHV-USU

           MOVE  CTE-PROG(5:8)         TO  SGTS014G-NM-PGM

           MOVE LENGTH OF DADOS-LOG    TO  EIBCALEN

           CALL SGTS014G            USING  DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM         NOT EQUAL ZEROS
               SET   ERRO-DE-PGM       TO  TRUE
               MOVE  SGTS014G          TO  PGM-ERRO
               MOVE  0112              TO  CD-RTN-PGM
               MOVE  'Erro na gravação das informações do Log.'
                                       TO  MSG-ERRO
               MOVE SGTS014G           TO  MSG1-NM-PGM-ACND
               MOVE SGTS014G-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               MOVE SGTS014G-MSG-ERRO  TO  MSG1-TX-RTN-PGM-ACND
               PERFORM  999001-ERRO-001
           END-IF
           .
       983000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      * 999000-ERRO SECTION
      *-----------------------------------------------------------------
      *----------------------------
      * 999001-ERRO-001.
      *----------------------------
-INC HLPKE004
-INC HLPKE012

      *--> 0001 - Erro de db2
           IF  TIP-RTN-PGM                 EQUAL  0001
               IF  CD-RTN-PGM              NOT EQUAL  +100  AND  -803
                   MOVE  9997          TO  CD-RTN-PGM
               END-IF
           END-IF

      *--> 0002 - Erro cics
           IF  TIP-RTN-PGM                 EQUAL  0002
               IF  CD-RTN-PGM              EQUAL  ZEROS  OR  0333
                   MOVE  9999          TO  CD-RTN-PGM
               END-IF
           END-IF

           PERFORM  999999-ENCERRAR
           .
       999001-SAI.
           EXIT.

      *----------------------------
      * 999002-ERRO-DB2
      *----------------------------
-INC HLPKE005
      *----------------------------
      * 999003-TRATA-ABEND-CICS
      * 999004-TRATA-CONDITION-CICS
      *----------------------------
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
           MOVE  SGTST029-TIP-RTN-PGM  TO  TIP-RTN-PGM
      *--> Alterar os Campos de Entrada conforme o Book do módulo
           PERFORM  999100-MONITORAR-ENTRADA

      *--> Movimenta Campos Básicos da Ocorrência de Erro
           MOVE  CD-RTN-PGM            TO  MSG1-CD-ERRO-TS
           MOVE  PSC-ERRO-PGM          TO  MSG1-CD-SEQL-ERRO
           MOVE  MSG-ERRO              TO  MSG1-TX-ERRO-TS

      *--> Realiza a tradução do tipo de erro do COMMAREA
      *     para tipos de erro da Mensageria
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
           MOVE 'T029-001-CD-CHV-USU'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST029-CD-CHV-USU   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'T029-002-ACAO'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST029-ACAO         TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'T029-003-CD-CTGR-TVM'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST029-CD-CTGR-TVM  TO MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'T029-004-NM-CTGR-TVM'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST029-NM-CTGR-TVM  TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'T029-005-IN-CTGR-TVM'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST029-IN-CTGR-TVM  TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'T029-006-TS-ATL'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST029-TS-ATL       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'T029-007-CD-USU-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST029-CD-USU-ATL   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'T029-008-TX-MSG-RTN'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST029-TX-MSG-RTN   TO  MSGKW999-VL-VRV-MNT
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
                     MOVE  SGTS014G-PGM-ERRO
                                       TO  MSG1-NM-PGM-ACND
                     MOVE  SGTS014G-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
                     MOVE  SGTS014G-PSC-ERRO-PGM
                                       TO  MSG1-CD-SEQL-PGM-ACND
                     MOVE  SGTS014G-MSG-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND

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

               WHEN  SBTIMEDT
                     MOVE  'HLPKE001-CD-FUC'     TO  MSGKW999-NM-VRV-MNT
                     MOVE  HLPKE001-CD-FUC       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE  'HLPKE001-AREA-TRB'   TO  MSGKW999-NM-VRV-MNT
                     MOVE  HLPKE001-AREA-TRB     TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE  'HLPKE001-FMT-TS'     TO  MSGKW999-NM-VRV-MNT
                     MOVE  HLPKE001-FMT-TS       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE  'HLPKE001-TAM-TS'     TO  MSGKW999-NM-VRV-MNT
                     MOVE  HLPKE001-TAM-TS       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE  'HLPKE001-TS-FMTD'    TO  MSGKW999-NM-VRV-MNT
                     MOVE  HLPKE001-TS-FMTD      TO  MSGKW999-VL-VRV-MNT
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

      *-----------------------------------------------------------------
      *    Fim do Tratamento através do Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *----------------------------------------
       999999-ENCERRAR                 SECTION.
      *----------------------------------------

           MOVE  RTN-PGM               TO  SGTST029-RTN-PGM

           IF  EXECUCAO-ONLINE
               PERFORM  999000-TRATAR-ERRO-MSG
           END-IF

           GOBACK
           .
       999999-SAI.
           EXIT.
      *------------------------- FIM SGTST029 --------------------------