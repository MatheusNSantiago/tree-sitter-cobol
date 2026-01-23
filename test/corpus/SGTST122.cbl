      *-----------------------------------------------------------------
      * PROGRAMA..: SGTST122
      * ANALISTA..: F9158937 - SILVIA AGUIAR
      * AUTOR.....: F9158937 - SILVIA AGUIAR
      * DATA......: 19.06.2019
      * OBJETIVO..: PERSISTENCIA DA TABELA  LDR_SIS
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 19.06.2019 - F9158937 -  IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTST122.

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

       77  CTE-PROG                    PIC  X(17)
                                       VALUE '*** SGTST122 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *--> Registro da tabela
       01  REG-CACHE.
           03  CHAVE-CACHE.
               05  DT-INCL-LDR         PIC  X(10)  VALUE ' '.
               05  DT-INCL-LDR-R       REDEFINES   DT-INCL-LDR.
                   07 DIA              PIC  9(02).
                   07 FILLER           PIC  X(01).
                   07 MES              PIC  9(02).
                   07 FILLER           PIC  X(01).
                   07 ANO              PIC  9(04).
               05  FILLER              PIC  X(20)  VALUE ' '.
           03  DADOS-CACHE.
               05  DT-ULT-MVT-ENV      PIC  X(10)  VALUE ' '.
               05  DT-ULT-MVT-ENV-R    REDEFINES   DT-ULT-MVT-ENV.
                   07 DIA              PIC  9(02).
                   07 FILLER           PIC  X(01).
                   07 MES              PIC  9(02).
                   07 FILLER           PIC  X(01).
                   07 ANO              PIC  9(04).
               05  TS-FCHT-CMCO-ONL    PIC  X(26) VALUE ' '.
               05  TS-RBT-CMCO-ONL     PIC  X(26) VALUE ' '.


      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTS014G                PIC  X(08) VALUE 'SGTS014G'.

      *--> Definição dos Variaveis do Cobolito
      * -INC ALMKCBL1

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE 0.
       01  DT-SQL-ATU                  PIC  X(10)
                                                  VALUE '  .  .    '.
       01  DT-SQL-ATU-R REDEFINES DT-SQL-ATU.
           03 DIA                      PIC  9(02).
           03 FILLER                   PIC  X(01).
           03 MES                      PIC  9(02).
           03 FILLER                   PIC  X(01).
           03 ANO                      PIC  9(04).
       01  TS-SQL-ATU                  PIC  X(26) VALUE ' '.

      *--> Indicadores de nulidades
       01  TS-FCHT-CMCO-ONL-N          PIC S9(04) COMP-5 VALUE 0.
       01  TS-RBT-CMCO-ONL-N           PIC S9(04) COMP-5 VALUE 0.

      *--> Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

      * Books da definicao de dados a serem usados na subrotina de erros
-INC HLPKE001

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
       LINKAGE                         SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTKT122

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

           IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
              MOVE  0220              TO  CD-RTN-PGM
              MOVE  'Area com tamanho inválido - verifique book/local.'
                                      TO  MSG-ERRO
              PERFORM 999001-ERRO-001
           END-IF

           MOVE  0001                 TO  PSC-ERRO-PGM

           IF  NOT EXECUCAO-ONLINE  AND
               NOT EXECUCAO-BATCH
               CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      *--> Habilita Testes no Cobolito
      * -INC ALMKCBL2

      *--> WHENEVER DB2
-INC HLPKE002

      *--> WHENEVER CICS
-INC HLPKE003

           PERFORM 100000-VALIDA-PARAMETRO

           EVALUATE TRUE
              WHEN SGTST122-INCLUIR
                   PERFORM 400000-INCLUIR
              WHEN SGTST122-ALTERAR
                   PERFORM 500000-ALTERAR
              WHEN SGTST122-EXCLUIR
                   PERFORM 600000-EXCLUIR
              WHEN SGTST122-CONSULTAR
                   IF  CHAVE-CACHE         EQUAL  SGTST122-CHAVE
                       MOVE  DADOS-CACHE
                                       TO  SGTST122-AREA-DADOS
                   ELSE
                       PERFORM 700000-CONSULTA
                   END-IF
           END-EVALUATE

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-VALIDA-PARAMETRO         SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  PSC-ERRO-PGM

           INITIALIZE                  SGTST122-RTN-PGM

           IF  NOT SGTST122-INCLUIR
           AND NOT SGTST122-EXCLUIR
           AND NOT SGTST122-ALTERAR
           AND NOT SGTST122-CONSULTAR
               MOVE  0116              TO  CD-RTN-PGM
               MOVE  'Ação inválida.'  TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           PERFORM 130000-VALIDA-CAMPOS

           IF  SGTST122-INCLUIR
           OR  SGTST122-ALTERAR
               PERFORM 160000-TRATA-NULIDADE
           END-IF
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-VALIDA-CAMPOS            SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  PSC-ERRO-PGM

      *--> Data de Inclusão não informada
           IF SGTST122-DT-INCL-LDR     EQUAL SPACES
               MOVE  1055          TO  CD-RTN-PGM
               MOVE  'Data de inclusão do arquivo lider não informada.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

      *--> Validar Usuário Atualização
           IF  SGTST122-INCLUIR
            OR SGTST122-ALTERAR
            OR SGTST122-EXCLUIR
               IF  SGTST122-CD-CHV-USU-ATL  EQUAL SPACES
                   MOVE  0226          TO  CD-RTN-PGM
                   MOVE  'Código do usuário responsável pela atualização
      -                  ' inválido.'      TO  MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF

      *--> Validar dados Inclusão e Alteração
           IF  SGTST122-INCLUIR
           OR  SGTST122-ALTERAR

               IF  SGTST122-DT-ULT-MVT-ENV EQUAL SPACES
                   MOVE  0336          TO  CD-RTN-PGM
                   MOVE  'Data movimentação não informada.'
                                       TO  MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF

           END-IF

      *--> Movimentar Timestamp e Data Atuais quando
      *    o valor recebido é igual a CURRENT

           PERFORM 140000-BUSCA-TS-DT

           MOVE  CORR HLPKE001-TS-FMTD TO  DT-SQL-ATU-R
           MOVE  HLPKE001-TS-FMTD      TO  TS-SQL-ATU

           IF  SGTST122-TS-FCHT-CMCO-ONL   EQUAL 'CURRENT'
               MOVE  TS-SQL-ATU        TO  SGTST122-TS-FCHT-CMCO-ONL
           END-IF

           IF SGTST122-TS-RBT-CMCO-ONL     EQUAL 'CURRENT'
               MOVE  TS-SQL-ATU        TO  SGTST122-TS-RBT-CMCO-ONL
           END-IF


           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-BUSCA-TS-DT              SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  PSC-ERRO-PGM

           CALL  SBTIMEDT           USING  HLPKE001-CD-FUC
                                           HLPKE001-AREA-TRB
                                           HLPKE001-FMT-TS
                                           HLPKE001-TAM-TS
                                           HLPKE001-TS-FMTD

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               SET   SGTST122-ERRO-DE-PGM
                                       TO  TRUE
               MOVE  SBTIMEDT          TO  PGM-ERRO
               MOVE  0050              TO  CD-RTN-PGM
               MOVE  'Erro ao obter data e hora atual.'
                                       TO  MSG-ERRO
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       160000-TRATA-NULIDADE           SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  PSC-ERRO-PGM

           IF  SGTST122-TS-FCHT-CMCO-ONL   EQUAL SPACES
               MOVE  -001              TO  TS-FCHT-CMCO-ONL-N
           END-IF

           IF  SGTST122-TS-RBT-CMCO-ONL   EQUAL SPACES
               MOVE  -001              TO  TS-RBT-CMCO-ONL-N
           END-IF

           .
       160000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       400000-INCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  PSC-ERRO-PGM

      *--> Desligar erro automatico do DB2
           EXEC SQL WHENEVER
                    SQLERROR CONTINUE
           END-EXEC

           EXEC SQL
              INSERT INTO DB2SGT.LDR_SIS
              ( DT_INCL_LDR
              , DT_ULT_MVT_ENV
              , TS_FCHT_CMCO_ONL
              , TS_RBT_CMCO_ONL)
              VALUES
              ( :SGTST122-DT-INCL-LDR
              , :SGTST122-DT-ULT-MVT-ENV
              , :SGTST122-TS-FCHT-CMCO-ONL
                :TS-FCHT-CMCO-ONL-N
              , :SGTST122-TS-RBT-CMCO-ONL
                :TS-RBT-CMCO-ONL-N )
           END-EXEC

      *--> Ligar erro automatico do DB2
           EXEC SQL WHENEVER SQLERROR
                    GO TO 999002-ERRO-DB2
           END-EXEC

           EVALUATE  SQLCODE
               WHEN  -803
                     MOVE  0803        TO  CD-RTN-PGM
                     MOVE 'Inclusão não efetuada, registro existente.'
                                       TO  MSG-ERRO
                     PERFORM  999001-ERRO-001
               WHEN  ZEROS
                     CONTINUE
               WHEN  OTHER
      *--> Aciona o tratamento de erro padrao para os demais casos
                     PERFORM  999002-ERRO-DB2
           END-EVALUATE

           PERFORM 980101-GUARDA-CACHE

           INITIALIZE                  DADOS-LOG
           MOVE  'I'                   TO  SGTS014G-CD-TIP-ACAO

           PERFORM 980102-GRAVA-LOG
           .
       400000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       500000-ALTERAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0014                  TO  PSC-ERRO-PGM

           EXEC SQL
             UPDATE DB2SGT.LDR_SIS
             SET DT_ULT_MVT_ENV     = :SGTST122-DT-ULT-MVT-ENV
               , TS_FCHT_CMCO_ONL   = :SGTST122-TS-FCHT-CMCO-ONL
                                      :TS-FCHT-CMCO-ONL-N
               , TS_RBT_CMCO_ONL    = :SGTST122-TS-RBT-CMCO-ONL
                                      :TS-RBT-CMCO-ONL-N

             WHERE   DT_INCL_LDR   =  :SGTST122-DT-INCL-LDR
           END-EXEC

           IF  SQLCODE                     EQUAL +100
               INITIALIZE                  CHAVE-CACHE
               MOVE  0100              TO  CD-RTN-PGM
               MOVE 'Registro inexistente.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GUARDA-CACHE

           INITIALIZE                      DADOS-LOG
           MOVE  'A'                   TO  SGTS014G-CD-TIP-ACAO

           PERFORM  980102-GRAVA-LOG
           .
       500000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       600000-EXCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  PSC-ERRO-PGM

           INITIALIZE                      DADOS-LOG
           MOVE  'E'                   TO  SGTS014G-CD-TIP-ACAO

           PERFORM 980102-GRAVA-LOG

           EXEC SQL
                DELETE FROM DB2SGT.LDR_SIS
                WHERE  DT_INCL_LDR   = :SGTST122-DT-INCL-LDR
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
       700000-CONSULTA                 SECTION.
      *-----------------------------------------------------------------
           MOVE  0016                  TO  PSC-ERRO-PGM

      *--> Campos nulos sao inicializados na consulta
           INITIALIZE        SGTST122-TS-FCHT-CMCO-ONL
                             SGTST122-TS-RBT-CMCO-ONL

           EXEC SQL
                SELECT  DT_ULT_MVT_ENV
                      , TS_FCHT_CMCO_ONL
                      , TS_RBT_CMCO_ONL
                  INTO  :SGTST122-DT-ULT-MVT-ENV
                      , :SGTST122-TS-FCHT-CMCO-ONL
                        :TS-FCHT-CMCO-ONL-N
                      , :SGTST122-TS-RBT-CMCO-ONL
                        :TS-RBT-CMCO-ONL-N
                FROM  DB2SGT.LDR_SIS
                WHERE DT_INCL_LDR   = :SGTST122-DT-INCL-LDR
           END-EXEC

           IF  SQLCODE                     EQUAL +100
               INITIALIZE                  SGTST122-AREA-DADOS
               MOVE  0100              TO  CD-RTN-PGM
               MOVE  'Registro inexistente.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GUARDA-CACHE
           .
       700000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       980101-GUARDA-CACHE             SECTION.
      *-----------------------------------------------------------------
           MOVE  0019                  TO  PSC-ERRO-PGM

           MOVE  SGTST122-CHAVE        TO  CHAVE-CACHE
           MOVE  SGTST122-AREA-DADOS   TO  DADOS-CACHE
           .
       980101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980102-GRAVA-LOG                SECTION.
      *-----------------------------------------------------------------
           MOVE      0020                 TO  PSC-ERRO-PGM

           MOVE      'LDR_SIS'            TO  SGTS014G-NM-ENT
           MOVE      'SGTST122'           TO  SGTS014G-NM-PGM

           MOVE  SGTST122-CD-CHV-USU-ATL  TO  SGTS014G-CD-CHV-USU

           MOVE  SGTST122-DT-INCL-LDR     TO  SGTS014G-CHV-PRMR

           MOVE  LENGTH OF DADOS-LOG      TO  EIBCALEN

           CALL  SGTS014G           USING  DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM         NOT EQUAL ZEROS
               SET   SGTST122-ERRO-DE-PGM
                                       TO  TRUE
               MOVE  SGTS014G          TO  PGM-ERRO
               MOVE  0112              TO  CD-RTN-PGM
               MOVE  'Erro na gravação das informações do Log.'
                                       TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF
           .
      *--------------------
      * 999000-ERRO                    SECTION
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

           MOVE  RTN-PGM               TO  SGTST122-RTN-PGM
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


      *---------------------- FIM SGTST122 ----------------------------