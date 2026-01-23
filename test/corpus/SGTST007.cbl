      *-----------------------------------------------------------------
      * Programa..: SGTST007
      * Analista..: F2054109 - Cintia Ap Soares Iglesias
      * Autor.....: F8186823 - Paulo Sergio Miranda Duarte
      * Data......: 09.04.2015
      * Objetivo..: Persistencia da Tabela TIP_EST_OPR_ETRD
      * Compilacao: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS003 29/11/2018 F9158937 - Deixar o programa como Corporativo
      * VRS002 10/02/2017 F8785286 DE 2016/145195 - Inclusão campo
      *              SGTS0007-CD-USU-RSP-ATL para chave log.
      * VRS001 09.04.2015 - F8186823 - DE 2015/134565 - Implantacao.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTST007.

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
       01  CTE-PROG                    PIC  X(17)
                                       VALUE '*** SGTST007 ***'.
       01  CTE-VERS                    PIC  X(06) VALUE 'VRS003'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTS014G                PIC  X(08) VALUE 'SGTS014G'.

      *--> Definição dos Variaveis do Cobolito
       COPY ALMKCBL1.

      *--> Registro da tabela
       01  REG-CACHE.
           03  CHAVE-CACHE.
               05  CD-TIP-EST-OPR      PIC S9(09) COMP-5 VALUE ZEROS.
           03  DADOS-CACHE.
               05  TX-DCR-TIP-EST-OPR  PIC  X(50) VALUE SPACES.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
       COPY HLPKE01B.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE ZEROS.

      *--> Indicadores de nulidades
       01  QT-REG-SELD-NULO            PIC S9(04) COMP-5 VALUE ZEROS.

      *--> Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-TIP-EST-OPR-LOG      PIC  9(10) VALUE ZEROS.

      *--> Book Usado na Subrotina de Ocorrência de Erro
       COPY HLPKE001.

      *-----------------------------------------------------------------
      *    Books  das Subrotinas chamadas
      *-----------------------------------------------------------------

      *--> Book dos dados do log
       01  DADOS-LOG.
       COPY SGTK014G.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
       LINKAGE                         SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
       COPY SGTKT007.

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

           IF  NOT EXECUCAO-ONLINE  AND
               NOT EXECUCAO-BATCH
               CALL  CICS5000       USING  CICS5000-COMMAREA
           END-IF

      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
       COPY ALMKCBL2.

      *--> Habilita tratamento automatico de Erros DB2
       COPY HLPKE002.

      *--> Habilita captura automatica de abend e erros CICS
       COPY HLPKE003.

           PERFORM  100000-VALIDAR-PARAMETRO

           EVALUATE  TRUE
               WHEN  SGTST007-INCLUIR
                     PERFORM  400000-INCLUIR

               WHEN  SGTST007-ALTERAR
                     PERFORM  500000-ALTERAR

               WHEN  SGTST007-EXCLUIR
                     PERFORM  600000-EXCLUIR

               WHEN  SGTST007-CONSULTAR
                     IF  CHAVE-CACHE       EQUAL  SGTST007-CHAVE
                         MOVE  DADOS-CACHE
                                       TO  SGTST007-AREA-DADOS
                     ELSE
                         PERFORM  700000-CONSULTA
                     END-IF
           END-EVALUATE

           GOBACK

           .
       000000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       100000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  PSC-ERRO-PGM

           INITIALIZE                      SGTST007-RTN-PGM

           IF  NOT SGTST007-INCLUIR  AND
               NOT SGTST007-EXCLUIR  AND
               NOT SGTST007-ALTERAR  AND
               NOT SGTST007-CONSULTAR
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0116              TO  CD-RTN-PGM
               MOVE  'Ação inválida.'  TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           PERFORM 110000-VALIDAR-CAMPOS

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-CAMPOS           SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  PSC-ERRO-PGM

           IF  SGTST007-CD-TIP-EST-OPR     EQUAL  ZEROS
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0001              TO  CD-RTN-PGM
               MOVE  'Código do tipo estado operação inválido.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTST007-INCLUIR
           OR  SGTST007-ALTERAR
               IF  SGTST007-TX-DCR-TIP-EST-OPR
                                           EQUAL  SPACES
                   SET   ERRO-CODIFICADO
                                       TO  TRUE
                   MOVE  0002          TO  CD-RTN-PGM
                   MOVE  'Texto da descrição tipo estado operação inváli
      -                  'do.'         TO  MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF

           IF  SGTST007-INCLUIR
           OR  SGTST007-ALTERAR
           OR  SGTST007-EXCLUIR
               IF  SGTS0007-CD-USU-RSP-ATL EQUAL  SPACES
                   SET   ERRO-CODIFICADO
                                       TO  TRUE
                   MOVE  0003          TO  CD-RTN-PGM
                   MOVE  'Código usuário responsável atualizacão inválid
      -                  'o.'          TO  MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       400000-INCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  PSC-ERRO-PGM

      *--> Desligar erro automatico do DB2
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC

           EXEC SQL
                INSERT INTO DB2SGT.TIP_EST_OPR_ETRD
                ( CD_TIP_EST_OPR
                , TX_DCR_TIP_EST_OPR )
                VALUES
                ( :SGTST007-CD-TIP-EST-OPR
                , :SGTST007-TX-DCR-TIP-EST-OPR )
           END-EXEC

      *--> Ligar erro automatico do DB2
           EXEC SQL WHENEVER SQLERROR   GO TO 999002-ERRO-DB2 END-EXEC

           EVALUATE  SQLCODE
               WHEN  -803
                     MOVE  'Inclusão nao efetuada, registro existente.'
                                       TO  MSG-ERRO
                     MOVE  SQLCODE     TO  CD-RTN-PGM
                     PERFORM 999001-ERRO-001

               WHEN  ZEROS
                     CONTINUE

               WHEN  OTHER
      *------------> Aciona o tratamento erro para os demais casos
                     PERFORM 999002-ERRO-DB2
           END-EVALUATE

           PERFORM 980101-GUARDA-CACHE

           INITIALIZE                      DADOS-LOG
           MOVE  'I'                   TO  SGTS014G-CD-TIP-ACAO

           PERFORM  980102-GRAVA-LOG
           .
       400000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       500000-ALTERAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  PSC-ERRO-PGM

           EXEC SQL
             UPDATE DB2SGT.TIP_EST_OPR_ETRD
             SET TX_DCR_TIP_EST_OPR = :SGTST007-TX-DCR-TIP-EST-OPR
                WHERE CD_TIP_EST_OPR     = :SGTST007-CD-TIP-EST-OPR
           END-EXEC

           IF  SQLCODE                     EQUAL  +100
               INITIALIZE                  CHAVE-CACHE
               MOVE  'Alteração não efetuada, registro inexistente'
                                       TO  MSG-ERRO
               MOVE  SQLCODE           TO  CD-RTN-PGM
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
       600000-EXCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  PSC-ERRO-PGM

           INITIALIZE                      DADOS-LOG
           MOVE  'E'                   TO  SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG

           EXEC SQL
                DELETE FROM DB2SGT.TIP_EST_OPR_ETRD
                WHERE CD_TIP_EST_OPR    =  :SGTST007-CD-TIP-EST-OPR
           END-EXEC

           INITIALIZE                      REG-CACHE

           IF  SQLCODE                     EQUAL  +100
               MOVE  'Exclusão não efetuada, registro inexistente.'
                                       TO  MSG-ERRO
               MOVE  SQLCODE           TO  CD-RTN-PGM
               PERFORM 999001-ERRO-001
           END-IF
           .
       600000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       700000-CONSULTA                 SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  PSC-ERRO-PGM

           EXEC SQL
                 SELECT TX_DCR_TIP_EST_OPR
                   INTO  :SGTST007-TX-DCR-TIP-EST-OPR
                       FROM DB2SGT.TIP_EST_OPR_ETRD
                WHERE CD_TIP_EST_OPR     = :SGTST007-CD-TIP-EST-OPR
           END-EXEC

           IF  SQLCODE                     EQUAL  +100
               INITIALIZE                  SGTST007-AREA-DADOS
               MOVE  'Tipo estado operação estruturado inexistente.'
                                       TO  MSG-ERRO
               MOVE  SQLCODE           TO  CD-RTN-PGM
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GUARDA-CACHE
           .
       700000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980101-GUARDA-CACHE             SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  PSC-ERRO-PGM

           MOVE  SGTST007-CHAVE        TO  CHAVE-CACHE
           MOVE  SGTST007-AREA-DADOS   TO  DADOS-CACHE
           .
       980101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980102-GRAVA-LOG                SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  PSC-ERRO-PGM

           MOVE  'TIP_EST_OPR_ETRD'    TO  SGTS014G-NM-ENT

           IF  EXECUCAO-ONLINE
               MOVE  SGTS0007-CD-USU-RSP-ATL
                                       TO  SGTS014G-CD-CHV-USU
      *        EXEC CICS
      *             ASSIGN USERID(SGTS014G-CD-CHV-USU)
      *        END-EXEC
           ELSE
               MOVE  'SISTEMA'         TO  SGTS014G-CD-CHV-USU
           END-IF

           MOVE  'SGTST007'            TO  SGTS014G-NM-PGM
           MOVE  SGTST007-CD-TIP-EST-OPR
                                       TO  CD-TIP-EST-OPR-LOG

           STRING CD-TIP-EST-OPR-LOG
               DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR

           MOVE  LENGTH OF DADOS-LOG   TO  EIBCALEN

           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM         NOT EQUAL  ZEROS
               MOVE  SGTS014G-MSG-ERRO-PGM
                                       TO  MSG-ERRO-PGM
               PERFORM 999001-ERRO-001
           END-IF
           .
       980102-SAI.
           EXIT.

      *-----------------------------------------------------------------
      * 999000-ERRO                    SECTION
      *-----------------------------------------------------------------
      *----------------------------
      * 999001-ERRO-001.
      *----------------------------
       COPY HLPKE004.
       COPY HLPKE012.

           MOVE  RTN-PGM               TO  SGTST007-RTN-PGM

           PERFORM  999000-ENCERRAR

           .
       999001-SAI.
           EXIT.

      *----------------------------
      * 999002-ERRO-DB2
      *----------------------------
       COPY HLPKE005.
      *----------------------------
      * 999003-TRATA-ABEND-CICS
      * 999004-TRATA-CONDITION-CICS
      *----------------------------
       COPY HLPKE006.

      *-----------------------------------------------------------------
       999000-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------

           GOBACK
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTST007 -----------------------------
