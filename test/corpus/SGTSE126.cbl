      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSE126
      * ANALISTA..: F9158937 - SILVIA AGUIAR DE OLIVEIRA
      * AUTOR.....: F9158937 - SILVIA AGUIAR DE OLIVEIRA
      * OBJETIVO..: EXCLUSAO NA TABELA ACSS_CTRA_TVM
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 19.07.2023 - F9158937 - IMPLANTACAO                     *
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSE126.
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
                                       VALUE '*** SGTSE126 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

       77  SGTS014G                    PIC  X(08) VALUE 'SGTS014G'.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      *--> Area de trabalho
       01  CD-USU-ATL                  PIC  X(08) VALUE SPACES.

      *--> Variáveis para gravação do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TVM-LOG         PIC  9(10) VALUE 0.
           03  CD-UOR-ACSS-CTRA-LOG    PIC  9(10) VALUE 0.

      *--> Area para a  consulta
       01 AREA-CONSULTA.
          03 AUX-CD-CTRA-TVM           PIC S9(9) COMP-5 VALUE 0.
          03 AUX-CD-UOR-ACSS-CTRA      PIC S9(9) COMP-5 VALUE 0.


      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book dos Container do Operador
-INC IIBKOPRD

      *--> Book trata data e hora atual
-INC HLPKTIME

      *--> Book dos dados do Log
       01  DADOS-LOG.
-INC SGTK014G

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book do container de requisicao e resposta deste modulo
-INC SGTKE126

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       77  CTE-FINAL-SS                PIC  X(40) VALUE
               '*** Storage Section termina aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

           IF  NOT EXECUCAO-ONLINE  AND
               NOT EXECUCAO-BATCH
               CALL  CICS5000       USING  CICS5000-COMMAREA
           END-IF

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997

      *--> Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999


           PERFORM  100000-INICIAR

           PERFORM  200000-EXCLUIR

           PERFORM  300000-POSTAR-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  110000-DESCOBRIR-CANAL

           PERFORM  120000-EXCLUIR-CTNR-ERRO

           PERFORM  130000-RECEBER-REQUISICAO

           PERFORM  150000-VALIDAR-CAMPOS

           PERFORM  170000-LER-DADOS-OPRD
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           SET  ERRO-CANAL  TO  TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

           SET   SEM-ERRO              TO  TRUE
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

      *    Limpar variáveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE 'PRM-ERRO'             TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE
           SET ERRO-CONTAINER          TO TRUE

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER (NM-CTNR   )
                     RESP      (MSG1-RESP1)
                     RESP2     (MSG1-RESP2)
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE

           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-CCT-LIDO
           MOVE  LENGTH OF SGTSE126-RQSC
                                       TO  QT-CCT-EPRO
      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE


           EXEC CICS GET
                     CONTAINER ( 'SGTSE126-RQSC' )
                     CHANNEL   ( NM-CNL          )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA TO TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
                     CONTAINER ( 'SGTSE126-RQSC'         )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSE126-RQSC )
                     INTO      ( SGTSE126-RQSC           )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALIDAR-CAMPOS           SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Chave
           IF  SGTSE126-CD-CTRA-TVM     EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'  TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE126-CD-CTRA-TVM      NOT POSITIVE
               SET   ERRO-PROGRAMADO    TO  TRUE
               MOVE  0001               TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE126-CD-UOR-ACSS-CTRA    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO    TO  TRUE
               MOVE  604                TO  MSG1-CD-ERRO-TS
               MOVE  'Código UOR informado inválido.'
                                        TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       150000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO  TRUE
           MOVE  'PRM-INF-OPRD'        TO  NM-CTNR

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           INITIALIZE                      PRM-INF-OPRD

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                              )
                      INTO      ( PRM-INF-OPRD                         )
                      FLENGTH   ( LENGTH OF PRM-INF-OPRD               )
           END-EXEC
      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE


           IF  EXECUCAO-ONLINE
               MOVE  KOPRD-CD-OPRD     TO  CD-USU-ATL
           ELSE
               MOVE  'SISTEMA'         TO  CD-USU-ATL
           END-IF

           .
       170000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-EXCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

      *--> Verifica se o registro a ser excluido existe
      *--> Desligar erro automatico do DB2
           EXEC SQL WHENEVER SQLERROR
                    CONTINUE
           END-EXEC

           EXEC SQL
                SELECT CD_CTRA_TVM, CD_UOR_ACSS_CTRA
                INTO
                   :AUX-CD-CTRA-TVM ,
                   :AUX-CD-UOR-ACSS-CTRA
                FROM DB2SGT.ACSS_CTRA_TVM
                WHERE CD_CTRA_TVM        = :SGTSE126-CD-CTRA-TVM
                  AND CD_UOR_ACSS_CTRA   = :SGTSE126-CD-UOR-ACSS-CTRA
           END-EXEC

      *--> Ligar erro automático do DB2
           EXEC SQL WHENEVER SQLERROR
                GO TO 999010-TRATA-ERRO-DB2
           END-EXEC

           EVALUATE  SQLCODE
               WHEN +100
                    SET   ERRO-PROGRAMADO     TO  TRUE
                    MOVE  100                 TO  MSG1-CD-ERRO-TS
                    MOVE  'Exclusão não efetuada, registro inexistente.'
                                               TO  MSG1-TX-ERRO-TS
                    PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               WHEN ZEROS
                    CONTINUE
               WHEN OTHER
      *--> Aciona o tratamento de erro padrao para os demais casos
                     PERFORM  999010-TRATA-ERRO-DB2
           END-EVALUATE

           PERFORM  981000-GRAVAR-LOG

            MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

      *--> Exclui o registro
      *--> Desligar erro automatico do DB2
           EXEC SQL WHENEVER SQLERROR
                    CONTINUE
           END-EXEC

           EXEC SQL
                DELETE FROM DB2SGT.ACSS_CTRA_TVM
                WHERE CD_CTRA_TVM        = :SGTSE126-CD-CTRA-TVM
                  AND CD_UOR_ACSS_CTRA   = :SGTSE126-CD-UOR-ACSS-CTRA
           END-EXEC

      *--> Ligar erro automático do DB2
           EXEC SQL WHENEVER SQLERROR
                GO TO 999010-TRATA-ERRO-DB2
           END-EXEC

           EVALUATE  SQLCODE
               WHEN +100
                    SET   ERRO-PROGRAMADO     TO  TRUE
                    MOVE  100                 TO  MSG1-CD-ERRO-TS
                    MOVE  'Exclusão não efetuada, registro inexistente.'
                                               TO  MSG1-TX-ERRO-TS
                    PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               WHEN ZEROS
                    CONTINUE
               WHEN OTHER
      *--> Aciona o tratamento de erro padrao para os demais casos
                     PERFORM  999010-TRATA-ERRO-DB2
           END-EVALUATE

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'Exclusão efetuada com sucesso.'
                                       TO  SGTSE126-TX-RTN-PRCT
           EXEC CICS PUT
                     CONTAINER ( 'SGTSE126-RPST'         )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSE126-RPST )
                     FROM      ( SGTSE126-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       981000-GRAVAR-LOG               SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      DADOS-LOG

           SET   SGTS014G-EXCLUSAO     TO  TRUE
           MOVE  'ACSS_CTRA_TVM'       TO  SGTS014G-NM-ENT
           MOVE  CD-USU-ATL            TO  SGTS014G-CD-CHV-USU
           MOVE  'SGTSE126'            TO  SGTS014G-NM-PGM

           MOVE  SGTSE126-CD-CTRA-TVM
                                       TO  CD-CTRA-TVM-LOG
           MOVE  SGTSE126-CD-UOR-ACSS-CTRA
                                       TO  CD-UOR-ACSS-CTRA-LOG
           STRING   CD-CTRA-TVM-LOG
                   ';' CD-UOR-ACSS-CTRA-LOG
                   DELIMITED BY SIZE
                   INTO                SGTS014G-CHV-PRMR

           MOVE  LENGTH OF DADOS-LOG   TO  EIBCALEN

           CALL  SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM         NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  112               TO  MSG1-CD-ERRO-TS
               MOVE 'Erro na gravação das informações do Log.'
                                       TO  MSG1-TX-ERRO-TS
               MOVE  SGTS014G          TO  MSG1-NM-PGM-ACND
               MOVE  SGTS014G-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTS014G-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SGTS014G-PSC-ERRO-PGM
                                       TO  MSG1-CD-SEQL-PGM-ACND
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       981000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA                     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Verifica se o tipo de erro gerado possui tratamento interno
           EVALUATE  TRUE
               WHEN  ERRO-CONTAINER
                     PERFORM  999004-TRATA-ERRO-CTNR
               WHEN  ERRO-TAMANHO-AREA
                     PERFORM  999005-TRATA-ERRO-AREA-DADOS
               WHEN  ERRO-CANAL
                     PERFORM  999006-TRATA-ERRO-CANAL
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

      *--> Adiciona Chave da Operação Estruturada
           MOVE 'SGTSE126-CD-CTRA-TVM'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSE126-CD-CTRA-TVM
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTSE126-CD-UOR-ACSS-CTRA'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSE126-CD-UOR-ACSS-CTRA
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava o erro no Sistema MSG
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

           EXEC CICS PUT
                     CONTAINER ( 'PRM-ERRO'                  )
                     CHANNEL   ( NM-CNL                      )
                     FROM      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     NOHANDLE
           END-EXEC

           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      *----------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      *----------------------------------------
-INC HLPKP999
      *----------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
-INC SGTKP999
      *----------------------------------------
      * 999010-TRATA-ERRO-DB2          SECTION.
      *----------------------------------------
-INC HLPKP997

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

      *----------------------------------------
       999999-ENCERRAR                 SECTION.
      *----------------------------------------

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.

