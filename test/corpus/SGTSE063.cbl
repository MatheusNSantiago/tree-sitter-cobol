      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSE063
      * ANALISTA..: F9188847 - SOILA KARLA LINO AKAMATSU TARDELLI
      * AUTOR.....: F9188847 - SOILA KARLA LINO AKAMATSU TARDELLI
      * DATA......: 01/02/2017
      * OBJETIVO..: EXCLUSAO DA TABELA IN_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS002 29/03/2019 - F2804421 2016/145195 - Incluir tratamento
      *                    para o campo SGTSE063-NR-IN-OPR-ETRD.
      * VRS001 01/02/2017 - F9188847 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSE063.

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
       77  CTE-INICIO-SS                 PIC  X(040)  VALUE
                  '*** S.S. COMECA AQUI ***'.
       77  CTE-PROG                PIC  X(17) VALUE '*** SGTSE063 ***'.
       77  CTE-VERS                PIC  X(06) VALUE 'VRS002'.
       77  SGTS014G                PIC  X(08) VALUE 'SGTS014G'.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE 0.
       01  IND                         PIC X(01).

      *--> Indicadores de nulidades

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *--> Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG            PIC  9(10) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG           PIC  9(11) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG           PIC  9(5) VALUE 0.
           03  CD-TIP-IN-LOG                 PIC  9(5) VALUE 0.
           03  NR-IN-OPR-ETRD-LOG            PIC  9(5) VALUE 0.

      *--> Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book dos Container do Operador
-INC IIBKOPRD

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999

      *--> Book de para acionamento do MSGS0001
-INC MSGKWMG1

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book do container de requisição e resposta deste módulo
-INC SGTKE063

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                  PIC X(40)      VALUE
                   '*** STORAGE SECTION TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

           IF  NOT  EXECUCAO-ONLINE  AND
               NOT  EXECUCAO-BATCH
               CALL CICS5000        USING  CICS5000-COMMAREA
           END-IF

           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO
      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997

      *--> Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM  100000-DESCOBRIR-CANAL
           PERFORM  120000-EXCLUIR-CTNR-ERRO
           PERFORM  130000-RECEBER-REQUISICAO
           PERFORM  140000-VALIDA-PARAMETRO

           PERFORM  200000-EXCLUIR
           PERFORM  300000-POSTA-RESPOSTA

           GOBACK
           .

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           SET   DEL-CONTAINER         TO  TRUE

           EXEC CICS DELETE
                     CONTAINER ( 'PRM-ERRO'  )
                     CHANNEL   ( NM-CNL      )
                     NOHANDLE
           END-EXEC

           IF  NOT (EIBRESP                EQUAL  DFHRESP(NORMAL)  OR
                    EIBRESP                EQUAL  DFHRESP(CONTAINERERR))
               SET   ERRO-CONTAINER    TO  TRUE
               PERFORM  999002-TRATA-CONDITION-CICS
           END-IF
           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                     TO  QT-CCT-LIDO
           MOVE  LENGTH OF SGTSE063-RQSC   TO  QT-CCT-EPRO

           MOVE 'SGTSE063-RQSC'            TO NM-CTNR

           SET   ERRO-CONTAINER            TO  TRUE
           SET   GET-CONTAINER             TO  TRUE

           EXEC CICS GET
                     CONTAINER ( NM-CTNR         )
                     CHANNEL   ( NM-CNL          )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF


      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER ( NM-CTNR                 )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSE063-RQSC )
                     INTO      ( SGTSE063-RQSC           )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDA-PARAMETRO         SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSE063-CD-CTRA-TIT-VL     EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.' TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE063-CD-CTRA-TIT-VL     NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE063-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE063-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'         TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE063-CD-OPR-ETRD-MCP    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais in
      -              'válido.'         TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE063-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE063-NR-VRS-OPR-ETRD    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido
      -              '.'               TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE063-CD-TIP-IN          NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0070              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do tipo de indicador inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE063-NR-IN-OPR-ETRD EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0930             TO  MSG1-CD-ERRO-TS
               MOVE  'Número sequencial do indicador não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-EXCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE DADOS-LOG

           MOVE  'E'                   TO  SGTS014G-CD-TIP-ACAO

           MOVE SPACES                 TO IND

           EXEC SQL
                SELECT 'S'
                INTO
                    :IND
                FROM DB2SGT.IN_OPR_ETRD
                WHERE CD_CTRA_TIT_VL    =  :SGTSE063-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP   =  :SGTSE063-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD   =  :SGTSE063-NR-VRS-OPR-ETRD
                  AND CD_TIP_IN         =  :SGTSE063-CD-TIP-IN
                  AND NR_IN_OPR_ETRD    =  :SGTSE063-NR-IN-OPR-ETRD
           END-EXEC

           IF IND EQUAL 'S'
              PERFORM  980102-GRAVA-LOG
           END-IF

           EXEC SQL
                DELETE FROM DB2SGT.IN_OPR_ETRD
                WHERE CD_CTRA_TIT_VL    =  :SGTSE063-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP   =  :SGTSE063-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD   =  :SGTSE063-NR-VRS-OPR-ETRD
                  AND CD_TIP_IN         =  :SGTSE063-CD-TIP-IN
                  AND NR_IN_OPR_ETRD    =  :SGTSE063-NR-IN-OPR-ETRD
           END-EXEC

           IF  SQLCODE                     EQUAL +100
               SET   ERRO-PROGRAMADO          TO  TRUE
               MOVE  0100                     TO  MSG1-CD-ERRO-TS
               MOVE  'Registro inexistente.'  TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'Exclusão efetuada com sucesso.'
                                       TO  SGTSE063-TX-RTN-PRCT

           MOVE 'SGTSE063-RPST'        TO  NM-CTNR

           SET   ERRO-CONTAINER        TO  TRUE
           SET   PUT-CONTAINER         TO  TRUE

           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                 )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSE063-RPST )
                     FROM      ( SGTSE063-RPST           )
           END-EXEC

           SET SEM-ERRO                TO  TRUE
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980102-GRAVA-LOG                SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'IN_OPR_ETRD       '      TO  SGTS014G-NM-ENT

           IF  EXECUCAO-ONLINE
               PERFORM  980103-LER-DADOS-OPRD
               MOVE  KOPRD-CD-OPRD         TO  SGTS014G-CD-CHV-USU
           ELSE
               MOVE  'SYSTEM'              TO  SGTS014G-CD-CHV-USU
           END-IF

           MOVE  'SGTSE063'                TO  SGTS014G-NM-PGM
           MOVE  SGTSE063-CD-CTRA-TIT-VL   TO  CD-CTRA-TIT-VL-LOG
           MOVE  SGTSE063-CD-OPR-ETRD-MCP  TO  CD-OPR-ETRD-MCP-LOG
           MOVE  SGTSE063-NR-VRS-OPR-ETRD  TO  NR-VRS-OPR-ETRD-LOG
           MOVE  SGTSE063-CD-TIP-IN        TO  CD-TIP-IN-LOG
           MOVE  SGTSE063-NR-IN-OPR-ETRD   TO  NR-IN-OPR-ETRD-LOG

           STRING  CD-CTRA-TIT-VL-LOG
                   ';'    CD-OPR-ETRD-MCP-LOG
                   ';'    NR-VRS-OPR-ETRD-LOG
                   ';'    CD-TIP-IN-LOG
                   ';'    NR-IN-OPR-ETRD-LOG
                   DELIMITED BY SIZE
                   INTO                    SGTS014G-CHV-PRMR
           END-STRING

           MOVE  LENGTH OF DADOS-LOG   TO  EIBCALEN

           CALL  SGTS014G           USING  DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM         NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO    TO  TRUE
               MOVE  112                       TO  MSG1-CD-ERRO-TS
               MOVE 'Erro na gravação das informações do Log.'
                                               TO  MSG1-TX-ERRO-TS
               MOVE  SGTS014G                  TO  MSG1-NM-PGM-ACND
               MOVE  SGTS014G-MSG-ERRO-PGM     TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTS014G-CD-RTN-PGM       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       980102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980103-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-INF-OPRD'        TO  NM-CTNR

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           INITIALIZE PRM-INF-OPRD

           EXEC CICS GET
                     CONTAINER ( NM-CTNR                )
                     INTO      ( PRM-INF-OPRD           )
                     FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       980103-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL SPACES
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

           MOVE  'SGTSE063-CD-CTRA-TIT-VL'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSE063-CD-CTRA-TIT-VL        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSE063-CD-OPR-ETRD-MCP'     TO  MSGKW999-NM-VRV-MNT
           IF  SGTSE063-CD-OPR-ETRD-MCP         IS  NUMERIC
               MOVE  SGTSE063-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX         TO  MSGKW999-VL-VRV-MNT
           ELSE
0              MOVE  'Valor não numérico.'      TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSE063-NR-VRS-OPR-ETRD'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSE063-NR-VRS-OPR-ETRD       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSE063-CD-TIP-IN'           TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSE063-CD-TIP-IN             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSE063-NR-IN-OPR-ETRD'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSE063-NR-IN-OPR-ETRD        TO  MSGKW999-VL-VRV-MNT
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
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
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

      *---------------------- FIM SGTSE063 -----------------------------