       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSI095
      * ANALISTA..: F8785286 - ROSELI AKEMI KAWAHATA
      * AUTOR.....: F8785286 - ROSELI AKEMI KAWAHATA
      * OBJETIVO..: INCLUSAO DA TABELA ITEM_EXCO_OPR
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 26.09.2018 - F8785286 - DE 2016/145195 - Inclusão da
      *           section 430000-BUSCA-NR-SEQUENCIA.
      *        14.05.2018 - F8785286 - DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSI095.
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
       01  CTE-PROG                PIC  X(17) VALUE '*** SGTSI095 ***'.
       01  CTE-VERS                PIC  X(06) VALUE 'VRS001'.
       77  SGTS014G                PIC  X(08) VALUE 'SGTS014G'.

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      * Book trata mensagens de erro
-INC IIBKERRO
-INC MSGKWMG1
-INC HLPKW999

      * Book utilizado pelo container
-INC SGTKERR1

      * Book trata dados do operador
-INC IIBKOPRD

      * Area de trabalho

       01  QT-REG-SELD                      PIC S9(04) COMP-5 VALUE 0.

-INC HLPKTIME
       01  TS-SQL-ATU                       PIC  X(26) VALUE ' '.

      * Indicadores de nulidades
       01  QT-REG-SELD-NULO                 PIC S9(4) COMP-5 VALUE 0.
       01  MSG1-NM-CMP                      PIC X(50) VALUE SPACES.
       01  VL-RLZD-ITEM-EXCO-N              PIC S9(4) COMP-5 VALUE 0.
       01  TX-RLZD-ITEM-EXCO-N              PIC S9(4) COMP-5 VALUE 0.
       01  NR-RLZD-ITEM-EXCO-N              PIC S9(4) COMP-5 VALUE 0.
       01  TX-OBS-ITEM-EXCO-N               PIC S9(4) COMP-5 VALUE 0.

      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG            PIC  9(10) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG           PIC  9(11) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG           PIC  9(5)  VALUE 0.
           03  VL-RLZD-ITEM-EXCO-LOG         PIC  9(19) VALUE 0.
           03  NR-SEQL-EXCO-PRFL-LOG         PIC  9(10) VALUE 0.
           03  NR-SEQL-ITEM-EXCO-LOG         PIC  9(5)  VALUE 0.

      * Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      * Book deste módulo
-INC SGTKI095

       01  CTE-FINAL-SS                     PIC X(40) VALUE
               '*** S.S. TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      * Habilita tratamento automatico de Erros DB2
-INC HLPKE997

      * Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDA-PARAMETRO

           PERFORM 200000-INCLUIR

           PERFORM 981000-GRAVA-LOG

           PERFORM 300000-GRAVAR-RESPOSTA

           GOBACK
           .
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           SET  ERRO-CANAL  TO  TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL )
           END-EXEC

           SET   SEM-ERRO              TO  TRUE
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   ( NM-CNL     )
              NOHANDLE
           END-EXEC

           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
               EIBRESP EQUAL DFHRESP(CONTAINERERR) )
               PERFORM 999002-TRATA-CONDITION-CICS
           END-IF
           .
       120000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisicao
           MOVE CTE-PROG(5:8)          TO NM-PGM
           MOVE 'SGTSI095-RQSC'        TO NM-CTNR

           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSI095-RQSC     TO QT-CCT-EPRO

           SET  GET-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS GET
              CONTAINER ( 'SGTSI095-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           SET  SEM-ERRO               TO TRUE

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA         TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           SET  GET-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS GET
              CONTAINER ( 'SGTSI095-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSI095-RQSC )
              INTO      ( SGTSI095-RQSC           )
           END-EXEC

           SET  SEM-ERRO               TO TRUE
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDA-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           PERFORM 150000-VALIDA-CAMPOS

           PERFORM 160000-TRATA-NULIDADE
           PERFORM 170001-VALIDA-EXCO-PRFL-SELD
           PERFORM 170002-VALIDA-ITC-RGR-VLDC-CRC
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       150000-VALIDA-CAMPOS SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           IF  SGTSI095-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 001                TO  MSG1-CD-ERRO-TS
               MOVE 'Código carteira título valor inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI095-CD-OPR-ETRD-MCP NOT NUMERIC
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -         ' numérico.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI095-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 004                TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturado mercado de capitais inv
      -        'álido.'                TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI095-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 006                TO  MSG1-CD-ERRO-TS
               MOVE 'Numero versão operação estruturado inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI095-NR-SEQL-EXCO-PRFL EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 638                TO  MSG1-CD-ERRO-TS
               MOVE 'Numero sequencial exceção perfil inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *    IF  SGTSI095-NR-SEQL-ITEM-EXCO EQUAL ZEROS
      *        SET  ERRO-PROGRAMADO    TO TRUE
      *        MOVE 721                TO  MSG1-CD-ERRO-TS
      *        MOVE 'Numero sequencial item exceção inválido.'
      *                                TO MSG1-TX-ERRO-TS
      *        PERFORM 999000-GRAVA-ERRO-E-ENCERRA
      *    END-IF

           IF  SGTSI095-CD-RGR-VLDC    EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 076                TO  MSG1-CD-ERRO-TS
               MOVE 'Código regra validação inválido' TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI095-NR-VRS-RGR-VLDC EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 083                TO  MSG1-CD-ERRO-TS
               MOVE 'Numero versão regra validação inválido'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI095-NR-SEQL-ITC    EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 413                TO  MSG1-CD-ERRO-TS
               MOVE 'Numero sequencial instrução inválido'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI095-IN-ITEM-CRIO-EXCO     EQUAL SPACES
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 722                TO  MSG1-CD-ERRO-TS
               MOVE 'Indicador item criador exceção inválido'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       150000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       160000-TRATA-NULIDADE SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           IF  SGTSI095-VL-RLZD-ITEM-EXCO EQUAL ZEROS
               MOVE -1                 TO VL-RLZD-ITEM-EXCO-N
           END-IF

           IF  SGTSI095-TX-RLZD-ITEM-EXCO EQUAL SPACES
               MOVE -1                 TO TX-RLZD-ITEM-EXCO-N
           END-IF

           IF  SGTSI095-NR-RLZD-ITEM-EXCO EQUAL ZEROS
               MOVE -1                 TO NR-RLZD-ITEM-EXCO-N
           END-IF

           IF  SGTSI095-TX-OBS-ITEM-EXCO EQUAL SPACES
               MOVE -1                 TO TX-OBS-ITEM-EXCO-N
           END-IF
           .
       160000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       170001-VALIDA-EXCO-PRFL-SELD   SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO QT-REG-SELD

           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.EXCO_PRFL_SELD
               WHERE CD_CTRA_TIT_VL     = :SGTSI095-CD-CTRA-TIT-VL
                 AND CD_OPR_ETRD_MCP    = :SGTSI095-CD-OPR-ETRD-MCP
                 AND NR_VRS_OPR_ETRD    = :SGTSI095-NR-VRS-OPR-ETRD
                 AND NR_SEQL_EXCO_PRFL  = :SGTSI095-NR-SEQL-EXCO-PRFL
           END-EXEC

           IF  QT-REG-SELD = 0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 723                TO  MSG1-CD-ERRO-TS
               MOVE 'Excecao perfil selecionado inexistente'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       170001-SAI.
           EXIT.
      *-----------------------------------------------------------------
       170002-VALIDA-ITC-RGR-VLDC-CRC SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO QT-REG-SELD

           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.ITC_RGR_VLDC_CRCT
               WHERE CD_RGR_VLDC       = :SGTSI095-CD-RGR-VLDC
                 AND NR_VRS_RGR_VLDC   = :SGTSI095-NR-VRS-RGR-VLDC
                 AND NR_SEQL_ITC       = :SGTSI095-NR-SEQL-ITC
           END-EXEC

           IF  QT-REG-SELD = 0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 724                TO  MSG1-CD-ERRO-TS
               MOVE 'Instrucao regra validação caracteristica inexistent
      -        'e'                     TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       170002-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-INCLUIR    SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO
      *
           PERFORM 430000-BUSCA-NR-SEQUENCIA
      *
           EXEC SQL
                INSERT INTO DB2SGT.ITEM_EXCO_OPR
                ( CD_CTRA_TIT_VL
                , CD_OPR_ETRD_MCP
                , NR_VRS_OPR_ETRD
                , NR_SEQL_EXCO_PRFL
                , NR_SEQL_ITEM_EXCO
                , CD_RGR_VLDC
                , NR_VRS_RGR_VLDC
                , NR_SEQL_ITC
                , IN_ITEM_CRIO_EXCO
                , VL_RLZD_ITEM_EXCO
                , TX_RLZD_ITEM_EXCO
                , NR_RLZD_ITEM_EXCO
                , TX_OBS_ITEM_EXCO )
                VALUES
                ( :SGTSI095-CD-CTRA-TIT-VL
                , :SGTSI095-CD-OPR-ETRD-MCP
                , :SGTSI095-NR-VRS-OPR-ETRD
                , :SGTSI095-NR-SEQL-EXCO-PRFL
                , :SGTSI095-NR-SEQL-ITEM-EXCO
                , :SGTSI095-CD-RGR-VLDC
                , :SGTSI095-NR-VRS-RGR-VLDC
                , :SGTSI095-NR-SEQL-ITC
                , :SGTSI095-IN-ITEM-CRIO-EXCO
                , :SGTSI095-VL-RLZD-ITEM-EXCO
                  :VL-RLZD-ITEM-EXCO-N
                , :SGTSI095-TX-RLZD-ITEM-EXCO
                  :TX-RLZD-ITEM-EXCO-N
                , :SGTSI095-NR-RLZD-ITEM-EXCO
                  :NR-RLZD-ITEM-EXCO-N
                , :SGTSI095-TX-OBS-ITEM-EXCO
                  :TX-OBS-ITEM-EXCO-N )
           END-EXEC
      *
      * Ligar erro automatico do DB2
      *
           EXEC SQL WHENEVER SQLERROR
                GO TO 999010-TRATA-ERRO-DB2
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN -803
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 803                 TO MSG1-CD-ERRO-TS
               MOVE 'Inclusão não efetuada, registro existente.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               WHEN 0
                    MOVE 'Inclusão efetuada com sucesso.'
                                        TO SGTSI095-TX-MSG-RTN
               WHEN OTHER
      * Aciona o tratamento padrao de erro padrao para os demais casos
                    GO TO 999010-TRATA-ERRO-DB2
           END-EVALUATE
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-GRAVAR-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
              CONTAINER ( 'SGTSI095-RPST'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSI095-RPST )
              FROM      ( SGTSI095-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       430000-BUSCA-NR-SEQUENCIA SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
               SELECT VALUE (MAX(NR_SEQL_ITEM_EXCO) , 0 )
                INTO :SGTSI095-NR-SEQL-ITEM-EXCO
                FROM DB2SGT.ITEM_EXCO_OPR
                WHERE CD_CTRA_TIT_VL     = :SGTSI095-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP    = :SGTSI095-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD    = :SGTSI095-NR-VRS-OPR-ETRD
                  AND NR_SEQL_EXCO_PRFL  = :SGTSI095-NR-SEQL-EXCO-PRFL
           END-EXEC

           ADD 1                       TO SGTSI095-NR-SEQL-ITEM-EXCO
           .
       430000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       981000-GRAVA-LOG SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE DADOS-LOG
           MOVE 'I'                    TO SGTS014G-CD-TIP-ACAO
           MOVE 'ITEM_EXCO_OPR     '   TO SGTS014G-NM-ENT

           IF  EXECUCAO-ONLINE
               PERFORM 981100-LE-DADOS-OPERADOR
               MOVE  KOPRD-CD-OPRD     TO SGTS014G-CD-CHV-USU
           ELSE
               MOVE 'SYSTEM'           TO SGTS014G-CD-CHV-USU
           END-IF

           MOVE 'SGTSI095'             TO SGTS014G-NM-PGM
           MOVE SGTSI095-CD-CTRA-TIT-VL    TO CD-CTRA-TIT-VL-LOG
           MOVE SGTSI095-CD-OPR-ETRD-MCP   TO CD-OPR-ETRD-MCP-LOG
           MOVE SGTSI095-NR-VRS-OPR-ETRD   TO NR-VRS-OPR-ETRD-LOG
           MOVE SGTSI095-NR-SEQL-EXCO-PRFL TO NR-SEQL-EXCO-PRFL-LOG
           MOVE SGTSI095-NR-SEQL-ITEM-EXCO TO NR-SEQL-ITEM-EXCO-LOG

           STRING CD-CTRA-TIT-VL-LOG
           ';'    CD-OPR-ETRD-MCP-LOG
           ';'    NR-VRS-OPR-ETRD-LOG
           ';'    NR-SEQL-EXCO-PRFL-LOG
           ';'    NR-SEQL-ITEM-EXCO-LOG
               DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR

           MOVE LENGTH OF DADOS-LOG    TO EIBCALEN
           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM  NOT EQUAL ZEROS
               SET   ERRO-PROGRAMA-ACIONADO TO  TRUE
               MOVE  112                    TO  MSG1-CD-ERRO-TS
               MOVE 'Erro na gravação das informações do Log.'
                                            TO  MSG1-TX-ERRO-TS
               MOVE  SGTS014G               TO  MSG1-NM-PGM-ACND
                                                NM-PGM
               MOVE  SGTS014G-MSG-ERRO-PGM  TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTS014G-CD-RTN-PGM    TO  MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       981000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       981100-LE-DADOS-OPERADOR        SECTION.
      *----------------------------------------------------------------*
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO  TRUE
           MOVE 'PRM-INF-OPRD'         TO  NM-CTNR

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO  TRUE

           INITIALIZE PRM-INF-OPRD

      *--> Lê o container com dados do usuário
      *--> não há captura automática de erro CICS
           EXEC CICS GET
                     CONTAINER ( NM-CTNR            )
                     INTO      ( PRM-INF-OPRD           )
                     FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       981100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                        TO MSG1-TX-ERRO-TS
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


           MOVE 'SGTSI095-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI095-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI095-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
           IF  SGTSI095-CD-OPR-ETRD-MCP      IS NUMERIC
               MOVE  SGTSI095-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX      TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                             TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI095-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI095-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI095-NR-SEQL-EXCO-PRFL' TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI095-NR-SEQL-ITEM-EXCO   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI095-CD-RGR-VLDC'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI095-CD-RGR-VLDC         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI095-NR-VRS-RGR-VLDC'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI095-NR-VRS-RGR-VLDC     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI095-NR-SEQL-ITC'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI095-NR-SEQL-ITC         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI095-IN-ITEM-CRIO-EXCO' TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI095-IN-ITEM-CRIO-EXCO   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI095-VL-RLZD-ITEM-EXCO' TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI095-VL-RLZD-ITEM-EXCO   TO VL-RLZD-ITEM-EXCO-LOG
           MOVE VL-RLZD-ITEM-EXCO-LOG        TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI095-TX-RLZD-ITEM-EXCO' TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI095-TX-RLZD-ITEM-EXCO   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI095-NR-RLZD-ITEM-EXCO' TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI095-NR-RLZD-ITEM-EXCO   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI095-TX-OBS-ITEM-EXCO'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI095-TX-OBS-ITEM-EXCO    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS              TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS              TO IIBKERRO-TX-ERRO

           EXEC CICS PUT
              CONTAINER ( 'PRM-ERRO'                  )
              CHANNEL   ( NM-CNL                      )
              FROM      ( IIBKERRO-PRM-ERRO           )
              FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
              NOHANDLE
           END-EXEC

           PERFORM 999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      * --------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      * --------------------------------------*
-INC HLPKP999

      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      * --------------------------------------*
-INC SGTKP999

      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
      * --------------------------------------*
-INC HLPKP997

      * --------------------------------------*
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
      * --------------------------------------*
-INC MSGKPMG1

      * --------------------------------------*
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.