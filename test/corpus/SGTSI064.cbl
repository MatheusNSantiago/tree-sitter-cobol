      *-----------------------------------------------------------------
      * Programa..: SGTSI064
      * Analista..: F2054109 Cintia Ap Soares Iglesias
      * Autor.....: F9188847 Soila Karla Lino Akamatsu Tardelli
      * Data......: 28/09/2016
      * Objetivo..: Inclusao na Tabela BCO_PCT_OPR
      * Compilação: Cobol/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 05/02/2018 - F9158937 - Acerto mensageria
      *        02/02/2017 - F8785286 - DE 2016/145195
      *                     Inclusão leitura container operador
      *                     para gravação no LOG.
      *        28/09/2016 - F9188847 - DE 2015/137233 - Implantacao
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSI064.

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
       77  CTE-PROG                    PIC  X(17) VALUE
                                       '*** SGTSI064 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

       77  SGTS014G                    PIC  X(08) VALUE 'SGTS014G'.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  IN-BCO-LDR                  PIC  X(01) VALUE SPACES.
       01  CD-USU-ATL                  PIC  X(08) VALUE SPACES.
       01  TS-ATL                      PIC  X(26) VALUE SPACES.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).
       01  PC-PRTC-BCO-TX.
           03  PC-PRTC-BCO             PIC  ZZZ9,9(4).

      *--> Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG      PIC  9(10) VALUE ZEROS.
           03  CD-OPR-ETRD-MCP-LOG     PIC  9(11) VALUE ZEROS.
           03  NR-VRS-OPR-ETRD-LOG     PIC  9(05) VALUE ZEROS.
           03  CD-BCO-PCT-LOG          PIC  9(05) VALUE ZEROS.
           03  CD-TIP-PRTC-LOG         PIC  9(10) VALUE ZEROS.

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
      *01  SGTSI064-DADOS.
-INC SGTKI064

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       77  CTE-FINAL-SS                PIC X(40) VALUE
               '*** Storage Section termina aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

           IF  NOT EXECUCAO-ONLINE  AND
               NOT EXECUCAO-BATCH
               CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997

      *--> Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM  100000-INICIAR

           PERFORM  200000-INCLUIR

           PERFORM  300000-POSTAR-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------

           PERFORM  110000-DESCOBRIR-CANAL

           PERFORM  120000-EXCLUIR-CTNR-ERRO

           PERFORM  130000-RECEBER-REQUISICAO

           PERFORM  140000-VALIDAR-PARAMETRO

           PERFORM  150000-LER-DADOS-OPRD

           PERFORM  160000-BUSCAR-TS-DT

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS  ASSIGN
                      CHANNEL ( NM-CNL                                 )
           END-EXEC

           SET   SEM-ERRO              TO  TRUE
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           SET   DEL-CONTAINER         TO  TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  DELETE
                      CONTAINER ( 'PRM-ERRO'                           )
                      CHANNEL   ( NM-CNL                               )
                      RESP      ( MSG1-RESP1                           )
                      RESP2     ( MSG1-RESP2                           )
           END-EXEC

           IF  NOT (MSG1-RESP1             EQUAL  DFHRESP(NORMAL)  OR
                    MSG1-RESP1             EQUAL  DFHRESP(CONTAINERERR))
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

           MOVE  ZEROS                 TO  QT-CCT-LIDO
           MOVE  LENGTH OF SGTSI064-RQSC
                                       TO  QT-CCT-EPRO

           EXEC CICS  GET
                      CONTAINER ( 'SGTSI064-RQSC'                      )
                      CHANNEL   ( NM-CNL                               )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO                          )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS  GET
                      CONTAINER ( 'SGTSI064-RQSC'                      )
                      CHANNEL   ( NM-CNL                               )
                      FLENGTH   ( LENGTH OF SGTSI064-RQSC              )
                      INTO      ( SGTSI064-RQSC                        )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSI064-CD-CTRA-TIT-VL     EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores monetários
      -              'não informado.'  TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI064-CD-CTRA-TIT-VL     NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores monetários
      -              'inválido.'       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI064-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI064-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'         TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI064-CD-OPR-ETRD-MCP    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais in
      -              'válido.'         TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI064-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI064-NR-VRS-OPR-ETRD    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão operação estruturado inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI064-CD-BCO-PCT         NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0024              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do banco participante inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI064-CD-TIP-PRTC        NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0008              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do tipo participacao inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  FUNCTION UPPER-CASE      (SGTSI064-IN-BCO-LDR)
                                       TO  IN-BCO-LDR
           IF  IN-BCO-LDR                  NOT EQUAL  'S'  AND  'N'
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0102              TO  MSG1-CD-ERRO-TS
               MOVE  'Indicador banco líder inválido'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI064-PC-PRTC-BCO        NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0103              TO  MSG1-CD-ERRO-TS
              MOVE  'Percentual de participação do banco não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI064-TS-ULT-ATL         EQUAL SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0106              TO  MSG1-CD-ERRO-TS
               MOVE  'Timestamp última atualização inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           MOVE  'PRM-INF-OPRD'        TO  NM-CTNR

           INITIALIZE                      PRM-INF-OPRD

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                              )
                      INTO      ( PRM-INF-OPRD                         )
                      FLENGTH   ( LENGTH OF PRM-INF-OPRD               )
           END-EXEC

           IF  EXECUCAO-ONLINE
               MOVE  KOPRD-CD-OPRD     TO  CD-USU-ATL
           ELSE
               MOVE  'SISTEMA'         TO  CD-USU-ATL
           END-IF
           .
       150000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       160000-BUSCAR-TS-DT              SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           CALL  SBTIMEDT           USING  HLPKE001-CD-FUC
                                           HLPKE001-AREA-TRB
                                           HLPKE001-FMT-TS
                                           HLPKE001-TAM-TS
                                           HLPKE001-TS-FMTD

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  0050              TO  MSG1-CD-ERRO-TS
               MOVE  'Erro ao obter data e hora atual.'
                                       TO  MSG1-TX-ERRO-TS
                                           MSG1-TX-RTN-PGM-ACND
               MOVE  SBTIMEDT          TO  MSG1-NM-PGM-ACND
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  HLPKE001-TS-FMTD      TO  TS-ATL

           IF  SGTSI064-TS-ULT-ATL         EQUAL 'CURRENT'
               MOVE  TS-ATL            TO  SGTSI064-TS-ULT-ATL
           END-IF

           .
       160000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-INCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

      *--> Desligar erro automatico do DB2
           EXEC SQL WHENEVER SQLERROR
                    CONTINUE
           END-EXEC

           EXEC SQL
                INSERT INTO DB2SGT.BCO_PCT_OPR
                ( CD_CTRA_TIT_VL
                , CD_OPR_ETRD_MCP
                , NR_VRS_OPR_ETRD
                , CD_BCO_PCT
                , CD_TIP_PRTC
                , IN_BCO_LDR
                , PC_PRTC_BCO
                , CD_USU_ULT_ATL
                , TS_ULT_ATL )
                VALUES
                ( :SGTSI064-CD-CTRA-TIT-VL
                , :SGTSI064-CD-OPR-ETRD-MCP
                , :SGTSI064-NR-VRS-OPR-ETRD
                , :SGTSI064-CD-BCO-PCT
                , :SGTSI064-CD-TIP-PRTC
                , :SGTSI064-IN-BCO-LDR
                , :SGTSI064-PC-PRTC-BCO
                , :CD-USU-ATL
                , :SGTSI064-TS-ULT-ATL )
           END-EXEC

      *--> Ligar erro automatico do DB2
           EXEC SQL WHENEVER SQLERROR
                    GO TO  999010-TRATA-ERRO-DB2
           END-EXEC

           EVALUATE  SQLCODE
               WHEN  -803

                     SET   ERRO-PROGRAMADO   TO  TRUE
                     MOVE  803               TO  MSG1-CD-ERRO-TS
                     MOVE  'Inclusão não efetuada, registro existente.'
                                             TO  MSG1-TX-ERRO-TS
                     PERFORM 999000-GRAVA-ERRO-E-ENCERRA

               WHEN  ZEROS
                     CONTINUE

               WHEN OTHER
      *--> Aciona o tratamento de erro padrao para os demais casos
                    PERFORM  999010-TRATA-ERRO-DB2
           END-EVALUATE

           INITIALIZE                      DADOS-LOG
           MOVE  'I'                   TO  SGTS014G-CD-TIP-ACAO

           PERFORM  981000-GRAVAR-LOG
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'Inclusão efetuada com sucesso.'
                                       TO  SGTSI064-TX-RTN-PRCT

           EXEC CICS  PUT
                      CONTAINER ( 'SGTSI064-RPST'                      )
                      CHANNEL   ( NM-CNL                               )
                      FLENGTH   ( LENGTH OF SGTSI064-RPST              )
                      FROM      ( SGTSI064-RPST                        )
           END-EXEC

           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       981000-GRAVAR-LOG               SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'BCO_PCT_OPR       '  TO  SGTS014G-NM-ENT
           MOVE  CD-USU-ATL            TO  SGTS014G-CD-CHV-USU
           MOVE  'SGTSI064'            TO  SGTS014G-NM-PGM

           MOVE  SGTSI064-CD-CTRA-TIT-VL
                                       TO  CD-CTRA-TIT-VL-LOG
           MOVE  SGTSI064-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP-LOG
           MOVE  SGTSI064-NR-VRS-OPR-ETRD
                                       TO  NR-VRS-OPR-ETRD-LOG
           MOVE  SGTSI064-CD-BCO-PCT   TO  CD-BCO-PCT-LOG
           MOVE  SGTSI064-CD-TIP-PRTC  TO  CD-TIP-PRTC-LOG

           STRING  CD-CTRA-TIT-VL-LOG      ';'
                   CD-OPR-ETRD-MCP-LOG     ';'
                   NR-VRS-OPR-ETRD-LOG     ';'
                   CD-BCO-PCT-LOG          ';'
                   CD-TIP-PRTC-LOG
                   DELIMITED BY SIZE
                   INTO                    SGTS014G-CHV-PRMR

           MOVE  LENGTH OF  DADOS-LOG  TO  EIBCALEN
           CALL  SGTS014G           USING  DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM         NOT EQUAL ZEROS
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
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       981000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *Verifica se o tipo de erro gerado possui tratamento interno
           EVALUATE TRUE
               WHEN ERRO-CONTAINER
                    PERFORM 999004-TRATA-ERRO-CTNR
               WHEN ERRO-TAMANHO-AREA
                    PERFORM 999005-TRATA-ERRO-AREA-DADOS
               WHEN ERRO-CANAL
                    PERFORM 999006-TRATA-ERRO-CANAL
               WHEN OTHER
                    CONTINUE
           END-EVALUATE

           IF  MSG1-CD-ERRO-TS             EQUAL ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

           MOVE  'SGTSI064-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI064-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI064-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTSI064-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE  SGTSI064-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI064-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI064-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI064-CD-BCO-PCT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI064-CD-BCO-PCT   TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI064-CD-TIP-PRTC'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI064-CD-TIP-PRTC  TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI064-IN-BCO-LDR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI064-IN-BCO-LDR   TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI064-PC-PRTC-BCO'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTSI064-PC-PRTC-BCO    IS NUMERIC
               MOVE  SGTSI064-PC-PRTC-BCO
                                       TO  PC-PRTC-BCO
               MOVE  PC-PRTC-BCO-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'CD-USU-ATL'          TO  MSGKW999-NM-VRV-MNT
           MOVE  CD-USU-ATL            TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI064-TS-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI064-TS-ULT-ATL   TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *--> Grava o Erro no MSG
           PERFORM  MSG1-NTF-MSG
           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                           )
                      CHANNEL   ( NM-CNL                               )
                      FROM      ( IIBKERRO-PRM-ERRO                    )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO          )
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


      *---------------------- FIM SGTSI064 -----------------------------