      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSP071
      * ANALISTA..: F8214779 - PEDRO ARGOLO LACERDA
      * AUTOR.....: F8214779 - PEDRO ARGOLO LACERDA
      * OBJETIVO..: Consultar a Soma dos Valores Vinculados
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 27/01/2017 F8214779 DE 2016/145195
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSP071.

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
                                       '*** S.S. Comeca Aqui ***'.
       77  CTE-PROG                    PIC  X(17) VALUE
                                       '*** SGTSP071 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  SDD-VL-PGTO-PGDR        PIC S9(15)V99 COMP-3 VALUE 0.
       01  SDD-VL-PGTO-VCLD        PIC S9(15)V99 COMP-3 VALUE 0.

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *--> Indicadores de Nulidade
       01  SDD-VL-PGTO-PGDR-N               PIC S9(4) COMP-5 VALUE 0.
       01  SDD-VL-PGTO-VCLD-N               PIC S9(4) COMP-5 VALUE 0.

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book do container de requisicao e resposta deste modulo
       01  SGTSP071-DADOS.
-INC SGTKP071

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

           IF  NOT EXECUCAO-ONLINE  AND
               NOT EXECUCAO-BATCH
               CALL CICS5000        USING  CICS5000-COMMAREA
           END-IF

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997

      *--> Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM  100000-INICIAR

           PERFORM  200000-SOMAR-VL-PGTO

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

           PERFORM  130000-EXCLUIR-CTNR-RPST

           PERFORM  140000-RECEBER-REQUISICAO

           PERFORM  150000-VALIDAR-PARAMETRO
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'PRM-ERRO' )
                      CHANNEL   ( NM-CNL     )
                      RESP      ( MSG1-RESP1 )
                      RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CICS   TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'SGTSP071-RPST' )
                      CHANNEL   ( NM-CNL          )
                      RESP      ( MSG1-RESP1      )
                      RESP2     ( MSG1-RESP2      )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CICS   TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-CCT-LIDO
           MOVE  LENGTH OF  SGTSP071-RQSC
                                       TO  QT-CCT-EPRO
           EXEC CICS  GET
                      CONTAINER ( 'SGTSP071-RQSC' )
                      CHANNEL   ( NM-CNL          )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA TO TRUE
               PERFORM  999000-ERRO
           END-IF

           EXEC CICS  GET
                      CONTAINER ( 'SGTSP071-RQSC'         )
                      CHANNEL   ( NM-CNL                  )
                      FLENGTH   ( LENGTH OF SGTSP071-RQSC )
                      INTO      ( SGTSP071-RQSC           )
           END-EXEC
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Operação Pagadora
           IF  SGTSP071-CD-CTRA-OPR-PGDR   EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0496              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira da operação pagadora não inform
      -              'ado.'            TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTSP071-CD-CTRA-OPR-PGDR   NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0497              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira da operação pagadora inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTSP071-CD-OPR-PGDR        NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0498              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada pagadora não numéri
      -              'co.'             TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTSP071-CD-OPR-PGDR        EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0499              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada pagadora não inform
      -              'ado.'            TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTSP071-CD-OPR-PGDR        NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0500              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada pagadora inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTSP071-NR-VRS-OPR-PGDR    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0501              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação pagadora não informad
      -              'o.'              TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTSP071-NR-VRS-OPR-PGDR    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0502              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação pagadora inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

      *--> Valida a Operação Vinculada
           IF  SGTSP071-CD-CTRA-OPR-VCLD   EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0503              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira da operação vinculada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTSP071-CD-CTRA-OPR-VCLD   NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0504              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira da operação vinculada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTSP071-CD-OPR-ETRD-VCLD   NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0505              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada vinculada não numér
      -              'ico.'            TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTSP071-CD-OPR-ETRD-VCLD   EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0506              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada vinculada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTSP071-CD-OPR-ETRD-VCLD   NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0507              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada vinculada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTSP071-NR-VRS-OPR-VCLD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0508              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação vinculada não informa
      -              'do.'             TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTSP071-NR-VRS-OPR-VCLD    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0509              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação vinculada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           .
       150000-SAI.
           EXIT.

      *--------------------------------------------------------------
       200000-SOMAR-VL-PGTO            SECTION.
      *--------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  SDD-VL-PGTO-VCLD

           EXEC SQL
             SELECT SUM(VL_PGTO)
              INTO :SDD-VL-PGTO-VCLD :SDD-VL-PGTO-VCLD-N
               FROM DB2SGT.OPR_PG_OTR_OPR
               WHERE CD_CTRA_OPR_PGDR  = :SGTSP071-CD-CTRA-OPR-PGDR
                 AND CD_OPR_PGDR  = :SGTSP071-CD-OPR-PGDR
                 AND NR_VRS_OPR_PGDR = :SGTSP071-NR-VRS-OPR-PGDR
           END-EXEC

           MOVE     SDD-VL-PGTO-VCLD   TO  SGTSP071-SDD-VL-PGTO-VCLD

           MOVE  ZEROS                 TO  SDD-VL-PGTO-PGDR

           EXEC SQL
             SELECT SUM(VL_PGTO)
              INTO :SDD-VL-PGTO-PGDR :SDD-VL-PGTO-PGDR-N
               FROM DB2SGT.OPR_PG_OTR_OPR
               WHERE CD_CTRA_OPR_VCLD  = :SGTSP071-CD-CTRA-OPR-VCLD
                 AND CD_OPR_ETRD_VCLD = :SGTSP071-CD-OPR-ETRD-VCLD
                 AND NR_VRS_OPR_VCLD = :SGTSP071-NR-VRS-OPR-VCLD
           END-EXEC

           MOVE     SDD-VL-PGTO-PGDR   TO  SGTSP071-SDD-VL-PGTO-PGDR

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
                     CONTAINER ( 'SGTSP071-RPST'         )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSP071-RPST )
                     FROM      ( SGTSP071-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-ERRO                     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

           PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-ERRO-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE 'SGTSP071-CD-CTRA-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSP071-CD-CTRA-OPR-PGDR
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSP071-CD-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSP071-CD-OPR-PGDR    IS NUMERIC
               MOVE  SGTSP071-CD-OPR-PGDR
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSP071-NR-VRS-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSP071-NR-VRS-OPR-PGDR
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSP071-CD-CTRA-OPR-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSP071-CD-CTRA-OPR-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSP071-CD-OPR-ETRD-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSP071-CD-OPR-ETRD-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSP071-NR-VRS-OPR-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSP071-NR-VRS-OPR-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *--> Grava as Variaveis Monitoradas no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava a Ocorrencia de Erro no Container de Erro
           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                  )
                      CHANNEL   ( NM-CNL                      )
                      FROM      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      NOHANDLE
           END-EXEC

      *--> Encerra a Execução do Módulo
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

      *-----------------------------------------------------------------
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.

      *---------------------- FIM SGTSP071 -----------------------------