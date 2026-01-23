      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSPE05
      * ANALISTA..: F2804421 - Eliana Cristina Ramos da Silva
      * AUTOR.....: F2804421 - Eliana Cristina Ramos da Silva
      * OBJETIVO..: Consultar o estado e o tipo da última versão da
      *             operação. tabelas:OPR_ETRD_MCP E EST_VRS_OPR_ETRD.
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 10.05.2017 - F2804421 DE 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSPE05.

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
                                       VALUE '*** SGTSPE05 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book do container de requisicao e resposta deste modulo
       01  SGTSPE05-DADOS.
-INC SGTKPE05

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC  X(40) VALUE
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

           PERFORM  200000-CONSULTAR

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

           PERFORM  140000-VALIDAR-PARAMETRO

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
                     CONTAINER ( 'PRM-ERRO' )
                     CHANNEL   ( NM-CNL     )
                     NOHANDLE
           END-EXEC

           IF  NOT (EIBRESP                EQUAL  DFHRESP(NORMAL)  OR
                    EIBRESP                EQUAL  DFHRESP(CONTAINERERR))
               PERFORM  999002-TRATA-CONDITION-CICS
           END-IF
           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-CCT-LIDO
           MOVE  LENGTH OF SGTSPE05-RQSC
                                       TO  QT-CCT-EPRO

           EXEC CICS GET
                     CONTAINER ( 'SGTSPE05-RQSC' )
                     CHANNEL   ( NM-CNL          )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  9001              TO  MSG1-CD-ERRO-TS
               MOVE  QT-CCT-EPRO       TO  VL-EXBR-1
               MOVE  QT-CCT-LIDO       TO  VL-EXBR-2
               STRING  'Erro no tamanho de área de requisição do '
                       NM-PGM '.'
                       ', Tamanho Esperado: '  VL-EXBR-1
                       ', Tamanho Recebido: '  VL-EXBR-2
                       DELIMITED BY SIZE
                       INTO  MSG1-TX-ERRO-TS
               END-STRING
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
                     CONTAINER ( 'SGTSPE05-RQSC'         )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSPE05-RQSC )
                     INTO      ( SGTSPE05-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

      *--> Valida Chave da Operação Estruturada
           IF  SGTSPE05-CD-CTRA-TIT-VL     EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'  TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSPE05-CD-CTRA-TIT-VL     NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSPE05-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSPE05-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'    TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSPE05-CD-OPR-ETRD-MCP    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais in
      -              'válido.'         TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-CONSULTAR                SECTION.
      *--> Verifica estado e tipo da última versão referente a
      *    carteira e operação informadas.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           EXEC SQL
                SELECT A.NR_VRS_OPR_ETRD
                     , A.CD_TIP_VRS_OPR
                     , B.CD_TIP_EST_VRS
                     , B.TS_EST_VRS
                  INTO :SGTSPE05-NR-VRS-OPR-ETRD
                     , :SGTSPE05-CD-TIP-VRS-OPR
                     , :SGTSPE05-CD-TIP-EST-VRS
                     , :SGTSPE05-TS-EST-VRS
                  FROM      DB2SGT.OPR_ETRD_MCP A
                INNER JOIN  DB2SGT.EST_VRS_OPR_ETRD B
                       ON
                           A.CD_CTRA_TIT_VL  = B.CD_CTRA_TIT_VL
                       AND A.CD_OPR_ETRD_MCP = B.CD_OPR_ETRD_MCP
                       AND A.NR_VRS_OPR_ETRD = B.NR_VRS_OPR_ETRD
                WHERE
                       A.CD_CTRA_TIT_VL  = :SGTSPE05-CD-CTRA-TIT-VL
                   AND A.CD_OPR_ETRD_MCP = :SGTSPE05-CD-OPR-ETRD-MCP
                ORDER BY B.TS_EST_VRS DESC
                FETCH FIRST 1 ROWS ONLY
           END-EXEC
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
                     CONTAINER ( 'SGTSPE05-RPST'         )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSPE05-RPST )
                     FROM      ( SGTSPE05-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Adiciona Chave da Operação Estruturada
           MOVE 'SGTSPE05-CD-CTRA-TIT-VL'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSPE05-CD-CTRA-TIT-VL    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTSPE05-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSPE05-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE  SGTSPE05-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
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

      *---------------------- FIM SGTSI081 -----------------------------