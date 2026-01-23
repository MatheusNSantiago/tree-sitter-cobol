      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSC069
      * ANALISTA..: F2054109 - Cintia Ap Soares Iglesias
      * AUTOR.....: F6361711 - Luis A P Valamede
      * OBJETIVO..: Consultar estado da versão da operação estruturada.
      *             na tabela: DB2SGT.EST_VRS_OPR_ETRD
      * COMPILAÇÃO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 06.04.2017 F6361711  DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSC069.

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
       77  CTE-INICIO-SS               PIC  X(040)  VALUE
                                       '*** S.S. COMEÇA AQUI ***'.
       77  CTE-PROG                    PIC  X(17) VALUE
                                        '*** SGTSC069 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *-----------------------------------------------------------------
      *                     CONTAINERS UTILIZADOS
      *-----------------------------------------------------------------
      *--> Container de erro
       77 CTNR-PRM-ERRO                PIC X(16) VALUE 'PRM-ERRO'.
      *--> Container da requisição desta subrotina
       77 CTNR-SGTSC069-RQSC           PIC X(16) VALUE 'SGTSC069-RQSC'.

      *--> Container da resposta desta subrotina
       77 CTNR-SGTSC069-RPST           PIC X(16) VALUE 'SGTSC069-RPST'.
      * ----------------------------------------------------------------

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Indicadores de nulidades
       01  TX-OBS-EST-VRS-N            PIC S9(4) COMP-5 VALUE 0.

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      ---------------9.

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
       01  SGTSC069-DADOS.
-INC SGTKC069

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC X(40) VALUE
               '*** STORAGE SECTION TERMINA AQUI ***'.
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

           PERFORM  100000-DESCOBRIR-CANAL
           PERFORM  200000-EXCLUIR-CTNR-RPST
           PERFORM  300000-EXCLUIR-CTNR-ERRO
           PERFORM  400000-RECEBER-REQUISICAO
           PERFORM  500000-VALIDA-CAMPOS
           PERFORM  600000-CONSULTAR
           PERFORM  700000-POSTA-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

      *--> Devido à captura automática de erro CICS, deve ser
      *    previsto tipo de erro.
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum.
           SET   SEM-ERRO              TO  TRUE
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisição.
           MOVE  CTE-PROG(5:8)         TO  NM-PGM

           MOVE  CTNR-SGTSC069-RPST    TO  NM-CTNR

           SET   DEL-CONTAINER         TO  TRUE

           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR )
                     CHANNEL   ( NM-CNL  )
                     NOHANDLE
           END-EXEC

           IF  NOT (EIBRESP                EQUAL  DFHRESP(NORMAL)  OR
                    EIBRESP                EQUAL  DFHRESP(CONTAINERERR))
               PERFORM  999002-TRATA-CONDITION-CICS
           END-IF
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           MOVE  CTNR-PRM-ERRO         TO  NM-CTNR

           SET   DEL-CONTAINER         TO  TRUE

           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR    )
                     CHANNEL   ( NM-CNL     )
                     NOHANDLE
           END-EXEC
           IF  NOT (EIBRESP                EQUAL  DFHRESP(NORMAL)  OR
                    EIBRESP                EQUAL  DFHRESP(CONTAINERERR))
               PERFORM  999002-TRATA-CONDITION-CICS
           END-IF
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       400000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-CCT-LIDO
           MOVE  LENGTH OF SGTSC069-RQSC
                                       TO  QT-CCT-EPRO

           MOVE  CTNR-SGTSC069-RQSC    TO  NM-CTNR

           EXEC CICS GET
                     CONTAINER ( NM-CTNR     )
                     CHANNEL   ( NM-CNL      )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA  TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
                     CONTAINER ( NM-CTNR                 )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSC069-RQSC )
                     INTO      ( SGTSC069-RQSC           )
           END-EXEC
           .
       400000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       500000-VALIDA-CAMPOS            SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSC069-CD-CTRA-TIT-VL     EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.' TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSC069-CD-CTRA-TIT-VL     NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSC069-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSC069-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'    TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSC069-CD-OPR-ETRD-MCP    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais in
      -              'válido.'         TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSC069-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSC069-NR-VRS-OPR-ETRD    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido
      -              '.'               TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       500000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       600000-CONSULTAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

      *--> Campos nulos sao inicializados na consulta.
           INITIALIZE                      SGTSC069-CD-USU-ULT-ATL
                                           SGTSC069-CD-TIP-EST-VRS
                                           SGTSC069-TX-OBS-EST-VRS
                                           TX-OBS-EST-VRS-N

           IF  SGTSC069-TS-EST-VRS         EQUAL  ZEROS  OR
               SGTSC069-TS-EST-VRS         EQUAL  SPACES
      *--->    Executa a consulta sem informar Timestamp.
      *        Recupera, da chave informada, o registro com maior
      *        Timestamp.
               EXEC SQL
                    SELECT DISTINCT
                           A.CD_USU_ULT_ATL
                         , A.CD_TIP_EST_VRS
                         , A.TX_OBS_EST_VRS
                         , A.TS_EST_VRS
                      INTO :SGTSC069-CD-USU-ULT-ATL
                         , :SGTSC069-CD-TIP-EST-VRS
                         , :SGTSC069-TX-OBS-EST-VRS
                           :TX-OBS-EST-VRS-N
                         , :SGTSC069-TS-EST-VRS-S
                     FROM  DB2SGT.EST_VRS_OPR_ETRD  A
                    WHERE  A.CD_CTRA_TIT_VL  = :SGTSC069-CD-CTRA-TIT-VL
                      AND  A.CD_OPR_ETRD_MCP = :SGTSC069-CD-OPR-ETRD-MCP
                      AND  A.NR_VRS_OPR_ETRD = :SGTSC069-NR-VRS-OPR-ETRD
                      AND  A.TS_EST_VRS      =
                           (SELECT MAX(TS_EST_VRS)
                     FROM  DB2SGT.EST_VRS_OPR_ETRD  B
                    WHERE  B.CD_CTRA_TIT_VL  = :SGTSC069-CD-CTRA-TIT-VL
                      AND  B.CD_OPR_ETRD_MCP = :SGTSC069-CD-OPR-ETRD-MCP
                      AND  B.NR_VRS_OPR_ETRD =
                                              :SGTSC069-NR-VRS-OPR-ETRD)
                           FETCH FIRST 1 ROW ONLY
               END-EXEC
           ELSE
      *--->    Executa a consulta informando o Timestamp.
               EXEC SQL
                    SELECT CD_USU_ULT_ATL
                         , CD_TIP_EST_VRS
                         , TX_OBS_EST_VRS
                      INTO :SGTSC069-CD-USU-ULT-ATL
                         , :SGTSC069-CD-TIP-EST-VRS
                         , :SGTSC069-TX-OBS-EST-VRS
                           :TX-OBS-EST-VRS-N
                      FROM DB2SGT.EST_VRS_OPR_ETRD
                     WHERE CD_CTRA_TIT_VL  = :SGTSC069-CD-CTRA-TIT-VL
                       AND CD_OPR_ETRD_MCP = :SGTSC069-CD-OPR-ETRD-MCP
                       AND NR_VRS_OPR_ETRD = :SGTSC069-NR-VRS-OPR-ETRD
                       AND TS_EST_VRS      = :SGTSC069-TS-EST-VRS
               END-EXEC
           END-IF

           IF  SQLCODE                     EQUAL  +100
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0100              TO  MSG1-CD-ERRO-TS
               MOVE  'Registro inexistente.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       600000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       700000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           MOVE  CTNR-SGTSC069-RPST    TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Devido à captura automática de erro CICS, deve ser
      *    previsto erro de container.
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                 )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSC069-RPST )
                     FROM      ( SGTSC069-RPST           )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum.
           SET   SEM-ERRO              TO  TRUE
           .
       700000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------
      *--> Monitora variàveis do pgm, grava e encerra processamento

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Verifica se o tipo de erro gerado possui tratamento interno
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

      *--> Adiciona as variáveis para monitoração
           MOVE  'SGTSC069-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTSC069-CD-OPR-ETRD-MCP    NUMERIC
               MOVE  SGTSC069-CD-OPR-ETRD-MCP
                                       TO  MCR-S9-15
               MOVE  MCR-S9-15         TO  MCR-S9-15
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC069-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC069-CD-CTRA-TIT-VL
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC069-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC069-CD-OPR-ETRD-MCP
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC069-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC069-NR-VRS-OPR-ETRD
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava o erro no Sistema MSG
           PERFORM  MSG1-NTF-MSG
           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

           EXEC CICS PUT
                     CONTAINER ( CTNR-PRM-ERRO               )
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
      *---------------------- FIM SGTSC069 -----------------------------