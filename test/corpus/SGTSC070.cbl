      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSC070
      * ANALISTA..: F2054109 - Cintia Ap Soares Iglesias
      * AUTOR.....: F6361711 - Luis A P Valamede
      * OBJETIVO..: CONSULTA CONTAINER DA TABELA RCBT_PRVT_OPR_ETRD
      * COMPILAÇÃO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 22/01/2019 F9188847 DE 2016/145195 - Inclusão monitoração
      *                   variáveis para rotina SBDATAF E acerto mensage
      *                   ria tramento de data.
      *        27.04.2017 F6361711  DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSC070.

      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-SS               PIC  X(40)  VALUE
                             '*** S.S. COMECA AQUI ***'.
       77  CTE-PROG                    PIC  X(17) VALUE
                             '*** SGTSC070 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      * Book do programa SBDATA para validação de datas
       77  SBDATAF                     PIC  X(08) VALUE 'SBDATAF'.

      *----------------------------------------------------------------*
      *                     CONTAINERS UTILIZADOS                      *
      *----------------------------------------------------------------*
      *--> Container de erro
       77 CTNR-PRM-ERRO               PIC X(16) VALUE 'PRM-ERRO'.
      * ---------------------------------------------------------------
      * Container da requisição desta subrotina
       77 CTNR-SGTSC070-RQSC           PIC X(16) VALUE 'SGTSC070-RQSC'.

      * Container da resposta desta subrotina
       77 CTNR-SGTSC070-RPST           PIC X(16) VALUE 'SGTSC070-RPST'.
      * ---------------------------------------------------------------*

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informações
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      *    Campos de trabalho
      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *    Campos utilizados validação de datas, valida data Gregoriana.
       01  FUC                         PIC  X(003) VALUE 'F01'.
       01  ARG01                       PIC  9(008) VALUE 000.
       01  ARG02                       PIC  9(001) VALUE 004.

       01  WORKAREA                    PIC X(4095).
       01  AREA-RED REDEFINES WORKAREA.
           05  AREA-TAM                PIC 9(0008) COMP.
           05  FILLER                  PIC X(4071).
           05  AREA-FIM                PIC X(0020).

      * Registro da tabela
       01  DT-RCBT-PRVT
                                       PIC X(10) VALUE '00.00.0000'.
       01  DT-RCBT-PRVT-R              REDEFINES DT-RCBT-PRVT.
           03  AA                      PIC X(4).
           03  FILLER                  PIC X(1).
           03  MM                      PIC X(2).
           03  FILLER                  PIC X(1).
           03  DD                      PIC X(2).

      * Indicadores de nulidades
       01  PC-RCBT-PRVT-N              PIC S9(4) COMP-5 VALUE 0.
       01  VL-RCBT-PRVT-N              PIC S9(4) COMP-5 VALUE 0.
       01  TX-OBS-RCBT-PRVT-N          PIC S9(4) COMP-5 VALUE 0.

      * Book de para acionamento do MSG1
-INC IIBKERRO
-INC MSGKWMG1
-INC HLPKW999
-INC SGTKERR1

      * Book do programa
       01  SGTSC070-DADOS.
-INC SGTKC070

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC X(40) VALUE
               '*** STORAGE SECTION TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      * Habilita tratamento automático de Erros DB2
-INC HLPKE997

      * Habilita captura autoámtica de Abend e Erros CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 200000-EXCLUIR-CTNR-RPST
           PERFORM 300000-EXCLUIR-CTNR-ERRO
           PERFORM 400000-RECEBER-REQUISICAO
           PERFORM 500000-VALIDA-CAMPOS
           PERFORM 600000-CONSULTAR
           PERFORM 700000-POSTA-RESPOSTA
           GOBACK
           .
       000000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      * Devido à captura automática de erro CICS, deve ser previsto o
      * tipo de erro.
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum.
           SET  SEM-ERRO               TO TRUE
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisição.
           MOVE CTE-PROG(5:8)          TO NM-PGM

           MOVE CTNR-SGTSC070-RPST     TO NM-CTNR

           SET DEL-CONTAINER           TO TRUE

           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR )
                     CHANNEL   ( NM-CNL  )
                     NOHANDLE
           END-EXEC

           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)
               OR   EIBRESP EQUAL DFHRESP(CONTAINERERR) )

               PERFORM 999002-TRATA-CONDITION-CICS
           END-IF
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE CTNR-PRM-ERRO          TO NM-CTNR

           SET DEL-CONTAINER           TO TRUE

           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR    )
                     CHANNEL   ( NM-CNL     )
                     NOHANDLE
           END-EXEC

           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)
               OR   EIBRESP EQUAL DFHRESP(CONTAINERERR) )

               PERFORM 999002-TRATA-CONDITION-CICS
           END-IF
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       400000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO QT-CCT-LIDO
           MOVE CTNR-SGTSC070-RQSC     TO NM-CTNR
           SET  GET-CONTAINER          TO TRUE

           EXEC CICS GET
              CONTAINER ( NM-CTNR     )
              CHANNEL   ( NM-CNL      )
              NODATA
              FLENGTH   ( QT-CCT-LIDO )
           END-EXEC

           MOVE LENGTH OF SGTSC070-RQSC
                                       TO QT-CCT-EPRO

           MOVE 0005                   TO MSG1-CD-SEQL-ERRO
      *Devido à captura automática de erro CICS, deve ser previsto o
      *erro de container.
           SET ERRO-CONTAINER          TO TRUE

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA  TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSC070-RQSC )
              INTO      ( SGTSC070-RQSC           )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum.
           SET  SEM-ERRO               TO TRUE
           .
       400000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       500000-VALIDA-CAMPOS            SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           IF  SGTSC070-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0002               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de títulos de valores mobiliário
      -             's não informado.'  TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSC070-CD-OPR-ETRD-MCP
                                       IS NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada de mercado de capita
      -        'is não numérico.'      TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSC070-CD-OPR-ETRD-MCP
                                       EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0005               TO MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada de mercado de capita
      -        'is não informado.'     TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSC070-NR-VRS-OPR-ETRD
                                       EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0007               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada não inform
      -             'ado.'
                 TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validar Tipo de Recebimento Previsto
      *    01 - Normal de Juros
      *    02 - Normal de Capital
      *    03 - Extraordinario de Juros
      *    04 - Extraordinario de Capital
           IF  SGTSC070-CD-TIP-RCBT-PRVT LESS 0001 OR
               SGTSC070-CD-TIP-RCBT-PRVT GREATER 0004
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0109               TO MSG1-CD-ERRO-TS
               MOVE 'Tipo de recebimento previsto inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSC070-DT-RCBT-PRVT   EQUAL SPACES
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0119               TO MSG1-CD-ERRO-TS
               MOVE 'Data do recebimento previsto não informada.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE SGTSC070-DT-RCBT-PRVT  TO ARG01

           PERFORM  970100-CHAMA-SBDATAF
           IF  RETURN-CODE             NOT EQUAL  ZEROS AND  0024
               SET  ERRO-PROGRAMA-ACIONADO    TO TRUE
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE 0051               TO MSG1-CD-ERRO-TS
               MOVE 'Erro no tratamento de datas.'
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS

               MOVE 'FUC'              TO  MSGKW999-NM-VRV-MNT
005600         MOVE  FUC               TO  MSGKW999-VL-VRV-MNT
005600         PERFORM 999003-ADICIONA-VRV-MNT
005600
               MOVE 'AREA-TAM'         TO MSGKW999-NM-VRV-MNT
               MOVE  AREA-TAM          TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'AREA-FIM'         TO MSGKW999-NM-VRV-MNT
               MOVE  AREA-FIM          TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT
005600
005600         MOVE 'ARG01'           TO  MSGKW999-NM-VRV-MNT
005600         MOVE  ARG01            TO  MSGKW999-VL-VRV-MNT
005600         PERFORM 999003-ADICIONA-VRV-MNT
005600
               MOVE 'ARG02'           TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG02            TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF RETURN-CODE            EQUAL 0024
               SET  ERRO-PROGRAMA-ACIONADO    TO TRUE
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE 0120               TO MSG1-CD-ERRO-TS
               MOVE 'Data do recebimento previsto inválida.'
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS

               MOVE 'FUC'              TO  MSGKW999-NM-VRV-MNT
005600         MOVE  FUC               TO  MSGKW999-VL-VRV-MNT
005600         PERFORM 999003-ADICIONA-VRV-MNT
005600
               MOVE 'AREA-TAM'         TO MSGKW999-NM-VRV-MNT
               MOVE  AREA-TAM          TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'AREA-FIM'         TO MSGKW999-NM-VRV-MNT
               MOVE  AREA-FIM          TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT
005600
005600         MOVE 'ARG01'           TO  MSGKW999-NM-VRV-MNT
005600         MOVE  ARG01            TO  MSGKW999-VL-VRV-MNT
005600         PERFORM 999003-ADICIONA-VRV-MNT
005600
               MOVE 'ARG02'           TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG02            TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE '0000-00-00'           TO DT-RCBT-PRVT-R
           MOVE CORR SGTSC070-DT-RCBT-PRVT-R
                                       TO DT-RCBT-PRVT-R
           .
       500000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       600000-CONSULTAR                     SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

      *    Campos nulos são inicializados na consulta.
           INITIALIZE SGTSC070-CD-USU            SGTSC070-TS-ULT-ATL
                      SGTSC070-PC-RCBT-PRVT      PC-RCBT-PRVT-N
                      SGTSC070-VL-RCBT-PRVT      VL-RCBT-PRVT-N
                      SGTSC070-TX-OBS-RCBT-PRVT  TX-OBS-RCBT-PRVT-N

      *--->    Executa a consulta sem informar o número Sequencial do
      *        Recebimento Previsto.
      *        Recupera, a partir da chave informada, o registro
      *        com maior Número Sequencial do Recebimento Previsto.
           IF  SGTSC070-NR-SEQL-RCBT-PRVT          EQUAL ZEROS
               EXEC SQL
                    SELECT DISTINCT
                           A.CD_USU
                         , A.TS_ULT_ATL
                         , A.PC_RCBT_PRVT
                         , A.VL_RCBT_PRVT
                         , A.TX_OBS_RCBT_PRVT
                      INTO :SGTSC070-CD-USU
                         , :SGTSC070-TS-ULT-ATL
                         , :SGTSC070-PC-RCBT-PRVT
                           :PC-RCBT-PRVT-N
                         , :SGTSC070-VL-RCBT-PRVT
                           :VL-RCBT-PRVT-N
                         , :SGTSC070-TX-OBS-RCBT-PRVT
                           :TX-OBS-RCBT-PRVT-N
                      FROM DB2SGT.RCBT_PRVT_OPR_ETRD  A
                     WHERE A.CD_CTRA_TIT_VL  = :SGTSC070-CD-CTRA-TIT-VL
                       AND A.CD_OPR_ETRD_MCP = :SGTSC070-CD-OPR-ETRD-MCP
                       AND A.NR_VRS_OPR_ETRD = :SGTSC070-NR-VRS-OPR-ETRD
                       AND A.CD_TIP_RCBT_PRVT =
                                              :SGTSC070-CD-TIP-RCBT-PRVT
                       AND A.DT_RCBT_PRVT    = :DT-RCBT-PRVT
                       AND A.NR_SEQL_RCBT_PRVT =
                           (SELECT MAX(NR_SEQL_RCBT_PRVT)
                      FROM DB2SGT.RCBT_PRVT_OPR_ETRD  B
                     WHERE B.CD_CTRA_TIT_VL  = :SGTSC070-CD-CTRA-TIT-VL
                       AND B.CD_OPR_ETRD_MCP = :SGTSC070-CD-OPR-ETRD-MCP
                       AND B.NR_VRS_OPR_ETRD = :SGTSC070-NR-VRS-OPR-ETRD
                       AND B.CD_TIP_RCBT_PRVT =
                                              :SGTSC070-CD-TIP-RCBT-PRVT
                       AND B.DT_RCBT_PRVT    = :DT-RCBT-PRVT)
                       FETCH FIRST 1 ROW ONLY
               END-EXEC
           ELSE
      *--->    Executa a consulta informando o número Sequencial do
      *        Recebimento Previsto.
               EXEC SQL
                    SELECT CD_USU
                         , TS_ULT_ATL
                         , PC_RCBT_PRVT
                         , VL_RCBT_PRVT
                         , TX_OBS_RCBT_PRVT
                      INTO :SGTSC070-CD-USU
                         , :SGTSC070-TS-ULT-ATL
                         , :SGTSC070-PC-RCBT-PRVT
                           :PC-RCBT-PRVT-N
                         , :SGTSC070-VL-RCBT-PRVT
                           :VL-RCBT-PRVT-N
                         , :SGTSC070-TX-OBS-RCBT-PRVT
                          :TX-OBS-RCBT-PRVT-N
                      FROM DB2SGT.RCBT_PRVT_OPR_ETRD
                     WHERE CD_CTRA_TIT_VL    = :SGTSC070-CD-CTRA-TIT-VL
                       AND CD_OPR_ETRD_MCP   = :SGTSC070-CD-OPR-ETRD-MCP
                       AND NR_VRS_OPR_ETRD   = :SGTSC070-NR-VRS-OPR-ETRD
                       AND NR_SEQL_RCBT_PRVT =
                                             :SGTSC070-NR-SEQL-RCBT-PRVT
                       AND CD_TIP_RCBT_PRVT  =
                                             :SGTSC070-CD-TIP-RCBT-PRVT
                       AND DT_RCBT_PRVT      = :DT-RCBT-PRVT
               END-EXEC
           END-IF

           IF  SQLCODE = +100
               INITIALIZE SGTSC070-RPST
               SET  ERRO-PROGRAMADO           TO TRUE
               MOVE 0100                      TO MSG1-CD-ERRO-TS
               MOVE 'Registro inexistente.'   TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       600000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       700000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE CTNR-SGTSC070-RPST     TO NM-CTNR
           SET  PUT-CONTAINER          TO TRUE

      *Devido à captura automática de erro CICS, deve ser previsto o
      *erro de container.
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                 )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSC070-RPST )
                     FROM      ( SGTSC070-RPST           )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum.
           SET  SEM-ERRO               TO TRUE
           .
       700000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970100-CHAMA-SBDATAF            SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           MOVE 4095                   TO AREA-TAM
           MOVE 'SBDATAF -WRKAREA-FIM' TO AREA-FIM

           CALL  SBDATAF              USING   FUC
                                              WORKAREA
                                              ARG01
                                              ARG02
           .
       970100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF
           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                       TO MSG1-TX-ERRO-TS
           END-IF


      *Verifica se o tipo de erro gerado possui tratamento interno.
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

           MOVE 'SGTSC070-CD-CTRA-TIT-VL'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC070-CD-CTRA-TIT-VL          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT


           MOVE 'SGTSC070-CD-OPR-ETRD-MCP'       TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSC070-CD-OPR-ETRD-MCP     IS NUMERIC
               MOVE  SGTSC070-CD-OPR-ETRD-MCP    TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX          TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'        TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC070-NR-VRS-OPR-ETRD'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC070-NR-VRS-OPR-ETRD         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC070-NR-SEQL-RCBT-PRVT'     TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC070-NR-SEQL-RCBT-PRVT       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC070-CD-TIP-RCBT-PRVT'      TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC070-CD-TIP-RCBT-PRVT        TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC070-DT-RCBT-PRVT'          TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC070-DT-RCBT-PRVT            TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
           EXEC CICS PUT
              CONTAINER ( CTNR-PRM-ERRO               )
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
      *---------------------------------------*
-INC SGTKP999
      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
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
-INC MSGKPMG1
      * --------------------------------------*
       999999-ENCERRAR                 SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *------------------------- Fim SGTSC070 -------------------------*