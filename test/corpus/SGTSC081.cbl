      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSC081
      * ANALISTA..: F9188847 - SOILA KARLA LINO AKAMATSU TARDELLI
      * AUTOR.....: F9188847 - SOILA KARLA LINO AKAMATSU TARDELLI
      * OBJETIVO..: CONSULTA DA TABELA PRV_CMPS_GRT
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 26.07.2018 - F2804421 - 2016/145195 - Acerto mensageria,
      *                   carregar nome container na variável NM-CTNR.
      *        19.01.2017 - F9188847 - 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSC081.

      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTSC081 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
       COPY HLPKE01B.
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * Book de para acionamento do MSG1
       COPY IIBKERRO.
       COPY MSGKWMG1.
       COPY HLPKW999.

      * Area de trabalho

       01  QT-REG-SELD                      PIC S9(4) COMP-5 VALUE 0.
       01  DT-SQL-ATU                       PIC  X(10)
                                                 VALUE '  .  .    '.
       01  DT-SQL-ATU-R REDEFINES DT-SQL-ATU.
           03 DIA                           PIC  9(02).
           03 FILLER                        PIC  X(01).
           03 MES                           PIC  9(02).
           03 FILLER                        PIC  X(01).
           03 ANO                           PIC  9(04).
       01  TS-SQL-ATU                       PIC  X(26) VALUE ' '.

      * Indicadores de nulidades

       01  QT-REG-SELD-NULO                 PIC S9(4) COMP-5 VALUE 0.
       01  PC-GRT-CMPS-PRVT-N               PIC S9(4) COMP-5 VALUE 0.
       01  VL-GRT-CMPS-PRVT-N               PIC S9(4) COMP-5 VALUE 0.

      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG            PIC  9(10) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG           PIC  9(11) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG           PIC  9(5) VALUE 0.
           03  NR-PRV-CMPS-GRT-LOG           PIC  9(10) VALUE 0.

      * Book dos dados do log
       01  DADOS-LOG.
       COPY SGTK014G.

       01  SGTS014G VALUE 'SGTS014G'         PIC X(8).

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).


       COPY SGTKC081.
       COPY SGTKERR1.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO
      * Habilita tratamento automatico de Erros DB2
       COPY HLPKE997.
      * Habilita captura automatica de Abend e Erros CICS
       COPY HLPKE999.

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 110000-EXCLUIR-CTNR-RPST
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 150000-VALIDA-CAMPOS

           PERFORM 200000-CONSULTAR
           PERFORM 300000-POSTA-RESPOSTA

           GOBACK
           .
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSC081-RPST' )
              CHANNEL   ( NM-CNL      )
              NOHANDLE
           END-EXEC
           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
               EIBRESP EQUAL DFHRESP(CONTAINERERR) )
               PERFORM 999002-TRATA-CONDITION-CICS
           END-IF
           .
       110000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   ( NM-CNL      )
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
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSC081-RQSC      TO QT-CCT-EPRO
           MOVE 'SGTSC081-RQSC'        TO NM-CTNR
           SET  ERRO-CONTAINER         TO TRUE
           SET  GET-CONTAINER          TO TRUE

           EXEC CICS GET
              CONTAINER ( NM-CTNR     )
              CHANNEL   ( NM-CNL      )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( NM-CTNR     )
              CHANNEL   ( NM-CNL      )
              FLENGTH ( LENGTH OF SGTSC081-RQSC )
              INTO    ( SGTSC081-RQSC )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALIDA-CAMPOS SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO
           IF  SGTSC081-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0002                   TO  MSG1-CD-ERRO-TS
               MOVE 'Codigo carteira titulo valor inválido.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSC081-CD-CTRA-TIT-VL     NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE 0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores monetários
      -              'inválido.'       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSC081-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE 0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSC081-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE 0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'    TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSC081-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE 0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSC081-NR-PRV-CMPS-GRT EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0117               TO  MSG1-CD-ERRO-TS
               MOVE 'Número da previsão de constituição de garantia invá
      -         'lido.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       135000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-CONSULTAR SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO
      *    Campos nulos sao inicializados na consulta
           INITIALIZE SGTSC081-PC-GRT-CMPS-PRVT
                      SGTSC081-VL-GRT-CMPS-PRVT
           EXEC SQL
                 SELECT CD_USU_ULT_ATL
                      , TS_ULT_ATL
                      , CD_NTZ_GRT
                      , CD_FMA_VCLC
                      , DT_LIM_GRT
                      , PC_GRT_CMPS_PRVT
                      , VL_GRT_CMPS_PRVT
                   INTO  :SGTSC081-CD-USU-ULT-ATL
                       , :SGTSC081-TS-ULT-ATL
                       , :SGTSC081-CD-NTZ-GRT
                       , :SGTSC081-CD-FMA-VCLC
                       , :SGTSC081-DT-LIM-GRT
                       , :SGTSC081-PC-GRT-CMPS-PRVT
                         :PC-GRT-CMPS-PRVT-N
                       , :SGTSC081-VL-GRT-CMPS-PRVT
                         :VL-GRT-CMPS-PRVT-N
                       FROM DB2SGT.PRV_CMPS_GRT
                WHERE CD_CTRA_TIT_VL     = :SGTSC081-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP    = :SGTSC081-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD    = :SGTSC081-NR-VRS-OPR-ETRD
                  AND NR_PRV_CMPS_GRT    = :SGTSC081-NR-PRV-CMPS-GRT
           END-EXEC
           IF  SQLCODE = +100
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 100               TO  MSG1-CD-ERRO-TS
               MOVE 'Registro Inexistente.'
                                             TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE 'SGTSC081-RPST'        TO NM-CTNR
           SET  ERRO-CONTAINER         TO TRUE
           SET  PUT-CONTAINER          TO TRUE

           EXEC CICS PUT
              CONTAINER ( NM-CTNR     )
              CHANNEL   ( NM-CNL      )
              FLENGTH ( LENGTH OF SGTSC081-RPST )
              FROM    ( SGTSC081-RPST )
           END-EXEC

           SET SEM-ERRO                TO TRUE
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                     TO MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> VERIFICA SE O TIPO DE ERRO GERADO POSSUI TRATAMENTO INTERNO
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

           MOVE 'SGTSC081-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC081-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC081-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
           IF   SGTSC081-CD-OPR-ETRD-MCP     IS NUMERIC
               MOVE SGTSC081-CD-OPR-ETRD-MCP TO CD-OPR-ETRD-MCP
               MOVE CD-OPR-ETRD-MCP          TO MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Valor não numérico'
                                             TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC081-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC081-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC081-NR-PRV-CMPS-GRT'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC081-NR-PRV-CMPS-GRT     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO
           EXEC CICS PUT
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   ( NM-CNL      )
              FROM      ( IIBKERRO-PRM-ERRO)
              FLENGTH ( LENGTH OF IIBKERRO-PRM-ERRO )
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
      * --------------------------------------*
       COPY HLPKP999.
      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
       COPY HLPKP997.
      * --------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
       COPY MSGKPMG1.
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
       COPY SGTKP999.
      * --------------------------------------*
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC.
