      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSC073
      * ANALISTA..: F2020988 - Christina Celeste Guimarães
      * AUTOR.....: F2020988 - Christina Celeste Guimarães
      * OBJETIVO..: CONSULTA CONTAINER DA TABELA NOTA_CLSC_PCT_OPR
      * COMPILAÇÃO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 02.08.2019 F2020988  DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSC073.

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
       77  CTE-INICIO-SS               PIC  X(040)  VALUE
                             '*** S.S. COMECA AQUI ***'.
       77  CTE-PROG                    PIC  X(17) VALUE
                             '*** SGTSC073 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *----------------------------------------------------------------*
      *                     CONTAINERS UTILIZADOS                      *
      *----------------------------------------------------------------*
      *--> Container de erro
       77 CTNR-PRM-ERRO               PIC X(16) VALUE 'PRM-ERRO'.
      * ---------------------------------------------------------------
      * Container da requisição desta subrotina
       77 CTNR-SGTSC073-RQSC           PIC X(16) VALUE 'SGTSC073-RQSC'.

      * Container da resposta desta subrotina
       77 CTNR-SGTSC073-RPST           PIC X(16) VALUE 'SGTSC073-RPST'.
      * ---------------------------------------------------------------*

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      * Registro da tabela
       01  REG-WS.
           03  DADOS.
               05  TS-ULT-ATL
                   PIC X(26) VALUE '0000-00-00-00.00.00.000000'.
               05  TS-ULT-ATL-R
                   REDEFINES TS-ULT-ATL.
                   07  AA              PIC X(4).
                   07  ANRC            PIC X(1).
                   07  MM              PIC X(2).
                   07  ANRC            PIC X(1).
                   07  DD              PIC X(2).
                   07  ANRC            PIC X(1).
                   07  HH              PIC X(2).
                   07  ANRC            PIC X(1).
                   07  MNTO            PIC X(2).
                   07  ANRC            PIC X(1).
                   07  SGDO            PIC X(2).
                   07  ANRC            PIC X(1).
                   07  MSGD            PIC X(6).

      * Book de para acionamento do MSG1
-INC IIBKERRO

-INC MSGKWMG1

-INC HLPKW999

-INC SGTKERR1
      * Book do programa
       01  SGTSC073-DADOS.
-INC SGTKC073
      * Indicadores de nulidades

       01  CD-ARQ-OPR-ETRD-N                PIC S9(4) COMP-5 VALUE 0.
       01  DT-FIM-ATBC-NOTA-N               PIC S9(4) COMP-5 VALUE 0.
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

      * Habilita tratamento automatico de Erros DB2
-INC HLPKE997
      * Habilita captura automatica de Abend e Erros CICS
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

           MOVE CTNR-SGTSC073-RPST     TO NM-CTNR

           SET DEL-CONTAINER           TO TRUE

           EXEC CICS DELETE
              CONTAINER ( NM-CTNR )
              CHANNEL   ( NM-CNL  )
              NOHANDLE
           END-EXEC
           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
               EIBRESP EQUAL DFHRESP(CONTAINERERR) )
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
           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
               EIBRESP EQUAL DFHRESP(CONTAINERERR) )
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
           MOVE CTNR-SGTSC073-RQSC     TO NM-CTNR
           SET  GET-CONTAINER          TO TRUE

           EXEC CICS GET
              CONTAINER ( NM-CTNR     )
              CHANNEL   ( NM-CNL      )
              NODATA
              FLENGTH   ( QT-CCT-LIDO )
           END-EXEC

           MOVE LENGTH OF SGTSC073-RQSC
                                       TO QT-CCT-EPRO

      *Devido à captura automática de erro CICS, deve ser previsto o
      *erro de container.
           SET  ERRO-CONTAINER         TO TRUE
           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA TO TRUE
               PERFORM 999000-ERRO
           END-IF

           EXEC CICS GET
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSC073-RQSC )
              INTO      ( SGTSC073-RQSC           )
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

           INITIALIZE                  REG-WS

           IF  SGTSC073-CD-CLI             EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 189                TO  MSG1-CD-ERRO-TS
               MOVE 'Código cliente não informado.' TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSC073-NR-SEQL-NOTA-PCT   EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 279                TO  MSG1-CD-ERRO-TS
               MOVE 'Número sequencial nota participante não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           .
       500000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       600000-CONSULTAR                     SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

      *    Campos nulos sao inicializados na consulta.
           INITIALIZE SGTSC073-CD-ARQ-OPR-ETRD
                      SGTSC073-DT-FIM-ATBC-NOTA

           EXEC SQL
                SELECT CD_USU_ULT_ATL
                     , TS_ULT_ATL
                     , CD_AG_CLSC_RSCO
                     , NR_ETRD_AG_CLSC
                     , NR_SEQL_NOTA_AG
                     , DT_INC_ATBC_NOTA
                     , DT_FIM_ATBC_NOTA
                     , CD_ARQ_OPR_ETRD
                     , IN_NOTA_PCT_CNCD
                  INTO :SGTSC073-CD-USU-ULT-ATL
                     , :SGTSC073-TS-ULT-ATL
                     , :SGTSC073-CD-AG-CLSC-RSCO
                     , :SGTSC073-NR-ETRD-AG-CLSC
                     , :SGTSC073-NR-SEQL-NOTA-AG
                     , :SGTSC073-DT-INC-ATBC-NOTA
                     , :SGTSC073-DT-FIM-ATBC-NOTA
                       :DT-FIM-ATBC-NOTA-N
                     , :SGTSC073-CD-ARQ-OPR-ETRD
                       :CD-ARQ-OPR-ETRD-N
                     , :SGTSC073-IN-CNCT-NOTA-CLSC
                  FROM DB2SGT.NOTA_CLSC_PCT_OPR
                 WHERE CD_CLI            =  :SGTSC073-CD-CLI
                   AND NR_SEQL_NOTA_PCT  =  :SGTSC073-NR-SEQL-NOTA-PCT
           END-EXEC

           IF  SQLCODE = +100
               INITIALIZE SGTSC073-RPST
               MOVE 100                TO MSG1-CD-ERRO-TS
               MOVE 'Operação paga por outra operação inexistente.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF
           .
       600000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       700000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE CTNR-SGTSC073-RPST     TO NM-CTNR
           SET  PUT-CONTAINER          TO TRUE

      *Devido à captura automática de erro CICS, deve ser previsto o
      *erro de container.
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSC073-RPST )
              FROM      ( SGTSC073-RPST           )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum.
           SET  SEM-ERRO               TO TRUE
           .
       700000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       999000-ERRO                     SECTION.
      *----------------------------------------------------------------*

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF
           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                        TO MSG1-TX-ERRO-TS
           END-IF
           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-ERRO-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

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

           MOVE 'SGTSC073-CD-CLI                       '
                                       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC073-CD-CLI
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT


           MOVE 'SGTSC073-NR-SEQL-NOTA-PCT              '
                                       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC073-NR-SEQL-NOTA-PCT
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO
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
      *----------------------------------------
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