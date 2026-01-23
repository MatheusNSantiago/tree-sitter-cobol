      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSC055
      * ANALISTA..: F2054109 - CINTIA AP SOARES IGLESIAS
      * AUTOR.....: F2054109 - CINTIA AP SOARES IGLESIAS
      * OBJETIVO..: CONSULTA DA TABELA NOTA_TCN_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 09/02/2018 F8785286 DE 2016/145195 - Acerto dos códigos
      *          das mensagens para mensageria.
      *        10.11.2017 - F2054109 DE 2016/145195 IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSC055.

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
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTSC055 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * Book de para acionamento do MSG1
-INC IIBKERRO

-INC MSGKWMG1

-INC HLPKW999

       01  GRP-CONTAINER.
           03  CANAL                        PIC X(16) VALUE SPACES.
      *    03  QT-CCT-LIDO                  PIC S9(9) COMP-5 VALUE 0.
      *    03  QT-CCT-EPRO                  PIC S9(9) COMP-5 VALUE 0.
      *    03  VL-EXBR-1                    PIC 9(10)        VALUE 0.
      *    03  VL-EXBR-2                    PIC 9(10)        VALUE 0.
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
       01  CD-ETR-DCS-N                     PIC S9(4) COMP-5 VALUE 0.
       01  CD-TIP-DOC-TCN-N                 PIC S9(4) COMP-5 VALUE 0.
       01  CD-ARQ-OPR-ETRD-N                PIC S9(4) COMP-5 VALUE 0.

      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  NR-NOTA-TCN-OPR-LOG           PIC  9(5) VALUE 0.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

-INC SGTKC055

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

           MOVE 0001                   TO MSG1-CD-SEQL-ERRO
      * Habilita tratamento automatico de Erros DB2
-INC HLPKE997
      * Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 110000-EXCLUIR-CTNR-RPST
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 150000-VALIDA-CAMPOS

           PERFORM 200000-CONSULTAR
           PERFORM 300000-POSTA-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO  TRUE

           EXEC CICS ASSIGN
              CHANNEL ( CANAL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSC055-RPST' )
              CHANNEL   ( CANAL      )
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
              CHANNEL   ( CANAL      )
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

           MOVE ZEROS                        TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSC055-RQSC      TO QT-CCT-EPRO

           EXEC CICS GET
              CONTAINER ( 'SGTSC055-RQSC' )
              CHANNEL   ( CANAL      )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSC055-RQSC' )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSC055-RQSC )
              INTO    ( SGTSC055-RQSC )
           END-EXEC
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       150000-VALIDA-CAMPOS SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           IF  SGTSC055-NR-NOTA-TCN-OPR EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 181               TO  MSG1-CD-ERRO-TS
               MOVE 'Numero nota tecnico operacao inválido.'
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
           INITIALIZE SGTSC055-CD-ETR-DCS
                      SGTSC055-CD-TIP-DOC-TCN
                      SGTSC055-CD-ARQ-OPR-ETRD

           EXEC SQL
                 SELECT CD_UOR_DRTA_OGNL
                      , CD_REF_NOTA_TCN
                      , DT_EMS_NOTA
                      , DT_APVC_NOTA
                      , CD_ETR_DCS
                      , CD_TIP_DOC_TCN
                      , CD_USU_RSP_ULT_ATL
                      , TS_ULT_ATL
                      , CD_ARQ_OPR_ETRD
                   INTO  :SGTSC055-CD-UOR-DRTA-OGNL
                       , :SGTSC055-CD-REF-NOTA-TCN
                       , :SGTSC055-DT-EMS-NOTA
                       , :SGTSC055-DT-APVC-NOTA
                       , :SGTSC055-CD-ETR-DCS
                         :CD-ETR-DCS-N
                       , :SGTSC055-CD-TIP-DOC-TCN
                         :CD-TIP-DOC-TCN-N
                       , :SGTSC055-CD-USU-RSP-ULT-ATL
                       , :SGTSC055-TS-ULT-ATL
                       , :SGTSC055-CD-ARQ-OPR-ETRD
                         :CD-ARQ-OPR-ETRD-N
                       FROM DB2SGT.NOTA_TCN_OPR_ETRD
                WHERE NR_NOTA_TCN_OPR    = :SGTSC055-NR-NOTA-TCN-OPR
           END-EXEC

           IF  SQLCODE = +100
               SET  ERRO-PROGRAMADO     TO TRUE

      * Manter o codigo do erro = 100 para que o modulo de negocio
      * consiga identificar inexistencia de registro

               MOVE 0100                   TO  MSG1-CD-ERRO-TS
               MOVE 'Nota tecnico operacao estruturado inexistente'
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

           EXEC CICS PUT
              CONTAINER ( 'SGTSC055-RPST' )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSC055-RPST )
              FROM    ( SGTSC055-RPST )
           END-EXEC
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------
      *--> Monitora variàveis do pgm, grava e encerra

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
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

           MOVE 'SGTSC055-NR-NOTA-TCN-OPR'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC055-NR-NOTA-TCN-OPR     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC055-CD-UOR-DRTA-OGNL'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC055-CD-UOR-DRTA-OGNL    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC055-CD-REF-NOTA-TCN'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC055-CD-REF-NOTA-TCN     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC055-DT-EMS-NOTA'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC055-DT-EMS-NOTA         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC055-DT-APVC-NOTA'      TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC055-DT-APVC-NOTA        TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC055-CD-ETR-DCS'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC055-CD-ETR-DCS          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC055-CD-TIP-DOC-TCN'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC055-CD-TIP-DOC-TCN      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC055-CD-USU-RSP-ULT-ATL'TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC055-CD-USU-RSP-ULT-ATL  TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC055-TS-ULT-ATL'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC055-TS-ULT-ATL          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC055-CD-ARQ-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC055-CD-ARQ-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO

           EXEC CICS PUT
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   ( CANAL      )
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
-INC HLPKP999
      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997
      * --------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
-INC MSGKPMG1
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999
      * --------------------------------------*
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC.