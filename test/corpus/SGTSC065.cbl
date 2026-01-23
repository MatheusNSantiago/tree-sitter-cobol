       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSC065
      * ANALISTA..: F8608799 - RICARDO RAMOS
      * AUTOR.....: F8608799 - RICARDO RAMOS
      * OBJETIVO..: CONSULTA DA TABELA IMP_MVTC_FNCR_TIT
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS002 09.04.2018 - F2634012 - INLCUSÃO DE CAMPOS
      * VRS001 04.07.2016 - F8608799 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSC065.

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
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTSC065 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

      * Módulos acionados
       77  SBDATA                           PIC  X(08) VALUE 'SBDATA'.
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
-INC SGTKERR1
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

      * Indicadores de nulidades

       01  QT-REG-SELD-NULO                 PIC S9(4) COMP-5 VALUE 0.
       01  CD-TIT-OPR-ETRD-N                PIC S9(4) COMP-5 VALUE 0.
       01  NR-DVS-TIT-N                     PIC S9(4) COMP-5 VALUE 0.
       01  DT-EFT-MVTC-N                    PIC S9(4) COMP-5 VALUE 0.
       01  VL-EFT-MVTC-N                    PIC S9(4) COMP-5 VALUE 0.
       01  DT-CLCD-MVTC-N                   PIC S9(4) COMP-5 VALUE 0.
       01  VL-CLCD-MVTC-N                   PIC S9(4) COMP-5 VALUE 0.
       01  CD-TIP-MVTC-N                    PIC S9(4) COMP-5 VALUE 0.
       01  NM-TIP-MVTC-N                    PIC S9(4) COMP-5 VALUE 0.
       01  QT-EVT-N                         PIC S9(4) COMP-5 VALUE 0.
       01  TX-OBS-MVTC-N                    PIC S9(4) COMP-5 VALUE 0.
       01  TX-DCR-ERRO-IMP-N                PIC S9(4) COMP-5 VALUE 0.
       01  TX-DCR-TIP-CPTR-N                PIC S9(4) COMP-5 VALUE 0.
       01  CD-TIP-LCTO-CTB-N                PIC S9(4) COMP-5 VALUE 0.
       01  CD-CTRA-TIT-VL-N                 PIC S9(4) COMP-5 VALUE 0.

      * Variáveis formato display para mensageria
       01 AUX-VL-15-14                      PIC S9(29) VALUE 0.
       01 AUX-VL-13-02                      PIC S9(15) VALUE 0.

      * Para cálculo de diferença entre datas (após validação SBDATA)
       01  AUX-DT-INC                       PIC 9(08) VALUE 0.
       01  AUX-DT-INC-AAAAMMDD REDEFINES AUX-DT-INC.
           03  ANO                          PIC 9(04).
           03  MES                          PIC 9(02).
           03  DIA                          PIC 9(02).
       01  AUX-DT-FIM                       PIC 9(08) VALUE 0.
       01  AUX-DT-FIM-AAAAMMDD REDEFINES AUX-DT-FIM.
           03  ANO                          PIC 9(04).
           03  MES                          PIC 9(02).
           03  DIA                          PIC 9(02).

      * Subrotinas utilizadas
      * Validar data
       01  AREA-SBDATA-F01.
           03  SBDATA-FUNCAO-F01       PIC  X(3) VALUE 'F01'.
           03  ARG01-F01               PIC  9(8) VALUE 0.
           03  ARG01-F01-R REDEFINES ARG01-F01.
               05  DIA                 PIC  9(2).
               05  MES                 PIC  9(2).
               05  ANO                 PIC  9(4).
           03  ARG02-F01               PIC  9(1) VALUE 0.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

-INC SGTKC065
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
           PERFORM 140000-VALIDA-PARAMETRO

           PERFORM 200000-CONSULTAR
           PERFORM 300000-POSTA-RESPOSTA

           GOBACK
           .
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
              CHANNEL (  NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE

           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSC065-RPST' )
              CHANNEL   (  NM-CNL      )
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
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSC065-RQSC      TO QT-CCT-EPRO
           EXEC CICS GET
              CONTAINER ( 'SGTSC065-RQSC' )
              CHANNEL   ( NM-CNL      )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           EXEC CICS GET
              CONTAINER ( 'SGTSC065-RQSC' )
              CHANNEL   ( NM-CNL       )
              FLENGTH ( LENGTH OF SGTSC065-RQSC )
              INTO    ( SGTSC065-RQSC )
           END-EXEC
           SET  SEM-ERRO               TO TRUE
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDA-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           PERFORM 150000-VALIDA-CAMPOS

           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       150000-VALIDA-CAMPOS SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO
           IF  SGTSC065-DT-IMP-MVTC-FNCR EQUAL SPACES
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0368               TO MSG1-CD-ERRO-TS
               MOVE 'Data importacao movimentacao financeira nao informa
      -             'da.'              TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
      *    Verifica se a data é válida
           MOVE CORR SGTSC065-DT-IMP-MVTC-FNCR-R
                                       TO ARG01-F01-R
           MOVE 4                      TO ARG02-F01
           PERFORM 940000-VALIDA-DATA

      *--> Return code = 24 - Data Inválida.
      *                  22 - Conteudo alfa em argumento numerico
           IF  RETURN-CODE EQUAL 24 OR 22
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0371               TO MSG1-CD-ERRO-TS
               MOVE 'Data importacao movimentacao financeira inválida.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSC065-HR-IMP-MVTC-FNCR EQUAL SPACES
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0369               TO MSG1-CD-ERRO-TS
               MOVE 'Horario importacao movimentacao financeira inválido
      -        '.' TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           IF  SGTSC065-NR-IMP-MVTC-FNCR EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0370               TO MSG1-CD-ERRO-TS
               MOVE 'Numero importacao movimentacao financeira inválido.
      -        '' TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       135000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-CONSULTAR SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO
      *    Campos nulos sao inicializados na consulta
           INITIALIZE SGTSC065-CD-TIT-OPR-ETRD
                      SGTSC065-NR-DVS-TIT
                      SGTSC065-DT-EFT-MVTC
                      SGTSC065-VL-EFT-MVTC
                      SGTSC065-DT-CLCD-MVTC
                      SGTSC065-VL-CLCD-MVTC
                      SGTSC065-CD-TIP-MVTC
                      SGTSC065-QT-EVT
                      SGTSC065-TX-OBS-MVTC
                      SGTSC065-TX-DCR-ERRO-IMP
                      SGTSC065-TX-DCR-TIP-CPTR
                      SGTSC065-CD-TIP-LCTO-CTB
                      SGTSC065-CD-CTRA-TIT-VL
             EXEC SQL
                 SELECT CD_TIT_OPR_ETRD
                      , NR_DVS_TIT
                      , CHAR(TO_CHAR(DT_EFT_MVTC,'DDMMYYYY') , 8)
                      , VL_EFT_MVTC
                      , CHAR(TO_CHAR(DT_CLCD_MVTC,'DDMMYYYY') , 8)
                      , VL_CLCD_MVTC
                      , CD_TIP_MVTC
                      , B.NM_TIP_MVTC_TIT
                      , QT_EVT
                      , CD_EST_IMP
                      , TX_OBS_MVTC
                      , TX_DCR_ERRO_IMP
                      , TX_DCR_TIP_CPTR
                      , CD_TIP_LCTO_CTB
                      , CD_CTRA_TIT_VL
                   INTO  :SGTSC065-CD-TIT-OPR-ETRD
                         :CD-TIT-OPR-ETRD-N
                       , :SGTSC065-NR-DVS-TIT
                         :NR-DVS-TIT-N
                       , :SGTSC065-DT-EFT-MVTC
                         :DT-EFT-MVTC-N
                       , :SGTSC065-VL-EFT-MVTC
                         :VL-EFT-MVTC-N
                       , :SGTSC065-DT-CLCD-MVTC
                         :DT-CLCD-MVTC-N
                       , :SGTSC065-VL-CLCD-MVTC
                         :VL-CLCD-MVTC-N
                       , :SGTSC065-CD-TIP-MVTC
                         :CD-TIP-MVTC-N
                       , :SGTSC065-NM-TIP-MVTC
                         :NM-TIP-MVTC-N
                       , :SGTSC065-QT-EVT
                         :QT-EVT-N
                       , :SGTSC065-CD-EST-IMP
                       , :SGTSC065-TX-OBS-MVTC
                         :TX-OBS-MVTC-N
                       , :SGTSC065-TX-DCR-ERRO-IMP
                         :TX-DCR-ERRO-IMP-N
                       , :SGTSC065-TX-DCR-TIP-CPTR
                         :TX-DCR-TIP-CPTR-N
                       , :SGTSC065-CD-TIP-LCTO-CTB
                         :CD-TIP-LCTO-CTB-N
                       , :SGTSC065-CD-CTRA-TIT-VL
                         :CD-CTRA-TIT-VL-N
                       FROM DB2SGT.IMP_MVTC_FNCR_TIT
                       LEFT OUTER JOIN
                            DB2SGT.TIP_MVTC_TIT B
                       ON  CD_TIP_MVTC = B.CD_TIP_MVTC_TIT

                WHERE DT_IMP_MVTC_FNCR   =  TO_CHAR(TO_DATE(
                   :SGTSC065-DT-IMP-MVTC-FNCR,'DDMMYYYY'), 'DD.MM.YYYY')
                  AND HR_IMP_MVTC_FNCR   = :SGTSC065-HR-IMP-MVTC-FNCR
                  AND NR_IMP_MVTC_FNCR   = :SGTSC065-NR-IMP-MVTC-FNCR
           END-EXEC
           IF  SQLCODE = +100
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0100               TO MSG1-CD-ERRO-TS
               MOVE 'Registro inexistente.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           SET  PUT-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
              CONTAINER ( 'SGTSC065-RPST' )
              CHANNEL   (  NM-CNL      )
              FLENGTH ( LENGTH OF SGTSC065-RPST )
              FROM    ( SGTSC065-RPST )
           END-EXEC
           SET SEM-ERRO                TO TRUE
           .
       300000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       940000-VALIDA-DATA              SECTION.
      *----------------------------------------------------------------*
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           CALL SBDATA USING SBDATA-FUNCAO-F01
                             ARG01-F01
                             ARG02-F01

      *--> Return code = 24 - Data Inválida.
      *                = 22 - Conteudo alfa em argumento numerico

           IF  NOT (RETURN-CODE EQUAL ZEROS OR 24 OR 22)
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE 0051               TO MSG1-CD-ERRO-TS
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE 'Erro no tratamento de datas.'
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE SBDATA             TO MSG1-NM-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       940000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
              MOVE 8888                TO MSG1-CD-ERRO-TS
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

           MOVE 'SGTSC065-DT-IMP-MVTC-FNCR'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC065-DT-IMP-MVTC-FNCR    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC065-HR-IMP-MVTC-FNCR'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC065-HR-IMP-MVTC-FNCR    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC065-NR-IMP-MVTC-FNCR'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC065-NR-IMP-MVTC-FNCR    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC065-CD-TIT-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC065-CD-TIT-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC065-NR-DVS-TIT'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC065-NR-DVS-TIT          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC065-DT-EFT-MVTC'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC065-DT-EFT-MVTC         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC065-VL-EFT-MVTC'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC065-VL-EFT-MVTC         TO AUX-VL-13-02
           MOVE AUX-VL-13-02                 TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC065-DT-CLCD-MVTC'      TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC065-DT-CLCD-MVTC        TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC065-VL-CLCD-MVTC'      TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC065-VL-CLCD-MVTC        TO AUX-VL-13-02
           MOVE AUX-VL-13-02                 TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC065-CD-TIP-MVTC'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC065-CD-TIP-MVTC         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC065-QT-EVT'            TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC065-QT-EVT              TO AUX-VL-15-14
           MOVE AUX-VL-15-14                 TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC065-CD-EST-IMP '       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC065-CD-EST-IMP          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC065-TX-OBS-MVTC'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC065-TX-OBS-MVTC         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC065-TX-DCR-ERRO-IMP'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC065-TX-DCR-ERRO-IMP     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC065-TX-DCR-TIP-CPTR'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC065-TX-DCR-TIP-CPTR     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC065-CD-TIP-LCTO-CTB'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC065-CD-TIP-LCTO-CTB     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC065-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC065-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO
           EXEC CICS PUT
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   ( NM-CNL     )
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
-INC MSGKPMG1
      * --------------------------------------*
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC.