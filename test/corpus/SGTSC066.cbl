       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSC066
      * ANALISTA..: F8608799 - RICARDO RAMOS
      * AUTOR.....: F8608799 - RICARDO RAMOS
      * OBJETIVO..: CONSULTA DA TABELA IMP_PSC_DRIA_SDO
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS002 15.06.2018 - F2634012 - DE 2016/145195 - Inclusão dos
      *                     campos cd_ctra_tit_vl e cd_est_imp
      * VRS001 04.07.2016 - F8608799 - DE 2015/137233 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSC066.

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
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTSC066 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS002'.

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
       01  DT-PSC-N                         PIC S9(4) COMP-5 VALUE 0.
       01  QT-PAPL-TIT-DT-PSC-N             PIC S9(4) COMP-5 VALUE 0.
       01  VL-PU-TIT-DT-PSC-N               PIC S9(4) COMP-5 VALUE 0.
       01  VL-SDO-TIT-DT-PSC-N              PIC S9(4) COMP-5 VALUE 0.
       01  DT-CPR-TIT-N                     PIC S9(4) COMP-5 VALUE 0.
       01  QT-BLQD-PAPL-TIT-N               PIC S9(4) COMP-5 VALUE 0.
       01  QT-DSPN-PAPL-TIT-N               PIC S9(4) COMP-5 VALUE 0.
       01  TX-DCR-ERRO-IMP-N                PIC S9(4) COMP-5 VALUE 0.
       01  TX-DCR-TIP-ATI-N                 PIC S9(4) COMP-5 VALUE 0.
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

-INC SGTKC066
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
              CHANNEL ( NM-CNL )
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
              CONTAINER ( 'SGTSC066-RPST' )
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
           MOVE LENGTH OF SGTSC066-RQSC     TO QT-CCT-EPRO
           EXEC CICS GET
              CONTAINER ( 'SGTSC066-RQSC' )
              CHANNEL   ( NM-CNL      )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSC066-RQSC' )
              CHANNEL   ( NM-CNL       )
              FLENGTH ( LENGTH OF SGTSC066-RQSC )
              INTO    ( SGTSC066-RQSC )
           END-EXEC
           SET  SEM-ERRO               TO TRUE
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDA-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           PERFORM 150000-VALIDA-CAMPOS

           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       150000-VALIDA-CAMPOS SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO
           IF  SGTSC066-DT-IMP-PSC-SDO EQUAL SPACES
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0738               TO MSG1-CD-ERRO-TS
               MOVE 'Data importação posição saldo inválida.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
      *    Verifica se a data é válida                                    VRS001
           MOVE CORR SGTSC066-DT-IMP-PSC-SDO-R
                                       TO ARG01-F01-R                     VRS001
           MOVE 4                      TO ARG02-F01                       VRS001
           PERFORM 940000-VALIDA-DATA                                     VRS001
                                                                          VRS001
      *--> Return code = 24 - Data Inválida.
      *                  22 - Conteudo alfa em argumento numerico
           IF  RETURN-CODE EQUAL 24 OR 22                                 VRS001
               SET ERRO-PROGRAMADO     TO TRUE                            VRS001
               MOVE 0738               TO MSG1-CD-ERRO-TS                 VRS001
               MOVE 'Data importacao posicao saldo inválida.'              VRS00
                                       TO MSG1-TX-ERRO-TS                 VRS001
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF


           IF  SGTSC066-HR-IMP-PSC-SDO EQUAL SPACES
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0737               TO  MSG1-CD-ERRO-TS
               MOVE 'Horario importacao posicao saldo inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           IF  SGTSC066-NR-IMP-PSC-SDO EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0736               TO  MSG1-CD-ERRO-TS
               MOVE 'Numero importacao posicao saldo inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       135000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-CONSULTAR SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO
      *    Campos nulos sao inicializados na consulta
           INITIALIZE SGTSC066-CD-TIT-OPR-ETRD
                      SGTSC066-NR-DVS-TIT
                      SGTSC066-DT-PSC
                      SGTSC066-QT-PAPL-TIT-DT-PSC
                      SGTSC066-VL-PU-TIT-DT-PSC
                      SGTSC066-VL-SDO-TIT-DT-PSC
                      SGTSC066-DT-CPR-TIT
                      SGTSC066-QT-BLQD-PAPL-TIT
                      SGTSC066-QT-DSPN-PAPL-TIT
                      SGTSC066-TX-DCR-ERRO-IMP
                      SGTSC066-TX-DCR-TIP-ATI
                      SGTSC066-CD-CTRA-TIT-VL
           EXEC SQL
                 SELECT CD_TIT_OPR_ETRD
                      , NR_DVS_TIT
                      , CHAR(TO_CHAR(DT_PSC,'DDMMYYYY') , 8)
                      , QT_PAPL_TIT_DT_PSC
                      , VL_PU_TIT_DT_PSC
                      , VL_SDO_TIT_DT_PSC
                      , CHAR(TO_CHAR(DT_CPR_TIT,'DDMMYYYY') , 8)
                      , QT_BLQD_PAPL_TIT
                      , QT_DSPN_PAPL_TIT
                      , CD_EST_IMP
                      , TX_DCR_ERRO_IMP
                      , TX_DCR_TIP_ATI
                      , CD_CTRA_TIT_VL
                   INTO  :SGTSC066-CD-TIT-OPR-ETRD
                         :CD-TIT-OPR-ETRD-N
                       , :SGTSC066-NR-DVS-TIT
                         :NR-DVS-TIT-N
                       , :SGTSC066-DT-PSC
                         :DT-PSC-N
                       , :SGTSC066-QT-PAPL-TIT-DT-PSC
                         :QT-PAPL-TIT-DT-PSC-N
                       , :SGTSC066-VL-PU-TIT-DT-PSC
                         :VL-PU-TIT-DT-PSC-N
                       , :SGTSC066-VL-SDO-TIT-DT-PSC
                         :VL-SDO-TIT-DT-PSC-N
                       , :SGTSC066-DT-CPR-TIT
                         :DT-CPR-TIT-N
                       , :SGTSC066-QT-BLQD-PAPL-TIT
                         :QT-BLQD-PAPL-TIT-N
                       , :SGTSC066-QT-DSPN-PAPL-TIT
                         :QT-DSPN-PAPL-TIT-N
                       , :SGTSC066-CD-EST-IMP
                       , :SGTSC066-TX-DCR-ERRO-IMP
                         :TX-DCR-ERRO-IMP-N
                       , :SGTSC066-TX-DCR-TIP-ATI
                         :TX-DCR-TIP-ATI-N
                       , :SGTSC066-CD-CTRA-TIT-VL
                         :CD-CTRA-TIT-VL-N
                FROM DB2SGT.IMP_PSC_DRIA_SDO
                WHERE DT_IMP_PSC_SDO     = TO_CHAR(TO_DATE(
                    :SGTSC066-DT-IMP-PSC-SDO,'DDMMYYYY'), 'DD.MM.YYYY')
                  AND HR_IMP_PSC_SDO     = :SGTSC066-HR-IMP-PSC-SDO
                  AND NR_IMP_PSC_SDO     = :SGTSC066-NR-IMP-PSC-SDO
           END-EXEC
           IF  SQLCODE = +100
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0100               TO  MSG1-CD-ERRO-TS
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
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO
           SET  PUT-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
              CONTAINER ( 'SGTSC066-RPST' )
              CHANNEL   ( NM-CNL       )
              FLENGTH ( LENGTH OF SGTSC066-RPST )
              FROM    ( SGTSC066-RPST )
           END-EXEC
           SET SEM-ERRO                TO TRUE
           .
       300000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       940000-VALIDA-DATA              SECTION.
      *----------------------------------------------------------------*
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

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

           MOVE 'SGTSC066-DT-IMP-PSC-SDO'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC066-DT-IMP-PSC-SDO      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC066-HR-IMP-PSC-SDO'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC066-HR-IMP-PSC-SDO      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC066-NR-IMP-PSC-SDO'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC066-NR-IMP-PSC-SDO      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC066-CD-TIT-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC066-CD-TIT-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC066-NR-DVS-TIT'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC066-NR-DVS-TIT          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC066-DT-PSC'            TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC066-DT-PSC              TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC066-QT-PAPL-TIT-DT-PSC'TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC066-QT-PAPL-TIT-DT-PSC  TO AUX-VL-15-14
           MOVE AUX-VL-15-14                 TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC066-VL-PU-TIT-DT-PSC'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC066-VL-PU-TIT-DT-PSC    TO AUX-VL-15-14
           MOVE AUX-VL-15-14                 TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC066-VL-SDO-TIT-DT-PSC' TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC066-VL-SDO-TIT-DT-PSC   TO AUX-VL-13-02
           MOVE AUX-VL-13-02                 TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC066-DT-CPR-TIT'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC066-DT-CPR-TIT          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC066-QT-BLQD-PAPL-TIT'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC066-QT-BLQD-PAPL-TIT    TO AUX-VL-15-14
           MOVE AUX-VL-15-14                 TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC066-QT-DSPN-PAPL-TIT'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC066-QT-DSPN-PAPL-TIT    TO AUX-VL-15-14
           MOVE AUX-VL-15-14                 TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC066-IN-IMP-SDO-SCS'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC066-CD-EST-IMP          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC066-TX-DCR-ERRO-IMP'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC066-TX-DCR-ERRO-IMP     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC066-TX-DCR-TIP-ATI'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC066-TX-DCR-TIP-ATI      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSC066-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC066-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS              TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS              TO IIBKERRO-TX-ERRO
           EXEC CICS PUT
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   (  NM-CNL      )
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