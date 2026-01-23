      *-----------------------------------------------------------------
      * Programa..: SGTSC086
      * Analista..: F2054109 - Cintia Ap Soares Iglesias
      * Autor.....: F6361711 - Luis A P Valamede
      * Objetivo..: Consultar demonstrativo de caixa do título.
      *             na tabela: DB2SGT.DMTV_CX_TIT
      * Compilação: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 02.05.2017 F6361711  DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSC086.

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
               '*** STORAGE SECTION começa aqui ***'.
       77  CTE-PROG                    PIC  X(16) VALUE
                             '*** SGTSC086 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

       77  SBDATA                      PIC  X(08) VALUE 'SBDATA'.

      *--> Container de erro
       77  CTNR-PRM-ERRO               PIC X(16) VALUE 'PRM-ERRO'.
      *--> Container da requisição desta subrotina
       77  CTNR-SGTSC086-RQSC           PIC X(16) VALUE 'SGTSC086-RQSC'.
      *--> Container da resposta desta subrotina
       77  CTNR-SGTSC086-RPST           PIC X(16) VALUE 'SGTSC086-RPST'.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Registro da tabela
       01  DMTV-CX-TIT.
           03  CHAVE.
               05  TS-REG-PGTO         PIC  X(26)
                   VALUE '0000-00-00-00.00.00.000000'.
               05  TS-REG-PGTO-R
                   REDEFINES TS-REG-PGTO.
                   07  AA              PIC  X(04).
                   07  FILLER          PIC  X(01).
                   07  MM              PIC  X(02).
                   07  FILLER          PIC  X(01).
                   07  DD              PIC  X(02).
                   07  FILLER          PIC  X(01).
                   07  HH              PIC  X(02).
                   07  FILLER          PIC  X(01).
                   07  MNTO            PIC  X(02).
                   07  FILLER          PIC  X(01).
                   07  SGDO            PIC  X(02).
                   07  FILLER          PIC  X(01).
                   07  MSGD            PIC  X(06).
           03  DADOS.
               05  DT-EFT-MVTC         PIC  X(10) VALUE '00.00.0000'.
               05  DT-EFT-MVTC-R       REDEFINES DT-EFT-MVTC.
                   07  DD              PIC  X(02).
                   07  FILLER          PIC  X(01).
                   07  MM              PIC  X(02).
                   07  FILLER          PIC  X(01).
                   07  AA              PIC  X(04).
               05  DT-PRVT-MVTC        PIC  X(10) VALUE '00.00.0000'.
               05  DT-PRVT-MVTC-R      REDEFINES DT-PRVT-MVTC.
                   07  DD              PIC  X(02).
                   07  FILLER          PIC  X(01).
                   07  MM              PIC  X(02).
                   07  FILLER          PIC  X(01).
                   07  AA              PIC  X(04).

       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE 0.

      * Área para formatação de variaveis monitoradas
       01  NRC-MSG                     PIC +999999999999999.99.

      *--> Indicadores de nulidades
       01  QT-REG-SELD-NULO            PIC S9(04) COMP-5 VALUE 0.
       01  DT-PRVT-MVTC-N              PIC S9(04) COMP-5 VALUE 0.
       01  VL-PRVT-MVTC-N              PIC S9(04) COMP-5 VALUE 0.
       01  VL-EFT-MVTC-N               PIC S9(04) COMP-5 VALUE 0.

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book do programa
       01  SGTSC086-DADOS.
-INC SGTKC086

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       77  CTE-FINAL-SS                PIC X(40) VALUE
               '*** STORAGE SECTION termina aqui ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

           IF  NOT  EXECUCAO-ONLINE  AND
               NOT  EXECUCAO-BATCH
               CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      *--> Habilita tratamento automático de Erros DB2
-INC HLPKE997
      *--> Habilita captura automática de Abend e Erros CICS
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
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

      *--> Devido à captura automática de erro CICS, deve ser previsto o
      *    tipo de erro.
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS  ASSIGN
                      CHANNEL ( NM-CNL                                 )
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

           MOVE  CTNR-SGTSC086-RPST    TO  NM-CTNR

           SET   DEL-CONTAINER         TO  TRUE

           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR                              )
                      CHANNEL   ( NM-CNL                               )
                      NOHANDLE
           END-EXEC
           IF  NOT (EIBRESP                EQUAL  DFHRESP(NORMAL) OR
                    EIBRESP                EQUAL  DFHRESP(CONTAINERERR))
               PERFORM 999002-TRATA-CONDITION-CICS
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

           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR                              )
                      CHANNEL   ( NM-CNL                               )
                      NOHANDLE
           END-EXEC
           IF  NOT (EIBRESP                EQUAL  DFHRESP(NORMAL)   OR
                    EIBRESP                EQUAL DFHRESP(CONTAINERERR))
               PERFORM 999002-TRATA-CONDITION-CICS
           END-IF
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       400000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-CCT-LIDO
           MOVE  CTNR-SGTSC086-RQSC    TO  NM-CTNR
           SET   GET-CONTAINER         TO  TRUE

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                              )
                      CHANNEL   ( NM-CNL                               )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO                          )
           END-EXEC

           MOVE  LENGTH OF  SGTSC086-RQSC
                                       TO  QT-CCT-EPRO

      *--> Devido à captura automática de erro CICS, deve ser previsto o
      *    erro de container.
           SET   ERRO-CONTAINER        TO  TRUE
           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                              )
                      CHANNEL   ( NM-CNL                               )
                      FLENGTH   ( LENGTH OF SGTSC086-RQSC              )
                      INTO      ( SGTSC086-RQSC                        )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum.
           SET   SEM-ERRO              TO  TRUE
           .
       400000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       500000-VALIDA-CAMPOS            SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  DMTV-CX-TIT

           IF  SGTSC086-CD-TIT-OPR-ETRD    EQUAL  SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do título da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSC086-NR-DVS-TIT         EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da divisão do título não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSC086-TS-REG-PGTO        EQUAL ZEROS  OR
               SGTSC086-TS-REG-PGTO        EQUAL SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Timestamp do registro do pagamento não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  SGTSC086-TS-REG-PGTO
                                       TO  TS-REG-PGTO-R
           .
       500000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       600000-CONSULTAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

      *--> Campos nulos sao inicializados na consulta.
           INITIALIZE                      DT-EFT-MVTC
                                           SGTSC086-CD-TIP-MVTC-TIT
                                           DT-PRVT-MVTC
                                           DT-PRVT-MVTC-N
                                           SGTSC086-VL-PRVT-MVTC
                                           VL-PRVT-MVTC-N
                                           SGTSC086-VL-EFT-MVTC
                                           VL-EFT-MVTC-N
                                           SGTSC086-IN-MVTC-ATV
                                           SGTSC086-CD-USU-IMP

           EXEC SQL
                SELECT DT_EFT_MVTC
                     , CD_TIP_MVTC_TIT
                     , DT_PRVT_MVTC
                     , VL_PRVT_MVTC
                     , VL_EFT_MVTC
                     , IN_MVTC_ATV
                     , CD_USU_IMP
                  INTO :DT-EFT-MVTC
                     , :SGTSC086-CD-TIP-MVTC-TIT
                     , :DT-PRVT-MVTC
                       :DT-PRVT-MVTC-N
                     , :SGTSC086-VL-PRVT-MVTC
                       :VL-PRVT-MVTC-N
                     , :SGTSC086-VL-EFT-MVTC
                       :VL-EFT-MVTC-N
                     , :SGTSC086-IN-MVTC-ATV
                     , :SGTSC086-CD-USU-IMP
                  FROM DB2SGT.DMTV_CX_TIT
                 WHERE CD_TIT_OPR_ETRD      =
                                       :SGTSC086-CD-TIT-OPR-ETRD
                   AND NR_DVS_TIT           =
                                       :SGTSC086-NR-DVS-TIT
                   AND TS_REG_PGTO          =
                                       :TS-REG-PGTO
           END-EXEC

           IF  SQLCODE                     EQUAL  +100
               INITIALIZE  SGTSC086-RPST
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Demonstrativo de caixa do título inexistente.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  '00000000'            TO  SGTSC086-DT-EFT-MVTC-R
           MOVE  CORR DT-EFT-MVTC-R    TO  SGTSC086-DT-EFT-MVTC-R

           MOVE  '00000000'            TO  SGTSC086-DT-PRVT-MVTC-R
           MOVE  CORR DT-PRVT-MVTC-R   TO  SGTSC086-DT-PRVT-MVTC-R
           .
       600000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       700000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           MOVE  CTNR-SGTSC086-RPST    TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Devido à captura automática de erro CICS, deve ser previsto o
      *    erro de container.
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                              )
                      CHANNEL   ( NM-CNL                               )
                      FLENGTH   ( LENGTH OF SGTSC086-RPST              )
                      FROM      ( SGTSC086-RPST                        )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum.
           SET   SEM-ERRO              TO  TRUE
           .
       700000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

      *--> Verifica se o tipo de erro gerado possui tratamento interno.
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

           MOVE  'SGTSC086-CD-TIT-OPR-ETRD               '
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC086-CD-TIT-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC086-NR-DVS-TIT                    '
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC086-NR-DVS-TIT   TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC086-TS-REG-PGTO                   '
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC086-TS-REG-PGTO  TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *--> Grava o erro no MSG
           PERFORM MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

           EXEC CICS  PUT
                      CONTAINER ( CTNR-PRM-ERRO                        )
                      CHANNEL   ( NM-CNL                               )
                      FROM      ( IIBKERRO-PRM-ERRO                    )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO          )
                      NOHANDLE
           END-EXEC

           PERFORM 999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      * ---------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * ---------------------------------------
-INC HLPKP999
      * ---------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
-INC SGTKP999
      * ---------------------------------------
      * 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997
      * ---------------------------------------
      * MSG1-NTF-MSG                   SECTION.
-INC MSGKPMG1
      * ---------------------------------------
       999999-ENCERRAR                 SECTION.
      * ---------------------------------------

           EXEC CICS
                RETURN
           END-EXEC.