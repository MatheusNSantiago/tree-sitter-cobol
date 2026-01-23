      *-----------------------------------------------------------------
      * Programa..: SGTSC005
      * Analista..: F0175702 Adriano Seiji Nakamura
      * Autor.....: F0175702 Adriano Seiji Nakamura
      * Data......: 15/12/2016
      * Objetivo..: Consulta da Tabela OPR_ETRD_MCP
      * Compilacao: Cobol/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 15/12/2016 - F0175702 - DE 2015/137233 - Implantacao
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSC005.

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
       77  CTE-INICIO-SS               PIC  X(40) VALUE
                                       '*** S.S. Comeca Aqui ***'.
       77  CTE-PROG                    PIC  X(18) VALUE
                                       '*** SGTSC005 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
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

      *--> Indicadores de nulidades

       01  CD-BCO-CRD-VL-OPR-N              PIC S9(4) COMP-5 VALUE 0.
       01  CD-DEPE-CRD-VL-OPR-N             PIC S9(4) COMP-5 VALUE 0.
       01  CD-SUB-DEPE-CRD-VL-N             PIC S9(4) COMP-5 VALUE 0.
       01  CD-UOR-BB-CRD-VL-N               PIC S9(4) COMP-5 VALUE 0.
       01  NR-CC-OTR-BCO-N                  PIC S9(4) COMP-5 VALUE 0.
       01  DV-CC-OTR-BCO-N                  PIC S9(4) COMP-5 VALUE 0.
       01  DT-CPR-OPR-N                     PIC S9(4) COMP-5 VALUE 0.
       01  DT-VNCT-OPR-N                    PIC S9(4) COMP-5 VALUE 0.
       01  DT-LQDC-OPR-DRIV-N               PIC S9(4) COMP-5 VALUE 0.
       01  CD-TIT-OPR-ETRD-N                PIC S9(4) COMP-5 VALUE 0.
       01  NR-DVS-TIT-N                     PIC S9(4) COMP-5 VALUE 0.
       01  NR-UNCO-CTR-OPR-N                PIC S9(4) COMP-5 VALUE 0.
       01  CD-TIP-OPR-ETRD-N                PIC S9(4) COMP-5 VALUE 0.
       01  CD-ITC-CVM-OPR-N                 PIC S9(4) COMP-5 VALUE 0.
       01  CD-CTGR-TVM-N                    PIC S9(4) COMP-5 VALUE 0.
       01  CD-ESPC-DBTR-OPR-N               PIC S9(4) COMP-5 VALUE 0.
       01  CD-TIP-COTA-TIT-N                PIC S9(4) COMP-5 VALUE 0.
       01  NR-VRS-TIP-COTA-N                PIC S9(4) COMP-5 VALUE 0.
       01  CD-TIP-MC-CPR-N                  PIC S9(4) COMP-5 VALUE 0.
       01  CD-RSCO-OPR-N                    PIC S9(4) COMP-5 VALUE 0.
       01  CD-CMRA-OPR-ETRD-N               PIC S9(4) COMP-5 VALUE 0.
       01  NR-EMS-TIT-OPR-N                 PIC S9(4) COMP-5 VALUE 0.
       01  NR-SRE-EMS-TIT-OPR-N             PIC S9(4) COMP-5 VALUE 0.
       01  DT-INC-VGC-VRS-OPR-N             PIC S9(4) COMP-5 VALUE 0.
       01  DT-MNDT-OPR-N                    PIC S9(4) COMP-5 VALUE 0.
       01  DT-EMS-TIT-OPR-N                 PIC S9(4) COMP-5 VALUE 0.
       01  DT-EMS-PREF-N                    PIC S9(4) COMP-5 VALUE 0.
       01  QT-DD-PZ-VLD-PREF-N              PIC S9(4) COMP-5 VALUE 0.
       01  QT-MM-CARE-JUR-N                 PIC S9(4) COMP-5 VALUE 0.
       01  QT-MM-CARE-OPR-N                 PIC S9(4) COMP-5 VALUE 0.
       01  QT-MM-CARE-RSGT-N                PIC S9(4) COMP-5 VALUE 0.
       01  QT-MM-CARE-AMTZ-N                PIC S9(4) COMP-5 VALUE 0.
       01  QT-MM-CARE-AQS-N                 PIC S9(4) COMP-5 VALUE 0.
       01  PC-TARF-GRT-TAXA-N               PIC S9(4) COMP-5 VALUE 0.
       01  VL-PRV-ENCT-N                    PIC S9(4) COMP-5 VALUE 0.
       01  VL-MNDT-OPR-N                    PIC S9(4) COMP-5 VALUE 0.
       01  VL-APVD-BB-OPR-N                 PIC S9(4) COMP-5 VALUE 0.
       01  VL-GRT-BB-OPR-N                  PIC S9(4) COMP-5 VALUE 0.
       01  TX-PGTO-OPR-VCLD-N               PIC S9(4) COMP-5 VALUE 0.
       01  DT-CMPS-FUT-NOTA-N               PIC S9(4) COMP-5 VALUE 0.
       01  NR-VRS-REF-N                     PIC S9(4) COMP-5 VALUE 0.
       01  VL-EVT-VCLD-VRS-N                PIC S9(4) COMP-5 VALUE 0.
       01  DT-EVT-VCLD-VRS-N                PIC S9(4) COMP-5 VALUE 0.
       01  TX-OBS-EVT-N                     PIC S9(4) COMP-5 VALUE 0.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-TX.
           03  CD-OPR-ETRD             PIC  9(11).
       01  VL-7V4                      PIC  --.---.--9,9999.
       01  VL-4V10                     PIC  --.--9,9999999999.
       01  VL-15V2                     PIC ----.---.---.---.--9,99.
       01  VL-9V8                      PIC ----.---.--9,99999999.
      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book do container de requisicao e resposta deste modulo
      *01  SGTSC005-DADOS.
-INC SGTKC005

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

           IF  NOT EXECUCAO-ONLINE  AND
               NOT EXECUCAO-BATCH
               CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997

      *--> Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM  100000-DESCOBRIR-CANAL

           PERFORM  110000-EXCLUIR-CTNR-RPST

           PERFORM  120000-EXCLUIR-CTNR-ERRO

           PERFORM  130000-RECEBER-REQUISICAO

           PERFORM  140000-VALIDAR-PARAMETRO

           PERFORM  200000-CONSULTAR

           PERFORM  300000-POSTA-RESPOSTA

           GOBACK
           .

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS  ASSIGN
                      CHANNEL ( NM-CNL                                 )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'SGTSC005-RPST'                      )
                      CHANNEL   ( NM-CNL                               )
                      NOHANDLE
           END-EXEC

           IF  NOT (EIBRESP                EQUAL  DFHRESP(NORMAL)  OR
                    EIBRESP                EQUAL  DFHRESP(CONTAINERERR))
               PERFORM  999002-TRATA-CONDITION-CICS
           END-IF
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'PRM-ERRO'                           )
                      CHANNEL   ( NM-CNL                               )
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
           MOVE  LENGTH OF SGTSC005-RQSC
                                       TO  QT-CCT-EPRO
           EXEC CICS  GET
                      CONTAINER ( 'SGTSC005-RQSC'                      )
                      CHANNEL   ( NM-CNL                               )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO                          )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS  GET
                      CONTAINER ( 'SGTSC005-RQSC'                      )
                      CHANNEL   ( NM-CNL                               )
                      FLENGTH   ( LENGTH OF SGTSC005-RQSC              )
                      INTO      ( SGTSC005-RQSC                        )
           END-EXEC

           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSC005-CD-CTRA-TIT-VL     NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSC005-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturado mercado de capitais
      -              'não numérico.'   TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSC005-CD-OPR-ETRD-MCP    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada mercado de capitais
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSC005-NR-VRS-OPR-ETRD    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturado inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-CONSULTAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

      *--> Campos nulos sao inicializados na consulta
           INITIALIZE SGTSC005-CD-BCO-CRD-VL-OPR
                      SGTSC005-CD-DEPE-CRD-VL-OPR
                      SGTSC005-CD-SUB-DEPE-CRD-VL
                      SGTSC005-CD-UOR-BB-CRD-VL
                      SGTSC005-NR-CC-OTR-BCO
                      SGTSC005-DV-CC-OTR-BCO
                      SGTSC005-DT-CPR-OPR
                      SGTSC005-DT-VNCT-OPR
                      SGTSC005-DT-LQDC-OPR-DRIV
                      SGTSC005-CD-TIT-OPR-ETRD
                      SGTSC005-NR-DVS-TIT
                      SGTSC005-NR-UNCO-CTR-OPR
                      SGTSC005-CD-TIP-OPR-ETRD
                      SGTSC005-CD-ITC-CVM-OPR
                      SGTSC005-CD-CTGR-TVM
                      SGTSC005-CD-ESPC-DBTR-OPR
                      SGTSC005-CD-TIP-COTA-TIT
                      SGTSC005-NR-VRS-TIP-COTA
                      SGTSC005-CD-TIP-MC-CPR
                      SGTSC005-CD-RSCO-OPR
                      SGTSC005-CD-CMRA-OPR-ETRD
                      SGTSC005-NR-EMS-TIT-OPR
                      SGTSC005-NR-SRE-EMS-TIT-OPR
                      SGTSC005-DT-INC-VGC-VRS-OPR
                      SGTSC005-DT-MNDT-OPR
                      SGTSC005-DT-EMS-TIT-OPR
                      SGTSC005-DT-EMS-PREF
                      SGTSC005-QT-DD-PZ-VLD-PREF
                      SGTSC005-QT-MM-CARE-JUR
                      SGTSC005-QT-MM-CARE-OPR
                      SGTSC005-QT-MM-CARE-RSGT
                      SGTSC005-QT-MM-CARE-AMTZ
                      SGTSC005-QT-MM-CARE-AQS
                      SGTSC005-PC-TARF-GRT-TAXA
                      SGTSC005-VL-PRV-ENCT
                      SGTSC005-VL-MNDT-OPR
                      SGTSC005-VL-APVD-BB-OPR
                      SGTSC005-VL-GRT-BB-OPR
                      SGTSC005-TX-PGTO-OPR-VCLD
                      SGTSC005-DT-CMPS-FUT-NOTA
                      SGTSC005-NR-VRS-REF
                      SGTSC005-VL-EVT-VCLD-VRS
                      SGTSC005-DT-EVT-VCLD-VRS
                      SGTSC005-TX-OBS-EVT
           EXEC SQL
                 SELECT CD_USU_ULT_ATL
                      , TS_ULT_ATL
                      , CD_BCO_CRD_VL_OPR
                      , CD_DEPE_CRD_VL_OPR
                      , CD_SUB_DEPE_CRD_VL
                      , CD_UOR_DTTR_OPR
                      , CD_PRD
                      , CD_MDLD
                      , CD_UOR_BB_CRD_VL
                      , CD_UOR_BB
                      , NR_CC_BB
                      , IN_RMNC_VRV
                      , NR_CC_OTR_BCO
                      , DV_CC_OTR_BCO
                      , QT_PAPL_TIT_EMS
                      , QT_PAPL_TIT_CPR
                      , DT_CPR_OPR
                      , DT_VNCT_OPR
                      , DT_LQDC_OPR_DRIV
                      , CD_TIT_OPR_ETRD
                      , NR_DVS_TIT
                      , NR_UNCO_CTR_OPR
                      , CD_TIP_OPR_ETRD
                      , IN_OPR_ETRD_SPRO
                      , IN_OPR_ETRD_VRJ
                      , IN_OPR_ETRD_DSCP
                      , IN_REG_OPR_BB
                      , IN_OPR_N_NEGO
                      , IN_EXPO_VCLD_GRT
                      , CD_ITC_CVM_OPR
                      , CD_CTGR_TVM
                      , CD_ESPC_DBTR_OPR
                      , CD_TIP_COTA_TIT
                      , NR_VRS_TIP_COTA
                      , CD_TIP_MC_CPR
                      , CD_RSCO_OPR
                      , CD_CMRA_OPR_ETRD
                      , CD_TIP_VRS_OPR
                      , NR_EMS_TIT_OPR
                      , NR_SRE_EMS_TIT_OPR
                      , DT_INC_VGC_VRS_OPR
                      , DT_MNDT_OPR
                      , DT_EMS_TIT_OPR
                      , DT_EMS_PREF
                      , QT_DD_PZ_VLD_PREF
                      , QT_MM_CARE_JUR
                      , QT_MM_CARE_OPR
                      , QT_MM_CARE_RSGT
                      , QT_MM_CARE_AMTZ
                      , QT_MM_CARE_AQS
                      , PC_TARF_GRT_TAXA
                      , VL_PRV_ENCT
                      , VL_CPR_OPR
                      , VL_MNDT_OPR
                      , VL_PU_EMS_TIT_OPR
                      , VL_APVD_BB_OPR
                      , VL_PU_CPR_TIT_OPR
                      , VL_GRT_BB_OPR
                      , TX_PGTO_OPR_VCLD
                      , DT_CMPS_FUT_NOTA
                      , NR_VRS_REF
                      , VL_EVT_VCLD_VRS
                      , DT_EVT_VCLD_VRS
                      , TX_OBS_EVT
                   INTO  :SGTSC005-CD-USU-ULT-ATL
                       , :SGTSC005-TS-ULT-ATL
                       , :SGTSC005-CD-BCO-CRD-VL-OPR
                         :CD-BCO-CRD-VL-OPR-N
                       , :SGTSC005-CD-DEPE-CRD-VL-OPR
                         :CD-DEPE-CRD-VL-OPR-N
                       , :SGTSC005-CD-SUB-DEPE-CRD-VL
                         :CD-SUB-DEPE-CRD-VL-N
                       , :SGTSC005-CD-UOR-DTTR-OPR
                       , :SGTSC005-CD-PRD
                       , :SGTSC005-CD-MDLD
                       , :SGTSC005-CD-UOR-BB-CRD-VL
                         :CD-UOR-BB-CRD-VL-N
                       , :SGTSC005-CD-UOR-BB
                       , :SGTSC005-NR-CC-BB
                       , :SGTSC005-IN-RMNC-VRV
                       , :SGTSC005-NR-CC-OTR-BCO
                         :NR-CC-OTR-BCO-N
                       , :SGTSC005-DV-CC-OTR-BCO
                         :DV-CC-OTR-BCO-N
                       , :SGTSC005-QT-PAPL-TIT-EMS
                       , :SGTSC005-QT-PAPL-TIT-CPR
                       , :SGTSC005-DT-CPR-OPR
                         :DT-CPR-OPR-N
                       , :SGTSC005-DT-VNCT-OPR
                         :DT-VNCT-OPR-N
                       , :SGTSC005-DT-LQDC-OPR-DRIV
                         :DT-LQDC-OPR-DRIV-N
                       , :SGTSC005-CD-TIT-OPR-ETRD
                         :CD-TIT-OPR-ETRD-N
                       , :SGTSC005-NR-DVS-TIT
                         :NR-DVS-TIT-N
                       , :SGTSC005-NR-UNCO-CTR-OPR
                         :NR-UNCO-CTR-OPR-N
                       , :SGTSC005-CD-TIP-OPR-ETRD
                         :CD-TIP-OPR-ETRD-N
                       , :SGTSC005-IN-OPR-ETRD-SPRO
                       , :SGTSC005-IN-OPR-ETRD-VRJ
                       , :SGTSC005-IN-OPR-ETRD-DSCP
                       , :SGTSC005-IN-REG-OPR-BB
                       , :SGTSC005-IN-OPR-N-NEGO
                       , :SGTSC005-IN-EXPO-VCLD-GRT
                       , :SGTSC005-CD-ITC-CVM-OPR
                         :CD-ITC-CVM-OPR-N
                       , :SGTSC005-CD-CTGR-TVM
                         :CD-CTGR-TVM-N
                       , :SGTSC005-CD-ESPC-DBTR-OPR
                         :CD-ESPC-DBTR-OPR-N
                       , :SGTSC005-CD-TIP-COTA-TIT
                         :CD-TIP-COTA-TIT-N
                       , :SGTSC005-NR-VRS-TIP-COTA
                         :NR-VRS-TIP-COTA-N
                       , :SGTSC005-CD-TIP-MC-CPR
                         :CD-TIP-MC-CPR-N
                       , :SGTSC005-CD-RSCO-OPR
                         :CD-RSCO-OPR-N
                       , :SGTSC005-CD-CMRA-OPR-ETRD
                         :CD-CMRA-OPR-ETRD-N
                       , :SGTSC005-CD-TIP-VRS-OPR
                       , :SGTSC005-NR-EMS-TIT-OPR
                         :NR-EMS-TIT-OPR-N
                       , :SGTSC005-NR-SRE-EMS-TIT-OPR
                         :NR-SRE-EMS-TIT-OPR-N
                       , :SGTSC005-DT-INC-VGC-VRS-OPR
                         :DT-INC-VGC-VRS-OPR-N
                       , :SGTSC005-DT-MNDT-OPR
                         :DT-MNDT-OPR-N
                       , :SGTSC005-DT-EMS-TIT-OPR
                         :DT-EMS-TIT-OPR-N
                       , :SGTSC005-DT-EMS-PREF
                         :DT-EMS-PREF-N
                       , :SGTSC005-QT-DD-PZ-VLD-PREF
                         :QT-DD-PZ-VLD-PREF-N
                       , :SGTSC005-QT-MM-CARE-JUR
                         :QT-MM-CARE-JUR-N
                       , :SGTSC005-QT-MM-CARE-OPR
                         :QT-MM-CARE-OPR-N
                       , :SGTSC005-QT-MM-CARE-RSGT
                         :QT-MM-CARE-RSGT-N
                       , :SGTSC005-QT-MM-CARE-AMTZ
                         :QT-MM-CARE-AMTZ-N
                       , :SGTSC005-QT-MM-CARE-AQS
                         :QT-MM-CARE-AQS-N
                       , :SGTSC005-PC-TARF-GRT-TAXA
                         :PC-TARF-GRT-TAXA-N
                       , :SGTSC005-VL-PRV-ENCT
                         :VL-PRV-ENCT-N
                       , :SGTSC005-VL-CPR-OPR
                       , :SGTSC005-VL-MNDT-OPR
                         :VL-MNDT-OPR-N
                       , :SGTSC005-VL-PU-EMS-TIT-OPR
                       , :SGTSC005-VL-APVD-BB-OPR
                         :VL-APVD-BB-OPR-N
                       , :SGTSC005-VL-PU-CPR-TIT-OPR
                       , :SGTSC005-VL-GRT-BB-OPR
                         :VL-GRT-BB-OPR-N
                       , :SGTSC005-TX-PGTO-OPR-VCLD
                         :TX-PGTO-OPR-VCLD-N
                       , :SGTSC005-DT-CMPS-FUT-NOTA
                         :DT-CMPS-FUT-NOTA-N
                       , :SGTSC005-NR-VRS-REF
                         :NR-VRS-REF-N
                       , :SGTSC005-VL-EVT-VCLD-VRS
                         :VL-EVT-VCLD-VRS-N
                       , :SGTSC005-DT-EVT-VCLD-VRS
                         :DT-EVT-VCLD-VRS-N
                       , :SGTSC005-TX-OBS-EVT
                         :TX-OBS-EVT-N
                       FROM DB2SGT.OPR_ETRD_MCP
                WHERE CD_CTRA_TIT_VL     = :SGTSC005-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP    = :SGTSC005-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD    = :SGTSC005-NR-VRS-OPR-ETRD
           END-EXEC

           IF  SQLCODE                     EQUAL +100
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0100              TO  MSG1-CD-ERRO-TS
               MOVE  'Registro inexistente.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'SGTSC005-RPST'                      )
                      CHANNEL   ( NM-CNL                               )
                      FLENGTH   ( LENGTH OF SGTSC005-RPST              )
                      FROM      ( SGTSC005-RPST                        )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------
           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Verifica se o tipo de erro gerado possui tratamento interno
           EVALUATE  TRUE
               WHEN  ERRO-CONTAINER
                     PERFORM  999004-TRATA-ERRO-CTNR
               WHEN  ERRO-TAMANHO-AREA
                     PERFORM  999005-TRATA-ERRO-AREA-DADOS
               WHEN  ERRO-CANAL
                     PERFORM  999006-TRATA-ERRO-CANAL
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

      *--> Campos da Requisição
           MOVE  'SGTSC005-CD-CTRA-TIT-VL'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-CD-CTRA-TIT-VL        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-CD-OPR-ETRD-MCP'     TO  MSGKW999-NM-VRV-MNT
           IF  SGTSC005-CD-OPR-ETRD-MCP    NUMERIC
               MOVE  SGTSC005-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD
               MOVE  CD-OPR-ETRD-TX             TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-NR-VRS-OPR-ETRD'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-NR-VRS-OPR-ETRD       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Campos da Resposta
           MOVE  'SGTSC005-CD-USU-ULT-ATL'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-CD-USU-ULT-ATL        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-TS-ULT-ATL'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-TS-ULT-ATL            TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-CD-BCO-CRD-VL-OPR'   TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-CD-BCO-CRD-VL-OPR     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC005-CD-DEPE-CRD-VL-OPR'   TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-CD-DEPE-CRD-VL-OPR    TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC005-CD-SUB-DEPE-CRD-VL'   TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-CD-SUB-DEPE-CRD-VL    TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-CD-UOR-DTTR-OPR'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-CD-UOR-DTTR-OPR       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-CD-PRD'              TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-CD-PRD                TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-CD-MDLD'             TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-CD-MDLD               TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-CD-UOR-BB-CRD-VL'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-CD-UOR-BB-CRD-VL      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-CD-UOR-BB'           TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-CD-UOR-BB             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-NR-CC-BB'            TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-NR-CC-BB              TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-IN-RMNC-VRV'         TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-IN-RMNC-VRV           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-NR-CC-OTR-BCO'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-NR-CC-OTR-BCO         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-DV-CC-OTR-BCO'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-DV-CC-OTR-BCO         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-QT-PAPL-TIT-EMS'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-QT-PAPL-TIT-EMS       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-QT-PAPL-TIT-CPR'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-QT-PAPL-TIT-CPR       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-DT-CPR-OPR'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-DT-CPR-OPR            TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-DT-VNCT-OPR'         TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-DT-VNCT-OPR           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-DT-LQDC-OPR-DRIV'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-DT-LQDC-OPR-DRIV      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-CD-TIT-OPR-ETRD'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-CD-TIT-OPR-ETRD       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-NR-DVS-TIT'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-NR-DVS-TIT            TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-NR-UNCO-CTR-OPR'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-NR-UNCO-CTR-OPR       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-CD-TIP-OPR-ETRD'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-CD-TIP-OPR-ETRD       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-IN-OPR-ETRD-SPRO'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-IN-OPR-ETRD-SPRO      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-IN-OPR-ETRD-VRJ'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-IN-OPR-ETRD-VRJ       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-IN-OPR-ETRD-DSCP'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-IN-OPR-ETRD-DSCP      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-IN-REG-OPR-BB'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-IN-REG-OPR-BB         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-IN-OPR-N-NEGO'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-IN-OPR-N-NEGO         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-IN-EXPO-VCLD-GRT'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-IN-EXPO-VCLD-GRT      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-CD-ITC-CVM-OPR'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-CD-ITC-CVM-OPR        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-CD-CTGR-TVM'         TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-CD-CTGR-TVM           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-CD-ESPC-DBTR-OPR'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-CD-ESPC-DBTR-OPR      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-CD-TIP-COTA-TIT'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-CD-TIP-COTA-TIT       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-NR-VRS-TIP-COTA'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-NR-VRS-TIP-COTA       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-CD-TIP-MC-CPR'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-CD-TIP-MC-CPR         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-CD-RSCO-OPR'         TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-CD-RSCO-OPR           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-CD-CMRA-OPR-ETRD'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-CD-CMRA-OPR-ETRD      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-CD-TIP-VRS-OPR'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-CD-TIP-VRS-OPR        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-NR-EMS-TIT-OPR'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-NR-EMS-TIT-OPR        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-NR-SRE-EMS-TIT-OPR'  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-NR-SRE-EMS-TIT-OPR    TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-DT-INC-VGC-VRS-OPR'  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-DT-INC-VGC-VRS-OPR    TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-DT-MNDT-OPR'         TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-DT-MNDT-OPR           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-DT-EMS-TIT-OPR'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-DT-EMS-TIT-OPR        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-DT-EMS-PREF'         TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-DT-EMS-PREF           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-QT-DD-PZ-VLD-PREF'   TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-QT-DD-PZ-VLD-PREF     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-QT-MM-CARE-JUR'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-QT-MM-CARE-JUR        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-QT-MM-CARE-OPR'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-QT-MM-CARE-OPR        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-QT-MM-CARE-RSGT'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-QT-MM-CARE-RSGT       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-QT-MM-CARE-AMTZ'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-QT-MM-CARE-AMTZ       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-QT-MM-CARE-AQS'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-QT-MM-CARE-AQS        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-PC-TARF-GRT-TAXA'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-PC-TARF-GRT-TAXA      TO  VL-4V10
           MOVE  VL-4V10                        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-VL-PRV-ENCT'         TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-VL-PRV-ENCT           TO  VL-15V2
           MOVE  VL-15V2                        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-VL-CPR-OPR'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-VL-CPR-OPR            TO  VL-15V2
           MOVE  VL-15V2                        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-VL-MNDT-OPR'         TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-VL-MNDT-OPR           TO  VL-15V2
           MOVE  VL-15V2                        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-VL-PU-EMS-TIT-OPR'   TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-VL-PU-EMS-TIT-OPR     TO  VL-9V8
           MOVE  VL-9V8                         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-VL-APVD-BB-OPR'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-VL-APVD-BB-OPR        TO  VL-15V2
           MOVE  VL-15V2                        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-VL-PU-CPR-TIT-OPR'   TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-VL-PU-CPR-TIT-OPR     TO  VL-9V8
           MOVE  VL-9V8                         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-VL-GRT-BB-OPR'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-VL-GRT-BB-OPR         TO  VL-15V2
           MOVE  VL-15V2                        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-TX-PGTO-OPR-VCLD'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-TX-PGTO-OPR-VCLD      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-DT-CMPS-FUT-NOTA'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-DT-CMPS-FUT-NOTA      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-NR-VRS-REF'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-NR-VRS-REF            TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-VL-EVT-VCLD-VRS'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-VL-EVT-VCLD-VRS       TO  VL-15V2
           MOVE  VL-15V2                        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-DT-EVT-VCLD-VRS'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-DT-EVT-VCLD-VRS       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSC005-TX-OBS-EVT'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSC005-TX-OBS-EVT            TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava o erro no sistema MSG
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                           )
                      CHANNEL   ( NM-CNL                               )
                      FROM      ( IIBKERRO-PRM-ERRO                    )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO          )
                      NOHANDLE
           END-EXEC

           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      * --------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * MSG1-CD-ERRO-TS = 9999
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * MSG1-CD-ERRO-TS = 9998
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      * --------------------------------------*
-INC HLPKP999
      *----------------------------------------
      * 999010-TRATA-ERRO-DB2          SECTION.
      * MSG1-CD-ERRO-TS = 9997
      *----------------------------------------
-INC HLPKP997
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * MSG1-CD-ERRO-TS = 3000
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * MSG1-CD-ERRO-TS = 3001
      * 999006-TRATA-ERRO-CANAL        SECTION.
      * MSG1-CD-ERRO-TS = 3003
      *---------------------------------------*
-INC SGTKP999
      *----------------------------------------
      * MSG1-NTF-MSG                   SECTION.
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
      *---------------------- FIM SGTSC005 -----------------------------