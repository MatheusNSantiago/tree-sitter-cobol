      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSA005
      * ANALISTA..: F0175702 - ADRIANO SEIJI NAKAMURA
      * AUTOR.....: F0175702 - ADRIANO SEIJI NAKAMURA
      * DATA......: 31/08/2017
      * OBJETIVO..: Alteração da Tabela OPR_ETRD_MCP
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 03/02/2017 - F2804421 2016/145195 - Inclusão campo
      *              SGTSA005-TX-PGTO-OPR-VCLD e alteração no tamanho
      *              dos campos SGTSA005-QT-PAPL-TIT-EMS e
      *                         SGTST005-QT-PAPL-TIT-CPR
      *              container operador para gravação no LOG.
      *        26/01/2017 - F8785286 2016/145195 - Inclusão leitura
      *              container operador para gravação no LOG.
      *        10/01/2017 - F0175702 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSA005.

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
       77  CTE-INICIO-SS                 PIC  X(040) VALUE
                    '*** S.S. COMECA AQUI ***'.

       77  CTE-PROG                      PIC  X(017) VALUE
                  '*** SGTSA005 ***'.
       77  CTE-VERS                      PIC  X(006) VALUE 'VRS001'.
       77  SGTS014G                      PIC  X(008) VALUE 'SGTS014G'.

      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B

      * Book de para acionamento do MSG1
-INC IIBKERRO

      * Book dos dados do usuário
-INC IIBKOPRD

      * Book para captura de erro via MSG1
-INC MSGKWMG1
      * Book para captura de abend cics da bloco código MSGKP999
      * Inclui book HLPK0201 para recuperar erros do CICS EIBFN e RESP
-INC HLPKW999

      * Book para tratamento de erro para mensageria
-INC SGTKERR1

      * Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

      * Book deste o programa
       01 DADOS-SGTKA005.
-INC SGTKA005

      * Book HLPKTIME
-INC HLPKTIME

      * Area de trabalho

       01  QT-REG-SELD                      PIC S9(4) COMP-5 VALUE 0.

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      * Àreas auxiliares para monitoração variáveis e display
       01  AUX-CAMPO-TAM11                  PIC  9(11) VALUE 0.

       01  NRO-DEC-AUX                      PIC  9(7)V9999   VALUE 0.
       01  NRO-DEC-AUX-R   REDEFINES        NRO-DEC-AUX.
           03  NRO-DEC-ALFA                 PIC  9(11).

       01  AUX-CAMPO-TAM17                  PIC  9(17)       VALUE 0.

       01  VL-15V2                          PIC ----.---.---.---.--9,99.
       01  VL-4V10                          PIC --.--9,9999999999.
       01  VL-9V8                           PIC ----.---.--9,99999999.

      * Demais áreas auxiliares

       01  DT-SQL-ATU                       PIC  X(10)       VALUE
             '  .  .    '.
       01  DT-SQL-ATU-R REDEFINES DT-SQL-ATU.
           03 DIA                           PIC  9(02).
           03 FILLER                        PIC  X(01).
           03 MES                           PIC  9(02).
           03 FILLER                        PIC  X(01).
           03 ANO                           PIC  9(04).

       01  TS-SQL-ATU                       PIC  X(26)       VALUE ' '.

      * Indicadores de nulidades

       01  CD-BCO-CRD-VL-OPR-N              PIC S9(4) COMP-5 VALUE 0.
       01  CD-DEPE-CRD-VL-OPR-N             PIC S9(4) COMP-5 VALUE 0.
       01  CD-SUB-DEPE-CRD-VL-N             PIC S9(4) COMP-5 VALUE 0.
       01  CD-UOR-BB-CRD-VL-N               PIC S9(4) COMP-5 VALUE 0.
       01  NR-CC-OTR-BCO-N                  PIC S9(4) COMP-5 VALUE 0.
       01  DV-CC-OTR-BCO-N                  PIC S9(4) COMP-5 VALUE 0.
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
       01  DT-CMPS-FUT-NOTA-N               PIC S9(4) COMP-5 VALUE 0.
       01  DT-EMS-TIT-OPR-N                 PIC S9(4) COMP-5 VALUE 0.
       01  DT-EMS-PREF-N                    PIC S9(4) COMP-5 VALUE 0.
       01  DT-EVT-VCLD-VRS-N                PIC S9(4) COMP-5 VALUE 0.
       01  NR-VRS-REF-N                     PIC S9(4) COMP-5 VALUE 0.
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
       01  VL-EVT-VCLD-VRS-N                PIC S9(4) COMP-5 VALUE 0.
       01  VL-GRT-BB-OPR-N                  PIC S9(4) COMP-5 VALUE 0.
       01  TX-OBS-EVT-N                     PIC S9(4) COMP-5 VALUE 0.

      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG           PIC  9(10)       VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG          PIC  9(11)       VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG          PIC  9(5)        VALUE 0.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-INICIO-SS                     PIC  X(040)     VALUE
                  '*** S.S. COMECA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL SECTION.
      *-----------------------------------------------------------------

           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

           MOVE 0001                   TO  MSG1-CD-SEQL-ERRO
      * Habilita tratamento automatico de Erros DB2
-INC HLPKE997
      * Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDA-PARAMETRO

           PERFORM 200000-ALTERAR

           MOVE 'Alteração efetuada com sucesso.'
                                       TO  SGTSA005-TX-RTN-PRCT
           PERFORM 300000-POSTA-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO  MSG1-CD-SEQL-ERRO

           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL )
           END-EXEC

           SET  SEM-ERRO               TO TRUE

           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO  MSG1-CD-SEQL-ERRO

      *    Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

           SET DEL-CONTAINER           TO TRUE

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER ('PRM-ERRO')
                     RESP      (MSG1-RESP1)
                     RESP2     (MSG1-RESP2)
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE

           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                         TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROS                        TO  QT-CCT-LIDO

           MOVE LENGTH OF SGTSA005-RQSC      TO  QT-CCT-EPRO

           EXEC CICS GET
              CONTAINER ( 'SGTSA005-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA         TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *Configura dados para leitura do container de requisição

           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container

           EXEC CICS GET
              CONTAINER ( 'SGTSA005-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSA005-RQSC )
              INTO      ( SGTSA005-RQSC           )
           END-EXEC

      * Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDA-PARAMETRO SECTION.
      *-----------------------------------------------------------------

           PERFORM 150000-VALIDA-CAMPOS

           PERFORM 160000-TRATA-NULIDADE

           PERFORM 170001-VALIDA-CTRA-TIT-VL-MNTR
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALIDA-CAMPOS SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                     TO  MSG1-CD-SEQL-ERRO

           PERFORM 151000-BUSCA-TS-DT

           MOVE CORR HLPKE001-TS-FMTD    TO  DT-SQL-ATU-R
           MOVE HLPKE001-TS-FMTD         TO  TS-SQL-ATU

           IF  SGTSA005-CD-CTRA-TIT-VL   EQUAL ZEROS
               SET  ERRO-PROGRAMADO      TO  TRUE
               MOVE 002                  TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'     TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA005-CD-OPR-ETRD-MCP  IS NOT NUMERIC
               SET  ERRO-PROGRAMADO      TO  TRUE
               MOVE 003                  TO  MSG1-CD-ERRO-TS
               MOVE 'Código Operação Estruturado Mercado de Capitais não
      -        ' numérico.'              TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA005-CD-OPR-ETRD-MCP  EQUAL ZEROS
               SET  ERRO-PROGRAMADO      TO  TRUE
               MOVE 005                  TO  MSG1-CD-ERRO-TS
               MOVE 'Código Operação Estruturado Mercado de Capitais não
      -        ' informado.'             TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA005-NR-VRS-OPR-ETRD  EQUAL ZEROS
               SET  ERRO-PROGRAMADO      TO  TRUE
               MOVE 007                  TO  MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada não inform
      -             'ado.'               TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA005-CD-UOR-DTTR-OPR  EQUAL ZEROS
               SET  ERRO-PROGRAMADO      TO  TRUE
               MOVE 012                  TO  MSG1-CD-ERRO-TS
               MOVE 'Código da UOR detentora da operação inválido.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA005-CD-PRD           EQUAL ZEROS
               SET  ERRO-PROGRAMADO      TO  TRUE
               MOVE 068                  TO  MSG1-CD-ERRO-TS
               MOVE 'Código do produto não informado.'
                                         TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA005-CD-MDLD          EQUAL ZEROS
               SET  ERRO-PROGRAMADO      TO  TRUE
               MOVE 069                  TO  MSG1-CD-ERRO-TS
               MOVE 'Código da modalidade do produto não informado.'
                                         TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA005-DT-CPR-OPR       EQUAL SPACES
               SET  ERRO-PROGRAMADO      TO  TRUE
               MOVE 237                  TO  MSG1-CD-ERRO-TS
               MOVE 'Data de compra da operação não informada.'
                                         TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA005-DT-VNCT-OPR      EQUAL SPACES
               SET  ERRO-PROGRAMADO      TO  TRUE
               MOVE 239                  TO  MSG1-CD-ERRO-TS
               MOVE 'Data de vencimento da operação não informada.'
                                         TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA005-CD-TIP-VRS-OPR   EQUAL ZEROS
               SET  ERRO-PROGRAMADO      TO  TRUE
               MOVE 241                  TO  MSG1-CD-ERRO-TS
               MOVE 'Código do tipo de versão da operação não informado.
      -             ''                   TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA005-TS-ULT-ATL       EQUAL 'CURRENT' OR SPACES
               MOVE TS-SQL-ATU           TO  SGTSA005-TS-ULT-ATL
           END-IF

           IF  SGTSA005-DT-CPR-OPR       EQUAL 'CURRENT'
               MOVE DT-SQL-ATU           TO  SGTSA005-DT-CPR-OPR
           END-IF

           IF  SGTSA005-DT-VNCT-OPR      EQUAL 'CURRENT'
               MOVE DT-SQL-ATU           TO  SGTSA005-DT-VNCT-OPR
           END-IF

           IF  SGTSA005-DT-LQDC-OPR-DRIV EQUAL 'CURRENT'
               MOVE DT-SQL-ATU           TO  SGTSA005-DT-LQDC-OPR-DRIV
           END-IF

           IF  SGTSA005-DT-INC-VGC-VRS-OPR EQUAL 'CURRENT'
               MOVE DT-SQL-ATU           TO  SGTSA005-DT-INC-VGC-VRS-OPR
           END-IF

           IF  SGTSA005-DT-MNDT-OPR      EQUAL 'CURRENT'
               MOVE DT-SQL-ATU           TO  SGTSA005-DT-MNDT-OPR
           END-IF

           IF  SGTSA005-DT-EMS-TIT-OPR   EQUAL 'CURRENT'
               MOVE DT-SQL-ATU           TO  SGTSA005-DT-EMS-TIT-OPR
           END-IF

           IF  SGTSA005-DT-EMS-PREF       EQUAL 'CURRENT'
               MOVE DT-SQL-ATU           TO  SGTSA005-DT-EMS-PREF
           END-IF
           .
       150000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       151000-BUSCA-TS-DT SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO  MSG1-CD-SEQL-ERRO

           CALL SBTIMEDT            USING  HLPKE001-CD-FUC
                                           HLPKE001-AREA-TRB
                                           HLPKE001-FMT-TS
                                           HLPKE001-TAM-TS
                                           HLPKE001-TS-FMTD

           IF  RETURN-CODE      NOT EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 050                TO  MSG1-CD-ERRO-TS
               MOVE 'Erro ao obter data e hora atual.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       151000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       160000-TRATA-NULIDADE SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO  MSG1-CD-SEQL-ERRO

           IF  SGTSA005-CD-BCO-CRD-VL-OPR  EQUAL ZEROS
               MOVE -1                 TO  CD-BCO-CRD-VL-OPR-N
           END-IF

           IF  SGTSA005-CD-DEPE-CRD-VL-OPR EQUAL ZEROS
               MOVE -1                 TO  CD-DEPE-CRD-VL-OPR-N
           END-IF

           IF  SGTSA005-CD-SUB-DEPE-CRD-VL EQUAL ZEROS
               MOVE -1                 TO  CD-SUB-DEPE-CRD-VL-N
           END-IF

           IF  SGTSA005-CD-UOR-BB-CRD-VL   EQUAL ZEROS
               MOVE -1                 TO  CD-UOR-BB-CRD-VL-N
           END-IF

           IF  SGTSA005-NR-CC-OTR-BCO  EQUAL ZEROS
               MOVE -1                 TO  NR-CC-OTR-BCO-N
           END-IF

           IF  SGTSA005-DV-CC-OTR-BCO  EQUAL SPACES
               MOVE -1                 TO  DV-CC-OTR-BCO-N
           END-IF

           IF  SGTSA005-DT-LQDC-OPR-DRIV   EQUAL SPACES
               MOVE -1                 TO  DT-LQDC-OPR-DRIV-N
           END-IF

           IF  SGTSA005-CD-TIT-OPR-ETRD    EQUAL SPACES
               MOVE -1                 TO  CD-TIT-OPR-ETRD-N
           END-IF

           IF  SGTSA005-NR-DVS-TIT     EQUAL ZEROS
               MOVE -1                 TO  NR-DVS-TIT-N
           END-IF

           IF  SGTSA005-NR-UNCO-CTR-OPR    EQUAL ZEROS
               MOVE -1                 TO  NR-UNCO-CTR-OPR-N
           END-IF

           IF  SGTSA005-CD-TIP-OPR-ETRD    EQUAL ZEROS
               MOVE -1                 TO  CD-TIP-OPR-ETRD-N
           END-IF

           IF  SGTSA005-CD-ITC-CVM-OPR EQUAL ZEROS
               MOVE -1                 TO  CD-ITC-CVM-OPR-N
           END-IF

           IF  SGTSA005-CD-CTGR-TVM    EQUAL ZEROS
               MOVE -1                 TO  CD-CTGR-TVM-N
           END-IF

           IF  SGTSA005-CD-ESPC-DBTR-OPR   EQUAL ZEROS
               MOVE -1                 TO  CD-ESPC-DBTR-OPR-N
           END-IF

           IF  SGTSA005-CD-TIP-COTA-TIT    EQUAL ZEROS
               MOVE -1                 TO  CD-TIP-COTA-TIT-N
           END-IF

           IF  SGTSA005-NR-VRS-TIP-COTA    EQUAL ZEROS
               MOVE -1                 TO  NR-VRS-TIP-COTA-N
           END-IF

           IF  SGTSA005-CD-TIP-MC-CPR  EQUAL ZEROS
               MOVE -1                 TO  CD-TIP-MC-CPR-N
           END-IF

           IF  SGTSA005-CD-RSCO-OPR    EQUAL SPACES
               MOVE -1                 TO  CD-RSCO-OPR-N
           END-IF

           IF  SGTSA005-CD-CMRA-OPR-ETRD   EQUAL ZEROS
               MOVE -1                 TO  CD-CMRA-OPR-ETRD-N
           END-IF

           IF  SGTSA005-NR-EMS-TIT-OPR EQUAL ZEROS
               MOVE -1                 TO  NR-EMS-TIT-OPR-N
           END-IF

           IF  SGTSA005-NR-SRE-EMS-TIT-OPR    EQUAL ZEROS
               MOVE -1                 TO  NR-SRE-EMS-TIT-OPR-N
           END-IF

           IF  SGTSA005-DT-INC-VGC-VRS-OPR    EQUAL SPACES
               MOVE -1                 TO  DT-INC-VGC-VRS-OPR-N
           END-IF

           IF  SGTSA005-DT-MNDT-OPR    EQUAL SPACES
               MOVE -1                 TO  DT-MNDT-OPR-N
           END-IF

           IF  SGTSA005-DT-EMS-TIT-OPR EQUAL SPACES
               MOVE -1                 TO  DT-EMS-TIT-OPR-N
           END-IF

           IF  SGTSA005-DT-EMS-PREF    EQUAL SPACES
               MOVE -1                 TO  DT-EMS-PREF-N
           END-IF

           IF  SGTSA005-QT-DD-PZ-VLD-PREF       EQUAL ZEROS
               MOVE -1                 TO  QT-DD-PZ-VLD-PREF-N
           END-IF

           IF  SGTSA005-QT-MM-CARE-JUR EQUAL ZEROS
               MOVE -1                 TO  QT-MM-CARE-JUR-N
           END-IF

           IF  SGTSA005-QT-MM-CARE-OPR EQUAL ZEROS
               MOVE -1                 TO  QT-MM-CARE-OPR-N
           END-IF

           IF  SGTSA005-QT-MM-CARE-RSGT  EQUAL ZEROS
               MOVE -1                 TO  QT-MM-CARE-RSGT-N
           END-IF

           IF  SGTSA005-QT-MM-CARE-AMTZ  EQUAL ZEROS
               MOVE -1                 TO  QT-MM-CARE-AMTZ-N
           END-IF

           IF  SGTSA005-QT-MM-CARE-AQS EQUAL ZEROS
               MOVE -1                 TO  QT-MM-CARE-AQS-N
           END-IF

           IF  SGTSA005-PC-TARF-GRT-TAXA  EQUAL ZEROS
               MOVE -1                 TO  PC-TARF-GRT-TAXA-N
           END-IF

           IF  SGTSA005-VL-PRV-ENCT    EQUAL ZEROS
               MOVE -1                 TO  VL-PRV-ENCT-N
           END-IF

           IF  SGTSA005-VL-MNDT-OPR    EQUAL ZEROS
               MOVE -1                 TO  VL-MNDT-OPR-N
           END-IF

           IF  SGTSA005-VL-APVD-BB-OPR EQUAL ZEROS
               MOVE -1                 TO  VL-APVD-BB-OPR-N
           END-IF

           IF  SGTSA005-VL-GRT-BB-OPR EQUAL ZEROS
               MOVE -1                 TO  VL-GRT-BB-OPR-N
           END-IF

           IF  SGTSA005-DT-CMPS-FUT-NOTA EQUAL SPACES
               MOVE -1                 TO  DT-CMPS-FUT-NOTA-N
           END-IF

           IF  SGTSA005-DT-EVT-VCLD-VRS  EQUAL SPACES
               MOVE -1                 TO  DT-EVT-VCLD-VRS-N
           END-IF

           IF  SGTSA005-NR-VRS-REF    EQUAL ZEROS
               MOVE -1                 TO  NR-VRS-REF-N
           END-IF

           IF  SGTSA005-TX-OBS-EVT    EQUAL SPACES
               MOVE -1                 TO  TX-OBS-EVT-N
           END-IF

           IF  SGTSA005-VL-EVT-VCLD-VRS EQUAL ZEROS
               MOVE -1                 TO  VL-EVT-VCLD-VRS-N
           END-IF
           .
       160000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170001-VALIDA-CTRA-TIT-VL-MNTR SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO  QT-REG-SELD

           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.CTRA_TIT_VL_MNTR
               WHERE CD_CTRA_TIT_VL     = :SGTSA005-CD-CTRA-TIT-VL
           END-EXEC

           IF  QT-REG-SELD = 0
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 247                TO  MSG1-CD-ERRO-TS
               MOVE 'Carteira de título e valores mobiliários inexistent
      -             'e.'               TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       170001-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-ALTERAR SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO  MSG1-CD-SEQL-ERRO

           EXEC SQL
             UPDATE DB2SGT.OPR_ETRD_MCP
             SET CD_USU_ULT_ATL     =  :SGTSA005-CD-USU-ULT-ATL
               , TS_ULT_ATL         =  :SGTSA005-TS-ULT-ATL
               , CD_BCO_CRD_VL_OPR  =  :SGTSA005-CD-BCO-CRD-VL-OPR
                                       :CD-BCO-CRD-VL-OPR-N
               , CD_DEPE_CRD_VL_OPR =  :SGTSA005-CD-DEPE-CRD-VL-OPR
                                       :CD-DEPE-CRD-VL-OPR-N
               , CD_SUB_DEPE_CRD_VL =  :SGTSA005-CD-SUB-DEPE-CRD-VL
                                       :CD-SUB-DEPE-CRD-VL-N
               , CD_UOR_DTTR_OPR    =  :SGTSA005-CD-UOR-DTTR-OPR
               , CD_PRD             =  :SGTSA005-CD-PRD
               , CD_MDLD            =  :SGTSA005-CD-MDLD
               , CD_UOR_BB_CRD_VL   =  :SGTSA005-CD-UOR-BB-CRD-VL
                                       :CD-UOR-BB-CRD-VL-N
               , CD_UOR_BB          =  :SGTSA005-CD-UOR-BB
               , NR_CC_BB           =  :SGTSA005-NR-CC-BB
               , IN_RMNC_VRV        =  :SGTSA005-IN-RMNC-VRV
               , NR_CC_OTR_BCO      =  :SGTSA005-NR-CC-OTR-BCO
                                       :NR-CC-OTR-BCO-N
               , DV_CC_OTR_BCO      =  :SGTSA005-DV-CC-OTR-BCO
                                       :DV-CC-OTR-BCO-N
               , QT_PAPL_TIT_EMS    =  :SGTSA005-QT-PAPL-TIT-EMS
               , QT_PAPL_TIT_CPR    =  :SGTSA005-QT-PAPL-TIT-CPR
               , DT_CPR_OPR         =  :SGTSA005-DT-CPR-OPR
               , DT_VNCT_OPR        =  :SGTSA005-DT-VNCT-OPR
               , DT_LQDC_OPR_DRIV   =  :SGTSA005-DT-LQDC-OPR-DRIV
                                       :DT-LQDC-OPR-DRIV-N
               , CD_TIT_OPR_ETRD    =  :SGTSA005-CD-TIT-OPR-ETRD
                                       :CD-TIT-OPR-ETRD-N
               , NR_DVS_TIT         =  :SGTSA005-NR-DVS-TIT
                                       :NR-DVS-TIT-N
               , NR_UNCO_CTR_OPR    =  :SGTSA005-NR-UNCO-CTR-OPR
                                       :NR-UNCO-CTR-OPR-N
               , CD_TIP_OPR_ETRD    =  :SGTSA005-CD-TIP-OPR-ETRD
                                       :CD-TIP-OPR-ETRD-N
               , IN_OPR_ETRD_SPRO   =  :SGTSA005-IN-OPR-ETRD-SPRO
               , IN_OPR_ETRD_VRJ    =  :SGTSA005-IN-OPR-ETRD-VRJ
               , IN_OPR_ETRD_DSCP   =  :SGTSA005-IN-OPR-ETRD-DSCP
               , IN_REG_OPR_BB      =  :SGTSA005-IN-REG-OPR-BB
               , IN_OPR_N_NEGO      =  :SGTSA005-IN-OPR-N-NEGO
               , IN_EXPO_VCLD_GRT   =  :SGTSA005-IN-EXPO-VCLD-GRT
               , CD_ITC_CVM_OPR     =  :SGTSA005-CD-ITC-CVM-OPR
                                       :CD-ITC-CVM-OPR-N
               , CD_CTGR_TVM        =  :SGTSA005-CD-CTGR-TVM
                                       :CD-CTGR-TVM-N
               , CD_ESPC_DBTR_OPR   =  :SGTSA005-CD-ESPC-DBTR-OPR
                                       :CD-ESPC-DBTR-OPR-N
               , CD_TIP_COTA_TIT    =  :SGTSA005-CD-TIP-COTA-TIT
                                       :CD-TIP-COTA-TIT-N
               , NR_VRS_TIP_COTA    =  :SGTSA005-NR-VRS-TIP-COTA
                                       :NR-VRS-TIP-COTA-N
               , CD_TIP_MC_CPR      =  :SGTSA005-CD-TIP-MC-CPR
                                       :CD-TIP-MC-CPR-N
               , CD_RSCO_OPR        =  :SGTSA005-CD-RSCO-OPR
                                       :CD-RSCO-OPR-N
               , CD_CMRA_OPR_ETRD   =  :SGTSA005-CD-CMRA-OPR-ETRD
                                       :CD-CMRA-OPR-ETRD-N
               , CD_TIP_VRS_OPR     =  :SGTSA005-CD-TIP-VRS-OPR
               , NR_EMS_TIT_OPR     =  :SGTSA005-NR-EMS-TIT-OPR
                                       :NR-EMS-TIT-OPR-N
               , NR_SRE_EMS_TIT_OPR =  :SGTSA005-NR-SRE-EMS-TIT-OPR
                                       :NR-SRE-EMS-TIT-OPR-N
               , DT_INC_VGC_VRS_OPR =  :SGTSA005-DT-INC-VGC-VRS-OPR
                                       :DT-INC-VGC-VRS-OPR-N
               , DT_MNDT_OPR        =  :SGTSA005-DT-MNDT-OPR
                                       :DT-MNDT-OPR-N
               , DT_EMS_TIT_OPR     =  :SGTSA005-DT-EMS-TIT-OPR
                                       :DT-EMS-TIT-OPR-N
               , DT_EMS_PREF        =  :SGTSA005-DT-EMS-PREF
                                       :DT-EMS-PREF-N
               , DT_CMPS_FUT_NOTA   =  :SGTSA005-DT-CMPS-FUT-NOTA
                                       :DT-CMPS-FUT-NOTA-N
               , DT_EVT_VCLD_VRS    =  :SGTSA005-DT-EVT-VCLD-VRS
                                       :DT-EVT-VCLD-VRS-N
               , NR_VRS_REF         =  :SGTSA005-NR-VRS-REF
                                       :NR-VRS-REF-N
               , QT_DD_PZ_VLD_PREF  =  :SGTSA005-QT-DD-PZ-VLD-PREF
                                       :QT-DD-PZ-VLD-PREF-N
               , QT_MM_CARE_JUR     =  :SGTSA005-QT-MM-CARE-JUR
                                       :QT-MM-CARE-JUR-N
               , QT_MM_CARE_OPR     =  :SGTSA005-QT-MM-CARE-OPR
                                       :QT-MM-CARE-OPR-N
               , QT_MM_CARE_RSGT    =  :SGTSA005-QT-MM-CARE-RSGT
                                       :QT-MM-CARE-RSGT-N
               , QT_MM_CARE_AMTZ    =  :SGTSA005-QT-MM-CARE-AMTZ
                                       :QT-MM-CARE-AMTZ-N
               , QT_MM_CARE_AQS     =  :SGTSA005-QT-MM-CARE-AQS
                                       :QT-MM-CARE-AQS-N
               , PC_TARF_GRT_TAXA   =  :SGTSA005-PC-TARF-GRT-TAXA
                                       :PC-TARF-GRT-TAXA-N
               , VL_PRV_ENCT        =  :SGTSA005-VL-PRV-ENCT
                                       :VL-PRV-ENCT-N
               , VL_CPR_OPR         =  :SGTSA005-VL-CPR-OPR
               , VL_MNDT_OPR        =  :SGTSA005-VL-MNDT-OPR
                                       :VL-MNDT-OPR-N
               , VL_PU_EMS_TIT_OPR  =  :SGTSA005-VL-PU-EMS-TIT-OPR
               , VL_APVD_BB_OPR     =  :SGTSA005-VL-APVD-BB-OPR
                                       :VL-APVD-BB-OPR-N
               , VL_PU_CPR_TIT_OPR  =  :SGTSA005-VL-PU-CPR-TIT-OPR
               , VL_GRT_BB_OPR      =  :SGTSA005-VL-GRT-BB-OPR
                                       :VL-GRT-BB-OPR-N
               , VL_EVT_VCLD_VRS    =  :SGTSA005-VL-EVT-VCLD-VRS
                                       :VL-EVT-VCLD-VRS-N
               , TX_OBS_EVT         =  :SGTSA005-TX-OBS-EVT
                                       :TX-OBS-EVT-N
               , TX_PGTO_OPR_VCLD   =  :SGTSA005-TX-PGTO-OPR-VCLD
                WHERE CD_CTRA_TIT_VL   =  :SGTSA005-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP  =  :SGTSA005-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD  =  :SGTSA005-NR-VRS-OPR-ETRD
           END-EXEC

           IF  SQLCODE EQUAL +100

               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 100                TO  MSG1-CD-ERRO-TS
               MOVE 'Registro inexistente.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           INITIALIZE DADOS-LOG

           MOVE 'A'                    TO  SGTS014G-CD-TIP-ACAO

           PERFORM 980102-GRAVA-LOG
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO  MSG1-CD-SEQL-ERRO

           SET PUT-CONTAINER           TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container

           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
              CONTAINER ( 'SGTSA005-RPST'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSA005-RPST )
              FROM      ( SGTSA005-RPST           )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE

           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980102-GRAVA-LOG SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'OPR_ETRD_MCP      '   TO  SGTS014G-NM-ENT

           IF  EXECUCAO-ONLINE
               PERFORM 980103-LER-DADOS-OPRD
               MOVE  KOPRD-CD-OPRD         TO  SGTS014G-CD-CHV-USU
           ELSE
               MOVE 'SYSTEM'           TO  SGTS014G-CD-CHV-USU
           END-IF

           MOVE 'SGTSA005'                TO  SGTS014G-NM-PGM
           MOVE SGTSA005-CD-CTRA-TIT-VL   TO  CD-CTRA-TIT-VL-LOG
           MOVE SGTSA005-CD-OPR-ETRD-MCP  TO  CD-OPR-ETRD-MCP-LOG
           MOVE SGTSA005-NR-VRS-OPR-ETRD  TO  NR-VRS-OPR-ETRD-LOG

           STRING CD-CTRA-TIT-VL-LOG
             ';'    CD-OPR-ETRD-MCP-LOG
             ';'    NR-VRS-OPR-ETRD-LOG
                    DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR
           END-STRING

           MOVE LENGTH OF DADOS-LOG    TO  EIBCALEN

           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM    NOT  EQUAL ZEROS
               SET  ERRO-PROGRAMA-ACIONADO        TO  TRUE
               MOVE 112                    TO  MSG1-CD-ERRO-TS
               MOVE 'Erro na gravação das informações do Log.'
                                           TO  MSG1-TX-ERRO-TS
               MOVE SGTS014G               TO  MSG1-NM-PGM-ACND
               MOVE  SGTS014G-CD-RTN-PGM   TO  MSG1-CD-RTN-PGM-ACND
               MOVE SGTS014G-MSG-ERRO-PGM  TO  MSG1-TX-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       980102-SAI.
           EXIT.

      *----------------------------------------------------------------*
       980103-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE PRM-INF-OPRD

           SET  GET-CONTAINER          TO TRUE

           EXEC CICS GET
                CONTAINER ( 'PRM-INF-OPRD'         )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

           SET  SEM-ERRO               TO TRUE
           .
       980103-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------

           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      * Inicializa os campos de erro
           IF MSG1-CD-ERRO-TS EQUAL ZEROS
              MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF
           IF MSG1-TX-ERRO-TS EQUAL SPACES
              MOVE 'Erro não especificado no módulo.'
                                       TO MSG1-TX-ERRO-TS
           END-IF

      *Verifica se o tipo de erro gerado possui tratamento interno
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

      *Adiciona as variáveis de entrada para monitoração
           IF  MNT-VRV
              MOVE 'SGTSA005-CD-CTRA-TIT-VL'    TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-CD-CTRA-TIT-VL      TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-CD-OPR-ETRD-MCP'   TO  MSGKW999-NM-VRV-MNT
              IF  SGTSA005-CD-OPR-ETRD-MCP      IS NUMERIC
                  MOVE SGTSA005-CD-OPR-ETRD-MCP TO  CD-OPR-ETRD-MCP
                  MOVE CD-OPR-ETRD-MCP-TX       TO  MSGKW999-VL-VRV-MNT
              ELSE
                  MOVE 'Código da operação estruturada do mercado de cap
      -                'itais não númerico.'    TO  MSGKW999-VL-VRV-MNT
              END-IF
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-NR-VRS-OPR-ETRD'   TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-NR-VRS-OPR-ETRD     TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-CD-USU-ULT-ATL'    TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-CD-USU-ULT-ATL      TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-TS-ULT-ATL'        TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-TS-ULT-ATL          TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-CD-BCO-CRD-VL-OPR' TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-CD-BCO-CRD-VL-OPR   TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-CD-DEPE-CRD-VL-OPR' TO MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-CD-DEPE-CRD-VL-OPR  TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-CD-SUB-DEPE-CRD-VL' TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-CD-SUB-DEPE-CRD-VL  TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-CD-UOR-DTTR-OPR'   TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-CD-UOR-DTTR-OPR     TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-CD-PRD'            TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-CD-PRD              TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-CD-MDLD'           TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-CD-MDLD             TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-CD-UOR-BB-CRD-VL'  TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-CD-UOR-BB-CRD-VL    TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-CD-UOR-BB'         TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-CD-UOR-BB           TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-NR-CC-BB'          TO  MSGKW999-NM-VRV-MNT
              IF  SGTSA005-NR-CC-BB             IS NUMERIC
                  MOVE SGTSA005-NR-CC-BB        TO  AUX-CAMPO-TAM11
                  MOVE AUX-CAMPO-TAM11          TO  MSGKW999-VL-VRV-MNT
              ELSE
                  MOVE 'Número Conta Corrente Banco do Brasil não numéri
      -                'co.'                    TO  MSGKW999-VL-VRV-MNT
              END-IF
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-IN-RMNC-VRV'       TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-IN-RMNC-VRV         TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-NR-CC-OTR-BCO'     TO  MSGKW999-NM-VRV-MNT
              IF  SGTSA005-NR-CC-OTR-BCO IS NUMERIC
                  MOVE SGTSA005-NR-CC-OTR-BCO   TO  AUX-CAMPO-TAM11
                  MOVE AUX-CAMPO-TAM11          TO  MSGKW999-VL-VRV-MNT
              ELSE
                  MOVE 'Número da Conta Corrente de Outros Bancos não nú
      -               'mérico.'                 TO  MSGKW999-VL-VRV-MNT
              END-IF
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-DV-CC-OTR-BCO'     TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-DV-CC-OTR-BCO       TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-QT-PAPL-TIT-EMS'   TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-QT-PAPL-TIT-EMS     TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-QT-PAPL-TIT-CPR'   TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-QT-PAPL-TIT-CPR     TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-DT-CPR-OPR'        TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-DT-CPR-OPR          TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-DT-VNCT-OPR'       TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-DT-VNCT-OPR         TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-DT-LQDC-OPR-DRIV'  TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-DT-LQDC-OPR-DRIV    TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-CD-TIT-OPR-ETRD'   TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-CD-TIT-OPR-ETRD     TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-NR-DVS-TIT'        TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-NR-DVS-TIT          TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-NR-UNCO-CTR-OPR'   TO  MSGKW999-NM-VRV-MNT
              IF  SGTSA005-NR-UNCO-CTR-OPR      IS NUMERIC
                  MOVE SGTSA005-NR-UNCO-CTR-OPR TO AUX-CAMPO-TAM17
                  MOVE AUX-CAMPO-TAM17          TO MSGKW999-VL-VRV-MNT
              ELSE
                  MOVE 'Número Único Contrato Operação não numérico.'
                                                TO  MSGKW999-VL-VRV-MNT
              END-IF
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-CD-TIP-OPR-ETRD'   TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-CD-TIP-OPR-ETRD     TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-IN-OPR-ETRD-SPRO'  TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-IN-OPR-ETRD-SPRO    TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-IN-OPR-ETRD-VRJ'   TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-IN-OPR-ETRD-VRJ     TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-IN-OPR-ETRD-DSCP'  TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-IN-OPR-ETRD-DSCP    TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-IN-REG-OPR-BB'     TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-IN-REG-OPR-BB       TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-IN-OPR-N-NEGO'     TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-IN-OPR-N-NEGO       TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-IN-EXPO-VCLD-GRT'  TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-IN-EXPO-VCLD-GRT    TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-CD-ITC-CVM-OPR'    TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-CD-ITC-CVM-OPR      TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-CD-CTGR-TVM'       TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-CD-CTGR-TVM         TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-CD-ESPC-DBTR-OPR'  TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-CD-ESPC-DBTR-OPR    TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-CD-TIP-COTA-TIT'   TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-CD-TIP-COTA-TIT     TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-NR-VRS-TIP-COTA'   TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-NR-VRS-TIP-COTA     TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-CD-TIP-MC-CPR'     TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-CD-TIP-MC-CPR       TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-CD-RSCO-OPR'       TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-CD-RSCO-OPR         TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-CD-CMRA-OPR-ETRD'  TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-CD-CMRA-OPR-ETRD    TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-CD-TIP-VRS-OPR'    TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-CD-TIP-VRS-OPR      TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-NR-EMS-TIT-OPR'    TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-NR-EMS-TIT-OPR      TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-NR-SRE-EMS-TIT-OPR' TO MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-NR-SRE-EMS-TIT-OPR  TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-DT-INC-VGC-VRS-OPR' TO MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-DT-INC-VGC-VRS-OPR  TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-DT-MNDT-OPR'       TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-DT-MNDT-OPR         TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-DT-EMS-TIT-OPR'    TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-DT-EMS-TIT-OPR      TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-DT-EMS-PREF'        TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-DT-EMS-PREF          TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-QT-DD-PZ-VLD-PREF'  TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-QT-DD-PZ-VLD-PREF    TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-QT-MM-CARE-JUR'    TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-QT-MM-CARE-JUR      TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-QT-MM-CARE-OPR'    TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-QT-MM-CARE-OPR      TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-QT-MM-CARE-RSGT'   TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-QT-MM-CARE-RSGT     TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-QT-MM-CARE-AMTZ'   TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-QT-MM-CARE-AMTZ     TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-QT-MM-CARE-AQS'    TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-QT-MM-CARE-AQS      TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-PC-TARF-GRT-TAXA'  TO  MSGKW999-NM-VRV-MNT
              IF  SGTSA005-PC-TARF-GRT-TAXA     IS NUMERIC
                  MOVE SGTSA005-PC-TARF-GRT-TAXA   TO VL-4V10
                  MOVE VL-4V10                  TO  MSGKW999-VL-VRV-MNT
              ELSE
                  MOVE 'Percentual da Tarifa Garantia da Taxa não numéri
      -                'co.'                    TO  MSGKW999-VL-VRV-MNT
              END-IF
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-VL-PRV-ENCT'       TO  MSGKW999-NM-VRV-MNT
              IF  SGTSA005-VL-PRV-ENCT          IS NUMERIC
                  MOVE SGTSA005-VL-PRV-ENCT     TO  VL-15V2
                  MOVE VL-15V2                  TO  MSGKW999-VL-VRV-MNT
              ELSE
                  MOVE 'Valor da Previsão do Encarteiramento não numéric
      -               'o.'                      TO  MSGKW999-VL-VRV-MNT
              END-IF
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-VL-CPR-OPR'        TO  MSGKW999-NM-VRV-MNT
              IF  SGTSA005-VL-CPR-OPR           IS NUMERIC
                  MOVE SGTSA005-VL-CPR-OPR      TO  VL-15V2
                  MOVE VL-15V2                  TO  MSGKW999-VL-VRV-MNT
              ELSE
                  MOVE 'Valor de Compra da Operação não numérico.'
                                                TO  MSGKW999-VL-VRV-MNT
              END-IF
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-VL-MNDT-OPR'       TO  MSGKW999-NM-VRV-MNT
              IF  SGTSA005-VL-MNDT-OPR          IS NUMERIC
                  MOVE SGTSA005-VL-MNDT-OPR     TO  VL-15V2
                  MOVE VL-15V2                  TO  MSGKW999-VL-VRV-MNT
              ELSE
                  MOVE 'Valor do Mandato Operação não numérico.'
                                                TO  MSGKW999-VL-VRV-MNT
              END-IF
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-VL-PU-EMS-TIT-OPR' TO  MSGKW999-NM-VRV-MNT
              IF  SGTSA005-VL-PU-EMS-TIT-OPR    IS NUMERIC
                  MOVE SGTSA005-VL-PU-EMS-TIT-OPR TO  VL-9V8
                  MOVE VL-9V8                   TO  MSGKW999-VL-VRV-MNT
              ELSE
                  MOVE 'Valor do Preço Unitário de Emissão do Título da
      -                'operação não numérico.' TO  MSGKW999-VL-VRV-MNT
              END-IF
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-VL-APVD-BB-OPR'    TO  MSGKW999-NM-VRV-MNT
              IF  SGTSA005-VL-APVD-BB-OPR       IS NUMERIC
                  MOVE SGTSA005-VL-APVD-BB-OPR  TO  VL-15V2
                  MOVE VL-15V2                  TO  MSGKW999-VL-VRV-MNT
              ELSE
                  MOVE 'Valor Aprovado no BB da Operação não numérico.'
                                                TO  MSGKW999-VL-VRV-MNT
              END-IF
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-VL-PU-CPR-TIT-OPR' TO  MSGKW999-NM-VRV-MNT
              IF  SGTSA005-VL-PU-CPR-TIT-OPR     IS NUMERIC
                  MOVE SGTSA005-VL-PU-CPR-TIT-OPR TO  VL-9V8
                  MOVE VL-9V8                   TO  MSGKW999-VL-VRV-MNT
              ELSE
                  MOVE 'Valor do Preço Unitário de Compra do Título da O
      -                'peração não numérico.'  TO  MSGKW999-VL-VRV-MNT
              END-IF
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-VL-GRT-BB-OPR'     TO  MSGKW999-NM-VRV-MNT
              IF  SGTSA005-VL-GRT-BB-OPR        IS NUMERIC
                  MOVE SGTSA005-VL-GRT-BB-OPR   TO  VL-15V2
                  MOVE VL-15V2                  TO  MSGKW999-VL-VRV-MNT
              ELSE
                  MOVE 'Valor da Garantia BB na Operação não numérico.'
                                                TO  MSGKW999-VL-VRV-MNT
              END-IF
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-TX-PGTO-OPR-VCLD'  TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-TX-PGTO-OPR-VCLD    TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-DT-CMPS-FUT-NOTA'  TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-DT-CMPS-FUT-NOTA    TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-DT-EVT-VCLD-VRS'   TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-DT-EVT-VCLD-VRS     TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-NR-VRS-REF'        TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-NR-VRS-REF          TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-TX-OBS-EVT'        TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSA005-TX-OBS-EVT          TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSA005-VL-EVT-VCLD-VRS'   TO  MSGKW999-NM-VRV-MNT
              IF  SGTSA005-VL-EVT-VCLD-VRS      IS NUMERIC
                  MOVE SGTSA005-VL-EVT-VCLD-VRS TO  VL-15V2
                  MOVE VL-15V2                  TO  MSGKW999-VL-VRV-MNT
              ELSE
                  MOVE 'Valor do Evento Vinculado Versão não numérico.'
                                                TO  MSGKW999-VL-VRV-MNT
              END-IF
              PERFORM 999003-ADICIONA-VRV-MNT
           END-IF

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG

           MOVE MSG1-CD-ERRO-TS              TO  IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS              TO  IIBKERRO-TX-ERRO

           EXEC CICS PUT
              CONTAINER ( 'PRM-ERRO'                  )
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
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.