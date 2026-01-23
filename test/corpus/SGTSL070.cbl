      CBL ARITH(EXTEND)
      * ============================================================== *
      * ============================================================== *
      * -------------       S I S T E M A     S G T     -------------- *
      * -------------        Sistema de Gestão TVM      -------------- *
      * -------------------------------------------------------------- *
      * PROGRAMA..: SGTSD005                                           *
      * ANALISTA..: ALESSANDER TAVARES DE PINHO                        *
      * AUTOR.....: ALESSANDER TAVARES DE PINHO                        *
      * CHAVE.....: C1077250                                           *
      * DATA......: 11.07.2015                                         *
      * OBJETIVO..: Consultar dados da Operacao Estruturada            *
      * -------------------------------------------------------------- *
      * SERVICO/VRS/OPERACAO/VRS : XXXXXXXXX / 0001 / 01241558 / 0001  *
      * -------------------------------------------------------------- *
      * VRS003 14.02.2019 F8785286 - Somente recompilação por alteração
      *           no book SGTST006.
      * VRS002 13.03.2018 Christine  - excluir campos da area de retorno
      *                                do módulo SGTST005 e movimentar
      *                                zeros no lugar deles
      * VRS001 13.08.15 - ALESSANDER - BUSCAR CLIENTE DA OPERACAO      *
      *        11.07.15 - ALESSANDER - IMPLANTACAO                     *
      * ============================================================== *
      *
      ******************************************************************
       IDENTIFICATION                  DIVISION.
      ******************************************************************
      *
       PROGRAM-ID.   SGTSD005.
      *
      ******************************************************************
       ENVIRONMENT                     DIVISION.
      ******************************************************************
      *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
      ******************************************************************
       DATA                            DIVISION.
      ******************************************************************
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       77  CTE-PROG                    PIC  X(242) VALUE 'SGTSD005'.
       77  CTE-VERS                    PIC  X(006) VALUE 'VRS003'.
       77  SGTST004                    PIC  X(008) VALUE 'SGTST004'.
       77  SGTST005                    PIC  X(008) VALUE 'SGTST005'.
       77  SGTST006                    PIC  X(008) VALUE 'SGTST006'.
       77  SGTST013                    PIC  X(008) VALUE 'SGTST013'.
       77  SGTST023                    PIC  X(008) VALUE 'SGTST023'.
      *
       01 WS-RESP-CICS.
           05 WS-RESP                  PIC 9(08) VALUE 0.
           05 WS-RESP2                 PIC 9(08) VALUE 0.
      *
       01 GD-SQLCODE                   PIC -999.
       01 GDA-TIP-CLI                  PIC 9(004) VALUE ZEROS.
       01 GDA-CLIENTE                  PIC 9(009) VALUE ZEROS.
       01 GDA-CPF-CGC                  PIC 9(014) VALUE ZEROS.
       01 GDA-CNPJ-EMISSOR             PIC 9(014) VALUE ZEROS.
       01 GDA-TS-TEMP                  PIC X(026) VALUE SPACES.
       01 FILLER REDEFINES GDA-TS-TEMP.
          03 GDA-TS-TEMP-FIL           PIC X(012).
          03 GDA-TS-TEMP-CNPJ          PIC 9(014).
      *
       01  SERVICO-IIB-GENERICO.
           03  KIIBG-SRVC-ID-SRVC           PIC  9(09).
           03  KIIBG-SRVC-VRS-SRVC          PIC  9(04).
           03  KIIBG-SRVC-ID-OPR            PIC  9(08).
           03  KIIBG-SRVC-VRS-OPR           PIC  9(04).
           03  KIIBG-NM-PGM-CHAMAR          PIC  X(08).
      
       01  WS-DADOS-RQSC                    PIC  X(90000)  VALUE SPACES.
       01  WS-DADOS-RPST                    PIC  X(90000)  VALUE SPACES.
       01  WS-TAM-DADOS-RQSC                PIC S9(09) COMP VALUE ZEROS.
       01  WS-TAM-DADOS-RPST                PIC S9(09) COMP VALUE ZEROS.
      
       01  WS-NM-CANAL                      PIC  X(16) VALUE 'CANAL-'.
       01  FILLER REDEFINES WS-NM-CANAL.
           03  WS-CANAL                     PIC  X(06).
           03  WS-NM-PGM                    PIC  X(08).
           03  FILLER                       PIC  X(02).
      *
      *----------------------------------------------------------------*
      *-- AUXILIARES
      *----------------------------------------------------------------*
      *
       01  I                           PIC  9(004) VALUE ZEROS.
       01  WIDX                        PIC  9(09)  VALUE ZEROS.
       01  WIND                        PIC  9(09)  VALUE ZEROS.
      *
       01  GDA-SEQL-ERRO               PIC  9(004) VALUE ZEROS.
       01  LNK-RESP                    PIC S9(009) COMP.
       01  GDA-CHANNEL                 PIC  X(016) VALUE SPACES.
       01  IND                         PIC  9(09)  VALUE ZEROS.
      *
      *----------------------------------------------------------------*
      *-- BOOK'S
      *----------------------------------------------------------------*
      *
       01  MCIS0001-DADOS.
       COPY MCIB0001.                                                           0000
      *
       01  PRM-ST004.
       COPY SGTKT004.                                                           0000
      *
       01  PRM-ST005.
       COPY SGTKT005.                                                           0000
      *
       01  PRM-ST006.
       COPY SGTKT006.                                                           0000
      *
       01  PRM-ST013.
       COPY SGTKT013.                                                           0000
      *
       01  PRM-ST023.
       COPY SGTKT023.                                                           0000
      *
       01  KD005-PRM.
          03  KD005-DADOS.
       COPY SGTKD005.                                                           0000
      *
      *
      *--- Book com area de dados da subrotina
      *--- Book com area de erro p/ retorno ao IIB
       COPY SGTKIIBE.                                                           0000
      *
      *--- Book do container de Erro
       COPY IIBKERRO.                                                           0000
      *
      *--- Book do container de Requisição de Serviço
       COPY IIBKSRVC.                                                           0000
      *
       COPY IIBKCHV.                                                            0000
       COPY IIBKOPRD.                                                           0000
      *
      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
      *
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
      *
      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*
      
           PERFORM 100000-GET-MESSAGE
      
           PERFORM 200000-VALIDA-ENTRADA
           PERFORM 300000-BUSCA-OPR-ETRD
           PERFORM 400000-BUSCA-PSC-SDO-OPR
           PERFORM 500000-BUSCA-CLI-OPR-ETRD
           PERFORM 600000-BUSCA-AGRUPAMENTO
           PERFORM 700000-PUT-RESPOSTA
           PERFORM 999000-ENCERRA
      
           .
       000099-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       100000-GET-MESSAGE              SECTION.
      *----------------------------------------------------------------*
      
           EXEC CICS
               GET CONTAINER ( 'SGTSD005-RQSC' )
                   INTO      (  KD005-ENTD    )
           END-EXEC
      
           IF  EIBRESP NOT EQUAL DFHRESP(NORMAL)
               MOVE EIBRESP            TO KERRO-CD-ERRO
               MOVE 001                TO GDA-SEQL-ERRO
               MOVE 'ERRO AO EFETUAR GET NO CONTAINER SGTKD005-RQSC'
                                       TO KERRO-TX-ERRO
               PERFORM 990000-ERRO
           END-IF
      
           .
       100099-GET-MESSAGE.
           EXIT.
      
      *----------------------------------------------------------------*
       200000-VALIDA-ENTRADA           SECTION.
      *----------------------------------------------------------------*
      
           IF KD005-DT-PSC             EQUAL SPACES
              MOVE 001                 TO KERRO-CD-ERRO
              MOVE 'Informe a data da posição.'
                TO KERRO-TX-ERRO
              PERFORM 990000-ERRO
           END-IF
      
           IF KD005-CD-TIT-OPR-ETRD    EQUAL SPACES
              MOVE 002                 TO KERRO-CD-ERRO
              MOVE 'Informe o titulo da posição estruturada.'
                TO KERRO-TX-ERRO
              PERFORM 990000-ERRO
           END-IF
      
           COMPUTE KD005-NR-DVS-TIT       = KD005-NR-DVS-TIT       * 1
             ON SIZE ERROR
              MOVE ZEROS               TO KD005-NR-DVS-TIT
           END-COMPUTE.
      
           IF KD005-NR-DVS-TIT         EQUAL ZEROS
              MOVE 003                 TO KERRO-CD-ERRO
              MOVE 'Informe a Tranche do título.'
                TO KERRO-TX-ERRO
              PERFORM 990000-ERRO
           END-IF
      
           COMPUTE KD005-NR-SEQL-PSC-DRIA = KD005-NR-SEQL-PSC-DRIA * 1
             ON SIZE ERROR
              MOVE ZEROS               TO KD005-NR-SEQL-PSC-DRIA
           END-COMPUTE.
      
           IF KD005-NR-SEQL-PSC-DRIA   EQUAL ZEROS
              MOVE 004                 TO KERRO-CD-ERRO
              MOVE 'Informe o sequencial da posicao diaria do título.'
                TO KERRO-TX-ERRO
              PERFORM 990000-ERRO
           END-IF
      
           COMPUTE KD005-CD-OPR-ETRD-MCP  = KD005-CD-OPR-ETRD-MCP  * 1
             ON SIZE ERROR
              MOVE ZEROS               TO KD005-CD-OPR-ETRD-MCP
           END-COMPUTE.
      
           IF KD005-CD-OPR-ETRD-MCP    EQUAL ZEROS
              MOVE 005                 TO KERRO-CD-ERRO
              MOVE 'Informe o codigo da operacao estruturada no SGT.'
                TO KERRO-TX-ERRO
              PERFORM 990000-ERRO
           END-IF
      
           COMPUTE KD005-NR-VRS-OPR-ETRD  = KD005-NR-VRS-OPR-ETRD  * 1
             ON SIZE ERROR
              MOVE ZEROS               TO KD005-NR-VRS-OPR-ETRD
           END-COMPUTE.
      
           IF KD005-NR-VRS-OPR-ETRD    EQUAL ZEROS
              MOVE 006                 TO KERRO-CD-ERRO
              MOVE 'Informe a versao da operação estruturada no SGT.'
                TO KERRO-TX-ERRO
              PERFORM 990000-ERRO
           END-IF
           .
       200099-VALIDA-ENTRADA.
           EXIT.
      
      *----------------------------------------------------------------*
       300000-BUSCA-OPR-ETRD           SECTION.
      *----------------------------------------------------------------*
      
           INITIALIZE PRM-ST005
      
           MOVE 4                        TO SGTST005-ACAO
           MOVE KD005-CD-CTRA-TIT-VL     TO SGTST005-CD-CTRA-TIT-VL
           MOVE KD005-CD-OPR-ETRD-MCP    TO SGTST005-CD-OPR-ETRD-MCP
           MOVE KD005-NR-VRS-OPR-ETRD    TO SGTST005-NR-VRS-OPR-ETRD
      
           MOVE LENGTH OF PRM-ST005    TO EIBCALEN
           CALL SGTST005 USING DFHEIBLK PRM-ST005
      
           IF SGTST005-MSG-ERRO   NOT EQUAL SPACES
              MOVE SGTST005-CD-RTN-PGM TO KERRO-CD-ERRO
              MOVE SGTST005-MSG-ERRO   TO KERRO-TX-ERRO
              PERFORM 990000-ERRO
           END-IF
      
      * Campos inibidos pois não existem mais no book SGTKT005 e por não
      * serem usados no Natural que acessa este módulo foi movimentado
      * zeros em seu lugar
           MOVE SGTST005-CD-BCO-CRD-VL-OPR  TO KD005-CD-BCO-CRD-VL-OPR
           MOVE SGTST005-CD-DEPE-CRD-VL-OPR TO KD005-CD-DEPE-CRD-VL-OPR
           MOVE SGTST005-CD-SUB-DEPE-CRD-VL TO KD005-CD-SUB-DEPE-CRD-VL
      *    MOVE SGTST005-CD-PERC-RMNC       TO KD005-CD-PERC-RMNC
           MOVE ZEROS                       TO KD005-CD-PERC-RMNC
      *    MOVE SGTST005-CD-IEC-RMNC-OPR    TO KD005-CD-IEC-RMNC-OPR
           MOVE ZEROS                       TO KD005-CD-IEC-RMNC-OPR
           MOVE SGTST005-CD-UOR-DTTR-OPR    TO KD005-CD-UOR-DTTR-OPR
           MOVE SGTST005-CD-PRD             TO KD005-CD-PRD
           MOVE SGTST005-CD-MDLD            TO KD005-CD-MDLD
           MOVE SGTST005-CD-UOR-BB-CRD-VL   TO KD005-CD-UOR-BB-CRD-VL
           MOVE SGTST005-CD-UOR-BB          TO KD005-CD-UOR-BB
           MOVE SGTST005-NR-CC-BB           TO KD005-NR-CC-BB
      *    MOVE SGTST005-CD-TIP-FMA-APL     TO KD005-CD-TIP-FMA-APL
           MOVE ZEROS                       TO KD005-CD-TIP-FMA-APL
           MOVE SGTST005-IN-RMNC-VRV        TO KD005-IN-RMNC-VRV
           MOVE SGTST005-NR-CC-OTR-BCO      TO KD005-NR-CC-OTR-BCO
           MOVE SGTST005-DV-CC-OTR-BCO      TO KD005-DV-CC-OTR-BCO
           MOVE SGTST005-QT-PAPL-TIT-EMS    TO KD005-QT-PAPL-TIT-EMS
           MOVE SGTST005-QT-PAPL-TIT-CPR    TO KD005-QT-PAPL-TIT-CPR
           MOVE SGTST005-VL-CPR-OPR         TO KD005-VL-CPR-OPR
           MOVE SGTST005-VL-PU-EMS-TIT-OPR  TO KD005-VL-PU-EMS-TIT-OPR
           MOVE SGTST005-VL-PU-CPR-TIT-OPR  TO KD005-VL-PU-CPR-TIT-OPR
           MOVE SGTST005-DT-CPR-OPR         TO KD005-DT-CPR-OPR
           MOVE SGTST005-DT-VNCT-OPR        TO KD005-DT-VNCT-OPR
           MOVE SGTST005-DT-LQDC-OPR-DRIV   TO KD005-DT-LQDC-OPR-DRIV
           MOVE SGTST005-CD-TIT-OPR-ETRD    TO KD005-CD-TIT-OPR-ETRD
           MOVE SGTST005-NR-DVS-TIT         TO KD005-NR-DVS-TIT
           MOVE SGTST005-NR-UNCO-CTR-OPR    TO KD005-NR-UNCO-CTR-OPR
           MOVE SGTST005-CD-TIP-OPR-ETRD    TO KD005-CD-TIP-OPR-ETRD
           MOVE SGTST005-IN-OPR-ETRD-SPRO   TO KD005-IN-OPR-ETRD-SPRO
           MOVE SGTST005-IN-OPR-ETRD-VRJ    TO KD005-IN-OPR-ETRD-VRJ
           MOVE SGTST005-IN-OPR-ETRD-DSCP   TO KD005-IN-OPR-ETRD-DSCP
           MOVE SGTST005-IN-REG-OPR-BB      TO KD005-IN-REG-OPR-BB
           MOVE SGTST005-IN-OPR-N-NEGO      TO KD005-IN-OPR-N-NEGO
           MOVE SGTST005-IN-EXPO-VCLD-GRT   TO KD005-IN-EXPO-VCLD-GRT
           MOVE SGTST005-CD-TIP-COTA-TIT    TO KD005-CD-EST-RSTD-ITE
      
           .
       300099-BUSCA-OPR-ETRD.
           EXIT.
      
      *----------------------------------------------------------------*
       400000-BUSCA-PSC-SDO-OPR        SECTION.
      *----------------------------------------------------------------*
      
           INITIALIZE PRM-ST004
      
           MOVE 4                        TO SGTST004-ACAO
           MOVE KD005-CD-CTRA-TIT-VL     TO SGTST004-CD-CTRA-TIT-VL
           MOVE KD005-DT-PSC             TO SGTST004-DT-PSC
           MOVE KD005-CD-TIT-OPR-ETRD    TO SGTST004-CD-TIT-OPR-ETRD
           MOVE KD005-NR-DVS-TIT         TO SGTST004-NR-DVS-TIT
           MOVE KD005-NR-SEQL-PSC-DRIA   TO SGTST004-NR-SEQL-PSC-DRIA
      
           MOVE LENGTH OF PRM-ST004    TO EIBCALEN
           CALL SGTST004 USING DFHEIBLK PRM-ST004
      
           IF SGTST004-MSG-ERRO   NOT EQUAL SPACES
              MOVE SGTST004-CD-RTN-PGM TO KERRO-CD-ERRO
              MOVE SGTST004-MSG-ERRO   TO KERRO-TX-ERRO
              PERFORM 990000-ERRO
           END-IF
      
           MOVE SGTST004-TS-ULT-ATL         TO KD005-TS-ULT-ATL
           MOVE SGTST004-CD-USU-RSP-ATL     TO KD005-CD-USU-RSP-ATL
           MOVE SGTST004-QT-PAPL-TIT-DT-PSC TO KD005-QT-PAPL-TIT-DT-PSC
           MOVE SGTST004-VL-PU-TIT-DT-PSC   TO KD005-VL-PU-TIT-DT-PSC
           MOVE SGTST004-VL-SDO-TIT-DT-PSC  TO KD005-VL-SDO-TIT-DT-PSC
      ***  MOVE SGTST004-CD-EST-RSTD-ITE    TO KD005-CD-EST-RSTD-ITE
           MOVE SGTST004-DT-APL-TIT         TO KD005-DT-APL-TIT
           MOVE SGTST004-VL-TAXA-IDX        TO KD005-VL-TAXA-IDX
           MOVE SGTST004-CD-IC              TO KD005-CD-IC
           MOVE SGTST004-VL-EMS-PPL-TIT     TO KD005-VL-EMS-PPL-TIT
           MOVE SGTST004-VL-CTB-APL-MOEN    TO KD005-VL-CTB-APL-MOEN
           MOVE SGTST004-VL-JUR-ULT-MM      TO KD005-VL-JUR-ULT-MM
           MOVE SGTST004-VL-AMTZ            TO KD005-VL-AMTZ
           MOVE SGTST004-VL-PVS-APL-MOEN    TO KD005-VL-PVS-APL-MOEN
           MOVE SGTST004-TX-RTN-PRCT-OPR    TO KD005-TX-RTN-PRCT-OPR
           MOVE SGTST004-CD-USU-ENVO-PSC    TO KD005-CD-USU-ENVO-PSC
      
           .
       400099-BUSCA-PSC-SDO-OPR.
           EXIT.
      
      *----------------------------------------------------------------*
       500000-BUSCA-CLI-OPR-ETRD       SECTION.
      *----------------------------------------------------------------*
      
           MOVE 1 TO GDA-TIP-CLI
           PERFORM 510000-BUSCA-CLI-ST006
           IF SGTST006-CD-CLI EQUAL ZEROS
              PERFORM 520000-BUSCA-CLI-ST023
           END-IF
           MOVE GDA-CLIENTE            TO KD005-CD-UOR-BB-CRD-VL
           MOVE GDA-CPF-CGC            TO GDA-CNPJ-EMISSOR
           MOVE GDA-CNPJ-EMISSOR       TO GDA-TS-TEMP-CNPJ
           MOVE '000000000000'         TO GDA-TS-TEMP-FIL
           MOVE GDA-TS-TEMP            TO KD005-TS-ENVO-PSC
           MOVE ZEROS                  TO KD005-TS-ENVO-PSC
      
           MOVE 4 TO GDA-TIP-CLI
           PERFORM 510000-BUSCA-CLI-ST006
           IF SGTST006-CD-CLI EQUAL ZEROS
              PERFORM 520000-BUSCA-CLI-ST023
           END-IF
           MOVE GDA-CLIENTE            TO KD005-CD-UOR-BB
           MOVE GDA-CPF-CGC            TO KD005-NR-UNCO-CTR-OPR
      
           .
       500099-BUSCA-CLI-OPR-ETRD.
           EXIT.
      
      *----------------------------------------------------------------*
       510000-BUSCA-CLI-ST006          SECTION.
      *----------------------------------------------------------------*
      
           INITIALIZE PRM-ST006
      
           MOVE 5                      TO SGTST006-ACAO
           MOVE KD005-CD-CTRA-TIT-VL   TO SGTST006-CD-CTRA-TIT-VL
           MOVE KD005-CD-OPR-ETRD-MCP  TO SGTST006-CD-OPR-ETRD-MCP
           MOVE KD005-NR-VRS-OPR-ETRD  TO SGTST006-NR-VRS-OPR-ETRD
           MOVE GDA-TIP-CLI            TO SGTST006-CD-TIP-FUC-CLI
      
           MOVE LENGTH OF PRM-ST006    TO EIBCALEN
           CALL SGTST006 USING DFHEIBLK PRM-ST006
      
           IF NOT ( SGTST006-CD-RTN-PGM EQUAL 0
             OR     SGTST006-CD-RTN-PGM EQUAL +100 )
              MOVE SGTST006-CD-RTN-PGM TO KERRO-CD-ERRO
              MOVE SGTST006-MSG-ERRO   TO KERRO-TX-ERRO
              PERFORM 990000-ERRO
           END-IF
      
           MOVE SGTST006-CD-CLI        TO GDA-CLIENTE
      
           IF SGTST006-CD-CLI NOT EQUAL ZEROS
              PERFORM 511000-BUSCA-CLIENTE-MCI
           END-IF
      
           .
       510099-BUSCA-CLI-ST006.
           EXIT.
      
      *----------------------------------------------------------------*
       511000-BUSCA-CLIENTE-MCI SECTION.
      *----------------------------------------------------------------*
      
           INITIALIZE MCIS0001-DADOS
      
           MOVE 19485                    TO KIIBG-SRVC-ID-SRVC
           MOVE 1                        TO KIIBG-SRVC-VRS-SRVC
           MOVE 207827                   TO KIIBG-SRVC-ID-OPR
           MOVE 1                        TO KIIBG-SRVC-VRS-OPR
           MOVE 'MCIS0001'               TO KIIBG-NM-PGM-CHAMAR
      
           MOVE GDA-CLIENTE              TO MCIS0001-CLIENTE
      
           MOVE LENGTH OF MCIS0001-DADOS TO WS-TAM-DADOS-RQSC
           MOVE MCIS0001-DADOS           TO WS-DADOS-RQSC
      
           PERFORM 600000-RQSC-SRVC-IIB-GENERICO
      
           MOVE WS-DADOS-RPST            TO MCIS0001-DADOS
      
           EVALUATE MCIS0001-RET-CODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 MOVE ZEROS              TO MCIS0001-CPF-CGC
              WHEN OTHER
                 MOVE 005                 TO KERRO-CD-ERRO
                 MOVE 'Erro ao validar MCI do cliente.'
                   TO KERRO-TX-ERRO
                 PERFORM 990000-ERRO
           END-EVALUATE
      
           MOVE MCIS0001-CPF-CGC   TO GDA-CPF-CGC
      
      
           .
       510099-BUSCA-CLI-ST006.
           EXIT.
      
      *----------------------------------------------------------------*
       520000-BUSCA-CLI-ST023          SECTION.
      *----------------------------------------------------------------*
      
           INITIALIZE PRM-ST023
      
           MOVE 4                        TO SGTST023-ACAO
           MOVE KD005-CD-CTRA-TIT-VL     TO SGTST023-CD-CTRA-TIT-VL
           MOVE KD005-CD-OPR-ETRD-MCP    TO SGTST023-CD-OPR-ETRD-MCP
           MOVE KD005-NR-VRS-OPR-ETRD    TO SGTST023-NR-VRS-OPR-ETRD
           MOVE 0                        TO SGTST023-CD-STIT-OPR-ETRD
           MOVE GDA-TIP-CLI              TO SGTST023-CD-TIP-FUC-CLI
      
           MOVE LENGTH OF PRM-ST023    TO EIBCALEN
           CALL SGTST023 USING DFHEIBLK PRM-ST023
      
           IF NOT ( SGTST023-CD-RTN-PGM EQUAL 0
             OR     SGTST023-CD-RTN-PGM EQUAL +100 )
              MOVE SGTST023-CD-RTN-PGM TO KERRO-CD-ERRO
              MOVE SGTST023-MSG-ERRO   TO KERRO-TX-ERRO
              PERFORM 990000-ERRO
           END-IF
      
           MOVE SGTST023-CD-CLI           TO GDA-CLIENTE
           MOVE SGTST023-NR-IDFR-CLI-EXNO TO GDA-CPF-CGC
      
           .
       520099-BUSCA-CLI-ST023.
           EXIT.
      
      *----------------------------------------------------------------*
       600000-BUSCA-AGRUPAMENTO        SECTION.
      *----------------------------------------------------------------*
      
           INITIALIZE PRM-ST013
      
           MOVE 5                        TO SGTST013-ACAO
           MOVE KD005-CD-CTRA-TIT-VL     TO SGTST013-CD-CTRA-TIT-VL
           MOVE KD005-CD-OPR-ETRD-MCP    TO SGTST013-CD-OPR-ETRD-MCP
           MOVE KD005-NR-VRS-OPR-ETRD    TO SGTST013-NR-VRS-OPR-ETRD
      
           MOVE LENGTH OF PRM-ST013    TO EIBCALEN
           CALL SGTST013 USING DFHEIBLK PRM-ST013
      
           IF SGTST013-CD-RTN-PGM NOT EQUAL +100
              IF SGTST013-MSG-ERRO   NOT EQUAL SPACES
                 MOVE SGTST013-CD-RTN-PGM TO KERRO-CD-ERRO
                 MOVE SGTST013-MSG-ERRO   TO KERRO-TX-ERRO
                 PERFORM 990000-ERRO
              END-IF
           END-IF
      
           MOVE SGTST013-CD-AGPT-TIT-OPR TO KD005-CD-IC
           .
       600099-BUSCA-AGRUPAMENTO.
           EXIT.
      
      *----------------------------------------------------------------*
       600000-RQSC-SRVC-IIB-GENERICO     SECTION.
      *----------------------------------------------------------------*
      
           PERFORM 610000-PUT-CTNR-RQSC-SRVC
           PERFORM 620000-PUT-CTNR-DADOS-RQSC
           PERFORM 630000-LINK-IIBP2000
           PERFORM 640000-GET-CTRN-ERRO
      
           IF  WS-RESP EQUAL DFHRESP(CONTAINERERR)
               PERFORM 650000-GET-CTRN-RPST
           END-IF
           .
       600099-RQSC-SRVC-IIB-GENERICO.
           EXIT.
      
      *----------------------------------------------------------------*
       610000-PUT-CTNR-RQSC-SRVC       SECTION.
      *----------------------------------------------------------------*
      *    Grava container de requisição de serviço.
      *    Os dados a serem enviados nesse container devem ter sidos
      *    previamente cadastrados no sistema IIB.
      *----------------------------------------------------------------*
      *
           MOVE KIIBG-SRVC-ID-SRVC    TO SRVC-ID-SRVC.
           MOVE KIIBG-SRVC-VRS-SRVC   TO SRVC-VRS-SRVC.
           MOVE KIIBG-SRVC-ID-OPR     TO SRVC-ID-OPR.
           MOVE KIIBG-SRVC-VRS-OPR    TO SRVC-VRS-OPR.
           MOVE KIIBG-NM-PGM-CHAMAR   TO WS-NM-PGM
      *
           EXEC CICS PUT
                     CONTAINER ( 'PRM-RQSC-SRVC'              )
                     CHANNEL   ( WS-NM-CANAL                  )
                     FROM      ( SRVC-PRM-RQSC-SRVC           )
                     FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC )
                     RESP      ( WS-RESP                      )
                     RESP2     ( WS-RESP2                     )
           END-EXEC.
      *
           EVALUATE TRUE
               WHEN WS-RESP EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    MOVE 601              TO IIBKERRO-CD-ERRO
                    STRING 'SD005-Indisponibilidade de recurso ('
                            SRVC-ID-OPR  '-'
                            IIBKERRO-CD-ERRO '-'
                            WS-RESP          '-'
                            WS-RESP2         ')'
                                DELIMITED BY SIZE
                                        INTO IIBKERRO-TX-ERRO
                    PERFORM 990000-ERRO
           END-EVALUATE.
      *
       610099-PUT-CTNR-RQSC-SRVC.
           EXIT.
      *
      *----------------------------------------------------------------*
       620000-PUT-CTNR-DADOS-RQSC      SECTION.
      *----------------------------------------------------------------*
      *    Grava container com parametros do serviço.
      *----------------------------------------------------------------*
      *
           EXEC CICS PUT
                     CONTAINER ( 'PRM-DADOS-RQSC' )
                     CHANNEL   ( WS-NM-CANAL      )
                     FROM      ( WS-DADOS-RQSC        )
                     FLENGTH   ( WS-TAM-DADOS-RQSC    )
                     RESP      ( WS-RESP              )
                     RESP2     ( WS-RESP2             )
           END-EXEC.
      *
           EVALUATE TRUE
               WHEN WS-RESP EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    MOVE 602              TO IIBKERRO-CD-ERRO
                    STRING 'SD005-Indisponibilidade de recurso ('
                            SRVC-ID-OPR  '-'
                            IIBKERRO-CD-ERRO '-'
                            WS-RESP          '-'
                            WS-RESP2         ')'
                                DELIMITED BY SIZE
                                        INTO IIBKERRO-TX-ERRO
                    PERFORM 990000-ERRO
           END-EVALUATE.
      *
       620099-PUT-CTNR-DADOS-RQSC.
           EXIT.
      *
      *----------------------------------------------------------------*
       630000-LINK-IIBP2000            SECTION.
      *----------------------------------------------------------------*
      *    Link do programa IIBP2000 para requisição do serviço.
      *----------------------------------------------------------------*
      *
           EXEC CICS LINK
                     PROGRAM   ( 'IIBP2000'       )
                     CHANNEL   ( WS-NM-CANAL      )
                     RESP      ( WS-RESP          )
                     RESP2     ( WS-RESP2         )
           END-EXEC.
      *
           EVALUATE TRUE
               WHEN WS-RESP EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    MOVE 603              TO IIBKERRO-CD-ERRO
                    STRING 'SD005-Indisponibilidade de recurso ('
                            SRVC-ID-OPR  '-'
                            IIBKERRO-CD-ERRO
                           '-IIBP2000'
                            WS-RESP  ')'
                                DELIMITED BY SIZE
                                        INTO IIBKERRO-TX-ERRO
                    PERFORM 990000-ERRO
           END-EVALUATE.
      *
       630099-LINK-IIBP2000.
           EXIT.
      
      *----------------------------------------------------------------*
       640000-GET-CTRN-ERRO            SECTION.
      *----------------------------------------------------------------*
      *    A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *----------------------------------------------------------------*
      *
           EXEC CICS GET
                     CONTAINER ( 'PRM-ERRO'                  )
                     CHANNEL   ( WS-NM-CANAL                 )
                     INTO      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( WS-RESP                     )
                     RESP2     ( WS-RESP2                    )
           END-EXEC.
      *
           EVALUATE TRUE
               WHEN WS-RESP EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN WS-RESP EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    MOVE 604              TO IIBKERRO-CD-ERRO
                    STRING 'SD005-Indisponibilidade de recurso ('
                            SRVC-ID-OPR  '-'
                            IIBKERRO-CD-ERRO '-'
                            WS-NM-PGM        '-'
                            WS-RESP          '-'
                            WS-RESP2         ')'
                                DELIMITED BY SIZE
                                        INTO IIBKERRO-TX-ERRO
                    PERFORM 990000-ERRO
           END-EVALUATE.
      *
       640099-GET-CTRN-ERRO.
           EXIT.
      
      *----------------------------------------------------------------*
       650000-GET-CTRN-RPST            SECTION.
      *----------------------------------------------------------------*
      *    Recupera o container com os dados devolvidos pelo provedor
      *    do serviço.
      *----------------------------------------------------------------*
      *
           MOVE 0 TO WS-RESP
                     WS-RESP2
           EXEC CICS GET
                     CONTAINER ( 'PRM-DADOS-RPST'  )
                     CHANNEL   ( WS-NM-CANAL       )
                     NODATA
                     FLENGTH   ( WS-TAM-DADOS-RPST )
                     RESP      ( WS-RESP           )
                     RESP2     ( WS-RESP2          )
           END-EXEC.
      *
           EVALUATE TRUE
               WHEN WS-RESP EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    STRING 'Erro get-1 container PRM-DADOS-RPST, '
                            SRVC-ID-OPR  '-'
                           'RESP=' WS-RESP ', RESP-2=' WS-RESP2
                           ' - ' CTE-PROG
                                DELIMITED BY SIZE
                                        INTO IIBKERRO-TX-ERRO
                    PERFORM 990000-ERRO
           END-EVALUATE.
      *
           EXEC CICS GET
                     CONTAINER ( 'PRM-DADOS-RPST'  )
                     CHANNEL   ( WS-NM-CANAL       )
                     INTO      ( WS-DADOS-RPST     )
                     FLENGTH   ( WS-TAM-DADOS-RPST )
                     RESP      ( WS-RESP           )
                     RESP2     ( WS-RESP2          )
           END-EXEC.
      *
           EVALUATE TRUE
               WHEN WS-RESP EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    STRING 'Erro get-2 container PRM-DADOS-RPST, '
                           'RESP=' WS-RESP ', RESP-2=' WS-RESP2
                           ' - ' CTE-PROG
                                DELIMITED BY SIZE
                                        INTO IIBKERRO-TX-ERRO
                    PERFORM 990000-ERRO
           END-EVALUATE.
      *
       650099-GET-CTRN-RPST.
           EXIT.
      *
      
      *----------------------------------------------------------------*
       700000-PUT-RESPOSTA             SECTION.
      *----------------------------------------------------------------*
      
           EXEC CICS
               PUT CONTAINER ( 'SGTSD005-RPST' )
                   FROM      (  KD005-SAID      )
           END-EXEC
      
           IF  EIBRESP NOT EQUAL DFHRESP(NORMAL)
               MOVE EIBRESP            TO KERRO-CD-ERRO
               MOVE 006                TO GDA-SEQL-ERRO
               MOVE 'Erro ao gravar saída de dados da operação.'
                                       TO KERRO-TX-ERRO
               PERFORM 990000-ERRO
           END-IF
      
           .
       700099-PUT-RESPOSTA.
           EXIT.
      
      *----------------------------------------------------------------*
       990000-ERRO                     SECTION.
      *----------------------------------------------------------------*
      
           EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      
              EXEC CICS
                  PUT CONTAINER ( 'PRM-ERRO       ' )
                      FROM      ( KERRO-IIB-ERRO    )
              END-EXEC
      
           PERFORM 999000-ENCERRA
           .
       990099-ERRO.
           EXIT.
      
      *----------------------------------------------------------------*
       999000-ENCERRA                  SECTION.
      *----------------------------------------------------------------*
      
           EXEC CICS RETURN END-EXEC
      
           .
       999099-ENCERRA.
           EXIT.
      
      *================================================================*
