      * ============================================================== *
      * -------------       S I S T E M A    S G T     --------------- *
      * ============================================================== *
      * PROGRAMA..: SGTP0200.
      * ANALISTA..: Marcel Ginotti Pires
      * DATA......: 20/12/2018.
      * OBJETIVO..: Gera informacoes para carga de tabela em baixa
      *             plataforma - Projeto Analytics
      * COMPILACAO: COBOL II BATCH COM DB2 / BIND COM CARDLIB
      *-----------------------------------------------------------------
      * VRS001 22/01/2018 F3929037 - Implementação chamadas IIB
      *        20/12/2018 F6750805 - Geração de informações para carga
      *                              de tabela em baixa plataforma
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTP0200.
      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       INPUT-OUTPUT                    SECTION.
      *-----------------------------------------------------------------
       FILE-CONTROL.
           SELECT SGTF200S ASSIGN TO UT-S-SGTF200S.
      *-----------------------------------------------------------------
       DATA                            DIVISION.

      *-----------------------------------------------------------------
       FILE                            SECTION.
      *-----------------------------------------------------------------
       FD  SGTF200S
           BLOCK     0
           RECORDING F
           RECORD    1784.

       01  SGTF200S-REG-FD             PIC X(1784).
      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       01  CTE-INICIO                  PIC X(18) VALUE
           'W.S.S. COMECA AQUI'.
       01  CTE-PROG                    PIC X(16) VALUE
           '*** SGTP0200 ***'.
       01  CTE-VERS                    PIC X(06) VALUE 'VRS001'.
      *    SUBROTINAS UTILIZADAS
       77  CACS153C                    PIC X(08) VALUE 'CACS153C'.
       77  CAFS0007                    PIC X(08) VALUE 'CAFS0007'.
       77  CAFS0053                    PIC X(08) VALUE 'CAFS0053'.
       77  IIBS5000                    PIC X(08) VALUE 'IIBS5000'.
       77  SBABEND                     PIC X(08) VALUE 'SBABEND'.
       77  SBVERSAO                    PIC X(08) VALUE 'SBVERSAO'.
       77  SGTST027                    PIC X(08) VALUE 'SGTST027'.
       77  SGTST029                    PIC X(08) VALUE 'SGTST029'.
       77  SGTST030                    PIC X(08) VALUE 'SGTST030'.
       77  ICLS081C                    PIC X(08) VALUE 'ICLS081C'.
      *    SERVICOS UTILIZADOS
       77  CD-IIB-MSTS8042             PIC X(25) VALUE
           '0000000000000019986460001'.
       77  CD-IIB-SGTS0084             PIC X(25) VALUE
           '0000000000000036364390001'.
      *    CONSTANTES DIVERSAS
      *    ARQUIVO CSV
       77  TX-CSV                      PIC X(01) VALUE ';'.
      *    EIBCALEN/DFHEIBLK
       COPY HLPKDFHE.
      *    CICS5000
       COPY HLPKE01B.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      *    VARIAVEIS DIVERSAS
       77  W0084-RC                    PIC S9(4) COMP-5 VALUE 0.
       77  W0084-TX-ERRO               PIC X(120) VALUE ' '.
       77  W8042-RC                    PIC S9(4) COMP-5 VALUE 0.
       77  W8042-TX-ERRO               PIC X(120) VALUE ' '.

      *    VARIAVEIS HOST
       01  .
           03  CD-CTRA-TIT-VL          PIC S9(9) COMP-5 VALUE 0.
           03  CD-OPR-ETRD-MCP         PIC S9(11) COMP-3 VALUE 0.
           03  NR-VRS-OPR-ETRD         PIC S9(4) COMP-5 VALUE 0.
           03  CD-TIT-OPR-ETRD         PIC X(12) VALUE ' '.
           03  NR-DVS-TIT              PIC S9(4) COMP-5 VALUE 0.
           03  NR-UNCO-CTR-OPR         PIC S9(17) COMP-3 VALUE 0.
           03  CD-PRD                  PIC S9(4) COMP-5 VALUE 0.
           03  CD-MDLD                 PIC S9(4) COMP-5 VALUE 0.
           03  NR-EMS-TIT-OPR          PIC S9(9) COMP-5 VALUE 0.
           03  NR-SRE-EMS-TIT-OPR      PIC S9(9) COMP-5 VALUE 0.
           03  CD-UOR-DTTR-OPR         PIC S9(9) COMP-5 VALUE 0.
           03  NM-UOR-DTTR-OPR         PIC X(30) VALUE SPACES.
           03  CD-BCO-CRD-VL-OPR       PIC S9(4) COMP-5 VALUE 0.
           03  NM-BCO-CRD-VL-OPR       PIC X(40) VALUE ' '.
           03  CD-DEPE-CRD-VL-OPR      PIC S9(4) COMP-5 VALUE 0.
           03  NM-DEPE-CRD-VL-OPR      PIC X(40) VALUE ' '.
           03  NR-CC-BB                PIC S9(11) COMP-3 VALUE 0.
           03  NR-CC-OTR-BCO           PIC S9(11) COMP-3 VALUE 0.
           03  CD-ITC-CVM-OPR          PIC S9(4) COMP-5 VALUE 0.
           03  NM-ITC-CVM              PIC X(40) VALUE ' '.
           03  CD-CTGR-TVM             PIC S9(4) COMP-5 VALUE 0.
           03  NM-CTGR-TVM             PIC X(40) VALUE ' '.
           03  CD-ESPC-DBTR-OPR        PIC S9(4) COMP-5 VALUE 0.
           03  TX-DCR-TIP-GRT          PIC X(35) VALUE ' '.
           03  CD-TIP-COTA-TIT         PIC S9(4) COMP-5 VALUE 0.
           03  NM-TIP-COTA-TIT         PIC X(40) VALUE ' '.
           03  NR-VRS-TIP-COTA         PIC S9(4) COMP-5 VALUE 0.
           03  CD-TIP-MC-CPR           PIC S9(4) COMP-5 VALUE 0.
           03  CD-RSCO-OPR             PIC X(02) VALUE ' '.
           03  CD-CMRA-OPR-ETRD        PIC S9(4) COMP-5 VALUE 0.
           03  NM-CMRA-OPR-ETRD        PIC X(40) VALUE ' '.
           03  VL-APVD-BB-OPR          PIC S9(15)V9(2) COMP-3 VALUE 0.
           03  VL-PRV-ENCT             PIC S9(15)V9(2) COMP-3 VALUE 0.
           03  DT-MNDT-OPR             PIC X(10) VALUE ' '.
           03  VL-MNDT-OPR             PIC S9(15)V9(2) COMP-3 VALUE 0.
           03  VL-GRT-BB-OPR           PIC S9(15)V9(2) COMP-3 VALUE 0.
           03  DT-EMS-TIT-OPR          PIC X(10) VALUE ' '.
           03  VL-PU-EMS-TIT-OPR       PIC S9(9)V9(8) COMP-3 VALUE 0.
           03  QT-PAPL-TIT-EMS         PIC S9(9) COMP-5 VALUE 0.
           03  VL-EMS                  PIC S9(15)V9(2) COMP-3 VALUE 0.
           03  DT-CPR-OPR              PIC X(10) VALUE ' '.
           03  QT-PAPL-TIT-CPR         PIC S9(9) COMP-5 VALUE 0.
           03  VL-PU-CPR-TIT-OPR       PIC S9(9)V9(8) COMP-3 VALUE 0.
           03  VL-CPR-OPR              PIC S9(15)V9(2) COMP-3 VALUE 0.
           03  DT-EVT-VCLD-VRS         PIC X(10) VALUE ' '.
           03  VL-EVT-VCLD-VRS         PIC S9(15)V9(2) COMP-3 VALUE 0.
           03  TX-OBS-EVT              PIC X(100) VALUE ' '.
           03  DT-VNCT-OPR             PIC X(10) VALUE ' '.
           03  PZ                      PIC S9(4) COMP-5 VALUE 0.
           03  DT-LQDC-OPR-DRIV        PIC X(10) VALUE ' '.
           03  DT-INC-VGC-VRS-OPR      PIC X(10) VALUE ' '.
           03  CD-TIP-VRS-OPR          PIC S9(4) COMP-5 VALUE 0.
           03  CD-CLI-EMT              PIC S9(9) COMP-5 VALUE 0.
           03  CD-CLI-IFLC             PIC S9(9) COMP-5 VALUE 0.
           03  CD-CLI-OGM              PIC S9(9) COMP-5 VALUE 0.
           03  CD-CLI-FDAR             PIC S9(9) COMP-5 VALUE 0.
           03  CD-CLI-ADM              PIC S9(9) COMP-5 VALUE 0.
           03  CD-TIP-FUC-CLI          PIC S9(9) COMP-5 VALUE 0.
           03  NM-CLI-EMT              PIC X(60) VALUE ' '.
           03  CD-RSCO-CLI-EMT         PIC X(09) VALUE ' '.
           03  CD-GR-CLI-EMT           PIC S9(9) COMP-5 VALUE 0.
           03  NM-GR-CLI-EMT           PIC X(60) VALUE ' '.
           03  NM-CLI-IFLC             PIC X(60) VALUE ' '.
           03  CD-RSCO-CLI-IFLC        PIC X(09) VALUE ' '.
           03  CD-GR-CLI-IFLC          PIC S9(9) COMP-5 VALUE 0.
           03  NM-GR-CLI-IFLC          PIC X(60) VALUE ' '.
           03  NM-CLI-OGM              PIC X(60) VALUE ' '.
           03  CD-RSCO-CLI-OGM         PIC X(9)  VALUE ' '.
           03  CD-GR-CLI-OGM           PIC S9(9) COMP-5 VALUE 0.
           03  NM-GR-CLI-OGM           PIC X(60) VALUE ' '.
           03  NM-CLI-FDAR             PIC X(60) VALUE ' '.
           03  CD-GR-CLI-FDAR          PIC S9(9) COMP-5 VALUE 0.
           03  NM-GR-CLI-FDAR          PIC X(60) VALUE ' '.
           03  NM-CLI-ADM              PIC X(60) VALUE ' '.
           03  CD-GR-CLI-ADM           PIC S9(9) COMP-5 VALUE 0.
           03  NM-GR-CLI-ADM           PIC X(60) VALUE ' '.
           03  CD-TIP-IN-OP-EXC        PIC X(01) VALUE ' '.
           03  CD-TIP-IN-OP-CT-PBL     PIC X(01) VALUE ' '.
           03  CD-TIP-IN-OP-INFRA      PIC X(01) VALUE ' '.
           03  CD-TIP-IN-OP-NAO-CONV   PIC X(01) VALUE ' '.
           03  CD-TIP-IN-OP-FACILITY   PIC X(01) VALUE ' '.
           03  IN-IFLC-MNL-EXCO        PIC X(01) VALUE ' '.
           03  CD-ISIN-TIT-OPR         PIC X(12) VALUE ' '.
           03  CD-TIP-FMA-APL          PIC S9(4) COMP-5 VALUE 0.
           03  CD-IEC-RMNC             PIC S9(4) COMP-5 VALUE 0.
           03  CD-PERC-CPTZ-JUR        PIC S9(4) COMP-5 VALUE 0.
           03  PC-TAXA-RMNC-OPR        PIC S9(4)V9(10) COMP-3 VALUE 0.
           03  DT-PSC                  PIC X(10) VALUE ' '.
           03  VL-SDO-TIT-DT-PSC       PIC S9(15)V9(2) COMP-3 VALUE 0.

      *    INDICADORES DE NULIDADE
       01  .
           03  CD-TIT-OPR-ETRD-N       PIC S9(4) COMP-5 VALUE 0.
           03  NR-DVS-TIT-N            PIC S9(4) COMP-5 VALUE 0.
           03  NR-UNCO-CTR-OPR-N       PIC S9(4) COMP-5 VALUE 0.
           03  NR-EMS-TIT-OPR-N        PIC S9(4) COMP-5 VALUE 0.
           03  NR-SRE-EMS-TIT-OPR-N    PIC S9(4) COMP-5 VALUE 0.
           03  CD-BCO-CRD-VL-OPR-N     PIC S9(4) COMP-5 VALUE 0.
           03  CD-DEPE-CRD-VL-OPR-N    PIC S9(4) COMP-5 VALUE 0.
           03  NR-CC-OTR-BCO-N         PIC S9(4) COMP-5 VALUE 0.
           03  CD-CLI-EMT-N            PIC S9(4) COMP-5 VALUE 0.
           03  CD-CLI-IFLC-N           PIC S9(4) COMP-5 VALUE 0.
           03  CD-CLI-OGM-N            PIC S9(4) COMP-5 VALUE 0.
           03  CD-CLI-FDAR-N           PIC S9(4) COMP-5 VALUE 0.
           03  CD-CLI-ADM-N            PIC S9(4) COMP-5 VALUE 0.
           03  CD-ITC-CVM-OPR-N        PIC S9(4) COMP-5 VALUE 0.
           03  CD-CTGR-TVM-N           PIC S9(4) COMP-5 VALUE 0.
           03  CD-ESPC-DBTR-OPR-N      PIC S9(4) COMP-5 VALUE 0.
           03  CD-TIP-COTA-TIT-N       PIC S9(4) COMP-5 VALUE 0.
           03  NR-VRS-TIP-COTA-N       PIC S9(4) COMP-5 VALUE 0.
           03  CD-TIP-MC-CPR-N         PIC S9(4) COMP-5 VALUE 0.
           03  CD-RSCO-OPR-N           PIC S9(4) COMP-5 VALUE 0.
           03  CD-CMRA-OPR-ETRD-N      PIC S9(4) COMP-5 VALUE 0.
           03  VL-APVD-BB-OPR-N        PIC S9(4) COMP-5 VALUE 0.
           03  VL-PRV-ENCT-N           PIC S9(4) COMP-5 VALUE 0.
           03  DT-MNDT-OPR-N           PIC S9(4) COMP-5 VALUE 0.
           03  VL-MNDT-OPR-N           PIC S9(4) COMP-5 VALUE 0.
           03  VL-GRT-BB-OPR-N         PIC S9(4) COMP-5 VALUE 0.
           03  DT-EMS-TIT-OPR-N        PIC S9(4) COMP-5 VALUE 0.
           03  VL-EMS-N                PIC S9(4) COMP-5 VALUE 0.
           03  DT-CPR-OPR-N            PIC S9(4) COMP-5 VALUE 0.
           03  DT-EVT-VCLD-VRS-N       PIC S9(4) COMP-5 VALUE 0.
           03  VL-EVT-VCLD-VRS-N       PIC S9(4) COMP-5 VALUE 0.
           03  TX-OBS-EVT-N            PIC S9(4) COMP-5 VALUE 0.
           03  DT-VNCT-OPR-N           PIC S9(4) COMP-5 VALUE 0.
           03  PZ-N                    PIC S9(4) COMP-5 VALUE 0.
           03  DT-LQDC-OPR-DRIV-N      PIC S9(4) COMP-5 VALUE 0.
           03  DT-INC-VGC-VRS-OPR-N    PIC S9(4) COMP-5 VALUE 0.

      *-----------------------------------------------------------------
      *    CONTROLE DE CURSOR
      *-----------------------------------------------------------------
       01  .
           03  CD-SQL-CURSOR           PIC S9(4) COMP-5 VALUE   0.
               88  SQLCODE-CURSOR-FIM                   VALUE 100.
           03  FILLER                  PIC X(01) VALUE 'N'.
               88  CURSOR-ABTO-SIM               VALUE 'S'.
               88  CURSOR-ABTO-NAO               VALUE 'N'.
      *-----------------------------------------------------------------
      *    CONTROLE DE ARQUIVO
      *-----------------------------------------------------------------
       01  .
           03  QT-REG-E                PIC S9(9) COMP-5 VALUE 0.
           03  QT-REG-E-VAL            PIC S9(9) COMP-5 VALUE 0.
           03  QT-REG-S                PIC S9(9) COMP-5 VALUE 0.

      *--> Book do arquivo de Saida
       01  SGTF200S-REG.
       COPY SGTK200S.

      *-----------------------------------------------------------------
      *    BOOKS DAS SUBROTINAS CHAMADAS
      *-----------------------------------------------------------------
      *    CACS153C
       01  CACS153C-DADOS.
       COPY CACK153C.

      *    CAFS0007
       01  CAFS0007-DADOS.
           05  CAFS0007-BANCO-INF      PIC 9(03) VALUE 0.
           05  CAFS0007-AGE-INF        PIC 9(04) VALUE 0.
           05  CAFS0007-MUNICIPIO      PIC 9(07) VALUE 0.
           05  FILLER                  REDEFINES
               CAFS0007-MUNICIPIO.
               07  CAFS0007-CD-MUN     PIC 9(06).
               07  CAFS0007-DV-MUN     PIC 9(01).
           05  CAFS0007-RET-CODE       PIC 9(02) VALUE 0.
           05  CAFS0007-SQLCODE        PIC S9(05) VALUE 0.
           05  CAFS0007-SECTION        PIC X(30) VALUE ' '.
           05  CAFS0007-SQLCA          PIC X(146) VALUE ' '.
           05  CAFS0007-NM-MUNICIPIO   PIC X(38) VALUE ' '.
           05  CAFS0007-UF-MUNICIPIO   PIC X(02) VALUE ' '.
           05  CAFS0007-NOME-AGEN      PIC X(30) VALUE ' '.
           05  CAFS0007-NOME-BANCO     PIC X(50) VALUE ' '.

      *    CAFS0053
       01  CAFS0053-DADOS.
       COPY CAFK0053.
      *    ICLS081C
       01  ICLS081C-DADOS.
       COPY ICLKERRO.
       COPY ICLK081C.
      *    MSTS8042 - IIB
       COPY MSTK8042.
      *--> CRUD TIP_COTA_TIT_OPR
       01  SGTST027-DADOS.
       COPY SGTKT027.
      *--> CRUD CTGR_BC_TVM
       01  SGTST029-DADOS.
       COPY SGTKT029.
      *--> CRUD ITC_CVM_OPR_ETRD
       01  SGTST030-DADOS.
       COPY SGTKT030.
      *--> Consultar Clientes da Operacao - IIB
       COPY SGTK0084.

      *------ Book de requisio de servios ao IIB
       COPY IIBKW500.
      *------ Area de dados da mensagem  Dados de requisio/resposta
       01  AREA-DADOS-MSG.
           03  FILLER                  PIC X(01) OCCURS 1 TO 2000 TIMES
                                       DEPENDING ON W500-TAM-MSG.
      *    Book de erro
       COPY HLPKE001.

           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.

       77  CTE-FINAL-SS                PIC X(36) VALUE
           '*** STORAGE SECTION TERMINA AQUI ***'.
      *-----------------------------------------------------------------
      *    CURSOR
      *-----------------------------------------------------------------
           EXEC SQL
             DECLARE  LST-OPER-EST CURSOR FOR
               SELECT  A.CD_CTRA_TIT_VL,
                   A.CD_OPR_ETRD_MCP,
                   A.NR_VRS_OPR_ETRD,
                   VALUE (A.CD_TIT_OPR_ETRD, ' '),
                   VALUE (A.NR_DVS_TIT, 0),
                   VALUE (A.NR_UNCO_CTR_OPR, 0),
                   A.CD_PRD,
                   A.CD_MDLD,
                   VALUE (A.NR_EMS_TIT_OPR, 0),
                   VALUE (A.NR_SRE_EMS_TIT_OPR, 0),
                   A.CD_UOR_DTTR_OPR,
                   VALUE (A.CD_BCO_CRD_VL_OPR, 0),
                   VALUE (A.CD_DEPE_CRD_VL_OPR, 0),
                   A.NR_CC_BB,
                   VALUE (A.NR_CC_OTR_BCO, 0),
                   C.CD_CLI_EMISSOR,
                   D.CD_CLI_IMPACTO,
                   E.CD_CLI_ORIGEM,
                   F.CD_AG_FIDUCIARIO,
                   G.CD_ADMINISTRADOR,
                   CASE WHEN H.OP_CONT_PUBL IS NULL THEN 'N' ELSE 'S'
                   END OP_CONT_PUBL,
                   CASE WHEN I.OP_INFRA IS NULL THEN 'N' ELSE 'S'
                   END OP_INFRA,
                   CASE WHEN J.OP_NAO_CONVERSIVEL IS NULL
                                        THEN 'N' ELSE 'S'
                   END OP_NAO_CONVERSIVEL,
                   CASE WHEN K.OP_FACILITY IS NULL THEN 'N' ELSE 'S'
                   END OP_FACILITY,
                   CASE M.IN_IFLC_MNL_EXCO WHEN 'S' THEN 'S' ELSE CASE
                        WHEN N.CD_OPR_ETRD_MCP IS NULL
                             THEN 'N'
                             ELSE 'S'
                             END
                   END OP_EXCECAO,
                   O.CD_ISIN_TIT_OPR,
                   VALUE (A.CD_ITC_CVM_OPR, 0),
                   VALUE (A.CD_CTGR_TVM, 0),
                   VALUE (A.CD_ESPC_DBTR_OPR, 0),
                   VALUE (A.CD_TIP_COTA_TIT, 0),
                   VALUE (A.NR_VRS_TIP_COTA, 0),
                   VALUE (A.CD_TIP_MC_CPR, 0),
                   VALUE (A.CD_RSCO_OPR, ' '),
                   VALUE (A.CD_CMRA_OPR_ETRD, 0),
                   VALUE (A.VL_APVD_BB_OPR, 0),
                   VALUE (A.VL_PRV_ENCT, 0),
                   A.DT_MNDT_OPR,
                   VALUE (A.VL_MNDT_OPR, 0),
                   VALUE (A.VL_GRT_BB_OPR, 0),
                   A.DT_EMS_TIT_OPR,
                   A.VL_PU_EMS_TIT_OPR,
                   A.QT_PAPL_TIT_EMS,
                  (A.VL_PU_EMS_TIT_OPR * A.QT_PAPL_TIT_EMS) VL_EMISSAO,
                   A.DT_CPR_OPR,
                   A.QT_PAPL_TIT_CPR,
                   A.VL_PU_CPR_TIT_OPR,
                   A.VL_CPR_OPR,
                   A.DT_EVT_VCLD_VRS,
                   A.VL_EVT_VCLD_VRS,
                   A.TX_OBS_EVT,
                   A.DT_VNCT_OPR,
                 ((DAYS(A.DT_VNCT_OPR) - DAYS(A.DT_EMS_TIT_OPR)) / 365)
                     PRAZO,
                   P.CD_TIP_FMA_APL,
                   P.CD_IEC_RMNC,
                   P.CD_PERC_CPTZ_JUR,
                   P.PC_TAXA_RMNC_OPR,
                   A.DT_LQDC_OPR_DRIV,
                   CASE WHEN A.DT_INC_VGC_VRS_OPR IS NULL
                       THEN A.DT_CPR_OPR
                       ELSE A.DT_INC_VGC_VRS_OPR
                   END AS DT_INC_VGC_VRS_OPR,
                   A.CD_TIP_VRS_OPR,
                   L.DT_PSC,
                   L.VL_SDO_TIT_DT_PSC
               FROM
                   DB2SGT.OPR_ETRD_MCP A
                   INNER JOIN (SELECT A.CD_CTRA_TIT_VL,
                   A.CD_OPR_ETRD_MCP, A.NR_VRS_OPR_ETRD,
                   A.CD_TIP_EST_OPR, A.TS_EST_OPR_ETRD
                               FROM DB2SGT.EST_OPR_ETRD A
                               INNER JOIN (SELECT CD_CTRA_TIT_VL,
                               CD_OPR_ETRD_MCP, NR_VRS_OPR_ETRD,
                               MAX(TS_EST_OPR_ETRD) TS_EST_OPR_ETRD
                         FROM DB2SGT.EST_OPR_ETRD
                         GROUP BY CD_CTRA_TIT_VL, CD_OPR_ETRD_MCP,
                                        NR_VRS_OPR_ETRD) B
               ON A.CD_CTRA_TIT_VL = B.CD_CTRA_TIT_VL AND
                  B.CD_CTRA_TIT_VL > 0 AND
                  A.CD_OPR_ETRD_MCP = B.CD_OPR_ETRD_MCP AND
                  A.NR_VRS_OPR_ETRD = B.NR_VRS_OPR_ETRD AND
                  A.TS_EST_OPR_ETRD = B.TS_EST_OPR_ETRD) B
               ON A.CD_CTRA_TIT_VL = B.CD_CTRA_TIT_VL AND
                  A.CD_OPR_ETRD_MCP = B.CD_OPR_ETRD_MCP AND
                  A.NR_VRS_OPR_ETRD = B.NR_VRS_OPR_ETRD
                  INNER JOIN (SELECT A.CD_CTRA_TIT_VL,
                                     A.CD_OPR_ETRD_MCP,
                                     A.NR_VRS_OPR_ETRD,
                                     A.CD_TIP_EST_VRS, A.TS_EST_VRS
                              FROM DB2SGT.EST_VRS_OPR_ETRD A
                              INNER JOIN (SELECT CD_CTRA_TIT_VL,
                                          CD_OPR_ETRD_MCP,
                                          NR_VRS_OPR_ETRD,
                                          MAX(TS_EST_VRS) TS_EST_VRS
                              FROM DB2SGT.EST_VRS_OPR_ETRD
                              GROUP BY CD_CTRA_TIT_VL,
                                       CD_OPR_ETRD_MCP,
                                       NR_VRS_OPR_ETRD) B
             ON A.CD_CTRA_TIT_VL = B.CD_CTRA_TIT_VL AND
                A.CD_OPR_ETRD_MCP = B.CD_OPR_ETRD_MCP AND
                A.NR_VRS_OPR_ETRD = B.NR_VRS_OPR_ETRD AND
                A.TS_EST_VRS = B.TS_EST_VRS
                WHERE A.CD_TIP_EST_VRS = 3 AND A.CD_CTRA_TIT_VL > 0) V
             ON A.CD_CTRA_TIT_VL = V.CD_CTRA_TIT_VL AND
                A.CD_OPR_ETRD_MCP = V.CD_OPR_ETRD_MCP AND
                A.NR_VRS_OPR_ETRD = V.NR_VRS_OPR_ETRD
                INNER JOIN (SELECT CD_CTRA_TIT_VL, CD_OPR_ETRD_MCP,
                                   NR_VRS_OPR_ETRD,
                                   CD_CLI AS CD_CLI_EMISSOR
                            FROM DB2SGT.CLI_OPR_ETRD
                            WHERE CD_TIP_FUC_CLI = 1) C
             ON A.CD_CTRA_TIT_VL  = C.CD_CTRA_TIT_VL AND
                A.CD_OPR_ETRD_MCP = C.CD_OPR_ETRD_MCP AND
                A.NR_VRS_OPR_ETRD = C.NR_VRS_OPR_ETRD
                LEFT JOIN (SELECT CD_CTRA_TIT_VL, CD_OPR_ETRD_MCP,
                                  NR_VRS_OPR_ETRD,
                                  CD_CLI AS CD_CLI_IMPACTO
                           FROM DB2SGT.CLI_OPR_ETRD
                           WHERE CD_TIP_FUC_CLI = 2) D
             ON A.CD_CTRA_TIT_VL  = D.CD_CTRA_TIT_VL AND
                A.CD_OPR_ETRD_MCP = D.CD_OPR_ETRD_MCP AND
                A.NR_VRS_OPR_ETRD = D.NR_VRS_OPR_ETRD
                LEFT JOIN (SELECT CD_CTRA_TIT_VL, CD_OPR_ETRD_MCP,
                                  NR_VRS_OPR_ETRD,
                                  CD_CLI AS CD_CLI_ORIGEM
                           FROM DB2SGT.CLI_OPR_ETRD
                           WHERE CD_TIP_FUC_CLI = 3) E
             ON A.CD_CTRA_TIT_VL  = E.CD_CTRA_TIT_VL  AND
                A.CD_OPR_ETRD_MCP = E.CD_OPR_ETRD_MCP AND
                A.NR_VRS_OPR_ETRD = E.NR_VRS_OPR_ETRD
                LEFT JOIN (SELECT CD_CTRA_TIT_VL, CD_OPR_ETRD_MCP,
                                  NR_VRS_OPR_ETRD,
                                  CD_CLI AS CD_AG_FIDUCIARIO
                           FROM DB2SGT.CLI_OPR_ETRD
                           WHERE CD_TIP_FUC_CLI = 4) F
             ON A.CD_CTRA_TIT_VL  = F.CD_CTRA_TIT_VL AND
                A.CD_OPR_ETRD_MCP = F.CD_OPR_ETRD_MCP AND
                A.NR_VRS_OPR_ETRD = F.NR_VRS_OPR_ETRD
                LEFT JOIN (SELECT CD_CTRA_TIT_VL, CD_OPR_ETRD_MCP,
                                  NR_VRS_OPR_ETRD,
                                  CD_CLI AS CD_ADMINISTRADOR
                           FROM DB2SGT.CLI_OPR_ETRD
                           WHERE CD_TIP_FUC_CLI = 5) G
             ON A.CD_CTRA_TIT_VL  = G.CD_CTRA_TIT_VL AND
                A.CD_OPR_ETRD_MCP = G.CD_OPR_ETRD_MCP AND
                A.NR_VRS_OPR_ETRD = G.NR_VRS_OPR_ETRD
                LEFT JOIN (SELECT CD_CTRA_TIT_VL, CD_OPR_ETRD_MCP,
                                  NR_VRS_OPR_ETRD,
                                  CD_TIP_IN AS OP_CONT_PUBL
                           FROM DB2SGT.IN_OPR_ETRD
                           WHERE CD_TIP_IN = 1) H
            ON A.CD_CTRA_TIT_VL  = H.CD_CTRA_TIT_VL AND
               A.CD_OPR_ETRD_MCP = H.CD_OPR_ETRD_MCP AND
               A.NR_VRS_OPR_ETRD = H.NR_VRS_OPR_ETRD
               LEFT JOIN (SELECT CD_CTRA_TIT_VL, CD_OPR_ETRD_MCP,
                                 NR_VRS_OPR_ETRD,
                                 CD_TIP_IN AS OP_INFRA
                          FROM DB2SGT.IN_OPR_ETRD
                          WHERE CD_TIP_IN = 3) I
            ON A.CD_CTRA_TIT_VL  = I.CD_CTRA_TIT_VL AND
               A.CD_OPR_ETRD_MCP = I.CD_OPR_ETRD_MCP AND
               A.NR_VRS_OPR_ETRD = I.NR_VRS_OPR_ETRD
               LEFT JOIN (SELECT CD_CTRA_TIT_VL, CD_OPR_ETRD_MCP,
                                 NR_VRS_OPR_ETRD,
                                 CD_TIP_IN AS OP_NAO_CONVERSIVEL
                          FROM DB2SGT.IN_OPR_ETRD
                          WHERE CD_TIP_IN = 4) J
            ON A.CD_CTRA_TIT_VL  = J.CD_CTRA_TIT_VL AND
               A.CD_OPR_ETRD_MCP = J.CD_OPR_ETRD_MCP AND
               A.NR_VRS_OPR_ETRD = J.NR_VRS_OPR_ETRD
               LEFT JOIN (SELECT CD_CTRA_TIT_VL, CD_OPR_ETRD_MCP,
                                 NR_VRS_OPR_ETRD,
                                 CD_TIP_IN AS OP_FACILITY
                          FROM DB2SGT.IN_OPR_ETRD
                          WHERE CD_TIP_IN = 5) K
            ON A.CD_CTRA_TIT_VL  = K.CD_CTRA_TIT_VL AND
               A.CD_OPR_ETRD_MCP = K.CD_OPR_ETRD_MCP  AND
               A.NR_VRS_OPR_ETRD = K.NR_VRS_OPR_ETRD
               INNER JOIN (SELECT A.CD_CTRA_TIT_VL, A.CD_OPR_ETRD_MCP,
                                  C.NR_VRS_OPR, A.CD_TIT_OPR_ETRD,
                                  A.NR_DVS_TIT, A.DT_INC_VGC_VRS_OPR,
                                  A.DT_PSC, A.NR_SEQL_PSC_DRIA,
                                  B.VL_SDO_TIT_DT_PSC
                           FROM (SELECT A.CD_CTRA_TIT_VL, A.DT_PSC,
                                        A.CD_TIT_OPR_ETRD,
                                        A.NR_DVS_TIT,
                                        A.NR_SEQL_PSC_DRIA,
                          MAX(B.DT_INC_VGC_VRS_OPR) DT_INC_VGC_VRS_OPR,
                                        B.CD_OPR_ETRD_MCP
                          FROM (SELECT CD_CTRA_TIT_VL, DT_PSC,
                                       CD_TIT_OPR_ETRD, NR_DVS_TIT,
                          MAX(NR_SEQL_PSC_DRIA) NR_SEQL_PSC_DRIA
                          FROM DB2SGT.PSC_DRIA_SDO_OPR O
                          GROUP BY CD_CTRA_TIT_VL, DT_PSC,
                                   CD_TIT_OPR_ETRD,
                                   NR_DVS_TIT) A
                          INNER JOIN (SELECT DISTINCT CD_CTRA_TIT_VL,
                                             CD_OPR_ETRD_MCP,
                                             CD_TIT_OPR_ETRD,
                                             NR_DVS_TIT,
                          CASE WHEN DT_INC_VGC_VRS_OPR IS NULL
                                    THEN DT_CPR_OPR
                                    ELSE DT_INC_VGC_VRS_OPR
                                    END AS DT_INC_VGC_VRS_OPR
                          FROM DB2SGT.OPR_ETRD_MCP) B
                          ON A.CD_CTRA_TIT_VL = B.CD_CTRA_TIT_VL AND
                             A.CD_TIT_OPR_ETRD = B.CD_TIT_OPR_ETRD AND
                             A.NR_DVS_TIT = B.NR_DVS_TIT
                             WHERE B.DT_INC_VGC_VRS_OPR <= A.DT_PSC
                             GROUP BY A.CD_CTRA_TIT_VL, A.DT_PSC,
                                      A.CD_TIT_OPR_ETRD, A.NR_DVS_TIT,
                                      A.NR_SEQL_PSC_DRIA,
                                      B.CD_OPR_ETRD_MCP) A
                          INNER JOIN DB2SGT.PSC_DRIA_SDO_OPR B
                          ON B.CD_CTRA_TIT_VL > 0 AND
                          A.CD_CTRA_TIT_VL = B.CD_CTRA_TIT_VL AND
                             A.DT_PSC = B.DT_PSC AND
                             A.CD_TIT_OPR_ETRD = B.CD_TIT_OPR_ETRD AND
                             A.NR_DVS_TIT = B.NR_DVS_TIT AND
                             A.NR_SEQL_PSC_DRIA = B.NR_SEQL_PSC_DRIA
                             INNER JOIN (SELECT A.CD_CTRA_TIT_VL,
                                                A.CD_OPR_ETRD_MCP,
                                                A.CD_TIT_OPR_ETRD,
                                                A.NR_DVS_TIT,
                             CASE WHEN DT_INC_VGC_VRS_OPR IS NULL
                                       THEN DT_CPR_OPR
                                       ELSE DT_INC_VGC_VRS_OPR
                                       END AS DT_INC_VGC_VRS_OPR,
                                  MAX(A.NR_VRS_OPR_ETRD) NR_VRS_OPR
                                         FROM DB2SGT.OPR_ETRD_MCP A
                                         GROUP BY A.CD_CTRA_TIT_VL,
                                                  A.CD_OPR_ETRD_MCP,
                                                  A.CD_TIT_OPR_ETRD,
                                                  A.NR_DVS_TIT, CASE
                                  WHEN DT_INC_VGC_VRS_OPR IS NULL
                                       THEN DT_CPR_OPR
                                       ELSE DT_INC_VGC_VRS_OPR END) C
                          ON A.CD_CTRA_TIT_VL = C.CD_CTRA_TIT_VL AND
                             A.CD_TIT_OPR_ETRD > ' ' AND
                             A.CD_CTRA_TIT_VL > 0 AND
                             A.CD_OPR_ETRD_MCP = C.CD_OPR_ETRD_MCP AND
                             A.CD_TIT_OPR_ETRD = C.CD_TIT_OPR_ETRD AND
                             B.NR_DVS_TIT = C.NR_DVS_TIT AND
                        A.DT_INC_VGC_VRS_OPR = C.DT_INC_VGC_VRS_OPR) L
                          ON A.CD_CTRA_TIT_VL = L.CD_CTRA_TIT_VL AND
                             A.CD_OPR_ETRD_MCP = L.CD_OPR_ETRD_MCP AND
                             A.NR_VRS_OPR_ETRD = L.NR_VRS_OPR
                             LEFT JOIN (SELECT A.CD_CTRA_TIT_VL,
                                               A.CD_TIT_OPR_ETRD,
                                               A.NR_DVS_TIT,
                                               A.TS_ATL_IFLC_MNL,
                                               A.IN_IFLC_MNL_EXCO
                                        FROM DB2SGT.IFLC_MNL_LIM_EXCO A
                             INNER JOIN (SELECT CD_CTRA_TIT_VL,
                                                CD_TIT_OPR_ETRD,
                                                NR_DVS_TIT,
                             MAX(TS_ATL_IFLC_MNL) TS_ATL_IFLC_MNL
                                         FROM DB2SGT.IFLC_MNL_LIM_EXCO
                                         GROUP BY CD_CTRA_TIT_VL,
                                                  CD_TIT_OPR_ETRD,
                                                  NR_DVS_TIT) B
                          ON A.CD_CTRA_TIT_VL = B.CD_CTRA_TIT_VL AND
                             A.CD_TIT_OPR_ETRD = B.CD_TIT_OPR_ETRD AND
                             A.NR_DVS_TIT = B.NR_DVS_TIT AND
                             A.TS_ATL_IFLC_MNL = B.TS_ATL_IFLC_MNL) M
                          ON L.CD_CTRA_TIT_VL = M.CD_CTRA_TIT_VL AND
                             L.CD_TIT_OPR_ETRD = M.CD_TIT_OPR_ETRD AND
                             L.NR_DVS_TIT = M.NR_DVS_TIT
                             LEFT JOIN
                             (SELECT DISTINCT CD_CTRA_TIT_VL,
                                     CD_OPR_ETRD_MCP,
                                     NR_VRS_OPR_ETRD
                              FROM DB2SGT.EXCO_PRFL_SELD Y
                              WHERE
                              IN_EXCO_ATV = 'S' AND
                              NOT EXISTS(SELECT *
                                         FROM DB2SGT.IN_OPR_ETRD Z
                                         WHERE
                                    Z.CD_CTRA_TIT_VL > 0
                                    AND
                                    Y.CD_CTRA_TIT_VL = Z.CD_CTRA_TIT_VL
                                    AND
                              Y.CD_OPR_ETRD_MCP = Z.CD_OPR_ETRD_MCP AND
                              Y.NR_VRS_OPR_ETRD = Z.NR_VRS_OPR_ETRD AND
                              Z.CD_TIP_IN = 2)) N
                          ON  A.CD_CTRA_TIT_VL = N.CD_CTRA_TIT_VL AND
                              A.CD_OPR_ETRD_MCP = N.CD_OPR_ETRD_MCP AND
                              A.NR_VRS_OPR_ETRD = N.NR_VRS_OPR_ETRD
                              INNER JOIN DB2SGT.TIT_OPR_ETRD_MCP O
                          ON  A.CD_TIT_OPR_ETRD = O.CD_TIT_OPR_ETRD AND
                              A.NR_DVS_TIT = O.NR_DVS_TIT
                              INNER JOIN DB2SGT.RMNC_OPR_ETRD P
                          ON A.CD_CTRA_TIT_VL = P.CD_CTRA_TIT_VL AND
                             A.CD_OPR_ETRD_MCP = P.CD_OPR_ETRD_MCP AND
                             A.NR_VRS_OPR_ETRD = P.NR_VRS_OPR_ETRD
                             WHERE P.DT_INC_TAXA_RMNC > '0001-01-01'
                          AND A.CD_CTRA_TIT_VL > 0
                          AND A.CD_TIT_OPR_ETRD > ' '
         AND L.DT_PSC BETWEEN P.DT_INC_TAXA_RMNC AND P.DT_FIM_TAXA_RMNC
                             ORDER BY A.CD_CTRA_TIT_VL,
                                      A.CD_OPR_ETRD_MCP,
                                      A.NR_VRS_OPR_ETRD,
                                      L.DT_PSC
           END-EXEC.
      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO PSC-ERRO-PGM

           CALL SBVERSAO               USING CTE-PROG CTE-VERS
      *    WHENEVER DB2
       COPY HLPKE002.
           SET  EXECUCAO-BATCH         TO TRUE

           PERFORM 100000-INICIA

           PERFORM 200000-PROCESSA
             UNTIL SQLCODE-CURSOR-FIM

           PERFORM 300000-TERMINA

           STOP RUN
           .
       000000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       100000-INICIA                   SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO PSC-ERRO-PGM

      *    ARQUIVO
           OPEN OUTPUT SGTF200S

      *    CURSOR
           EXEC SQL
               OPEN LST-OPER-EST
           END-EXEC
           SET  CURSOR-ABTO-SIM        TO TRUE

           PERFORM 110000-ABRE-SERVICOS

           PERFORM 120000-GRAVA-CABEC
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       110000-ABRE-SERVICOS            SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO PSC-ERRO-PGM

           SET  W500-PREPARA           TO TRUE
           MOVE 00000000               TO W500-TAM-MSG

           MOVE CD-IIB-MSTS8042        TO W500-ID-SRVC
           PERFORM 905000-PREPARA-SERVICOS

           MOVE CD-IIB-SGTS0084        TO W500-ID-SRVC
           PERFORM 905000-PREPARA-SERVICOS

      *    ANTES DE 200000-PROCESSA (LOOP)
           SET  W500-PROCESSA          TO TRUE
           .
       110000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       120000-GRAVA-CABEC              SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO PSC-ERRO-PGM

           MOVE SPACES                 TO SGTF200S-REG

           STRING 'CD-CTRA-TIT-VL'     DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-OPR-ETRD-MCP'    DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NR-VRS-OPR-ETRD'    DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-TIT-OPR-ETRD'    DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NR-DVS-TIT'         DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NR-UNCO-CTR-OPR'    DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-PRD'             DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-MDLD'            DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NR-EMS-TIT-OPR'     DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NR-SRE-EMS-TIT-OPR' DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-UOR-DTTR-OPR'    DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NM-UOR-DTTR-OPR'    DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-BCO-CRD-VL-OPR'  DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NM-BCO-CRD-VL-OPR'  DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-DEPE-CRD-VL-OPR' DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NM-DEPE-CRD-VL-OPR' DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NR-CC-BB'           DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NR-CC-OTR-BCO'      DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-ITC-CVM-OPR'     DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NM-ITC-CVM'         DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-CTGR-TVM'        DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NM-CTGR-TVM'        DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-ESPC-DBTR-OPR'   DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'TX-DCR-TIP-GRT'     DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-TIP-COTA-TIT'    DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NM-TIP-COTA-TIT'    DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NR-VRS-TIP-COTA'    DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-TIP-MC-CPR'      DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-RSCO-OPR'        DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-CMRA-OPR-ETRD'   DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NM-CMRA-OPR-ETRD'   DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'VL-APVD-BB-OPR'     DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'VL-PRV-ENCT'        DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'DT-MNDT-OPR'        DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'VL-MNDT-OPR'        DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'VL-GRT-BB-OPR'      DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'DT-EMS-TIT-OPR'     DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'VL-PU-EMS-TIT-OPR'  DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'QT-PAPL-TIT-EMS'    DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'VL-EMS'             DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'DT-CPR-OPR'         DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'QT-PAPL-TIT-CPR'    DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'VL-PU-CPR-TIT-OPR'  DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'VL-CPR-OPR'         DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'DT-EVT-VCLD-VRS'    DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'VL-EVT-VCLD-VRS'    DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'TX-OBS-EVT'         DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'DT-VNCT-OPR'        DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'PZ'                 DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'DT-LQDC-OPR-DRIV'   DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'DT-INC-VGC-VRS-OPR' DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-TIP-VRS-OPR'     DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-CLI-EMT'         DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-CLI-IFLC'        DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-CLI-OGM'         DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-CLI-FDAR'        DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-CLI-ADM'         DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-TIP-FUC-CLI'     DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NM-CLI-EMT'         DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-RSCO-CLI-EMT'    DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-GR-CLI-EMT'      DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NM-GR-CLI-EMT'      DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NM-CLI-IFLC'        DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-RSCO-CLI-IFLC'   DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-GR-CLI-IFLC'     DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NM-GR-CLI-IFLC'     DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NM-CLI-OGM'         DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-RSCO-CLI-OGM'    DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-GR-CLI-OGM'      DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NM-GR-CLI-OGM'      DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NM-CLI-FDAR'        DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-GR-CLI-FDAR'     DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NM-GR-CLI-FDAR'     DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NM-CLI-ADM'         DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-GR-CLI-ADM'      DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'NM-GR-CLI-ADM'      DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-TIP-IN-OP-EXC'   DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-TIP-IN-OP-CT-PBL' DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-TIP-IN-OP-INFRA' DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-TIP-IN-OP-NAO-CONV' DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-TIP-IN-OP-FACILITY' DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'IN-IFLC-MNL-EXCO'   DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-ISIN-TIT-OPR'    DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-TIP-FMA-APL'     DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-IEC-RMNC'        DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'CD-PERC-CPTZ-JUR'   DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'PC-TAXA-RMNC-OPR'   DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'DT-PSC'             DELIMITED BY SIZE
                   TX-CSV              DELIMITED BY SIZE
                  'VL-SDO-TIT-DT-PSC'  DELIMITED BY SIZE
               INTO SGTF200S-REG
           END-STRING

           PERFORM 980200-GRAVA-SGTF200S
           .
       120000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSA                 SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO PSC-ERRO-PGM

           EXEC SQL
              FETCH LST-OPER-EST
                 INTO :CD-CTRA-TIT-VL
                 ,    :CD-OPR-ETRD-MCP
                 ,    :NR-VRS-OPR-ETRD
                 ,    :CD-TIT-OPR-ETRD
                      :CD-TIT-OPR-ETRD-N
                 ,    :NR-DVS-TIT
                      :NR-DVS-TIT-N
                 ,    :NR-UNCO-CTR-OPR
                      :NR-UNCO-CTR-OPR-N
                 ,    :CD-PRD
                 ,    :CD-MDLD
                 ,    :NR-EMS-TIT-OPR
                      :NR-EMS-TIT-OPR-N
                 ,    :NR-SRE-EMS-TIT-OPR
                      :NR-SRE-EMS-TIT-OPR-N
                 ,    :CD-UOR-DTTR-OPR
                 ,    :CD-BCO-CRD-VL-OPR
                      :CD-BCO-CRD-VL-OPR-N
                 ,    :CD-DEPE-CRD-VL-OPR
                      :CD-DEPE-CRD-VL-OPR-N
                 ,    :NR-CC-BB
                 ,    :NR-CC-OTR-BCO
                      :NR-CC-OTR-BCO-N
                 ,    :CD-CLI-EMT
      *                INDICADOR NECESSARIO, EMBORA CD_CLI SEJA
      *                "NOT NULL"
                      :CD-CLI-EMT-N
                 ,    :CD-CLI-IFLC
                      :CD-CLI-IFLC-N
                 ,    :CD-CLI-OGM
                      :CD-CLI-OGM-N
                 ,    :CD-CLI-FDAR
                      :CD-CLI-FDAR-N
                 ,    :CD-CLI-ADM
                      :CD-CLI-ADM-N
                 ,    :CD-TIP-IN-OP-CT-PBL
                 ,    :CD-TIP-IN-OP-INFRA
                 ,    :CD-TIP-IN-OP-NAO-CONV
                 ,    :CD-TIP-IN-OP-FACILITY
                 ,    :CD-TIP-IN-OP-EXC
                 ,    :CD-ISIN-TIT-OPR
                 ,    :CD-ITC-CVM-OPR
                      :CD-ITC-CVM-OPR-N
                 ,    :CD-CTGR-TVM
                      :CD-CTGR-TVM-N
                 ,    :CD-ESPC-DBTR-OPR
                      :CD-ESPC-DBTR-OPR-N
                 ,    :CD-TIP-COTA-TIT
                      :CD-TIP-COTA-TIT-N
                 ,    :NR-VRS-TIP-COTA
                      :NR-VRS-TIP-COTA-N
                 ,    :CD-TIP-MC-CPR
                      :CD-TIP-MC-CPR-N
                 ,    :CD-RSCO-OPR
                      :CD-RSCO-OPR-N
                 ,    :CD-CMRA-OPR-ETRD
                      :CD-CMRA-OPR-ETRD-N
                 ,    :VL-APVD-BB-OPR
                      :VL-APVD-BB-OPR-N
                 ,    :VL-PRV-ENCT
                      :VL-PRV-ENCT-N
                 ,    :DT-MNDT-OPR
                      :DT-MNDT-OPR-N
                 ,    :VL-MNDT-OPR
                      :VL-MNDT-OPR-N
                 ,    :VL-GRT-BB-OPR
                      :VL-GRT-BB-OPR-N
                 ,    :DT-EMS-TIT-OPR
                      :DT-EMS-TIT-OPR-N
                 ,    :VL-PU-EMS-TIT-OPR
                 ,    :QT-PAPL-TIT-EMS
                 ,    :VL-EMS
      *                INDICADOR NECESSARIO, EMBORA OS VALORES USADOS
      *                NO CALCULO SEJAM "NOT NULL"
                      :VL-EMS-N
                 ,    :DT-CPR-OPR
                      :DT-CPR-OPR-N
                 ,    :QT-PAPL-TIT-CPR
                 ,    :VL-PU-CPR-TIT-OPR
                 ,    :VL-CPR-OPR
                 ,    :DT-EVT-VCLD-VRS
                      :DT-EVT-VCLD-VRS-N
                 ,    :VL-EVT-VCLD-VRS
                      :VL-EVT-VCLD-VRS-N
                 ,    :TX-OBS-EVT
                      :TX-OBS-EVT-N
                 ,    :DT-VNCT-OPR
                      :DT-VNCT-OPR-N
                 ,    :PZ
                      :PZ-N
                 ,    :CD-TIP-FMA-APL
                 ,    :CD-IEC-RMNC
                 ,    :CD-PERC-CPTZ-JUR
                 ,    :PC-TAXA-RMNC-OPR
                 ,    :DT-LQDC-OPR-DRIV
                      :DT-LQDC-OPR-DRIV-N
                 ,    :DT-INC-VGC-VRS-OPR
                      :DT-INC-VGC-VRS-OPR-N
                 ,    :CD-TIP-VRS-OPR
                 ,    :DT-PSC
                 ,    :VL-SDO-TIT-DT-PSC
           END-EXEC
      
           IF  SQLCODE                 EQUAL ZEROS
               ADD  1                  TO QT-REG-E
               IF  ZEROS               EQUAL CD-CTRA-TIT-VL  AND
                                             CD-OPR-ETRD-MCP AND
                                             NR-VRS-OPR-ETRD
                   DISPLAY 'QT-REG-E: ' QT-REG-E
                   DISPLAY
                  'CD-CTRA-TIT-VAL/CD-OPR-ETRD-MCP/NR-VRS-OPR-ETRD = 0'
               ELSE
                   ADD  1              TO QT-REG-E-VAL
                   PERFORM 210000-FORMATA-SAIDA
                   PERFORM 980200-GRAVA-SGTF200S
               END-IF
           END-IF

           MOVE SQLCODE                TO CD-SQL-CURSOR
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       210000-FORMATA-SAIDA            SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO PSC-ERRO-PGM

           INITIALIZE SGTS200S-RG

           PERFORM 211000-MOVE-VARIAVEIS-HOST

           PERFORM 212000-MOVE-TX-CSV

      *    MSTS8042 - IIB
           PERFORM 978042-OBTEM-NM-UOR

      *    CAFS0053 - ON LINE/BATCH - CORPORATIVO
           PERFORM 970053-OBTEM-NM-BCO

      *    CAFS0007 - ON LINE/BATCH - CORPORATIVO
           PERFORM 970007-OBTEM-NM-DEPE

      *    SGTS0084 - IIB
           PERFORM 970084-OBTEM-DADOS-CLIENTES

      *    SGTST030
           PERFORM 970030-OBTEM-NM-ITC

      *    SGTST029
           PERFORM 970029-OBTEM-NM-CTGR

      *    SGTST027
           PERFORM 970027-OBTEM-NM-TIP-COTA

      *    CACS153C - ON LINE/BATCH - CORPORATIVO
           PERFORM 970153-OBTEM-TX-DCR-TIP-GRT

      *    ICLS081C
           PERFORM 970081-OBTEM-NM-CLRG
           .
       210000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       211000-MOVE-VARIAVEIS-HOST      SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO PSC-ERRO-PGM

           MOVE          CD-CTRA-TIT-VL
             TO SGTS200S-CD-CTRA-TIT-VL
           MOVE          CD-OPR-ETRD-MCP
             TO SGTS200S-CD-OPR-ETRD-MCP
           MOVE          NR-VRS-OPR-ETRD
             TO SGTS200S-NR-VRS-OPR-ETRD
           MOVE          CD-TIT-OPR-ETRD
             TO SGTS200S-CD-TIT-OPR-ETRD
           MOVE          NR-DVS-TIT
             TO SGTS200S-NR-DVS-TIT
           MOVE          NR-UNCO-CTR-OPR
             TO SGTS200S-NR-UNCO-CTR-OPR
           MOVE          CD-PRD
             TO SGTS200S-CD-PRD
           MOVE          CD-MDLD
             TO SGTS200S-CD-MDLD
           MOVE          NR-EMS-TIT-OPR
             TO SGTS200S-NR-EMS-TIT-OPR
           MOVE          NR-SRE-EMS-TIT-OPR
             TO SGTS200S-NR-SRE-EMS-TIT-OPR
           MOVE          CD-UOR-DTTR-OPR
             TO SGTS200S-CD-UOR-DTTR-OPR
           MOVE          NM-UOR-DTTR-OPR
             TO SGTS200S-NM-UOR-DTTR-OPR
           MOVE          CD-BCO-CRD-VL-OPR
             TO SGTS200S-CD-BCO-CRD-VL-OPR
           MOVE          NM-BCO-CRD-VL-OPR
             TO SGTS200S-NM-BCO-CRD-VL-OPR
           MOVE          CD-DEPE-CRD-VL-OPR
             TO SGTS200S-CD-DEPE-CRD-VL-OPR
           MOVE          NM-DEPE-CRD-VL-OPR
             TO SGTS200S-NM-DEPE-CRD-VL-OPR
           MOVE          NR-CC-BB
             TO SGTS200S-NR-CC-BB
           MOVE          NR-CC-OTR-BCO
             TO SGTS200S-NR-CC-OTR-BCO
           MOVE          CD-ITC-CVM-OPR
             TO SGTS200S-CD-ITC-CVM-OPR
           MOVE          NM-ITC-CVM
             TO SGTS200S-NM-ITC-CVM
           MOVE          CD-CTGR-TVM
             TO SGTS200S-CD-CTGR-TVM
           MOVE          NM-CTGR-TVM
             TO SGTS200S-NM-CTGR-TVM
           MOVE          CD-ESPC-DBTR-OPR
             TO SGTS200S-CD-ESPC-DBTR-OPR
           MOVE          TX-DCR-TIP-GRT
             TO SGTS200S-TX-DCR-TIP-GRT
           MOVE          CD-TIP-COTA-TIT
             TO SGTS200S-CD-TIP-COTA-TIT
           MOVE          NM-TIP-COTA-TIT
             TO SGTS200S-NM-TIP-COTA-TIT
           MOVE          NR-VRS-TIP-COTA
             TO SGTS200S-NR-VRS-TIP-COTA
           MOVE          CD-TIP-MC-CPR
             TO SGTS200S-CD-TIP-MC-CPR
           MOVE          CD-RSCO-OPR
             TO SGTS200S-CD-RSCO-OPR
           MOVE          CD-CMRA-OPR-ETRD
             TO SGTS200S-CD-CMRA-OPR-ETRD
           MOVE          NM-CMRA-OPR-ETRD
             TO SGTS200S-NM-CMRA-OPR-ETRD
           MOVE          VL-APVD-BB-OPR
             TO SGTS200S-VL-APVD-BB-OPR
           MOVE          VL-PRV-ENCT
             TO SGTS200S-VL-PRV-ENCT
           MOVE          DT-MNDT-OPR
             TO SGTS200S-DT-MNDT-OPR
           MOVE          VL-MNDT-OPR
             TO SGTS200S-VL-MNDT-OPR
           MOVE          VL-GRT-BB-OPR
             TO SGTS200S-VL-GRT-BB-OPR
           MOVE          DT-EMS-TIT-OPR
             TO SGTS200S-DT-EMS-TIT-OPR
           MOVE          VL-PU-EMS-TIT-OPR
             TO SGTS200S-VL-PU-EMS-TIT-OPR
           MOVE          QT-PAPL-TIT-EMS
             TO SGTS200S-QT-PAPL-TIT-EMS
           MOVE          VL-EMS
             TO SGTS200S-VL-EMS
           MOVE          DT-CPR-OPR
             TO SGTS200S-DT-CPR-OPR
           MOVE          QT-PAPL-TIT-CPR
             TO SGTS200S-QT-PAPL-TIT-CPR
           MOVE          VL-PU-CPR-TIT-OPR
             TO SGTS200S-VL-PU-CPR-TIT-OPR
           MOVE          VL-CPR-OPR
             TO SGTS200S-VL-CPR-OPR
           MOVE          DT-EVT-VCLD-VRS
             TO SGTS200S-DT-EVT-VCLD-VRS
           MOVE          VL-EVT-VCLD-VRS
             TO SGTS200S-VL-EVT-VCLD-VRS
           MOVE          TX-OBS-EVT
             TO SGTS200S-TX-OBS-EVT
           MOVE          DT-VNCT-OPR
             TO SGTS200S-DT-VNCT-OPR
           MOVE          PZ
             TO SGTS200S-PZ
           MOVE          DT-LQDC-OPR-DRIV
             TO SGTS200S-DT-LQDC-OPR-DRIV
           MOVE          DT-INC-VGC-VRS-OPR
             TO SGTS200S-DT-INC-VGC-VRS-OPR
           MOVE          CD-TIP-VRS-OPR
             TO SGTS200S-CD-TIP-VRS-OPR
           MOVE          CD-CLI-EMT
             TO SGTS200S-CD-CLI-EMT
           MOVE          CD-CLI-IFLC
             TO SGTS200S-CD-CLI-IFLC
           MOVE          CD-CLI-OGM
             TO SGTS200S-CD-CLI-OGM
           MOVE          CD-CLI-FDAR
             TO SGTS200S-CD-CLI-FDAR
           MOVE          CD-CLI-ADM
             TO SGTS200S-CD-CLI-ADM
           MOVE          CD-TIP-FUC-CLI
             TO SGTS200S-CD-TIP-FUC-CLI
           MOVE          NM-CLI-EMT
             TO SGTS200S-NM-CLI-EMT
           MOVE          CD-RSCO-CLI-EMT
             TO SGTS200S-CD-RSCO-CLI-EMT
           MOVE          CD-GR-CLI-EMT
             TO SGTS200S-CD-GR-CLI-EMT
           MOVE          NM-GR-CLI-EMT
             TO SGTS200S-NM-GR-CLI-EMT
           MOVE          NM-CLI-IFLC
             TO SGTS200S-NM-CLI-IFLC
           MOVE          CD-RSCO-CLI-IFLC
             TO SGTS200S-CD-RSCO-CLI-IFLC
           MOVE          CD-GR-CLI-IFLC
             TO SGTS200S-CD-GR-CLI-IFLC
           MOVE          NM-GR-CLI-IFLC
             TO SGTS200S-NM-GR-CLI-IFLC
           MOVE          NM-CLI-OGM
             TO SGTS200S-NM-CLI-OGM
           MOVE          CD-RSCO-CLI-OGM
             TO SGTS200S-CD-RSCO-CLI-OGM
           MOVE          CD-GR-CLI-OGM
             TO SGTS200S-CD-GR-CLI-OGM
           MOVE          NM-GR-CLI-OGM
             TO SGTS200S-NM-GR-CLI-OGM
           MOVE          NM-CLI-FDAR
             TO SGTS200S-NM-CLI-FDAR
           MOVE          CD-GR-CLI-FDAR
             TO SGTS200S-CD-GR-CLI-FDAR
           MOVE          NM-GR-CLI-FDAR
             TO SGTS200S-NM-GR-CLI-FDAR
           MOVE          NM-CLI-ADM
             TO SGTS200S-NM-CLI-ADM
           MOVE          CD-GR-CLI-ADM
             TO SGTS200S-CD-GR-CLI-ADM
           MOVE          NM-GR-CLI-ADM
             TO SGTS200S-NM-GR-CLI-ADM
           MOVE          CD-TIP-IN-OP-EXC
             TO SGTS200S-CD-TIP-IN-OP-EXC
           MOVE          CD-TIP-IN-OP-CT-PBL
             TO SGTS200S-CD-TIP-IN-OP-CT-PBL
           MOVE          CD-TIP-IN-OP-INFRA
             TO SGTS200S-CD-TIP-IN-OP-INFRA
           MOVE          CD-TIP-IN-OP-NAO-CONV
             TO SGTS200S-CD-TIP-IN-OP-NAO-CONV
           MOVE          CD-TIP-IN-OP-FACILITY
             TO SGTS200S-CD-TIP-IN-OP-FACILITY
           MOVE          IN-IFLC-MNL-EXCO
             TO SGTS200S-IN-IFLC-MNL-EXCO
           MOVE          CD-ISIN-TIT-OPR
             TO SGTS200S-CD-ISIN-TIT-OPR
           MOVE          CD-TIP-FMA-APL
             TO SGTS200S-CD-TIP-FMA-APL
           MOVE          CD-IEC-RMNC
             TO SGTS200S-CD-IEC-RMNC
           MOVE          CD-PERC-CPTZ-JUR
             TO SGTS200S-CD-PERC-CPTZ-JUR
           MOVE          PC-TAXA-RMNC-OPR
             TO SGTS200S-PC-TAXA-RMNC-OPR
           MOVE          DT-PSC
             TO SGTS200S-DT-PSC
           MOVE          VL-SDO-TIT-DT-PSC
             TO SGTS200S-VL-SDO-TIT-DT-PSC
           .
       211000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       212000-MOVE-TX-CSV              SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO PSC-ERRO-PGM

           MOVE TX-CSV                 TO SGTS200S-TX-CSV-01
                                          SGTS200S-TX-CSV-02
                                          SGTS200S-TX-CSV-03
                                          SGTS200S-TX-CSV-04
                                          SGTS200S-TX-CSV-05
                                          SGTS200S-TX-CSV-06
                                          SGTS200S-TX-CSV-07
                                          SGTS200S-TX-CSV-08
                                          SGTS200S-TX-CSV-09
                                          SGTS200S-TX-CSV-10
                                          SGTS200S-TX-CSV-11
                                          SGTS200S-TX-CSV-12
                                          SGTS200S-TX-CSV-13
                                          SGTS200S-TX-CSV-14
                                          SGTS200S-TX-CSV-15
                                          SGTS200S-TX-CSV-16
                                          SGTS200S-TX-CSV-17
                                          SGTS200S-TX-CSV-18
                                          SGTS200S-TX-CSV-19
                                          SGTS200S-TX-CSV-20
                                          SGTS200S-TX-CSV-21
                                          SGTS200S-TX-CSV-22
                                          SGTS200S-TX-CSV-23
                                          SGTS200S-TX-CSV-24
                                          SGTS200S-TX-CSV-25
                                          SGTS200S-TX-CSV-26
                                          SGTS200S-TX-CSV-27
                                          SGTS200S-TX-CSV-28
                                          SGTS200S-TX-CSV-29
                                          SGTS200S-TX-CSV-30
                                          SGTS200S-TX-CSV-31
                                          SGTS200S-TX-CSV-32
                                          SGTS200S-TX-CSV-33
                                          SGTS200S-TX-CSV-34

                                          SGTS200S-TX-CSV-40
                                          SGTS200S-TX-CSV-41
                                          SGTS200S-TX-CSV-42

                                          SGTS200S-TX-CSV-48
                                          SGTS200S-TX-CSV-49
                                          SGTS200S-TX-CSV-50
                                          SGTS200S-TX-CSV-51

                                          SGTS200S-TX-CSV-57
                                          SGTS200S-TX-CSV-58
                                          SGTS200S-TX-CSV-59
                                          SGTS200S-TX-CSV-60

                                          SGTS200S-TX-CSV-66
                                          SGTS200S-TX-CSV-67
                                          SGTS200S-TX-CSV-68

                                          SGTS200S-TX-CSV-74
                                          SGTS200S-TX-CSV-75

                                          SGTS200S-TX-CSV-81

                                          SGTS200S-TX-CSV-87
                                          SGTS200S-TX-CSV-88
                                          SGTS200S-TX-CSV-89
                                          SGTS200S-TX-CSV-90
                                          SGTS200S-TX-CSV-91
                                          SGTS200S-TX-CSV-92
                                          SGTS200S-TX-CSV-93
                                          SGTS200S-TX-CSV-94
                                          SGTS200S-TX-CSV-95
                                          SGTS200S-TX-CSV-96
                                          SGTS200S-TX-CSV-97
                                          SGTS200S-TX-CSV-98
                                          SGTS200S-TX-CSV-99
                                          SGTS200S-TX-CSV-100
                                          SGTS200S-TX-CSV-101
                                          SGTS200S-TX-CSV-102
                                          SGTS200S-TX-CSV-103
                                          SGTS200S-TX-CSV-104
                                          SGTS200S-TX-CSV-105
                                          SGTS200S-TX-CSV-106
                                          SGTS200S-TX-CSV-107
                                          SGTS200S-TX-CSV-108
                                          SGTS200S-TX-CSV-109
                                          SGTS200S-TX-CSV-110
                                          SGTS200S-TX-CSV-111
                                          SGTS200S-TX-CSV-112
                                          SGTS200S-TX-CSV-113
                                          SGTS200S-TX-CSV-114
                                          SGTS200S-TX-CSV-115
                                          SGTS200S-TX-CSV-116
                                          SGTS200S-TX-CSV-117
                                          SGTS200S-TX-CSV-118
                                          SGTS200S-TX-CSV-119
                                          SGTS200S-TX-CSV-120
                                          SGTS200S-TX-CSV-121
                                          SGTS200S-TX-CSV-122
                                          SGTS200S-TX-CSV-123
           .
       212000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-TERMINA                  SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO PSC-ERRO-PGM

           PERFORM 310000-FECHA-SERVICOS

      *    CURSOR
           EXEC SQL
              CLOSE LST-OPER-EST
           END-EXEC
           SET  CURSOR-ABTO-NAO        TO TRUE

      *    ARQUIVO
           CLOSE SGTF200S

      *    RESUMO
           DISPLAY '--------------------------------------------------'
           DISPLAY '999  QT-REG LIDOS            : ' QT-REG-E
           DISPLAY '999  QT-REG LIDOS VALIDOS    : ' QT-REG-E-VAL
           DISPLAY '999  QT-REG GRAVADOS SGTF200S: ' QT-REG-S
           DISPLAY ' '
           DISPLAY '999 ' CTE-PROG ' 999 - FIM NORMAL'
           DISPLAY '--------------------------------------------------'
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       310000-FECHA-SERVICOS           SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO PSC-ERRO-PGM

           SET  W500-FIM-EXEA          TO TRUE
           MOVE 00000000               TO W500-TAM-MSG

           MOVE CD-IIB-MSTS8042        TO W500-ID-SRVC
           PERFORM 905000-PREPARA-SERVICOS

           MOVE CD-IIB-SGTS0084        TO W500-ID-SRVC
           PERFORM 905000-PREPARA-SERVICOS
           .
       310000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       905000-PREPARA-SERVICOS         SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO PSC-ERRO-PGM

           PERFORM 979500-CHAMA-IIBS5000

           IF                      NOT W500-RC-OK
               DISPLAY ' '
               DISPLAY 'PROGRAMA         : ' IIBS5000
               DISPLAY '    W500-IND-EXEA: '     W500-IND-EXEA
               DISPLAY '    W500-ID-SRVC : '     W500-ID-SRVC
               DISPLAY '    W500-RC      : '     W500-RC
               DISPLAY '    W500-TX-ERRO : '     W500-TX-ERRO
           END-IF
           .
       905000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970007-OBTEM-NM-DEPE            SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO PSC-ERRO-PGM

           IF           CD-BCO-CRD-VL-OPR  EQUAL
               CAFS0007-BANCO-INF                AND
                        CD-DEPE-CRD-VL-OPR EQUAL
               CAFS0007-AGE-INF
               CONTINUE
           ELSE
               INITIALIZE CAFS0007-DADOS
               MOVE          CD-BCO-CRD-VL-OPR
                 TO CAFS0007-BANCO-INF
               MOVE          CD-DEPE-CRD-VL-OPR
                 TO CAFS0007-AGE-INF

               MOVE LENGTH OF CAFS0007-DADOS TO EIBCALEN
               CALL CAFS0007           USING DFHEIBLK CAFS0007-DADOS
           END-IF

           IF  CAFS0007-RET-CODE       EQUAL ZEROS
               MOVE CAFS0007-NOME-AGEN
                 TO SGTS200S-NM-DEPE-CRD-VL-OPR
           ELSE
               DISPLAY ' '
               DISPLAY '         QT-REG-S : '          QT-REG-S
               DISPLAY 'CAFS0007-RET-CODE : ' CAFS0007-RET-CODE
               DISPLAY 'CAFS0007-SQLCODE  : ' CAFS0007-SQLCODE
               DISPLAY 'CAFS0007-SECTION  : ' CAFS0007-SECTION
               DISPLAY ' '
               DISPLAY 'CAFS0007-BANCO-INF: ' CAFS0007-BANCO-INF
               DISPLAY 'CAFS0007-AGE-INF  : ' CAFS0007-AGE-INF
           END-IF
           .
       970007-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970027-OBTEM-NM-TIP-COTA        SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO PSC-ERRO-PGM

           IF           CD-TIP-COTA-TIT EQUAL
               SGTST027-CD-TIP-COTA-TIT       AND
                        NR-VRS-TIP-COTA EQUAL
               SGTST027-NR-VRS-TIP-COTA
               CONTINUE
           ELSE
               INITIALIZE  SGTST027-DADOS
               SET  SGTST027-CONSULTAR TO TRUE
               MOVE          CD-TIP-COTA-TIT
                 TO SGTST027-CD-TIP-COTA-TIT
               MOVE          NR-VRS-TIP-COTA
                 TO SGTST027-NR-VRS-TIP-COTA

               MOVE LENGTH OF SGTST027-DADOS TO EIBCALEN
               CALL SGTST027           USING DFHEIBLK SGTST027-DADOS
           END-IF

           IF  SGTST027-CD-RTN-PGM     EQUAL ZEROS
               MOVE SGTST027-NM-TIP-COTA-TIT
                 TO SGTS200S-NM-TIP-COTA-TIT
           ELSE
               DISPLAY ' '
               DISPLAY '         QT-REG-S       : '
                                 QT-REG-S
               DISPLAY 'SGTST027-CD-RTN-PGM     : '
                        SGTST027-CD-RTN-PGM
               DISPLAY 'SGTST027-MSG-ERRO       : '
                        SGTST027-MSG-ERRO
               DISPLAY ' '
               DISPLAY 'SGTST027-CD-TIP-COTA-TIT: '
                        SGTST027-CD-TIP-COTA-TIT
               DISPLAY 'SGTST027-NR-VRS-TIP-COTA: '
                        SGTST027-NR-VRS-TIP-COTA
           END-IF
           .
       970027-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970029-OBTEM-NM-CTGR            SECTION.
      *-----------------------------------------------------------------
           MOVE 0014                   TO PSC-ERRO-PGM

           IF           CD-CTGR-TVM    EQUAL
               SGTST029-CD-CTGR-TVM
               CONTINUE
           ELSE
               INITIALIZE SGTST029-DADOS
               SET  SGTST029-CONSULTAR-COD-CTGR-BC TO TRUE
               MOVE          CD-CTGR-TVM
                 TO SGTST029-CD-CTGR-TVM

               MOVE LENGTH OF SGTST029-DADOS TO EIBCALEN
               CALL SGTST029           USING DFHEIBLK SGTST029-DADOS
           END-IF

           IF  SGTST029-CD-RTN-PGM     EQUAL ZEROS
               MOVE SGTST029-NM-CTGR-TVM
                TO  SGTS200S-NM-CTGR-TVM
           ELSE
               DISPLAY ' '
               DISPLAY '         QT-REG-S   : '          QT-REG-S
               DISPLAY 'SGTST029-CD-RTN-PGM : ' SGTST029-CD-RTN-PGM
               DISPLAY 'SGTST029-MSG-ERRO   : ' SGTST029-MSG-ERRO
               DISPLAY ' '
               DISPLAY 'SGTST029-CD-CTGR-TVM: ' SGTST029-CD-CTGR-TVM
           END-IF
           .
       970029-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970030-OBTEM-NM-ITC             SECTION.
      *-----------------------------------------------------------------
           MOVE 0015                   TO PSC-ERRO-PGM

           IF           CD-ITC-CVM-OPR EQUAL
               SGTST030-CD-ITC-CVM-OPR
               CONTINUE
           ELSE
               INITIALIZE  SGTST030-DADOS
               SET  SGTST030-CONSULTAR TO TRUE
               MOVE          CD-ITC-CVM-OPR
                 TO SGTST030-CD-ITC-CVM-OPR

               MOVE LENGTH OF SGTST030-DADOS TO EIBCALEN
               CALL SGTST030           USING DFHEIBLK SGTST030-DADOS
           END-IF

           IF  SGTST030-CD-RTN-PGM     EQUAL ZEROS
               MOVE SGTST030-NM-ITC-CVM
                 TO SGTS200S-NM-ITC-CVM
           ELSE
               DISPLAY ' '
               DISPLAY '         QT-REG-S      : '
                                 QT-REG-S
               DISPLAY 'SGTST030-CD-RTN-PGM    : '
                        SGTST030-CD-RTN-PGM
               DISPLAY 'SGTST030-MSG-ERRO      : '
                        SGTST030-MSG-ERRO
               DISPLAY ' '
               DISPLAY 'SGTST030-CD-ITC-CVM-OPR: '
                        SGTST030-CD-ITC-CVM-OPR
           END-IF
           .
       970030-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970053-OBTEM-NM-BCO             SECTION.
      *-----------------------------------------------------------------
           MOVE 0016                   TO PSC-ERRO-PGM

           IF       CD-BCO-CRD-VL-OPR  EQUAL
               S053-CD-CPSO            OF S053-ENTD
               CONTINUE
           ELSE
               INITIALIZE CAFS0053-DADOS
      *                  CD.COMPE
               MOVE       2
                 TO S053-FUC           OF S053-ENTD
               MOVE      CD-BCO-CRD-VL-OPR
                 TO S053-CD-CPSO       OF S053-ENTD

               MOVE LENGTH OF CAFS0053-DADOS TO EIBCALEN
               CALL CAFS0053           USING DFHEIBLK CAFS0053-DADOS
           END-IF

           IF  S053-CD-RTN             EQUAL ZEROS
               MOVE     S053-NM-INST
                 TO SGTS200S-NM-BCO-CRD-VL-OPR
           ELSE
               DISPLAY ' '
               DISPLAY '         QT-REG-S: '
                                 QT-REG-S
               DISPLAY 'PROGRAMA         : '
                        CAFS0053
               DISPLAY '    S053-CD-RTN  : '
                            S053-CD-RTN
               DISPLAY '    S053-MSG-RTN : '
                            S053-MSG-RTN
               DISPLAY '    S053-LCL-ERRO: '
                            S053-LCL-ERRO
               DISPLAY '    S053-SQLC    : '
                            S053-SQLC
               DISPLAY ' '
               DISPLAY '    S053-CD-CPSO : '
                            S053-CD-CPSO OF S053-ENTD
           END-IF
           .
       970053-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970081-OBTEM-NM-CLRG            SECTION.
      *-----------------------------------------------------------------
           MOVE 0017                   TO PSC-ERRO-PGM

           IF        CD-CMRA-OPR-ETRD  EQUAL
               K081C-CD-CLRG-E
               CONTINUE
           ELSE
               INITIALIZE ICLS081C-DADOS
               MOVE       CD-CMRA-OPR-ETRD
                 TO K081C-CD-CLRG-E
      *                   NM.CLEARING
               MOVE        2
                 TO K081C-CD-ACAO-E

               MOVE LENGTH OF ICLS081C-DADOS TO EIBCALEN
               CALL ICLS081C           USING DFHEIBLK ICLS081C-DADOS
           END-IF

           IF  KERRO-CD-RTN-PGM        EQUAL ZEROS
               MOVE    K081C-NM-CLRG-S        (1)
                 TO SGTS200S-NM-CMRA-OPR-ETRD
           ELSE
               DISPLAY ' '
               DISPLAY '      QT-REG-S    : '          QT-REG-S
               DISPLAY 'PROGRAMA          : ' ICLS081C
               DISPLAY 'KERRO-CD-RTN-PGM  : '    KERRO-CD-RTN-PGM
               DISPLAY 'KERRO-MSG-ERRO-PGM: '    KERRO-MSG-ERRO-PGM
               DISPLAY ' '
               DISPLAY 'K081C-CD-CLRG-E   : '    K081C-CD-CLRG-E
           END-IF
           .
       970081-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970084-OBTEM-DADOS-CLIENTES     SECTION.
      *-----------------------------------------------------------------
           MOVE 0018                   TO PSC-ERRO-PGM

           IF           CD-CTRA-TIT-VL  EQUAL
               SGTS0084-CD-CTRA-TIT-VL        AND
                        CD-OPR-ETRD-MCP EQUAL
               SGTS0084-CD-OPR-ETRD           AND
                        NR-VRS-OPR-ETRD EQUAL
               SGTS0084-NR-VRS-OPR-ETRD
               CONTINUE
           ELSE
               MOVE CD-IIB-SGTS0084    TO W500-ID-SRVC
               MOVE LENGTH OF SGTS0084-RQSC
                                       TO W500-TAM-MSG

               INITIALIZE SGTS0084-RQSC
               MOVE          CD-CTRA-TIT-VL
                 TO SGTS0084-CD-CTRA-TIT-VL
               MOVE          CD-OPR-ETRD-MCP
                 TO SGTS0084-CD-OPR-ETRD
               MOVE          NR-VRS-OPR-ETRD
                 TO SGTS0084-NR-VRS-OPR-ETRD
               MOVE SGTS0084-RQSC      TO AREA-DADOS-MSG

               PERFORM 979500-CHAMA-IIBS5000
               IF  W500-RC-OK
                   MOVE AREA-DADOS-MSG TO SGTS0084-RPST
                   MOVE ZEROS          TO    W0084-RC
               ELSE
                   MOVE W500-RC        TO    W0084-RC
                   MOVE W500-TX-ERRO   TO    W0084-TX-ERRO
               END-IF
           END-IF

           IF  W0084-RC                EQUAL ZEROS
               PERFORM 970084-MOVE-DADOS-CLIENTES
           ELSE
               DISPLAY ' '
               DISPLAY '         QT-REG-S: '
                                 QT-REG-S
               DISPLAY '    W500-RC      : '
                           W0084-RC
               DISPLAY '    W500-TX-ERRO : '
                           W0084-TX-ERRO
               DISPLAY ' '
               DISPLAY 'SGTS0084-CD-CTRA-TIT-VL : '
                        SGTS0084-CD-CTRA-TIT-VL
               DISPLAY 'SGTS0084-CD-OPR-ETRD    : '
                        SGTS0084-CD-OPR-ETRD
               DISPLAY 'SGTS0084-NR-VRS-OPR-ETRD: '
                        SGTS0084-NR-VRS-OPR-ETRD
           END-IF
           .
       970084-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970084-MOVE-DADOS-CLIENTES      SECTION.
      *-----------------------------------------------------------------
           MOVE 0019                   TO PSC-ERRO-PGM
      *    EMT
           MOVE SGTS0084-NM-CLI-EMT
             TO SGTS200S-NM-CLI-EMT
           MOVE SGTS0084-CD-RSCO-CLI-EMT
             TO SGTS200S-CD-RSCO-CLI-EMT
           MOVE SGTS0084-CD-GR-CLI-EMT
             TO SGTS200S-CD-GR-CLI-EMT
           MOVE SGTS0084-NM-GR-CLI-EMT
             TO SGTS200S-NM-GR-CLI-EMT
      *    IFLC
           MOVE SGTS0084-NM-CLI-IFLC-LMCR
             TO SGTS200S-NM-CLI-IFLC
           MOVE SGTS0084-CD-RSCO-CLI-IFLC-LMCR
             TO SGTS200S-CD-RSCO-CLI-IFLC
           MOVE SGTS0084-CD-GR-CLI-IFLC-LMCR
             TO SGTS200S-CD-GR-CLI-IFLC
           MOVE SGTS0084-NM-GR-CLI-IFLC-LMCR
             TO SGTS200S-NM-GR-CLI-IFLC
      *    OGM
           MOVE SGTS0084-NM-CLI-CDT
             TO SGTS200S-NM-CLI-OGM
           MOVE SGTS0084-CD-RSCO-CLI-CDT
             TO SGTS200S-CD-RSCO-CLI-OGM
           MOVE SGTS0084-CD-GR-CLI-CDT
             TO SGTS200S-CD-GR-CLI-OGM
           MOVE SGTS0084-NM-GR-CLI-CDT
             TO SGTS200S-NM-GR-CLI-OGM
      *    FDAR
           MOVE SGTS0084-NM-CLI-AGT-FDAR
             TO SGTS200S-NM-CLI-FDAR
           MOVE SGTS0084-CD-GR-CLI-AGT-FDAR
             TO SGTS200S-CD-GR-CLI-FDAR
           MOVE SGTS0084-NM-GR-CLI-AGT-FDAR
             TO SGTS200S-NM-GR-CLI-FDAR
      *    ADM
           MOVE SGTS0084-NM-CLI-ADM
             TO SGTS200S-NM-CLI-ADM
           MOVE SGTS0084-CD-GR-CLI-ADM
             TO SGTS200S-CD-GR-CLI-ADM
           MOVE SGTS0084-NM-GR-CLI-ADM
             TO SGTS200S-NM-GR-CLI-ADM
           .
       970084-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970153-OBTEM-TX-DCR-TIP-GRT     SECTION.
      *-----------------------------------------------------------------
           MOVE 0020                   TO PSC-ERRO-PGM

           IF           CD-ESPC-DBTR-OPR EQUAL
               CACS153C-CD-TIP-GRT
               CONTINUE
           ELSE
               INITIALIZE CACS153C-DADOS
               SET  CACS153C-CONSULTAR TO TRUE
               MOVE          CD-ESPC-DBTR-OPR
                 TO CACS153C-CD-TIP-GRT

               MOVE LENGTH OF CACS153C-DADOS TO EIBCALEN
               CALL CACS153C           USING DFHEIBLK CACS153C-DADOS
           END-IF

           IF  CACS153C-CD-RTN-PGM     EQUAL ZEROS
               MOVE CACS153C-TX-DCR-TIP-GRT
                 TO SGTS200S-TX-DCR-TIP-GRT
           ELSE
               DISPLAY ' '
               DISPLAY '         QT-REG-S  : '          QT-REG-S
               DISPLAY 'CACS153C-CD-RTN-PGM: ' CACS153C-CD-RTN-PGM
               DISPLAY 'CACS153C-MSG-ERRO  : ' CACS153C-MSG-ERRO
               DISPLAY ' '
               DISPLAY 'CACS153C-CD-TIP-GRT: ' CACS153C-CD-TIP-GRT
           END-IF
           .
       970153-SAI.
           EXIT.
      *-----------------------------------------------------------------
       978042-OBTEM-NM-UOR             SECTION.
      *-----------------------------------------------------------------
           MOVE 0021                   TO PSC-ERRO-PGM

           IF           CD-UOR-DTTR-OPR EQUAL
               MSTS8042-CD-UOR
               CONTINUE
           ELSE
               MOVE CD-IIB-MSTS8042    TO W500-ID-SRVC
               MOVE LENGTH OF MSTS8042-RQSC
                                       TO W500-TAM-MSG

               INITIALIZE MSTS8042-RQSC
               MOVE          CD-UOR-DTTR-OPR
                 TO MSTS8042-CD-UOR
               MOVE MSTS8042-RQSC      TO AREA-DADOS-MSG

               PERFORM 979500-CHAMA-IIBS5000
               IF  W500-RC-OK
                   MOVE AREA-DADOS-MSG TO MSTS8042-RPST
                   MOVE ZEROS          TO    W8042-RC
               ELSE
                   MOVE W500-RC        TO    W8042-RC
                   MOVE W500-TX-ERRO   TO    W8042-TX-ERRO
               END-IF
           END-IF

           IF  W8042-RC                EQUAL ZEROS
               MOVE MSTS8042-NM-UOR-RDZ
                 TO SGTS200S-NM-UOR-DTTR-OPR
           ELSE
               DISPLAY ' '
               DISPLAY '         QT-REG-S: '          QT-REG-S
               DISPLAY '    W500-RC      : '    W8042-RC
               DISPLAY '    W500-TX-ERRO : '    W8042-TX-ERRO
               DISPLAY ' '
               DISPLAY 'MSTS8042-CD-UOR  : ' MSTS8042-CD-UOR
           END-IF
           .
       978042-SAI.
           EXIT.
      *-----------------------------------------------------------------
       979500-CHAMA-IIBS5000           SECTION.
      *-----------------------------------------------------------------
           MOVE 0022                   TO PSC-ERRO-PGM

           CALL IIBS5000               USING W500-AREA
                                             AREA-DADOS-MSG
           .
       979500-SAI.
           EXIT.
      *-----------------------------------------------------------------
       980200-GRAVA-SGTF200S           SECTION.
      *-----------------------------------------------------------------
           MOVE 0023                   TO PSC-ERRO-PGM

           WRITE SGTF200S-REG-FD       FROM SGTF200S-REG

           ADD  1                      TO QT-REG-S
           .
       980200-SAI.
           EXIT.
      *------------
      * 999000-ERRO
      *----------------
      * 999001-ERRO-001
      *----------------
       COPY HLPKE004.

           DISPLAY '888 ' CTE-PROG ' PROGRAMA: ' PGM-ERRO
           DISPLAY '888 ' CTE-PROG ' CANCELADO POR'
           DISPLAY '888 ' CTE-PROG '   '         MSG-ERRO
           DISPLAY '888 ' CTE-PROG ' CD-RTN  : ' CD-RTN-PGM
           DISPLAY '888 ' CTE-PROG ' TIP-RTN : ' TIP-RTN-PGM
           DISPLAY '888 ' CTE-PROG ' PSC-ERRO: ' PSC-ERRO-PGM
           DISPLAY '888 ' CTE-PROG ' QT-REG-E: ' QT-REG-E
           DISPLAY '888 ' CTE-PROG ' QT-REG-S: ' QT-REG-S
           DISPLAY '888 ' CTE-PROG ' MAIS INFORMACOES: VIDE DUMP'

           CALL SBABEND
           .
      *----------------
      * 999002-ERRO-DB2
      *----------------
       COPY HLPKE005.

      *-----------------------------------------------------------------
      *                  FIM DO PROGRAMA SGTP0200                      *
      *-----------------------------------------------------------------
