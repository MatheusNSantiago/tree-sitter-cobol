      *-----------------------------------------------------------------
      * Programa..: SGTSLC05
      * Analista..: F8650584 - Roberto Fernandes Lima
      * Autor.....: F8650584 - Roberto Fernandes Lima
      * Data......: 02/03/2019
      * Objetivo..: Listar Operações Estruturadas Vigentes
      *             Consulta Tabelas: DB2SGT.OPR_ETRD_MCP,
      *             DB2SGT.EST_OPR_ETRD e DB2SGT.EST_VRS_OPR_ETRD
      * Compilação: Cobol/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 02/03/2019 - F8650584 - DE 2016/145195 - Implantacao
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSLC05.

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
           '*** Storage Section Comeca Aqui ***'.
       77  CTE-PROG                    PIC  X(16) VALUE
                                       '*** SGTSLC05 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  IIBP2000                PIC  X(08) VALUE 'IIBP2000'.

       77  OPERACAO-ENCARTEIRADA       PIC S9(09) COMP-5 VALUE 0005.
       77  VERSAO-APROVADA             PIC S9(04) COMP-5 VALUE 0003.
       77  CLIENTE-EMISSOR             PIC S9(04) COMP-5 VALUE 0001.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informações
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE ZEROS.

       01  AUXILIARES-PESQUISA.
           03  CD-CTRA-TIT-INC         PIC S9(09) COMP-5 VALUE ZEROS.
           03  CD-CTRA-TIT-FIM         PIC S9(09) COMP-5 VALUE ZEROS.
           03  CD-OPR-ETRD-INC         PIC S9(11) COMP-3 VALUE ZEROS.
           03  CD-OPR-ETRD-FIM         PIC S9(11) COMP-3 VALUE ZEROS.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-TX.
           03  CD-OPR-ETRD             PIC  9(17).

      *--> Campos utilizados para leitura
       01  OPERACOES-VIGENTES.
           03  CD-CTRA-TIT             PIC S9(09) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  CD-OPR-ETRD-MCP         PIC S9(11) COMP-3
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  NR-VRS-OPR-ETRD         PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  CD-TIT-OPR-ETRD         PIC  X(12)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03  CD-TIT-OPR-ETRD-N       PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  NR-DVS-TIT              PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  NR-DVS-TIT-N            PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  CD-TIP-VRS-OPR          PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  CD-TIP-EST-OPR          PIC S9(09) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  CD-TIP-EST-OPR-N        PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  CD-TIP-EST-VRS          PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  CD-PRD                  PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  CD-MDLD                 PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  QT-PAPL-TIT-EMS         PIC S9(09) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  VL-PU-EMS-TIT-OPR       PIC S9(09)V9(08) COMP-3
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  DT-EMS-TIT-OPR          PIC  X(08)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03  DT-EMS-TIT-OPR-N        PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  QT-PAPL-TIT-CPR         PIC S9(09) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  VL-PU-CPR-TIT-OPR       PIC S9(09)V9(08) COMP-3
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  VL-CPR-OPR              PIC S9(15)V9(02) COMP-3
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  DT-CPR-OPR              PIC  X(08)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03  DT-CPR-OPR-N            PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  CD-UOR-DTTR-OPR         PIC S9(09) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  DT-VNCT-OPR             PIC  X(08)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03  DT-VNCT-OPR-N           PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  DT-LQDC-OPR-DRIV        PIC  X(08)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03  DT-LQDC-OPR-DRIV-N      PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  DT-INC-VGC-VRS-OPR      PIC  X(08)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03  DT-INC-VGC-VRS-OPR-N    PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  DT-EVT-VCLD-VRS         PIC  X(08)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03  DT-EVT-VCLD-VRS-N       PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  VL-EVT-VCLD-VRS         PIC S9(15)V9(02) COMP-3
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  VL-EVT-VCLD-VRS-N       PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  TX-OBS-EVT              PIC  X(100)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03  TX-OBS-EVT-N            PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  CD-USU-ATL              PIC  X(08)
                                       OCCURS 0101 TIMES  VALUE SPACES.

      *--> Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE ZEROS.
               88  FIM-CURSOR-PSQ                        VALUE +700.
           03  IN-CURSOR-PSQ-ABTO      PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO                VALUE 'S'.


      *--> Campos utilizados para manipulação de container
       01  GRP-CONTAINER.
           03  NM-CNL                  PIC  X(16) VALUE SPACES.
           03  QT-CCT-LIDO             PIC S9(09) COMP-5 VALUE ZEROS.
           03  QT-CCT-EPRO             PIC S9(09) COMP-5 VALUE ZEROS.
           03  VL-EXBR-1               PIC  9(10) VALUE ZEROS.
           03  VL-EXBR-2               PIC  9(10) VALUE ZEROS.
           03  NM-PGM                  PIC  X(08) VALUE SPACES.
           03  NM-CTNR                 PIC  X(16) VALUE SPACES.
           03  NM-ACAO                 PIC  X(03) VALUE SPACES.
               88 GET-CONTAINER                   VALUE 'GET'.
               88 PUT-CONTAINER                   VALUE 'PUT'.
               88 DEL-CONTAINER                   VALUE 'DEL'.

      *--> Indicadores dos tipos de erro previstos na execução do módulo
       01  IN-TIP-ERRO                 PIC  9(01) VALUE ZEROS.
           88 SEM-ERRO                            VALUE 0000.
           88 ERRO-CONTAINER                      VALUE 0001.
           88 ERRO-TAMANHO-AREA                   VALUE 0002.
           88 ERRO-CANAL                          VALUE 0003.
           88 ERRO-LOG                            VALUE 0004.

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book para captura de abend cics da bloco código MSGKP999
-INC HLPKW999

      *--> Book do container de requisição e resposta deste módulo
       01  SGTSLC05-DADOS.
-INC SGTKLC05

      *-----------------------------------------------------------------
      *    Cursores de Pesquisa
      *-----------------------------------------------------------------

      *--> Cursor de Pesquisa 01
      *    Listar todos Operações Vigentes de todas as Carteiras
      *    ordenadas pela Carteira, Titulo e Numero Divisao
      *    Estado da Operação - 0005 Encarteirada
      *    Estado da Versao - 0003 Aprovada
           EXEC SQL
           DECLARE CURSOR-PSQ-01 CURSOR
                     WITH ROWSET POSITIONING FOR
           SELECT
                   A.CD_CTRA_TIT_VL
                 , A.CD_OPR_ETRD_MCP
                 , A.NR_VRS_OPR_ETRD
                 , IFNULL(A.CD_TIT_OPR_ETRD, ' ' )
                 , IFNULL(A.NR_DVS_TIT, 0000 )
                 , A.CD_TIP_VRS_OPR
                 , IFNULL(B.CD_TIP_EST_OPR, 0000 )
                 , C.CD_TIP_EST_VRS
                 , A.CD_PRD
                 , A.CD_MDLD
                 , A.QT_PAPL_TIT_EMS
                 , A.VL_PU_EMS_TIT_OPR
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_EMS_TIT_OPR , 'DDMMYYYY'),
                          0008 ), '01010001')
                 , A.QT_PAPL_TIT_CPR
                 , A.VL_PU_CPR_TIT_OPR
                 , A.VL_CPR_OPR
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_CPR_OPR , 'DDMMYYYY'),
                          0008), '01010001')
                 , A.CD_UOR_DTTR_OPR
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_VNCT_OPR , 'DDMMYYYY'),
                          0008), '31129999')
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_LQDC_OPR_DRIV , 'DDMMYYYY'),
                          0008), '31129999')
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_INC_VGC_VRS_OPR , 'DDMMYYYY'),
                          0008), '01010001')
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_EVT_VCLD_VRS , 'DDMMYYYY'),
                          0008), '01010001')
                 , IFNULL(A.VL_EVT_VCLD_VRS, 0 )
                 , IFNULL(A.TX_OBS_EVT, ' ' )
                 , A.CD_USU_ULT_ATL
           FROM
                   DB2SGT.OPR_ETRD_MCP A
      *--> Busca Operacoes no Estado de Encarteirada
           INNER JOIN
                   DB2SGT.EST_OPR_ETRD  B
           ON
                   B.CD_CTRA_TIT_VL        = A.CD_CTRA_TIT_VL
           AND     B.CD_OPR_ETRD_MCP       = A.CD_OPR_ETRD_MCP
           AND     B.NR_VRS_OPR_ETRD       = A.NR_VRS_OPR_ETRD
           AND     B.CD_TIP_EST_OPR        = :OPERACAO-ENCARTEIRADA
           AND     B.TS_EST_OPR_ETRD       =
                  (SELECT
                             MAX(B0.TS_EST_OPR_ETRD)
                   FROM
                                  DB2SGT.EST_OPR_ETRD B0
                   WHERE
                           B0.CD_CTRA_TIT_VL
                                           = B.CD_CTRA_TIT_VL
                   AND     B0.CD_OPR_ETRD_MCP
                                           = B.CD_OPR_ETRD_MCP)
      *--> Busca Versoes da Operacoes no Estado de Aprovada
           INNER JOIN
                   DB2SGT.EST_VRS_OPR_ETRD  C
           ON
                   C.CD_CTRA_TIT_VL       = A.CD_CTRA_TIT_VL
           AND     C.CD_OPR_ETRD_MCP      = A.CD_OPR_ETRD_MCP
           AND     C.NR_VRS_OPR_ETRD      = A.NR_VRS_OPR_ETRD
           AND     C.CD_TIP_EST_VRS       = :VERSAO-APROVADA
           AND     C.TS_EST_VRS           =
                  (SELECT
                           MAX(C0.TS_EST_VRS)
                   FROM
                                  DB2SGT.EST_VRS_OPR_ETRD C0
                   WHERE
                           C0.CD_CTRA_TIT_VL
                                           = C.CD_CTRA_TIT_VL
                   AND     C0.CD_OPR_ETRD_MCP
                                           = C.CD_OPR_ETRD_MCP)
      *--> Verifica se Existe Cliente Emissor Cadastrado
           INNER JOIN
                   DB2SGT.CLI_OPR_ETRD D
           ON
                   D.CD_CTRA_TIT_VL      = A.CD_CTRA_TIT_VL
           AND     D.CD_OPR_ETRD_MCP     = A.CD_OPR_ETRD_MCP
           AND     D.NR_VRS_OPR_ETRD     = A.NR_VRS_OPR_ETRD
           AND     D.CD_TIP_FUC_CLI      = :CLIENTE-EMISSOR
           WHERE
                   A.CD_CTRA_TIT_VL      = B.CD_CTRA_TIT_VL
           AND     A.CD_OPR_ETRD_MCP     = B.CD_OPR_ETRD_MCP
           AND     A.NR_VRS_OPR_ETRD     = B.NR_VRS_OPR_ETRD
           AND     A.DT_CPR_OPR            < CURRENT DATE
           AND     A.DT_INC_VGC_VRS_OPR    < CURRENT DATE
           AND     A.DT_VNCT_OPR           > CURRENT DATE
      *-----------> Paginacao
           AND   ((A.CD_CTRA_TIT_VL   > :SGTSLC05-CD-CTRA-TIT-ULT)
           OR     (A.CD_CTRA_TIT_VL   = :SGTSLC05-CD-CTRA-TIT-ULT   AND
                   A.CD_TIT_OPR_ETRD  > :SGTSLC05-CD-TIT-OPR-ULT)
           OR     (A.CD_CTRA_TIT_VL   = :SGTSLC05-CD-CTRA-TIT-ULT   AND
                   A.CD_TIT_OPR_ETRD  = :SGTSLC05-CD-TIT-OPR-ULT    AND
                   A.NR_DVS_TIT       > :SGTSLC05-NR-DVS-TIT-ULT))
           ORDER BY
                   A.CD_CTRA_TIT_VL
                 , A.CD_TIT_OPR_ETRD
                 , A.NR_DVS_TIT
           END-EXEC.

      *--> Cursor de Pesquisa 02
      *    Listar todos Operações Vigentes de uma Carteira
      *    ordenadas pela Carteira, Titulo e Numero Divisao
      *    Estado da Operação - 0005 Encarteirada
      *    Estado da Versao - 0003 Aprovada
           EXEC SQL
           DECLARE CURSOR-PSQ-02 CURSOR
                     WITH ROWSET POSITIONING FOR
           SELECT
                   A.CD_CTRA_TIT_VL
                 , A.CD_OPR_ETRD_MCP
                 , A.NR_VRS_OPR_ETRD
                 , IFNULL(A.CD_TIT_OPR_ETRD, ' ' )
                 , IFNULL(A.NR_DVS_TIT, 0000 )
                 , A.CD_TIP_VRS_OPR
                 , IFNULL(B.CD_TIP_EST_OPR, 0000 )
                 , C.CD_TIP_EST_VRS
                 , A.CD_PRD
                 , A.CD_MDLD
                 , A.QT_PAPL_TIT_EMS
                 , A.VL_PU_EMS_TIT_OPR
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_EMS_TIT_OPR , 'DDMMYYYY'),
                          0008 ), '01010001')
                 , A.QT_PAPL_TIT_CPR
                 , A.VL_PU_CPR_TIT_OPR
                 , A.VL_CPR_OPR
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_CPR_OPR , 'DDMMYYYY'),
                          0008), '01010001')
                 , A.CD_UOR_DTTR_OPR
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_VNCT_OPR , 'DDMMYYYY'),
                          0008), '31129999')
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_LQDC_OPR_DRIV , 'DDMMYYYY'),
                          0008), '31129999')
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_INC_VGC_VRS_OPR , 'DDMMYYYY'),
                          0008), '01010001')
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_EVT_VCLD_VRS , 'DDMMYYYY'),
                          0008), '01010001')
                 , IFNULL(A.VL_EVT_VCLD_VRS, 0 )
                 , IFNULL(A.TX_OBS_EVT, ' ' )
                 , A.CD_USU_ULT_ATL
           FROM
                   DB2SGT.OPR_ETRD_MCP A
      *--> Busca Operacoes no Estado de Encarteirada
           INNER JOIN
                   DB2SGT.EST_OPR_ETRD  B
           ON
                   B.CD_CTRA_TIT_VL        = A.CD_CTRA_TIT_VL
           AND     B.CD_OPR_ETRD_MCP       = A.CD_OPR_ETRD_MCP
           AND     B.NR_VRS_OPR_ETRD       = A.NR_VRS_OPR_ETRD
           AND     B.CD_TIP_EST_OPR        = :OPERACAO-ENCARTEIRADA
           AND     B.TS_EST_OPR_ETRD       =
                  (SELECT
                             MAX(B0.TS_EST_OPR_ETRD)
                   FROM
                                  DB2SGT.EST_OPR_ETRD B0
                   WHERE
                           B0.CD_CTRA_TIT_VL
                                           = B.CD_CTRA_TIT_VL
                   AND     B0.CD_OPR_ETRD_MCP
                                           = B.CD_OPR_ETRD_MCP)
      *--> Busca Versoes da Operacoes no Estado de Aprovada
           INNER JOIN
                   DB2SGT.EST_VRS_OPR_ETRD  C
           ON
                   C.CD_CTRA_TIT_VL       = A.CD_CTRA_TIT_VL
           AND     C.CD_OPR_ETRD_MCP      = A.CD_OPR_ETRD_MCP
           AND     C.NR_VRS_OPR_ETRD      = A.NR_VRS_OPR_ETRD
           AND     C.CD_TIP_EST_VRS       = :VERSAO-APROVADA
           AND     C.TS_EST_VRS           =
                  (SELECT
                           MAX(C0.TS_EST_VRS)
                   FROM
                                  DB2SGT.EST_VRS_OPR_ETRD C0
                   WHERE
                           C0.CD_CTRA_TIT_VL
                                           = C.CD_CTRA_TIT_VL
                   AND     C0.CD_OPR_ETRD_MCP
                                           = C.CD_OPR_ETRD_MCP)
      *--> Verifica se Existe Cliente Emissor Cadastrado
           INNER JOIN
                   DB2SGT.CLI_OPR_ETRD D
           ON
                   D.CD_CTRA_TIT_VL      = A.CD_CTRA_TIT_VL
           AND     D.CD_OPR_ETRD_MCP     = A.CD_OPR_ETRD_MCP
           AND     D.NR_VRS_OPR_ETRD     = A.NR_VRS_OPR_ETRD
           AND     D.CD_TIP_FUC_CLI      = :CLIENTE-EMISSOR
           WHERE
                  (A.CD_CTRA_TIT_VL  >= :CD-CTRA-TIT-INC            AND
                   A.CD_CTRA_TIT_VL  <= :CD-CTRA-TIT-FIM)
           AND     A.DT_CPR_OPR            < CURRENT DATE
           AND     A.DT_INC_VGC_VRS_OPR    < CURRENT DATE
           AND     A.DT_VNCT_OPR           > CURRENT DATE
      *-----------> Paginacao
           AND   ((A.CD_TIT_OPR_ETRD  > :SGTSLC05-CD-TIT-OPR-ULT)
           OR     (A.CD_TIT_OPR_ETRD  = :SGTSLC05-CD-TIT-OPR-ULT    AND
                   A.NR_DVS_TIT       > :SGTSLC05-NR-DVS-TIT-ULT))
           ORDER BY
                   A.CD_CTRA_TIT_VL
                 , A.CD_TIT_OPR_ETRD
                 , A.NR_DVS_TIT

           END-EXEC.

      *--> Cursor de Pesquisa 03
      *    Listar todas Operacoes Vigentes a partir de Titulo
      *    ordenadas pela Carteira, Titulo e Numero Divisao
      *    Estado da Operação - 0005 Encarteirada
      *    Estado da Versao - 0003 Aprovada
           EXEC SQL
           DECLARE CURSOR-PSQ-03 CURSOR
                     WITH ROWSET POSITIONING FOR
           SELECT
                   A.CD_CTRA_TIT_VL
                 , A.CD_OPR_ETRD_MCP
                 , A.NR_VRS_OPR_ETRD
                 , IFNULL(A.CD_TIT_OPR_ETRD, ' ' )
                 , IFNULL(A.NR_DVS_TIT, 0000 )
                 , A.CD_TIP_VRS_OPR
                 , IFNULL(B.CD_TIP_EST_OPR, 0000 )
                 , C.CD_TIP_EST_VRS
                 , A.CD_PRD
                 , A.CD_MDLD
                 , A.QT_PAPL_TIT_EMS
                 , A.VL_PU_EMS_TIT_OPR
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_EMS_TIT_OPR , 'DDMMYYYY'),
                          0008 ), '01010001')
                 , A.QT_PAPL_TIT_CPR
                 , A.VL_PU_CPR_TIT_OPR
                 , A.VL_CPR_OPR
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_CPR_OPR , 'DDMMYYYY'),
                          0008), '01010001')
                 , A.CD_UOR_DTTR_OPR
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_VNCT_OPR , 'DDMMYYYY'),
                          0008), '31129999')
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_LQDC_OPR_DRIV , 'DDMMYYYY'),
                          0008), '31129999')
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_INC_VGC_VRS_OPR , 'DDMMYYYY'),
                          0008), '01010001')
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_EVT_VCLD_VRS , 'DDMMYYYY'),
                          0008), '01010001')
                 , IFNULL(A.VL_EVT_VCLD_VRS, 0 )
                 , IFNULL(A.TX_OBS_EVT, ' ' )
                 , A.CD_USU_ULT_ATL
           FROM
                   DB2SGT.OPR_ETRD_MCP A
      *--> Busca Operacoes no Estado de Encarteirada
           INNER JOIN
                   DB2SGT.EST_OPR_ETRD  B
           ON
                   B.CD_CTRA_TIT_VL        = A.CD_CTRA_TIT_VL
           AND     B.CD_OPR_ETRD_MCP       = A.CD_OPR_ETRD_MCP
           AND     B.NR_VRS_OPR_ETRD       = A.NR_VRS_OPR_ETRD
           AND     B.CD_TIP_EST_OPR        = :OPERACAO-ENCARTEIRADA
           AND     B.TS_EST_OPR_ETRD       =
                  (SELECT
                             MAX(B0.TS_EST_OPR_ETRD)
                   FROM
                                  DB2SGT.EST_OPR_ETRD B0
                   WHERE
                           B0.CD_CTRA_TIT_VL
                                           = B.CD_CTRA_TIT_VL
                   AND     B0.CD_OPR_ETRD_MCP
                                           = B.CD_OPR_ETRD_MCP)
      *--> Busca Versoes da Operacoes no Estado de Aprovada
           INNER JOIN
                   DB2SGT.EST_VRS_OPR_ETRD  C
           ON
                   C.CD_CTRA_TIT_VL       = A.CD_CTRA_TIT_VL
           AND     C.CD_OPR_ETRD_MCP      = A.CD_OPR_ETRD_MCP
           AND     C.NR_VRS_OPR_ETRD      = A.NR_VRS_OPR_ETRD
           AND     C.CD_TIP_EST_VRS       = :VERSAO-APROVADA
           AND     C.TS_EST_VRS           =
                  (SELECT
                           MAX(C0.TS_EST_VRS)
                   FROM
                                  DB2SGT.EST_VRS_OPR_ETRD C0
                   WHERE
                           C0.CD_CTRA_TIT_VL
                                           = C.CD_CTRA_TIT_VL
                   AND     C0.CD_OPR_ETRD_MCP
                                           = C.CD_OPR_ETRD_MCP)
      *--> Verifica se Existe Cliente Emissor Cadastrado
           INNER JOIN
                   DB2SGT.CLI_OPR_ETRD D
           ON
                   D.CD_CTRA_TIT_VL      = A.CD_CTRA_TIT_VL
           AND     D.CD_OPR_ETRD_MCP     = A.CD_OPR_ETRD_MCP
           AND     D.NR_VRS_OPR_ETRD     = A.NR_VRS_OPR_ETRD
           AND     D.CD_TIP_FUC_CLI      = :CLIENTE-EMISSOR
           WHERE
                  (A.CD_CTRA_TIT_VL  >= :CD-CTRA-TIT-INC            AND
                   A.CD_CTRA_TIT_VL  <= :CD-CTRA-TIT-FIM)
           AND     UPPER(COALESCE(A.CD_TIT_OPR_ETRD, ' ')) LIKE
                                 UPPER(:SGTSLC05-CD-TIT-OPR-PSQ)
           AND     A.DT_CPR_OPR            < CURRENT DATE
           AND     A.DT_INC_VGC_VRS_OPR    < CURRENT DATE
           AND     A.DT_VNCT_OPR           > CURRENT DATE
      *-----------> Paginacao
             AND ((A.CD_CTRA_TIT_VL   > :SGTSLC05-CD-CTRA-TIT-ULT)
             OR   (A.CD_CTRA_TIT_VL   = :SGTSLC05-CD-CTRA-TIT-ULT   AND
                   A.CD_TIT_OPR_ETRD  > :SGTSLC05-CD-TIT-OPR-ULT)
             OR   (A.CD_CTRA_TIT_VL   = :SGTSLC05-CD-CTRA-TIT-ULT   AND
                   A.CD_TIT_OPR_ETRD  = :SGTSLC05-CD-TIT-OPR-ULT    AND
                   A.NR_DVS_TIT       > :SGTSLC05-NR-DVS-TIT-ULT))
             ORDER BY
                   A.CD_CTRA_TIT_VL
                 , A.CD_TIT_OPR_ETRD
                 , A.NR_DVS_TIT
           END-EXEC.

      *--> Cursor de Pesquisa 04
      *    Listar todas Operacoes Estruturadas Vigentes
      *    ordenadas pela Carteira, Operacao e Versao
      *    Estado da Operação - 0005 Encarteirada
      *    Estado da Versao - 0003 Aprovada
           EXEC SQL
           DECLARE CURSOR-PSQ-04 CURSOR
                     WITH ROWSET POSITIONING FOR
           SELECT
                   A.CD_CTRA_TIT_VL
                 , A.CD_OPR_ETRD_MCP
                 , A.NR_VRS_OPR_ETRD
                 , IFNULL(A.CD_TIT_OPR_ETRD, ' ' )
                 , IFNULL(A.NR_DVS_TIT, 0000 )
                 , A.CD_TIP_VRS_OPR
                 , IFNULL(B.CD_TIP_EST_OPR, 0000 )
                 , C.CD_TIP_EST_VRS
                 , A.CD_PRD
                 , A.CD_MDLD
                 , A.QT_PAPL_TIT_EMS
                 , A.VL_PU_EMS_TIT_OPR
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_EMS_TIT_OPR , 'DDMMYYYY'),
                          0008 ), '01010001')
                 , A.QT_PAPL_TIT_CPR
                 , A.VL_PU_CPR_TIT_OPR
                 , A.VL_CPR_OPR
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_CPR_OPR , 'DDMMYYYY'),
                          0008), '01010001')
                 , A.CD_UOR_DTTR_OPR
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_VNCT_OPR , 'DDMMYYYY'),
                          0008), '31129999')
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_LQDC_OPR_DRIV , 'DDMMYYYY'),
                          0008), '31129999')
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_INC_VGC_VRS_OPR , 'DDMMYYYY'),
                          0008), '01010001')
                 , IFNULL(CHAR(
                          TO_CHAR(A.DT_EVT_VCLD_VRS , 'DDMMYYYY'),
                          0008), '01010001')
                 , IFNULL(A.VL_EVT_VCLD_VRS, 0 )
                 , IFNULL(A.TX_OBS_EVT, ' ' )
                 , A.CD_USU_ULT_ATL
           FROM
                   DB2SGT.OPR_ETRD_MCP A
      *--> Busca Operacoes no Estado de Encarteirada
           INNER JOIN
                   DB2SGT.EST_OPR_ETRD  B
           ON
                   B.CD_CTRA_TIT_VL        = A.CD_CTRA_TIT_VL
           AND     B.CD_OPR_ETRD_MCP       = A.CD_OPR_ETRD_MCP
           AND     B.NR_VRS_OPR_ETRD       = A.NR_VRS_OPR_ETRD
           AND     B.CD_TIP_EST_OPR        = :OPERACAO-ENCARTEIRADA
           AND     B.TS_EST_OPR_ETRD       =
                  (SELECT
                             MAX(B0.TS_EST_OPR_ETRD)
                   FROM
                                  DB2SGT.EST_OPR_ETRD B0
                   WHERE
                           B0.CD_CTRA_TIT_VL
                                           = B.CD_CTRA_TIT_VL
                   AND     B0.CD_OPR_ETRD_MCP
                                           = B.CD_OPR_ETRD_MCP)
      *--> Busca Versoes da Operacoes no Estado de Aprovada
           INNER JOIN
                   DB2SGT.EST_VRS_OPR_ETRD  C
           ON
                   C.CD_CTRA_TIT_VL       = A.CD_CTRA_TIT_VL
           AND     C.CD_OPR_ETRD_MCP      = A.CD_OPR_ETRD_MCP
           AND     C.NR_VRS_OPR_ETRD      = A.NR_VRS_OPR_ETRD
           AND     C.CD_TIP_EST_VRS       = :VERSAO-APROVADA
           AND     C.TS_EST_VRS           =
                  (SELECT
                           MAX(C0.TS_EST_VRS)
                   FROM
                                  DB2SGT.EST_VRS_OPR_ETRD C0
                   WHERE
                           C0.CD_CTRA_TIT_VL
                                           = C.CD_CTRA_TIT_VL
                   AND     C0.CD_OPR_ETRD_MCP
                                           = C.CD_OPR_ETRD_MCP)
      *--> Verifica se Existe Cliente Emissor Cadastrado
           INNER JOIN
                   DB2SGT.CLI_OPR_ETRD D
           ON
                   D.CD_CTRA_TIT_VL      = A.CD_CTRA_TIT_VL
           AND     D.CD_OPR_ETRD_MCP     = A.CD_OPR_ETRD_MCP
           AND     D.NR_VRS_OPR_ETRD     = A.NR_VRS_OPR_ETRD
           AND     D.CD_TIP_FUC_CLI      = :CLIENTE-EMISSOR
           WHERE
                  (A.CD_CTRA_TIT_VL  >= :CD-CTRA-TIT-INC            AND
                   A.CD_CTRA_TIT_VL  <= :CD-CTRA-TIT-FIM)
           AND    (A.CD_OPR_ETRD_MCP >= :CD-OPR-ETRD-INC            AND
                   A.CD_OPR_ETRD_MCP <= :CD-OPR-ETRD-FIM)
           AND     A.DT_CPR_OPR            < CURRENT DATE
           AND     A.DT_INC_VGC_VRS_OPR    < CURRENT DATE
           AND     A.DT_VNCT_OPR           > CURRENT DATE
      *-----------> Paginacao
           AND   ((A.CD_CTRA_TIT_VL   > :SGTSLC05-CD-CTRA-TIT-ULT)
            OR    (A.CD_CTRA_TIT_VL   = :SGTSLC05-CD-CTRA-TIT-ULT   AND
                   A.CD_OPR_ETRD_MCP  > :SGTSLC05-CD-OPR-ETRD-ULT)
            OR    (A.CD_CTRA_TIT_VL   = :SGTSLC05-CD-CTRA-TIT-ULT   AND
                   A.CD_OPR_ETRD_MCP  = :SGTSLC05-CD-OPR-ETRD-ULT   AND
                   A.NR_VRS_OPR_ETRD  > :SGTSLC05-NR-VRS-OPR-ULT))
           ORDER BY
                   A.CD_CTRA_TIT_VL
                 , A.CD_OPR_ETRD_MCP
                 , A.NR_VRS_OPR_ETRD

           END-EXEC.

      *--> Parâmetros para a utilizacao do DB2
           EXEC SQL
                     INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC  X(40) VALUE
           '*** Storage Section Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
-INC ALMKCBL2

      *--> Habilita tratamento automático de erros DB2
      *    Whenever DB2
-INC HLPKE997

      *--> Habilita captura automática de abend e erros CICS
      *    Whenever CICS
-INC HLPKE999

           PERFORM  100000-INICIAR

           PERFORM  200000-PROCESSAR-PESQUISA

           PERFORM  300000-POSTAR-RESPOSTA

           PERFORM  999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

      *--> Define o nome do programa para manipulação de container
           MOVE  CTE-PROG(5:8)         TO  NM-PGM

           PERFORM  110000-OBTER-CANAL

           PERFORM  120000-EXCLUIR-CTNR-RPST

           PERFORM  130000-EXCLUIR-CTNR-ERRO

           PERFORM  140000-RECEBER-REQUISICAO

           PERFORM  150000-VALIDAR-PARAMETRO

           PERFORM  160000-VALIDAR-TITULO

           PERFORM  170000-VALIDAR-OPERACAO

           PERFORM  180000-VALIDAR-RECHAMADA

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-OBTER-CANAL              SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  ASSIGN
                      CHANNEL ( NM-CNL                                 )
           END-EXEC
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'SGTSLC05-RPST'                      )
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
       130000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

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
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-CCT-LIDO
           MOVE  LENGTH OF  SGTSLC05-RQSC
                                       TO  QT-CCT-EPRO
           EXEC CICS  GET
                      CONTAINER ( 'SGTSLC05-RQSC'                      )
                      CHANNEL   ( NM-CNL                               )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO                          )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               MOVE  3001              TO  MSG1-CD-ERRO-TS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  QT-CCT-EPRO       TO  VL-EXBR-1
               MOVE  QT-CCT-LIDO       TO  VL-EXBR-2
               STRING
                   'Container RQSC'
                   ', Tamanho Esperado: '  VL-EXBR-1
                   ', Tamanho Recebido: '  VL-EXBR-2
                   DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS  GET
                      CONTAINER ( 'SGTSLC05-RQSC'                      )
                      CHANNEL   ( NM-CNL                               )
                      FLENGTH   ( LENGTH OF SGTSLC05-RQSC              )
                      INTO      ( SGTSLC05-RQSC                        )
           END-EXEC
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           IF  NOT SGTSLC05-OPR-VIGENTES-TODOS  AND
               NOT SGTSLC05-OPR-VIGENTES-CTRA   AND
               NOT SGTSLC05-OPR-VIGENTES-TIT    AND
               NOT SGTSLC05-OPR-VIGENTES-OPR
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0107              TO  MSG1-CD-ERRO-TS
               MOVE  'Código tipo serviço inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validar Carteira TVM Pesquisada
           IF  SGTSLC05-CD-CTRA-TIT-PSQ    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSLC05-CD-CTRA-TIT-PSQ    EQUAL ZEROS  AND
               SGTSLC05-OPR-VIGENTES-CTRA
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.' TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       150000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       160000-VALIDAR-TITULO           SECTION.
      *--> Validar Chave Título TVM
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSLC05-CD-TIT-OPR-PSQ     EQUAL SPACES  AND
               SGTSLC05-OPR-VIGENTES-TIT

               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0231              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do título da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           INSPECT  SGTSLC05-CD-TIT-OPR-PSQ
                                           REPLACING ALL SPACES BY '%'

           IF  SGTSLC05-NR-DVS-TIT-PSQ     NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0013              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da divisão de título inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       160000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170000-VALIDAR-OPERACAO         SECTION.
      *--> Validar Chave Operação Estruturada Pesquisada
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSLC05-CD-OPR-ETRD-PSQ    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSLC05-CD-OPR-ETRD-PSQ    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       170000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       180000-VALIDAR-RECHAMADA        SECTION.
      *--> Validar Campos de Rechamada
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSLC05-CD-CTRA-TIT-ULT    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSLC05-NR-DVS-TIT-ULT     NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0013              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da divisão de título inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validar Chave Operação Estruturada Rechamada
           IF  SGTSLC05-CD-OPR-ETRD-ULT    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSLC05-CD-OPR-ETRD-ULT    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSLC05-NR-VRS-OPR-ULT     NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       180000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR-PESQUISA       SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

      *--> Preparar Filtros de Pesquisa
           MOVE  999999999             TO  CD-CTRA-TIT-FIM
           MOVE  99999999999           TO  CD-OPR-ETRD-FIM

           EVALUATE  TRUE
               WHEN  SGTSLC05-OPR-VIGENTES-CTRA
                     IF  SGTSLC05-CD-CTRA-TIT-PSQ
                                           POSITIVE
                         MOVE  SGTSLC05-CD-CTRA-TIT-PSQ
                                       TO  CD-CTRA-TIT-INC
                                           CD-CTRA-TIT-FIM
                     END-IF
               WHEN  SGTSLC05-OPR-VIGENTES-TIT
                     IF  SGTSLC05-CD-CTRA-TIT-PSQ
                                           POSITIVE
                         MOVE  SGTSLC05-CD-CTRA-TIT-PSQ
                                       TO  CD-CTRA-TIT-INC
                                           CD-CTRA-TIT-FIM
                     END-IF
               WHEN  SGTSLC05-OPR-VIGENTES-OPR
                     IF  SGTSLC05-CD-CTRA-TIT-PSQ
                                           POSITIVE
                         MOVE  SGTSLC05-CD-CTRA-TIT-PSQ
                                       TO  CD-CTRA-TIT-INC
                                           CD-CTRA-TIT-FIM
                     END-IF
                     IF  SGTSLC05-CD-OPR-ETRD-PSQ
                                           POSITIVE
                         MOVE  SGTSLC05-CD-OPR-ETRD-PSQ
                                       TO  CD-OPR-ETRD-INC
                     END-IF
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

           PERFORM  971000-ABRE-CURSOR-PSQ

           PERFORM  972000-LER-CURSOR-PSQ

           PERFORM  973000-FECHA-CURSOR-PSQ

           PERFORM  210000-MONTAR-LISTA
                    VARYING                SGTSLC05-IC-TAB
                    FROM                   0001 BY 0001
                    UNTIL                  SGTSLC05-IC-TAB  GREATER
                                           SGTSLC05-QT-OCR-TAB
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-MONTAR-LISTA             SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           MOVE  CD-CTRA-TIT(SGTSLC05-IC-TAB)
             TO  SGTSLC05-CD-CTRA-TIT(SGTSLC05-IC-TAB)

           MOVE  CD-OPR-ETRD-MCP(SGTSLC05-IC-TAB)
             TO  SGTSLC05-CD-OPR-ETRD(SGTSLC05-IC-TAB)

           MOVE  NR-VRS-OPR-ETRD(SGTSLC05-IC-TAB)
             TO  SGTSLC05-NR-VRS-OPR-ETRD(SGTSLC05-IC-TAB)

           MOVE  CD-TIT-OPR-ETRD(SGTSLC05-IC-TAB)
             TO  SGTSLC05-CD-TIT-OPR-ETRD(SGTSLC05-IC-TAB)

           MOVE  NR-DVS-TIT(SGTSLC05-IC-TAB)
             TO  SGTSLC05-NR-DVS-TIT(SGTSLC05-IC-TAB)

      *--> Situação da Operação e da Versão
           MOVE  CD-TIP-VRS-OPR(SGTSLC05-IC-TAB)
             TO  SGTSLC05-CD-TIP-VRS-OPR(SGTSLC05-IC-TAB)

           MOVE  CD-TIP-EST-OPR(SGTSLC05-IC-TAB)
             TO  SGTSLC05-CD-TIP-EST-OPR(SGTSLC05-IC-TAB)

           MOVE  CD-TIP-EST-VRS(SGTSLC05-IC-TAB)
             TO  SGTSLC05-CD-TIP-EST-VRS(SGTSLC05-IC-TAB)

      *--> Produto e Modalidade
           MOVE  CD-PRD(SGTSLC05-IC-TAB)
             TO  SGTSLC05-CD-PRD(SGTSLC05-IC-TAB)
           MOVE  CD-MDLD(SGTSLC05-IC-TAB)
             TO  SGTSLC05-CD-MDLD(SGTSLC05-IC-TAB)

      *--> Dados Emissão do Título
           MOVE  QT-PAPL-TIT-EMS(SGTSLC05-IC-TAB)
             TO  SGTSLC05-QT-PAPL-TIT-EMS(SGTSLC05-IC-TAB)

           MOVE  VL-PU-EMS-TIT-OPR(SGTSLC05-IC-TAB)
             TO  SGTSLC05-VL-PU-EMS-TIT-OPR(SGTSLC05-IC-TAB)

           MOVE  DT-EMS-TIT-OPR(SGTSLC05-IC-TAB)
             TO  SGTSLC05-DT-EMS-TIT-OPR(SGTSLC05-IC-TAB)

      *----------> Dados Compra Título
           MOVE  QT-PAPL-TIT-CPR(SGTSLC05-IC-TAB)
             TO  SGTSLC05-QT-PAPL-TIT-CPR(SGTSLC05-IC-TAB)

           MOVE  VL-PU-CPR-TIT-OPR(SGTSLC05-IC-TAB)
             TO  SGTSLC05-VL-PU-CPR-TIT-OPR(SGTSLC05-IC-TAB)

           MOVE  VL-CPR-OPR(SGTSLC05-IC-TAB)
             TO  SGTSLC05-VL-CPR-OPR(SGTSLC05-IC-TAB)

           MOVE  DT-CPR-OPR(SGTSLC05-IC-TAB)
             TO  SGTSLC05-DT-CPR-OPR(SGTSLC05-IC-TAB)

           MOVE  CD-UOR-DTTR-OPR(SGTSLC05-IC-TAB)
             TO  SGTSLC05-CD-UOR-DTTR-OPR(SGTSLC05-IC-TAB)

      *----------> Datas de Vencimento e Liquidacao
           MOVE  DT-VNCT-OPR(SGTSLC05-IC-TAB)
             TO  SGTSLC05-DT-VNCT-OPR(SGTSLC05-IC-TAB)

           MOVE  DT-LQDC-OPR-DRIV(SGTSLC05-IC-TAB)
             TO  SGTSLC05-DT-LQDC-OPR-DRIV(SGTSLC05-IC-TAB)

      *----------> Data Inicio Vigencia Versao e Dados Evento Vinculado
           MOVE  DT-INC-VGC-VRS-OPR(SGTSLC05-IC-TAB)
             TO  SGTSLC05-DT-INC-VGC-VRS-OPR(SGTSLC05-IC-TAB)

           MOVE  DT-EVT-VCLD-VRS(SGTSLC05-IC-TAB)
             TO  SGTSLC05-DT-EVT-VCLD-VRS(SGTSLC05-IC-TAB)

           MOVE  VL-EVT-VCLD-VRS(SGTSLC05-IC-TAB)
             TO  SGTSLC05-VL-EVT-VCLD-VRS(SGTSLC05-IC-TAB)

           MOVE  TX-OBS-EVT(SGTSLC05-IC-TAB)
             TO  SGTSLC05-TX-OBS-EVT(SGTSLC05-IC-TAB)

           MOVE  CD-USU-ATL(SGTSLC05-IC-TAB)
             TO  SGTSLC05-CD-USU-ATL(SGTSLC05-IC-TAB)
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'SGTSLC05-RPST'                      )
                      CHANNEL   ( NM-CNL                               )
                      FLENGTH   ( LENGTH OF SGTSLC05-RPST              )
                      FROM      ( SGTSLC05-RPST                        )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       971000-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

           EVALUATE  TRUE
               WHEN  SGTSLC05-OPR-VIGENTES-TODOS
                     EXEC SQL
                          OPEN CURSOR-PSQ-01
                     END-EXEC
               WHEN  SGTSLC05-OPR-VIGENTES-CTRA
                     EXEC SQL
                          OPEN CURSOR-PSQ-02
                     END-EXEC
               WHEN  SGTSLC05-OPR-VIGENTES-TIT
                     EXEC SQL
                          OPEN CURSOR-PSQ-03
                     END-EXEC
               WHEN  SGTSLC05-OPR-VIGENTES-OPR
                     EXEC SQL
                          OPEN CURSOR-PSQ-04
                     END-EXEC
           END-EVALUATE

           MOVE  'S'                   TO  IN-CURSOR-PSQ-ABTO
           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-CURSOR-PSQ           SECTION.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-REG-SELD

           MOVE  SPACES                TO OPERACOES-VIGENTES
           INITIALIZE                     OPERACOES-VIGENTES
                                          REPLACING NUMERIC BY  ZEROS

           EVALUATE  TRUE

               WHEN  SGTSLC05-OPR-VIGENTES-TODOS
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-01
                                FOR 0101 ROWS
                           INTO
                                :CD-CTRA-TIT
                              , :CD-OPR-ETRD-MCP
                              , :NR-VRS-OPR-ETRD
                              , :CD-TIT-OPR-ETRD
                                :CD-TIT-OPR-ETRD-N
                              , :NR-DVS-TIT
                                :NR-DVS-TIT-N
                              , :CD-TIP-VRS-OPR
                              , :CD-TIP-EST-OPR
                                :CD-TIP-EST-OPR-N
                              , :CD-TIP-EST-VRS
                              , :CD-PRD
                              , :CD-MDLD
                              , :QT-PAPL-TIT-EMS
                              , :VL-PU-EMS-TIT-OPR
                              , :DT-EMS-TIT-OPR
                                :DT-EMS-TIT-OPR-N
                              , :QT-PAPL-TIT-CPR
                              , :VL-PU-CPR-TIT-OPR
                              , :VL-CPR-OPR
                              , :DT-CPR-OPR
                                :DT-CPR-OPR-N
                              , :CD-UOR-DTTR-OPR
                              , :DT-VNCT-OPR
                                :DT-VNCT-OPR-N
                              , :DT-LQDC-OPR-DRIV
                                :DT-LQDC-OPR-DRIV-N
                              , :DT-INC-VGC-VRS-OPR
                                :DT-INC-VGC-VRS-OPR-N
                              , :DT-EVT-VCLD-VRS
                                :DT-EVT-VCLD-VRS-N
                              , :VL-EVT-VCLD-VRS
                                :VL-EVT-VCLD-VRS-N
                              , :TX-OBS-EVT
                                :TX-OBS-EVT-N
                              , :CD-USU-ATL
                     END-EXEC

               WHEN  SGTSLC05-OPR-VIGENTES-CTRA
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-02
                                FOR 0101 ROWS
                           INTO
                                :CD-CTRA-TIT
                              , :CD-OPR-ETRD-MCP
                              , :NR-VRS-OPR-ETRD
                              , :CD-TIT-OPR-ETRD
                                :CD-TIT-OPR-ETRD-N
                              , :NR-DVS-TIT
                                :NR-DVS-TIT-N
                              , :CD-TIP-VRS-OPR
                              , :CD-TIP-EST-OPR
                                :CD-TIP-EST-OPR-N
                              , :CD-TIP-EST-VRS
                              , :CD-PRD
                              , :CD-MDLD
                              , :QT-PAPL-TIT-EMS
                              , :VL-PU-EMS-TIT-OPR
                              , :DT-EMS-TIT-OPR
                                :DT-EMS-TIT-OPR-N
                              , :QT-PAPL-TIT-CPR
                              , :VL-PU-CPR-TIT-OPR
                              , :VL-CPR-OPR
                              , :DT-CPR-OPR
                                :DT-CPR-OPR-N
                              , :CD-UOR-DTTR-OPR
                              , :DT-VNCT-OPR
                                :DT-VNCT-OPR-N
                              , :DT-LQDC-OPR-DRIV
                                :DT-LQDC-OPR-DRIV-N
                              , :DT-INC-VGC-VRS-OPR
                                :DT-INC-VGC-VRS-OPR-N
                              , :DT-EVT-VCLD-VRS
                                :DT-EVT-VCLD-VRS-N
                              , :VL-EVT-VCLD-VRS
                                :VL-EVT-VCLD-VRS-N
                              , :TX-OBS-EVT
                                :TX-OBS-EVT-N
                              , :CD-USU-ATL
                     END-EXEC

               WHEN  SGTSLC05-OPR-VIGENTES-TIT
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-03
                                FOR 0101 ROWS
                           INTO
                                :CD-CTRA-TIT
                              , :CD-OPR-ETRD-MCP
                              , :NR-VRS-OPR-ETRD
                              , :CD-TIT-OPR-ETRD
                                :CD-TIT-OPR-ETRD-N
                              , :NR-DVS-TIT
                                :NR-DVS-TIT-N
                              , :CD-TIP-VRS-OPR
                              , :CD-TIP-EST-OPR
                                :CD-TIP-EST-OPR-N
                              , :CD-TIP-EST-VRS
                              , :CD-PRD
                              , :CD-MDLD
                              , :QT-PAPL-TIT-EMS
                              , :VL-PU-EMS-TIT-OPR
                              , :DT-EMS-TIT-OPR
                                :DT-EMS-TIT-OPR-N
                              , :QT-PAPL-TIT-CPR
                              , :VL-PU-CPR-TIT-OPR
                              , :VL-CPR-OPR
                              , :DT-CPR-OPR
                                :DT-CPR-OPR-N
                              , :CD-UOR-DTTR-OPR
                              , :DT-VNCT-OPR
                                :DT-VNCT-OPR-N
                              , :DT-LQDC-OPR-DRIV
                                :DT-LQDC-OPR-DRIV-N
                              , :DT-INC-VGC-VRS-OPR
                                :DT-INC-VGC-VRS-OPR-N
                              , :DT-EVT-VCLD-VRS
                                :DT-EVT-VCLD-VRS-N
                              , :VL-EVT-VCLD-VRS
                                :VL-EVT-VCLD-VRS-N
                              , :TX-OBS-EVT
                                :TX-OBS-EVT-N
                              , :CD-USU-ATL
                     END-EXEC

               WHEN  SGTSLC05-OPR-VIGENTES-OPR
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-04
                                FOR 0101 ROWS
                           INTO
                                :CD-CTRA-TIT
                              , :CD-OPR-ETRD-MCP
                              , :NR-VRS-OPR-ETRD
                              , :CD-TIT-OPR-ETRD
                                :CD-TIT-OPR-ETRD-N
                              , :NR-DVS-TIT
                                :NR-DVS-TIT-N
                              , :CD-TIP-VRS-OPR
                              , :CD-TIP-EST-OPR
                                :CD-TIP-EST-OPR-N
                              , :CD-TIP-EST-VRS
                              , :CD-PRD
                              , :CD-MDLD
                              , :QT-PAPL-TIT-EMS
                              , :VL-PU-EMS-TIT-OPR
                              , :DT-EMS-TIT-OPR
                                :DT-EMS-TIT-OPR-N
                              , :QT-PAPL-TIT-CPR
                              , :VL-PU-CPR-TIT-OPR
                              , :VL-CPR-OPR
                              , :DT-CPR-OPR
                                :DT-CPR-OPR-N
                              , :CD-UOR-DTTR-OPR
                              , :DT-VNCT-OPR
                                :DT-VNCT-OPR-N
                              , :DT-LQDC-OPR-DRIV
                                :DT-LQDC-OPR-DRIV-N
                              , :DT-INC-VGC-VRS-OPR
                                :DT-INC-VGC-VRS-OPR-N
                              , :DT-EVT-VCLD-VRS
                                :DT-EVT-VCLD-VRS-N
                              , :VL-EVT-VCLD-VRS
                                :VL-EVT-VCLD-VRS-N
                              , :TX-OBS-EVT
                                :TX-OBS-EVT-N
                              , :CD-USU-ATL
                     END-EXEC

           END-EVALUATE

           MOVE  SQLERRD(3)            TO  QT-REG-SELD

           IF  QT-REG-SELD                 EQUAL  0101
               SET   SGTSLC05-TEM-MAIS-REGISTRO
                                       TO  TRUE
               MOVE  0100              TO  QT-REG-SELD
           END-IF

           MOVE  QT-REG-SELD           TO  SGTSLC05-QT-OCR-TAB
           MOVE  SQLCODE               TO  CD-FIM-CURSOR-PSQ
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973000-FECHA-CURSOR-PSQ         SECTION.
      *-----------------------------------------------------------------
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'N'                   TO  IN-CURSOR-PSQ-ABTO

           EVALUATE  TRUE
               WHEN  SGTSLC05-OPR-VIGENTES-TODOS
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-01
                     END-EXEC
               WHEN  SGTSLC05-OPR-VIGENTES-CTRA
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-02
                     END-EXEC
               WHEN  SGTSLC05-OPR-VIGENTES-TIT
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-03
                     END-EXEC
               WHEN  SGTSLC05-OPR-VIGENTES-OPR
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-04
                     END-EXEC
           END-EVALUATE
           .
       973000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  IN-CURSOR-PSQ-ABTO          EQUAL  'S'
               PERFORM  973000-FECHA-CURSOR-PSQ
           END-IF

           IF  MSG1-CD-ERRO-TS             EQUAL ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Adiciona as Variaveis de Entrada para Monitoracao
           MOVE  'LC05-001-CD-TIP-SRVC'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLC05-CD-TIP-SRVC  TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'LC05-002-CD-CTRA-TIT-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLC05-CD-CTRA-TIT-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'LC05-003-CD-TIT-OPR-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLC05-CD-TIT-OPR-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'LC05-004-NR-DVS-TIT-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLC05-NR-DVS-TIT-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'LC05-005-CD-OPR-ETRD-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSLC05-CD-OPR-ETRD-PSQ    NUMERIC
               MOVE  SGTSLC05-CD-OPR-ETRD-PSQ
                                       TO  CD-OPR-ETRD
               MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'LC05-006-CD-CTRA-TIT-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLC05-CD-CTRA-TIT-ULT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'LC05-007-CD-TIT-OPR-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLC05-CD-TIT-OPR-ULT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'LC05-008-NR-DVS-TIT-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLC05-NR-DVS-TIT-ULT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'LC05-009-CD-OPR-ETRD-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSLC05-CD-OPR-ETRD-ULT    NUMERIC
               MOVE  SGTSLC05-CD-OPR-ETRD-ULT
                                       TO  CD-OPR-ETRD
               MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'LC05-010-NR-VRS-OPR-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLC05-NR-VRS-OPR-ULT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Adiciona as Variaveis de Saida para Monitoracao
           MOVE  'LC05-011-QT-OCR-TAB'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLC05-QT-OCR-TAB
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'LC05-012-IC-TAB'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLC05-IC-TAB
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava a Ocorrencia de Erro no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

      *--> Grava a Ocorrencia de Erro no Container de Erro
           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                           )
                      CHANNEL   ( NM-CNL                               )
                      FROM      ( IIBKERRO-PRM-ERRO                    )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO          )
                      NOHANDLE
           END-EXEC

      *--> Encerra a Transacao
           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      *----------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      *----------------------------------------
-INC HLPKP999
      *----------------------------------------
      * 999010-TRATA-ERRO-DB2          SECTION.
      *----------------------------------------
-INC HLPKP997
      *----------------------------------------
      * MSG1-NTF-MSG                   SECTION.
      *----------------------------------------
-INC MSGKPMG1

      *-----------------------------------------------------------------
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.

      *-------------------------- FIM SGTSLC05 -------------------------