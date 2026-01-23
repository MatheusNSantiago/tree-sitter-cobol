      *-----------------------------------------------------------------
      * Programa..: SGTSL587
      * Analista..: F8650584 - Roberto Fernandes Lima
      * Autor.....: F8650584 - Roberto Fernandes Lima
      * Data......: 02/03/2019
      * Objetivo..: Listar Pagamentos da Operação Estruturada Vigente
      *             COM Vinculos Parciais de Recebimento Previsto
      *             Consulta nas Tabelas DB2SGT.DMTV_CX_TIT e
      *             DB2SGT.RCBT_RLZD_OPR
      * Compilação: Cobol/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 02/03/2019 - F8650584 - DE 2016/145195 - Implantacao
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSL587.

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
                                       '*** SGTSL587 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  IIBP2000                PIC  X(08) VALUE 'IIBP2000'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informações
-INC HLPKE01B

      *--> Constantes Utilizadas
       77  MOVIMENTACAO-ATIVA          PIC  X(01) VALUE 'S'.
       77  ENCARTEIRADA                PIC S9(09) COMP-5 VALUE 0005.
       77  APROVADA                    PIC S9(04) COMP-5 VALUE 0003.
       77  CLIENTE-EMISSOR             PIC S9(04) COMP-5 VALUE 0001.


      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE ZEROS.

       01  AUXILIARES-PESQUISA.
           03  CD-TIT-OPR-PSQ          PIC  X(12) VALUE SPACES.
           03  NR-SEQL-RCBT-PRVT-PSQ   PIC S9(09) COMP-5 VALUE ZEROS.
           03  NR-SEQL-RCBT-PRVT-INI   PIC S9(09) COMP-5 VALUE ZEROS.
           03  NR-SEQL-RCBT-PRVT-FIM   PIC S9(09) COMP-5 VALUE ZEROS.
           03  DT-EFT-MVTC-ULT         PIC  X(10) VALUE SPACES.
           03  DT-EFT-MVTC-ULT-R       REDEFINES DT-EFT-MVTC-ULT.
               05  ANO                 PIC  9(04).
               05  FILLER              PIC  X(01).
               05  MES                 PIC  9(02).
               05  FILLER              PIC  X(01).
               05  DIA                 PIC  9(02).
           03  TS-REG-PGTO-ULT         PIC  X(26)
               VALUE '0000-00-00-00.00.00.000000'.
           03  TS-REG-PGTO-ULT-R       REDEFINES TS-REG-PGTO-ULT.
               05  ANO                 PIC  9(04).
               05  FILLER              PIC  X(01).
               05  MES                 PIC  9(02).
               05  FILLER              PIC  X(01).
               05  DIA                 PIC  9(02).
               05  FILLER              PIC  X(01).
               05  HORA                PIC  9(02).
               05  FILLER              PIC  X(01).
               05  MINUTO              PIC  9(02).
               05  FILLER              PIC  X(01).
               05  SEGUNDO             PIC  9(02).
               05  FILLER              PIC  X(01).
               05  MILISEGUNDO         PIC  9(06).

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-TX.
           03  CD-OPR-ETRD             PIC  9(17).

      *--> Campos Utilizados para Leitura da Tabela DB2SGT.DMTV_CX_TIT
      *    e DB2SGT.RCBT_RLZD_OPR
       01  VINCULO-PARCIAL.
           03  CD-CTRA-TIT             PIC S9(09) COMP-5
                                       OCCURS 0401 TIMES  VALUE ZEROS.
           03  CD-TIT-OPR-ETRD         PIC  X(12)
                                       OCCURS 0401 TIMES  VALUE SPACES.
           03  NR-DVS-TIT              PIC S9(04) COMP-5
                                       OCCURS 0401 TIMES  VALUE ZEROS.
           03  CD-OPR-ETRD-MCP         PIC S9(11) COMP-3
                                       OCCURS 0401 TIMES  VALUE ZEROS.
           03  NR-VRS-OPR-ETRD         PIC S9(04) COMP-5
                                       OCCURS 0401 TIMES  VALUE ZEROS.
           03  NR-SEQL-RCBT-PRVT       PIC S9(09) COMP-5
                                       OCCURS 0401 TIMES  VALUE ZEROS.
           03  TS-REG-PGTO             PIC  X(26)
                                       OCCURS 0401 TIMES  VALUE SPACES.
           03  CD-TIP-MVTC-TIT         PIC S9(04) COMP-5
                                       OCCURS 0401 TIMES  VALUE ZEROS.
           03  CD-TIP-LCTO-CTB         PIC S9(04) COMP-5
                                       OCCURS 0401 TIMES  VALUE ZEROS.
           03  DT-EFT-MVTC             PIC  X(10)
                                       OCCURS 0401 TIMES  VALUE SPACES.
           03  VL-EFT-MVTC             PIC S9(15)V9(2) COMP-3
                                       OCCURS 0401 TIMES  VALUE ZEROS.
           03  VL-EFT-MVTC-N           PIC S9(04) COMP-5
                                       OCCURS 0401 TIMES  VALUE ZEROS.
           03  VL-RLZD-VCLD            PIC S9(15)V9(2) COMP-3
                                       OCCURS 0401 TIMES  VALUE ZEROS.
           03  VL-RLZD-VCLD-N          PIC S9(04) COMP-5
                                       OCCURS 0401 TIMES  VALUE ZEROS.
           03  IN-MVTC-ATV             PIC  X(01)
                                       OCCURS 0401 TIMES  VALUE SPACES.
           03  CD-USU-IMP              PIC  X(08)
                                       OCCURS 0401 TIMES  VALUE SPACES.
           03  DT-IMP-MVTC-FNCR        PIC  X(10)
                                       OCCURS 0401 TIMES  VALUE SPACES.
           03  TX-DCR-MVTC             PIC  X(50)
                                       OCCURS 0401 TIMES  VALUE SPACES.

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

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book para captura de abend cics da bloco código MSGKP999
-INC HLPKW999

      *--> Book do container de requisição e resposta deste módulo
       01  SGTSL587-DADOS.
-INC SGTKL587

      *-----------------------------------------------------------------
      *    Cursores de Pesquisa
      *-----------------------------------------------------------------

      *--> Cursor de Pesquisa 01
      *    Listar todos Recebimentos Previstos em Atraso
      *    de Todas Operações Estruturadas
           EXEC SQL
             DECLARE CURSOR-PSQ-01 CURSOR
                     WITH ROWSET POSITIONING FOR
           SELECT
                  B.CD_CTRA_TIT_VL
                , A.CD_TIT_OPR_ETRD
                , A.NR_DVS_TIT
                , A.TS_REG_PGTO
                , B.CD_OPR_ETRD_MCP
                , B.NR_VRS_OPR_ETRD
                , B.NR_SEQL_RCBT_PRVT
                , A.CD_TIP_MVTC_TIT
                , A.CD_TIP_LCTO_CTB
                , A.IN_MVTC_ATV
                , A.TX_DCR_MVTC
                , A.CD_USU_IMP
                , CHAR(TO_CHAR(A.DT_IMP_MVTC_FNCR, 'DDMMYYYY'), 0008)
                , CHAR(TO_CHAR(A.DT_EFT_MVTC, 'DDMMYYYY'), 0008)
                , IFNULL(A.VL_EFT_MVTC, 0000)
                , IFNULL(B.VALOR_REALIZADO, 0000)
           FROM
                  DB2SGT.DMTV_CX_TIT    A
           INNER JOIN
                 (SELECT  B1.CD_CTRA_TIT_VL
                        , B1.CD_OPR_ETRD_MCP
                        , B1.NR_VRS_OPR_ETRD
                        , B1.NR_SEQL_RCBT_PRVT
                        , B1.CD_TIT_OPR_ETRD
                        , B1.NR_DVS_TIT
                        , B1.TS_REG_PGTO
                        , SUM(B1.VL_RLZD_VCLD) AS VALOR_REALIZADO
                  FROM
                          DB2SGT.RCBT_RLZD_OPR B1
                  WHERE
                          B1.NR_SEQL_RCBT_PRVT
                                        =
                         (SELECT
                             MAX(B2.NR_SEQL_RCBT_PRVT)
                          FROM
                                  DB2SGT.RCBT_RLZD_OPR B2
                          WHERE
                                  B1.CD_CTRA_TIT_VL
                                                  = B2.CD_CTRA_TIT_VL
                          AND     B1.CD_OPR_ETRD_MCP
                                                  = B2.CD_OPR_ETRD_MCP
                          AND     B1.NR_VRS_OPR_ETRD
                                                  = B2.NR_VRS_OPR_ETRD)
                  GROUP BY
                          B1.CD_CTRA_TIT_VL
                        , B1.CD_OPR_ETRD_MCP
                        , B1.NR_VRS_OPR_ETRD
                        , B1.NR_SEQL_RCBT_PRVT
                        , B1.CD_TIT_OPR_ETRD
                        , B1.NR_DVS_TIT
                        , B1.TS_REG_PGTO ) B
           ON     A.CD_CTRA_TIT_VL      = B.CD_CTRA_TIT_VL
           AND    A.CD_TIT_OPR_ETRD     = B.CD_TIT_OPR_ETRD
           AND    A.NR_DVS_TIT          = B.NR_DVS_TIT
           AND    A.TS_REG_PGTO         = B.TS_REG_PGTO
           AND    A.IN_MVTC_ATV         = :MOVIMENTACAO-ATIVA
           AND    A.VL_EFT_MVTC         > B.VALOR_REALIZADO
           INNER JOIN
                 (SELECT  E10.CD_CTRA_TIT_VL
                        , E10.CD_TIT_OPR_ETRD
                        , E10.NR_DVS_TIT
                        , E10.CD_OPR_ETRD_MCP
                        , E10.NR_VRS_OPR_ETRD
                        , E20.CD_TIP_EST_OPR
                        , E20.TS_EST_OPR_ETRD
                        , E30.CD_TIP_EST_VRS
                        , E30.TS_EST_VRS
                  FROM
                          DB2SGT.OPR_ETRD_MCP E10
                  INNER JOIN
                          DB2SGT.EST_OPR_ETRD  E20
                  ON
                          E20.CD_CTRA_TIT_VL      = E10.CD_CTRA_TIT_VL
                  AND     E20.CD_OPR_ETRD_MCP     = E10.CD_OPR_ETRD_MCP
                  AND     E20.NR_VRS_OPR_ETRD     = E10.NR_VRS_OPR_ETRD
                  AND     E20.CD_TIP_EST_OPR      = :ENCARTEIRADA
                  AND     E20.TS_EST_OPR_ETRD     =
                         (SELECT
                             MAX(E21.TS_EST_OPR_ETRD)
                          FROM
                                  DB2SGT.EST_OPR_ETRD E21
                          WHERE
                                  E21.CD_CTRA_TIT_VL
                                                  = E20.CD_CTRA_TIT_VL
                          AND     E21.CD_OPR_ETRD_MCP
                                                  = E20.CD_OPR_ETRD_MCP)
                  INNER JOIN
                          DB2SGT.EST_VRS_OPR_ETRD  E30
                  ON
                          E30.CD_CTRA_TIT_VL      = E10.CD_CTRA_TIT_VL
                  AND     E30.CD_OPR_ETRD_MCP     = E10.CD_OPR_ETRD_MCP
                  AND     E30.NR_VRS_OPR_ETRD     = E10.NR_VRS_OPR_ETRD
                  AND     E30.CD_TIP_EST_VRS      = :APROVADA
                  AND     E30.TS_EST_VRS          =
                         (SELECT
                                  MAX(E31.TS_EST_VRS)
                          FROM
                                  DB2SGT.EST_VRS_OPR_ETRD E31
                          WHERE
                                  E31.CD_CTRA_TIT_VL
                                                  = E30.CD_CTRA_TIT_VL
                          AND     E31.CD_OPR_ETRD_MCP
                                                  = E30.CD_OPR_ETRD_MCP)
                  INNER JOIN
                          DB2SGT.CLI_OPR_ETRD E40
                  ON
                          E40.CD_CTRA_TIT_VL      = E10.CD_CTRA_TIT_VL
                  AND     E40.CD_OPR_ETRD_MCP     = E10.CD_OPR_ETRD_MCP
                  AND     E40.NR_VRS_OPR_ETRD     = E10.NR_VRS_OPR_ETRD
                  AND     E40.CD_TIP_FUC_CLI      = :CLIENTE-EMISSOR
                  WHERE
                          E10.DT_CPR_OPR          < CURRENT DATE
                  AND     E10.DT_INC_VGC_VRS_OPR  < CURRENT DATE
                  AND     E10.DT_VNCT_OPR         > CURRENT DATE
      *    --     AND     E10.DT_LQDC_OPR_DRIV    > CURRENT DATE
                                                                 ) E00
           ON
                  E00.CD_CTRA_TIT_VL              =  B.CD_CTRA_TIT_VL
           AND    E00.CD_TIT_OPR_ETRD             =  A.CD_TIT_OPR_ETRD
           AND    E00.NR_DVS_TIT                  =  A.NR_DVS_TIT
           AND    E00.CD_OPR_ETRD_MCP             =  B.CD_OPR_ETRD_MCP
           AND    E00.NR_VRS_OPR_ETRD             =  B.NR_VRS_OPR_ETRD
           WHERE
      *---------> Paginacao
                ((B.CD_CTRA_TIT_VL      > :SGTSL587-CD-CTRA-TIT-ULT)  OR
                 (B.CD_CTRA_TIT_VL      = :SGTSL587-CD-CTRA-TIT-ULT  AND
                  A.CD_TIT_OPR_ETRD     > :SGTSL587-CD-TIT-OPR-ULT)   OR
                 (B.CD_CTRA_TIT_VL      = :SGTSL587-CD-CTRA-TIT-ULT  AND
                  A.CD_TIT_OPR_ETRD     = :SGTSL587-CD-TIT-OPR-ULT   AND
                  A.NR_DVS_TIT          > :SGTSL587-NR-DVS-TIT-ULT)   OR
                 (B.CD_CTRA_TIT_VL      = :SGTSL587-CD-CTRA-TIT-ULT  AND
                  A.CD_TIT_OPR_ETRD     = :SGTSL587-CD-TIT-OPR-ULT   AND
                  A.NR_DVS_TIT          = :SGTSL587-NR-DVS-TIT-ULT   AND
                  A.DT_EFT_MVTC         < :DT-EFT-MVTC-ULT)           OR
                 (B.CD_CTRA_TIT_VL      = :SGTSL587-CD-CTRA-TIT-ULT  AND
                  A.CD_TIT_OPR_ETRD     = :SGTSL587-CD-TIT-OPR-ULT   AND
                  A.NR_DVS_TIT          = :SGTSL587-NR-DVS-TIT-ULT   AND
                  A.DT_EFT_MVTC         = :DT-EFT-MVTC-ULT           AND
                  A.TS_REG_PGTO         < :TS-REG-PGTO-ULT))
           ORDER BY
                  B.CD_CTRA_TIT_VL
                , A.CD_TIT_OPR_ETRD
                , A.NR_DVS_TIT
                , A.DT_EFT_MVTC DESC
                , A.TS_REG_PGTO DESC
           END-EXEC.

      *--> Cursor de Pesquisa 02
      *    Listar Pagamentos da Operação Estruturada Vigente
      *    com Vinculos Parciais de Recebimento Previsto
      *    de uma Carteira Especifica
           EXEC SQL
             DECLARE CURSOR-PSQ-02 CURSOR
                     WITH ROWSET POSITIONING FOR
           SELECT
                  B.CD_CTRA_TIT_VL
                , A.CD_TIT_OPR_ETRD
                , A.NR_DVS_TIT
                , A.TS_REG_PGTO
                , B.CD_OPR_ETRD_MCP
                , B.NR_VRS_OPR_ETRD
                , B.NR_SEQL_RCBT_PRVT
                , A.CD_TIP_MVTC_TIT
                , A.CD_TIP_LCTO_CTB
                , A.IN_MVTC_ATV
                , A.TX_DCR_MVTC
                , A.CD_USU_IMP
                , CHAR(TO_CHAR(A.DT_IMP_MVTC_FNCR, 'DDMMYYYY'), 8)
                , CHAR(TO_CHAR(A.DT_EFT_MVTC, 'DDMMYYYY'), 8)
                , IFNULL(A.VL_EFT_MVTC, 0000)
                , IFNULL(B.VALOR_REALIZADO, 0000)
           FROM
                  DB2SGT.DMTV_CX_TIT    A
           INNER JOIN
                 (SELECT  B1.CD_CTRA_TIT_VL
                        , B1.CD_OPR_ETRD_MCP
                        , B1.NR_VRS_OPR_ETRD
                        , B1.NR_SEQL_RCBT_PRVT
                        , B1.CD_TIT_OPR_ETRD
                        , B1.NR_DVS_TIT
                        , B1.TS_REG_PGTO
                        , SUM(B1.VL_RLZD_VCLD) AS VALOR_REALIZADO
                  FROM
                          DB2SGT.RCBT_RLZD_OPR B1
                  WHERE
                          B1.NR_SEQL_RCBT_PRVT
                                        =
                         (SELECT
                             MAX(B2.NR_SEQL_RCBT_PRVT)
                          FROM
                                  DB2SGT.RCBT_RLZD_OPR B2
                          WHERE
                                  B1.CD_CTRA_TIT_VL
                                                  = B2.CD_CTRA_TIT_VL
                          AND     B1.CD_OPR_ETRD_MCP
                                                  = B2.CD_OPR_ETRD_MCP
                          AND     B1.NR_VRS_OPR_ETRD
                                                  = B2.NR_VRS_OPR_ETRD)
                  GROUP BY
                          B1.CD_CTRA_TIT_VL
                        , B1.CD_OPR_ETRD_MCP
                        , B1.NR_VRS_OPR_ETRD
                        , B1.NR_SEQL_RCBT_PRVT
                        , B1.CD_TIT_OPR_ETRD
                        , B1.NR_DVS_TIT
                        , B1.TS_REG_PGTO ) B
           ON     A.CD_CTRA_TIT_VL      = B.CD_CTRA_TIT_VL
           AND    A.CD_TIT_OPR_ETRD     = B.CD_TIT_OPR_ETRD
           AND    A.NR_DVS_TIT          = B.NR_DVS_TIT
           AND    A.TS_REG_PGTO         = B.TS_REG_PGTO
           AND    A.IN_MVTC_ATV         = :MOVIMENTACAO-ATIVA
           AND    A.VL_EFT_MVTC         > B.VALOR_REALIZADO
           INNER JOIN
                 (SELECT  E10.CD_CTRA_TIT_VL
                        , E10.CD_TIT_OPR_ETRD
                        , E10.NR_DVS_TIT
                        , E10.CD_OPR_ETRD_MCP
                        , E10.NR_VRS_OPR_ETRD
                        , E20.CD_TIP_EST_OPR
                        , E20.TS_EST_OPR_ETRD
                        , E30.CD_TIP_EST_VRS
                        , E30.TS_EST_VRS
                  FROM
                          DB2SGT.OPR_ETRD_MCP E10
                  INNER JOIN
                          DB2SGT.EST_OPR_ETRD  E20
                  ON
                          E20.CD_CTRA_TIT_VL      = E10.CD_CTRA_TIT_VL
                  AND     E20.CD_OPR_ETRD_MCP     = E10.CD_OPR_ETRD_MCP
                  AND     E20.NR_VRS_OPR_ETRD     = E10.NR_VRS_OPR_ETRD
                  AND     E20.CD_TIP_EST_OPR      = :ENCARTEIRADA
                  AND     E20.TS_EST_OPR_ETRD     =
                         (SELECT
                             MAX(E21.TS_EST_OPR_ETRD)
                          FROM
                                  DB2SGT.EST_OPR_ETRD E21
                          WHERE
                                  E21.CD_CTRA_TIT_VL
                                                  = E20.CD_CTRA_TIT_VL
                          AND     E21.CD_OPR_ETRD_MCP
                                                  = E20.CD_OPR_ETRD_MCP)
                  INNER JOIN
                          DB2SGT.EST_VRS_OPR_ETRD  E30
                  ON
                          E30.CD_CTRA_TIT_VL      = E10.CD_CTRA_TIT_VL
                  AND     E30.CD_OPR_ETRD_MCP     = E10.CD_OPR_ETRD_MCP
                  AND     E30.NR_VRS_OPR_ETRD     = E10.NR_VRS_OPR_ETRD
                  AND     E30.CD_TIP_EST_VRS      = :APROVADA
                  AND     E30.TS_EST_VRS          =
                         (SELECT
                                  MAX(E31.TS_EST_VRS)
                          FROM
                                  DB2SGT.EST_VRS_OPR_ETRD E31
                          WHERE
                                  E31.CD_CTRA_TIT_VL
                                                  = E30.CD_CTRA_TIT_VL
                          AND     E31.CD_OPR_ETRD_MCP
                                                  = E30.CD_OPR_ETRD_MCP)
                  INNER JOIN
                          DB2SGT.CLI_OPR_ETRD E40
                  ON
                          E40.CD_CTRA_TIT_VL      = E10.CD_CTRA_TIT_VL
                  AND     E40.CD_OPR_ETRD_MCP     = E10.CD_OPR_ETRD_MCP
                  AND     E40.NR_VRS_OPR_ETRD     = E10.NR_VRS_OPR_ETRD
                  AND     E40.CD_TIP_FUC_CLI      = :CLIENTE-EMISSOR
                  WHERE
                          E10.DT_CPR_OPR          < CURRENT DATE
                  AND     E10.DT_INC_VGC_VRS_OPR  < CURRENT DATE
                  AND     E10.DT_VNCT_OPR         > CURRENT DATE
      *    --     AND     E10.DT_LQDC_OPR_DRIV    > CURRENT DATE
                                                                 ) E00
           ON
                  E00.CD_CTRA_TIT_VL              =  B.CD_CTRA_TIT_VL
           AND    E00.CD_TIT_OPR_ETRD             =  A.CD_TIT_OPR_ETRD
           AND    E00.NR_DVS_TIT                  =  A.NR_DVS_TIT
           AND    E00.CD_OPR_ETRD_MCP             =  B.CD_OPR_ETRD_MCP
           AND    E00.NR_VRS_OPR_ETRD             =  B.NR_VRS_OPR_ETRD
           WHERE
                  B.CD_CTRA_TIT_VL      = :SGTSL587-CD-CTRA-TIT-PSQ
      *---------> Paginacao
           AND  ((B.CD_CTRA_TIT_VL      > :SGTSL587-CD-CTRA-TIT-ULT)  OR
                 (B.CD_CTRA_TIT_VL      = :SGTSL587-CD-CTRA-TIT-ULT  AND
                  A.CD_TIT_OPR_ETRD     > :SGTSL587-CD-TIT-OPR-ULT)   OR
                 (B.CD_CTRA_TIT_VL      = :SGTSL587-CD-CTRA-TIT-ULT  AND
                  A.CD_TIT_OPR_ETRD     = :SGTSL587-CD-TIT-OPR-ULT   AND
                  A.NR_DVS_TIT          > :SGTSL587-NR-DVS-TIT-ULT)   OR
                 (B.CD_CTRA_TIT_VL      = :SGTSL587-CD-CTRA-TIT-ULT  AND
                  A.CD_TIT_OPR_ETRD     = :SGTSL587-CD-TIT-OPR-ULT   AND
                  A.NR_DVS_TIT          = :SGTSL587-NR-DVS-TIT-ULT   AND
                  A.DT_EFT_MVTC         < :DT-EFT-MVTC-ULT)           OR
                 (B.CD_CTRA_TIT_VL      = :SGTSL587-CD-CTRA-TIT-ULT  AND
                  A.CD_TIT_OPR_ETRD     = :SGTSL587-CD-TIT-OPR-ULT   AND
                  A.NR_DVS_TIT          = :SGTSL587-NR-DVS-TIT-ULT   AND
                  A.DT_EFT_MVTC         = :DT-EFT-MVTC-ULT           AND
                  A.TS_REG_PGTO         < :TS-REG-PGTO-ULT))
           ORDER BY
                  B.CD_CTRA_TIT_VL
                , A.CD_TIT_OPR_ETRD
                , A.NR_DVS_TIT
                , A.DT_EFT_MVTC DESC
                , A.TS_REG_PGTO DESC
           END-EXEC.

      *--> Cursor de Pesquisa 03
      *    Listar Pagamentos da Operação Estruturada Vigente
      *    com Vinculos Parciais de Recebimento Previsto
      *    de um Título Especifico
           EXEC SQL
             DECLARE CURSOR-PSQ-03 CURSOR
                     WITH ROWSET POSITIONING FOR
           SELECT
                  B.CD_CTRA_TIT_VL
                , A.CD_TIT_OPR_ETRD
                , A.NR_DVS_TIT
                , A.TS_REG_PGTO
                , B.CD_OPR_ETRD_MCP
                , B.NR_VRS_OPR_ETRD
                , B.NR_SEQL_RCBT_PRVT
                , A.CD_TIP_MVTC_TIT
                , A.CD_TIP_LCTO_CTB
                , A.IN_MVTC_ATV
                , A.TX_DCR_MVTC
                , A.CD_USU_IMP
                , CHAR(TO_CHAR(A.DT_IMP_MVTC_FNCR, 'DDMMYYYY'), 8)
                , CHAR(TO_CHAR(A.DT_EFT_MVTC, 'DDMMYYYY'), 8)
                , IFNULL(A.VL_EFT_MVTC, 0000)
                , IFNULL(B.VALOR_REALIZADO, 0000)
           FROM
                  DB2SGT.DMTV_CX_TIT    A
           INNER JOIN
                 (SELECT  B1.CD_CTRA_TIT_VL
                        , B1.CD_OPR_ETRD_MCP
                        , B1.NR_VRS_OPR_ETRD
                        , B1.NR_SEQL_RCBT_PRVT
                        , B1.CD_TIT_OPR_ETRD
                        , B1.NR_DVS_TIT
                        , B1.TS_REG_PGTO
                        , SUM(B1.VL_RLZD_VCLD) AS VALOR_REALIZADO
                  FROM
                          DB2SGT.RCBT_RLZD_OPR B1
                  WHERE
                          B1.NR_SEQL_RCBT_PRVT
                                        =
                         (SELECT
                             MAX(B2.NR_SEQL_RCBT_PRVT)
                          FROM
                                  DB2SGT.RCBT_RLZD_OPR B2
                          WHERE
                                  B1.CD_CTRA_TIT_VL
                                                  = B2.CD_CTRA_TIT_VL
                          AND     B1.CD_OPR_ETRD_MCP
                                                  = B2.CD_OPR_ETRD_MCP
                          AND     B1.NR_VRS_OPR_ETRD
                                                  = B2.NR_VRS_OPR_ETRD)
                  GROUP BY
                          B1.CD_CTRA_TIT_VL
                        , B1.CD_OPR_ETRD_MCP
                        , B1.NR_VRS_OPR_ETRD
                        , B1.NR_SEQL_RCBT_PRVT
                        , B1.CD_TIT_OPR_ETRD
                        , B1.NR_DVS_TIT
                        , B1.TS_REG_PGTO ) B
           ON     A.CD_CTRA_TIT_VL      = B.CD_CTRA_TIT_VL
           AND    A.CD_TIT_OPR_ETRD     = B.CD_TIT_OPR_ETRD
           AND    A.NR_DVS_TIT          = B.NR_DVS_TIT
           AND    A.TS_REG_PGTO         = B.TS_REG_PGTO
           AND    A.IN_MVTC_ATV         = :MOVIMENTACAO-ATIVA
           AND    A.VL_EFT_MVTC         > B.VALOR_REALIZADO
           INNER JOIN
                 (SELECT  E10.CD_CTRA_TIT_VL
                        , E10.CD_TIT_OPR_ETRD
                        , E10.NR_DVS_TIT
                        , E10.CD_OPR_ETRD_MCP
                        , E10.NR_VRS_OPR_ETRD
                        , E20.CD_TIP_EST_OPR
                        , E20.TS_EST_OPR_ETRD
                        , E30.CD_TIP_EST_VRS
                        , E30.TS_EST_VRS
                  FROM
                          DB2SGT.OPR_ETRD_MCP E10
                  INNER JOIN
                          DB2SGT.EST_OPR_ETRD  E20
                  ON
                          E20.CD_CTRA_TIT_VL      = E10.CD_CTRA_TIT_VL
                  AND     E20.CD_OPR_ETRD_MCP     = E10.CD_OPR_ETRD_MCP
                  AND     E20.NR_VRS_OPR_ETRD     = E10.NR_VRS_OPR_ETRD
                  AND     E20.CD_TIP_EST_OPR      = :ENCARTEIRADA
                  AND     E20.TS_EST_OPR_ETRD     =
                         (SELECT
                             MAX(E21.TS_EST_OPR_ETRD)
                          FROM
                                  DB2SGT.EST_OPR_ETRD E21
                          WHERE
                                  E21.CD_CTRA_TIT_VL
                                                  = E20.CD_CTRA_TIT_VL
                          AND     E21.CD_OPR_ETRD_MCP
                                                  = E20.CD_OPR_ETRD_MCP)
                  INNER JOIN
                          DB2SGT.EST_VRS_OPR_ETRD  E30
                  ON
                          E30.CD_CTRA_TIT_VL      = E10.CD_CTRA_TIT_VL
                  AND     E30.CD_OPR_ETRD_MCP     = E10.CD_OPR_ETRD_MCP
                  AND     E30.NR_VRS_OPR_ETRD     = E10.NR_VRS_OPR_ETRD
                  AND     E30.CD_TIP_EST_VRS      = :APROVADA
                  AND     E30.TS_EST_VRS          =
                         (SELECT
                                  MAX(E31.TS_EST_VRS)
                          FROM
                                  DB2SGT.EST_VRS_OPR_ETRD E31
                          WHERE
                                  E31.CD_CTRA_TIT_VL
                                                  = E30.CD_CTRA_TIT_VL
                          AND     E31.CD_OPR_ETRD_MCP
                                                  = E30.CD_OPR_ETRD_MCP)
                  INNER JOIN
                          DB2SGT.CLI_OPR_ETRD E40
                  ON
                          E40.CD_CTRA_TIT_VL      = E10.CD_CTRA_TIT_VL
                  AND     E40.CD_OPR_ETRD_MCP     = E10.CD_OPR_ETRD_MCP
                  AND     E40.NR_VRS_OPR_ETRD     = E10.NR_VRS_OPR_ETRD
                  AND     E40.CD_TIP_FUC_CLI      = :CLIENTE-EMISSOR
                  WHERE
                          E10.DT_CPR_OPR          < CURRENT DATE
                  AND     E10.DT_INC_VGC_VRS_OPR  < CURRENT DATE
                  AND     E10.DT_VNCT_OPR         > CURRENT DATE
      *    --     AND     E10.DT_LQDC_OPR_DRIV    > CURRENT DATE
                                                                 ) E00
           ON
                  E00.CD_CTRA_TIT_VL              =  B.CD_CTRA_TIT_VL
           AND    E00.CD_TIT_OPR_ETRD             =  A.CD_TIT_OPR_ETRD
           AND    E00.NR_DVS_TIT                  =  A.NR_DVS_TIT
           AND    E00.CD_OPR_ETRD_MCP             =  B.CD_OPR_ETRD_MCP
           AND    E00.NR_VRS_OPR_ETRD             =  B.NR_VRS_OPR_ETRD
           WHERE
      *---------> Alterado para Atender o Comportamento da Tela
                  UPPER(COALESCE(E00.CD_TIT_OPR_ETRD, ' '))
                                           LIKE  UPPER(:CD-TIT-OPR-PSQ)
      *  --       E00.CD_CTRA_TIT_VL    = :SGTSL587-CD-CTRA-TIT-PSQ
      *  --AND    E00.CD_TIT_OPR_ETRD   = :SGTSL587-CD-TIT-OPR-PSQ
      *  --AND    E00.NR_DVS_TIT        = :SGTSL587-NR-DVS-TIT-PSQ
      *---------> Paginacao
           AND  ((B.CD_CTRA_TIT_VL      > :SGTSL587-CD-CTRA-TIT-ULT)  OR
                 (B.CD_CTRA_TIT_VL      = :SGTSL587-CD-CTRA-TIT-ULT  AND
                  A.CD_TIT_OPR_ETRD     > :SGTSL587-CD-TIT-OPR-ULT)   OR
                 (B.CD_CTRA_TIT_VL      = :SGTSL587-CD-CTRA-TIT-ULT  AND
                  A.CD_TIT_OPR_ETRD     = :SGTSL587-CD-TIT-OPR-ULT   AND
                  A.NR_DVS_TIT          > :SGTSL587-NR-DVS-TIT-ULT)   OR
                 (B.CD_CTRA_TIT_VL      = :SGTSL587-CD-CTRA-TIT-ULT  AND
                  A.CD_TIT_OPR_ETRD     = :SGTSL587-CD-TIT-OPR-ULT   AND
                  A.NR_DVS_TIT          = :SGTSL587-NR-DVS-TIT-ULT   AND
                  A.DT_EFT_MVTC         < :DT-EFT-MVTC-ULT)           OR
                 (B.CD_CTRA_TIT_VL      = :SGTSL587-CD-CTRA-TIT-ULT  AND
                  A.CD_TIT_OPR_ETRD     = :SGTSL587-CD-TIT-OPR-ULT   AND
                  A.NR_DVS_TIT          = :SGTSL587-NR-DVS-TIT-ULT   AND
                  A.DT_EFT_MVTC         = :DT-EFT-MVTC-ULT           AND
                  A.TS_REG_PGTO         < :TS-REG-PGTO-ULT))
           ORDER BY
                  B.CD_CTRA_TIT_VL
                , A.CD_TIT_OPR_ETRD
                , A.NR_DVS_TIT
                , A.DT_EFT_MVTC DESC
                , A.TS_REG_PGTO DESC
           END-EXEC.

      *--> Cursor de Pesquisa 04
      *    Listar Pagamentos da Operação Estruturada Vigente
      *    com Vinculos Parciais de Recebimento Previsto
      *    de uma Operação Estruturada Especifica
           EXEC SQL
             DECLARE CURSOR-PSQ-04 CURSOR
                     WITH ROWSET POSITIONING FOR
           SELECT
                  B.CD_CTRA_TIT_VL
                , A.CD_TIT_OPR_ETRD
                , A.NR_DVS_TIT
                , A.TS_REG_PGTO
                , B.CD_OPR_ETRD_MCP
                , B.NR_VRS_OPR_ETRD
                , B.NR_SEQL_RCBT_PRVT
                , A.CD_TIP_MVTC_TIT
                , A.CD_TIP_LCTO_CTB
                , A.IN_MVTC_ATV
                , A.TX_DCR_MVTC
                , A.CD_USU_IMP
                , CHAR(TO_CHAR(A.DT_IMP_MVTC_FNCR, 'DDMMYYYY'), 8)
                , CHAR(TO_CHAR(A.DT_EFT_MVTC, 'DDMMYYYY'), 8)
                , IFNULL(A.VL_EFT_MVTC, 0000)
                , IFNULL(B.VALOR_REALIZADO, 0000)
           FROM
                  DB2SGT.DMTV_CX_TIT    A
           INNER JOIN
                 (SELECT  B1.CD_CTRA_TIT_VL
                        , B1.CD_OPR_ETRD_MCP
                        , B1.NR_VRS_OPR_ETRD
                        , B1.NR_SEQL_RCBT_PRVT
                        , B1.CD_TIT_OPR_ETRD
                        , B1.NR_DVS_TIT
                        , B1.TS_REG_PGTO
                        , SUM(B1.VL_RLZD_VCLD) AS VALOR_REALIZADO
                  FROM
                          DB2SGT.RCBT_RLZD_OPR B1
                  WHERE
                          B1.NR_SEQL_RCBT_PRVT
                                        =
                         (SELECT
                             MAX(B2.NR_SEQL_RCBT_PRVT)
                          FROM
                                  DB2SGT.RCBT_RLZD_OPR B2
                          WHERE
                                  B1.CD_CTRA_TIT_VL
                                                  = B2.CD_CTRA_TIT_VL
                          AND     B1.CD_OPR_ETRD_MCP
                                                  = B2.CD_OPR_ETRD_MCP
                          AND     B1.NR_VRS_OPR_ETRD
                                                  = B2.NR_VRS_OPR_ETRD)
                  GROUP BY
                          B1.CD_CTRA_TIT_VL
                        , B1.CD_OPR_ETRD_MCP
                        , B1.NR_VRS_OPR_ETRD
                        , B1.NR_SEQL_RCBT_PRVT
                        , B1.CD_TIT_OPR_ETRD
                        , B1.NR_DVS_TIT
                        , B1.TS_REG_PGTO ) B
           ON     A.CD_CTRA_TIT_VL      = B.CD_CTRA_TIT_VL
           AND    A.CD_TIT_OPR_ETRD     = B.CD_TIT_OPR_ETRD
           AND    A.NR_DVS_TIT          = B.NR_DVS_TIT
           AND    A.TS_REG_PGTO         = B.TS_REG_PGTO
           AND    A.IN_MVTC_ATV         = :MOVIMENTACAO-ATIVA
           AND    A.VL_EFT_MVTC         > B.VALOR_REALIZADO
           INNER JOIN
                 (SELECT  E10.CD_CTRA_TIT_VL
                        , E10.CD_TIT_OPR_ETRD
                        , E10.NR_DVS_TIT
                        , E10.CD_OPR_ETRD_MCP
                        , E10.NR_VRS_OPR_ETRD
                        , E20.CD_TIP_EST_OPR
                        , E20.TS_EST_OPR_ETRD
                        , E30.CD_TIP_EST_VRS
                        , E30.TS_EST_VRS
                  FROM
                          DB2SGT.OPR_ETRD_MCP E10
                  INNER JOIN
                          DB2SGT.EST_OPR_ETRD  E20
                  ON
                          E20.CD_CTRA_TIT_VL      = E10.CD_CTRA_TIT_VL
                  AND     E20.CD_OPR_ETRD_MCP     = E10.CD_OPR_ETRD_MCP
                  AND     E20.NR_VRS_OPR_ETRD     = E10.NR_VRS_OPR_ETRD
                  AND     E20.CD_TIP_EST_OPR      = :ENCARTEIRADA
                  AND     E20.TS_EST_OPR_ETRD     =
                         (SELECT
                             MAX(E21.TS_EST_OPR_ETRD)
                          FROM
                                  DB2SGT.EST_OPR_ETRD E21
                          WHERE
                                  E21.CD_CTRA_TIT_VL
                                                  = E20.CD_CTRA_TIT_VL
                          AND     E21.CD_OPR_ETRD_MCP
                                                  = E20.CD_OPR_ETRD_MCP)
                  INNER JOIN
                          DB2SGT.EST_VRS_OPR_ETRD  E30
                  ON
                          E30.CD_CTRA_TIT_VL      = E10.CD_CTRA_TIT_VL
                  AND     E30.CD_OPR_ETRD_MCP     = E10.CD_OPR_ETRD_MCP
                  AND     E30.NR_VRS_OPR_ETRD     = E10.NR_VRS_OPR_ETRD
                  AND     E30.CD_TIP_EST_VRS      = :APROVADA
                  AND     E30.TS_EST_VRS          =
                         (SELECT
                                  MAX(E31.TS_EST_VRS)
                          FROM
                                  DB2SGT.EST_VRS_OPR_ETRD E31
                          WHERE
                                  E31.CD_CTRA_TIT_VL
                                                  = E30.CD_CTRA_TIT_VL
                          AND     E31.CD_OPR_ETRD_MCP
                                                  = E30.CD_OPR_ETRD_MCP)
                  INNER JOIN
                          DB2SGT.CLI_OPR_ETRD E40
                  ON
                          E40.CD_CTRA_TIT_VL      = E10.CD_CTRA_TIT_VL
                  AND     E40.CD_OPR_ETRD_MCP     = E10.CD_OPR_ETRD_MCP
                  AND     E40.NR_VRS_OPR_ETRD     = E10.NR_VRS_OPR_ETRD
                  AND     E40.CD_TIP_FUC_CLI      = :CLIENTE-EMISSOR
                  WHERE
                          E10.DT_CPR_OPR          < CURRENT DATE
                  AND     E10.DT_INC_VGC_VRS_OPR  < CURRENT DATE
                  AND     E10.DT_VNCT_OPR         > CURRENT DATE
      *    --     AND     E10.DT_LQDC_OPR_DRIV    > CURRENT DATE
                                                                 ) E00
           ON
                  E00.CD_CTRA_TIT_VL              =  B.CD_CTRA_TIT_VL
           AND    E00.CD_TIT_OPR_ETRD             =  A.CD_TIT_OPR_ETRD
           AND    E00.NR_DVS_TIT                  =  A.NR_DVS_TIT
           AND    E00.CD_OPR_ETRD_MCP             =  B.CD_OPR_ETRD_MCP
           AND    E00.NR_VRS_OPR_ETRD             =  B.NR_VRS_OPR_ETRD
           WHERE
                  B.CD_CTRA_TIT_VL      = :SGTSL587-CD-CTRA-TIT-PSQ
           AND    B.CD_OPR_ETRD_MCP     = :SGTSL587-CD-OPR-ETRD-PSQ
           AND    B.NR_VRS_OPR_ETRD     = :SGTSL587-NR-VRS-OPR-PSQ
           AND    B.NR_SEQL_RCBT_PRVT   = :NR-SEQL-RCBT-PRVT-PSQ
      *---------> Paginacao
           AND  ((B.CD_CTRA_TIT_VL      > :SGTSL587-CD-CTRA-TIT-ULT)  OR
                 (B.CD_CTRA_TIT_VL      = :SGTSL587-CD-CTRA-TIT-ULT  AND
                  A.CD_TIT_OPR_ETRD     > :SGTSL587-CD-TIT-OPR-ULT)   OR
                 (B.CD_CTRA_TIT_VL      = :SGTSL587-CD-CTRA-TIT-ULT  AND
                  A.CD_TIT_OPR_ETRD     = :SGTSL587-CD-TIT-OPR-ULT   AND
                  A.NR_DVS_TIT          > :SGTSL587-NR-DVS-TIT-ULT)   OR
                 (B.CD_CTRA_TIT_VL      = :SGTSL587-CD-CTRA-TIT-ULT  AND
                  A.CD_TIT_OPR_ETRD     = :SGTSL587-CD-TIT-OPR-ULT   AND
                  A.NR_DVS_TIT          = :SGTSL587-NR-DVS-TIT-ULT   AND
                  A.DT_EFT_MVTC         < :DT-EFT-MVTC-ULT)           OR
                 (B.CD_CTRA_TIT_VL      = :SGTSL587-CD-CTRA-TIT-ULT  AND
                  A.CD_TIT_OPR_ETRD     = :SGTSL587-CD-TIT-OPR-ULT   AND
                  A.NR_DVS_TIT          = :SGTSL587-NR-DVS-TIT-ULT   AND
                  A.DT_EFT_MVTC         = :DT-EFT-MVTC-ULT           AND
                  A.TS_REG_PGTO         < :TS-REG-PGTO-ULT))
           ORDER BY
                  B.CD_CTRA_TIT_VL
                , A.CD_TIT_OPR_ETRD
                , A.NR_DVS_TIT
                , A.DT_EFT_MVTC DESC
                , A.TS_REG_PGTO DESC
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

           MOVE  'SGTSL587-RPST'       TO  NM-CTNR

           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR                              )
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

           MOVE  'PRM-ERRO'            TO  NM-CTNR

           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR                              )
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
           MOVE  LENGTH OF  SGTSL587-RQSC
                                       TO  QT-CCT-EPRO

           MOVE  'SGTSL587-RQSC'       TO  NM-CTNR

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                              )
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
                      CONTAINER ( 'SGTSL587-RQSC'                      )
                      CHANNEL   ( NM-CNL                               )
                      FLENGTH   ( LENGTH OF SGTSL587-RQSC              )
                      INTO      ( SGTSL587-RQSC                        )
           END-EXEC
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           IF  NOT SGTSL587-VCL-PARCIAL-TODOS     AND
               NOT SGTSL587-VCL-PARCIAL-CARTEIRA  AND
               NOT SGTSL587-VCL-PARCIAL-TITULO    AND
               NOT SGTSL587-VCL-PARCIAL-OPERACAO
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0107              TO  MSG1-CD-ERRO-TS
               MOVE  'Código tipo serviço inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validar Carteira TVM
           IF  SGTSL587-CD-CTRA-TIT-PSQ    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL587-CD-CTRA-TIT-PSQ    EQUAL ZEROS  AND
               SGTSL587-VCL-PARCIAL-CARTEIRA
      *        (SGTSL587-VCL-PARCIAL-CARTEIRA            OR
      *         SGTSL587-VCL-PARCIAL-TITULO              OR
      *         SGTSL587-VCL-PARCIAL-OPERACAO)
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
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Chave Título TVM
           IF  SGTSL587-CD-TIT-OPR-PSQ     EQUAL SPACES  AND
               SGTSL587-VCL-PARCIAL-TITULO
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0231              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do título da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  SGTSL587-CD-TIT-OPR-PSQ
                                       TO  CD-TIT-OPR-PSQ
           INSPECT  CD-TIT-OPR-PSQ         REPLACING ALL SPACES BY '%'

           IF  SGTSL587-NR-DVS-TIT-PSQ     NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0013              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da divisão de título inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *-->     Retirado para atender melhor o Comportamento na Tela
      *    IF  SGTSL587-NR-DVS-TIT-PSQ     EQUAL ZEROS  AND
      *        SGTSL587-VCL-PARCIAL-TITULO
      *        SET   ERRO-PROGRAMADO   TO  TRUE
      *        MOVE  0232              TO  MSG1-CD-ERRO-TS
      *        MOVE  'Número da divisão do título da operação não inform
      *-             'ado.'            TO  MSG1-TX-ERRO-TS
      *        PERFORM  999000-GRAVA-ERRO-E-ENCERRA
      *    END-IF

           .
       160000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170000-VALIDAR-OPERACAO         SECTION.
      *--> Validar Chave Operação Estruturada
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSL587-CD-OPR-ETRD-PSQ    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL587-CD-OPR-ETRD-PSQ    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL587-CD-OPR-ETRD-PSQ    EQUAL ZEROS  AND
               SGTSL587-VCL-PARCIAL-OPERACAO
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL587-NR-VRS-OPR-PSQ     NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL587-NR-VRS-OPR-PSQ     EQUAL ZEROS  AND
               SGTSL587-VCL-PARCIAL-OPERACAO
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       170000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       180000-VALIDAR-RECHAMADA        SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Carteira TVM
           IF  SGTSL587-CD-CTRA-TIT-ULT    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validar Chave Título TVM de Rechamada
           IF  SGTSL587-CD-TIT-OPR-ULT     EQUAL  SPACES  AND
               SGTSL587-CD-CTRA-TIT-ULT    POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0231              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do título da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL587-NR-DVS-TIT-ULT     EQUAL  ZEROS  AND
               SGTSL587-CD-CTRA-TIT-ULT    POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0232              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da divisão do título da operação não inform
      -              'ado.'            TO  MSG1-TX-ERRO-TS
              PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL587-NR-DVS-TIT-ULT     NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0013              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da divisão de título inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validar Data Efetiva e Timestamp de Registro de Rechamada
           MOVE CORR  SGTSL587-DT-EFT-MVTC-ULT-R
                                       TO  DT-EFT-MVTC-ULT-R

           IF  SGTSL587-DT-EFT-MVTC-ULT    NOT EQUAL  SPACES
               IF  ANO OF DT-EFT-MVTC-ULT-R
                                           NOT NUMERIC  OR
                   MES OF DT-EFT-MVTC-ULT-R
                                           NOT NUMERIC  OR
                   DIA OF DT-EFT-MVTC-ULT-R
                                           NOT NUMERIC
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0342          TO  MSG1-CD-ERRO-TS
                   MOVE  'Data efetiva da movimentação inválida.'
                                       TO  MSG1-TX-ERRO-TS
                   PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               END-IF
               IF  ANO OF DT-EFT-MVTC-ULT-R
                                           NOT POSITIVE  OR
                   MES OF DT-EFT-MVTC-ULT-R
                                           NOT POSITIVE  OR
                   DIA OF DT-EFT-MVTC-ULT-R
                                           NOT POSITIVE
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0342          TO  MSG1-CD-ERRO-TS
                   MOVE  'Data efetiva da movimentação inválida.'
                                       TO  MSG1-TX-ERRO-TS
                   PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           END-IF

           MOVE  SGTSL587-TS-REG-PGTO-ULT
                                       TO  TS-REG-PGTO-ULT

           IF  SGTSL587-TS-REG-PGTO-ULT    NOT EQUAL  SPACES
               IF  ANO OF TS-REG-PGTO-ULT-R
                                           NOT NUMERIC  OR
                   MES OF TS-REG-PGTO-ULT-R
                                           NOT NUMERIC  OR
                   DIA OF TS-REG-PGTO-ULT-R
                                           NOT NUMERIC  OR
                   HORA OF TS-REG-PGTO-ULT-R
                                           NOT NUMERIC  OR
                   MINUTO OF TS-REG-PGTO-ULT-R
                                           NOT NUMERIC  OR
                   SEGUNDO OF TS-REG-PGTO-ULT-R
                                           NOT NUMERIC  OR
                   MILISEGUNDO OF TS-REG-PGTO-ULT-R
                                           NOT NUMERIC
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0670          TO  MSG1-CD-ERRO-TS
                   MOVE  'Timestamp do registro de pagamento inválido.'
                                       TO  MSG1-TX-ERRO-TS
                   PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               END-IF
               IF  ANO OF TS-REG-PGTO-ULT-R
                                           NOT POSITIVE  OR
                   MES OF TS-REG-PGTO-ULT-R
                                           NOT POSITIVE  OR
                   DIA OF TS-REG-PGTO-ULT-R
                                           NOT POSITIVE  OR
                   HORA OF TS-REG-PGTO-ULT-R
                                           NEGATIVE  OR
                   MINUTO OF TS-REG-PGTO-ULT-R
                                           NEGATIVE  OR
                   SEGUNDO OF TS-REG-PGTO-ULT-R
                                           NEGATIVE  OR
                   MILISEGUNDO OF TS-REG-PGTO-ULT-R
                                           NEGATIVE
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0670          TO  MSG1-CD-ERRO-TS
                   MOVE  'Timestamp do registro de pagamento inválido.'
                                       TO  MSG1-TX-ERRO-TS
                   PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           END-IF

      *--> Validar Chave Operação Estruturada
           IF  SGTSL587-CD-OPR-ETRD-ULT    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL587-CD-OPR-ETRD-ULT    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL587-NR-VRS-OPR-ULT     NEGATIVE
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

      *--> Se Data Efetiva não informado, formatar no padrão DB2
           IF  SGTSL587-DT-EFT-MVTC-ULT    EQUAL  SPACES
               MOVE  '9999-12-31'
                                       TO  DT-EFT-MVTC-ULT
           END-IF

      *--> Se Timestamp não informado, formatar no padrão DB2
           IF  SGTSL587-TS-REG-PGTO-ULT    EQUAL  SPACES
               MOVE  '9999-12-31-23.59.59.000000'
                                       TO  TS-REG-PGTO-ULT
           END-IF

           EVALUATE  TRUE
               WHEN  SGTSL587-VCL-PARCIAL-OPERACAO
                     PERFORM  210000-BUSCAR-SEQUENCIAL-PRVT
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

           PERFORM  971000-ABRE-CURSOR-PSQ

           PERFORM  972000-LER-CURSOR-PSQ

           PERFORM  973000-FECHA-CURSOR-PSQ

           PERFORM  220000-MONTAR-LISTA
                    VARYING                SGTSL587-IC-TAB
                    FROM                   0001 BY 0001
                    UNTIL                  SGTSL587-IC-TAB  GREATER
                                           SGTSL587-QT-OCR-TAB
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-BUSCAR-SEQUENCIAL-PRVT       SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           EXEC SQL
             SELECT  IFNULL (MAX(A.NR_SEQL_RCBT_PRVT), 0000)
             INTO    :NR-SEQL-RCBT-PRVT-PSQ
               FROM  DB2SGT.RCBT_PRVT_OPR_ETRD  A
              WHERE  A.CD_CTRA_TIT_VL
                                      =  :SGTSL587-CD-CTRA-TIT-PSQ
                AND  A.CD_OPR_ETRD_MCP
                                      =  :SGTSL587-CD-OPR-ETRD-PSQ
                AND  A.NR_VRS_OPR_ETRD
                                      =  :SGTSL587-NR-VRS-OPR-PSQ
           END-EXEC

           IF  NR-SEQL-RCBT-PRVT-PSQ       EQUAL  ZEROS  OR
               NR-SEQL-RCBT-PRVT-PSQ       NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0320              TO  MSG1-CD-ERRO-TS
               MOVE  'Recebimento previsto da operação estruturada inexi
      -              'stente.'         TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  NR-SEQL-RCBT-PRVT-PSQ       GREATER  ZEROS
               MOVE  NR-SEQL-RCBT-PRVT-PSQ
                                       TO  NR-SEQL-RCBT-PRVT-INI
                                           NR-SEQL-RCBT-PRVT-FIM
           END-IF

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-MONTAR-LISTA             SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

      *--> Informações Título e Operação Estruturada
           MOVE  CD-CTRA-TIT(SGTSL587-IC-TAB)
             TO  SGTSL587-CD-CTRA-TIT(SGTSL587-IC-TAB)

           MOVE  CD-TIT-OPR-ETRD(SGTSL587-IC-TAB)
             TO  SGTSL587-CD-TIT-OPR(SGTSL587-IC-TAB)

           MOVE  NR-DVS-TIT(SGTSL587-IC-TAB)
             TO  SGTSL587-NR-DVS-TIT(SGTSL587-IC-TAB)

           MOVE  CD-OPR-ETRD-MCP(SGTSL587-IC-TAB)
             TO  SGTSL587-CD-OPR-ETRD(SGTSL587-IC-TAB)

           MOVE  NR-VRS-OPR-ETRD(SGTSL587-IC-TAB)
             TO  SGTSL587-NR-VRS-OPR(SGTSL587-IC-TAB)

      *--> Informações Pagamento - Demonstrativo de Caixa
           MOVE  TS-REG-PGTO(SGTSL587-IC-TAB)
             TO  SGTSL587-TS-REG-PGTO(SGTSL587-IC-TAB)

           MOVE  CD-TIP-MVTC-TIT(SGTSL587-IC-TAB)
             TO  SGTSL587-CD-TIP-MVTC-TIT(SGTSL587-IC-TAB)

           MOVE  CD-TIP-LCTO-CTB(SGTSL587-IC-TAB)
             TO  SGTSL587-CD-TIP-LCTO-CTB(SGTSL587-IC-TAB)

           MOVE  DT-EFT-MVTC(SGTSL587-IC-TAB)
             TO  SGTSL587-DT-EFT-MVTC(SGTSL587-IC-TAB)

           MOVE  VL-EFT-MVTC(SGTSL587-IC-TAB)
             TO  SGTSL587-VL-EFT-MVTC(SGTSL587-IC-TAB)

           MOVE  VL-RLZD-VCLD(SGTSL587-IC-TAB)
             TO  SGTSL587-VL-RLZD-VCLD(SGTSL587-IC-TAB)

           MOVE  IN-MVTC-ATV(SGTSL587-IC-TAB)
             TO  SGTSL587-IN-MVTC-ATV(SGTSL587-IC-TAB)

           MOVE  CD-USU-IMP(SGTSL587-IC-TAB)
             TO  SGTSL587-CD-USU-IMP(SGTSL587-IC-TAB)

           MOVE  DT-IMP-MVTC-FNCR(SGTSL587-IC-TAB)
             TO  SGTSL587-DT-IMP-MVTC-FNCR(SGTSL587-IC-TAB)

           MOVE  TX-DCR-MVTC(SGTSL587-IC-TAB)
             TO  SGTSL587-TX-DCR-MVTC(SGTSL587-IC-TAB)

           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'SGTSL587-RPST'       TO  NM-CTNR

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                              )
                      CHANNEL   ( NM-CNL                               )
                      FLENGTH   ( LENGTH OF SGTSL587-RPST              )
                      FROM      ( SGTSL587-RPST                        )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       971000-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

           EVALUATE  TRUE
               WHEN  SGTSL587-VCL-PARCIAL-TODOS
                     EXEC SQL
                          OPEN CURSOR-PSQ-01
                     END-EXEC
               WHEN  SGTSL587-VCL-PARCIAL-CARTEIRA
                     EXEC SQL
                          OPEN CURSOR-PSQ-02
                     END-EXEC
               WHEN  SGTSL587-VCL-PARCIAL-TITULO
                     EXEC SQL
                          OPEN CURSOR-PSQ-03
                     END-EXEC
               WHEN  SGTSL587-VCL-PARCIAL-OPERACAO
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
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-REG-SELD

           MOVE  SPACES                TO  VINCULO-PARCIAL
           INITIALIZE                      VINCULO-PARCIAL
                                           REPLACING NUMERIC BY  ZEROS

           EVALUATE  TRUE

               WHEN  SGTSL587-VCL-PARCIAL-TODOS
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-01
                                FOR 0401 ROWS
                           INTO
                                :CD-CTRA-TIT
                              , :CD-TIT-OPR-ETRD
                              , :NR-DVS-TIT
                              , :TS-REG-PGTO
                              , :CD-OPR-ETRD-MCP
                              , :NR-VRS-OPR-ETRD
                              , :NR-SEQL-RCBT-PRVT
                              , :CD-TIP-MVTC-TIT
                              , :CD-TIP-LCTO-CTB
                              , :IN-MVTC-ATV
                              , :TX-DCR-MVTC
                              , :CD-USU-IMP
                              , :DT-IMP-MVTC-FNCR
                              , :DT-EFT-MVTC
                              , :VL-EFT-MVTC
                                :VL-EFT-MVTC-N
                              , :VL-RLZD-VCLD
                                :VL-RLZD-VCLD-N
                     END-EXEC

               WHEN  SGTSL587-VCL-PARCIAL-CARTEIRA
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-02
                                FOR 0401 ROWS
                           INTO
                                :CD-CTRA-TIT
                              , :CD-TIT-OPR-ETRD
                              , :NR-DVS-TIT
                              , :TS-REG-PGTO
                              , :CD-OPR-ETRD-MCP
                              , :NR-VRS-OPR-ETRD
                              , :NR-SEQL-RCBT-PRVT
                              , :CD-TIP-MVTC-TIT
                              , :CD-TIP-LCTO-CTB
                              , :IN-MVTC-ATV
                              , :TX-DCR-MVTC
                              , :CD-USU-IMP
                              , :DT-IMP-MVTC-FNCR
                              , :DT-EFT-MVTC
                              , :VL-EFT-MVTC
                                :VL-EFT-MVTC-N
                              , :VL-RLZD-VCLD
                                :VL-RLZD-VCLD-N
                     END-EXEC

               WHEN  SGTSL587-VCL-PARCIAL-TITULO
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-03
                                FOR 0401 ROWS
                           INTO
                                :CD-CTRA-TIT
                              , :CD-TIT-OPR-ETRD
                              , :NR-DVS-TIT
                              , :TS-REG-PGTO
                              , :CD-OPR-ETRD-MCP
                              , :NR-VRS-OPR-ETRD
                              , :NR-SEQL-RCBT-PRVT
                              , :CD-TIP-MVTC-TIT
                              , :CD-TIP-LCTO-CTB
                              , :IN-MVTC-ATV
                              , :TX-DCR-MVTC
                              , :CD-USU-IMP
                              , :DT-IMP-MVTC-FNCR
                              , :DT-EFT-MVTC
                              , :VL-EFT-MVTC
                                :VL-EFT-MVTC-N
                              , :VL-RLZD-VCLD
                                :VL-RLZD-VCLD-N
                     END-EXEC

               WHEN  SGTSL587-VCL-PARCIAL-OPERACAO
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-04
                                FOR 0401 ROWS
                           INTO
                                :CD-CTRA-TIT
                              , :CD-TIT-OPR-ETRD
                              , :NR-DVS-TIT
                              , :TS-REG-PGTO
                              , :CD-OPR-ETRD-MCP
                              , :NR-VRS-OPR-ETRD
                              , :NR-SEQL-RCBT-PRVT
                              , :CD-TIP-MVTC-TIT
                              , :CD-TIP-LCTO-CTB
                              , :IN-MVTC-ATV
                              , :TX-DCR-MVTC
                              , :CD-USU-IMP
                              , :DT-IMP-MVTC-FNCR
                              , :DT-EFT-MVTC
                              , :VL-EFT-MVTC
                                :VL-EFT-MVTC-N
                              , :VL-RLZD-VCLD
                                :VL-RLZD-VCLD-N
                     END-EXEC

           END-EVALUATE

           MOVE  SQLERRD(3)            TO  QT-REG-SELD

           IF  QT-REG-SELD                 EQUAL  0401
               SET   SGTSL587-TEM-MAIS-REGISTRO
                                       TO  TRUE
               MOVE  0400              TO  QT-REG-SELD
           END-IF

           MOVE  QT-REG-SELD           TO  SGTSL587-QT-OCR-TAB

           MOVE  SQLCODE               TO  CD-FIM-CURSOR-PSQ

           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973000-FECHA-CURSOR-PSQ         SECTION.
      *-----------------------------------------------------------------
           MOVE  0017                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'N'                   TO  IN-CURSOR-PSQ-ABTO

           EVALUATE  TRUE
               WHEN  SGTSL587-VCL-PARCIAL-TODOS
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-01
                     END-EXEC
               WHEN  SGTSL587-VCL-PARCIAL-CARTEIRA
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-02
                     END-EXEC
               WHEN  SGTSL587-VCL-PARCIAL-TITULO
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-03
                     END-EXEC
               WHEN  SGTSL587-VCL-PARCIAL-OPERACAO
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
           MOVE  'L587-001-CD-TIP-SRVC'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL587-CD-TIP-SRVC  TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L587-002-CD-CTRA-TIT-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL587-CD-CTRA-TIT-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L587-003-CD-TIT-OPR-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL587-CD-TIT-OPR-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L587-004-NR-DVS-TIT-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL587-NR-DVS-TIT-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L587-005-CD-OPR-ETRD-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSL587-CD-OPR-ETRD-PSQ    NUMERIC
               MOVE  SGTSL587-CD-OPR-ETRD-PSQ
                                       TO  CD-OPR-ETRD
               MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L587-006-NR-VRS-OPR-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL587-NR-VRS-OPR-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L587-007-CD-CTRA-TIT-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL587-CD-CTRA-TIT-ULT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L587-008-CD-TIT-OPR-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL587-CD-TIT-OPR-ULT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L587-009-NR-DVS-TIT-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL587-NR-DVS-TIT-ULT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L587-010-DT-EFT-MVTC-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL587-DT-EFT-MVTC-ULT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L587-011-TS-REG-PGTO-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL587-TS-REG-PGTO-ULT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L587-012-CD-OPR-ETRD-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSL587-CD-OPR-ETRD-ULT    NUMERIC
               MOVE  SGTSL587-CD-OPR-ETRD-ULT
                                       TO  CD-OPR-ETRD
               MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L587-013-NR-VRS-OPR-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL587-NR-VRS-OPR-ULT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT


      *--> Adiciona as Variaveis de Saida para Monitoracao
           MOVE  'L587-014-QT-OCR-TAB'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL587-QT-OCR-TAB   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L587-015-IC-TAB'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL587-IC-TAB       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava a Ocorrencia de Erro no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

      *--> Grava a Ocorrencia de Erro no Container de Erro
           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

           MOVE  'PRM-ERRO'            TO  NM-CTNR

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                              )
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

      *-------------------------- FIM SGTSL587 -------------------------