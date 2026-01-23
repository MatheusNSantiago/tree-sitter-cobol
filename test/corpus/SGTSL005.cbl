      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL005
      * ANALISTA..: CINTIA IGLESIAS
      * AUTOR.....: SOILA AKAMATSU
      * OBJETIVO..: LISTAR OPERAÇÃO ESTRUTURADA DE MERCADO DE CAPITAIS
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS003 21/09/2020 F2020988 - DE 2020/145195
      *          inclusao do cliente emitente na pesquisa 3
      * VRS002 15/07/2019 F2804421 - DE 2016/145195 - Inclusão da lista
      *          de pesquisa para cursor3 - Última versão vigente de
      *          cada operação estruturada.
      *
      * VRS001 30/05/2018 F8785286 - DE 2016/145195 - Inclusão dos cpos
      *          de nulidade DT-CPR-OPR-N e DT-VNCT-OPR-N por erro nos
      *          testes quando os respectivos campos são nulo.
      *        19/05/2017 F9188847 - DE 2015/137233 -
      *                              Alteração do tam dos campos:
      *                              VL-PU-EMS-TIT-OPR e
      *                              VL-PU-CPR-TIT-OPR
      *        06/02/2017 F2804421 - DE 2015/137233 -
      *                               Alteração no tamanho dos campos
      *                              SGTSL005-QT-PAPL-TIT-EMS e
      *                              SGTSL005-QT-PAPL-TIT-CPR
      *        09/08/2016 F2054109 DE 2015/137233 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSL005.

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
                                       '*** S.S. Comeca Aqui ***'.
       77  CTE-PROG                    PIC  X(17)
                                       VALUE '*** SGTSL005 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS003'.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  CD-OPR-ETRD-INI                 PIC S9(11) COMP-3 VALUE 0.
       01  CD-OPR-ETRD-FIM                 PIC S9(11) COMP-3
                                           VALUE 99999999999.
       01  CD-CTRA-TIT-VL-INI              PIC S9(9) COMP-5 VALUE 0.
       01  CD-CTRA-TIT-VL-FIM              PIC S9(9) COMP-5
                                           VALUE 999999999.
       01  CD-PRD-INI                      PIC S9(4) COMP-5 VALUE 0.
       01  CD-PRD-FIM                      PIC S9(4) COMP-5 VALUE 9999.
       01  CD-MDLD-INI                     PIC S9(4) COMP-5 VALUE 0.
       01  CD-MDLD-FIM                     PIC S9(4) COMP-5 VALUE 9999.
       01  CD-UOR-DTTR-OPR-INI             PIC S9(9) COMP-5 VALUE 0.
       01  CD-UOR-DTTR-OPR-FIM             PIC S9(9) COMP-5
                                           VALUE 999999999.
       01  CD-CLI-INI                      PIC S9(9) COMP-5 VALUE 0.
       01  CD-CLI-FIM                      PIC S9(9) COMP-5
                                           VALUE 999999999.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP-D       PIC  9(11).

      *--> Campos utilizados para leitura
       01  REG-WS.
           03  CD-CTRA-TIT-VL              PIC S9(9)  COMP-5 VALUE 0.
           03  CD-OPR-ETRD-MCP             PIC S9(11) COMP-3 VALUE 0.
           03  NR-VRS-OPR-ETRD             PIC S9(4) COMP-5 VALUE 0.
           03  CD-TIT-OPR-ETRD             PIC X(12) VALUE ' '.
           03  NR-DVS-TIT                  PIC S9(4) COMP-5 VALUE 0.
           03  CD-PRD                      PIC S9(4) COMP-5 VALUE 0.
           03  CD-MDLD                     PIC S9(4) COMP-5 VALUE 0.
           03  QT-PAPL-TIT-EMS             PIC S9(9) COMP-5 VALUE 0.
           03  VL-PU-EMS-TIT-OPR      PIC S9(9)V9(8) COMP-3 VALUE 0.
           03  DT-EMS-TIT-OPR              PIC X(10) VALUE ' '.
           03  DT-EMS-TIT-OPR-R REDEFINES
                   DT-EMS-TIT-OPR.
               05  DIA                     PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  MES                     PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  ANO                     PIC  9(4).
           03  QT-PAPL-TIT-CPR             PIC S9(9) COMP-5 VALUE 0.
           03  VL-CPR-OPR             PIC S9(15)V9(2) COMP-3 VALUE 0.
           03  VL-PU-CPR-TIT-OPR      PIC S9(9)V9(8) COMP-3 VALUE 0.
           03  DT-CPR-OPR                  PIC X(10) VALUE ' '.
           03  DT-CPR-OPR-R REDEFINES
                  DT-CPR-OPR.
               05  DIA                     PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  MES                     PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  ANO                     PIC  9(4).

           03  DT-VNCT-OPR                 PIC X(10) VALUE ' '.
           03  DT-VNCT-OPR-R REDEFINES
                  DT-VNCT-OPR.
               05  DIA                     PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  MES                     PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  ANO                     PIC  9(4).
           03  CD-UOR-DTTR-OPR        PIC S9(9) COMP-5 VALUE 0.
           03  CD-CLI                 PIC S9(9) COMP-5 VALUE 0.

           03  CD-USU-ULT-ATL         PIC X(8) VALUE ' '.
           03  TS-ULT-ATL             PIC  X(26) VALUE ' '.


           03  DT-CMPS-FUT-NOTA       PIC  X(10)  VALUE SPACES.
           03  DT-CMPS-FUT-NOTA-R REDEFINES
                  DT-CMPS-FUT-NOTA.
               05  DIA                     PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  MES                     PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  ANO                     PIC  9(4).

           03  DT-EVT-VCLD-VRS        PIC  X(10)  VALUE SPACES.
           03  DT-EVT-VCLD-VRS-R REDEFINES
                  DT-EVT-VCLD-VRS.
               05  DIA                     PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  MES                     PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  ANO                     PIC  9(4).

           03  VL-EVT-VCLD-VRS
                                       PIC S9(15)V9(2) COMP-3 VALUE 0.
           03  CD-TIP-VRS-OPR
                                       PIC S9(4) COMP-5 VALUE 0.

           03  DT-INC-VGC-VRS-OPR        PIC  X(10)  VALUE SPACES.
           03  DT-INC-VGC-VRS-OPR-R REDEFINES
                  DT-INC-VGC-VRS-OPR.
               05  DIA                     PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  MES                     PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  ANO                     PIC  9(4).

           03  TX-OBS-EVT                  PIC  X(100) VALUE SPACES.

      *--> Indicadores de nulidades
       01  CD-TIT-OPR-ETRD-N               PIC S9(4) COMP-5 VALUE 0.
       01  NR-DVS-TIT-N                    PIC S9(4) COMP-5 VALUE 0.
       01  DT-EMS-TIT-OPR-N                PIC S9(4) COMP-5 VALUE 0.
       01  CD-USU-ULT-ATL-N                PIC S9(4) COMP-5 VALUE 0.
       01  TS-ULT-ATL-N                    PIC S9(4) COMP-5 VALUE 0.
       01  DT-CMPS-FUT-NOTA-N              PIC S9(4) COMP-5 VALUE 0.
       01  DT-INC-VGC-VRS-OPR-N            PIC S9(4) COMP-5 VALUE 0.
       01  VL-EVT-VCLD-VRS-N               PIC S9(4) COMP-5 VALUE 0.
       01  DT-EVT-VCLD-VRS-N               PIC S9(4) COMP-5 VALUE 0.
       01  TX-OBS-EVT-N                    PIC S9(4) COMP-5 VALUE 0.
       01  DT-CPR-OPR-N                    PIC S9(4) COMP-5 VALUE 0.
       01  DT-VNCT-OPR-N                   PIC S9(4) COMP-5 VALUE 0.

      *--> Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ           PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ            VALUE +100.
           03  IN-CURSOR-PSQ-ABTO          PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO          VALUE 'S'.


      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book do container de requisicao e resposta deste modulo
      *    DB2SGT.OPR_ETRD_MCP
-INC SGTKL005


      *-----------------------------------------------------------------
      *    Cursores de Pesquisa
      *-----------------------------------------------------------------

      *--> Cursor 01
      *    Listar as Operações Estruradas e Todas as suas Versões
           EXEC SQL
             DECLARE CURSOR-PSQ-1 CURSOR  FOR
             SELECT
               A.CD_CTRA_TIT_VL,
               A.CD_OPR_ETRD_MCP,
               A.NR_VRS_OPR_ETRD,
               A.CD_TIT_OPR_ETRD,
               A.NR_DVS_TIT,
               A.CD_PRD,
               A.CD_MDLD,
               A.QT_PAPL_TIT_EMS,
               A.VL_PU_EMS_TIT_OPR,
               A.DT_EMS_TIT_OPR,
               A.QT_PAPL_TIT_CPR,
               A.VL_CPR_OPR,
               A.VL_PU_CPR_TIT_OPR,
               A.DT_CPR_OPR,
               A.DT_VNCT_OPR,
               A.CD_UOR_DTTR_OPR,
               B.CD_CLI,
               A.CD_USU_ULT_ATL,
               A.TS_ULT_ATL,
               A.DT_CMPS_FUT_NOTA,
               A.DT_EVT_VCLD_VRS,
               A.VL_EVT_VCLD_VRS,
               A.CD_TIP_VRS_OPR,
               A.DT_INC_VGC_VRS_OPR,
               A.TX_OBS_EVT
             FROM
                 DB2SGT.OPR_ETRD_MCP A
             INNER JOIN
                 DB2SGT.CLI_OPR_ETRD B
             ON
                 A.CD_CTRA_TIT_VL  = B.CD_CTRA_TIT_VL
             AND A.CD_OPR_ETRD_MCP = B.CD_OPR_ETRD_MCP
             AND A.NR_VRS_OPR_ETRD = B.NR_VRS_OPR_ETRD

            WHERE

              B.CD_TIP_FUC_CLI = 1

             AND ( A.CD_CTRA_TIT_VL > :SGTSL005-CD-ULT-CTRA-TIT-VL
             OR  ( A.CD_CTRA_TIT_VL = :SGTSL005-CD-ULT-CTRA-TIT-VL
                   AND A.CD_OPR_ETRD_MCP > :SGTSL005-CD-ULT-OPR-ETRD)
             OR   ( A.CD_CTRA_TIT_VL = :SGTSL005-CD-ULT-CTRA-TIT-VL
                    AND A.CD_OPR_ETRD_MCP = :SGTSL005-CD-ULT-OPR-ETRD
                    AND A.NR_VRS_OPR_ETRD >
                                      :SGTSL005-NR-ULT-VRS-OPR-ETRD))

              AND (A.CD_CTRA_TIT_VL >= :CD-CTRA-TIT-VL-INI
                   AND  A.CD_CTRA_TIT_VL <= :CD-CTRA-TIT-VL-FIM)

              AND (A.CD_OPR_ETRD_MCP >= :CD-OPR-ETRD-INI
                   AND A.CD_OPR_ETRD_MCP <= :CD-OPR-ETRD-FIM)

              AND (A.CD_PRD >= :CD-PRD-INI
                    AND A.CD_PRD <= :CD-PRD-FIM)

              AND (A.CD_MDLD >= :CD-MDLD-INI
                    AND A.CD_MDLD <= :CD-MDLD-FIM)

               AND (A.CD_UOR_DTTR_OPR >= :CD-UOR-DTTR-OPR-INI
                    AND A.CD_UOR_DTTR_OPR <= :CD-UOR-DTTR-OPR-FIM)

               AND UPPER(COALESCE(A.CD_TIT_OPR_ETRD, ' ')) LIKE
                                UPPER(:SGTSL005-E-CD-TIT-OPR-ETRD)

               AND  (B.CD_CLI >= :CD-CLI-INI
                     AND B.CD_CLI <= :CD-CLI-FIM)

            ORDER BY
                  A.CD_CTRA_TIT_VL,
            	     A.CD_OPR_ETRD_MCP,
                  A.NR_VRS_OPR_ETRD

           END-EXEC.

      *--> Cursor 02
      *    Listar as Operações Estruradas de Maior Versão (Versão Atual)
           EXEC SQL
             DECLARE CURSOR-PSQ-2 CURSOR  FOR

             WITH
                 TABMAIORVERSAO (CD_CTRA_TIT_VL, CD_OPR_ETRD_MCP,
                                 MAIORVERSAO )
             AS  (SELECT
                    CD_CTRA_TIT_VL,
		               	  CD_OPR_ETRD_MCP,
			                 MAX(NR_VRS_OPR_ETRD) AS MAIORVERSAO
		                FROM
		                  DB2SGT.OPR_ETRD_MCP
	                 WHERE
                     CD_CTRA_TIT_VL > :SGTSL005-CD-ULT-CTRA-TIT-VL
                     OR  ( CD_CTRA_TIT_VL = :SGTSL005-CD-ULT-CTRA-TIT-VL
                        AND CD_OPR_ETRD_MCP > :SGTSL005-CD-ULT-OPR-ETRD)
		           GROUP BY
               CD_CTRA_TIT_VL,
			            CD_OPR_ETRD_MCP
		           ORDER BY
               CD_CTRA_TIT_VL,
			            CD_OPR_ETRD_MCP)

              SELECT
                A.CD_CTRA_TIT_VL,
		              A.CD_OPR_ETRD_MCP,
		              A.NR_VRS_OPR_ETRD,
		              A.CD_TIT_OPR_ETRD,
		              A.NR_DVS_TIT,
		              A.CD_PRD,
		              A.CD_MDLD,
		              A.QT_PAPL_TIT_EMS,
		              A.VL_PU_EMS_TIT_OPR,
		              A.DT_EMS_TIT_OPR,
		              A.QT_PAPL_TIT_CPR,
		              A.VL_CPR_OPR,
		              A.VL_PU_CPR_TIT_OPR,
		              A.DT_CPR_OPR,
		              A.DT_VNCT_OPR,
		              A.CD_UOR_DTTR_OPR,
		              C.CD_CLI,
		              A.CD_USU_ULT_ATL,
                A.TS_ULT_ATL,
                A.DT_CMPS_FUT_NOTA,
                A.DT_EVT_VCLD_VRS,
                A.VL_EVT_VCLD_VRS,
                A.CD_TIP_VRS_OPR,
                A.DT_INC_VGC_VRS_OPR,
                A.TX_OBS_EVT
	            FROM
               DB2SGT.OPR_ETRD_MCP A
	            INNER JOIN
	              DB2SGT.CLI_OPR_ETRD C
               ON
                      A.CD_CTRA_TIT_VL  = C.CD_CTRA_TIT_VL
                  AND A.CD_OPR_ETRD_MCP = C.CD_OPR_ETRD_MCP
                  AND A.NR_VRS_OPR_ETRD = C.NR_VRS_OPR_ETRD
             INNER JOIN
        		     TABMAIORVERSAO B
	            ON
                    A.CD_CTRA_TIT_VL  = B.CD_CTRA_TIT_VL
		              AND A.CD_OPR_ETRD_MCP = B.CD_OPR_ETRD_MCP
		              AND A.NR_VRS_OPR_ETRD = B.MAIORVERSAO

             WHERE
                   C.CD_TIP_FUC_CLI = 1

             AND (A.CD_CTRA_TIT_VL > :SGTSL005-CD-ULT-CTRA-TIT-VL
             OR  (A.CD_CTRA_TIT_VL = :SGTSL005-CD-ULT-CTRA-TIT-VL
                   AND A.CD_OPR_ETRD_MCP > :SGTSL005-CD-ULT-OPR-ETRD)
             OR   (A.CD_CTRA_TIT_VL = :SGTSL005-CD-ULT-CTRA-TIT-VL
                    AND A.CD_OPR_ETRD_MCP = :SGTSL005-CD-ULT-OPR-ETRD
                    AND A.NR_VRS_OPR_ETRD >
                                      :SGTSL005-NR-ULT-VRS-OPR-ETRD))

              AND (A.CD_CTRA_TIT_VL >= :CD-CTRA-TIT-VL-INI
                   AND  A.CD_CTRA_TIT_VL <= :CD-CTRA-TIT-VL-FIM)

               AND (A.CD_OPR_ETRD_MCP >= :CD-OPR-ETRD-INI
                 AND A.CD_OPR_ETRD_MCP <= :CD-OPR-ETRD-FIM)

               AND (A.CD_PRD >= :CD-PRD-INI
                    AND A.CD_PRD <= :CD-PRD-FIM)

               AND (A.CD_MDLD >= :CD-MDLD-INI
                    AND A.CD_MDLD <= :CD-MDLD-FIM)

               AND (A.CD_UOR_DTTR_OPR >= :CD-UOR-DTTR-OPR-INI
                    AND A.CD_UOR_DTTR_OPR <= :CD-UOR-DTTR-OPR-FIM)

               AND UPPER(COALESCE(A.CD_TIT_OPR_ETRD, ' ')) LIKE
                                UPPER(:SGTSL005-E-CD-TIT-OPR-ETRD)

               AND  (C.CD_CLI >= :CD-CLI-INI
                     AND C.CD_CLI <= :CD-CLI-FIM)

              ORDER BY
               A.CD_CTRA_TIT_VL,
	         A.CD_OPR_ETRD_MCP,
               A.NR_VRS_OPR_ETRD

           END-EXEC.


      *--> Cursor 03
      *    Listar as Operações Estruradas de Maior Versão Vigente.
           EXEC SQL
             DECLARE CURSOR-PSQ-3 CURSOR  FOR

             WITH
                    TAB_MAIOR_VERSAO_APROVADA (CD_CTRA_TIT_VL,
                                               CD_OPR_ETRD_MCP,
                                               MAIOR_VERSAO,
                                               MAIOR_TS)
                      AS  (SELECT  CD_CTRA_TIT_VL,
                                   CD_OPR_ETRD_MCP,
                                  MAX(NR_VRS_OPR_ETRD) AS MAIOR_VERSAO,
                                  MAX(TS_EST_VRS)      AS MAIOR_TS
                           FROM
                              DB2SGT.EST_VRS_OPR_ETRD
                           WHERE
                                  CD_CTRA_TIT_VL > 0
                              AND CD_TIP_EST_VRS = 3
		              GROUP BY
                            CD_CTRA_TIT_VL,
			                 CD_OPR_ETRD_MCP
                     ORDER BY
                            CD_CTRA_TIT_VL,
                            CD_OPR_ETRD_MCP),

                     TAB_CLI_TIP_01 (CD_CTRA_TIT_VL,
                                     CD_OPR_ETRD_MCP,
                                     NR_VRS_OPR_ETRD,
                                     CD_CLI)

                      AS  (SELECT CD_CTRA_TIT_VL,
                                  CD_OPR_ETRD_MCP,
                                  NR_VRS_OPR_ETRD,
                                  CD_CLI
                           FROM
                              DB2SGT.CLI_OPR_ETRD
                           WHERE
                                  CD_CTRA_TIT_VL > 0
                              AND CD_TIP_FUC_CLI = 1
                           ORDER BY
                              CD_CTRA_TIT_VL,
                              CD_OPR_ETRD_MCP,
                              NR_VRS_OPR_ETRD),

                    TAB_MAIOR_EST_OPERACAO (CD_CTRA_TIT_VL,
                                            CD_OPR_ETRD_MCP,
                                            MAIOR_VRS_OPR,
                                            MAIOR_EST_OPR,
                                            MAIOR_TS_OPR)
                    AS  (SELECT CD_CTRA_TIT_VL,
                                CD_OPR_ETRD_MCP,
                                MAX (NR_VRS_OPR_ETRD) AS MAIOR_VRS_OPR,
                                MAX (CD_TIP_EST_OPR)  AS MAIOR_EST_OPR,
                                MAX (TS_EST_OPR_ETRD) AS MAIOR_TS_OPR
                         FROM
                            DB2SGT.EST_OPR_ETRD
                         WHERE
                            CD_CTRA_TIT_VL > 0
                         GROUP BY
                            CD_CTRA_TIT_VL,
                            CD_OPR_ETRD_MCP,
                            NR_VRS_OPR_ETRD
                         ORDER BY
                            CD_CTRA_TIT_VL,
                            CD_OPR_ETRD_MCP,
                            NR_VRS_OPR_ETRD)

              SELECT
                     A.CD_CTRA_TIT_VL,
		              A.CD_OPR_ETRD_MCP,
		              A.NR_VRS_OPR_ETRD,
		              A.CD_TIT_OPR_ETRD,
		              A.NR_DVS_TIT,
		              A.CD_PRD,
		              A.CD_MDLD,
		              A.QT_PAPL_TIT_EMS,
		              A.VL_PU_EMS_TIT_OPR,
		              A.DT_EMS_TIT_OPR,
		              A.QT_PAPL_TIT_CPR,
		              A.VL_CPR_OPR,
		              A.VL_PU_CPR_TIT_OPR,
		              A.DT_CPR_OPR,
		              A.DT_VNCT_OPR,
		              A.CD_UOR_DTTR_OPR,
		              C.CD_CLI,
		              A.CD_USU_ULT_ATL,
                     A.TS_ULT_ATL,
                     A.DT_CMPS_FUT_NOTA,
                     A.DT_EVT_VCLD_VRS,
                     A.VL_EVT_VCLD_VRS,
                     A.CD_TIP_VRS_OPR,
                     A.DT_INC_VGC_VRS_OPR,
                     A.TX_OBS_EVT

	            FROM
                  DB2SGT.OPR_ETRD_MCP A

                  INNER JOIN
                        TAB_MAIOR_VERSAO_APROVADA B
                  ON
                       A.CD_CTRA_TIT_VL  = B.CD_CTRA_TIT_VL
                  AND  A.CD_OPR_ETRD_MCP = B.CD_OPR_ETRD_MCP
                  AND  A.NR_VRS_OPR_ETRD = MAIOR_VERSAO

	            INNER JOIN
	                  TAB_CLI_TIP_01 C
                  ON
                      A.CD_CTRA_TIT_VL  = C.CD_CTRA_TIT_VL
                  AND A.CD_OPR_ETRD_MCP = C.CD_OPR_ETRD_MCP
                  AND A.NR_VRS_OPR_ETRD = C.NR_VRS_OPR_ETRD

                  INNER JOIN
                        TAB_MAIOR_EST_OPERACAO D
                  ON
                      A.CD_CTRA_TIT_VL  = D.CD_CTRA_TIT_VL
                  AND A.CD_OPR_ETRD_MCP = D.CD_OPR_ETRD_MCP
                  AND A.NR_VRS_OPR_ETRD = MAIOR_VRS_OPR

             WHERE
                 MAIOR_EST_OPR  =  5
              AND
                 (A.CD_CTRA_TIT_VL > :SGTSL005-CD-ULT-CTRA-TIT-VL
             OR  (A.CD_CTRA_TIT_VL = :SGTSL005-CD-ULT-CTRA-TIT-VL
                   AND A.CD_OPR_ETRD_MCP > :SGTSL005-CD-ULT-OPR-ETRD)
              OR   (A.CD_CTRA_TIT_VL = :SGTSL005-CD-ULT-CTRA-TIT-VL
                     AND A.CD_OPR_ETRD_MCP = :SGTSL005-CD-ULT-OPR-ETRD
                     AND A.NR_VRS_OPR_ETRD >
                                       :SGTSL005-NR-ULT-VRS-OPR-ETRD))

              AND (A.CD_CTRA_TIT_VL >= :CD-CTRA-TIT-VL-INI
                   AND  A.CD_CTRA_TIT_VL <= :CD-CTRA-TIT-VL-FIM)

               AND (A.CD_OPR_ETRD_MCP >= :CD-OPR-ETRD-INI
                 AND A.CD_OPR_ETRD_MCP <= :CD-OPR-ETRD-FIM)

               AND (A.CD_PRD >= :CD-PRD-INI
                    AND A.CD_PRD <= :CD-PRD-FIM)

               AND (A.CD_MDLD >= :CD-MDLD-INI
                    AND A.CD_MDLD <= :CD-MDLD-FIM)

               AND (A.CD_UOR_DTTR_OPR >= :CD-UOR-DTTR-OPR-INI
                    AND A.CD_UOR_DTTR_OPR <= :CD-UOR-DTTR-OPR-FIM)

               AND  (C.CD_CLI >= :CD-CLI-INI
                     AND C.CD_CLI <= :CD-CLI-FIM)

               AND UPPER(COALESCE(A.CD_TIT_OPR_ETRD, ' ')) LIKE
                                UPPER(:SGTSL005-E-CD-TIT-OPR-ETRD)

              ORDER BY
               A.CD_CTRA_TIT_VL,
	              A.CD_OPR_ETRD_MCP,
               A.NR_VRS_OPR_ETRD

           END-EXEC.

      *--> Parâmetros para a utilizacao do DB2
           EXEC SQL
                     INCLUDE SQLCA
           END-EXEC.

       77  CTE-FINAL-SS               PIC X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.
      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997

      *--> Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 110000-EXCLUIR-CTNR-RPST
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDAR-PARAMETRO

           EVALUATE TRUE
           WHEN SGTSL005-LISTA-TODAS-VERSOES
              PERFORM 970101-ABRE-CURSOR-PSQ-1
              PERFORM 970102-LER-CURSOR-PSQ-1
              PERFORM 200000-PROCESSA-PESQUISA UNTIL  FIM-CURSOR-PSQ
                       OR SGTSL005-PREENCHIDO-TAB
              IF  NOT FIM-CURSOR-PSQ
                  MOVE 'S'                TO SGTSL005-IN-CNTO-PSQ
              END-IF
              PERFORM 970103-FECHA-CURSOR-PSQ-1

           WHEN SGTSL005-LISTA-ULTIMA-VERSAO
              PERFORM 970104-ABRE-CURSOR-PSQ-2
              PERFORM 970105-LER-CURSOR-PSQ-2
              PERFORM 200000-PROCESSA-PESQUISA UNTIL FIM-CURSOR-PSQ
                       OR SGTSL005-PREENCHIDO-TAB
              IF  NOT FIM-CURSOR-PSQ
                  MOVE 'S'                TO SGTSL005-IN-CNTO-PSQ
              END-IF
              PERFORM 970106-FECHA-CURSOR-PSQ-2

           WHEN SGTSL005-LISTA-VERSAO-VIGENTE
              PERFORM 970107-ABRE-CURSOR-PSQ-3
              PERFORM 970108-LER-CURSOR-PSQ-3
              PERFORM 200000-PROCESSA-PESQUISA UNTIL FIM-CURSOR-PSQ
                       OR SGTSL005-PREENCHIDO-TAB
              IF  NOT FIM-CURSOR-PSQ
                  MOVE 'S'                TO SGTSL005-IN-CNTO-PSQ
              END-IF
              PERFORM 970109-FECHA-CURSOR-PSQ-3
           END-EVALUATE

           PERFORM 300000-POSTA-RESPOSTA

           GOBACK
           .

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSL005-RPST' )
              CHANNEL   ( NM-CNL          )
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
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL005-RQSC     TO QT-CCT-EPRO
           EXEC CICS GET
              CONTAINER ( 'SGTSL005-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA         TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSL005-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL005-RQSC )
              INTO      ( SGTSL005-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO SGTSL005-IN-CNTO-PSQ

           IF  NOT SGTSL005-LISTA-TODAS-VERSOES  AND
               NOT SGTSL005-LISTA-ULTIMA-VERSAO  AND
               NOT SGTSL005-LISTA-VERSAO-VIGENTE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0110              TO  MSG1-CD-ERRO-TS
               MOVE  'Tipo de consulta inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL005-CD-ULT-CTRA-TIT-VL   < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL005-CD-ULT-OPR-ETRD    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0111              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da última operação estruturada não numérico
      -              '.'               TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL005-E-CD-OPR-ETRD-MCP  NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada mercado de capitais
      -              ' não numérico.'  TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF


           IF SGTSL005-E-CD-CTRA-TIT-VL > ZEROS
               MOVE  SGTSL005-E-CD-CTRA-TIT-VL
                                       TO  CD-CTRA-TIT-VL-INI
                                           CD-CTRA-TIT-VL-FIM
           END-IF

           IF  SGTSL005-E-CD-OPR-ETRD-MCP  > ZEROS
               MOVE  SGTSL005-E-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-INI
                                           CD-OPR-ETRD-FIM
           END-IF

           IF  SGTSL005-E-CD-PRD           >  ZEROS
               MOVE  SGTSL005-E-CD-PRD
                                       TO  CD-PRD-INI
                                           CD-PRD-FIM
           END-IF

           IF  SGTSL005-E-CD-MDLD          >  ZEROS
               MOVE  SGTSL005-E-CD-MDLD
                                       TO  CD-MDLD-INI
                                           CD-MDLD-FIM
           END-IF

           IF  SGTSL005-E-CD-CLI-EMT       > ZEROS
               MOVE  SGTSL005-E-CD-CLI-EMT
                                       TO  CD-CLI-INI
                                           CD-CLI-FIM
           END-IF

           IF  SGTSL005-E-CD-UOR-DTTR-OPR  > ZEROS
               MOVE  SGTSL005-E-CD-UOR-DTTR-OPR
                                       TO  CD-UOR-DTTR-OPR-INI
                                           CD-UOR-DTTR-OPR-FIM
           END-IF

           INSPECT SGTSL005-E-CD-TIT-OPR-ETRD
                             REPLACING ALL SPACES BY '%'
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           ADD  1   TO SGTSL005-QT-OCR-TAB
              ON SIZE ERROR
              SET  ERRO-PROGRAMADO     TO TRUE
              MOVE 0113                TO MSG1-CD-ERRO-TS
              MOVE 'Erro no cálculo da  quantidade de ocorrências.'
                                       TO MSG1-TX-ERRO-TS
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-ADD

           ADD  1   TO   SGTSL005-IC-TAB
              ON SIZE ERROR
              SET  ERRO-PROGRAMADO     TO TRUE
              MOVE 0114                TO MSG1-CD-ERRO-TS
              MOVE 'Erro no cálculo do contador de selecionados.'
                                       TO MSG1-TX-ERRO-TS
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-ADD

           IF  SGTSL005-ESTOURO-TAB
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0115               TO MSG1-CD-ERRO-TS
               MOVE 'Tabela da comunicação estourou.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE CD-CTRA-TIT-VL     TO
                             SGTSL005-CD-CTRA-TIT-VL(SGTSL005-IC-TAB)
           MOVE CD-OPR-ETRD-MCP    TO
                             SGTSL005-CD-OPR-ETRD-MCP(SGTSL005-IC-TAB)

           MOVE CD-TIT-OPR-ETRD    TO
                             SGTSL005-CD-TIT-OPR-ETRD(SGTSL005-IC-TAB)
           MOVE CD-PRD             TO
                             SGTSL005-CD-PRD(SGTSL005-IC-TAB)
           MOVE CD-MDLD            TO
                             SGTSL005-CD-MDLD(SGTSL005-IC-TAB)
           MOVE DT-EMS-TIT-OPR     TO
                             SGTSL005-DT-EMS-TIT-OPR(SGTSL005-IC-TAB)
           MOVE VL-CPR-OPR         TO
                             SGTSL005-VL-CPR-OPR(SGTSL005-IC-TAB)
           MOVE CD-UOR-DTTR-OPR    TO
                             SGTSL005-CD-UOR-DTTR-OPR(SGTSL005-IC-TAB)
           MOVE CD-CLI             TO
                             SGTSL005-CD-CLI-EMT(SGTSL005-IC-TAB)
           MOVE DT-CPR-OPR         TO
                             SGTSL005-DT-CPR-OPR(SGTSL005-IC-TAB)
           MOVE DT-VNCT-OPR        TO
                             SGTSL005-DT-VNCT-OPR(SGTSL005-IC-TAB)
           MOVE NR-DVS-TIT         TO
                             SGTSL005-NR-DVS-TIT(SGTSL005-IC-TAB)
           MOVE NR-VRS-OPR-ETRD    TO
                             SGTSL005-NR-VRS-OPR-ETRD(SGTSL005-IC-TAB)
           MOVE QT-PAPL-TIT-CPR    TO
                             SGTSL005-QT-PAPL-TIT-CPR(SGTSL005-IC-TAB)
           MOVE QT-PAPL-TIT-EMS    TO
                             SGTSL005-QT-PAPL-TIT-EMS(SGTSL005-IC-TAB)
           MOVE VL-PU-CPR-TIT-OPR  TO
                             SGTSL005-VL-PU-CPR-TIT-OPR(SGTSL005-IC-TAB)
           MOVE VL-PU-EMS-TIT-OPR  TO
                             SGTSL005-VL-PU-EMS-TIT-OPR(SGTSL005-IC-TAB)
           MOVE CD-USU-ULT-ATL     TO
                             SGTSL005-CD-USU-ULT-ATL(SGTSL005-IC-TAB)
           MOVE TS-ULT-ATL         TO
                             SGTSL005-TS-ULT-ATL(SGTSL005-IC-TAB)
           MOVE DT-CMPS-FUT-NOTA    TO
                             SGTSL005-DT-CMPS-FUT-NOTA(SGTSL005-IC-TAB)
           MOVE DT-EVT-VCLD-VRS     TO
                             SGTSL005-DT-EVT-VCLD-VRS(SGTSL005-IC-TAB)
           MOVE VL-EVT-VCLD-VRS     TO
                             SGTSL005-VL-EVT-VCLD-VRS(SGTSL005-IC-TAB)
           MOVE CD-TIP-VRS-OPR      TO
                             SGTSL005-CD-TIP-VRS-OPR(SGTSL005-IC-TAB)
           MOVE DT-INC-VGC-VRS-OPR  TO
                           SGTSL005-DT-INC-VGC-VRS-OPR(SGTSL005-IC-TAB)
           MOVE TX-OBS-EVT          TO
                           SGTSL005-TX-OBS-EVT(SGTSL005-IC-TAB)

           EVALUATE TRUE
              WHEN SGTSL005-LISTA-TODAS-VERSOES
                 PERFORM 970102-LER-CURSOR-PSQ-1
              WHEN SGTSL005-LISTA-ULTIMA-VERSAO
                 PERFORM 970105-LER-CURSOR-PSQ-2
              WHEN SGTSL005-LISTA-VERSAO-VIGENTE
                 PERFORM 970108-LER-CURSOR-PSQ-3
           END-EVALUATE
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ-1 SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                OPEN CURSOR-PSQ-1
           END-EXEC
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ-1 SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS
           EXEC SQL
                FETCH CURSOR-PSQ-1
                INTO   :CD-CTRA-TIT-VL
                     , :CD-OPR-ETRD-MCP
                     , :NR-VRS-OPR-ETRD
                     , :CD-TIT-OPR-ETRD
                       :CD-TIT-OPR-ETRD-N
                     , :NR-DVS-TIT
                       :NR-DVS-TIT-N
                     , :CD-PRD
                     , :CD-MDLD
                     , :QT-PAPL-TIT-EMS
                     , :VL-PU-EMS-TIT-OPR
                     , :DT-EMS-TIT-OPR
                       :DT-EMS-TIT-OPR-N
                     , :QT-PAPL-TIT-CPR
                     , :VL-CPR-OPR
                     , :VL-PU-CPR-TIT-OPR
                     , :DT-CPR-OPR
                       :DT-CPR-OPR-N
                     , :DT-VNCT-OPR
                       :DT-VNCT-OPR-N
                     , :CD-UOR-DTTR-OPR
                     , :CD-CLI
                     , :CD-USU-ULT-ATL
                     , :TS-ULT-ATL
                     , :DT-CMPS-FUT-NOTA
                       :DT-CMPS-FUT-NOTA-N
                     , :DT-EVT-VCLD-VRS
                       :DT-EVT-VCLD-VRS-N
                     , :VL-EVT-VCLD-VRS
                       :VL-EVT-VCLD-VRS-N
                     , :CD-TIP-VRS-OPR
                     , :DT-INC-VGC-VRS-OPR
                       :DT-INC-VGC-VRS-OPR-N
                     , :TX-OBS-EVT
                       :TX-OBS-EVT-N
           END-EXEC
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ-1 SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
           EXEC SQL
                CLOSE CURSOR-PSQ-1
           END-EXEC
           .
       970103-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970104-ABRE-CURSOR-PSQ-2 SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                OPEN CURSOR-PSQ-2
           END-EXEC
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
           .
       970104-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970105-LER-CURSOR-PSQ-2 SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS
           EXEC SQL
                FETCH CURSOR-PSQ-2
                INTO   :CD-CTRA-TIT-VL
                     , :CD-OPR-ETRD-MCP
                     , :NR-VRS-OPR-ETRD
                     , :CD-TIT-OPR-ETRD
                       :CD-TIT-OPR-ETRD-N
                     , :NR-DVS-TIT
                       :NR-DVS-TIT-N
                     , :CD-PRD
                     , :CD-MDLD
                     , :QT-PAPL-TIT-EMS
                     , :VL-PU-EMS-TIT-OPR
                     , :DT-EMS-TIT-OPR
                       :DT-EMS-TIT-OPR-N
                     , :QT-PAPL-TIT-CPR
                     , :VL-CPR-OPR
                     , :VL-PU-CPR-TIT-OPR
                     , :DT-CPR-OPR
                       :DT-CPR-OPR-N
                     , :DT-VNCT-OPR
                       :DT-VNCT-OPR-N
                     , :CD-UOR-DTTR-OPR
                     , :CD-CLI
                     , :CD-USU-ULT-ATL
                       :CD-USU-ULT-ATL-N
                     , :TS-ULT-ATL
                       :TS-ULT-ATL-N
                     , :DT-CMPS-FUT-NOTA
                       :DT-CMPS-FUT-NOTA-N
                     , :DT-EVT-VCLD-VRS
                       :DT-EVT-VCLD-VRS-N
                     , :VL-EVT-VCLD-VRS
                       :VL-EVT-VCLD-VRS-N
                     , :CD-TIP-VRS-OPR
                     , :DT-INC-VGC-VRS-OPR
                       :DT-INC-VGC-VRS-OPR-N
                     , :TX-OBS-EVT
                       :TX-OBS-EVT-N
           END-EXEC
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970105-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970106-FECHA-CURSOR-PSQ-2 SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
           EXEC SQL
                CLOSE CURSOR-PSQ-2
           END-EXEC
           .
       970106-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970107-ABRE-CURSOR-PSQ-3 SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                OPEN CURSOR-PSQ-3
           END-EXEC
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
           .
       970107-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970108-LER-CURSOR-PSQ-3 SECTION.
      *-----------------------------------------------------------------
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS
           EXEC SQL
                FETCH CURSOR-PSQ-3
                INTO   :CD-CTRA-TIT-VL
                     , :CD-OPR-ETRD-MCP
                     , :NR-VRS-OPR-ETRD
                     , :CD-TIT-OPR-ETRD
                       :CD-TIT-OPR-ETRD-N
                     , :NR-DVS-TIT
                       :NR-DVS-TIT-N
                     , :CD-PRD
                     , :CD-MDLD
                     , :QT-PAPL-TIT-EMS
                     , :VL-PU-EMS-TIT-OPR
                     , :DT-EMS-TIT-OPR
                       :DT-EMS-TIT-OPR-N
                     , :QT-PAPL-TIT-CPR
                     , :VL-CPR-OPR
                     , :VL-PU-CPR-TIT-OPR
                     , :DT-CPR-OPR
                       :DT-CPR-OPR-N
                     , :DT-VNCT-OPR
                       :DT-VNCT-OPR-N
                     , :CD-UOR-DTTR-OPR
                     , :CD-CLI
                     , :CD-USU-ULT-ATL
                       :CD-USU-ULT-ATL-N
                     , :TS-ULT-ATL
                       :TS-ULT-ATL-N
                     , :DT-CMPS-FUT-NOTA
                       :DT-CMPS-FUT-NOTA-N
                     , :DT-EVT-VCLD-VRS
                       :DT-EVT-VCLD-VRS-N
                     , :VL-EVT-VCLD-VRS
                       :VL-EVT-VCLD-VRS-N
                     , :CD-TIP-VRS-OPR
                     , :DT-INC-VGC-VRS-OPR
                       :DT-INC-VGC-VRS-OPR-N
                     , :TX-OBS-EVT
                       :TX-OBS-EVT-N
           END-EXEC
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970108-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970109-FECHA-CURSOR-PSQ-3 SECTION.
      *-----------------------------------------------------------------
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
           EXEC SQL
                CLOSE CURSOR-PSQ-3
           END-EXEC
           .
       970109-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
              CONTAINER ( 'SGTSL005-RPST'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL005-RPST )
              FROM      ( SGTSL005-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------
           IF  IN-CURSOR-PSQ-ABTO  EQUAL  'S'
               EVALUATE TRUE
                  WHEN SGTSL005-LISTA-TODAS-VERSOES
                       PERFORM 970103-FECHA-CURSOR-PSQ-1
                  WHEN SGTSL005-LISTA-ULTIMA-VERSAO
                       PERFORM 970106-FECHA-CURSOR-PSQ-2
                  WHEN SGTSL005-LISTA-VERSAO-VIGENTE
                       PERFORM 970109-FECHA-CURSOR-PSQ-3
               END-EVALUATE
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

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                        TO MSG1-TX-ERRO-TS
           END-IF

           MOVE 'SGTSL005-IN-PSQ-ULT-VRS'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL005-IN-PSQ-ULT-VRS         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL005-CD-ULT-CTRA-TIT-VL'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL005-CD-ULT-CTRA-TIT-VL    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL005-CD-ULT-OPR-ETRD'      TO MSGKW999-NM-VRV-MNT
           IF   SGTSL005-CD-ULT-OPR-ETRD        IS NUMERIC
                MOVE SGTSL005-CD-ULT-OPR-ETRD   TO CD-OPR-ETRD-MCP-D
                MOVE CD-OPR-ETRD-MCP-D          TO MSGKW999-VL-VRV-MNT
           ELSE
                MOVE 'Código da última operação estruturada não numérico
      -              '.'                        TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL005-NR-ULT-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL005-NR-ULT-VRS-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL005-E-CD-CTRA-TIT-VL'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL005-E-CD-CTRA-TIT-VL    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL005-E-CD-OPR-ETRD-MCP'    TO MSGKW999-NM-VRV-MNT
           IF   SGTSL005-E-CD-OPR-ETRD-MCP      IS NUMERIC
                MOVE SGTSL005-E-CD-OPR-ETRD-MCP TO CD-OPR-ETRD-MCP-D
                MOVE CD-OPR-ETRD-MCP-D          TO MSGKW999-VL-VRV-MNT
           ELSE
                MOVE 'Código da operação estruturada de mercado de capit
      -              'ais não numérico.'        TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL005-E-CD-PRD'             TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL005-E-CD-PRD               TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL005-E-CD-MDLD'            TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL005-E-CD-MDLD              TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL005-E-CD-TIT-OPR-ETRD'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL005-E-CD-TIT-OPR-ETRD      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL005-E-CD-UOR-DTTR-OPR'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL005-E-CD-UOR-DTTR-OPR      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL005-E-CD-CLI-EMT'         TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL005-E-CD-CLI-EMT           TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL005-TIPO-CONSULTA'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL005-TIPO-CONSULTA          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO

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
      * --------------------------------------*
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