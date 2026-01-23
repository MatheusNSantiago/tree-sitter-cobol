      *-----------------------------------------------------------------
      * Programa..: SGTSL500
      * Analista..: F2054109 - Cintia Ap Soares Iglesias
      * Autor.....: F6361711 - Luís A. Paolillo Valamede
      * Data......: 15/03/2017
      * Objetivo..: Listar informações do Cliente Emissor e
      *             do Agente Fiduciario das Operações conforme
      *             os campos de entrada recebidos
      *             DB2SGT.OPR_ETRD_MCP    DB2SGT.TIT_OPR_ETRD_MCP
      *             DB2SGT.EST_OPR_ETRD    DB2SGT.CLI_OPR_ETRD
      * Compilação: Cobol/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 13/03/2017 - F6361711 - DE 2016/145195 - Implantação
      * VRS002 10/06/2019 - F8650584 - DE 2016/145195
      *                                Inclusão da Funcão UPPER
      *                                na variavel CD-TIT-PSQ do
      *                                Cursor de Pesquisa
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSL500.

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
       77  CTE-PROG                    PIC  X(18) VALUE
                                       '*** SGTSL500 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS002'.

      *--> Código do Tipo de Funcao do Cliente:
      *    0001 - Cliente Emissor
       77  K-CD-TIP-CLI-EMT            PIC  S9(04) COMP-5 VALUE 0001.
      *    0004 - Cliente Agente Fiduciario
       77  K-CD-TIP-CLI-AGT-FDAR       PIC  S9(04) COMP-5 VALUE 0004.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informações
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  IC-TAB                      PIC S9(04) COMP-5 VALUE ZEROS.
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE ZEROS.

       01  IN-PC-CCT                   PIC S9(04) COMP-5 VALUE ZEROS.

       01  CD-TIT-PSQ                  PIC  X(12) VALUE SPACES.
       01  CD-CLI-EMT-INC              PIC S9(09) COMP-5 VALUE ZEROS.
       01  CD-CLI-EMT-FIM              PIC S9(09) COMP-5 VALUE ZEROS.
       01  CD-CLI-AGT-FDAR-INC         PIC S9(09) COMP-5 VALUE ZEROS.
       01  CD-CLI-AGT-FDAR-FIM         PIC S9(09) COMP-5 VALUE ZEROS.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-TX.
           03  CD-OPR-ETRD             PIC  9(17).

      *--> Area Auxiliar para tabela DB2SGT.OPR_ETRD_MCP
       01  OPR-ETRD-MCP.
           03 CD-CTRA-TVM              PIC S9(09) COMP-5
                                       OCCURS 101 TIMES  VALUE ZEROS.
           03 CD-OPR-ETRD-MCP          PIC S9(11) COMP-3
                                       OCCURS 101 TIMES  VALUE ZEROS.
           03 NR-DVS-TIT-OPR           PIC  S9(04) COMP-5
                                       OCCURS 101 TIMES  VALUE ZEROS.
           03 NR-VRS-OPR-ETRD          PIC S9(04) COMP-5
                                       OCCURS 101 TIMES  VALUE ZEROS.
           03 CD-USU-ULT-ATL           PIC  X(08)
                                       OCCURS 101 TIMES  VALUE SPACES.
           03 TS-ULT-ATL               PIC  X(26)
                                       OCCURS 101 TIMES  VALUE SPACES.
           03 CD-PRD                   PIC S9(04) COMP-5
                                       OCCURS 101 TIMES  VALUE ZEROS.
           03 CD-MDLD                  PIC S9(04) COMP-5
                                       OCCURS 101 TIMES  VALUE ZEROS.

      *--> Area Auxiliar para tabela DB2SGT.TIT_OPR_ETRD_MCP
       01  TIT-OPR-ETRD-MCP.
           03 CD-TIT-OPR-ETRD          PIC  X(12)
                                       OCCURS 101 TIMES  VALUE SPACES.
           03 NR-DVS-TIT               PIC  S9(04) COMP-5
                                       OCCURS 101 TIMES  VALUE ZEROS.
           03 NM-TIT-OPR-ETRD          PIC  X(50)
                                       OCCURS 101 TIMES  VALUE SPACES.

      *--> Area Auxiliar para tabela DB2SGT.EST_OPR_ETRD
       01  EST-OPR-ETRD.
           03 CD-TIP-EST-OPR           PIC S9(09) COMP-5
                                       OCCURS 101 TIMES  VALUE ZEROS.
           03 CD-TIP-EST-OPR-N         PIC S9(04) COMP-5
                                       OCCURS 101 TIMES  VALUE ZEROS.

           03 TS-EST-OPR-ETRD          PIC  X(26)
                                       OCCURS 101 TIMES  VALUE SPACES.

      *--> Area Auxiliar para tabela DB2SGT.CLI_OPR_ETRD
       01  CLI-OPR-ETRD.
           03 CD-EMT                   PIC S9(09) COMP-5
                                       OCCURS 101 TIMES  VALUE ZEROS.
           03 CD-AGT-FDAR              PIC S9(09) COMP-5
                                       OCCURS 101 TIMES  VALUE ZEROS.

      *--> Variável(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE ZEROS.
               88  FIM-CURSOR-PSQ                 VALUE  +100.
           03  IN-CURSOR-PSQ-ABTO-01   PIC  X(01) VALUE  'N'.
               88  CURSOR-PSQ-ABTO-01             VALUE  'S'.

      *--> Campos utilizados para manipulação de container
       01  GRP-CONTAINER.
           03  CANAL                   PIC  X(16) VALUE SPACES.
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
       01  SGTSL500-DADOS.
-INC SGTKL500


      *--> Cursor de Pesquisa
      *    Listar o Cliente Emissor e o Agente Fiduciário
      *    - Filtro Codigo do Titulo
      *      CD-TIT-PSQ = Codigo do Título
      *    - Filtro Cliente Emissor
      *      CD-CLI = Codigo MCI do Cliente Emissor
      *      CD-TIP-FUC-CLI = 0001 Emissor
      *    - Filtro Agente Fiduciario
      *      CD-CLI = Codigo MCI do Cliente Emissor
      *      CD-TIP-FUC-CLI = 0004 Agente Fiduciario
           EXEC SQL
              DECLARE  CURSOR-PSQ  CURSOR
                       WITH ROWSET POSITIONING FOR
               SELECT  DISTINCT
                       A.CD_CTRA_TIT_VL
                    ,  A.CD_OPR_ETRD_MCP
                    ,  A.NR_VRS_OPR_ETRD
                    ,  A.CD_USU_ULT_ATL
                    ,  A.TS_ULT_ATL
                    ,  A.CD_PRD
                    ,  A.CD_MDLD
                    ,  B.CD_TIT_OPR_ETRD
                    ,  B.NR_DVS_TIT
                    ,  B.NM_TIT_OPR_ETRD
                    ,  C.CD_TIP_EST_OPR
                    ,  C.TS_EST_OPR_ETRD
                    ,  D.CD_CLI
                    ,  E.CD_CLI
                 FROM  DB2SGT.OPR_ETRD_MCP  A
      *------------->  Título da Operacao Estruturada
           INNER JOIN  DB2SGT.TIT_OPR_ETRD_MCP  B
                   ON  A.CD_TIT_OPR_ETRD    =  B.CD_TIT_OPR_ETRD
                  AND  A.NR_DVS_TIT         =  B.NR_DVS_TIT
      *------------->  Estado da Operacao Estruturada
           INNER JOIN  DB2SGT.EST_OPR_ETRD  C
                   ON  A.CD_CTRA_TIT_VL     =  C.CD_CTRA_TIT_VL
                  AND  A.CD_OPR_ETRD_MCP    =  C.CD_OPR_ETRD_MCP
                  AND  A.NR_VRS_OPR_ETRD    =  C.NR_VRS_OPR_ETRD
      *------------->  Cliente Emissor
      *                CD_TIP_FUC_CLI = 0001 Emissor
           INNER JOIN  DB2SGT.CLI_OPR_ETRD  D
                   ON  A.CD_CTRA_TIT_VL     =  D.CD_CTRA_TIT_VL
                  AND  A.CD_OPR_ETRD_MCP    =  D.CD_OPR_ETRD_MCP
                  AND  A.NR_VRS_OPR_ETRD    =  D.NR_VRS_OPR_ETRD
      *------------->  Cliente Agente Fiduciario
      *                CD_TIP_FUC_CLI = 0004 Agente Fiduciario
           INNER JOIN  DB2SGT.CLI_OPR_ETRD  E
                   ON  A.CD_CTRA_TIT_VL     =  E.CD_CTRA_TIT_VL
                  AND  A.CD_OPR_ETRD_MCP    =  E.CD_OPR_ETRD_MCP
                  AND  A.NR_VRS_OPR_ETRD    =  E.NR_VRS_OPR_ETRD
                WHERE
      *------------->  Filtro Codigo do Titulo
      *------------->  Filtro Alterado para não diferenciar
      *                Maiusculas de Minusculas
                       UPPER(COALESCE(B.CD_TIT_OPR_ETRD, ' '))
                                          LIKE UPPER (:CD-TIT-PSQ)
      *                B.CD_TIT_OPR_ETRD  LIKE :CD-TIT-PSQ
                  AND  C.TS_EST_OPR_ETRD    =
      *------------->  Estado da Operacao Estruturada Ativo
                      (SELECT MAX(TS_EST_OPR_ETRD)
                       FROM  DB2SGT.EST_OPR_ETRD  F
                       WHERE  C.CD_CTRA_TIT_VL   =  F.CD_CTRA_TIT_VL
                         AND  C.CD_OPR_ETRD_MCP  =  F.CD_OPR_ETRD_MCP
                         AND  C.NR_VRS_OPR_ETRD  =  F.NR_VRS_OPR_ETRD)
                  AND (D.CD_TIP_FUC_CLI     =  :K-CD-TIP-CLI-EMT
                  AND  E.CD_TIP_FUC_CLI     =  :K-CD-TIP-CLI-AGT-FDAR )
      *------------->  Filtro Cliente Emissor
                  AND (D.CD_CLI        BETWEEN :CD-CLI-EMT-INC
                  AND                          :CD-CLI-EMT-FIM
      *------------->  Filtro Cliente Agente Fiduciario
                  AND  E.CD_CLI        BETWEEN :CD-CLI-AGT-FDAR-INC
                  AND                          :CD-CLI-AGT-FDAR-FIM)
      *------------->  Utilizado para paginacao
                 AND  (A.CD_OPR_ETRD_MCP    > :SGTSL500-CD-OPR-ULT
                  OR  (A.CD_OPR_ETRD_MCP    = :SGTSL500-CD-OPR-ULT
                 AND   A.NR_VRS_OPR_ETRD    > :SGTSL500-NR-VRS-ULT))
             ORDER BY  A.CD_OPR_ETRD_MCP, A.NR_VRS_OPR_ETRD
           END-EXEC.

      *--> Parâmetro para a utilização do DB2
           EXEC SQL
                     INCLUDE SQLCA
           END-EXEC.


       01  CTE-FINAL-SS                PIC  X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO


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

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-OBTER-CANAL              SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  ASSIGN
                      CHANNEL ( CANAL                                  )
           END-EXEC
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'SGTSL500-RPST'                      )
                      CHANNEL   ( CANAL                                )
                      NOHANDLE
           END-EXEC
           IF  NOT (    EIBRESP  EQUAL  DFHRESP(NORMAL      )
                    OR  EIBRESP  EQUAL  DFHRESP(CONTAINERERR) )
               PERFORM  999002-TRATA-CONDITION-CICS
           END-IF
           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'PRM-ERRO'                           )
                      CHANNEL   ( CANAL                                )
                      NOHANDLE
           END-EXEC
           IF  NOT (    EIBRESP  EQUAL  DFHRESP(NORMAL      )
                    OR  EIBRESP  EQUAL  DFHRESP(CONTAINERERR) )
               PERFORM  999002-TRATA-CONDITION-CICS
           END-IF
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-CCT-LIDO
           MOVE  LENGTH OF  SGTSL500-RQSC
                                       TO  QT-CCT-EPRO
           EXEC CICS  GET
                      CONTAINER ( 'SGTSL500-RQSC'                      )
                      CHANNEL   ( CANAL                                )
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
                      CONTAINER ( 'SGTSL500-RQSC'                      )
                      CHANNEL   ( CANAL                                )
                      FLENGTH   ( LENGTH OF SGTSL500-RQSC              )
                      INTO      ( SGTSL500-RQSC                        )
           END-EXEC
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSL500-CD-OPR-ULT         NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL500-CD-OPR-ULT         NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL500-NR-VRS-ULT         NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL500-CD-EMT-PSQ         NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0028              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do cliente emissor inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL500-CD-AGT-FDAR-PSQ    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0032              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do cliente agente fiduciário inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       150000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR-PESQUISA       SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  210000-PREPARAR-FILTROS

           PERFORM  971100-ABRE-CURSOR-PSQ

           PERFORM  971200-LER-CURSOR-PSQ

           PERFORM  971300-FECHA-CURSOR-PSQ

           PERFORM  220000-MONTAR-LISTA
                    VARYING                IC-TAB
                    FROM                   0001 BY 0001
                    UNTIL                  IC-TAB  GREATER
                                           SGTSL500-QT-OCR
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-PREPARAR-FILTROS         SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

      *--> Filtro Codigo do Titulo
           MOVE  SGTSL500-CD-TIT-PSQ   TO  CD-TIT-PSQ

      *--> Substituir todos Espaços por '%'
           INSPECT  CD-TIT-PSQ
                    REPLACING ALL SPACES BY '%'

      *--> Filtro Emissor Titulo
           IF  SGTSL500-CD-EMT-PSQ         EQUAL  ZEROS
               MOVE  ZEROS             TO  CD-CLI-EMT-INC
               MOVE  999999999         TO  CD-CLI-EMT-FIM
           ELSE
               MOVE  SGTSL500-CD-EMT-PSQ
                                       TO  CD-CLI-EMT-INC
               MOVE  SGTSL500-CD-EMT-PSQ
                                       TO  CD-CLI-EMT-FIM
           END-IF

      *--> Filtro Agente Fiduciario
           IF  SGTSL500-CD-AGT-FDAR-PSQ    EQUAL  ZEROS
               MOVE  ZEROS             TO  CD-CLI-AGT-FDAR-INC
               MOVE  999999999         TO  CD-CLI-AGT-FDAR-FIM
           ELSE
               MOVE  SGTSL500-CD-AGT-FDAR-PSQ
                                       TO  CD-CLI-AGT-FDAR-INC
               MOVE  SGTSL500-CD-AGT-FDAR-PSQ
                                       TO  CD-CLI-AGT-FDAR-FIM
           END-IF

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-MONTAR-LISTA             SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           MOVE  CD-CTRA-TVM                     (IC-TAB)
             TO  SGTSL500-CD-CTRA-TVM            (IC-TAB)

           MOVE  CD-OPR-ETRD-MCP                 (IC-TAB)
             TO  SGTSL500-CD-OPR-ETRD            (IC-TAB)

           MOVE  NR-VRS-OPR-ETRD                 (IC-TAB)
             TO  SGTSL500-NR-VRS-OPR             (IC-TAB)

           MOVE  CD-USU-ULT-ATL                  (IC-TAB)
             TO  SGTSL500-CD-USU-ULT-ATL         (IC-TAB)

           MOVE  TS-ULT-ATL                      (IC-TAB)
             TO  SGTSL500-TS-ULT-ATL             (IC-TAB)

           MOVE  CD-PRD                          (IC-TAB)
             TO  SGTSL500-CD-PRD                 (IC-TAB)

           MOVE  CD-MDLD                         (IC-TAB)
             TO  SGTSL500-CD-MDLD                (IC-TAB)

           MOVE  CD-TIT-OPR-ETRD                 (IC-TAB)
             TO  SGTSL500-CD-TIT-OPR-ETRD        (IC-TAB)

           MOVE  NR-DVS-TIT-OPR                  (IC-TAB)
             TO  SGTSL500-NR-DVS-TIT             (IC-TAB)

           MOVE  NM-TIT-OPR-ETRD                 (IC-TAB)
             TO  SGTSL500-NM-TIT-OPR-ETRD        (IC-TAB)

           MOVE  CD-TIP-EST-OPR                  (IC-TAB)
             TO  SGTSL500-CD-TIP-EST-OPR         (IC-TAB)

           MOVE  CD-EMT                          (IC-TAB)
             TO  SGTSL500-CD-EMT                 (IC-TAB)

           MOVE  CD-AGT-FDAR                     (IC-TAB)
             TO  SGTSL500-CD-AGT-FDAR            (IC-TAB)


           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'SGTSL500-RPST'                      )
                      CHANNEL   ( CANAL                                )
                      FLENGTH   ( LENGTH OF SGTSL500-RPST              )
                      FROM      ( SGTSL500-RPST                        )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-------------------------------------------------------------------
       971100-ABRE-CURSOR-PSQ       SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           EXEC SQL
                     OPEN CURSOR-PSQ
           END-EXEC

           MOVE  'S'                   TO  IN-CURSOR-PSQ-ABTO-01
           .
       971100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       971200-LER-CURSOR-PSQ        SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-REG-SELD

           MOVE  SPACES                TO  OPR-ETRD-MCP
                                           TIT-OPR-ETRD-MCP
                                           EST-OPR-ETRD
                                           CLI-OPR-ETRD

           INITIALIZE                      OPR-ETRD-MCP
                                           TIT-OPR-ETRD-MCP
                                           EST-OPR-ETRD
                                           CLI-OPR-ETRD
                                           REPLACING NUMERIC BY ZEROS

           EXEC SQL
                FETCH NEXT ROWSET
                CURSOR-PSQ  FOR 101 ROWS
                INTO  :CD-CTRA-TVM
                    , :CD-OPR-ETRD-MCP
                    , :NR-VRS-OPR-ETRD
                    , :CD-USU-ULT-ATL
                    , :TS-ULT-ATL
                    , :CD-PRD
                    , :CD-MDLD
                    , :CD-TIT-OPR-ETRD
                    , :NR-DVS-TIT-OPR
                    , :NM-TIT-OPR-ETRD
                    , :CD-TIP-EST-OPR
                      :CD-TIP-EST-OPR-N
                    , :TS-EST-OPR-ETRD
                    , :CD-EMT
                    , :CD-AGT-FDAR
                    END-EXEC

           MOVE  SQLERRD(3)            TO  QT-REG-SELD

           IF  QT-REG-SELD             EQUAL  101
               SET   SGTSL500-TEM-MAIS-REGISTRO
                                       TO  TRUE
               MOVE  0100              TO  QT-REG-SELD
           END-IF

           MOVE  QT-REG-SELD           TO  SGTSL500-QT-OCR

           MOVE  SQLCODE               TO  CD-FIM-CURSOR-PSQ
           .
       971200-SAI.
           EXIT.

      *-----------------------------------------------------------------
       971300-FECHA-CURSOR-PSQ      SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'N'                   TO  IN-CURSOR-PSQ-ABTO-01

           EXEC SQL
                     CLOSE CURSOR-PSQ
           END-EXEC
           .
       971300-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           MOVE  'L500-001-CD-OPR-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSL500-CD-OPR-ULT         NUMERIC
               MOVE  SGTSL500-CD-OPR-ULT
                                       TO  CD-OPR-ETRD
               MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> precisa fazer para NR-DVS


           MOVE  'L500-002-NR-VRS-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL500-NR-VRS-ULT   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L500-003-CD-TIT-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL500-CD-TIT-PSQ   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L500-004-CD-EMT-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL500-CD-EMT-PSQ   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L500-005-CD-AGT-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL500-CD-AGT-FDAR-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT


      *--> Grava a Ocorrencia de Erro no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

      *--> Grava a Ocorrencia de Erro no Container de Erro
           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                           )
                      CHANNEL   ( CANAL                                )
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

      *-------------------------- FIM SGTSL500 -------------------------
