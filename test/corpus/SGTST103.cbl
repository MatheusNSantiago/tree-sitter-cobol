      *-----------------------------------------------------------------
      * Programa..: SGTSL07A - versão do programa SGTSL07A
      * Analista..: F2054109 Cintia Ap Soares Iglesias
      * Autores...: F0175702 Adriano Seiji Nakamura
      *             F8650584 Roberto Fernandes Lima
      * Objetivo..: LISTAR RECEBIMENTOS PREVISTOS DA OPERAçãO ESTRUT.
      * Data......: 24/02/2017
      * Compilacao: Cobol/CICS/DB2
      *-----------------------------------------------------------------
      * VRS005 30/12/2019 F8090555 Retira consistencias LISTA-POR-DATA.
      * VRS004 09/05/2019 F8785286 DE 2016/145195 - Inclusão serv-5.
      *          Obs.: não houve registro aqui da VRS0003, pulei para
      *          VRS0004 a partir de informações do SOS 51.
      * VRS002 05/02/2019 F9188847 DE 2016/145195 - Inclusão monitoração
      *          variáveis para rotina SBDATAF
      * VRS001 20/06/2017 F9188847 - DE 2016/145195 - Inclusão do
      *                              cursor 004
      *        24/02/2017 F0175702 DE 2016/145195 - Implantacao
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSL07A.
      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------

       77  CTE-INICIO-SS               PIC  X(40) VALUE
                                       '*** S.S. Comeca Aqui ***'.
       77  CTE-PROG                    PIC  X(18) VALUE
                                       '*** SGTSL07A ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS005'.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE 0.

      *--> Indicadores de nulidades

      *--> Datas Auxiliares
       01  DT-OITO-DD-MM-AA            PIC  X(08) VALUE SPACES.
       01  DT-OITO-DD-MM-AA-R          REDEFINES DT-OITO-DD-MM-AA.
           03  DIA                     PIC  X(02).
           03  MES                     PIC  X(02).
           03  ANO                     PIC  X(04).

       01  DT-RCBT-PRVT-DD-MM-AA       PIC  X(10) VALUE SPACES.
       01  DT-RCBT-PRVT-DD-MM-AA-R     REDEFINES DT-RCBT-PRVT-DD-MM-AA.
           03  DIA                     PIC  X(02).
           03  PONTO-01                PIC  X(01).
           03  MES                     PIC  X(02).
           03  PONTO-02                PIC  X(01).
           03  ANO                     PIC  X(04).

       01  DT-DB2-DD-MM-AA             PIC  X(10) VALUE SPACES.
       01  DT-DB2-DD-MM-AA-R           REDEFINES DT-DB2-DD-MM-AA.
           03  DIA                     PIC  X(02).
           03  FILLER                  PIC  X(01).
           03  MES                     PIC  X(02).
           03  FILLER                  PIC  X(01).
           03  ANO                     PIC  X(04).


      *--> Areas Auxiliares para Tabela DB2SGT.RCBT_PRVT_OPR_ETRD
       01  RCBT-PRVT-OPR-ETRD.
           03  NR-SEQL-RCBT-PRVT       PIC S9(04) COMP-5
                                       OCCURS 0131 TIMES  VALUE ZEROS.
           03  CD-TIP-RCBT-PRVT        PIC S9(04) COMP-5
                                       OCCURS 0131 TIMES  VALUE ZEROS.
           03  DT-RCBT-PRVT            PIC  X(10)
                                       OCCURS 0131 TIMES  VALUE SPACES.
           03  CD-USU                  PIC X(08)
                                       OCCURS 0131 TIMES  VALUE SPACES.
           03  TS-ULT-ATL              PIC  X(26)
                                       OCCURS 0131 TIMES  VALUE SPACES.
           03  PC-RCBT-PRVT            PIC S9(04)V9(4) COMP-3
                                       OCCURS 0131 TIMES  VALUE ZEROS.
           03  PC-RCBT-PRVT-N          PIC S9(04) COMP-5
                                       OCCURS 0131 TIMES  VALUE ZEROS.
           03  VL-RCBT-PRVT            PIC S9(15)V9(2) COMP-3
                                       OCCURS 0131 TIMES  VALUE ZEROS.
           03  VL-RCBT-PRVT-N          PIC S9(04) COMP-5
                                       OCCURS 0131 TIMES  VALUE ZEROS.
           03  TX-OBS-RCBT-PRVT        PIC  X(200)
                                       OCCURS 0131 TIMES  VALUE SPACES.
           03  TX-OBS-RCBT-PRVT-N      PIC S9(04) COMP-5
                                       OCCURS 0131 TIMES  VALUE ZEROS.


      *--> Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE ZEROS.
               88  FIM-CURSOR-PSQ                        VALUE +130.
           03  IN-CURSOR-PSQ-ABTO      PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO                VALUE 'S'.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).



      *--> Area de Parametros da Rotina SBDATAF
       01  WKSBDATA                    PIC  X(4095).
       01  AREA-RED                    REDEFINES WKSBDATA.
           03  AREA-TAM                PIC  9(0008) COMP.
           03  FILLER                  PIC  X(4071).
           03  AREA-FIM                PIC  X(0020).

       01  SBDATAF                     PIC  X(08) VALUE 'SBDATAF'.
       01  FUC                         PIC  X(03) VALUE 'F01'.
       01  ARG01                       PIC  9(08).
       01  ARG01-R                     REDEFINES  ARG01.
           03  DIA                     PIC  9(02).
           03  MES                     PIC  9(02).
           03  ANO                     PIC  9(04).
       01  ARG02                       PIC  9(01) VALUE 0004.

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999

      *--> Book de para acionamento do MSGS0001
-INC MSGKWMG1

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book do container de requisição e resposta deste módulo
      *    DB2SGT.RCBT_PRVT_OPR_ETRD
       01  SGTSL07A-DADOS.
-INC SGTKL07A

      *-----------------------------------------------------------------
      *    Cursores de Pesquisa
      *-----------------------------------------------------------------

      *--> Cursor de Pesquisa 01
      *    Lista Todos os Recebimentos Previstos de uma Operacao
      *    Ordenados por Tipo de Movimentacao e Data de Recebimento
           EXEC SQL
            DECLARE  CURSOR-PSQ-01 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT
                     A.NR_SEQL_RCBT_PRVT
                  ,  A.CD_TIP_RCBT_PRVT
                  ,  A.DT_RCBT_PRVT
                  ,  A.CD_USU
                  ,  A.TS_ULT_ATL
                  ,  IFNULL (A.PC_RCBT_PRVT, 0)
                  ,  IFNULL (A.VL_RCBT_PRVT, 0)
                  ,  IFNULL (A.TX_OBS_RCBT_PRVT, ' ')
               FROM  DB2SGT.RCBT_PRVT_OPR_ETRD  A
              WHERE  A.CD_CTRA_TIT_VL
                                      =  :SGTSL07A-E-CD-CTRA-TIT-VL
                AND  A.CD_OPR_ETRD_MCP
                                      =  :SGTSL07A-E-CD-OPR-ETRD-MCP
                AND  A.NR_VRS_OPR_ETRD
                                      =  :SGTSL07A-E-NR-VRS-OPR-ETRD
                AND  A.NR_SEQL_RCBT_PRVT
                                      =  :SGTSL07A-E-NR-SEQL-RCBT-PRVT
                AND (A.CD_TIP_RCBT_PRVT
                                      >  :SGTSL07A-CD-TIP-RCBT-PRVT-ULT
                 OR (A.CD_TIP_RCBT_PRVT
                                      =  :SGTSL07A-CD-TIP-RCBT-PRVT-ULT
                AND  A.DT_RCBT_PRVT   >  :DT-RCBT-PRVT-DD-MM-AA))
           ORDER BY  A.CD_TIP_RCBT_PRVT,
                     A.DT_RCBT_PRVT
           END-EXEC

      *--> Cursor de Pesquisa 02
      *    Lista os Recebimentos Previstos de um Tipo de Movimentacao
      *    informado
      *    Tipo de Recebimento Previsto
      *    01 - Normal de Juros
      *    02 - Normal de Capital
      *    03 - Extraordinario de Juros
      *    04 - Extraordinario de Capital
           EXEC SQL
            DECLARE  CURSOR-PSQ-02 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT
                     A.NR_SEQL_RCBT_PRVT
                  ,  A.CD_TIP_RCBT_PRVT
                  ,  A.DT_RCBT_PRVT
                  ,  A.CD_USU
                  ,  A.TS_ULT_ATL
                  ,  IFNULL (A.PC_RCBT_PRVT, 0)
                  ,  IFNULL (A.VL_RCBT_PRVT, 0)
                  ,  IFNULL (A.TX_OBS_RCBT_PRVT, ' ')
               FROM  DB2SGT.RCBT_PRVT_OPR_ETRD  A
              WHERE  A.CD_CTRA_TIT_VL
                                      =  :SGTSL07A-E-CD-CTRA-TIT-VL
                AND  A.CD_OPR_ETRD_MCP
                                      =  :SGTSL07A-E-CD-OPR-ETRD-MCP
                AND  A.NR_VRS_OPR_ETRD
                                      =  :SGTSL07A-E-NR-VRS-OPR-ETRD
                AND  A.NR_SEQL_RCBT_PRVT
                                      =  :SGTSL07A-E-NR-SEQL-RCBT-PRVT
                AND  A.CD_TIP_RCBT_PRVT
                                      =  :SGTSL07A-E-CD-TIP-RCBT-PRVT
                AND  A.DT_RCBT_PRVT   >= :DT-RCBT-PRVT-DD-MM-AA
           ORDER BY  A.CD_TIP_RCBT_PRVT,
                     A.DT_RCBT_PRVT
           END-EXEC

      *--> Cursor de Pesquisa 03
      *    Lista todos Recebimentos Previstos referentes ao último
      *    Numero Sequencial do Cronograma (NR_SEQL_RCBT_PRVT)
           EXEC SQL
            DECLARE  CURSOR-PSQ-03 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT
                     A.NR_SEQL_RCBT_PRVT
                  ,  A.CD_TIP_RCBT_PRVT
                  ,  A.DT_RCBT_PRVT
                  ,  A.CD_USU
                  ,  A.TS_ULT_ATL
                  ,  IFNULL (A.PC_RCBT_PRVT, 0)
                  ,  IFNULL (A.VL_RCBT_PRVT, 0)
                  ,  IFNULL (A.TX_OBS_RCBT_PRVT, ' ')
               FROM  DB2SGT.RCBT_PRVT_OPR_ETRD A
              WHERE  A.CD_CTRA_TIT_VL  =  :SGTSL07A-E-CD-CTRA-TIT-VL
                AND  A.CD_OPR_ETRD_MCP =  :SGTSL07A-E-CD-OPR-ETRD-MCP
                AND  A.NR_VRS_OPR_ETRD =  :SGTSL07A-E-NR-VRS-OPR-ETRD
                AND  A.NR_SEQL_RCBT_PRVT IN
                    (SELECT  MAX(NR_SEQL_RCBT_PRVT)
                       FROM  DB2SGT.RCBT_PRVT_OPR_ETRD  B
                      WHERE  B.CD_CTRA_TIT_VL
                                      =  :SGTSL07A-E-CD-CTRA-TIT-VL
                        AND  B.CD_OPR_ETRD_MCP
                                      =  :SGTSL07A-E-CD-OPR-ETRD-MCP
                        AND  B.NR_VRS_OPR_ETRD
                                      =  :SGTSL07A-E-NR-VRS-OPR-ETRD)
                AND (A.CD_TIP_RCBT_PRVT
                                      >  :SGTSL07A-CD-TIP-RCBT-PRVT-ULT
                 OR (A.CD_TIP_RCBT_PRVT
                                      =  :SGTSL07A-CD-TIP-RCBT-PRVT-ULT
                AND  A.DT_RCBT_PRVT
                                      >  :DT-RCBT-PRVT-DD-MM-AA))
           ORDER BY  A.CD_TIP_RCBT_PRVT,
                     A.DT_RCBT_PRVT
           END-EXEC.

      *--> Cursor de Pesquisa 04
      *    Lista Recebimento Previsto por versão
           EXEC SQL
            DECLARE  CURSOR-PSQ-04 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT
                     A.NR_SEQL_RCBT_PRVT
                  ,  A.CD_TIP_RCBT_PRVT
                  ,  A.DT_RCBT_PRVT
                  ,  A.CD_USU
                  ,  A.TS_ULT_ATL
                  ,  IFNULL (A.PC_RCBT_PRVT, 0)
                  ,  IFNULL (A.VL_RCBT_PRVT, 0)
                  ,  IFNULL (A.TX_OBS_RCBT_PRVT, ' ')
               FROM  DB2SGT.RCBT_PRVT_OPR_ETRD  A
              WHERE  A.CD_CTRA_TIT_VL
                                      =  :SGTSL07A-E-CD-CTRA-TIT-VL
                AND  A.CD_OPR_ETRD_MCP
                                      =  :SGTSL07A-E-CD-OPR-ETRD-MCP
                AND  A.NR_VRS_OPR_ETRD
                                      =  :SGTSL07A-E-NR-VRS-OPR-ETRD
                AND  (A.NR_SEQL_RCBT_PRVT
                                      >  :SGTSL07A-NR-SEQL-RCBT-PRVT-ULT
                OR ( A.NR_SEQL_RCBT_PRVT
                                      =  :SGTSL07A-NR-SEQL-RCBT-PRVT-ULT
                AND  A.CD_TIP_RCBT_PRVT
                                      >  :SGTSL07A-CD-TIP-RCBT-PRVT-ULT)
                OR ( A.NR_SEQL_RCBT_PRVT
                                      =  :SGTSL07A-NR-SEQL-RCBT-PRVT-ULT
                AND  A.CD_TIP_RCBT_PRVT
                                      =  :SGTSL07A-CD-TIP-RCBT-PRVT-ULT
                AND  A.DT_RCBT_PRVT   > :DT-RCBT-PRVT-DD-MM-AA))
           ORDER BY  A.NR_SEQL_RCBT_PRVT,
                     A.CD_TIP_RCBT_PRVT,
                     A.DT_RCBT_PRVT
           END-EXEC

      *--> Cursor de Pesquisa 05
      *    Lista todos Recebimentos Previstos referentes ao último
      *    Numero Sequencial do Cronograma (NR_SEQL_RCBT_PRVT)
      *    Classificado por dt_rcbt / tp_rcbt
           EXEC SQL
            DECLARE  CURSOR-PSQ-05 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT
                     A.NR_SEQL_RCBT_PRVT
                  ,  A.CD_TIP_RCBT_PRVT
                  ,  A.DT_RCBT_PRVT
                  ,  A.CD_USU
                  ,  A.TS_ULT_ATL
                  ,  IFNULL (A.PC_RCBT_PRVT, 0)
                  ,  IFNULL (A.VL_RCBT_PRVT, 0)
                  ,  IFNULL (A.TX_OBS_RCBT_PRVT, ' ')
               FROM  DB2SGT.RCBT_PRVT_OPR_ETRD A
              WHERE  A.CD_CTRA_TIT_VL  =  :SGTSL07A-E-CD-CTRA-TIT-VL
                AND  A.CD_OPR_ETRD_MCP =  :SGTSL07A-E-CD-OPR-ETRD-MCP
                AND  A.NR_VRS_OPR_ETRD =  :SGTSL07A-E-NR-VRS-OPR-ETRD
                AND  A.NR_SEQL_RCBT_PRVT IN
                    (SELECT  MAX(NR_SEQL_RCBT_PRVT)
                       FROM  DB2SGT.RCBT_PRVT_OPR_ETRD  B
                      WHERE  B.CD_CTRA_TIT_VL
                                      =  :SGTSL07A-E-CD-CTRA-TIT-VL
                        AND  B.CD_OPR_ETRD_MCP
                                      =  :SGTSL07A-E-CD-OPR-ETRD-MCP
                        AND  B.NR_VRS_OPR_ETRD
                                      =  :SGTSL07A-E-NR-VRS-OPR-ETRD)
                AND  A.CD_TIP_RCBT_PRVT IN (1 , 2)
                AND  A.DT_RCBT_PRVT   >  :DT-RCBT-PRVT-DD-MM-AA
           ORDER BY  A.DT_RCBT_PRVT ,
                     A.CD_TIP_RCBT_PRVT  DESC
           END-EXEC.

      *--> Parâmetros para a utilizacao do DB2
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

      *--> Habilita tratamento automatico de erros DB2
-INC HLPKE997

      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999

           PERFORM  100000-DESCOBRIR-CANAL
           PERFORM  110000-EXCLUIR-CTNR-RPST
           PERFORM  120000-EXCLUIR-CTNR-ERRO
           PERFORM  130000-RECEBER-REQUISICAO

           PERFORM  140000-VALIDAR-PARAMETRO

           PERFORM  200000-PROCESSAR-PESQUISA

           PERFORM  300000-POSTA-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

           SET   ERRO-CANAL            TO   TRUE
           EXEC CICS  ASSIGN
                      CHANNEL ( NM-CNL )
           END-EXEC
           SET   SEM-ERRO              TO TRUE

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'SGTSL07A-RPST'  )
                      CHANNEL   ( NM-CNL           )
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
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'PRM-ERRO' )
                      CHANNEL   ( NM-CNL     )
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
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-CCT-LIDO
           MOVE  LENGTH OF  SGTSL07A-RQSC
                                       TO  QT-CCT-EPRO

           EXEC CICS  GET
                      CONTAINER ( 'SGTSL07A-RQSC'  )
                      CHANNEL   ( NM-CNL           )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO      )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS  GET
                      CONTAINER ( 'SGTSL07A-RQSC'         )
                      CHANNEL   ( NM-CNL                  )
                      FLENGTH   ( LENGTH OF SGTSL07A-RQSC )
                      INTO      ( SGTSL07A-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           IF  NOT SGTSL07A-LISTA-TODOS        AND
               NOT SGTSL07A-LISTA-POR-TIPO     AND
               NOT SGTSL07A-LISTA-ULT-NR-SEQL  AND
               NOT SGTSL07A-LISTA-POR-VERSAO   AND
               NOT SGTSL07A-LISTA-POR-DATA
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0107              TO  MSG1-CD-ERRO-TS
               MOVE  'Código tipo serviço inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL07A-E-CD-CTRA-TIT-VL   EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.' TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL07A-E-CD-CTRA-TIT-VL   NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL07A-E-CD-OPR-ETRD-MCP  NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS

               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL07A-E-CD-OPR-ETRD-MCP  EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'     TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL07A-E-CD-OPR-ETRD-MCP  NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais in
      -              'válido.'         TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL07A-E-NR-VRS-OPR-ETRD  EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL07A-E-NR-VRS-OPR-ETRD  NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número versão operação estruturada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validar Numero Sequencial
           EVALUATE  TRUE
               WHEN  SGTSL07A-LISTA-TODOS
               WHEN  SGTSL07A-LISTA-POR-TIPO
                     IF  SGTSL07A-E-NR-SEQL-RCBT-PRVT
                                           NOT POSITIVE
                         SET   ERRO-PROGRAMADO
                                       TO  TRUE
                         MOVE  0108    TO  MSG1-CD-ERRO-TS
                         MOVE  'Número sequencial do recebimento previst
      -                        'o inválido.'
                                       TO  MSG1-TX-ERRO-TS

                         PERFORM  999000-GRAVA-ERRO-E-ENCERRA
                     END-IF
               WHEN  SGTSL07A-LISTA-ULT-NR-SEQL
      *        WHEN  SGTSL07A-LISTA-POR-DATA
                     IF  SGTSL07A-E-NR-SEQL-RCBT-PRVT
                                           NOT EQUAL  ZEROS
                         SET   ERRO-PROGRAMADO
                                       TO  TRUE
                         MOVE  0108    TO  MSG1-CD-ERRO-TS
                         MOVE  'Número sequencial do recebimento previst
      -                        'o inválido.'
                                       TO  MSG1-TX-ERRO-TS

                         PERFORM  999000-GRAVA-ERRO-E-ENCERRA
                     END-IF
           END-EVALUATE

      *--> Validar Tipo de Recebimento Previsto
      *    01 - Normal de Juros
      *    02 - Normal de Capital
      *    03 - Extraordinario de Juros
      *    04 - Extraordinario de Capital
           EVALUATE  TRUE
               WHEN  SGTSL07A-LISTA-TODOS
               WHEN  SGTSL07A-LISTA-ULT-NR-SEQL
      *        WHEN  SGTSL07A-LISTA-POR-DATA
                     IF  SGTSL07A-E-CD-TIP-RCBT-PRVT
                                           LESS  0000  OR
                         SGTSL07A-E-CD-TIP-RCBT-PRVT
                                           GREATER  0004
                         SET   ERRO-PROGRAMADO
                                       TO  TRUE
                         MOVE  0109    TO  MSG1-CD-ERRO-TS
                         MOVE  'Tipo do recebimento previsto inválido.'
                                       TO  MSG1-TX-ERRO-TS
                         PERFORM  999000-GRAVA-ERRO-E-ENCERRA
                     END-IF

               WHEN  SGTSL07A-LISTA-POR-TIPO
                     IF  SGTSL07A-E-CD-TIP-RCBT-PRVT
                                           LESS  0001  OR
                         SGTSL07A-E-CD-TIP-RCBT-PRVT
                                           GREATER  0004
                         SET   ERRO-PROGRAMADO
                                       TO  TRUE
                         MOVE  0109    TO  MSG1-CD-ERRO-TS
                         MOVE  'Tipo do recebimento previsto inválido.'
                                       TO  MSG1-TX-ERRO-TS
                         PERFORM  999000-GRAVA-ERRO-E-ENCERRA
                     END-IF
           END-EVALUATE

      *--> Validar Data do Recebimento Previsto
           IF  SGTSL07A-DT-RCBT-PRVT-ULT   EQUAL  SPACES  OR
               SGTSL07A-DT-RCBT-PRVT-ULT    EQUAL  '00000000'
               MOVE  '01.01.1900'      TO  DT-RCBT-PRVT-DD-MM-AA
           ELSE
               MOVE  CORR  SGTSL07A-DT-RCBT-PRVT-ULT-R
                                       TO  DT-RCBT-PRVT-DD-MM-AA-R
               MOVE  '.'               TO  PONTO-01
                                           PONTO-02
               PERFORM  141000-VALIDAR-DATA
           END-IF

           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       141000-VALIDAR-DATA             SECTION.
      *--> Acionar a Funcao 01 da Rotina SBDATAF e verificar se
      *    a Data de Recebimento Previsto é válida
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           MOVE  4095                  TO  AREA-TAM
           MOVE  'SBDATAF -WRKAREA-FIM' TO  AREA-FIM
           MOVE  CORR  DT-RCBT-PRVT-DD-MM-AA-R
                                       TO  ARG01-R

           CALL  SBDATAF            USING  FUC
                                           WKSBDATA
                                           ARG01
                                           ARG02

      *--> RETURN-CODE do SBDATA
      *    22 -  Conteudo do Argumento Nao Numerico
      *    24 -  Data Informada Invalida
      *    46 -  Data Fora da Faixa de 01/01/0001 a 31/12/9999

           IF  RETURN-CODE                 NOT EQUAL  ZEROS  AND
                                           0022  AND  0024   AND 0046
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  0051              TO  MSG1-CD-ERRO-TS
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  'Erro no tratamento de datas.'
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
               MOVE  SBDATAF           TO  MSG1-NM-PGM-ACND

               MOVE 'FUC'              TO  MSGKW999-NM-VRV-MNT
               MOVE  FUC               TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'AREA-TAM'         TO MSGKW999-NM-VRV-MNT
               MOVE  AREA-TAM          TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'AREA-FIM'         TO MSGKW999-NM-VRV-MNT
               MOVE  AREA-FIM          TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG01'            TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG01             TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG02'            TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG02             TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA

           END-IF

           IF  RETURN-CODE                 EQUAL  0022  OR
                                                  0024  OR 0046
               SET   ERRO-PROGRAMA-ACIONADO   TO  TRUE
               MOVE  0120              TO  MSG1-CD-ERRO-TS
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  'Data recebimento previsto inválida.'
                                       TO  MSG1-TX-ERRO-TS
                                           MSG1-TX-RTN-PGM-ACND
               MOVE  SBDATAF           TO  MSG1-NM-PGM-ACND

               MOVE 'FUC'               TO  MSGKW999-NM-VRV-MNT
               MOVE  FUC                TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'AREA-TAM'          TO MSGKW999-NM-VRV-MNT
               MOVE  AREA-TAM           TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'AREA-FIM'         TO MSGKW999-NM-VRV-MNT
               MOVE  AREA-FIM          TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG01'           TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG01            TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG02'           TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG02            TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       141000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR-PESQUISA       SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'N'                   TO  SGTSL07A-IN-CTNC-PSQ

           PERFORM  971000-ABRE-CURSOR-PSQ
           PERFORM  972000-LER-CURSOR-PSQ
           PERFORM  973000-FECHA-CURSOR-PSQ

           PERFORM  210000-MONTAR-TABELA-RESPOSTA
                    VARYING SGTSL07A-IC-TAB FROM  0001  BY  0001
                    UNTIL SGTSL07A-IC-TAB  GREATER  SGTSL07A-QT-OCR

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-MONTAR-TABELA-RESPOSTA   SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           MOVE  NR-SEQL-RCBT-PRVT                     (SGTSL07A-IC-TAB)
             TO  SGTSL07A-NR-SEQL-RCBT-PRVT            (SGTSL07A-IC-TAB)

           MOVE  CD-TIP-RCBT-PRVT                      (SGTSL07A-IC-TAB)
             TO  SGTSL07A-CD-TIP-RCBT-PRVT             (SGTSL07A-IC-TAB)

           MOVE  DT-RCBT-PRVT                          (SGTSL07A-IC-TAB)
             TO  DT-DB2-DD-MM-AA
           MOVE  CORR  DT-DB2-DD-MM-AA-R
                                       TO  DT-OITO-DD-MM-AA-R
           MOVE  DT-OITO-DD-MM-AA
             TO  SGTSL07A-DT-RCBT-PRVT                 (SGTSL07A-IC-TAB)

           MOVE  CD-USU                                (SGTSL07A-IC-TAB)
             TO  SGTSL07A-CD-USU                       (SGTSL07A-IC-TAB)
           MOVE  TS-ULT-ATL                            (SGTSL07A-IC-TAB)
             TO  SGTSL07A-TS-ULT-ATL                   (SGTSL07A-IC-TAB)
           MOVE  PC-RCBT-PRVT                          (SGTSL07A-IC-TAB)
             TO  SGTSL07A-PC-RCBT-PRVT                 (SGTSL07A-IC-TAB)
           MOVE  VL-RCBT-PRVT                          (SGTSL07A-IC-TAB)
             TO  SGTSL07A-VL-RCBT-PRVT                 (SGTSL07A-IC-TAB)

           MOVE  TX-OBS-RCBT-PRVT                      (SGTSL07A-IC-TAB)
             TO  SGTSL07A-TX-OBS-RCBT-PRVT             (SGTSL07A-IC-TAB)

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'SGTSL07A-RPST'         )
                      CHANNEL   ( NM-CNL                  )
                      FLENGTH   ( LENGTH OF SGTSL07A-RPST )
                      FROM      ( SGTSL07A-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       971000-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           EVALUATE  TRUE
               WHEN  SGTSL07A-LISTA-TODOS
                     EXEC SQL
                          OPEN CURSOR-PSQ-01
                     END-EXEC
               WHEN  SGTSL07A-LISTA-POR-TIPO
                     EXEC SQL
                          OPEN CURSOR-PSQ-02
                     END-EXEC
               WHEN  SGTSL07A-LISTA-ULT-NR-SEQL
                     EXEC SQL
                          OPEN CURSOR-PSQ-03
                     END-EXEC
               WHEN  SGTSL07A-LISTA-POR-VERSAO
                     EXEC SQL
                          OPEN CURSOR-PSQ-04
                     END-EXEC
               WHEN  SGTSL07A-LISTA-POR-DATA
                     EXEC SQL
                          OPEN CURSOR-PSQ-05
                     END-EXEC
           END-EVALUATE

           MOVE  'S'                   TO  IN-CURSOR-PSQ-ABTO
           .
       971000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       972000-LER-CURSOR-PSQ           SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO
           MOVE  ZEROS                 TO  QT-REG-SELD

           MOVE  SPACES                TO  RCBT-PRVT-OPR-ETRD
           INITIALIZE                      RCBT-PRVT-OPR-ETRD
                                           REPLACING NUMERIC BY  ZEROS

           EVALUATE  TRUE

               WHEN  SGTSL07A-LISTA-TODOS
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-01
                                FOR 0131 ROWS
                           INTO
                                :NR-SEQL-RCBT-PRVT
                              , :CD-TIP-RCBT-PRVT
                              , :DT-RCBT-PRVT
                              , :CD-USU
                              , :TS-ULT-ATL
                              , :PC-RCBT-PRVT
                                :PC-RCBT-PRVT-N
                              , :VL-RCBT-PRVT
                                :VL-RCBT-PRVT-N
                              , :TX-OBS-RCBT-PRVT
                                :TX-OBS-RCBT-PRVT-N
                     END-EXEC

               WHEN  SGTSL07A-LISTA-POR-TIPO
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-02
                                FOR 0131 ROWS
                           INTO
                                :NR-SEQL-RCBT-PRVT
                              , :CD-TIP-RCBT-PRVT
                              , :DT-RCBT-PRVT
                              , :CD-USU
                              , :TS-ULT-ATL
                              , :PC-RCBT-PRVT
                                :PC-RCBT-PRVT-N
                              , :VL-RCBT-PRVT
                                :VL-RCBT-PRVT-N
                              , :TX-OBS-RCBT-PRVT
                                :TX-OBS-RCBT-PRVT-N
                     END-EXEC

               WHEN  SGTSL07A-LISTA-ULT-NR-SEQL
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-03
                                FOR 0131 ROWS
                           INTO
                                :NR-SEQL-RCBT-PRVT
                              , :CD-TIP-RCBT-PRVT
                              , :DT-RCBT-PRVT
                              , :CD-USU
                              , :TS-ULT-ATL
                              , :PC-RCBT-PRVT
                                :PC-RCBT-PRVT-N
                              , :VL-RCBT-PRVT
                                :VL-RCBT-PRVT-N
                              , :TX-OBS-RCBT-PRVT
                                :TX-OBS-RCBT-PRVT-N
                     END-EXEC

               WHEN  SGTSL07A-LISTA-POR-VERSAO
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-04
                                FOR 0131 ROWS
                           INTO
                                :NR-SEQL-RCBT-PRVT
                              , :CD-TIP-RCBT-PRVT
                              , :DT-RCBT-PRVT
                              , :CD-USU
                              , :TS-ULT-ATL
                              , :PC-RCBT-PRVT
                                :PC-RCBT-PRVT-N
                              , :VL-RCBT-PRVT
                                :VL-RCBT-PRVT-N
                              , :TX-OBS-RCBT-PRVT
                                :TX-OBS-RCBT-PRVT-N
                     END-EXEC

               WHEN  SGTSL07A-LISTA-POR-DATA
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-05
                                FOR 0131 ROWS
                           INTO
                                :NR-SEQL-RCBT-PRVT
                              , :CD-TIP-RCBT-PRVT
                              , :DT-RCBT-PRVT
                              , :CD-USU
                              , :TS-ULT-ATL
                              , :PC-RCBT-PRVT
                                :PC-RCBT-PRVT-N
                              , :VL-RCBT-PRVT
                                :VL-RCBT-PRVT-N
                              , :TX-OBS-RCBT-PRVT
                                :TX-OBS-RCBT-PRVT-N
                     END-EXEC

           END-EVALUATE

           MOVE  SQLERRD(3)            TO  QT-REG-SELD

           IF  QT-REG-SELD                 EQUAL  0131
               SET   SGTSL07A-TEM-MAIS-REGISTRO
                                       TO  TRUE
               MOVE  0130              TO  QT-REG-SELD
           END-IF

           MOVE  QT-REG-SELD           TO  SGTSL07A-QT-OCR

           MOVE  SQLCODE               TO  CD-FIM-CURSOR-PSQ
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973000-FECHA-CURSOR-PSQ         SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'N'                   TO  IN-CURSOR-PSQ-ABTO

           EVALUATE  TRUE
               WHEN  SGTSL07A-LISTA-TODOS
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-01
                     END-EXEC
               WHEN  SGTSL07A-LISTA-POR-TIPO
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-02
                     END-EXEC
               WHEN  SGTSL07A-LISTA-ULT-NR-SEQL
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-03
                     END-EXEC
               WHEN  SGTSL07A-LISTA-POR-VERSAO
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-04
                     END-EXEC
               WHEN  SGTSL07A-LISTA-POR-DATA
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-05
                     END-EXEC
           END-EVALUATE

           .
       973000-SAI.
           EXIT.

      *--------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *--------------------------------------------------------------

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

           MOVE  'SGTSL07A-CD-TIP-SRVC'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL07A-E-CD-TIP-SRVC
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL07A-E-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL07A-E-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL07A-E-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTSL07A-E-CD-OPR-ETRD-MCP  NUMERIC
               MOVE  SGTSL07A-E-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL07A-E-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL07A-E-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL07A-E-NR-SEQL-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL07A-E-NR-SEQL-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL07A-E-CD-TIP-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL07A-E-CD-TIP-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL07A-NR-SEQL-RCBT-PRVT-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL07A-NR-SEQL-RCBT-PRVT-ULT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL07A-CD-TIP-RCBT-PRVT-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL07A-CD-TIP-RCBT-PRVT-ULT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL07A-DT-RCBT-PRVT-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL07A-DT-RCBT-PRVT-ULT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava o Erro no MSG
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                  )
                      CHANNEL   ( NM-CNL                      )
                      FROM      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      NOHANDLE
           END-EXEC

           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      *----------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      *----------------------------------------
-INC HLPKP999
      *----------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
-INC SGTKP999
      *----------------------------------------
      * 999010-TRATA-ERRO-DB2          SECTION.
      *----------------------------------------
-INC HLPKP997
      *----------------------------------------
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

      *----------------------------------------
       999999-ENCERRAR                 SECTION.
      *----------------------------------------
           EXEC CICS
                 RETURN
           END-EXEC

           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTSL07A -----------------------------