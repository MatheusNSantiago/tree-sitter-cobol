      *-----------------------------------------------------------------
      * Programa..: SGTSA070
      * Analista..: F6361711 - Luis A P Valamede
      * Autor.....: F6361711 - Luis A P Valamede
      * Objetivo..: Alteração na tabela RCBT_PRVT_OPR_ETRD
      * Compilação: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 17/05/2017 - F6361711 DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSA070.

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

       77  CTE-INICIO-SS               PIC X(35) VALUE
               '*** STORAGE SECTION COMECA AQUI ***'.
       77  CTE-PROG                    PIC X(18) VALUE
               '*** SGTSA070 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS001'.

      *----------------------------------------------------------------*
      *                    SUBROTINAS UTILIZADAS
      *----------------------------------------------------------------*

      * Programa de LOG para armazenar registros de trilha de auditoria
       77  SGTS014G                    PIC X(08) VALUE 'SGTS014G'.

      *----------------------------------------------------------------*
      *                            OUTROS
      *----------------------------------------------------------------*

      * Constante informada para se obter data atual do sistema
       77  SBTIMEDT                    PIC  X(08)  VALUE 'SBTIMEDT'.
      *----------------------------------------------------------------*

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *--> Subrotina SBTIMEDT
      *    Campos utilizados para obtencao de timestamp atual
       01  DADOS-SBTIMEDT.
           03  HLPKE001-CD-FUC         PIC X(03) VALUE 'F01'.
           03  HLPKE001-AREA-TRB.
               05  HLPKE001-AREA-TRB-TAM
                                       PIC S9(9) COMP VALUE 1024.
               05  HLPKE001-AREA-TRB-DADOS
                                       PIC  X(1024) VALUE ' '.
           03  HLPKE001-FMT-TS         PIC S9(4) COMP VALUE 1.
           03  HLPKE001-TAM-TS         PIC S9(4) COMP VALUE 30.
           03  HLPKE001-TS-FMTD.
               05  AA                  PIC  9(04).
               05  FILLER              PIC  X(01).
               05  MM                  PIC  9(02).
               05  FILLER              PIC  X(01).
               05  DD                  PIC  9(02).
               05  FILLER              PIC  X(01).
               05  HH                  PIC  X(02).
               05  FILLER              PIC  X(01).
               05  MNTO                PIC  X(02).
               05  FILLER              PIC  X(01).
               05  SGDO                PIC  X(02).
               05  FILLER              PIC  X(01).
               05  MSGD                PIC  X(06).

       01  TS-SQL-ATU                  PIC  X(26) VALUE
                                       '0000-00-00-00.00.00.000000'.
       01  TS-SQL-ATU-R
           REDEFINES TS-SQL-ATU.
           03  AA                      PIC  9(04).
           03  FILLER                  PIC  X(01).
           03  MM                      PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  DD                      PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  HH                      PIC  X(02).
           03  FILLER                  PIC  X(01).
           03  MNTO                    PIC  X(02).
           03  FILLER                  PIC  X(01).
           03  SGDO                    PIC  X(02).
           03  FILLER                  PIC  X(01).
           03  MSGD                    PIC  X(06).

      *--> Áreas auxiliares da tabela RCBT_PRVT_OPR_ETR
       01  PC-RCBT-PRVT                PIC  --.--9,9999.
       01  VL-RCBT-PRVT                PIC  ----.---.---.---.--9,99.
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE 0.

       01  DT-RCBT-PRVT                PIC  X(10) VALUE '  .  .    '.
       01  DT-RCBT-PRVT-R              REDEFINES  DT-RCBT-PRVT.
           03 DIA                      PIC  9(02).
           03 FILLER                   PIC  X(01).
           03 MES                      PIC  9(02).
           03 FILLER                   PIC  X(01).
           03 ANO                      PIC  9(04).

      *--> Indicadores de nulidades
       01  PC-RCBT-PRVT-N              PIC  S9(4) COMP-5 VALUE 0.
       01  VL-RCBT-PRVT-N              PIC  S9(4) COMP-5 VALUE 0.
       01  TX-OBS-RCBT-PRVT-N          PIC  S9(4) COMP-5 VALUE 0.


      *--> Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG      PIC  9(09) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG     PIC  9(11) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG     PIC  9(04) VALUE 0.
           03  NR-SEQL-RCBT-PRVT-LOG   PIC  9(04) VALUE 0.
           03  CD-TIP-RCBT-PRVT-LOG    PIC  9(04) VALUE 0.
           03  DT-RCBT-PRVT-LOG        PIC  X(10) VALUE '  .  .    '.
           03  DT-RCBT-PRVT-LOG-R      REDEFINES  DT-RCBT-PRVT-LOG.
               05 DIA                  PIC  9(02).
               05 FILLER               PIC  X(01).
               05 MES                  PIC  9(02).
               05 FILLER               PIC  X(01).
               05 ANO                  PIC  9(04).

      *----------------------------------------------------------------*
      *                      BOOKS UTILIZADOS
      *----------------------------------------------------------------*

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informações
-INC HLPKE01B

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      * Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

      * Book do container de erro
-INC IIBKERRO

      * Book dos dados do usuário
-INC IIBKOPRD

      * Book para captura de erro via MSG1
-INC MSGKWMG1

      * Book para captura de abend cics do bloco codigo MSGKP999
-INC HLPKW999

      * Book do programa SGTSA070, com áreas de requisição e resposta
       01  AREA-SGTSA070.
-INC SGTKA070

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC X(40) VALUE
               '*** STORAGE SECTION TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

           IF NOT EXECUCAO-ONLINE AND NOT  EXECUCAO-BATCH
              CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      * Habilita tratamento automatico de Erros DB2
-INC HLPKE997

      * Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM 110000-DESCOBRIR-CANAL

           PERFORM 120000-EXCLUIR-CTNR-ERRO

           PERFORM 130000-RECEBER-REQUISICAO

           PERFORM 140000-VALIDAR-PARAMETROS

           PERFORM 200000-CONSULTAR

           PERFORM 300000-ALTERAR

           PERFORM 400000-POSTAR-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL )
           END-EXEC
           .
       110000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'PRM-ERRO'  )
              CHANNEL   ( NM-CNL      )
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
       130000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-CCT-LIDO
           MOVE  LENGTH OF SGTSA070-RQSC
                                       TO  QT-CCT-EPRO
           EXEC CICS GET
              CONTAINER ( 'SGTSA070-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA
                                       TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSA070-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSA070-RQSC )
              INTO      ( SGTSA070-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETROS       SECTION.
      *-----------------------------------------------------------------

           PERFORM 141000-VALIDAR-CAMPOS

           PERFORM 142000-VALIDAR-OPERACAO

           PERFORM 143000-VALIDAR-PC-VALOR

           PERFORM 144000-BUSCAR-TS-DATA

           PERFORM 145000-BUSCAR-OPERADOR

           PERFORM 146000-TRATAR-NULIDADE
            .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       141000-VALIDAR-CAMPOS           SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSA070-CD-CTRA-TIT-VL     EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF


           IF  SGTSA070-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada de mercado de capit
      -              'ais não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA070-CD-OPR-ETRD-MCP    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada de mercado de capita
      -              'is inválido.'    TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA070-NR-VRS-OPR-ETRD    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA070-NR-SEQL-RCBT-PRVT  NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0108              TO  MSG1-CD-ERRO-TS
               MOVE 'Número sequencial do recebimento previsto inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validar Tipo de Recebimento Previsto
      *    01 - Normal de Juros
      *    02 - Normal de Capital
      *    03 - Extraordinário de Juros
      *    04 - Extraordinário de Capital
           IF  SGTSA070-CD-TIP-RCBT-PRVT   LESS 0001 OR
               SGTSA070-CD-TIP-RCBT-PRVT GREATER 0004
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0109              TO  MSG1-CD-ERRO-TS
               MOVE 'Código do tipo de recebimento previsto inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA070-DT-RCBT-PRVT       EQUAL SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0119              TO  MSG1-CD-ERRO-TS
               MOVE 'Data do recebimento previsto não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA070-TS-ULT-ATL         EQUAL SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0106              TO  MSG1-CD-ERRO-TS
               MOVE  'Timestamp da última atualização não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       141000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       142000-VALIDAR-OPERACAO         SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-REG-SELD

           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
              FROM DB2SGT.OPR_ETRD_MCP
              WHERE CD_CTRA_TIT_VL    =   :SGTSA070-CD-CTRA-TIT-VL
                AND CD_OPR_ETRD_MCP   =   :SGTSA070-CD-OPR-ETRD-MCP
                AND NR_VRS_OPR_ETRD   =   :SGTSA070-NR-VRS-OPR-ETRD
           END-EXEC

           IF  QT-REG-SELD                 EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0061              TO  MSG1-CD-ERRO-TS
               MOVE 'Operação estruturada de mercado de capitais inexist
      -              'ente.'           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       142000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       143000-VALIDAR-PC-VALOR         SECTION.
      *--> Validar Percentual e o Valor do Recebimento Previsto
      *    conforme o Tipo de Recebimento Previsto
      *-----------------------------------------------------------------
           MOVE 0007                   TO  MSG1-CD-SEQL-ERRO

           EVALUATE  SGTSA070-CD-TIP-RCBT-PRVT
      *------------> Validar Percentual e Valor para Juros
      *              01 - Normal de Juros
      *              03 - Extraordinário de Juros
               WHEN  0001
               WHEN  0003
                     IF  SGTSA070-PC-RCBT-PRVT
                                           NOT EQUAL  ZEROS
                         SET   ERRO-PROGRAMADO
                                       TO  TRUE
                         MOVE  0488    TO  MSG1-CD-ERRO-TS
                         MOVE  'Para juros normal ou extraordinário o pe
      -                        'rcentual previsto deverá ser igual a zer
      -                        'o.'    TO  MSG1-TX-ERRO-TS
                         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
                     END-IF
                     IF  SGTSA070-VL-RCBT-PRVT
                                           NOT EQUAL  ZEROS
                         SET  ERRO-PROGRAMADO
                                       TO  TRUE
                         MOVE  0489    TO  MSG1-CD-ERRO-TS
                         MOVE  'Para juros normal ou extraordinário o va
      -                        'lor previsto deverá ser igual a zero.'
                                       TO  MSG1-TX-ERRO-TS
                         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
                     END-IF
      *------------> Validar Percentual e Valor para Capital
      *              02 - Normal de Capital
      *              04 - Extraordinário de Capital
               WHEN  0002
               WHEN  0004
      *        Inibir o abaixo porque podemos ter o percentual zerado
      *        para parcelas antes do evento
      *               IF  SGTSA070-PC-RCBT-PRVT
      *                                     NOT POSITIVE
      *                   SET  ERRO-PROGRAMADO
      *                                 TO  TRUE
      *                   MOVE  0490    TO  MSG1-CD-ERRO-TS
      *                   MOVE  'Para capital o percentual previsto dever
      *-                        'á ser maior que zero.'
      *                                 TO  MSG1-TX-ERRO-TS
      *                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
      *               END-IF
                     IF  SGTSA070-VL-RCBT-PRVT
                                           NOT POSITIVE
                         SET   ERRO-PROGRAMADO
                                       TO  TRUE
                         MOVE  0491    TO  MSG1-CD-ERRO-TS
                         MOVE  'Para capital o valor previsto deverá ser
      -                        ' maior que zero.'
                                       TO  MSG1-TX-ERRO-TS
                         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
                     END-IF
           END-EVALUATE
           .
       143000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       144000-BUSCAR-TS-DATA           SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           CALL SBTIMEDT            USING HLPKE001-CD-FUC
                                          HLPKE001-AREA-TRB
                                          HLPKE001-FMT-TS
                                          HLPKE001-TAM-TS
                                          HLPKE001-TS-FMTD

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               MOVE  0050              TO  MSG1-CD-ERRO-TS
               MOVE 'Erro na rotina para obter data atual.'
                                       TO  MSG1-TX-ERRO-TS
                                          MSG1-TX-RTN-PGM-ACND
               MOVE  SBTIMEDT          TO  MSG1-NM-PGM-ACND
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  HLPKE001-TS-FMTD      TO  TS-SQL-ATU

           IF  SGTSA070-TS-ULT-ATL         EQUAL 'CURRENT'
               MOVE  TS-SQL-ATU        TO  SGTSA070-TS-ULT-ATL
           END-IF

           .
       144000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       145000-BUSCAR-OPERADOR          SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSA070-CD-USU             EQUAL SPACES
               IF  EXECUCAO-ONLINE
                   PERFORM 910000-LER-DADOS-OPRD
                   MOVE  KOPRD-CD-OPRD
                                       TO  SGTSA070-CD-USU
               ELSE
                   MOVE  'SISTEMA'
                                       TO  SGTSA070-CD-USU
               END-IF
           END-IF
           .
       145000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       146000-TRATAR-NULIDADE          SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSA070-TX-OBS-RCBT-PRVT   EQUAL SPACES
               MOVE  -001              TO  TX-OBS-RCBT-PRVT-N
           END-IF
           .
       146000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-CONSULTAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           MOVE  CORR SGTSA070-DT-RCBT-PRVT-R
                                       TO  DT-RCBT-PRVT-R

           EXEC SQL
                SELECT  1
                  INTO  :QT-REG-SELD
                  FROM  DB2SGT.RCBT_PRVT_OPR_ETRD
                 WHERE  CD_CTRA_TIT_VL    = :SGTSA070-CD-CTRA-TIT-VL
                   AND  CD_OPR_ETRD_MCP   = :SGTSA070-CD-OPR-ETRD-MCP
                   AND  NR_VRS_OPR_ETRD   = :SGTSA070-NR-VRS-OPR-ETRD
                   AND  NR_SEQL_RCBT_PRVT = :SGTSA070-NR-SEQL-RCBT-PRVT
                   AND  CD_TIP_RCBT_PRVT  = :SGTSA070-CD-TIP-RCBT-PRVT
                   AND  DT_RCBT_PRVT      = :DT-RCBT-PRVT
            END-EXEC

           IF  QT-REG-SELD                 EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE  0492              TO  MSG1-CD-ERRO-TS
               MOVE 'Recebimento previsto de operação estruturada inexis
      -             'tente.'           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .                                                            o
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-ALTERAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           EXEC SQL
                UPDATE DB2SGT.RCBT_PRVT_OPR_ETRD
                   SET CD_USU            = :SGTSA070-CD-USU
                     , TS_ULT_ATL        = :SGTSA070-TS-ULT-ATL
                     , PC_RCBT_PRVT      = :SGTSA070-PC-RCBT-PRVT
                                           :PC-RCBT-PRVT-N
                     , VL_RCBT_PRVT      = :SGTSA070-VL-RCBT-PRVT
                                           :VL-RCBT-PRVT-N
                     , TX_OBS_RCBT_PRVT  = :SGTSA070-TX-OBS-RCBT-PRVT
                                           :TX-OBS-RCBT-PRVT-N
                 WHERE CD_CTRA_TIT_VL    = :SGTSA070-CD-CTRA-TIT-VL
                   AND CD_OPR_ETRD_MCP   = :SGTSA070-CD-OPR-ETRD-MCP
                   AND NR_VRS_OPR_ETRD   = :SGTSA070-NR-VRS-OPR-ETRD
                   AND NR_SEQL_RCBT_PRVT = :SGTSA070-NR-SEQL-RCBT-PRVT
                   AND CD_TIP_RCBT_PRVT  = :SGTSA070-CD-TIP-RCBT-PRVT
                   AND DT_RCBT_PRVT      = :DT-RCBT-PRVT
           END-EXEC

           IF  SQLCODE                     EQUAL  +100
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0100              TO  MSG1-CD-ERRO-TS
               MOVE  'Registro inexistente.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           PERFORM 920000-GRAVA-LOG
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       400000-POSTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'Alteração efetuada com sucesso.'
                                       TO  SGTSA070-TX-RTN-PRCT
           EXEC CICS PUT
              CONTAINER ( 'SGTSA070-RPST'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSA070-RPST )
              FROM      ( SGTSA070-RPST           )
           END-EXEC
           .
       400000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       910000-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição.
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-INF-OPRD'        TO  NM-CTNR

      *--> Devido à captura automática de erro CICS, deve ser previsto o
      *    o erro de container.
           SET   ERRO-CONTAINER        TO  TRUE

           INITIALIZE                      PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *--> Ao chegar aqui, há certeza de que não houve erro algum.
           SET   SEM-ERRO              TO  TRUE
           .
       910000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       920000-GRAVA-LOG                SECTION.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      DADOS-LOG
           MOVE  'A'                   TO  SGTS014G-CD-TIP-ACAO

           MOVE  'RCBT_PRVT_OPR_ETRD  '
                                       TO  SGTS014G-NM-ENT

           MOVE  SGTSA070-CD-CTRA-TIT-VL
                                       TO  CD-CTRA-TIT-VL-LOG
           MOVE  SGTSA070-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP-LOG
           MOVE  SGTSA070-NR-VRS-OPR-ETRD
                                       TO  NR-VRS-OPR-ETRD-LOG
           MOVE  SGTSA070-NR-SEQL-RCBT-PRVT
                                       TO  NR-SEQL-RCBT-PRVT-LOG
           MOVE  SGTSA070-CD-TIP-RCBT-PRVT
                                       TO  CD-TIP-RCBT-PRVT-LOG
           MOVE  CORR SGTSA070-DT-RCBT-PRVT-R
                                       TO  DT-RCBT-PRVT-LOG-R
           STRING CD-CTRA-TIT-VL-LOG
              ';' CD-OPR-ETRD-MCP-LOG
              ';' NR-VRS-OPR-ETRD-LOG
              ';' NR-SEQL-RCBT-PRVT-LOG
              ';' CD-TIP-RCBT-PRVT-LOG
              ';' DT-RCBT-PRVT-LOG
                  DELIMITED BY SIZE    INTO SGTS014G-CHV-PRMR

           MOVE  SGTSA070-CD-USU       TO  SGTS014G-CD-CHV-USU

           MOVE  'SGTSA070'            TO  SGTS014G-NM-PGM

           MOVE  LENGTH OF DADOS-LOG   TO  EIBCALEN

           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM         NOT EQUAL ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  0112              TO  MSG1-CD-ERRO-TS
               MOVE  'Erro na gravação das informações do Log.'
                                       TO  MSG1-TX-ERRO-TS
               MOVE  SGTS014G-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SGTS014G-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       920000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

           IF  MSG1-CD-ERRO-TS             EQUAL ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

           MOVE 'SGTSA070-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSA070-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA070-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSA070-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE  SGTSA070-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA070-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSA070-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA070-NR-SEQL-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSA070-NR-SEQL-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA070-CD-TIP-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSA070-CD-TIP-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA070-DT-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSA070-DT-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'KOPRD-CD-OPRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  KOPRD-CD-OPRD         TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA070-TS-ULT-ATL'  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSA070-TS-ULT-ATL   TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA070-PC-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSA070-PC-RCBT-PRVT
                                       TO  PC-RCBT-PRVT
           MOVE  PC-RCBT-PRVT          TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA070-VL-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSA070-VL-RCBT-PRVT
                                       TO  VL-RCBT-PRVT
           MOVE  VL-RCBT-PRVT          TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA070-TX-OBS-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSA070-TX-OBS-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

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
      * --------------------------------------*
-INC SGTKP999
      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
      * --------------------------------------*
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
       999999-ENCERRAR                 SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.