      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSI070
      * ANALISTA..: F8658869 - Roberto Ioshimoto
      * AUTOR.....: F8658869 - Roberto Ioshimoto
      * OBJETIVO..: Inclusão na Tabela RCBT_PRVT_OPR_ETR
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS002 30/01/2019 F9188847 DE 2016/145195 - Inclusão monitoração
      *          variáveis para rotina SBDATAF e SBTIMEDT
      *        26/11/2018 - F9158937 - Permitir percentual zerado quando
      *        a parcela de capital for antes da data do evento.
      * VRS001 21.03.2017 - F8658869 - 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSI070.
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
       77  CTE-INICIO-SS               PIC  X(40)  VALUE
                                       '*** S.S. COMEÇA AQUI ***'.
       77  CTE-PROG                    PIC  X(17) VALUE
                                       '*** SGTSI070 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS002'.

       77  SGTS014G                    PIC  X(08) VALUE 'SGTS014G'.
       77  SBDATAF                     PIC  X(08) VALUE 'SBDATAF'.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *----------------------------------------------------------------*
      *                     CONTAINERS UTILIZADOS                      *
      *----------------------------------------------------------------*

      *--> Container de erro
       77 CTNR-PRM-ERRO                PIC X(16) VALUE 'PRM-ERRO'.

      *--> Container do usuário logado
       77 CTNR-PRM-INF-OPRD            PIC X(16) VALUE 'PRM-INF-OPRD'.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE ZEROS.
       01  CD-USU-ATL                  PIC  X(08) VALUE SPACES.

      *--> Áreas auxiliares da tabela RCBT_PRVT_OPR_ETR
       01  PC-RCBT-PRVT-MCR            PIC  --.--9,9999.

       01  VL-RCBT-PRVT                PIC  ----.---.---.---.--9,99.

       01  DT-RCBT-PRVT                PIC  X(10) VALUE '    -  -  '.
       01  DT-RCBT-PRVT-R              REDEFINES  DT-RCBT-PRVT.
           03 ANO                      PIC  9(04).
           03 FILLER                   PIC  X(01).
           03 MES                      PIC  9(02).
           03 FILLER                   PIC  X(01).
           03 DIA                      PIC  9(02).

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *----------------------------------------------------------------*
      *                 AREA DA SUBROTINA SBDATAF
      *----------------------------------------------------------------*
      *    Campos utilizados para validação de data Gregoriana.

       01  FUC                         PIC  X(03) VALUE 'F01'.
       01  ARG01                       PIC  9(08) VALUE 000.
       01  ARG02                       PIC  9(01) VALUE 4.

       01  AREAWORK.
         03  A-TAM                     PIC 9(0008) COMP VALUE 0.
         03  FILLER                    PIC X(4071) VALUE SPACES.
         03  A-FIM                     PIC X(0020) VALUE SPACES.

      *--> Subrotina SBTIMEDT
-INC HLPKTIME

       01  TS-SQL-ATU                  PIC  X(26)  VALUE
                                       '    -  -  -  .  .  .      '.
       01  TS-SQL-ATU-R               REDEFINES  TS-SQL-ATU.
           03  ANO                     PIC  9(04).
           03  FILLER                  PIC  X(01).
           03  MES                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  DIA                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  HORA                    PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  MINUTO                  PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  SEGUNDO                 PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  MICRO-SEGUNDO           PIC  9(06).



      *--> Indicadores de nulidades
       01  PC-RCBT-PRVT-N                   PIC S9(4) COMP-5 VALUE 0.
       01  VL-RCBT-PRVT-N                   PIC S9(4) COMP-5 VALUE 0.
       01  TX-OBS-RCBT-PRVT-N               PIC S9(4) COMP-5 VALUE 0.

      *--> Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG      PIC 9(09) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG     PIC 9(11) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG     PIC 9(04) VALUE 0.
           03  NR-SEQL-RCBT-PRVT-LOG   PIC 9(04) VALUE 0.
           03  CD-TIP-RCBT-PRVT-LOG    PIC 9(04) VALUE 0.
           03  DT-RCBT-PRVT-LOG        PIC X(10) VALUE ' '.

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book dos Container do Operador
-INC IIBKOPRD

      *--> Book dos dados do Log
       01  DADOS-LOG.
-INC SGTK014G

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book do container de requisicao e resposta deste modulo
       01  SGTSI070-DADOS.
-INC SGTKI070

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       77      CTE-FINAL-SS                PIC X(40) VALUE
               '*** STORAGE SECTION TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

           IF  NOT EXECUCAO-ONLINE  AND
               NOT EXECUCAO-BATCH
               CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997

      *--> Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM  100000-INICIAR

           PERFORM  200000-INCLUIR

           PERFORM  300000-POSTAR-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------

           PERFORM  110000-DESCOBRIR-CANAL

           PERFORM  120000-EXCLUIR-CTNR-ERRO

           PERFORM  130000-RECEBER-REQUISICAO

           PERFORM  140000-VALIDAR-PARAMETRO

           PERFORM  150000-LER-DADOS-OPRD

           PERFORM  160000-BUSCAR-TS-DT

           PERFORM  170000-TRATAR-NULIDADE
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       110000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

           SET   SEM-ERRO              TO  TRUE
           .
       110000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
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
           MOVE  LENGTH OF SGTSI070-RQSC
                                       TO  QT-CCT-EPRO
           EXEC CICS GET
                     CONTAINER ( 'SGTSI070-RQSC' )
                     CHANNEL   ( NM-CNL          )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
                     CONTAINER ( 'SGTSI070-RQSC'         )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSI070-RQSC )
                     INTO      ( SGTSI070-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------

           PERFORM  141000-VALIDAR-CAMPOS

           PERFORM  142000-VALIDAR-OPR-ETRD-MCP

           IF  SGTSI070-DT-RCBT-PRVT       EQUAL 'CURRENT'
               CONTINUE
           ELSE
               PERFORM  143000-VALIDAR-DATA
           END-IF
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       141000-VALIDAR-CAMPOS           SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada
           IF  SGTSI070-CD-CTRA-TIT-VL     EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI070-CD-CTRA-TIT-VL     NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI070-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI070-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI070-CD-OPR-ETRD-MCP    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'   TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI070-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI070-NR-VRS-OPR-ETRD    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validar Numero Sequencial
           IF  SGTSI070-NR-SEQL-RCBT-PRVT  NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0108              TO  MSG1-CD-ERRO-TS
               MOVE  'Número sequencial do recebimento previsto inválido
      -              '.'               TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validar Tipo de Recebimento Previsto
      *    01 - Normal de Juros
      *    02 - Normal de Capital
      *    03 - Extraordinario de Juros
      *    04 - Extraordinario de Capital
           IF  SGTSI070-CD-TIP-RCBT-PRVT   LESS 0001 OR
               SGTSI070-CD-TIP-RCBT-PRVT   GREATER 0004
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0109              TO  MSG1-CD-ERRO-TS
               MOVE  'Tipo do recebimento previsto inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI070-DT-RCBT-PRVT       EQUAL SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0119              TO  MSG1-CD-ERRO-TS
               MOVE  'Data do recebimento previsto não informada.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI070-TS-ULT-ATL         EQUAL SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0106              TO  MSG1-CD-ERRO-TS
               MOVE  'Timestamp última atualização não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validar Percentual e Valor de Recebimento
           IF  SGTSI070-PC-RCBT-PRVT       NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0121              TO  MSG1-CD-ERRO-TS
               MOVE  'Percentual de recebimento previsto não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI070-VL-RCBT-PRVT       NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0124              TO  MSG1-CD-ERRO-TS
               MOVE  'Valor de recebimento previsto não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI070-CD-TIP-RCBT-PRVT   EQUAL 0001 OR
               SGTSI070-CD-TIP-RCBT-PRVT   EQUAL 0003
      *------> Quando o Tipo de Recebimento for igual à
      *        01 - Normal de Juros ou 03 - Extraordinario de Juros
      *        o Percentual e Valor do Recebimento Previsto
      *        devem ser iguais à Zeros
               IF  SGTSI070-PC-RCBT-PRVT   NOT EQUAL ZEROS
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0123          TO  MSG1-CD-ERRO-TS
                   MOVE  'Percentual de recebimento previsto inválido.'
                                       TO  MSG1-TX-ERRO-TS
                   PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               END-IF
               IF  SGTSI070-VL-RCBT-PRVT   NOT EQUAL ZEROS
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0126          TO  MSG1-CD-ERRO-TS
                   MOVE  'Valor de recebimento previsto inválido.'
                                       TO  MSG1-TX-ERRO-TS
                   PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           ELSE
      *        Alterado o abaixo porque podemos ter o percentual zerado
      *        para parcelas antes do evento
               IF  SGTSI070-PC-RCBT-PRVT  IS NEGATIVE
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0123          TO  MSG1-CD-ERRO-TS
                   MOVE  'Percentual de recebimento previsto inválido.'
                                       TO  MSG1-TX-ERRO-TS
                   PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               END-IF
               IF  SGTSI070-VL-RCBT-PRVT   NOT POSITIVE
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0126          TO  MSG1-CD-ERRO-TS
                   MOVE  'Valor de recebimento previsto inválido.'
                                       TO  MSG1-TX-ERRO-TS
                   PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           END-IF
           .
       141000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       142000-VALIDAR-OPR-ETRD-MCP     SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-REG-SELD

           EXEC SQL
              SELECT 1
                INTO :QT-REG-SELD
                FROM DB2SGT.OPR_ETRD_MCP
               WHERE CD_CTRA_TIT_VL    =  :SGTSI070-CD-CTRA-TIT-VL
                 AND CD_OPR_ETRD_MCP   =  :SGTSI070-CD-OPR-ETRD-MCP
                 AND NR_VRS_OPR_ETRD   =  :SGTSI070-NR-VRS-OPR-ETRD
           END-EXEC

           IF  QT-REG-SELD                 EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0061              TO  MSG1-CD-ERRO-TS
               MOVE  'Operação estruturada inexistente.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       142000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       143000-VALIDAR-DATA             SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           MOVE  4095                  TO  A-TAM
           MOVE  'SBDATAF -WRKAREA-FIM'
                                       TO  A-FIM
           MOVE  SGTSI070-DT-RCBT-PRVT
                                       TO  ARG01

           CALL  SBDATAF            USING  FUC
                                           AREAWORK
                                           ARG01 ARG02

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  0120              TO  MSG1-CD-ERRO-TS
               MOVE  SBDATAF           TO  MSG1-NM-PGM-ACND
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  'Data recebimento prevista inválida.'
                                       TO  MSG1-TX-ERRO-TS
                                           MSG1-TX-RTN-PGM-ACND

               MOVE 'FUC'               TO  MSGKW999-NM-VRV-MNT
               MOVE  FUC                TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'A-TAM'             TO MSGKW999-NM-VRV-MNT
               MOVE  A-TAM              TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'A-FIM'             TO MSGKW999-NM-VRV-MNT
               MOVE  A-FIM              TO MSGKW999-VL-VRV-MNT
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
       143000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       150000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO  TRUE
           MOVE 'PRM-INF-OPRD'         TO  NM-CTNR

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO  TRUE

           INITIALIZE PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE

           IF  EXECUCAO-ONLINE
               MOVE  KOPRD-CD-OPRD     TO  CD-USU-ATL
           ELSE
               MOVE  'SISTEMA'         TO  CD-USU-ATL
           END-IF
           .
       150000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       160000-BUSCAR-TS-DT             SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           CALL  SBTIMEDT           USING  HLPKE001-CD-FUC
                                           HLPKE001-AREA-TRB
                                           HLPKE001-FMT-TS
                                           HLPKE001-TAM-TS
                                           HLPKE001-TS-FMTD

           IF  RETURN-CODE                  NOT EQUAL ZEROS
               SET  ERRO-PROGRAMA-ACIONADO  TO  TRUE
               MOVE  0050                   TO  MSG1-CD-ERRO-TS
               MOVE  'Erro ao obter data e hora atual.'
                                            TO  MSG1-TX-ERRO-TS
                                                MSG1-TX-RTN-PGM-ACND
               MOVE  SBTIMEDT               TO  MSG1-NM-PGM-ACND
               MOVE  RETURN-CODE            TO  MSG1-CD-RTN-PGM-ACND

               MOVE  'HLPKE001-CD-FUC'  TO  MSGKW999-NM-VRV-MNT
               MOVE  HLPKE001-CD-FUC    TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'HLPKE001-AREA-TRB' TO  MSGKW999-NM-VRV-MNT
               MOVE  HLPKE001-AREA-TRB   TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'HLPKE001-FMT-TS'   TO  MSGKW999-NM-VRV-MNT
               MOVE  HLPKE001-FMT-TS     TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'HLPKE001-TAM-TS'   TO  MSGKW999-NM-VRV-MNT
               MOVE  HLPKE001-TAM-TS     TO   MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'HLPKE001-TS-FMTD'  TO  MSGKW999-NM-VRV-MNT
               MOVE  HLPKE001-TS-FMTD    TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               PERFORM  999000-GRAVA-ERRO-E-ENCERRA

           END-IF

           MOVE  CORR HLPKE001-TS-FMTD      TO  TS-SQL-ATU-R

           IF  SGTSI070-DT-RCBT-PRVT        EQUAL 'CURRENT'
               MOVE  CORR HLPKE001-TS-FMTD  TO  DT-RCBT-PRVT-R
           ELSE
               MOVE  CORR SGTSI070-DT-RCBT-PRVT-R
                                            TO  DT-RCBT-PRVT-R
           END-IF
           .
       160000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       170000-TRATAR-NULIDADE          SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSI070-TX-OBS-RCBT-PRVT   EQUAL SPACES
               MOVE  -001              TO  TX-OBS-RCBT-PRVT-N
           END-IF
           .
       170000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-INCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

      *--> Desligar erro automatico do DB2
           EXEC SQL WHENEVER SQLERROR
                    CONTINUE
           END-EXEC

           EXEC SQL
                INSERT INTO DB2SGT.RCBT_PRVT_OPR_ETRD
                ( CD_CTRA_TIT_VL
                , CD_OPR_ETRD_MCP
                , NR_VRS_OPR_ETRD
                , NR_SEQL_RCBT_PRVT
                , CD_TIP_RCBT_PRVT
                , DT_RCBT_PRVT
                , CD_USU
                , TS_ULT_ATL
                , PC_RCBT_PRVT
                , VL_RCBT_PRVT
                , TX_OBS_RCBT_PRVT )
                VALUES
                ( :SGTSI070-CD-CTRA-TIT-VL
                , :SGTSI070-CD-OPR-ETRD-MCP
                , :SGTSI070-NR-VRS-OPR-ETRD
                , :SGTSI070-NR-SEQL-RCBT-PRVT
                , :SGTSI070-CD-TIP-RCBT-PRVT
                , :DT-RCBT-PRVT
                , :CD-USU-ATL
                , :TS-SQL-ATU
                , :SGTSI070-PC-RCBT-PRVT
                  :PC-RCBT-PRVT-N
                , :SGTSI070-VL-RCBT-PRVT
                  :VL-RCBT-PRVT-N
                , :SGTSI070-TX-OBS-RCBT-PRVT
                  :TX-OBS-RCBT-PRVT-N )
           END-EXEC

      *--> Ligar erro automatico do DB2
           EXEC SQL WHENEVER SQLERROR
                    GO TO  999010-TRATA-ERRO-DB2
           END-EXEC

           EVALUATE  SQLCODE
               WHEN  -803
                  SET   ERRO-PROGRAMADO     TO  TRUE
                  MOVE  803                 TO  MSG1-CD-ERRO-TS
                  MOVE  'Inclusão não efetuada, registro existente.'
                                            TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               WHEN  ZEROS
                     CONTINUE
               WHEN  OTHER
      *--> Aciona o tratamento de Erro Padrão para os demais casos
                     PERFORM  999010-TRATA-ERRO-DB2
           END-EVALUATE

           PERFORM  210000-GRAVAR-LOG
            .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       210000-GRAVAR-LOG               SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      DADOS-LOG

           SET   SGTS014G-INCLUSAO     TO  TRUE

           MOVE  'RCBT_PRVT_OPR_ETRD'  TO  SGTS014G-NM-ENT
           MOVE  CD-USU-ATL            TO  SGTS014G-CD-CHV-USU
           MOVE  'SGTSI070'            TO  SGTS014G-NM-PGM

           MOVE  SGTSI070-CD-CTRA-TIT-VL
                                       TO  CD-CTRA-TIT-VL-LOG
           MOVE  SGTSI070-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP-LOG
           MOVE  SGTSI070-NR-VRS-OPR-ETRD
                                       TO  NR-VRS-OPR-ETRD-LOG
           MOVE  SGTSI070-NR-SEQL-RCBT-PRVT
                                       TO  NR-SEQL-RCBT-PRVT-LOG
           MOVE  SGTSI070-CD-TIP-RCBT-PRVT
                                       TO  CD-TIP-RCBT-PRVT-LOG
           MOVE  DT-RCBT-PRVT
                                       TO  DT-RCBT-PRVT-LOG
           STRING CD-CTRA-TIT-VL-LOG
              ';' CD-OPR-ETRD-MCP-LOG
              ';' NR-VRS-OPR-ETRD-LOG
              ';' NR-SEQL-RCBT-PRVT-LOG
              ';' CD-TIP-RCBT-PRVT-LOG
              ';' DT-RCBT-PRVT-LOG
                  DELIMITED BY SIZE    INTO SGTS014G-CHV-PRMR

           MOVE  LENGTH OF DADOS-LOG   TO  EIBCALEN

           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM         NOT EQUAL ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  112               TO  MSG1-CD-ERRO-TS
               MOVE 'Erro na gravação das informações do Log.'
                                       TO  MSG1-TX-ERRO-TS
               MOVE  SGTS014G          TO  MSG1-NM-PGM-ACND
               MOVE  SGTS014G-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTS014G-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       210000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'Inclusão efetuada com sucesso.'
                                       TO  SGTSI070-TX-RTN-PRCT

           EXEC CICS PUT
                     CONTAINER ( 'SGTSI070-RPST'         )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSI070-RPST )
                     FROM      ( SGTSI070-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Verifica se o tipo de erro gerado possui tratamento interno
           EVALUATE  TRUE
               WHEN  ERRO-CONTAINER
                     PERFORM 999004-TRATA-ERRO-CTNR
               WHEN  ERRO-TAMANHO-AREA
                     PERFORM 999005-TRATA-ERRO-AREA-DADOS
               WHEN  ERRO-CANAL
                     PERFORM 999006-TRATA-ERRO-CANAL
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

           MOVE  'I070-000-IIBKERRO-TX-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  MSG1-TX-ERRO-TS       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'I070-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI070-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'I070-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTSI070-CD-OPR-ETRD-MCP    NUMERIC
               MOVE  SGTSI070-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'I070-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI070-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'I070-004-NR-SEQL-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI070-NR-SEQL-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'I070-005-CD-TIP-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI070-CD-TIP-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'I070-006-DT-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI070-DT-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'I070-007-KOPRD-CD-OPRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  KOPRD-CD-OPRD         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'I070-008-TS-ULT-ATL' TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI070-TS-ULT-ATL   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'I070-009-PC-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTSI070-PC-RCBT-PRVT       NUMERIC
               MOVE  SGTSI070-PC-RCBT-PRVT
                                       TO  PC-RCBT-PRVT-MCR
               MOVE  PC-RCBT-PRVT-MCR  TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'I070-010-VL-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTSI070-VL-RCBT-PRVT       NUMERIC
               MOVE  SGTSI070-VL-RCBT-PRVT
                                       TO  VL-RCBT-PRVT
               MOVE  VL-RCBT-PRVT      TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'I070-011-TX-OBS-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI070-TX-OBS-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava o erro no Sistema MSG
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

           EXEC CICS PUT
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
      *------------------------- Fim SGTSI070 -------------------------*