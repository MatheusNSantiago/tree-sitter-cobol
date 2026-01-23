      *-----------------------------------------------------------------
      * Programa..: SGTSA087
      * Analista..: F2054109 Cintia Ap Soares Iglesias
      * Autor.....: F2054109 Cintia Ap Soares Iglesias
      * Objetivo..: Alterar Recebimento Realizado do Cronograma
      *             Tabela RCBT_RLZD_OPR
      * Compilacao: Cobol/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 29.05.2017 F2054109  DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSA087.

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
       77  CTE-PROG                    PIC  X(18)  VALUE
                                       '*** SGTSA087 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *--> Módulos Acionados
       77  SGTS014G                    PIC  X(08) VALUE 'SGTS014G'.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  CD-USU-ATL                  PIC  X(08) VALUE SPACES.

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).
       01  VL-15V02                    PIC  ----.---.---.---.--9,99.

       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE 0.
-INC HLPKTIME
       01  DT-RCBT-PRVT                  PIC  X(10) VALUE '  .  .    '.
       01  DT-RCBT-PRVT-R REDEFINES DT-RCBT-PRVT.
           03 DIA                      PIC  9(02).
           03 FILLER                   PIC  X(01).
           03 MES                      PIC  9(02).
           03 FILLER                   PIC  X(01).
           03 ANO                      PIC  9(04).

       01  TS-SQL-ATU                  PIC  X(26) VALUE ' '.

       01  TS-REG-PGTO                 PIC  X(26)
                                     VALUE '    -  -  -  .  .  .      '.
       01  TS-REG-PGTO-R               REDEFINES TS-REG-PGTO.
           03  ANO                     PIC  9(04).
           03  FILLER                  PIC  X(01).
           03  MES                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  DIA                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  HORA                    PIC  9(02).
           03  FILLE                   PIC  X(01).
           03  MINUTO                  PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  SEGUNDO                 PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  MICRO-SEGUNDO           PIC  9(06).

      *--> Indicadores de nulidades
       01  VL-RLZD-VCLD-N              PIC S9(4) COMP-5 VALUE 0.

      *--> Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG      PIC  9(10) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG     PIC  9(11) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG     PIC  9(05) VALUE 0.
           03  NR-SEQL-RCBT-PRVT-LOG   PIC  9(05) VALUE 0.
           03  CD-TIP-RCBT-PRVT-LOG    PIC  9(05) VALUE 0.
           03  DT-RCBT-PRVT-LOG        PIC  X(10) VALUE ' '.
           03  CD-TIT-OPR-ETRD-LOG     PIC  X(12) VALUE SPACES.
           03  NR-DVS-TIT-LOG          PIC  9(05) VALUE 0.
           03  TS-REG-PGTO-LOG         PIC  X(26) VALUE ' '.

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book dos dados do usuário
-INC IIBKOPRD

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book do container de requisicao e resposta deste modulo
-INC SGTKA087

      *-----------------------------------------------------------------
      *    Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      *--> Book dos dados do Log
       01  DADOS-LOG.
-INC SGTK014G

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
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

           PERFORM  200000-ALTERAR

           PERFORM  300000-POSTAR-RESPOSTA

           GOBACK

           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  110000-DESCOBRIR-CANAL
           PERFORM  120000-EXCLUIR-CTNR-ERRO
           PERFORM  130000-RECEBER-REQUISICAO

           PERFORM  140000-VALIDAR-PARAMETRO

           PERFORM  150000-LER-DADOS-OPRD

           PERFORM  160000-BUSCAR-TS-DT

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'PRM-ERRO' )
                      CHANNEL   ( NM-CNL     )
                      RESP      ( MSG1-RESP1 )
                      RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CICS   TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-CCT-LIDO
           MOVE  LENGTH OF SGTSA087-RQSC
                                       TO  QT-CCT-EPRO
           EXEC CICS  GET
                      CONTAINER ( 'SGTSA087-RQSC' )
                      CHANNEL   ( NM-CNL          )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA  TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS  GET
                      CONTAINER ( 'SGTSA087-RQSC'         )
                      CHANNEL   ( NM-CNL                  )
                      FLENGTH   ( LENGTH OF SGTSA087-RQSC )
                      INTO      ( SGTSA087-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  141000-VALIDAR-CAMPOS

           PERFORM  142000-VALIDAR-RCBT-PREVISTO

           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       141000-VALIDAR-CAMPOS           SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada
           IF  SGTSA087-CD-CTRA-TIT-VL     EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA087-CD-CTRA-TIT-VL     NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA087-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não numérico.'
                                      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA087-CD-OPR-ETRD-MCP   EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA087-CD-OPR-ETRD-MCP    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'   TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA087-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA087-NR-VRS-OPR-ETRD    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validar o Numero Sequencial
           IF  SGTSA087-NR-SEQL-RCBT-PRVT  NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0315              TO  MSG1-CD-ERRO-TS
               MOVE  'Número sequencial do recebimento previsto não info
      -              'rmado.'           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validar Tipo de Recebimento Previsto
      *    01 - Normal de Juros
      *    02 - Normal de Capital
      *    03 - Extraordinario de Juros
      *    04 - Extraordinario de Capital
           IF  SGTSA087-CD-TIP-RCBT-PRVT   LESS  0001 OR
               SGTSA087-CD-TIP-RCBT-PRVT GREATER 0004
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0109              TO  MSG1-CD-ERRO-TS
               MOVE  'Tipo de recebimento previsto inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA087-DT-RCBT-PRVT       EQUAL  SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0119              TO  MSG1-CD-ERRO-TS
               MOVE  'Data do recebimento previsto não informada.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA087-CD-TIT-OPR-ETRD    EQUAL  SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0231              TO  MSG1-CD-ERRO-TS
               MOVE 'Código do título da operação estruturado não inform
      -             'ado.'             TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA087-NR-DVS-TIT         EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0232              TO  MSG1-CD-ERRO-TS
               MOVE 'Número da divisão do título da operação não informa
      -              'do.'             TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA087-TS-REG-PGTO        EQUAL  SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0316              TO  MSG1-CD-ERRO-TS
               MOVE  'Timestamp do registro de pagamento não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA087-IN-VCL-MNL         EQUAL  SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0317              TO  MSG1-CD-ERRO-TS
               MOVE  'Indicador do vínculo manual não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA087-CD-USU-VCL         EQUAL  SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0319              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do usuário que efetuou vínculo não informad
      -              'o.'               TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA087-TS-ULT-ATL         EQUAL  SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0106              TO  MSG1-CD-ERRO-TS
               MOVE  'Timestamp última atualização não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA087-VL-RLZD-VCLD       NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0318              TO  MSG1-CD-ERRO-TS
               MOVE  'Valor realizado vinculado não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       1410000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       142000-VALIDAR-RCBT-PREVISTO    SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

      *--> Ajusta o formato da Data para o padrão do DB2.
           MOVE  CORR SGTSA087-DT-RCBT-PRVT-R
                                       TO  DT-RCBT-PRVT-R

           MOVE  ZEROS                 TO  QT-REG-SELD
           EXEC SQL
             SELECT  1
               INTO :QT-REG-SELD
               FROM DB2SGT.RCBT_PRVT_OPR_ETRD
              WHERE CD_CTRA_TIT_VL     = :SGTSA087-CD-CTRA-TIT-VL
                AND CD_OPR_ETRD_MCP    = :SGTSA087-CD-OPR-ETRD-MCP
                AND NR_VRS_OPR_ETRD    = :SGTSA087-NR-VRS-OPR-ETRD
                AND NR_SEQL_RCBT_PRVT  = :SGTSA087-NR-SEQL-RCBT-PRVT
                AND CD_TIP_RCBT_PRVT   = :SGTSA087-CD-TIP-RCBT-PRVT
                AND DT_RCBT_PRVT       = :DT-RCBT-PRVT
           END-EXEC

           IF  QT-REG-SELD                 EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0320              TO  MSG1-CD-ERRO-TS
               MOVE  'Recebimento previsto operação estruturado inexiste
      -              'nte.'            TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       142000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      PRM-INF-OPRD

           EXEC CICS  GET
                      CONTAINER ( 'PRM-INF-OPRD'                       )
                      INTO      ( PRM-INF-OPRD                         )
                      FLENGTH   ( LENGTH OF PRM-INF-OPRD               )
           END-EXEC

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
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           CALL SBTIMEDT            USING  HLPKE001-CD-FUC
                                           HLPKE001-AREA-TRB
                                           HLPKE001-FMT-TS
                                           HLPKE001-TAM-TS
                                           HLPKE001-TS-FMTD
           IF  RETURN-CODE                 NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  0050              TO  MSG1-CD-ERRO-TS
               MOVE  'Erro ao obter data e hora atual.'
                                       TO  MSG1-TX-ERRO-TS
                                           MSG1-TX-RTN-PGM-ACND
               MOVE   SBTIMEDT         TO  MSG1-NM-PGM-ACND
               MOVE   RETURN-CODE      TO  MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  HLPKE001-TS-FMTD      TO  TS-SQL-ATU

           IF  SGTSA087-TS-ULT-ATL         EQUAL  'CURRENT'
               MOVE  TS-SQL-ATU        TO  SGTSA087-TS-ULT-ATL
           END-IF
           .
       160000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-ALTERAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

      *--> Trata Nulidade
           IF  SGTSA087-VL-RLZD-VCLD   EQUAL  ZEROS
               MOVE  -001              TO  VL-RLZD-VCLD-N
           END-IF

      *--> Ajusta o formato do Timestamp para o padrão do DB2.
           MOVE CORR SGTSA087-TS-REG-PGTO-R
                                       TO  TS-REG-PGTO-R
           EXEC SQL
             UPDATE DB2SGT.RCBT_RLZD_OPR
                SET IN_VCL_MNL         = :SGTSA087-IN-VCL-MNL
                  , CD_USU_VCL         = :CD-USU-ATL
                  , TS_ULT_ATL         = :SGTSA087-TS-ULT-ATL
                  , VL_RLZD_VCLD       = :SGTSA087-VL-RLZD-VCLD
                                         :VL-RLZD-VCLD-N
              WHERE CD_CTRA_TIT_VL     = :SGTSA087-CD-CTRA-TIT-VL
                AND CD_OPR_ETRD_MCP    = :SGTSA087-CD-OPR-ETRD-MCP
                AND NR_VRS_OPR_ETRD    = :SGTSA087-NR-VRS-OPR-ETRD
                AND NR_SEQL_RCBT_PRVT  = :SGTSA087-NR-SEQL-RCBT-PRVT
                AND CD_TIP_RCBT_PRVT   = :SGTSA087-CD-TIP-RCBT-PRVT
                AND DT_RCBT_PRVT       = :DT-RCBT-PRVT
                AND CD_TIT_OPR_ETRD    = :SGTSA087-CD-TIT-OPR-ETRD
                AND NR_DVS_TIT         = :SGTSA087-NR-DVS-TIT
                AND TS_REG_PGTO        = :TS-REG-PGTO
           END-EXEC

           IF  SQLCODE                 EQUAL  +100
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0100              TO  MSG1-CD-ERRO-TS
               MOVE  'Registro inexistente.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           PERFORM  210000-GRAVAR-LOG
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-GRAVAR-LOG               SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      DADOS-LOG

           SET   SGTS014G-ALTERACAO    TO  TRUE

           MOVE  'RCBT_RLZD_OPR'       TO  SGTS014G-NM-ENT
           MOVE  CD-USU-ATL            TO  SGTS014G-CD-CHV-USU
           MOVE  'SGTSA087'            TO  SGTS014G-NM-PGM

           MOVE  SGTSA087-CD-CTRA-TIT-VL
                                       TO  CD-CTRA-TIT-VL-LOG
           MOVE  SGTSA087-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP-LOG
           MOVE  SGTSA087-NR-VRS-OPR-ETRD
                                       TO  NR-VRS-OPR-ETRD-LOG
           MOVE  SGTSA087-NR-SEQL-RCBT-PRVT
                                       TO  NR-SEQL-RCBT-PRVT-LOG
           MOVE  SGTSA087-CD-TIP-RCBT-PRVT
                                       TO  CD-TIP-RCBT-PRVT-LOG
           MOVE  DT-RCBT-PRVT          TO  DT-RCBT-PRVT-LOG
           MOVE  SGTSA087-CD-TIT-OPR-ETRD
                                       TO  CD-TIT-OPR-ETRD-LOG
           MOVE  SGTSA087-NR-DVS-TIT   TO  NR-DVS-TIT-LOG
           MOVE  TS-REG-PGTO           TO  TS-REG-PGTO-LOG

           STRING  CD-CTRA-TIT-VL-LOG
                   ';'    CD-OPR-ETRD-MCP-LOG
                   ';'    NR-VRS-OPR-ETRD-LOG
                   ';'    NR-SEQL-RCBT-PRVT-LOG
                   ';'    CD-TIP-RCBT-PRVT-LOG
                   ';'    DT-RCBT-PRVT-LOG
                   ';'    CD-TIT-OPR-ETRD-LOG
                   ';'    NR-DVS-TIT-LOG
                   ';'    TS-REG-PGTO-LOG
                   DELIMITED BY SIZE
                   INTO                    SGTS014G-CHV-PRMR
           END-STRING

           MOVE  LENGTH OF DADOS-LOG   TO  EIBCALEN

           CALL  SGTS014G           USING  DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM         NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  0112              TO  MSG1-CD-ERRO-TS
               MOVE  'Erro na gravação das informações do Log.'
                                       TO  MSG1-TX-ERRO-TS
               MOVE  SGTS014G          TO  MSG1-NM-PGM-ACND
               MOVE  SGTS014G-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTS014G-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SGTS014G-PSC-ERRO-PGM
                                       TO  MSG1-CD-SEQL-PGM-ACND
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'Alteração efetuada com sucesso.'
                                       TO  SGTSA087-TX-RTN-PRCT
           EXEC CICS  PUT
                      CONTAINER ( 'SGTSA087-RPST'         )
                      CHANNEL   ( NM-CNL                  )
                      FLENGTH   ( LENGTH OF SGTSA087-RPST )
                      FROM      ( SGTSA087-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
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

      *--> Adiciona Variáveis para Monitoração
           MOVE 'SGTSA087-CD-CTRA-TIT-VL'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSA087-CD-CTRA-TIT-VL         TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA087-CD-OPR-ETRD-MCP'       TO  MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSA087-CD-OPR-ETRD-MCP          NUMERIC
               MOVE  SGTSA087-CD-OPR-ETRD-MCP    TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX          TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA087-NR-VRS-OPR-ETRD'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSA087-NR-VRS-OPR-ETRD        TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA087-NR-SEQL-RCBT-PRVT'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSA087-NR-SEQL-RCBT-PRVT      TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA087-CD-TIP-RCBT-PRVT'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSA087-CD-TIP-RCBT-PRVT       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA087-DT-RCBT-PRVT'          TO  MSGKW999-NM-VRV-MNT
           MOVE  DT-RCBT-PRVT                    TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA087-CD-TIT-OPR-ETRD'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSA087-CD-TIT-OPR-ETRD        TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA087-NR-DVS-TIT'            TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSA087-NR-DVS-TIT             TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA087-TS-REG-PGTO'           TO  MSGKW999-NM-VRV-MNT
           MOVE  TS-REG-PGTO                     TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA087-IN-VCL-MNL'            TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSA087-IN-VCL-MNL             TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA087-CD-USU-VCL'            TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSA087-CD-USU-VCL             TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA087-TS-ULT-ATL'            TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSA087-TS-ULT-ATL             TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA087-VL-RLZD-VCLD'         TO  MSGKW999-NM-VRV-MNT
           IF  SGTSA087-VL-RLZD-VCLD             NUMERIC
               MOVE  SGTSA087-VL-RLZD-VCLD       TO  VL-15V02
               MOVE  VL-15V02                    TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava o erro gerado no container de erro
           EXEC CICS  PUT
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
      *------------------------- Fim SGTSA087 -------------------------*