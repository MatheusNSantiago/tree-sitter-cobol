      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSI071
      * ANALISTA..: F2054109 Cintia Ap Soares Iglesias
      * AUTOR.....: F6361711 Luis A.P. Valamede
      * OBJETIVO..: INCLUIR OPERAÇÃO PAGA POR OUTRA OPERAÇÃO
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 10/04/2017 F6361711 -  2015/137233 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSI071.

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
                                       VALUE '*** SGTSI071 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *--> Modulos Acionados
       77  SBTIMEDT                 PIC  X(08) VALUE 'SBTIMEDT'.
       77  SGTS014G                 PIC  X(08) VALUE 'SGTS014G'.

      *--> Book do programa de ambiente - CICS5000
      *   Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  CD-USU-ATL                  PIC  X(08) VALUE SPACES.

      *    Variáveis para gravação do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-OPR-PGDR-LOG    PIC 9(09) VALUE 0.
           03  CD-OPR-PGDR-LOG         PIC 9(11) VALUE 0.
           03  NR-VRS-OPR-PGDR-LOG     PIC 9(04) VALUE 0.
           03  CD-CTRA-OPR-VCLD-LOG    PIC 9(09) VALUE 0.
           03  CD-OPR-ETRD-VCLD-LOG    PIC 9(11) VALUE 0.
           03  NR-VRS-OPR-VCLD-LOG     PIC 9(04) VALUE 0.

      *--> Campos utilizados para obtenção de timestamp atual
       01  DADOS-SBTIMEDT.
           03  HLPKE001-CD-FUC         PIC X(03) VALUE 'F01'.
           03  HLPKE001-AREA-TRB.
               05  HLPKE001-AREA-TRB-TAM
                                       PIC S9(9) COMP VALUE 1024.
               05  HLPKE001-AREA-TRB-DADOS
                                       PIC  X(1024) VALUE ' '.
           03  HLPKE001-FMT-TS         PIC S9(4) COMP VALUE 1.
           03  HLPKE001-TAM-TS         PIC S9(4) COMP VALUE 26.
           03  HLPKE001-TS-FMTD.
               05  AA                  PIC  9(04) VALUE 0.
               05  FILLER              PIC  X(01) VALUE ' '.
               05  MM                  PIC  9(02) VALUE 0.
               05  FILLER              PIC  X(01) VALUE ' '.
               05  DD                  PIC  9(02) VALUE 0.
               05  FILLER              PIC  X(01) VALUE ' '.
               05  HH                  PIC  X(02) VALUE ' '.
               05  FILLER              PIC  X(01) VALUE ' '.
               05  MNTO                PIC  X(02) VALUE ' '.
               05  FILLER              PIC  X(01) VALUE ' '.
               05  SGDO                PIC  X(02) VALUE ' '.
               05  FILLER              PIC  X(01) VALUE ' '.
               05  MSGD                PIC  X(06) VALUE ' '.

       01  TS-SQL-ATU                  PIC  X(26)
           VALUE '0000-00-00-00.00.00.000000'.
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

      * Quantidade de registros obtidos na consulta
       01  QT-REG-SELD                 PIC S9(4) COMP-5 VALUE 0.

       01  VL-PGTO.
           03  VL-PGTO-DISPLAY          PIC  ZZZZZZZZZZZZZZ9,9(2).

      * BOOK DOS DADOS DO LOG
       01  DADOS-LOG.
-INC SGTK014G


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
       01  SGTSI071-DADOS.
-INC SGTKI071

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
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  110000-DESCOBRIR-CANAL

           PERFORM  120000-EXCLUIR-CTNR-ERRO

           PERFORM  130000-EXCLUIR-CTNR-RPST

           PERFORM  140000-RECEBER-REQUISICAO

           PERFORM  150000-VALIDAR-PARAMETRO

           PERFORM  160000-LER-DADOS-OPRD

           PERFORM  170000-BUSCAR-TS-DT

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS  ASSIGN
                      CHANNEL ( NM-CNL )
           END-EXEC

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
       130000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'SGTSI071-RPST' )
                      CHANNEL   ( NM-CNL          )
                      RESP      ( MSG1-RESP1      )
                      RESP2     ( MSG1-RESP2      )
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
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-CCT-LIDO
           MOVE  LENGTH OF SGTSI071-RQSC
                                       TO  QT-CCT-EPRO

           EXEC CICS  GET
                      CONTAINER ( 'SGTSI071-RQSC'  )
                      CHANNEL   ( NM-CNL           )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO      )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS  GET
                      CONTAINER ( 'SGTSI071-RQSC'         )
                      CHANNEL   ( NM-CNL                  )
                      FLENGTH   ( LENGTH OF SGTSI071-RQSC )
                      INTO      ( SGTSI071-RQSC           )
           END-EXEC

           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  151000-VALIDAR-CAMPOS

           PERFORM  152000-VALIDAR-OPR-PAGADORA

           PERFORM  153000-VALIDAR-OPR-VINCULADA

           .
       150000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       151000-VALIDAR-CAMPOS           SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Operação Pagadora
           IF  SGTSI071-CD-CTRA-OPR-PGDR   EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0496              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira da operação pagadora não inform
      -              'ado.'            TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI071-CD-CTRA-OPR-PGDR   NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0497              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira da operação pagadora inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI071-CD-OPR-PGDR        NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0498              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada pagadora não numéri
      -              'co.'             TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI071-CD-OPR-PGDR        EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0499              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada pagadora não inform
      -              'ado.'            TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI071-CD-OPR-PGDR        NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0500              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada pagadora inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI071-NR-VRS-OPR-PGDR    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0501              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação pagadora não informad
      -              'o.'              TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI071-NR-VRS-OPR-PGDR    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0502              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação pagadora inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Valida a Operação Vinculada
           IF  SGTSI071-CD-CTRA-OPR-VCLD   EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0503              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira da operação vinculada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI071-CD-CTRA-OPR-VCLD   NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0504              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira da operação vinculada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI071-CD-OPR-ETRD-VCLD   NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0505              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada vinculada não numér
      -              'ico.'            TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI071-CD-OPR-ETRD-VCLD   EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0506              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada vinculada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI071-CD-OPR-ETRD-VCLD   NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0507              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada vinculada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI071-NR-VRS-OPR-VCLD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0508              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação vinculada não informa
      -              'do.'             TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI071-NR-VRS-OPR-VCLD    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0509              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação vinculada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Valida o Timestamp de Atualização
           IF  SGTSI071-TS-ULT-ATL         EQUAL  SPACES
                                           OR  EQUAL
                                           '0000-00-00-00.00.00.000000'
                                           OR  EQUAL
                                           '00000000000000000000000000'
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0277              TO  MSG1-CD-ERRO-TS
               MOVE  'Timestamp última atualização inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Valida o Valor do Pagamento
           IF  SGTSI071-VL-PGTO            NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0532              TO  MSG1-CD-ERRO-TS
               MOVE  'Valor do pagamento não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI071-VL-PGTO            NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0510              TO  MSG1-CD-ERRO-TS
               MOVE  'Valor do pagamento inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       151000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       152000-VALIDAR-OPR-PAGADORA     SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-REG-SELD

           EXEC SQL
                SELECT 1
                  INTO :QT-REG-SELD
                  FROM DB2SGT.OPR_ETRD_MCP
                 WHERE CD_CTRA_TIT_VL   =  :SGTSI071-CD-CTRA-OPR-PGDR
                   AND CD_OPR_ETRD_MCP  =  :SGTSI071-CD-OPR-PGDR
                   AND NR_VRS_OPR_ETRD  =  :SGTSI071-NR-VRS-OPR-PGDR
                        FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG-SELD                 EQUAL  ZEROS
               MOVE  0689              TO  MSG1-CD-ERRO-TS
               MOVE  'Operação estruturada pagadora inexistente.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       152000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       153000-VALIDAR-OPR-VINCULADA    SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO  QT-REG-SELD

           EXEC SQL
                SELECT 1
                  INTO :QT-REG-SELD
                  FROM DB2SGT.OPR_ETRD_MCP
                 WHERE CD_CTRA_TIT_VL   =  :SGTSI071-CD-CTRA-OPR-VCLD
                   AND CD_OPR_ETRD_MCP  =  :SGTSI071-CD-OPR-ETRD-VCLD
                   AND NR_VRS_OPR_ETRD  =  :SGTSI071-NR-VRS-OPR-VCLD
                 FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG-SELD                 EQUAL  ZEROS
               MOVE  0690              TO  MSG1-CD-ERRO-TS
               MOVE  'Operação estruturada vinculada inexistente.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       153000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       160000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

           MOVE  'PRM-INF-OPRD'        TO  NM-CTNR

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           INITIALIZE                      PRM-INF-OPRD

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                      )
                      INTO      ( PRM-INF-OPRD                         )
                      FLENGTH   ( LENGTH OF PRM-INF-OPRD               )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE

           IF  EXECUCAO-ONLINE
               MOVE  KOPRD-CD-OPRD     TO  CD-USU-ATL
           ELSE
               MOVE  'SISTEMA'         TO  CD-USU-ATL
           END-IF

           .
       160000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170000-BUSCAR-TS-DT             SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           CALL  SBTIMEDT           USING  HLPKE001-CD-FUC
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
               MOVE  SBTIMEDT          TO  MSG1-NM-PGM-ACND
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  HLPKE001-TS-FMTD      TO  TS-SQL-ATU

           IF  SGTSI071-TS-ULT-ATL         EQUAL  'CURRENT'
               MOVE  TS-SQL-ATU        TO  SGTSI071-TS-ULT-ATL
           END-IF

           .
       170000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-INCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

      *--> Desligar erro automático do DB2
           EXEC SQL
                WHENEVER SQLERROR CONTINUE
           END-EXEC

           EXEC SQL
                INSERT INTO DB2SGT.OPR_PG_OTR_OPR
                           ( CD_CTRA_OPR_PGDR
                           , CD_OPR_PGDR
                           , NR_VRS_OPR_PGDR
                           , CD_CTRA_OPR_VCLD
                           , CD_OPR_ETRD_VCLD
                           , NR_VRS_OPR_VCLD
                           , CD_USU_ULT_ATL
                           , TS_ULT_ATL
                           , VL_PGTO
                           )
                VALUES
                           ( :SGTSI071-CD-CTRA-OPR-PGDR
                           , :SGTSI071-CD-OPR-PGDR
                           , :SGTSI071-NR-VRS-OPR-PGDR
                           , :SGTSI071-CD-CTRA-OPR-VCLD
                           , :SGTSI071-CD-OPR-ETRD-VCLD
                           , :SGTSI071-NR-VRS-OPR-VCLD
                           , :CD-USU-ATL
                           , :SGTSI071-TS-ULT-ATL
                           , :SGTSI071-VL-PGTO
                           )
           END-EXEC

      *--> Ligar Erro Automatico do DB2
           EXEC SQL WHENEVER SQLERROR
                GO TO  999010-TRATA-ERRO-DB2
           END-EXEC

           EVALUATE  SQLCODE
               WHEN  -803
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  0803        TO  MSG1-CD-ERRO-TS
                     MOVE  'Inclusão não efetuada, registro existente.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               WHEN  ZEROS
                     CONTINUE
               WHEN  OTHER
      *--> Aciona o Tratamento de Erro Padrao para os demais casos
                     PERFORM  999010-TRATA-ERRO-DB2
           END-EVALUATE

           PERFORM  210000-GRAVAR-LOG
               .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-GRAVAR-LOG               SECTION.
      *-----------------------------------------------------------------
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      DADOS-LOG

           SET   SGTS014G-INCLUSAO     TO  TRUE

           MOVE  'OPR_PG_OTR_OPR'      TO  SGTS014G-NM-ENT
           MOVE  CD-USU-ATL            TO  SGTS014G-CD-CHV-USU
           MOVE  'SGTSI071'            TO  SGTS014G-NM-PGM

           MOVE  SGTSI071-CD-CTRA-OPR-PGDR
                                       TO  CD-CTRA-OPR-PGDR-LOG
           MOVE  SGTSI071-CD-OPR-PGDR
                                       TO  CD-OPR-PGDR-LOG
           MOVE  SGTSI071-NR-VRS-OPR-PGDR
                                       TO  NR-VRS-OPR-PGDR-LOG
           MOVE  SGTSI071-CD-CTRA-OPR-VCLD
                                       TO  CD-CTRA-OPR-VCLD-LOG
           MOVE  SGTSI071-CD-OPR-ETRD-VCLD
                                       TO  CD-OPR-ETRD-VCLD-LOG
           MOVE  SGTSI071-NR-VRS-OPR-VCLD
                                       TO  NR-VRS-OPR-VCLD-LOG

           STRING  CD-CTRA-OPR-PGDR-LOG
                   ';'  CD-OPR-PGDR-LOG
                   ';'  NR-VRS-OPR-PGDR-LOG
                   ';'  CD-CTRA-OPR-VCLD-LOG
                   ';'  CD-OPR-ETRD-VCLD-LOG
                   ';'  NR-VRS-OPR-VCLD-LOG
                   DELIMITED BY SIZE
                   INTO                    SGTS014G-CHV-PRMR


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
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'Inclusão efetuada com sucesso.'
                                       TO  SGTSI071-TX-RTN-PRCT
           EXEC CICS PUT
                     CONTAINER ( 'SGTSI071-RPST'         )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSI071-RPST )
                     FROM      ( SGTSI071-RPST           )
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
               MOVE 'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *Verifica se o tipo de erro gerado possui tratamento interno
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

      *--> Adiciona Variáveis para Monitoração
           MOVE  'I071-001-CD-CTRA-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI071-CD-CTRA-OPR-PGDR
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'I071-002-CD-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI071-CD-OPR-PGDR
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'I071-003-NR-VRS-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI071-NR-VRS-OPR-PGDR
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'I071-004-CD-CTRA-OPR-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI071-CD-CTRA-OPR-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'I071-005-CD-OPR-ETRD-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI071-CD-OPR-ETRD-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'I071-006-NR-VRS-OPR-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI071-NR-VRS-OPR-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'I071-007-CD-USU-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  CD-USU-ATL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'I071-008-TS-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI071-TS-ULT-ATL   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'I071-009-VL-PGTO'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI071-VL-PGTO      TO  VL-PGTO-DISPLAY
           MOVE  VL-PGTO               TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava as Variaveis Monitoradas no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava a Ocorrencia de Erro no Container de Erro
           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                  )
                      CHANNEL   ( NM-CNL                      )
                      FROM      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      NOHANDLE
           END-EXEC

      *--> Encerra a Execução do Módulo
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

      *-----------------------------------------------------------------
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.

      *---------------------- FIM SGTSI071 -----------------------------