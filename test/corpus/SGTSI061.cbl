      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSI061
      * ANALISTA..: F4535118 - IVONE DE MELO COSTA RIBEIRO
      * AUTOR.....: F4535118 - IVONE DE MELO COSTA RIBEIRO
      * OBJETIVO..: INCLUSÃO CONTAINER NA TABELA RPAC_PGMD_OPR
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS002 28/01/2019 F9188847 DE 2016/145195 - Inclusão monitoração
      *          variáveis para rotina SBDATAF
      * VRS001 05/02/2018 F9158937 - Acerto mensageria
      *        02/02/2017 F8785286 DE 2016/145195 - Inclusão leitura
      *              container operador para gravação no LOG.
      *        27.10.2016 - F4535118 DE 2015/137233 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSI061.

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
               '*** Storage Section começa aqui ***'.
       77  CTE-PROG                    PIC  X(17)
                                       VALUE '*** SGTSI061 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS002'.

       77  SBDATAF                     PIC  X(08) VALUE 'SBDATAF'.
       77  SGTS014G                    PIC  X(08) VALUE 'SGTS014G'.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE 0.
       01  CD-USU-ATL                  PIC  X(08) VALUE SPACES.
       01  TS-ATL                      PIC  X(26) VALUE SPACES.
       01  DT-SQL-ATU                  PIC  X(10) VALUE '  .  .    '.
       01  DT-SQL-ATU-R                REDEFINES DT-SQL-ATU.
           03 DIA                      PIC  9(02).
           03 FILLER                   PIC  X(01).
           03 MES                      PIC  9(02).
           03 FILLER                   PIC  X(01).
           03 ANO                      PIC  9(04).

      *--> Indicadores de nulidades
       01  QT-REG-SELD-N               PIC S9(04) COMP-5 VALUE 0.
       01  TX-MTV-RPAC-PGMD-N          PIC S9(04) COMP-5 VALUE 0.

      *--> Subrotina SBDATAF
      *    Valida no formato Gregoriano
       01  FUC                         PIC  X(03) VALUE 'F01'.
       01  ARG01                       PIC  9(08) VALUE 0.
       01  ARG01-R REDEFINES ARG01.
         03  DIA                       PIC  9(02).
         03  MES                       PIC  9(02).
         03  ANO                       PIC  9(04).
       01  ARG02                       PIC  9(01) VALUE 4.
       01  AREA-WORK                   PIC  X(4095).
       01  AREA-WORK-R REDEFINES AREA-WORK.
           03  TAM                     PIC  9(08) COMP.
           03  FILLER                  PIC  X(4071).
           03  FIM                     PIC  X(20).

      *--> Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG      PIC  9(10) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG     PIC  9(11) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG     PIC  9(05) VALUE 0.
           03  DT-RPAC-PGMD-LOG        PIC  X(10) VALUE ' '.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book dos Container do Operador
-INC IIBKOPRD

      *--> Book trata data e hora atual
-INC HLPKTIME

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
-INC SGTKI061

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       77  CTE-FINAL-SS                PIC  X(40) VALUE
               '*** Storage Section termina aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

           IF  NOT EXECUCAO-ONLINE  AND
               NOT EXECUCAO-BATCH
               CALL  CICS5000       USING  CICS5000-COMMAREA
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

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO  TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
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
           MOVE  LENGTH OF SGTSI061-RQSC
                                       TO  QT-CCT-EPRO
           EXEC CICS GET
                     CONTAINER ( 'SGTSI061-RQSC' )
                     CHANNEL   ( NM-CNL          )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
                     CONTAINER ( 'SGTSI061-RQSC'         )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSI061-RQSC )
                     INTO      ( SGTSI061-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------

           PERFORM  141000-VALIDAR-CAMPOS

           PERFORM  142000-VALIDAR-OPR-ETRD-MCP

           IF  SGTSI061-DT-RPAC-PGMD       EQUAL  'CURRENT'
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
           IF  SGTSI061-CD-CTRA-TIT-VL     EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores monetários
      -              'não informado.'  TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI061-CD-CTRA-TIT-VL     NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores monetários
      -              'inválido.'       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI061-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI061-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'    TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI061-CD-OPR-ETRD-MCP    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais in
      -              'válido.'         TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI061-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI061-NR-VRS-OPR-ETRD    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão operação estruturado inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Valida Outros Campos de Entrada
           IF  SGTSI061-DT-RPAC-PGMD       EQUAL SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0152              TO  MSG1-CD-ERRO-TS
               MOVE  'Data repactuação programada não informada.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI061-TS-ULT-ATL         EQUAL SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0106              TO  MSG1-CD-ERRO-TS
               MOVE  'Timestamp última atualização não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
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
                   :QT-REG-SELD-N
               FROM DB2SGT.OPR_ETRD_MCP
               WHERE CD_CTRA_TIT_VL     =  :SGTSI061-CD-CTRA-TIT-VL
                 AND CD_OPR_ETRD_MCP    =  :SGTSI061-CD-OPR-ETRD-MCP
                 AND NR_VRS_OPR_ETRD    =  :SGTSI061-NR-VRS-OPR-ETRD
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

           MOVE  4095                  TO  TAM
           MOVE  'SBDATAF -WRKAREA-FIM'
                                       TO  FIM
           MOVE  CORR SGTSI061-DT-RPAC-PGMD-R
                                       TO  ARG01-R

           CALL  SBDATAF            USING  FUC
                                           AREA-WORK
                                           ARG01
                                           ARG02

           IF  RETURN-CODE           NOT EQUAL ZEROS AND  0024
               SET  ERRO-PROGRAMA-ACIONADO    TO TRUE
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE 0051               TO MSG1-CD-ERRO-TS
               MOVE 'Erro no tratamento de datas.'
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS

               MOVE 'FUC'               TO  MSGKW999-NM-VRV-MNT
               MOVE  FUC                TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'TAM'              TO MSGKW999-NM-VRV-MNT
               MOVE  TAM               TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'FIM'              TO MSGKW999-NM-VRV-MNT
               MOVE  FIM               TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG01'           TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG01            TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG02'           TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG02            TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  RETURN-CODE           EQUAL   0024
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0153              TO  MSG1-CD-ERRO-TS
               MOVE  'Data de repactuação programada inválida.'
                                       TO  MSG1-TX-ERRO-TS

               MOVE 'SBDATAF'           TO  MSGKW999-NM-VRV-MNT
               MOVE  SBDATAF            TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'RETURN-CODE'       TO  MSGKW999-NM-VRV-MNT
               MOVE  RETURN-CODE        TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'FUC'               TO  MSGKW999-NM-VRV-MNT
               MOVE  FUC                TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'TAM'              TO MSGKW999-NM-VRV-MNT
               MOVE  TAM               TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'FIM'              TO MSGKW999-NM-VRV-MNT
               MOVE  FIM               TO MSGKW999-VL-VRV-MNT
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

           IF  RETURN-CODE                 NOT EQUAL  ZEROS
              SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  0050              TO  MSG1-CD-ERRO-TS
               MOVE  'Erro na rotina para obter data e hora atual.'
                                       TO  MSG1-TX-ERRO-TS
               MOVE  SBTIMEDT          TO  MSG1-NM-PGM-ACND
               MOVE  RETURN-CODE
                                       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  HLPKE001-TS-FMTD      TO  TS-ATL
           IF  SGTSI061-TS-ULT-ATL         EQUAL  'CURRENT'
               MOVE  TS-ATL            TO  SGTSI061-TS-ULT-ATL
           END-IF

           MOVE  CORR HLPKE001-TS-FMTD TO  DT-SQL-ATU-R
           IF  SGTSI061-DT-RPAC-PGMD       EQUAL  'CURRENT'
               MOVE  DT-SQL-ATU        TO  SGTSI061-DT-RPAC-PGMD
           END-IF

           .
       160000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170000-TRATAR-NULIDADE          SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSI061-TX-MTV-RPAC-PGMD   EQUAL SPACES
               MOVE  -0001             TO  TX-MTV-RPAC-PGMD-N
           END-IF

           .
       170000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-INCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

      *--> Desligar erro automático do DB2
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC

           EXEC SQL
                INSERT INTO DB2SGT.RPAC_PGMD_OPR
                ( CD_CTRA_TIT_VL
                , CD_OPR_ETRD_MCP
                , NR_VRS_OPR_ETRD
                , DT_RPAC_PGMD
                , CD_USU_ULT_ATL
                , TS_ULT_ATL
                , TX_MTV_RPAC_PGMD )
                VALUES
                ( :SGTSI061-CD-CTRA-TIT-VL
                , :SGTSI061-CD-OPR-ETRD-MCP
                , :SGTSI061-NR-VRS-OPR-ETRD
                , :SGTSI061-DT-RPAC-PGMD
                , :SGTSI061-CD-USU-ULT-ATL
                , :SGTSI061-TS-ULT-ATL
                , :SGTSI061-TX-MTV-RPAC-PGMD
                  :TX-MTV-RPAC-PGMD-N )
           END-EXEC

      *--> Ligar erro automatico do DB2
           EXEC SQL WHENEVER SQLERROR
                    GO TO  999010-TRATA-ERRO-DB2
           END-EXEC

           EVALUATE  SQLCODE
               WHEN  -803
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  803        TO  MSG1-CD-ERRO-TS
                     MOVE  'Inclusão não efetuada, registro existente.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM 999000-GRAVA-ERRO-E-ENCERRA

               WHEN  ZEROS
                     CONTINUE

               WHEN  OTHER
      *--> Aciona o tratamento de erro padrao para os demais casos
                     PERFORM  999010-TRATA-ERRO-DB2
           END-EVALUATE

           PERFORM  981000-GRAVAR-LOG
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'Inclusão efetuada com sucesso.'
                                       TO  SGTSI061-TX-RTN-PRCT

           EXEC CICS PUT
                     CONTAINER ( 'SGTSI061-RPST'         )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSI061-RPST )
                     FROM      ( SGTSI061-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       981000-GRAVAR-LOG               SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      DADOS-LOG

           SET   SGTS014G-INCLUSAO     TO  TRUE
           MOVE  'RPAC_PGMD_OPR     '  TO  SGTS014G-NM-ENT
           MOVE  CD-USU-ATL            TO  SGTS014G-CD-CHV-USU
           MOVE  'SGTSI061'            TO  SGTS014G-NM-PGM

           MOVE  SGTSI061-CD-CTRA-TIT-VL
                                       TO  CD-CTRA-TIT-VL-LOG
           MOVE  SGTSI061-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP-LOG
           MOVE  SGTSI061-NR-VRS-OPR-ETRD
                                       TO  NR-VRS-OPR-ETRD-LOG
           MOVE  SGTSI061-DT-RPAC-PGMD
                                       TO  DT-RPAC-PGMD-LOG

           STRING  CD-CTRA-TIT-VL-LOG
                   ';'    CD-OPR-ETRD-MCP-LOG
                   ';'    NR-VRS-OPR-ETRD-LOG
                   ';'    DT-RPAC-PGMD-LOG
                   DELIMITED BY SIZE
                   INTO                    SGTS014G-CHV-PRMR

           MOVE  LENGTH OF DADOS-LOG   TO  EIBCALEN

           CALL  SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM         NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  112               TO  MSG1-CD-ERRO-TS
               MOVE 'Erro na gravação das informações do Log.'
                                       TO                MSG1-TX-ERRO-TS
               MOVE  SGTS014G          TO  MSG1-NM-PGM-ACND
               MOVE  SGTS014G-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTS014G-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       981000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE  'Erro não especificado no módulo.'
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


           MOVE  'SGTSI061-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI061-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI061-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTSI061-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE  SGTSI061-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI061-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI061-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI061-DT-RPAC-PGMD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI061-DT-RPAC-PGMD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI061-CD-USU-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI061-CD-USU-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI061-TS-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI061-TS-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI061-TX-MTV-RPAC-PGMD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI061-TX-MTV-RPAC-PGMD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava o Erro no MSG
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                           )
                      CHANNEL   ( NM-CNL                               )
                      FROM      ( IIBKERRO-PRM-ERRO                    )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO          )
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

      *---------------------- FIM SGTSI061 -----------------------------