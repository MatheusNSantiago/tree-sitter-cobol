      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSI068
      * ANALISTA..: F0175702 - ADRIANO SEIJI NAKAMURA
      * AUTOR.....: F0175702 - ADRIANO SEIJI NAKAMURA
      * OBJETIVO..: INCLUSAO CONTAINER DA TABELA  PREF_TAXA_OPR
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS002 28/01/2019 F9188847 DE 2016/145195 - Inclusão monitoração
      *          variáveis para rotina SBDATAF
      * VRS001 05/02/2018 - F9158937 - Acerto mensageria
      *        24.08.2017 - F9188847 - Alteração nome Tabela:
      *                                 De: CND_MIN_TAXA_OPR
      *                                 Para: PREF_TAXA_OPR
      *               Inclusão campos:  CD_TIP_APL_PREF
      *                               , CD_PERC_APL_PREF
      *                               , CD_IEC_PREF
      *        03/02/2017 F8785286 DE 2016/145195 - Inclusão leitura
      *              container operador para gravação no LOG.
      *        13.12.2016 - F0175702 DE 2015/137233 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSI068.

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
                                       VALUE '*** SGTSI068 ***'.
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
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE ZEROS.
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
       01  PC-MIN-TAXA-N               PIC S9(4) COMP-5 VALUE 0.

      *--> Subrotina SBDATAF
      *    Campos utilizados para validação de data Gregoriana.
       01  FUC                         PIC  X(03) VALUE 'F01'.
       01  AREA-WORK                   PIC  X(4095).
       01  AREA-WORK-R                 REDEFINES AREA-WORK.
           03  TAM                     PIC  9(08) COMP.
           03  FILLER                  PIC  X(4071).
           03  FIM                     PIC  X(20).
       01  ARG01                       PIC  9(08) VALUE 0.
       01  ARG01-R                     REDEFINES ARG01.
           03  DIA                     PIC  9(02).
           03  MES                     PIC  9(02).
           03  ANO                     PIC  9(04).
      *--> Valida no formato Gregoriano
       01  ARG02                       PIC  9(01) VALUE 4.

      *--> Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG      PIC  9(10) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG     PIC  9(11) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG     PIC  9(5) VALUE 0.
           03  CD-TIP-TAXA-LOG         PIC  9(5) VALUE 0.
           03  DT-INC-CND-TAXA-LOG     PIC  X(10) VALUE ' '.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).
       01  PC-MIN-TAXA-TX.
           03  PC-MIN-TAXA             PIC  ZZZ9,9(10).


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
-INC SGTKI068

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
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

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
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

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
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

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
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-CCT-LIDO
           MOVE  LENGTH OF SGTSI068-RQSC
                                       TO  QT-CCT-EPRO

           EXEC CICS GET
                     CONTAINER ( 'SGTSI068-RQSC' )
                     CHANNEL   ( NM-CNL          )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-PROGRAMADO   TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
                     CONTAINER ( 'SGTSI068-RQSC'         )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSI068-RQSC )
                     INTO      ( SGTSI068-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  141000-VALIDAR-CAMPOS

           PERFORM  142000-VALIDAR-DATAS

           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       141000-VALIDAR-CAMPOS           SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada
           IF  SGTSI068-CD-CTRA-TIT-VL     EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores monetários
      -              'não informado.'  TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI068-CD-CTRA-TIT-VL     NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores monetários
      -              'inválido.'       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI068-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI068-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'    TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI068-CD-OPR-ETRD-MCP    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais in
      -              'válido.'         TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI068-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI068-NR-VRS-OPR-ETRD    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão operação estruturado inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Valida Outros Campos de Entrada
           IF  SGTSI068-TS-ULT-ATL         EQUAL  SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0106              TO  MSG1-CD-ERRO-TS
               MOVE  'Timestamp última atualização não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI068-CD-TIP-TAXA        EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0166              TO  MSG1-CD-ERRO-TS
               MOVE  'Codigo tipo de taxa não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI068-CD-TIP-TAXA        NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0167              TO  MSG1-CD-ERRO-TS
               MOVE  'Código tipo de taxa inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI068-DT-INC-CND-TAXA    EQUAL SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0168              TO  MSG1-CD-ERRO-TS
               MOVE  'Data início da condição da taxa não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI068-DT-FIM-CND-TAXA    EQUAL SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0170              TO  MSG1-CD-ERRO-TS
               MOVE  'Data fim da condição da taxa não informada.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI068-PC-MIN-TAXA        NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0172              TO  MSG1-CD-ERRO-TS
               MOVE  'Percentual mínimo da taxa não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI068-PC-MIN-TAXA        LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0174              TO  MSG1-CD-ERRO-TS
               MOVE  'Percentual mínimo da taxa inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validações para o Código do Tipo de Taxa da Operação:
      *    0001 - Juros
           IF SGTSI068-CD-TIP-TAXA         EQUAL  0001
              IF  SGTSI068-CD-TIP-APL-PREF
                                           EQUAL  ZEROS
                  SET   ERRO-PROGRAMADO
                                       TO  TRUE
                  MOVE  0175           TO  MSG1-CD-ERRO-TS
                  MOVE  'Código tipo forma de aplicação não informado.'
                                       TO  MSG1-TX-ERRO-TS
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-IF
              IF  SGTSI068-CD-TIP-APL-PREF
                                           NOT POSITIVE
                  SET   ERRO-PROGRAMADO
                                       TO  TRUE
                  MOVE  0176           TO  MSG1-CD-ERRO-TS
                  MOVE  'Código tipo forma de aplicação inválido.'
                                       TO  MSG1-TX-ERRO-TS
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-IF

              IF  SGTSI068-CD-PERC-APL-PREF
                                           EQUAL  ZEROS
                  SET   ERRO-PROGRAMADO
                                       TO  TRUE
                  MOVE  0177           TO  MSG1-CD-ERRO-TS
                  MOVE  'Código da periodicidade da aplicação não inform
      -                 'ado.'         TO  MSG1-TX-ERRO-TS
                  PERFORM  999000-GRAVA-ERRO-E-ENCERRA
              END-IF
              IF  SGTSI068-CD-PERC-APL-PREF
                                           NOT POSITIVE
                  SET   ERRO-PROGRAMADO
                                       TO  TRUE
                  MOVE  0178           TO  MSG1-CD-ERRO-TS
                  MOVE  'Código da periodicidade da aplicação inválido.'
                                       TO  MSG1-TX-ERRO-TS
                  PERFORM  999000-GRAVA-ERRO-E-ENCERRA
              END-IF

              IF  SGTSI068-CD-IEC-PREF     EQUAL  ZEROS
                  SET   ERRO-PROGRAMADO
                                       TO  TRUE
                  MOVE  0179           TO  MSG1-CD-ERRO-TS
                  MOVE  'Código do indicador econômico não informado.'
                                       TO  MSG1-TX-ERRO-TS
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-IF
              IF  SGTSI068-CD-IEC-PREF     NOT POSITIVE
                  SET   ERRO-PROGRAMADO
                                       TO  TRUE
                  MOVE  0180           TO  MSG1-CD-ERRO-TS
                  MOVE  'Código do indicador econômico inválido.'
                                       TO  MSG1-TX-ERRO-TS
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-IF
           END-IF
           .
       141000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       142000-VALIDAR-DATAS            SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSI068-DT-INC-CND-TAXA    EQUAL 'CURRENT'
               CONTINUE
           ELSE
               MOVE  CORR SGTSI068-DT-INC-CND-TAXA-R
                                       TO  ARG01-R

               PERFORM  910000-VALIDAR-DATA

               IF  RETURN-CODE         NOT EQUAL ZEROS AND 24
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
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0169          TO  MSG1-CD-ERRO-TS
                   MOVE  'Data início da condição de taxa inválida.'
                                       TO  MSG1-TX-ERRO-TS

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
           END-IF

           IF  SGTSI068-DT-FIM-CND-TAXA    EQUAL 'CURRENT'
               CONTINUE
           ELSE
               MOVE  CORR SGTSI068-DT-FIM-CND-TAXA-R
                                       TO  ARG01-R
               PERFORM  910000-VALIDAR-DATA
               IF  RETURN-CODE         NOT EQUAL ZEROS AND 24
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
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0171          TO  MSG1-CD-ERRO-TS
                   MOVE  'Data fim da condição de taxa inválida.'
                                       TO  MSG1-TX-ERRO-TS
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
           END-IF

           .
       142000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

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
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           CALL  SBTIMEDT           USING  HLPKE001-CD-FUC
                                           HLPKE001-AREA-TRB
                                           HLPKE001-FMT-TS
                                           HLPKE001-TAM-TS
                                           HLPKE001-TS-FMTD

           IF  RETURN-CODE                 NOT EQUAL  ZEROS
               SET  ERRO-PROGRAMA-ACIONADO TO  TRUE
               MOVE  0050                  TO  MSG1-CD-ERRO-TS
               MOVE  'Erro na rotina para obter data e hora atual.'
                                           TO  MSG1-TX-ERRO-TS
               MOVE  SBTIMEDT              TO  MSG1-NM-PGM-ACND
               MOVE  RETURN-CODE           TO  MSG1-CD-RTN-PGM-ACND
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  HLPKE001-TS-FMTD      TO  TS-ATL
           IF  SGTSI068-TS-ULT-ATL         EQUAL  'CURRENT'
               MOVE  TS-ATL            TO  SGTSI068-TS-ULT-ATL
           END-IF

           MOVE  CORR HLPKE001-TS-FMTD
                                       TO  DT-SQL-ATU-R

           IF  SGTSI068-DT-INC-CND-TAXA    EQUAL  'CURRENT'
               MOVE  DT-SQL-ATU        TO  SGTSI068-DT-INC-CND-TAXA
           END-IF

           IF  SGTSI068-DT-FIM-CND-TAXA    EQUAL  'CURRENT'
               MOVE  DT-SQL-ATU        TO  SGTSI068-DT-FIM-CND-TAXA
           END-IF

           .
       160000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170000-TRATAR-NULIDADE          SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSI068-PC-MIN-TAXA        EQUAL  ZEROS
               MOVE -001               TO  PC-MIN-TAXA-N
           END-IF

           .
       170000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-INCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

      *--> Desligar erro automatico do DB2
           EXEC SQL WHENEVER SQLERROR
                    CONTINUE
           END-EXEC

           EXEC SQL
                INSERT INTO DB2SGT.PREF_TAXA_OPR
                ( CD_CTRA_TIT_VL
                , CD_OPR_ETRD_MCP
                , NR_VRS_OPR_ETRD
                , CD_TIP_TAXA
                , DT_INC_CND_TAXA
                , CD_USU_ULT_ATL
                , TS_ULT_ATL
                , DT_FIM_CND_TAXA
                , PC_MIN_TAXA
                , CD_TIP_APL_PREF
                , CD_PERC_APL_PREF
                , CD_IEC_PREF)
                VALUES
                ( :SGTSI068-CD-CTRA-TIT-VL
                , :SGTSI068-CD-OPR-ETRD-MCP
                , :SGTSI068-NR-VRS-OPR-ETRD
                , :SGTSI068-CD-TIP-TAXA
                , :SGTSI068-DT-INC-CND-TAXA
                , :CD-USU-ATL
                , :SGTSI068-TS-ULT-ATL
                , :SGTSI068-DT-FIM-CND-TAXA
                , :SGTSI068-PC-MIN-TAXA
                  :PC-MIN-TAXA-N
                , :SGTSI068-CD-TIP-APL-PREF
                , :SGTSI068-CD-PERC-APL-PREF
                , :SGTSI068-CD-IEC-PREF
                )
           END-EXEC

      *--> Ligar erro automático do DB2
           EXEC SQL WHENEVER SQLERROR
                GO TO 999010-TRATA-ERRO-DB2
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
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'Inclusão efetuada com sucesso.'
                                       TO  SGTSI068-TX-RTN-PRCT

           EXEC CICS PUT
                     CONTAINER ( 'SGTSI068-RPST'         )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSI068-RPST )
                     FROM      ( SGTSI068-RPST           )
           END-EXEC

           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       910000-VALIDAR-DATA             SECTION.
      *-----------------------------------------------------------------
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

           MOVE  4095                  TO  TAM
           MOVE  'SBDATAF -WRKAREA-FIM'
                                       TO  FIM
           CALL SBDATAF            USING   FUC
                                           AREA-WORK
                                           ARG01
                                           ARG02

           .
       910000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       981000-GRAVAR-LOG               SECTION.
      *-----------------------------------------------------------------
           MOVE  0015
           TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      DADOS-LOG

           SET   SGTS014G-INCLUSAO     TO  TRUE
           MOVE  'PREF_TAXA_OPR  '     TO  SGTS014G-NM-ENT
           MOVE  CD-USU-ATL            TO  SGTS014G-CD-CHV-USU
           MOVE  'SGTSI068'            TO  SGTS014G-NM-PGM

           MOVE  SGTSI068-CD-CTRA-TIT-VL
                                       TO  CD-CTRA-TIT-VL-LOG
           MOVE  SGTSI068-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP-LOG
           MOVE  SGTSI068-NR-VRS-OPR-ETRD
                                       TO  NR-VRS-OPR-ETRD-LOG
           MOVE  SGTSI068-CD-TIP-TAXA
                                       TO  CD-TIP-TAXA-LOG
           MOVE  SGTSI068-DT-INC-CND-TAXA
                                       TO  DT-INC-CND-TAXA-LOG

           STRING  CD-CTRA-TIT-VL-LOG
                   ';'    CD-OPR-ETRD-MCP-LOG
                   ';'    NR-VRS-OPR-ETRD-LOG
                   ';'    CD-TIP-TAXA-LOG
                   ';'    DT-INC-CND-TAXA-LOG
                   DELIMITED BY SIZE
                   INTO                    SGTS014G-CHV-PRMR

           MOVE  LENGTH OF DADOS-LOG   TO  EIBCALEN

           CALL  SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM         NOT EQUAL  ZEROS
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
       981000-SAI.
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

      *--> Adiciona Chave da Operação Estruturada
           MOVE  'SGTSI068-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI068-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI068-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSI068-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE  SGTSI068-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI068-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI068-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *--> Adiciona Outros Campos de Entrada
           MOVE  'SGTSI068-CD-TIP-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI068-CD-TIP-TAXA
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI068-DT-INC-CND-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI068-DT-INC-CND-TAXA
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI068-CD-USU-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI068-CD-USU-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI068-TS-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI068-TS-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI068-DT-FIM-CND-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI068-DT-FIM-CND-TAXA
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI068-PC-MIN-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSI068-PC-MIN-TAXA    IS NUMERIC
               MOVE  SGTSI068-PC-MIN-TAXA
                                       TO  PC-MIN-TAXA
               MOVE  PC-MIN-TAXA-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI068-CD-TIP-APL-PREF'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI068-CD-TIP-APL-PREF
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI068-CD-PERC-APL-PREF'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI068-CD-PERC-APL-PREF
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI068-CD-IEC-PREF'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI068-CD-IEC-PREF
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

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

      *---------------------- FIM SGTSI068 -----------------------------