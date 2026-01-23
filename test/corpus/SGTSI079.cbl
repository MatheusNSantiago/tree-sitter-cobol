      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSI079
      * ANALISTA..: F2054109 Cintia Ap Soares Iglesias
      * AUTOR.....: F6361711 Luis Valamede
      * OBJETIVO..: Inclusão container na tabela CND_NEGD_OPR
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS003 03/03/2023 F9158937 - Inclusão campo SGTSI079-CD-FMA-APRC
      * VRS002 04/01/2019 F2804421  -Monitoração SBDATAF e SBTIMEDT.
      * VRS002 20/12/2018 F8090555 - Permite taxa zerada
      * VRS001 07/02/2018 F9158937 - Acerto mensageria
      *        28/08/2017 F9188847 de 2016/145195 - Mudança de nomes:
      *          Tabela   : De TAXA_QBRA_CND_NEGD para CND_NEGD_OPR
      *          Atributos: De DT-INC-TAXA-QBRA para DT-INC-VLD-CND
      *                     De DT-FIM-TAXA-QBRA para DT-FIM-VLD-CND
      *        13/07/2017 F8658869 DE 2016/145195 - Alteração LVF
      *        03/02/2017 F8785286 DE 2016/145195 - Inclusão tratamento
      *              batch para gravação no LOG.
      *        26.12.2016 - F6361711 - 2015/137233 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSI079.

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
                                       VALUE '*** SGTSI079 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS003'.

       77  SBDATAF                     PIC  X(08) VALUE 'SBDATAF'.
       77  SGTS014G                    PIC  X(08) VALUE 'SGTS014G'.

      *--> Constante informada para se obter data atual do sistema
       77 OBTER-DATA-ATUAL             PIC X(08) VALUE 'CURRENT '.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  QT-REG-SELD                 PIC S9(4) COMP-5 VALUE ZEROS.
       01  CD-USU-ATL                  PIC  X(08) VALUE SPACES.
       01  TS-SQL-ATU                  PIC  X(26) VALUE SPACES.
       01  TS-SQL-ATU-R
           REDEFINES TS-SQL-ATU.
           03  ANO                     PIC  9(04).
           03  FILLER                  PIC  X(01).
           03  MES                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  DIA                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  HORA                    PIC  X(02).
           03  FILLER                  PIC  X(01).
           03  MINUTO                  PIC  X(02).
           03  FILLER                  PIC  X(01).
           03  SEGUNDO                 PIC  X(02).
           03  FILLER                  PIC  X(01).
           03  MICRO-SEGUNDO           PIC  X(06).

      *--> Armazena a data inicio taxa quebra de fundo
       01  DT-INC-VLD-CND              PIC  X(10)  VALUE '00.00.0000'.
       01  DT-INC-VLD-CND-R REDEFINES DT-INC-VLD-CND.
           03  DIA                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  MES                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  ANO                     PIC  9(04).

      *--> Armazena a data fim taxa quebra de fundo
       01  DT-FIM-VLD-CND              PIC  X(10)  VALUE '00.00.0000'.
       01  DT-FIM-VLD-CND-R REDEFINES DT-FIM-VLD-CND.
           03  DIA                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  MES                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  ANO                     PIC  9(04).

      *--> Indicador da data testada.
       01  AX-IND-DT                   PIC  X(01)  VALUE SPACES.
           88  DATA-INICIO                         VALUE 'I'.
           88  DATA-FIM                            VALUE 'F'.

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

      *--> Variáveis para gravação do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG      PIC  9(10) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG     PIC  9(11) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG     PIC  9(05) VALUE 0.
           03  CD-TIP-CND-NEGD-LOG     PIC  9(05) VALUE 0.
           03  DT-INC-VLD-CND-LOG      PIC  X(10) VALUE SPACES.


      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

       01  PC-TAXA-QBRA-FNDO.
           03  TAXA-FNDO-DISPLAY       PIC ZZZ9,9(10).

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
       01  SGTSI079-DADOS.
-INC SGTKI079

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

           PERFORM  140000-BUSCA-TS-DT
           PERFORM  150000-VALIDAR-CAMPOS
           PERFORM  160000-VALIDA-OPR-ETRD-MCP

           PERFORM  150000-LER-DADOS-OPRD

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           SET  ERRO-CANAL  TO  TRUE

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
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

      *    Limpar variáveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE 'PRM-ERRO'             TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE
           SET ERRO-CONTAINER          TO TRUE

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER (NM-CTNR   )
                     RESP      (MSG1-RESP1)
                     RESP2     (MSG1-RESP2)
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE

           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-CCT-LIDO
           MOVE  LENGTH OF SGTSI079-RQSC
                                       TO  QT-CCT-EPRO
      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE


           EXEC CICS GET
                     CONTAINER ( 'SGTSI079-RQSC' )
                     CHANNEL   ( NM-CNL          )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA TO TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
                     CONTAINER ( 'SGTSI079-RQSC'         )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSI079-RQSC )
                     INTO      ( SGTSI079-RQSC           )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-BUSCA-TS-DT              SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           CALL SBTIMEDT            USING  HLPKE001-CD-FUC
                                           HLPKE001-AREA-TRB
                                           HLPKE001-FMT-TS
                                           HLPKE001-TAM-TS
                                           HLPKE001-TS-FMTD

           IF  RETURN-CODE                 NOT EQUAL  ZEROS
               SET  ERRO-PROGRAMA-ACIONADO TO  TRUE
               MOVE  0050                  TO  MSG1-CD-ERRO-TS
               MOVE  'Erro ao obter data e hora atual.'
                                           TO  MSG1-TX-ERRO-TS
               MOVE  SBTIMEDT              TO  MSG1-NM-PGM-ACND
               MOVE  RETURN-CODE           TO  MSG1-CD-RTN-PGM-ACND

               MOVE 'HLPKE001-CD-FUC'      TO  MSGKW999-NM-VRV-MNT
               MOVE  HLPKE001-CD-FUC       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'HLPKE001-AREA-TRB-TAM' TO  MSGKW999-NM-VRV-MNT
               MOVE HLPKE001-AREA-TRB-TAM   TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'HLPKE001-FMT-TS'       TO  MSGKW999-NM-VRV-MNT
               MOVE HLPKE001-FMT-TS         TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'HLPKE001-TAM-TS'       TO  MSGKW999-NM-VRV-MNT
               MOVE  HLPKE001-TAM-TS        TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'HLPKE001-TS-FMTD'      TO  MSGKW999-NM-VRV-MNT
               MOVE HLPKE001-TS-FMTD        TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE      HLPKE001-TS-FMTD  TO  TS-SQL-ATU

           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALIDAR-CAMPOS           SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada
           IF  SGTSI079-CD-CTRA-TIT-VL     EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'  TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI079-CD-CTRA-TIT-VL     NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI079-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI079-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'    TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI079-CD-OPR-ETRD-MCP    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais in
      -              'válido.'         TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI079-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI079-NR-VRS-OPR-ETRD    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Valida Outros Campos de Entrada
           IF  SGTSI079-CD-TIP-CND-NEGD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0210              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do tipo de condição negociada não informado
      -              'o.'              TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI079-CD-TIP-CND-NEGD     NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0211              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do tipo de condição negociada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI079-CD-TIP-CBT         EQUAL  ZEROS
               MOVE  0212              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do tipo de cobertura não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI079-CD-TIP-CBT         NOT POSITIVE
               MOVE  0213              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do tipo de cobertura inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI079-IN-OBGD-CND         EQUAL SPACES
               MOVE  0214              TO  MSG1-CD-ERRO-TS
               MOVE  'Indicador de obrigatoriedade da condição não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI079-IN-OBGD-CND        NOT EQUAL  'N'  AND
                                                      'S'
               MOVE  0215              TO  MSG1-CD-ERRO-TS
               MOVE  'Indicador de obrigatoriedade da condição inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI079-PC-TAXA-QBRA-FNDO  NOT NUMERIC
               MOVE  0216              TO  MSG1-CD-ERRO-TS
               MOVE  'Percentual da taxa de quebra de fundo não numérico
      -              '.'               TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *     IF  SGTSI079-PC-TAXA-QBRA-FNDO  EQUAL ZEROS
      *         MOVE  0217              TO  MSG1-CD-ERRO-TS
      *         MOVE  'Percentual da taxa de quebra de fundo não informad
      *-              'o.'              TO  MSG1-TX-ERRO-TS
      *         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
      *     END-IF

           IF  SGTSI079-CD-FMA-APRC  NOT EQUAL ZEROS    AND
               SGTSI079-CD-FMA-APRC  NOT EQUAL   1      AND
               SGTSI079-CD-FMA-APRC  NOT EQUAL   2
               MOVE  0328              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da Forma de Apuração inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF


      *--> Validar Data Início de Validade
           IF  SGTSI079-DT-INC-VLD-CND   EQUAL SPACES
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE  0144              TO  MSG1-CD-ERRO-TS
               MOVE  'Data início validade de condição não informada.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
      *------> Verifica se o sistema enviou a palavra CURRENT; sendo
      *        necessário obter a data atual do sistema.
               IF  SGTSI079-DT-INC-VLD-CND   EQUAL OBTER-DATA-ATUAL
                   MOVE  CORR HLPKE001-TS-FMTD
                                       TO  DT-INC-VLD-CND-R
                   MOVE  CORR DT-INC-VLD-CND-R
                                       TO  SGTSI079-DT-INC-VLD-CND-R
               ELSE
                   MOVE  SGTSI079-DT-INC-VLD-CND
                                       TO  ARG01
      *----------> Valida Data
                   SET DATA-INICIO     TO  TRUE
                   PERFORM  151000-VALIDAR-DATAS
      *             MOVE '00.00.0000'      TO  DT-INC-VLD-CND
                   MOVE CORR SGTSI079-DT-INC-VLD-CND-R
                                       TO  DT-INC-VLD-CND-R
               END-IF
           END-IF

      *--> Validar Data Fim de Validade
           IF  SGTSI079-DT-FIM-VLD-CND   EQUAL SPACES
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE  0208              TO  MSG1-CD-ERRO-TS
               MOVE  'Data fim de validade da condição não informada.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
      *-----> Valida Data
              MOVE  SGTSI079-DT-FIM-VLD-CND
                                       TO  ARG01
              SET DATA-FIM             TO  TRUE
              PERFORM  151000-VALIDAR-DATAS
           END-IF

           MOVE  CORR SGTSI079-DT-FIM-VLD-CND-R
                                       TO  DT-FIM-VLD-CND-R

           .
       150000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       151000-VALIDAR-DATAS            SECTION.
      *--> Aciona o SBDATAF para validar Data Início e Data Fim
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           MOVE  4095                  TO  TAM
           MOVE  'SBDATAF -WRKAREA-FIM'
                                       TO  FIM

           CALL  SBDATAF            USING  FUC AREA-WORK ARG01 ARG02

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE 'FUC'              TO  MSGKW999-NM-VRV-MNT
               MOVE  FUC               TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'TAM'              TO  MSGKW999-NM-VRV-MNT
               MOVE  TAM               TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'FIM'              TO  MSGKW999-NM-VRV-MNT
               MOVE  FIM               TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG01'            TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG01             TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG02'            TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG02             TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               EVALUATE  TRUE
                   WHEN  DATA-INICIO
                         MOVE  0187    TO  MSG1-CD-ERRO-TS
                         MOVE  'Data início de validade da condição invá
      -                        'lida.' TO  MSG1-TX-ERRO-TS

                   WHEN  DATA-FIM
                         MOVE  0209    TO  MSG1-CD-ERRO-TS
                         MOVE  'Data fim de validade da condição inválid
      -                        'a.'    TO  MSG1-TX-ERRO-TS

                   WHEN  OTHER
                         MOVE  0051    TO  MSG1-CD-ERRO-TS
                         MOVE  'Erro no tratamento de datas.'
                                       TO  MSG1-TX-ERRO-TS
               END-EVALUATE
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  SBDATAF           TO  MSG1-NM-PGM-ACND
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       151000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       160000-VALIDA-OPR-ETRD-MCP      SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-REG-SELD

           EXEC SQL
                SELECT 1
                  INTO :QT-REG-SELD
                  FROM DB2SGT.OPR_ETRD_MCP
                 WHERE NR_VRS_OPR_ETRD  =  :SGTSI079-NR-VRS-OPR-ETRD
                   AND CD_CTRA_TIT_VL   =  :SGTSI079-CD-CTRA-TIT-VL
                   AND CD_OPR_ETRD_MCP  =  :SGTSI079-CD-OPR-ETRD-MCP
                 FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG-SELD                 EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0061              TO  MSG1-CD-ERRO-TS
               MOVE  'Operação estruturada inexistente.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       160000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO  TRUE
           MOVE  'PRM-INF-OPRD'        TO  NM-CTNR

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           INITIALIZE                      PRM-INF-OPRD

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                              )
                      INTO      ( PRM-INF-OPRD                         )
                      FLENGTH   ( LENGTH OF PRM-INF-OPRD               )
           END-EXEC
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
       200000-INCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

      *--> Desligar erro automatico do DB2
           EXEC SQL WHENEVER SQLERROR
                    CONTINUE
           END-EXEC

           EXEC SQL
                INSERT INTO DB2SGT.CND_NEGD_OPR
                           ( CD_CTRA_TIT_VL
                           , CD_OPR_ETRD_MCP
                           , NR_VRS_OPR_ETRD
                           , CD_TIP_CND_NEGD
                           , DT_INC_VLD_CND
                           , CD_USU_ULT_ATL
                           , TS_ULT_ATL
                           , CD_TIP_CBT
                           , DT_FIM_VLD_CND
                           , IN_OBGD_CND
                           , PC_TAXA_QBRA_FNDO
                           , CD_FMA_APRC
                           )
                VALUES
                           ( :SGTSI079-CD-CTRA-TIT-VL
                           , :SGTSI079-CD-OPR-ETRD-MCP
                           , :SGTSI079-NR-VRS-OPR-ETRD
                           , :SGTSI079-CD-TIP-CND-NEGD
                           , :DT-INC-VLD-CND
                           , :CD-USU-ATL
                           , :TS-SQL-ATU
                           , :SGTSI079-CD-TIP-CBT
                           , :DT-FIM-VLD-CND
                           , :SGTSI079-IN-OBGD-CND
                           , :SGTSI079-PC-TAXA-QBRA-FNDO
                           , :SGTSI079-CD-FMA-APRC
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
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'Inclusão efetuada com sucesso.'
                                       TO  SGTSI079-TX-RTN-PRCT
           EXEC CICS PUT
                     CONTAINER ( 'SGTSI079-RPST'         )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSI079-RPST )
                     FROM      ( SGTSI079-RPST           )
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
           MOVE  'CND_NEGD_OPR'        TO  SGTS014G-NM-ENT
           MOVE  CD-USU-ATL            TO  SGTS014G-CD-CHV-USU
           MOVE  'SGTSI079'            TO  SGTS014G-NM-PGM

           MOVE  SGTSI079-CD-CTRA-TIT-VL
                                       TO  CD-CTRA-TIT-VL-LOG
           MOVE  SGTSI079-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP-LOG
           MOVE  SGTSI079-NR-VRS-OPR-ETRD
                                       TO  NR-VRS-OPR-ETRD-LOG
           MOVE  SGTSI079-CD-TIP-CND-NEGD
                                       TO  CD-TIP-CND-NEGD-LOG
           MOVE  DT-INC-VLD-CND
                                       TO  DT-INC-VLD-CND-LOG

           STRING  CD-CTRA-TIT-VL-LOG
                   ';' CD-OPR-ETRD-MCP-LOG
                   ';' NR-VRS-OPR-ETRD-LOG
                   ';' CD-TIP-CND-NEGD-LOG
                   ';' DT-INC-VLD-CND-LOG
                   DELIMITED BY SIZE
                   INTO                SGTS014G-CHV-PRMR

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
               MOVE  SGTS014G-PSC-ERRO-PGM
                                       TO  MSG1-CD-SEQL-PGM-ACND
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       981000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA                     SECTION.
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

      *--> Adiciona Chave da Operação Estruturada
           MOVE 'SGTSI079-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI079-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTSI079-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSI079-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE  SGTSI079-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI079-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI079-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *--> Adiciona Outros Campos de Entrada
           MOVE 'SGTSI079-CD-TIP-CND-NEGD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI079-CD-TIP-CND-NEGD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI079-DT-INC-VLD-CND'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI079-DT-INC-VLD-CND
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI079-CD-TIP-CBT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI079-CD-TIP-CBT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI079-DT-FIM-VLD-CND'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI079-DT-FIM-VLD-CND
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI079-IN-OBGD-CND'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI079-IN-OBGD-CND
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI079-PC-TAXA-QBRA-FNDO'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI079-PC-TAXA-QBRA-FNDO
                                       TO  TAXA-FNDO-DISPLAY
           MOVE  PC-TAXA-QBRA-FNDO
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

            MOVE 'SGTSI079-CD-FMA-APRC'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI079-CD-FMA-APRC
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

      *---------------------- FIM SGTSI079 -----------------------------