      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSE079
      * ANALISTA..: F6361711 Luis Valamede
      * AUTOR.....: F9507141 Vicente de Paulo Tavares de Souza
      * OBJETIVO..: Excluir Taxas de Quebra de Condição Negociada
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS002 24/01/2019 F9188847 DE 2016/145195 - Inclusão monitoração
      *          variáveis para rotina SBDATAF
      * VRS001 25/08/2017 F2804421 DE 2016/145195 - Mudança de nomes:
      *          Tabela TAXA_QBRA_CND_NEGD para CND_NEGD_OPR e atributos
      *          ref a DT-INC-TAXA-QBRA para DT-INC-VLD-CND
      *                DT-FIM-TAXA-QBRA para DT-FIM-VLD-CND
      *        23.01.2017 F6361711  DE 2016/145195 - Implantao
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSE079.

      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-SS               PIC X(35) VALUE
               '*** STORAGE SECTION COMECA AQUI ***'.
       77  CTE-PROG               PIC  X(17) VALUE '*** SGTSE079 ***'.
       77  CTE-VERS               PIC  X(06) VALUE 'VRS002'.

      * Book do programa SBDATAF para validação de datas
       77  SBDATAF                     PIC  X(008) VALUE 'SBDATAF'.
      *----------------------------------------------------------------*
      *                    SUBROTINAS UTILIZADAS
      *----------------------------------------------------------------*
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informações
-INC HLPKE01B
      *----------------------------------------------------------------*
      *                     CONTAINERS UTILIZADOS                      *
      *----------------------------------------------------------------*
      *--> Container de erro
       77 CTNR-PRM-ERRO                PIC  X(16) VALUE 'PRM-ERRO'.

      *--> Container do usuário logado
       77 CTNR-PRM-INF-OPRD            PIC  X(16) VALUE 'PRM-INF-OPRD'.

      *----------------------------------------------------------------*
      *                            OUTROS
      *----------------------------------------------------------------*
      *Tabela utilizada
       77  CND-NEGD-OPR                PIC  X(18) VALUE
                                                 'CND_NEGD_OPR'.
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * Book de para acionamento do MSG1
-INC IIBKERRO
      * Book com campos de abend do programa acionado
-INC MSGKWMG1
      * Book com campos da monitoração
-INC HLPKW999
      * Book com campos do container
-INC SGTKERR1

      * Area de trabalho
      * Armazena a data inicio de validade da condição.
       01  DT-INC-VLD-CND              PIC  X(10)  VALUE '00.00.0000'.
       01  DT-INC-VLD-CND-R REDEFINES DT-INC-VLD-CND.
           03  DIA                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  MES                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  ANO                     PIC  9(04).
      *----------------------------------------------------------------*
      *                 AREA DA SUBROTINA SBDATAF
      *----------------------------------------------------------------*
      *    Campos utilizados para validação de data Gregoriana.
       01  FUNCAO                      PIC  X(03) VALUE 'F01'.
       01  ARG01                       PIC  9(08) VALUE 000.
       01  ARG02                       PIC  9(01) VALUE 004.

       01  AREAWORK.
         03  A-TAM                     PIC 9(0008) COMP VALUE 0.
         03  FILLER                    PIC X(4071) VALUE SPACES.
         03  A-FIM                     PIC X(0020) VALUE SPACES.

       01  QT-REG-SELD                      PIC S9(4) COMP-5 VALUE 0.
      *----------------------------------------------------------------*
      *    Variáveis para gravação do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG      PIC  9(10) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG     PIC  9(11) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG     PIC  9(05) VALUE 0.
           03  CD-TIP-CND-NEGD-LOG     PIC  9(05) VALUE 0.
           03  DT-INC-VLD-CND-LOG      PIC  X(10) VALUE SPACES.

      *--> Book do container com dados do operador
-INC IIBKOPRD

      * Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

       01  SGTS014G VALUE 'SGTS014G'   PIC X(8).

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

-INC SGTKE079

       01  CTE-FINAL-SS                PIC X(40) VALUE
               '*** STORAGE SECTION TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO
      * Habilita tratamento automático de Erros DB2
-INC HLPKE997
      * Habilita captura automática de Abend e Erros CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDA-CAMPOS
           PERFORM 973000-LE-DADOS-OPERADOR
           PERFORM 200000-EXCLUIR
           PERFORM 300000-POSTA-RESPOSTA
           GOBACK
           .
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

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
       130000-RECEBER-REQUISICAO SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSE079-RQSC     TO QT-CCT-EPRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE
           MOVE 'SGTSE079-RQSC'        TO NM-CTNR

           EXEC CICS GET
              CONTAINER ( NM-CTNR            )
              CHANNEL   ( NM-CNL             )
              NODATA
              FLENGTH   ( QT-CCT-LIDO        )
           END-EXEC

      *    Valida tamanho esperado da requisição X tamanho recebido

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA   TO TRUE
               PERFORM 999000-ERRO
           END-IF

           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS GET
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSE079-RQSC )
              INTO      ( SGTSE079-RQSC           )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDA-CAMPOS SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           IF  SGTSE079-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0002               TO MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -        ' não informado.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSE079-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada de mercado de capita
      -        'is não numérico.'      TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSE079-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0005               TO MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada de mercado de capita
      -        'is não informado.'     TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSE079-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0007               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada não inform
      -             'ado.'             TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSE079-CD-TIP-CND-NEGD EQUAL ZEROS
               MOVE 0210               TO MSG1-CD-ERRO-TS
               MOVE 'Código do tipo de condição negociada não informado.
      -        ' '                     TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSE079-DT-INC-VLD-CND    EQUAL SPACES
           OR  SGTSE079-DT-INC-VLD-CND    EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0144               TO MSG1-CD-ERRO-TS
               MOVE 'Data início da validade da condição não informada.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           ELSE
               MOVE SGTSE079-DT-INC-VLD-CND
                                       TO ARG01
               PERFORM  970100-VALIDA-DATA
               IF  RETURN-CODE         NOT EQUAL  ZEROS
                   MOVE 0187           TO MSG1-CD-ERRO-TS
                   MOVE 'Data início da validade da condição inválida.'
                                       TO MSG1-TX-ERRO-TS
                   PERFORM 999000-ERRO
               END-IF
               MOVE '00.00.0000'       TO DT-INC-VLD-CND-R
               MOVE CORR SGTSE079-DT-INC-VLD-CND-R
                                       TO DT-INC-VLD-CND-R
           END-IF
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-EXCLUIR SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE DADOS-LOG
      *    Exclusão
           MOVE 'E'                    TO SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG

           EXEC SQL
                DELETE FROM DB2SGT.CND_NEGD_OPR
                WHERE CD_CTRA_TIT_VL     = :SGTSE079-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP    = :SGTSE079-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD    = :SGTSE079-NR-VRS-OPR-ETRD
                  AND CD_TIP_CND_NEGD    = :SGTSE079-CD-TIP-CND-NEGD
                  AND DT_INC_VLD_CND     = :DT-INC-VLD-CND
           END-EXEC
           IF  SQLCODE EQUAL +100
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0100                TO MSG1-CD-ERRO-TS
               MOVE 'Registro inexistente.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE 'Exclusão efetuada com sucesso.'
                                       TO SGTSE079-TX-RTN-PRCT

           SET PUT-CONTAINER           TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE
           MOVE 'SGTSE079-RPST'        TO NM-CTNR

           EXEC CICS PUT
              CONTAINER ( NM-CTNR         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSE079-RPST )
              FROM      ( SGTSE079-RPST           )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970100-VALIDA-DATA          SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE 4095                   TO  A-TAM
           MOVE 'SBDATAF -WRKAREA-FIM' TO  A-FIM

           CALL  SBDATAF               USING FUNCAO AREAWORK ARG01 ARG02

           IF  RETURN-CODE             NOT EQUAL ZEROS
               SET  ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               MOVE 0051               TO MSG1-CD-ERRO-TS
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE 'Erro no tratamento de datas.'
                                       TO MSG1-TX-ERRO-TS
                                          MSG1-TX-RTN-PGM-ACND

               MOVE 'FUNCAO'           TO  MSGKW999-NM-VRV-MNT
               MOVE  FUNCAO            TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'A-TAM'            TO MSGKW999-NM-VRV-MNT
               MOVE  A-TAM             TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'A-FIM'         TO MSGKW999-NM-VRV-MNT
               MOVE  A-FIM          TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG01'           TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG01            TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG02'           TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG02            TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT
               PERFORM 999000-ERRO
           END-IF
           .
       970100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       973000-LE-DADOS-OPERADOR        SECTION.
      *----------------------------------------------------------------*
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE IIBKERRO-PRM-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           MOVE 'PRM-INF-OPRD'         TO NM-CTNR

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           INITIALIZE PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980102-GRAVA-LOG SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           MOVE CND-NEGD-OPR                TO SGTS014G-NM-ENT
           MOVE SGTSE079-CD-CTRA-TIT-VL     TO CD-CTRA-TIT-VL-LOG
           MOVE SGTSE079-CD-OPR-ETRD-MCP    TO CD-OPR-ETRD-MCP-LOG
           MOVE SGTSE079-NR-VRS-OPR-ETRD    TO NR-VRS-OPR-ETRD-LOG
           MOVE SGTSE079-CD-TIP-CND-NEGD    TO CD-TIP-CND-NEGD-LOG
           MOVE DT-INC-VLD-CND              TO DT-INC-VLD-CND-LOG

           STRING CD-CTRA-TIT-VL-LOG
              ';' CD-OPR-ETRD-MCP-LOG
              ';' NR-VRS-OPR-ETRD-LOG
              ';' CD-TIP-CND-NEGD-LOG
              ';' DT-INC-VLD-CND-LOG
                  DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR

           MOVE KOPRD-CD-OPRD          TO SGTS014G-CD-CHV-USU
           MOVE CTE-PROG(5:8)          TO SGTS014G-NM-PGM

           MOVE LENGTH OF DADOS-LOG    TO EIBCALEN
           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM  NOT EQUAL ZEROS
               SET   ERRO-PROGRAMA-ACIONADO TO  TRUE
               MOVE  112                    TO  MSG1-CD-ERRO-TS
               MOVE 'Erro na gravação das informações do Log.'
                                            TO  MSG1-TX-ERRO-TS
               MOVE  SGTS014G               TO  MSG1-NM-PGM-ACND
               MOVE  SGTS014G-MSG-ERRO-PGM  TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTS014G-CD-RTN-PGM    TO  MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-ERRO
           END-IF
           .
       980102-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-ERRO     SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO  MSG1-CD-SEQL-ERRO

           IF MSG1-CD-ERRO-TS EQUAL ZEROS
              MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF
           IF MSG1-TX-ERRO-TS EQUAL SPACES
              MOVE 'Erro não especificado no módulo'
                                       TO MSG1-TX-ERRO-TS
           END-IF
           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-ERRO-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

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

           MOVE 'SGTSE079-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSE079-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE079-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSE079-CD-OPR-ETRD-MCP     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE079-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSE079-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE079-CD-TIP-CND-NEGD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSE079-CD-TIP-CND-NEGD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE079-DT-INC-VLD-CND  '  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSE079-DT-INC-VLD-CND      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS        TO  IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO  IIBKERRO-TX-ERRO

           EXEC CICS PUT
              CONTAINER ( CTNR-PRM-ERRO             )
              CHANNEL   ( NM-CNL                    )
              FROM      ( IIBKERRO-PRM-ERRO         )
              FLENGTH ( LENGTH OF IIBKERRO-PRM-ERRO )
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

           GOBACK
           .
       999999-SAI.
           EXIT.