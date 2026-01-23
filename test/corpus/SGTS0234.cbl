       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0234
      * ANALISTA..: F9188847 - Soila Akamatsu
      * AUTOR.....: F9188847 - Soila Akamatsu
      * DATA......: 27/09/2018
      * OBJETIVO..: Negócio  - Calcular saldos das carteiras a encartei
      *             rar de Titulos de Valores Mobiliarios
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 30/10/2018 F9188847  DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0234.
      *
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
      *
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------

       77  CTE-INICIO-SS               PIC  X(40)  VALUE
                                            '*** S.S. COMEÇA AQUI ***'.
       77  CTE-PROG                    PIC  X(18)  VALUE
                                            '*** SGTS0234 ***'.
       77  CTE-VERS                    PIC  X(06)  VALUE 'VRS001'.

      * Módulos acionados
       77  SGTSLB05                    PIC  X(08) VALUE 'SGTSLB05'.
       77  SGTS0225                    PIC  X(08) VALUE 'SGTS0225'.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      * Áreas de trabalho
       01  AUX-CD-OPR-ETRD         PIC  9(11).

      * Indicadores de nulidades
      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD         PIC  9(11).

       01  IND                         PIC S9(04) COMP-5 VALUE 0.

      *-----------------------------------------------------------------
      * Books dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      * Book para captura de abend cics do bloco código HLPKW999
-INC HLPKW999
      * Book para captura de erro via MSG1
-INC MSGKWMG1
      * Book do container de requisição e resposta deste módulo
       01 SGTS0234-DADOS.
-INC SGTK0234
      * Lista PSC_DRIA_SDO_OPR
-INC SGTKLB05
      *  Consulta as carteiras que compõem o limite global.
       01 SGTS0225-DADOS.
-INC SGTK0225
      * Calcula saldo operações encarteiradas de determinada carteira
       01 SGTS0227-DADOS.
-INC SGTK0227
      * Book do Container de Erro
-INC IIBKERRO

       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

           PERFORM 100000-INICIAR

           PERFORM 200000-PROCESSAR

           PERFORM 980000-GRAVAR-CONTAINER-RPST

           PERFORM 999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE 001                    TO MSG1-CD-SEQL-ERRO

      *--> WHENEVER DB2
      *-INC HLPKE997

      *    Ativa a captura automática de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM 970000-LER-CANAL

      *    Define o nome do programa p/ fins manipulação container
           MOVE CTE-PROG(5:8)          TO NM-PGM

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

           PERFORM 110000-VALIDAR-REQUISICAO

           PERFORM VARYING IND FROM 1 BY 1
                  UNTIL IND > 10
                   MOVE 10            TO SGTS0234-QT-SDO-CTRA
                   MOVE ZEROS         TO SGTS0234-CD-CTRA-TIT-VL (IND)
                                         SGTS0234-VL-SDO-CTRA-ENCT (IND)

           END-PERFORM

           MOVE ZEROS                  TO IND
                                          SGTS0234-QT-SDO-CTRA
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 002                    TO MSG1-CD-SEQL-ERRO

           IF  SGTS0234-E-CD-CTRA-TIT-VL     LESS ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0234-E-CD-CTRA-TIT-VL     EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------

              PERFORM 210000-CONSULTA-CTRA-GLB
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       210000-CONSULTA-CTRA-GLB         SECTION.
      *-----------------------------------------------------------------
           MOVE 003                    TO MSG1-CD-SEQL-ERRO

           MOVE SGTS0225               TO NM-PGM

           MOVE SGTS0234-E-CD-CTRA-TIT-VL
                                       TO SGTS0225-CD-CTRA-TIT-VL

           PERFORM 900000-EXECUTAR-MODULO

           IF SGTS0225-QT-OCR-FNCO > ZEROS
             MOVE SGTS0225-QT-OCR-FNCO  TO SGTS0234-QT-SDO-CTRA
           END-IF


           PERFORM VARYING IND   FROM 1 BY  1
                   UNTIL   IND   GREATER SGTS0225-QT-OCR-FNCO

      *  Carrega Carteiras Não encarteiradas e seus títulos
              MOVE SPACES              TO  SGTSLB05-IN-CTNC-PSQ
              MOVE ZEROS               TO  SGTSLB05-E-CD-OPR-ETRD-MCP
                                           SGTSLB05-E-CD-TIP-DOC-TCN
                                           AUX-CD-OPR-ETRD
                                           SGTSLB05-QT-OCR-TAB
                                           SGTSLB05-IC-TAB
              PERFORM UNTIL SGTSLB05-IN-CTNC-PSQ EQUAL 'N'
                 PERFORM 220000-LISTAR-TITULO
                 IF SGTSLB05-QT-OCR-TAB > ZEROS
                   PERFORM 230000-CALCULA-SALDO-N-ENCO
                       VARYING SGTSLB05-IC-TAB FROM 1 BY 1
                        UNTIL SGTSLB05-IC-TAB > SGTSLB05-QT-OCR-TAB
                END-IF

      * Campos de rechamada da Lista Títulos por carteira
                   IF SGTSLB05-QT-OCR-TAB > ZEROS
                     MOVE SGTSLB05-CD-OPR-ETRD-MCP (SGTSLB05-QT-OCR-TAB)
                                       TO SGTSLB05-E-CD-OPR-ETRD-MCP
                     MOVE SGTSLB05-CD-TIP-DOC-TCN (SGTSLB05-QT-OCR-TAB)
                                       TO SGTSLB05-E-CD-TIP-DOC-TCN
                   END-IF

              END-PERFORM
           END-PERFORM
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-LISTAR-TITULO          SECTION.
      *-----------------------------------------------------------------
           MOVE 004                    TO MSG1-CD-SEQL-ERRO

           MOVE SGTSLB05               TO NM-PGM

           SET SGTSLB05-LISTA-TIT-N-ENCO TO TRUE
           MOVE SGTS0225-CD-CTRA-TIT-FNCO-GLB (IND)
                                       TO SGTSLB05-E-CD-CTRA-TIT-VL
                                          SGTS0234-CD-CTRA-TIT-VL(IND)
           PERFORM 900000-EXECUTAR-MODULO

           .
       220000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       230000-CALCULA-SALDO-N-ENCO       SECTION.
      * Calcula saldo das Carteiras não Encarteiradas
      *-----------------------------------------------------------------
           MOVE 005                    TO MSG1-CD-SEQL-ERRO

           IF SGTSLB05-CD-OPR-ETRD-MCP (SGTSLB05-IC-TAB)
                                                 EQUAL AUX-CD-OPR-ETRD
              MOVE  SGTSLB05-CD-OPR-ETRD-MCP (SGTSLB05-IC-TAB)
                                       TO AUX-CD-OPR-ETRD
           ELSE
              IF SGTSLB05-CD-TIP-DOC-TCN (SGTSLB05-IC-TAB) = 1
                ADD SGTSLB05-VL-CPR-OPR (SGTSLB05-IC-TAB)
                                     TO SGTS0234-VL-SDO-CTRA-ENCT(IND)
                  ON SIZE ERROR
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0839          TO  MSG1-CD-ERRO-TS
                   MOVE  'Erro no cálculo de soma.'
                                       TO  MSG1-TX-ERRO-TS
                   MOVE  008           TO MSG1-CD-SEQL-ERRO
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-ADD

               ADD SGTSLB05-VL-CPR-OPR (SGTSLB05-IC-TAB)
                                    TO SGTS0234-VL-SDO-TTL-ENCT
                ON SIZE ERROR
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0839          TO  MSG1-CD-ERRO-TS
                   MOVE  'Erro no cálculo de soma.'
                                       TO  MSG1-TX-ERRO-TS
                   MOVE  009           TO MSG1-CD-SEQL-ERRO
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-ADD
              ELSE
                ADD SGTSLB05-VL-PRV-ENCT (SGTSLB05-IC-TAB)
                                     TO  SGTS0234-VL-SDO-CTRA-ENCT(IND)
                ON SIZE ERROR
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0839          TO  MSG1-CD-ERRO-TS
                   MOVE  'Erro no cálculo de soma.'
                                       TO  MSG1-TX-ERRO-TS
                   MOVE  010           TO MSG1-CD-SEQL-ERRO
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-ADD

               ADD SGTSLB05-VL-PRV-ENCT (SGTSLB05-IC-TAB)
                                     TO SGTS0234-VL-SDO-TTL-ENCT
                ON SIZE ERROR
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0839          TO  MSG1-CD-ERRO-TS
                   MOVE  'Erro no cálculo de soma.'
                                       TO  MSG1-TX-ERRO-TS
                   MOVE  011           TO MSG1-CD-SEQL-ERRO
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-ADD
              END-IF
              MOVE  SGTSLB05-CD-OPR-ETRD-MCP (SGTSLB05-IC-TAB)
                                       TO AUX-CD-OPR-ETRD
           END-IF

           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *-----------------------------------------------------------------
      * Executa um módulo implementado com CONTAINER                   *
      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *-----------------------------------------------------------------

      *    Grava o container de requisição do módulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM

               WHEN SGTSLB05
                    EXEC CICS LINK
                              PROGRAM ( SGTSLB05   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTS0225
                    EXEC CICS LINK
                              PROGRAM ( SGTS0225   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC


               WHEN OTHER
                    MOVE 9006          TO MSG1-CD-ERRO-TS
                    MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
      *
           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    MOVE 3004          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro ao acionar o módulo.'
                                       TO MSG1-TX-ERRO-TS
                    SET ERRO-CICS      TO TRUE
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *    Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *-----------------------------------------------------------------
      * Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------
      * Variáveis exclusivas de cada programa acionado
           EVALUATE NM-PGM

              WHEN SGTSLB05
               MOVE 'SGTSLB05-CD-TIP-SRVC'    TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSLB05-CD-TIP-SRVC     TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSLB05-E-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSLB05-E-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSLB05-E-CD-TIT-OPR-ETRD'
                                              TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSLB05-E-CD-TIT-OPR-ETRD
                                              TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSLB05-E-NR-DVS-TIT' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSLB05-E-NR-DVS-TIT  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

             WHEN SGTS0225
               MOVE 'SGTS0225-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0225-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

              WHEN OTHER
                  CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *                                                                *
      * SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
      *                                                                *
      *-----------------------------------------------------------------
       970000-LER-CANAL                SECTION.
      *    Obtem o canal corrente
      *-----------------------------------------------------------------
           MOVE 012                    TO MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       970000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------
           MOVE 013                    TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisição
           MOVE CTE-PROG(5:8)          TO NM-PGM
           MOVE 'SGTS0234-RQSC'        TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      *--> Obtem o tamanho esperado da requisição
           MOVE  LENGTH OF SGTS0234-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisição X tamanho recebido
           IF  QT-CCT-LIDO             NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE 014                    TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER ( NM-CTNR       )
                     FLENGTH   ( QT-CCT-LIDO   )
                     INTO      ( SGTS0234-RQSC )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *-----------------------------------------------------------------
      *    A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------

      *    Lê o container de erro dos módulos chamados
           SET GET-CONTAINER           TO TRUE
           MOVE 'PRM-ERRO'             TO NM-CTNR
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS GET
                     CONTAINER ( NM-CTNR                     )
                     INTO      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC

           EVALUATE TRUE
      *        Caso deseje personalizar ou não interromper o
      *        processamento, alterar para não dar erro aqui SOMENTE
      *        PARA O módulo CHAMADO e tratar na section chamadora
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    SET ERRO-PROGRAMA-ACIONADO TO TRUE
                    MOVE  NM-PGM       TO  MSG1-NM-PGM-ACND

                    IF  NM-PGM(1:3) EQUAL 'SGT'
                        MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                                           MSG1-CD-ERRO-TS
                        MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
                    ELSE
                       MOVE 3004          TO MSG1-CD-ERRO-TS
                       STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                         DELIMITED BY SIZE
                         INTO           MSG1-TX-ERRO-TS

                       MOVE IIBKERRO-TX-ERRO TO MSG1-TX-RTN-PGM-ACND
                       MOVE IIBKERRO-CD-ERRO TO MSG1-CD-RTN-PGM-ACND
                    END-IF

      *             Monitorar variáveis do módulo chamado
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *-----------------------------------------------------------------
      *    Recupera o container com os dados de resposta (saída).
      *-----------------------------------------------------------------

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

      *    Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

      *Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Lê a RESPOSTA de acordo com o programa que a gravou


           EVALUATE NM-PGM

      *    Calcula a quantidade de caracteres esperado e limpa a área de
      *    dados antes de executar o GET (somentepara matrizes com dados
      *    não variáveis)

               WHEN SGTSLB05
                    MOVE LENGTH OF SGTSLB05-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSLB05-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTS0225
                    MOVE LENGTH OF SGTS0225-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTS0225-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9003         TO  MSG1-CD-ERRO-TS
                    STRING 'Erro na leitura da resposta do módulo '
                           NM-PGM '.' DELIMITED BY SIZE
                           INTO  MSG1-TX-ERRO-TS
                    END-STRING
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Obtém a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER ( NM-CTNR     )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973100-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
      *                                                                *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *----------------------------------------------------------------*

      *    Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

           MOVE 015                    TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de resposta

           MOVE 'SGTS0234-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                )
                     FROM      (SGTS0234-RPST          )
                     FLENGTH   (LENGTH OF SGTS0234-RPST)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *-----------------------------------------------------------------
      *    Postar container com dados de requisição (entrada)
      *-----------------------------------------------------------------

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_modulo -RQSC

           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RQSC'       TO NM-CTNR(9:5)


      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisição do programa a executar
           EVALUATE NM-PGM

               WHEN SGTSLB05
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSLB05-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSLB05-RQSC )
                    END-EXEC
      *
               WHEN SGTS0225
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTS0225-RQSC           )
                              FLENGTH   ( LENGTH OF SGTS0225-RQSC )
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9004         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na gravação da requisição do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       982000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *-----------------------------------------------------------------
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *-----------------------------------------------------------------


      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RPST'       TO NM-CTNR(9:5)


           SET DEL-CONTAINER           TO TRUE

      *    Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR    )
                     RESP      ( MSG1-RESP1 )
                     RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *    Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------

      *    Limpar variáveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE 016                    TO MSG1-CD-SEQL-ERRO
           MOVE 'PRM-ERRO'             TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR    )
                     RESP      ( MSG1-RESP1 )
                     RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *-----------------------------------------------------------------

      *    Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           MOVE 'PRM-ERRO'             TO NM-CTNR
           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                     )
                     FROM      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC
           .
       984000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    As sections abaixo iniciadas com 999000 representam os
      *    tratamentos de erro utilizando o Sistema MSG
      *-----------------------------------------------------------------
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

      *--> Em caso de erro, desfaz as atualizações em banco de dados
           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC

      *--> Verifica se o tipo de erro gerado possui tratamento interno
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

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE 'SGTS0234-E-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0234-E-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *--> Grava a Ocorrência de Erro no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava a Ocorrência de Erro no Container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

      *--> Encerra a transacao
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
      *INC HLPKP997
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
      *---------------------- Fim SGTS0234 ----------------------------*