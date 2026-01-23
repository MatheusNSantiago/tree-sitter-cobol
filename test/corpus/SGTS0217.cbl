       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0217
      * ANALISTA..: F8785286 Roseli Akemi Kawahata
      * AUTOR.....: F8785286 Roseli Akemi Kawahata
      * DATA......: 07/08/2018
      * OBJETIVO..: Efetuar procedimentos para cancelamento  de cadastro
      *    de operações não encarteiradas.
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS004 01/12/2023 - F9158937 - Incluir chamada SGTS005B -
      *                     Cancelamento Reserva OPR
      * VRS001 08/08/2018 F8785286 DE 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS0217.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

       77  CTE-INICIO-SS               PIC  X(040)  VALUE
                    '*** S.S. COMECA AQUI ***'.

       77  CTE-PROG                    PIC  X(018)  VALUE
                    '*** SGTS0217 ***'.
       77  CTE-VERS                    PIC  X(006)  VALUE 'VRS002'.

      * Módulos acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTSC069                PIC  X(008) VALUE 'SGTSC069'.
           03  SGTSI069                PIC  X(008) VALUE 'SGTSI069'.
           03  SGTS005B                PIC  X(008) VALUE 'SGTS005B'.
           03  SGTSPE03                PIC  X(008) VALUE 'SGTSPE03'.
           03  SGTSPE05                PIC  X(008) VALUE 'SGTSPE05'.
           03  SGTST009                PIC  X(008) VALUE 'SGTST009'.
           03  SGTST012                PIC  X(008) VALUE 'SGTST012'.

      *--> Definição dos Variaveis do Cobolito
       COPY ALMKCBL1.

      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*
      * Areas de trabalho
       01  AREA-AUX.
           05  AUX-TS                  PIC X(26) VALUE SPACES.
       01  AUX-CD-OPR.
           03 AUX-CD-OPR-ETRD-MCP      PIC S9(11)             VALUE 0.

      *----------------------------------------------------------------*
      * Book dos Containers e das subrotinas chamadas
      *----------------------------------------------------------------*

      * Book das variáveis auxiliares do tratamento de erro via MSG
       COPY SGTKERR1.

      * Book para captura de abend cics da bloco código MSGKP999
       COPY HLPKW999.

      * Book para captura de erro via MSG1
       COPY MSGKWMG1.

      * Book do container de requisição e resposta deste módulo
       COPY SGTK0217.

      * Verificar em qual estado está a operação -  SGTST009
       01 SGTST009-DADOS.
       COPY SGTKT009.

      * Efetuar consulta da carteira -  SGTST012
       01 SGTST012-DADOS.
       COPY SGTKT012.

      * Efetuar consulta o maior estado da versão -  SGTSPE03
       01 SGTSPE03-DADOS.
       COPY SGTKPE03.

      * Consultar estado e tipo da última versão da operação -  SGTSPE05
       01 SGTSPE05-DADOS.
       COPY SGTKPE05.

      * Efetuar consulta do estado da versão enviada -  SGTSC069
       01 SGTSC069-DADOS.
       COPY SGTKC069.

      * Efetuar inclusão do estado da versão enviada -  SGTIC069
       01 SGTSI069-DADOS.
       COPY SGTKI069.
      *--> Programa que faz Cancelamento Reserva OPR  -  SGTS005B
       01  SGTS005B-DADOS.
       COPY SGTK005B.
      * Book do Container de Erro
       COPY IIBKERRO.

      * Book do container de Serviços (para acionamento de serviços)
       COPY IIBKSRVC.

      * Book dos dados do usuário
       COPY IIBKOPRD.

       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*

      *--> Habilita Testes no Cobolito
       COPY ALMKCBL2.

           PERFORM 100000-INICIAR

           PERFORM 200000-PROCESSAR

           PERFORM 980000-GRAVAR-CONTAINER-RPST

           PERFORM 990000-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       100000-INICIAR                  SECTION.
      *----------------------------------------------------------------*
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *    Ativa a captura automática de abend do CICS.
      *    WHENEVER CICS
       COPY HLPKE999.

           PERFORM 970000-LER-CANAL

      *    Define o nome do programa p/ fins manipulação container
           MOVE 'SGTS0217'             TO NM-PGM

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

           PERFORM 974000-LER-DADOS-OPRD

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       110000-VALIDAR-REQUISICAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *    Valida campo a campo da requisição

           IF  SGTS0217-CD-CTRA-TIT-VL    EQUAL ZEROES
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0002               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -              'não informado.' TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0217-CD-OPR-ETRD-MCP   NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0217-CD-OPR-ETRD-MCP   EQUAL ZEROES
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0005               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' informado.'      TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0217-NR-VRS-OPR-ETRD   EQUAL ZEROES
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0007               TO  MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0217-TX-JST-EST     EQUAL SPACES
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0777              TO  MSG1-CD-ERRO-TS
               MOVE 'Texto observação não informado.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0217-CD-MTV-ALT     EQUAL ZEROES
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0763               TO  MSG1-CD-ERRO-TS
               MOVE 'Código do motivo para o estado da operação estrutur
      -             'ada não informado.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF
           .
       110000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           PERFORM 210000-VER-ULT-ESTADO-OPRD

           PERFORM 220000-VER-ULT-ESTADO-VRS

           PERFORM 270000-CONSULTA-CARTEIRA

      *   Para carteira de Recuperação Judicial, só deve existir na ba
      *   se operações com estado=5 encarteirada e estado da versão = 3
      *   aprovada
           IF SGTST012-IN-RCPO-JDCL EQUAL 'S'
              SET ERRO-PROGRAMADO      TO TRUE
              MOVE 783                TO MSG1-CD-ERRO-TS
              MOVE 'Operação em recuperação judicial não pode ser cancel
      -            'ada por essa opção.'
                                       TO MSG1-TX-ERRO-TS
              PERFORM 969000-MONITORAR-VARIAVEIS
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
              PERFORM 230000-VER-MAIOR-ESTADO-VRS

              EVALUATE SGTSC069-CD-TIP-EST-VRS
      *           Estado da versão : 1 - Em edição
                  WHEN 1
                      PERFORM 240000-VRS-EM-EDICAO-CTRA-CMUM
      *           Estado da versão : 2 - A avaliar
                  WHEN 2
                      PERFORM 250000-VRS-A-AVALIAR-CTRA-CMUM
      *           Estado da versão : 3 - Aprovada
                  WHEN 3
                      PERFORM 260000-VRS-APROVADA-CTRA-CMUM

                  WHEN OTHER
                     SET ERRO-PROGRAMADO     TO TRUE
                     MOVE 613                TO MSG1-CD-ERRO-TS
                     MOVE 'Cód.tipo de estado da versão é diferente de 1
      -              ',2 e 3.'               TO MSG1-TX-ERRO-TS
                     PERFORM 969000-MONITORAR-VARIAVEIS
                     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-EVALUATE
           END-IF
           .
       200000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       210000-VER-ULT-ESTADO-OPRD      SECTION.
      *----------------------------------------------------------------*
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar os campos de entrada
           MOVE SGTST009               TO NM-PGM

           SET SGTST009-CONSULTAR-EST-ATIVO    TO TRUE

           MOVE SGTS0217-CD-CTRA-TIT-VL    TO SGTST009-CD-CTRA-TIT-VL
           MOVE SGTS0217-CD-OPR-ETRD-MCP   TO SGTST009-CD-OPR-ETRD-MCP
           MOVE SGTS0217-NR-VRS-OPR-ETRD   TO SGTST009-NR-VRS-OPR-ETRD
           MOVE SPACES                     TO SGTST009-TS-EST-OPR-ETRD
                                              SGTST009-CD-USU-RSP-ATL

           PERFORM 930000-CHAMAR-SGTST009

           MOVE SGTST009-CD-TIP-EST-OPR    TO  SGTS0217-CD-TIP-EST-OPR
           .
       210000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       220000-VER-ULT-ESTADO-VRS       SECTION.
      *----------------------------------------------------------------*
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSC069               TO NM-PGM

      *    Movimentar os campos de entrada

           MOVE SGTS0217-CD-CTRA-TIT-VL TO SGTSC069-CD-CTRA-TIT-VL
           MOVE SGTS0217-CD-OPR-ETRD-MCP
                                        TO SGTSC069-CD-OPR-ETRD-MCP
           MOVE SGTS0217-NR-VRS-OPR-ETRD
                                        TO SGTSC069-NR-VRS-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO

           .
       220000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       230000-VER-MAIOR-ESTADO-VRS     SECTION.
      *----------------------------------------------------------------*
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSPE03               TO NM-PGM

      *    Movimentar os campos de entrada

           MOVE SGTS0217-CD-CTRA-TIT-VL TO SGTSPE03-CD-CTRA-TIT-VL
           MOVE SGTS0217-CD-OPR-ETRD-MCP
                                        TO SGTSPE03-CD-OPR-ETRD-MCP
           MOVE SGTS0217-NR-VRS-OPR-ETRD
                                        TO SGTSPE03-NR-VRS-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO
           .
       230000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       240000-VRS-EM-EDICAO-CTRA-CMUM  SECTION.
      *----------------------------------------------------------------*
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

      *   Estado da versão : 1 - Em edição

           EVALUATE SGTST009-CD-TIP-EST-OPR
      *   Estado da operação = 1 - Em edição e 5 - Encarteirado
               WHEN 1
                  IF  SGTSPE03-IN-EST-VRS-02   =  'S'
      *              Pode fazer cancelar cadastro
                     PERFORM 280000-CANC-CAD
                  ELSE
                     SET ERRO-PROGRAMADO     TO TRUE
                     MOVE 0782               TO MSG1-CD-ERRO-TS
                     MOVE 'Estado da operação não permite cancelamento d
      -                    'e cadastro, somente  exclusão cadastro.'
                                             TO MSG1-TX-ERRO-TS
                     PERFORM 969000-MONITORAR-VARIAVEIS
                     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
                  END-IF

               WHEN 5
                     SET ERRO-PROGRAMADO     TO TRUE
                     MOVE 0780               TO MSG1-CD-ERRO-TS
                     MOVE 'Estado da operação e da versão não permite ca
      -                    'ncelamento de cadastro, somente encerramento
      -                    '.'      TO MSG1-TX-ERRO-TS
                     PERFORM 969000-MONITORAR-VARIAVEIS
                     PERFORM 999000-GRAVA-ERRO-E-ENCERRA

               WHEN OTHER
                  SET ERRO-PROGRAMADO  TO TRUE
                  MOVE 614             TO MSG1-CD-ERRO-TS
                  MOVE 'Estado da operação incompativel com estado da
      -                'versão da operação.'
                                       TO MSG1-TX-ERRO-TS
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .
       240000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       250000-VRS-A-AVALIAR-CTRA-CMUM  SECTION.
      *----------------------------------------------------------------*
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

      *        Estado da versão : 2 - A avaliar

           EVALUATE SGTST009-CD-TIP-EST-OPR
      *       Estado da operação = 2 - Autorizar compra
               WHEN 2
                  IF SGTSPE03-IN-EST-VRS-02   =   'S'
                     PERFORM 280000-CANC-CAD
                  ELSE
                     SET ERRO-PROGRAMADO     TO TRUE
                     MOVE 0415               TO MSG1-CD-ERRO-TS
                     MOVE 'Estado da operação não permite cancelamento d
      -                    'e cadastro, somente  exclusão cadastro.'
                                             TO MSG1-TX-ERRO-TS
                     PERFORM 969000-MONITORAR-VARIAVEIS
                     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
                  END-IF
      *
               WHEN 5
                     SET ERRO-PROGRAMADO     TO TRUE
                     MOVE 0780               TO MSG1-CD-ERRO-TS
                     MOVE 'Estado da operação e da versão não permite ca
      -                    'ncelamento de cadastro, somente encerramento
      -                    '.'      TO MSG1-TX-ERRO-TS
                     PERFORM 969000-MONITORAR-VARIAVEIS
                     PERFORM 999000-GRAVA-ERRO-E-ENCERRA

               WHEN OTHER
                  MOVE 614             TO MSG1-CD-ERRO-TS
                  SET ERRO-PROGRAMADO  TO TRUE
                  MOVE 'Estado da operação incompativel com estado da
      -                'versão da operação.'
                                       TO MSG1-TX-ERRO-TS
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .
       250000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       260000-VRS-APROVADA-CTRA-CMUM         SECTION.
      *----------------------------------------------------------------*
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

      *        Estado da versão : 3 - Aprovada

           EVALUATE SGTST009-CD-TIP-EST-OPR
      *   Estado da operação = 3 - Disparar Ordem de Compra
      *   Estado da operação = 4 - A Encarteirar
               WHEN 3
               WHEN 4
                     PERFORM 280000-CANC-CAD
                     PERFORM 290000-CANC-RESERVA-OPR

      *   Estado da operação = 5 - Encarteirado
               WHEN 5
                  PERFORM 261000-VER-EST-ULT-VERSAO

                  IF  SGTSPE05-CD-TIP-EST-VRS   >  2

      *   A ação de TRANSFERIR PARA OUTRA CARTEIRA
      *   e a ação ENCERRAR OPERAÇÃO só deve ser
      *   permitida se a versão que o usuário selecionou na tela for
      *   a ultima e estiver aprovada (não pode existir nenhuma outra
      *   versão pendente)
                      IF  SGTSPE05-CD-TIP-EST-VRS EQUAL 3
                          AND SGTS0217-NR-VRS-OPR-ETRD EQUAL
                                         SGTSPE05-NR-VRS-OPR-ETRD
                          SET ERRO-PROGRAMADO     TO TRUE
                          MOVE 0781               TO MSG1-CD-ERRO-TS
                          MOVE 'Estado da operação não permite cancelame
      -                    'nto de cadastro, somente  encerramento.'
                                             TO MSG1-TX-ERRO-TS
                          PERFORM 999000-GRAVA-ERRO-E-ENCERRA
                      END-IF
                  ELSE
                      SET ERRO-PROGRAMADO     TO TRUE
                      MOVE 0784               TO MSG1-CD-ERRO-TS
                      MOVE 'Operação não pode ser cancelada.'
                                         TO MSG1-TX-ERRO-TS
                  END-IF

               WHEN OTHER
                  SET ERRO-PROGRAMADO  TO TRUE
                  MOVE 614             TO MSG1-CD-ERRO-TS
                  MOVE 'Estado da operação incompativel com estado da
      -                'versão da operação.'
                                       TO MSG1-TX-ERRO-TS
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           .
       260000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       261000-VER-EST-ULT-VERSAO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

      *    Consulta o estado e tipo da última versão da operação.
      *    Movimentar o nome do programa a ser executado

           MOVE SGTSPE05               TO NM-PGM

      *    Movimentar os campos de entrada

           MOVE SGTS0217-CD-CTRA-TIT-VL TO SGTSPE05-CD-CTRA-TIT-VL
           MOVE SGTS0217-CD-OPR-ETRD-MCP
                                        TO SGTSPE05-CD-OPR-ETRD-MCP

           PERFORM 900000-EXECUTAR-MODULO
           .
       261000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       270000-CONSULTA-CARTEIRA        SECTION.
      *----------------------------------------------------------------*
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar os campos de entrada
           SET SGTST012-CONSULTAR       TO TRUE
           MOVE SGTS0217-CD-CTRA-TIT-VL TO SGTST012-CD-CTRA-TIT-VL

           PERFORM 920002-CHAMAR-SGTST012
           .
       270000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       280000-CANC-CAD                 SECTION.
      *----------------------------------------------------------------*
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

      *    Incluir estado da operação
           MOVE SGTST009               TO NM-PGM

           SET SGTST009-INCLUIR        TO TRUE

           MOVE SGTS0217-CD-CTRA-TIT-VL    TO SGTST009-CD-CTRA-TIT-VL
           MOVE SGTS0217-CD-OPR-ETRD-MCP   TO SGTST009-CD-OPR-ETRD-MCP
           MOVE SGTS0217-NR-VRS-OPR-ETRD   TO SGTST009-NR-VRS-OPR-ETRD
           MOVE 6                          TO SGTST009-CD-TIP-EST-OPR
           MOVE 'CURRENT'                  TO SGTST009-TS-EST-OPR-ETRD
           MOVE SGTS0217-TX-JST-EST        TO SGTST009-TX-JST-EST
           MOVE SGTS0217-CD-MTV-ALT        TO SGTST009-CD-MTV-ALT
           MOVE KOPRD-CD-OPRD              TO SGTST009-CD-USU-RSP-ATL
                                              SGTST009-CD-USU-RSP-EST

           PERFORM 930000-CHAMAR-SGTST009

      *    Incluir estado da versão da operação
           MOVE SGTSI069               TO NM-PGM

           MOVE SGTS0217-CD-CTRA-TIT-VL    TO SGTSI069-CD-CTRA-TIT-VL
           MOVE SGTS0217-CD-OPR-ETRD-MCP   TO SGTSI069-CD-OPR-ETRD-MCP
           MOVE SGTS0217-NR-VRS-OPR-ETRD   TO SGTSI069-NR-VRS-OPR-ETRD
           MOVE 3                          TO SGTSI069-CD-TIP-EST-VRS
           MOVE SGTS0217-TX-JST-EST        TO SGTSI069-TX-OBS-EST-VRS

           PERFORM 900000-EXECUTAR-MODULO
           .
       280000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       290000-CANC-RESERVA-OPR             SECTION.
      *----------------------------------------------------------------*
           MOVE 0021                       TO MSG1-CD-SEQL-ERRO

      *    Cancela Reserva OPR
           MOVE SGTS005B                   TO NM-PGM

           MOVE SGTS0217-CD-CTRA-TIT-VL    TO SGTS005B-CD-CTRA-TIT-VL
           MOVE SGTS0217-CD-OPR-ETRD-MCP   TO SGTS005B-CD-OPR-ETRD-MCP
           MOVE SGTS0217-NR-VRS-OPR-ETRD   TO SGTS005B-NR-VRS-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO
           .
       290000-SAI.
           EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *----------------------------------------------------------------*
      * Executa um módulo implementado com CONTAINER                   *
      * Não incluir nessa section os módulos chamados via COMMAREA     *
      *                                                                *
      *----------------------------------------------------------------*
       900000-EXECUTAR-MODULO          SECTION.
      *----------------------------------------------------------------*
      *    Grava o container de requisição do módulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM

               WHEN SGTSC069
                   EXEC CICS LINK
                             PROGRAM ( SGTSC069  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSI069
                   EXEC CICS LINK
                             PROGRAM ( SGTSI069  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSPE03
                   EXEC CICS LINK
                             PROGRAM ( SGTSPE03  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSPE05
                   EXEC CICS LINK
                             PROGRAM ( SGTSPE05  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTS005B
                   EXEC CICS LINK
                             PROGRAM ( SGTS005B  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN OTHER
                   SET   ERRO-PROGRAMADO TO  TRUE
                    MOVE  9006           TO  MSG1-CD-ERRO-TS
                    STRING  'Não definida a execução do módulo'
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO            MSG1-TX-ERRO-TS
                   PERFORM 999000-ERRO
           END-EVALUATE
      *
           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET   ERRO-CICS    TO  TRUE
                    MOVE  3004         TO  MSG1-CD-ERRO-TS
                    STRING
                    'Erro  ao acionar o módulo  ' NM-PGM  ', '
                    'RESP  = '  MSG1-RESP1  ', '
                    'RESP2 = '  MSG1-RESP2
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

           IF NM-PGM NOT EQUAL SGTS005B
      *    Efetua a leitura do Container de Resposta
               PERFORM 973000-LER-RESPOSTA
           END-IF
           .
       900000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       920002-CHAMAR-SGTST012          SECTION.
      *----------------------------------------------------------------*
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      *    Movimento nome do módulo para monitoração de variáveis
           MOVE SGTST012               TO NM-PGM

           EXEC CICS LINK
                PROGRAM  (SGTST012)
                COMMAREA (SGTST012-DADOS)
           END-EXEC

           IF  SGTST012-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE SGTST012-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS
               MOVE SGTST012-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       920002-SAI.
           EXIT.
      *----------------------------------------------------------------*
       930000-CHAMAR-SGTST009          SECTION.
      *----------------------------------------------------------------*
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

      *    Movimento nome do módulo para monitoração de variáveis
           MOVE SGTST009               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST009)
                COMMAREA (SGTST009-DADOS)
           END-EXEC

           IF  SGTST009-CD-RTN-PGM     NOT EQUAL ZEROS AND 100
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND
               MOVE SGTST009-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE SGTST009-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       930000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*
           EVALUATE NM-PGM

                WHEN SGTST009
                    MOVE 'SGTST009-ACAO'         TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST009-ACAO           TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST009-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST009-CD-CTRA-TIT-VL TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST009-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST009-CD-OPR-ETRD-MCP
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST009-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST009-NR-VRS-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST009-TS-EST-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST009-TS-EST-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST009-CD-USU-RSP-ATL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST009-CD-USU-RSP-ATL TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTST012
                    MOVE 'SGTST012-ACAO'         TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST012-ACAO           TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST012-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST012-CD-CTRA-TIT-VL TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSC069
                    MOVE 'SGTSC069-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSC069-CD-CTRA-TIT-VL TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSC069-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSC069-CD-OPR-ETRD-MCP
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSC069-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSC069-NR-VRS-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSC069-TS-EST-VRS'   TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSC069-TS-EST-VRS     TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSI069
                    MOVE 'SGTSI069-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI069-CD-CTRA-TIT-VL TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI069-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI069-CD-OPR-ETRD-MCP
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI069-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI069-NR-VRS-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI069-CD-TIP-EST-VRS'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI069-CD-TIP-EST-VRS TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI069-TX-OBS-EST-VRS'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI069-TX-OBS-EST-VRS TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSPE03
                    MOVE 'SGTSPE03-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSPE03-CD-CTRA-TIT-VL TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSPE03-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSPE03-CD-OPR-ETRD-MCP
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSPE03-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSPE03-NR-VRS-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSPE05
                    MOVE 'SGTSPE05-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSPE05-CD-CTRA-TIT-VL TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSPE05-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSPE05-CD-OPR-ETRD-MCP
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTS005B
                    MOVE 'SGTS005B-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTS005B-CD-CTRA-TIT-VL TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTS005B-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTS005B-CD-OPR-ETRD-MCP
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTS005B-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTS005B-NR-VRS-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN OTHER
                    CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      * SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
      *                                                                *
      *----------------------------------------------------------------*
       970000-LER-CANAL                SECTION.
      *    Obtem o canal corrente
      *----------------------------------------------------------------*
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

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
      *----------------------------------------------------------------*
       971000-LER-REQUISICAO           SECTION.
      *----------------------------------------------------------------*
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisição
           MOVE 'SGTS0217'             TO NM-PGM
           MOVE 'SGTS0217-RQSC'        TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0217-RQSC)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       971000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *----------------------------------------------------------------*
      *    A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *----------------------------------------------------------------*

      *    Le o container de erro dos modulos chamados
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
      *        PARA O MÓDULO CHAMADO e tratar na section chamadora
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                    MOVE  NM-PGM       TO  MSG1-NM-PGM-ACND

                    IF NM-PGM(1:3) EQUAL 'SGT'
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

                       MOVE IIBKERRO-TX-ERRO
                                          TO MSG1-TX-RTN-PGM-ACND
                       MOVE IIBKERRO-CD-ERRO
                                          TO MSG1-CD-RTN-PGM-ACND

                    END-IF
      *             Monitorar variáveis do módulo chamado
                       PERFORM 969000-MONITORAR-VARIAVEIS
                       PERFORM 999000-ERRO
               WHEN OTHER
                    SET ERRO-CONTAINER  TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       973000-LER-RESPOSTA             SECTION.
      *----------------------------------------------------------------*
      *    Recupera o container com os dados de resposta (saída).
      *
      *                        NOME DO CONTAINER RESPOSTA
      *                        --------------------------
      *
      *    Módulo CONTAINER  -     NOME_PGM-RPST
      *    Módulo IIB (FIXO) -     PRM-DADOS-RPST
      *
      *---------------------------------------------------------------*
      *    Monta o nome dos container de acordo com módulo a executar
           MOVE SPACES                 TO NM-CTNR

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_módulo -RPST
           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RPST'       TO NM-CTNR(9:5)

      *    Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

      *Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Lê a RESPOSTA de acordo com o programa que a gravou

           EVALUATE NM-PGM

               WHEN SGTSC069
                    MOVE LENGTH OF SGTSC069-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSC069-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSI069
                    MOVE LENGTH OF SGTSI069-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI069-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSPE03
                    MOVE LENGTH OF SGTSPE03-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSPE03-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSPE05
                    MOVE LENGTH OF SGTSPE05-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSPE05-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9003         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na leitura da resposta do módulo '
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       973100-LER-CONTAINER-NODATA     SECTION.
      *----------------------------------------------------------------*
      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Obtém a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973100-SAI.
           EXIT.
      *----------------------------------------------------------------*
       974000-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

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
       974000-SAI.
           EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVAÇÃO DE DADOS     *
      *                                                                *
      *----------------------------------------------------------------*
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *----------------------------------------------------------------*

      *    Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de resposta
           MOVE 'SGTS0217-RPST'     TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                    )
                     FROM      (SGTS0217-RPST          )
                     FLENGTH   (LENGTH OF SGTS0217-RPST)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *----------------------------------------------------------------*
      *    Postar container com dados de requisição (entrada)
      *
      *                        NOME DO CONTAINER Requisição
      *                        ----------------------------
      *
      *    Módulo CONTAINER  -     NOME_PGM-RQSC
      *    Módulo IIB (FIXO) -     PRM-DADOS-RQSC
      *
      *----------------------------------------------------------------*
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome dos container de acordo com módulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_módulo -RPST
           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RQSC'       TO NM-CTNR(9:5)

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisição do programa a executar
           EVALUATE NM-PGM

               WHEN SGTSI069
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSI069-RQSC )
                              FLENGTH   (LENGTH OF SGTSI069-RQSC)
                    END-EXEC

               WHEN SGTSC069
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSC069-RQSC )
                              FLENGTH   (LENGTH OF SGTSC069-RQSC)
                    END-EXEC

               WHEN SGTSPE03
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSPE03-RQSC )
                              FLENGTH   (LENGTH OF SGTSPE03-RQSC)
                    END-EXEC

               WHEN SGTSPE05
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSPE05-RQSC )
                              FLENGTH   (LENGTH OF SGTSPE05-RQSC)
                    END-EXEC

               WHEN SGTS005B
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTS005B-RQSC )
                              FLENGTH   (LENGTH OF SGTS005B-RQSC)
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9004         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na gravação da requisição do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       982000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em módulos executados mais de uma vez dentro da mesma sessão do*
      * CICS. Equivale ao initialize na área de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
           MOVE 0020                   TO MSG1-CD-SEQL-ERRO

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_módulo -RPST
           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RPST'       TO NM-CTNR(9:5)

           SET DEL-CONTAINER           TO TRUE

      *    Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER (NM-CTNR)
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

      *    Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *----------------------------------------------------------------*

      *    Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE 'PRM-ERRO'             TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER (NM-CTNR)
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
           .
       983100-SAI.
           EXIT.
      *----------------------------------------------------------------*
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *----------------------------------------------------------------*
      *    Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS PUT
                     CONTAINER ( 'PRM-ERRO'                  )
                     FROM      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      (MSG1-RESP1                   )
                     RESP2     (MSG1-RESP2                   )
           END-EXEC
           .
       984000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       990000-ENCERRAR                 SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                RETURN
           END-EXEC
           .
       990000-ENCERRAR-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
      *                                                                *
      * As sections abaixo iniciadas com 999000 representam os trata-  *
      * mentos de erro utilizando o Sistema MSG (rotina MSGS0001)      *
      *                                                                *
      *----------------------------------------------------------------*
       999000-ERRO                     SECTION.
      *----------------------------------------------------------------*

           IF MSG1-CD-ERRO-TS EQUAL ZEROS
              MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF

           IF MSG1-TX-ERRO-TS EQUAL SPACES
              MOVE 'Erro não especificado no módulo.' TO MSG1-TX-ERRO-TS
           END-IF

           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-ERRO-SAI.
           EXIT.
      *----------------------------------------------------------------*
      *     As sections abaixo representam os tratamentos de erro
      *    utilizando o sistema MSG de mensageria (rotina MSGS0001)
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *    Em caso de erro, desfaz as atualizações em banco de dados

           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC

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

      *Adiciona as variáveis de entrada para monitoração
           MOVE 'SGTS0217-CD-CTRA-TIT-VL'        TO  MSGKW999-NM-VRV-MNT
           MOVE SGTS0217-CD-CTRA-TIT-VL          TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0217-CD-OPR-ETRD-MCP'       TO MSGKW999-NM-VRV-MNT
           IF  SGTS0217-CD-OPR-ETRD-MCP          IS  NUMERIC
               MOVE SGTS0217-CD-OPR-ETRD-MCP     TO AUX-CD-OPR-ETRD-MCP
               MOVE  AUX-CD-OPR                  TO MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Código não numérico.'       TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0217-NR-VRS-OPR-ETRD'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTS0217-NR-VRS-OPR-ETRD         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0217-TX-JST-EST'            TO MSGKW999-NM-VRV-MNT
           MOVE SGTS0217-TX-JST-EST              TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0217-CD-MTV-ALT'            TO MSGKW999-NM-VRV-MNT
           MOVE SGTS0217-CD-MTV-ALT              TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *    Grava o erro gerado no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG

           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO

      *    Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

      *    Encerra a transação
           PERFORM 990000-ENCERRAR
           .
       999000-SAI.
           EXIT.
      * --------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * --------------------------------------*
       COPY HLPKP999.
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
       COPY SGTKP999.
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
       COPY MSGKPMG1.
      *---------------------- Fim SGTS0217 ----------------------------*
