      *-----------------------------------------------------------------
      * Programa..: SGTS0239
      * Analista..: F8785286 - Roseli Akemi Kawahata
      * Autor.....: F8785286 - Roseli Akemi Kawahata
      * Data......: 04/02/2019
      * Objetivo..: A partir dos dados de entrada efetua a inclusão do
      *             rating do Cliente Garantidor da Oper Estruturada.
      * Compilacao: Cobol/CICS
      *-----------------------------------------------------------------
      * VRS001 04/02/2018 - F8785286 - DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0239.
      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
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
       77  CTE-PROG                    PIC  X(18) VALUE
                                       '*** SGTS0239 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *--> Definição dos Nomes dos Programas Acionados
       77  IIBP2000                    PIC  X(08) VALUE 'IIBP2000'.
       77  SGTS0075                    PIC  X(08) VALUE 'SGTS0075'.
       77  SGTSL006                    PIC  X(08) VALUE 'SGTSL006'.
       77  SGTST006                    PIC  X(08) VALUE 'SGTST006'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

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

      *--> Book do container de requisição e resposta deste modulo
       01 SGTS0239-DADOS.
-INC SGTK0239

      * LISTA DA TABELA DB2SGT.CLI_OPR_ETRD
       01  SGTSL006-DADOS.
-INC SGTKL006

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      *--> Consultar Vínculo do Contato Interno da Operação
       01  SGTS0075-DADOS.
-INC SGTK0075

      *--> CRUD da Tabela CLI_OPR_ETRD - Commarea
       01  SGTST006-DADOS.
-INC SGTKT006

       77  CTE-FINAL-SS                PIC X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

      *--> Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM  100000-INICIAR

           PERFORM  200000-PROCESSAR

           PERFORM  999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  970000-LER-CANAL

      *--> Define o nome do programa p/ fins manipulacao container
           MOVE  'SGTS0239'            TO  NM-PGM

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM  971000-LER-REQUISICAO

           PERFORM  974000-LER-DADOS-OPRD

           PERFORM  110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *--> Valida campo a campo da requisicao
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada
           IF  SGTS0239-CD-CTRA-TVM        LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0239-CD-CTRA-TVM        EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0239-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0239-CD-OPR-ETRD-MCP    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'   TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0239-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0239-NR-VRS-OPR-ETRD    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0239-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validar Pessoa de Contato Interno
           IF  SGTS0239-CD-CLI             LESS ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0188              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do cliente inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0239-CD-CLI             EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0189              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do cliente não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF
           .
       110000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  210000-CONSULTAR-RATING-GAR

           IF  SGTSL006-QT-OCR-TAB     >   0
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0191              TO  MSG1-CD-ERRO-TS
               MOVE  'Vinculação não permitida. Cliente já existente na
      -              'Operação Estruturada.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           PERFORM  220000-INCLUIR-RATING-GAR
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       210000-CONSULTAR-RATING-GAR     SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTS0075-DADOS.

           MOVE  SGTSL006              TO  NM-PGM
           SET SGTSL006-IN-FUC-ATV     TO  TRUE

      *--> Movimentar os campos de entrada
           MOVE  SGTS0239-CD-CTRA-TVM
                                       TO  SGTSL006-CD-CTRA-TIT-VL
           MOVE  SGTS0239-CD-OPR-ETRD-MCP
                                       TO  SGTSL006-ULT-OPR-ETRD-MCP
           MOVE  SGTS0239-NR-VRS-OPR-ETRD
                                       TO  SGTSL006-ULT-VRS-OPR-ETRD
           MOVE  SGTS0239-CD-CLI       TO  SGTSL006-ULT-CD-CLI
           MOVE  7                     TO  SGTSL006-CD-TIP-FUC-CLI-E

           PERFORM  910000-CHAMAR-COM-SGTSL006
           .
       210000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       220000-INCLUIR-RATING-GAR       SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTST006-DADOS

      *--> Movimentar os campos de entrada
           SET   SGTST006-INCLUIR      TO  TRUE
           MOVE  SGTS0239-CD-CTRA-TVM  TO  SGTST006-CD-CTRA-TIT-VL
           MOVE  SGTS0239-CD-OPR-ETRD-MCP
                                       TO  SGTST006-CD-OPR-ETRD-MCP
           MOVE  SGTS0239-NR-VRS-OPR-ETRD
                                       TO  SGTST006-NR-VRS-OPR-ETRD
           MOVE  SGTS0239-CD-CLI
                                       TO  SGTST006-CD-CLI
           MOVE  0007                  TO  SGTST006-CD-TIP-FUC-CLI
           MOVE  KOPRD-CD-OPRD         TO  SGTST006-CD-USU-RSP-ATL
           MOVE  'CURRENT'             TO  SGTST006-TS-ULT-ATL

           PERFORM  920000-CHAMAR-COMMAREA
           .
       220000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      *   Section da Faixa de 900000 a 969900 para Demais Rotinas
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *--> Executa um modulo implementado com CONTAINER
      *-----------------------------------------------------------------

      *--> Grava o container de requisicao do modulo a executar
           PERFORM  982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE  NM-PGM
               WHEN  SGTS0075
                     EXEC CICS LINK
                               PROGRAM ( SGTS0075  )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9006        TO  MSG1-CD-ERRO-TS
                     STRING  'Não definida a execução do módulo ' NM-PGM
                             DELIMITED BY SIZE
                             INTO  MSG1-TX-ERRO-TS
                     END-STRING
                     PERFORM  999000-ERRO
           END-EVALUATE

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     STRING  'Erro na execução do módulo ' NM-PGM ', '
                             'RESP  = '    MSG1-RESP1  ', '
                             'RESP2 = '    MSG1-RESP2
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                    END-STRING
                    MOVE  3004         TO  MSG1-CD-ERRO-TS
                    SET   ERRO-CICS    TO  TRUE
                    PERFORM  999000-ERRO
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM  972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a leitura do Container de Resposta
           PERFORM  973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       910000-CHAMAR-COM-SGTSL006      SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE  SGTSL006              TO  NM-PGM

           EXEC CICS LINK
                     PROGRAM  (SGTSL006)
                     COMMAREA (SGTSL006-DADOS)
           END-EXEC

           IF  SGTSL006-CD-RTN-PGM         NOT EQUAL ZEROS  AND
               SGTSL006-CD-RTN-PGM         NOT EQUAL +100
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTSL006-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
               MOVE  SGTSL006-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
                                           MSG1-CD-ERRO-TS
      *        Monitorar variáveis do módulo chamado
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF
           .
       910000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       920000-CHAMAR-COMMAREA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE  SGTST006              TO  NM-PGM

           EXEC CICS LINK
                     PROGRAM  (SGTST006)
                     COMMAREA (SGTST006-DADOS)
           END-EXEC

           IF  SGTST006-CD-RTN-PGM         NOT EQUAL ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST006-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
               MOVE  SGTST006-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
                                           MSG1-CD-ERRO-TS
      *        Monitorar variáveis do módulo chamado
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF
           .
       920000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM
               WHEN  SGTS0075
                     MOVE 'SGTS0075-CD-CTRA-TVM' TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0075-CD-CTRA-TVM  TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTS0075-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTS0075-CD-OPR-ETRD-MCP    NUMERIC
                         MOVE  SGTS0075-CD-OPR-ETRD-MCP
                                                 TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                                 TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTS0075-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0075-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTS0075-CD-CLI'      TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0075-CD-CLI       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTS0075-CD-TIP-FUC-CLI'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0075-CD-TIP-FUC-CLI
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTS0075-IN-VCLC-CTT-ITNO-OPR'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0075-IN-VCLC-CTT-ITNO-OPR
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSL006
                     MOVE 'SGTSL006-ACAO'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL006-ACAO
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL006-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL006-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL006-ULT-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSL006-ULT-OPR-ETRD-MCP    NUMERIC
                         MOVE  SGTSL006-ULT-OPR-ETRD-MCP
                                                 TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                                 TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL006-ULT-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL006-ULT-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL006-ULT-CD-CLI'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL006-ULT-CD-CLI
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL006-ULT-TIP-FUC-CLI'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL006-ULT-TIP-FUC-CLI
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL006-DT-EMS-TIT-OPR'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL006-DT-EMS-TIT-OPR
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL006-CD-TIP-FUC-CLI-E'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL006-CD-TIP-FUC-CLI-E
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL006-CD-RTN-PGM'  TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL006-CD-RTN-PGM   TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL006-MSG-ERRO-PGM'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL006-MSG-ERRO-PGM TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTST006
                     MOVE 'SGTST006-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST006-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST006-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTST006-CD-OPR-ETRD-MCP    NUMERIC
                         MOVE  SGTST006-CD-OPR-ETRD-MCP
                                                 TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                                 TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST006-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST006-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST006-CD-CLI'      TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST006-CD-CLI       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST006-CD-TIP-FUC-CLI'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST006-CD-TIP-FUC-CLI
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST006-DT-FIM-VLD-FUC'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST006-DT-FIM-VLD-FUC
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST006-TS-ULT-ATL'  TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST006-TS-ULT-ATL   TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST006-CD-USU-RSP-ATL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST006-CD-USU-RSP-ATL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST006-CD-RTN-PGM'  TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST006-CD-RTN-PGM   TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST006-MSG-ERRO-PGM'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST006-MSG-ERRO-PGM TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST006-ACAO'        TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST006-ACAO         TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                     CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      *    Section da Faixa de 970000 a 979900 para Leitura de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       970000-LER-CANAL                SECTION.
      *--> Obtem o canal corrente
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       970000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------

      *--> Monta o nome do container de requisicao
           MOVE  'SGTS0239'            TO  NM-PGM
           MOVE  'SGTS0239-RQSC'       TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM  973100-LER-CONTAINER-NODATA

           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

      *--> Obtem o tamanho esperado da requisicao
           MOVE  LENGTH OF SGTS0239-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisicao X tamanho recebido

           IF  QT-CCT-LIDO                 NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA
                                       TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0239-RQSC)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       971000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *--> A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

      *--> Le o container de erro dos modulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           INITIALIZE                  IIBKERRO-PRM-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                     )
                      INTO      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      RESP      ( MSG1-RESP1                  )
                      RESP2     ( MSG1-RESP2                  )
           END-EXEC

           EVALUATE  TRUE
      *        Caso deseje personalizar ou não interromper o
      *        processamento, alterar para não dar erro aqui SOMENTE
      *        PARA O MÓDULO CHAMADO e tratar na section chamadora
               WHEN  MSG1-RESP1            EQUAL DFHRESP(CONTAINERERR)
                     CONTINUE
               WHEN  MSG1-RESP1            EQUAL DFHRESP(NORMAL)
                     SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                     MOVE  NM-PGM      TO  MSG1-NM-PGM-ACND
                     IF  NM-PGM(1:3)       EQUAL 'SGT'
                         MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                                           MSG1-CD-ERRO-TS
                         MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
                     ELSE
                         MOVE  3004    TO  MSG1-CD-ERRO-TS
                         STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                                 DELIMITED BY SIZE
                                 INTO      MSG1-TX-ERRO-TS
                         MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                         MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                     END-IF
      *              Monitorar variáveis do módulo chamado
                     PERFORM  969000-MONITORAR-VARIAVEIS
                     PERFORM  999000-ERRO
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *--> Recupera o container com os dados de resposta (saída).
      *    Módulo CONTAINER  -     NOME_PGM-RPST
      *    Módulo IIB (FIXO) -     PRM-DADOS-RPST
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)

      *--> Recupera o tamanho do container de resposta
           PERFORM  973100-LER-CONTAINER-NODATA

      *--> Configura dados para leitura do container de resposta
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Le a RESPOSTA de acordo com o programa que a gravou
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM
      *        Calcular a quantidade de caracteres esperado e limpar
      *        a area de dados antes de executar o GET
      *        Para matrizes com dados variaveis (clausula depending
      *        on) nao ha como utilizar o comando INITIALIZE

               WHEN  SGTS0075
                     INITIALIZE            SGTS0075-RPST
                     EXEC CICS GET
                               CONTAINER (NM-CTNR          )
                               INTO      (SGTS0075-RPST    )
                               FLENGTH   (QT-CCT-LIDO      )
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9003        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na leitura da resposta do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                    PERFORM  999000-ERRO
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Obtem a quantidade de caracteres no container
           MOVE  ZEROS                 TO  QT-CCT-LIDO

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR     )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       974000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-INF-OPRD'        TO  NM-CTNR

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           INITIALIZE                      PRM-INF-OPRD

           EXEC CICS GET
                     CONTAINER ( NM-CTNR                )
                     INTO      ( PRM-INF-OPRD           )
                     FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       974000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      *   Section da Faixa de 980000 a 989900 para Gravação de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *--> Postar container com dados de requisição (entrada)
      *    Modulo CONTAINER  -     NOME_PGM-RQSC
      *    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
      *-----------------------------------------------------------------
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RQSC'               TO  NM-CTNR(9:5)

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Faz PUT da requisicao do programa a executar
           EVALUATE  NM-PGM
               WHEN  SGTS0075
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                 )
                                FROM      ( SGTS0075-RQSC           )
                                FLENGTH   ( LENGTH OF SGTS0075-RQSC )
                     END-EXEC

               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9004        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na gravação da requisição do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     PERFORM  999000-ERRO
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       982000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *--> Exclui o container de resposta e de erro para evitar
      *    problemas em módulos executados mais de uma vez dentro
      *    da mesma sessão do CICS.
      *    Equivale ao initialize na área de resposta dos
      *    programas que utilizam COMMAREA.
      *-----------------------------------------------------------------
           MOVE  0017                  TO  MSG1-CD-SEQL-ERRO

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)

           SET   DEL-CONTAINER         TO  TRUE

      *--> Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR    )
                      RESP      ( MSG1-RESP1 )
                      RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *--> Excluir o container de erro
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------
           MOVE  0018                  TO  MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE                  IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           SET   DEL-CONTAINER         TO  TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR    )
                      RESP      ( MSG1-RESP1 )
                      RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *--> Grava o erro gerado no container de erro
      *-----------------------------------------------------------------

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                  )
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
      *    tratamentos de erro através do Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-ERRO                     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL SPACES
               MOVE  'Erro não específicado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

           PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *--> Em caso de erro, desfaz as atualizações em banco de dados
           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC

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

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE 'CD-CTRA-TVM'                  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0239-CD-CTRA-TVM          TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'CD-OPR-ETRD-MCP'              TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0239-CD-OPR-ETRD-MCP    NUMERIC
               MOVE  SGTS0239-CD-OPR-ETRD-MCP  TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX        TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'      TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'NR-VRS-OPR-ETRD'              TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0239-NR-VRS-OPR-ETRD      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'CD-CLI'                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0239-CD-CLI               TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS               TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS               TO  IIBKERRO-TX-ERRO

      *--> Grava o erro gerado no container de erro
           PERFORM  984000-GRAVAR-CONTAINER-ERRO

      *--> Encerra a transacao
           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      *---------------------------------------
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
      *---------------------- FIM SGTS0239 -----------------------------