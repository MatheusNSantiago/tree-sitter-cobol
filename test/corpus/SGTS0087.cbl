      *-----------------------------------------------------------------
      * Programa..: SGTS0087
      * Analista..: F2054109 Cintia Ap Soares Iglesias
      * Autor.....: F2054109 Cintia Ap Soares Iglesias
      * Data......: 15/08/2016
      * Objetivo..: Listar Bancos Participantes da Operação
      * Compilacao: Cobol/CICS
      *-----------------------------------------------------------------
      * VRS001 15/08/2016 - F2054109 - DE 2015/145195 - Implantacao
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0087.

      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------

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
                                       '*** SGTS0087 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *--> Definição dos Nomes dos Módulos Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  CAFS0053                PIC  X(08) VALUE 'CAFS0053'.
           03  IIBP2000                PIC  X(08) VALUE 'IIBP2000'.
           03  SGTSL064                PIC  X(08) VALUE 'SGTSL064'.

      *--> Definição dos Variaveis do Cobolito utilizado em Testes
-INC ALMKCBL1

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Areas de trabalho

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-TX.
           03  CD-OPR-ETRD             PIC  9(11).

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book para captura de abend cics da bloco codigo HLPKP999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book do container de requisicao e resposta deste modulo
       01  SGTS0087-DADOS.
-INC SGTK0087

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      *--> Recuperar Nomes de Bancos Participantes
       01  CAFS0053-DADOS.
-INC CAFK0053

      *--> Listar Bancos Participantes
-INC SGTKL064

       77  CTE-FINAL-SS                PIC X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

      *--> Habilita Testes no Cobolito
-INC ALMKCBL2

      *--> Ativa a captura automatica de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM  100000-INICIAR

           PERFORM  200000-PROCESSAR

           PERFORM  990000-ENCERRAR

           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  970000-LER-CANAL

      *--> Define o nome do programa p/ fins manipulacao container
           MOVE  'SGTS0087'            TO  NM-PGM

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM  971000-LER-REQUISICAO

           PERFORM  110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *--> Valida campo a campo da requisicao
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTS0087-CD-CTRA-TIT-VL < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
                MOVE  0001          TO  MSG1-CD-ERRO-TS
                MOVE  'Código da carteira de título e valores mobiliário
      -                's inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0087-CD-CTRA-TIT-VL EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002          TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliá
      -               'rios não informado.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF


           IF  SGTS0087-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0087-CD-OPR-ETRD-MCP    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'    TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0087-NR-VRS-OPR-ETRD    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0087-CD-TIP-PRTC        NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0008              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do tipo de participação inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0087-CD-ULT-BCO-PCT     LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0009              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do último banco participante inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  210000-LISTAR-BANCOS

           PERFORM  220000-TRATAR-LISTA-BANCOS
                    VARYING SGTSL064-IC-TAB
                    FROM  0001 BY 0001
                    UNTIL SGTSL064-IC-TAB > SGTSL064-QT-OCR-TAB

           MOVE  SGTSL064-QT-OCR-TAB   TO  SGTS0087-QT-OCR
           MOVE  SGTSL064-IN-CNTO-PSQ  TO  SGTS0087-IN-CTNC-PSQ

           PERFORM  980000-GRAVAR-CONTAINER-RPST

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-LISTAR-BANCOS            SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSL064              TO  NM-PGM

           MOVE  SGTS0087-CD-CTRA-TIT-VL  TO  SGTSL064-CD-CTRA-TIT-VL
           MOVE  SGTS0087-CD-OPR-ETRD-MCP
                                       TO  SGTSL064-CD-OPR-ETRD-MCP
           MOVE  SGTS0087-NR-VRS-OPR-ETRD
                                       TO  SGTSL064-NR-VRS-OPR-ETRD
           MOVE  SGTS0087-CD-TIP-PRTC  TO  SGTSL064-CD-TIP-PRTC

           MOVE  SGTS0087-CD-ULT-BCO-PCT
                                       TO  SGTSL064-CD-ULT-BCO-PCT

           PERFORM  900000-EXECUTAR-MODULO

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-TRATAR-LISTA-BANCOS      SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTSL064-CD-BCO-PCT                   (SGTSL064-IC-TAB)
             TO  SGTS0087-CD-BCO-PCT                   (SGTSL064-IC-TAB)
           MOVE  SGTSL064-IN-BCO-LDR                   (SGTSL064-IC-TAB)
             TO  SGTS0087-IN-BCO-LDR                   (SGTSL064-IC-TAB)
           MOVE  SGTSL064-PC-PRTC-BCO                  (SGTSL064-IC-TAB)
             TO  SGTS0087-PC-PRTC-BCO                  (SGTSL064-IC-TAB)
           MOVE  SGTSL064-CD-USU-ULT-ATL               (SGTSL064-IC-TAB)
             TO  SGTS0087-CD-USU-ULT-ATL               (SGTSL064-IC-TAB)
           MOVE  SGTSL064-TS-ULT-ATL                   (SGTSL064-IC-TAB)
             TO  SGTS0087-TS-ULT-ATL                   (SGTSL064-IC-TAB)

           PERFORM  221000-CONSULTAR-BANCO

           MOVE  S053-NM-INST
             TO  SGTS0087-NM-BCO-PCT                   (SGTSL064-IC-TAB)
           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       221000-CONSULTAR-BANCO          SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE  CAFS0053              TO  NM-PGM

           INITIALIZE                      CAFS0053-DADOS

      *--> Movimentar os campos de entrada
           MOVE  0002                  TO  S053-FUC
           MOVE  SGTSL064-CD-BCO-PCT                   (SGTSL064-IC-TAB)
                                       TO  S053-CD-CPSO OF S053-ENTD

           PERFORM  910000-CHAMAR-SERVICO

           IF  S053-CD-RTN                 NOT EQUAL  ZEROS
               MOVE  SPACES            TO  S053-NM-INST
           END-IF

           .
       221000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *  Section da Faixa de 900000 a 969900 para demais rotinas
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *--> Executa um modulo implementado com CONTAINER
      *    Nao incluir nessa section os modulos chamados via COMMAREA
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

      *--> Grava o container de requisicao do modulo a executar
           PERFORM  982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE  NM-PGM
               WHEN  SGTSL064
                     EXEC CICS  LINK
                                PROGRAM ( SGTSL064                     )
                                CHANNEL ( NM-CNL                       )
                                RESP    ( MSG1-RESP1                   )
                                RESP2   ( MSG1-RESP2                   )
                     END-EXEC
               WHEN  OTHER
               MOVE  9006              TO  MSG1-CD-ERRO-TS
               STRING  'Não definida a execução do módulo '
                        NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO  MSG1-TX-ERRO-TS
                     PERFORM  999000-ERRO
           END-EVALUATE

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     MOVE  3004              TO  MSG1-CD-ERRO-TS
                     STRING  'Erro ao acionar o módulo ' NM-PGM ', '
                             'RESP  ='   ' ' MSG1-RESP1  ', '
                             'RESP2 =' ' ' MSG1-RESP2
                             DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
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

      *----------------------------------------------------------------
       910000-CHAMAR-SERVICO           SECTION.
      *--> Acionar programa IIBP2000 para requisição do serviço.
      *----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

      *--> Grava o container com dados do serviço/operação a chamar
           PERFORM  981000-GRAVAR-CONTAINER-SRVC

      *--> Grava o container de requisicao do modulo a executar
           PERFORM  982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  LINK
                      PROGRAM ( IIBP2000                               )
                      CHANNEL ( NM-CNL                                 )
                      RESP    ( MSG1-RESP1                             )
                      RESP2   ( MSG1-RESP2                             )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     MOVE  9000              TO  MSG1-CD-ERRO-TS
                     STRING  'Erro ao acionar serviço: '
                             SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
                             SRVC-ID-OPR  '.' SRVC-VRS-OPR
                             'via IIBP2000, '
                             'RESP='       MSG1-RESP1
                             ', RESP-2='   MSG1-RESP2 '.'
                             DELIMITED BY SIZE
                             INTO MSG1-TX-ERRO-TS
                     SET   ERRO-CICS   TO  TRUE
                     PERFORM  999000-ERRO
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM  972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a leitura do Container de Resposta
           PERFORM  973000-LER-RESPOSTA

           .
       910000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM
               WHEN  SGTSL064
                     MOVE  'SGTSL064-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL064-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL064-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSL064-CD-OPR-ETRD-MCP  NUMERIC
                         MOVE  SGTSL064-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD
                         MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL064-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL064-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL064-CD-TIP-PRTC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL064-CD-TIP-PRTC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL064-CD-ULT-BCO-PCT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL064-CD-ULT-BCO-PCT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  CAFS0053
                     MOVE  'S053-FUC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  S053-FUC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'S053-CD-CPSO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  S053-CD-CPSO OF S053-ENTD
                                       TO  MSGKW999-VL-VRV-MNT
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
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS  ASSIGN
                      CHANNEL ( NM-CNL                                 )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       970000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao
           MOVE  'SGTS0087'            TO  NM-PGM
           MOVE  'SGTS0087-RQSC'       TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM  973100-LER-CONTAINER-NODATA

      *--> Obtem o tamanho esperado da requisicao
           MOVE  LENGTH OF SGTS0087-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisicao X tamanho recebido

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA
                                       TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                              )
                      FLENGTH   ( QT-CCT-LIDO                          )
                      INTO      ( SGTS0087-RQSC                        )
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
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

      *--> Le o container de erro dos modulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                              )
                      INTO      ( IIBKERRO-PRM-ERRO                    )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO          )
                      RESP      ( MSG1-RESP1                           )
                      RESP2     ( MSG1-RESP2                           )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
                     CONTINUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     SET   ERRO-PROGRAMA-ACIONADO TO  TRUE
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
      *    Modulo CONTAINER  -     NOME_PGM-RPST
      *    Modulo IIB (FIXO) -     PRM-DADOS-RPST
      *----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           EVALUATE  NM-PGM
               WHEN  CAFS0053
                     MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN  OTHER
                     MOVE  NM-PGM       TO  NM-CTNR(1:8)
                     MOVE  '-RPST'      TO  NM-CTNR(9:5)
           END-EVALUATE

      *--> Recupera o tamanho do container de resposta
           PERFORM  973100-LER-CONTAINER-NODATA

      *--> Configura dados para leitura do container de resposta
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE


      *--> Le a RESPOSTA de acordo com o programa que a gravou
           EVALUATE  NM-PGM
      *        Calcular a quantidade de caracteres esperado e limpar
      *        a area de dados antes de executar o GET
      *        Para matrizes com dados variaveis (clausula depending
      *        on) nao ha como utilizar o comando INITIALIZE

               WHEN  SGTSL064
                     MOVE  LENGTH OF SGTSL064-RPST
                                       TO  QT-CCT-EPRO
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR                    )
                                INTO      ( SGTSL064-RPST              )
                                FLENGTH   ( QT-CCT-LIDO                )
                     END-EXEC
               WHEN  CAFS0053
                     INITIALIZE CAFS0053-DADOS
                     MOVE  LENGTH OF CAFS0053-DADOS
                                       TO  QT-CCT-EPRO
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR                    )
                                INTO      ( CAFS0053-DADOS             )
                                FLENGTH   ( QT-CCT-LIDO                )
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO   TO  TRUE
                     MOVE  9003        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na leitura da resposta do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
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
                      CONTAINER ( NM-CTNR                              )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO                          )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973100-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *   Section da Faixa de 980000 a 989900 para Gravacao de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------

      *--> Exclui eventual container de erro de outros programas
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO

           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de resposta
           MOVE  'SGTS0087-RPST'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                              )
                      FROM      ( SGTS0087-RPST                        )
                      FLENGTH   ( LENGTH OF SGTS0087-RPST              )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       980000-SAI.
           EXIT.

      *----------------------------------------------------------------
       981000-GRAVAR-CONTAINER-SRVC    SECTION.
      *-->    Postar container de requisição de serviço.
      *----------------------------------------------------------------
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao do servico
           MOVE  'PRM-RQSC-SRVC'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

           EVALUATE  NM-PGM
               WHEN  CAFS0053
                     MOVE  0150834     TO  SRVC-ID-SRVC
                     MOVE  0001        TO  SRVC-VRS-SRVC
                     MOVE  1092761     TO  SRVC-ID-OPR
                     MOVE  0001        TO  SRVC-VRS-OPR
           END-EVALUATE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                              )
                      FROM      ( SRVC-PRM-RQSC-SRVC                   )
                      FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC         )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE

           .
       981000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *--> Postar container com dados de requisição (entrada)
      *    Modulo CONTAINER  -     NOME_PGM-RQSC
      *    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
      *-----------------------------------------------------------------
           MOVE  0017                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           EVALUATE  NM-PGM
               WHEN  CAFS0053
                     MOVE  'PRM-DADOS-RQSC'
                                       TO  NM-CTNR
               WHEN  OTHER
                     MOVE  NM-PGM      TO  NM-CTNR(1:8)
                     MOVE  '-RQSC'     TO  NM-CTNR(9:5)
           END-EVALUATE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Faz PUT da requisicao do programa a executar
           EVALUATE  NM-PGM
               WHEN  SGTSL064
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                    )
                                FROM      ( SGTSL064-RQSC              )
                                FLENGTH   ( LENGTH OF SGTSL064-RQSC    )
                     END-EXEC

               WHEN  CAFS0053
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                    )
                                FROM      ( CAFS0053-DADOS             )
                                FLENGTH   ( LENGTH OF CAFS0053-DADOS   )
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO   TO TRUE

                     MOVE  9004              TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na gravação da requisição do módulo '
                              NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO MSG1-TX-ERRO-TS
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
      *    problemas em modulos executados mais de uma vez
      *    dentro da mesma sessao do CICS.
      *    Equivale ao INITIALIZE na area de resposta dos programas
      *    que utilizam COMMAREA.
      *-----------------------------------------------------------------
           MOVE  0018                  TO  MSG1-CD-SEQL-ERRO

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           EVALUATE  NM-PGM
               WHEN  CAFS0053
                     MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN  OTHER
                     MOVE  NM-PGM      TO  NM-CTNR(1:8)
                     MOVE  '-RPST'     TO  NM-CTNR(9:5)
           END-EVALUATE

           SET   DEL-CONTAINER         TO  TRUE

      *--> Exclui o container de resposta
      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR                              )
                      RESP      ( MSG1-RESP1                           )
                      RESP2     ( MSG1-RESP2                           )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET ERRO-CONTAINER TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *--> Excluir o container de erro
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------
           MOVE  0019                  TO  MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE                      IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           SET   DEL-CONTAINER         TO  TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR                              )
                      RESP      ( MSG1-RESP1                           )
                      RESP2     ( MSG1-RESP2                           )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET ERRO-CONTAINER TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *-----------------------------------------------------------------

      *--> Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                           )
                      FROM      ( IIBKERRO-PRM-ERRO                    )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO          )
                      RESP      ( MSG1-RESP1                           )
                      RESP2     ( MSG1-RESP2                           )
           END-EXEC

           .
       984000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       990000-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------

           EXEC CICS
                RETURN
           END-EXEC
           .
       990000-ENCERRAR-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *--> As sections abaixo iniciadas com 999000 representam os
      *    tratamentos de erro utilizando o Sistema MSG (MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-ERRO                     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

           PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-ERRO-SAI.
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
           IF  MNT-VRV
               MOVE  'SGTS0087-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS0087-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'SGTS0087-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
               IF  SGTS0087-CD-OPR-ETRD-MCP  NUMERIC
                   MOVE  SGTS0087-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD
                   MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
               ELSE
                   MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
               END-IF
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'SGTS0087-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS0087-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'SGTS0087-CD-TIP-PRTC'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS0087-CD-TIP-PRTC
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'SGTS0087-CD-ULT-BCO-PCT'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS0087-CD-ULT-BCO-PCT
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT
           END-IF

      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava o erro gerado no container de erro
           PERFORM  984000-GRAVAR-CONTAINER-ERRO

      *--> Encerra a transacao
           PERFORM  990000-ENCERRAR
           .
       999000-SAI.
           EXIT.

      *----------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * ---------------------------------------
-INC HLPKP999
      *----------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
-INC SGTKP999
      *----------------------------------------
      * MSG1-NTF-MSG                   SECTION.
      *----------------------------------------
-INC MSGKPMG1
      *---------------------- FIM SGTS0087 -----------------------------