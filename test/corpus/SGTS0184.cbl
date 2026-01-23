      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0184
      * ANALISTA..: F2054109 CINTIA IGLESIAS
      * AUTOR.....: F2054109 CINTIA IGLESIAS
      * DATA......: 31/08/2017
      * OBJETIVO..: Programa de negócio para Listar Remuneração da
      *             operação estruturada.
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 31/01/2019 F0950413 Inclusão da sigla 'SGT' na section
      *                   972000-LER-CONTAINER-DE-ERRO
      *        31/08/2017 F2054109 DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0184.
      *
      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------
      *

       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------

       77  CTE-INICIO-SS               PIC  X(40) VALUE
                                       '*** S.S. Comeca Aqui ***'.
       77  CTE-PROG                    PIC  X(18) VALUE
                                       '*** SGTS0184 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *--> Modulos acionados
       77  SGTSL011                    PIC  X(08) VALUE 'SGTSL011'.
       77  SGTS0136                    PIC  X(08) VALUE 'SGTS0136'.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Áreas de trabalho
       01  VL-05V10                    PIC  --.--9,9999999999.
       01  VL-15V02                    PIC  ----.---.---.---.--9,99.
       01  VL-09V08                    PIC  ----.---.--9,99999999.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book do container de requisicao e resposta deste modulo
       01 SGTS0184-DADOS.
-INC SGTK0184

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      *--> Consulta da tab RMNC_OPR_ETRD
       01 SGTSL011-DADOS.
-INC SGTKL011

      *--> Formatar descrição da taxa
       01 SGTS0136-DADOS.
-INC SGTK0136

       01  CTE-FINAL-SS                PIC X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

           PERFORM  100000-INICIAR

           PERFORM  200000-PROCESSAR

           PERFORM  999999-ENCERRAR
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

      *--> Ativa a captura automatica de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM  970000-LER-CANAL

      *--> Define o nome do programa p/ fins manipulação container
           MOVE  'SGTS0184'            TO  NM-PGM

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM  971000-LER-REQUISICAO

           PERFORM  110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *--> Valida campo a campo da requisição
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada
           IF  SGTS0184-CD-CTRA-TIT-VL     EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0184-CD-CTRA-TIT-VL     NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0184-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não numérico.'
                                      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0184-CD-OPR-ETRD-MCP   EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0184-CD-OPR-ETRD-MCP    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'   TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0184-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0184-NR-VRS-OPR-ETRD    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------

           PERFORM  210000-LISTAR-REMUNERACOES

           PERFORM  220000-TRATAR-LISTA-RMNC
                   VARYING SGTSL011-IC-TAB FROM 1 BY 1
                   UNTIL SGTSL011-IC-TAB   > SGTSL011-QT-OCR-TAB

           MOVE  SGTSL011-QT-OCR-TAB   TO  SGTS0184-QT-OCR-TAB
           MOVE  SGTSL011-IN-CTNC-PSQ  TO  SGTS0184-IN-CTNC-PSQ

           PERFORM  300000-POSTAR-RESPOSTA
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-LISTAR-REMUNERACOES      SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTSL011              TO  NM-PGM

           MOVE  SGTS0184-CD-CTRA-TIT-VL
                                       TO  SGTSL011-CD-CTRA-TIT-VL
           MOVE  SGTS0184-CD-OPR-ETRD-MCP
                                       TO  SGTSL011-CD-OPR-ETRD-MCP
           MOVE  SGTS0184-NR-VRS-OPR-ETRD
                                       TO  SGTSL011-NR-VRS-OPR-ETRD
           MOVE  SGTS0184-DT-INC-TAXA-ULT
                                       TO  SGTSL011-DT-INC-TAXA-ULT

           PERFORM  900000-EXECUTAR-MODULO
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-TRATAR-LISTA-RMNC        SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTSL011-DT-INC-TAXA-RMNC(SGTSL011-IC-TAB)
             TO  SGTS0184-DT-INC-TAXA-RMNC(SGTSL011-IC-TAB)

           MOVE  SGTSL011-TS-ULT-ATL(SGTSL011-IC-TAB)
             TO  SGTS0184-TS-ULT-ATL(SGTSL011-IC-TAB)

           MOVE  SGTSL011-CD-USU-RSP-ATL(SGTSL011-IC-TAB)
             TO  SGTS0184-CD-USU-RSP-ATL(SGTSL011-IC-TAB)

           MOVE  SGTSL011-PC-TAXA-RMNC-OPR(SGTSL011-IC-TAB)
             TO  SGTS0184-PC-TAXA-RMNC-OPR(SGTSL011-IC-TAB)

           MOVE  SGTSL011-DT-FIM-TAXA-RMNC(SGTSL011-IC-TAB)
             TO  SGTS0184-DT-FIM-TAXA-RMNC(SGTSL011-IC-TAB)

           MOVE  SGTSL011-CD-TIP-FMA-APL(SGTSL011-IC-TAB)
             TO  SGTS0184-CD-TIP-FMA-APL(SGTSL011-IC-TAB)

           MOVE  SGTSL011-CD-IEC-RMNC(SGTSL011-IC-TAB)
             TO  SGTS0184-CD-IEC-RMNC(SGTSL011-IC-TAB)

           MOVE  SGTSL011-CD-PERC-CPTZ-JUR(SGTSL011-IC-TAB)
             TO  SGTS0184-CD-PERC-CPTZ-JUR(SGTSL011-IC-TAB)

           MOVE  SGTSL011-PC-TAXA-RMNC-OPR(SGTSL011-IC-TAB)
             TO  SGTS0184-PC-TAXA-RMNC-OPR(SGTSL011-IC-TAB)

           PERFORM  221000-FORMATAR-DESCR-TAXA

           MOVE  SGTS0136-TX-DCR-RMNC-OPR
             TO  SGTS0184-TX-DCR-TAXA-OPR(SGTSL011-IC-TAB)
           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       221000-FORMATAR-DESCR-TAXA      SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTS0136              TO  NM-PGM

           INITIALIZE                  SGTS0136-DADOS

           MOVE  SGTS0184-PC-TAXA-RMNC-OPR(SGTSL011-IC-TAB)
                                       TO  SGTS0136-PC-TAXA-RMNC-OPR
           MOVE  SGTS0184-CD-TIP-FMA-APL(SGTSL011-IC-TAB)
                                       TO  SGTS0136-CD-TIP-FMA-APL
           MOVE  SGTS0184-CD-IEC-RMNC(SGTSL011-IC-TAB)
                                       TO  SGTS0136-CD-IEC-RMNC

           PERFORM  900000-EXECUTAR-MODULO
           .
       221000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
                     CONTAINER ( 'SGTS0184-RPST'         )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTS0184-RPST )
                     FROM      ( SGTS0184-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *   Section da Faixa de 900000 a 969900 para Demais Rotinas
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *--> Executa um modulo Online implementado com CONTAINER
      *    Nao incluir nessa section os módulos chamados via COMMAREA
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

      *--> Grava o container de requisição do módulo a executar
           PERFORM  982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o módulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE  NM-PGM
               WHEN  SGTSL011
                     EXEC CICS LINK
                               PROGRAM ( SGTSL011   )
                               CHANNEL ( NM-CNL     )
                               RESP    ( MSG1-RESP1 )
                               RESP2   ( MSG1-RESP2 )
                     END-EXEC
               WHEN  SGTS0136
                     EXEC CICS LINK
                               PROGRAM ( SGTS0136   )
                               CHANNEL ( NM-CNL     )
                               RESP    ( MSG1-RESP1 )
                               RESP2   ( MSG1-RESP2 )
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9006        TO  MSG1-CD-ERRO-TS
                     STRING  'Não definida a execução do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     END-STRING
                     PERFORM  999000-ERRO
           END-EVALUATE

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     MOVE  3004        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na execução do módulo ' NM-PGM ', '
                             'RESP  = '    MSG1-RESP1  ', '
                             'RESP2 = '    MSG1-RESP2
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                             SET   ERRO-CICS
                                       TO  TRUE
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
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM
               WHEN  SGTSL011
                     MOVE 'SGTSL011-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL011-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL011-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSL011-CD-OPR-ETRD-MCP
                                           IS NUMERIC
                         MOVE  SGTSL011-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL011-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL011-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL011-DT-INC-TAXA-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL011-DT-INC-TAXA-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTS0136
                     MOVE 'SGTS0136-PC-TAXA-RMNC-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0136-PC-TAXA-RMNC-OPR
                                       TO  VL-05V10
                     MOVE  VL-05V10    TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTS0136-CD-TIP-FMA-APL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0136-CD-TIP-FMA-APL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTS0136-CD-IEC-RMNC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0136-CD-IEC-RMNC
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
       970000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------

      *--> Monta o nome do container de requisição
           MOVE  'SGTS0184'            TO  NM-PGM
           MOVE  'SGTS0184-RQSC'       TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM  973100-LER-CONTAINER-NODATA

           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

      *--> Obtem o tamanho esperado da requisição
           MOVE  LENGTH OF SGTS0184-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisição X tamanho recebido
           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA TO TRUE
               PERFORM  999000-ERRO
           END-IF

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER ( NM-CTNR       )
                     FLENGTH   ( QT-CCT-LIDO   )
                     INTO      ( SGTS0184-RQSC )
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
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

      *--> Le o container de erro dos módulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           INITIALIZE                  IIBKERRO-PRM-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS GET
                     CONTAINER ( NM-CTNR                     )
                     INTO      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC

           EVALUATE  TRUE
      *        Caso deseje personalizar ou não interromper o
      *        processamento, alterar para não dar erro aqui SOMENTE
      *        PARA O MODULO CHAMADO e tratar na section chamadora
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
      *--> Trata o erro de programa acionado por ser módulo interno
                SET ERRO-PROGRAMA-ACIONADO   TO  TRUE
                MOVE NM-PGM                  TO  MSG1-NM-PGM-ACND
                IF NM-PGM(1:3) EQUAL 'SGT'
                    MOVE IIBKERRO-CD-ERRO    TO  MSG1-CD-RTN-PGM-ACND
                                                 MSG1-CD-ERRO-TS
                    MOVE IIBKERRO-TX-ERRO    TO  MSG1-TX-RTN-PGM-ACND
                                                 MSG1-TX-ERRO-TS
                ELSE
                    MOVE  3004               TO  MSG1-CD-ERRO-TS
                    STRING  'Erro ao acionar o módulo ' NM-PGM      '.'
                            DELIMITED BY SIZE
                            INTO        MSG1-TX-ERRO-TS
                    END-STRING
                    MOVE IIBKERRO-CD-ERRO     TO  MSG1-CD-RTN-PGM-ACND
                    MOVE IIBKERRO-TX-ERRO     TO  MSG1-TX-RTN-PGM-ACND
                END-IF
      *-->      Monitorar variáveis do módulo chamado
                PERFORM 969000-MONITORAR-VARIAVEIS
                PERFORM 999000-ERRO
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
      *-----------------------------------------------------------------

      *--> Monta o nome dos container de acordo com módulo a executar
           MOVE  SPACES                TO  NM-CTNR

           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)

      *--> Recupera o tamanho do container de resposta
           PERFORM  973100-LER-CONTAINER-NODATA

           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           EVALUATE  NM-PGM
               WHEN  SGTS0136
                     MOVE  LENGTH OF SGTS0136-RPST
                                       TO  QT-CCT-EPRO
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

           IF  NM-PGM                      EQUAL  SGTS0136
               IF  QT-CCT-LIDO             NOT EQUAL  QT-CCT-EPRO
                   SET   ERRO-TAMANHO-AREA     TO  TRUE
                   PERFORM  969000-MONITORAR-VARIAVEIS
                   PERFORM  999000-ERRO
               END-IF
           END-IF

      *--> Configura dados para leitura do container de resposta
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Le a RESPOSTA de acordo com o programa que a gravou

           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           EVALUATE  NM-PGM
      *        Calcular a quantidade de caracteres esperado e limpar
      *        a area de dados antes de executar o GET
      *        Para matrizes com dados variáveis (clausula depending
      *        on) não há como utilizar o comando INITIALIZE
               WHEN  SGTSL011
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR          )
                               INTO      ( SGTSL011-RPST    )
                               FLENGTH   ( QT-CCT-LIDO      )
                     END-EXEC
               WHEN  SGTS0136
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR          )
                               INTO      ( SGTS0136-RPST    )
                               FLENGTH   ( QT-CCT-LIDO      )
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9003        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na leitura da resposta do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     END-STRING
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
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *    Obtem a quantidade de caracteres no container
           MOVE  ZEROS                 TO  QT-CCT-LIDO

           EXEC CICS GET
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
      *   Section da Faixa de 980000 a 989900 para Gravação de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------

      *--> Exclui eventual container de erro de outros programas
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO

           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de resposta
           MOVE  'SGTS0184-RPST'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                 )
                     FROM      ( SGTS0184-RPST           )
                     FLENGTH   ( LENGTH OF SGTS0184-RPST )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       980000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com módulo a executar
           MOVE  SPACES                TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RQSC'               TO  NM-CTNR(9:5)

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Faz put da requisição do programa a executar
           EVALUATE  NM-PGM
               WHEN  SGTSL011
                     EXEC CICS PUT
                                CONTAINER ( NM-CTNR                 )
                                FROM      ( SGTSL011-RQSC           )
                                FLENGTH   ( LENGTH OF SGTSL011-RQSC )
                     END-EXEC
               WHEN  SGTS0136
                     EXEC CICS PUT
                                CONTAINER ( NM-CTNR                 )
                                FROM      ( SGTS0136-RQSC           )
                                FLENGTH   ( LENGTH OF SGTS0136-RQSC )
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9004        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na gravação da requisição do módulo.'
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     END-STRING
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
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)

           SET   DEL-CONTAINER         TO  TRUE

      *--> Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR    )
                     RESP      ( MSG1-RESP1 )
                     RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL DFHRESP(NORMAL)
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
      *
      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------
           MOVE  0017                  TO  MSG1-CD-SEQL-ERRO

      *--> Limpar variáveis container de erro
           INITIALIZE                  IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro

           MOVE  'PRM-ERRO'            TO  NM-CTNR
           SET   DEL-CONTAINER         TO  TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
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
      *-----------------------------------------------------------------

      *--> Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS PUT
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
           MOVE 'SGTS0184-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0184-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0184-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTS0184-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE  SGTS0184-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0184-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0184-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0184-DT-INC-TAXA-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0184-DT-INC-TAXA-ULT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava o erro gerado no container de erro
           PERFORM  984000-GRAVAR-CONTAINER-ERRO

      *--> Encerra a transação
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

           GOBACK
           .
       999999-SAI.
           EXIT.

      *---------------------- FIM SGTS0184 -----------------------------