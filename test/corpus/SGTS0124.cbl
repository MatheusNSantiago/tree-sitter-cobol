      *-----------------------------------------------------------------
      * Programa..: SGTS0124
      * Analista..: F2054109 - Cintia Ap Soares Iglesias
      * Autor.....: F2804421 - Eliana Cristina Ramos da Silva
      * Data......: 26/01/2017
      * Objetivo..: Alterar Texto de Observação de Operaçoes Vinculadas.
      * Compilacao: Cobol/CICS
      *-----------------------------------------------------------------
      * VRS001 31/01/2019 - F950413 inclusão da sigla 'SGT' na section
      *                     972000-LER-CONTAINER-DE-ERRO
      *        26/01/2017 - F2804421 - DE 145475/176118 - Implantacao
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0124.

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
       77  CTE-INICIO-SS                 PIC  X(040)  VALUE
                    '*** S.S. COMECA AQUI ***'.

       77  CTE-PROG                      PIC  X(018)  VALUE
                    '*** SGTS0124 ***'.
       77  CTE-VERS                      PIC  X(006)  VALUE 'VRS001'.

      *--> Módulos acionados
       77  SGTST005                      PIC  X(008) VALUE 'SGTST005'.
       77  SGTS0081                      PIC  X(008) VALUE 'SGTS0081'.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Areas de trabalho

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).


      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book dos dados do Usuario
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
       01 SGTS0124-DADOS.
-INC SGTK0124

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      *--> CRUD TABELA OPR_ETRD_MCP - via commarea
       01  SGTST005-DADOS.
-INC SGTKT005

      *--> CONSULTA VERSAO DO TIPO DE COTA - via Container
-INC SGTK0081


       77  CTE-FINAL-SS                  PIC X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

           PERFORM 100000-INICIAR

           PERFORM 200000-PROCESSAR

           PERFORM 999999-ENCERRAR

           GOBACK
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

           PERFORM 970000-LER-CANAL

      *--> Define o nome do programa p/ fins manipulacao container
           MOVE  'SGTS0124'            TO  NM-PGM

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *--> Valida campo a campo da requisicao
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Operação Pagadora
           IF  SGTS0124-CD-CTRA-OPR-PGDR
                                           EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0496              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira da operação pagadora não inform
      -              'ado.'             TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0124-CD-CTRA-OPR-PGDR
                                           NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0497              TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira da operação pagadora inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0124-CD-OPR-PGDR        NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0498              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada pagadora não numéri
      -              'co.'             TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0124-CD-OPR-PGDR        EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0499              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada pagadora não inform
      -              'ado.'             TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0124-CD-OPR-PGDR        NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0500              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada pagadora inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0124-NR-VRS-OPR-PGDR    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0501              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação pagadora não informad
      -              'o.'              TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0124-NR-VRS-OPR-PGDR    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0502              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação pagadora inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

      *--> Valida o Texto da Operação Vinculada
           IF  SGTS0124-TX-PGTO-OPR-VCLD   EQUAL  SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0520              TO  MSG1-CD-ERRO-TS
               MOVE  'Texto da observação do pagamento da operação não i
      -              'nformado.'       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

      *--> Consultar indicadores de permissão de alteração (container)
           PERFORM  210000-CONSULTAR-INDICADOR

      *--> Consultar Operação (commarea)
           PERFORM  220000-CONSULTAR-OPERACAO

      *--> Alterar Operação (commarea)
           PERFORM  230000-ALTERAR-OPERACAO
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-CONSULTAR-INDICADOR      SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                       SGTS0081-DADOS

      *--> Movimentar o nome do programa a ser executado
           MOVE   SGTS0081             TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0124-CD-CTRA-OPR-PGDR  TO  SGTS0081-CD-CTRA-TIT-VL
           MOVE  SGTS0124-CD-OPR-PGDR       TO  SGTS0081-CD-OPR-ETRD
           MOVE  SGTS0124-NR-VRS-OPR-PGDR   TO  SGTS0081-NR-VRS-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO

           IF  SGTS0081-IN-HBLT-MENU-PGTO  EQUAL  'N'
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0515              TO  MSG1-CD-ERRO-TS
               MOVE  'Estado da operação pagadora não permite atualizaçã
      -              'o.'                 TO  MSG1-TX-ERRO-TS
              PERFORM 999000-ERRO
           END-IF
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-CONSULTAR-OPERACAO           SECTION.
      *--> Consultar Operação Estruturada.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                       SGTST005-DADOS

           SET   SGTST005-CONSULTAR    TO  TRUE

           MOVE  SGTS0124-CD-CTRA-OPR-PGDR
                                       TO  SGTST005-CD-CTRA-TIT-VL
           MOVE  SGTS0124-CD-OPR-PGDR
                                       TO  SGTST005-CD-OPR-ETRD-MCP
           MOVE  SGTS0124-NR-VRS-OPR-PGDR
                                       TO  SGTST005-NR-VRS-OPR-ETRD

           PERFORM 920000-CHAMAR-SGTST005
           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-ALTERAR-OPERACAO          SECTION.
      *--> Atualizar Texto de Pagamento da Operação.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           SET   SGTST005-ALTERAR      TO  TRUE

           MOVE  SGTS0124-TX-PGTO-OPR-VCLD
                                       TO  SGTST005-TX-PGTO-OPR-VCLD

           PERFORM 920000-CHAMAR-SGTST005
           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    Section da Faixa de 900000 a 969900 para Demais Rotinas
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *--> Executa um modulo Online implementado com CONTAINER
      *    Nao incluir nessa section os módulos chamados via COMMAREA
      *-----------------------------------------------------------------

      *--> Grava o container de requisição do módulo a executar
           PERFORM  982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o módulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE  NM-PGM
               WHEN  SGTS0081
                     EXEC CICS LINK
                             PROGRAM ( SGTS0081   )
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
       920000-CHAMAR-SGTST005          SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE  SGTST005              TO  NM-PGM

           EXEC CICS  LINK
                      PROGRAM  ( SGTST005       )
                      COMMAREA ( SGTST005-DADOS )
           END-EXEC

           IF  SGTST005-CD-RTN-PGM         NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST005-CD-RTN-PGM
                                       TO  MSG1-CD-ERRO-TS
                                           MSG1-CD-RTN-PGM-ACND
               MOVE  SGTST005-MSG-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
               MOVE  SGTST005-PSC-ERRO-PGM
                                       TO  MSG1-CD-SEQL-PGM-ACND
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
               WHEN  SGTS0081
                     MOVE 'SGTS0081-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0081-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE 'SGTS0081-CD-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTS0081-CD-OPR-ETRD
                                           IS NUMERIC
                         MOVE  SGTS0081-CD-OPR-ETRD
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTS0081-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0081-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

               WHEN  SGTST005
                     MOVE 'SGTST005-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST005-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST005-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTST005-CD-OPR-ETRD-MCP
                                           IS NUMERIC
                         MOVE  SGTST005-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST005-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST005-TX-PGTO-OPR-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-TX-PGTO-OPR-VCLD
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
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

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

      *--> Monta o nome do container de requisicao
           MOVE  'SGTS0124'            TO  NM-PGM
           MOVE  'SGTS0124-RQSC'       TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER ( NM-CTNR       )
                     FLENGTH   ( QT-CCT-LIDO   )
                     INTO      ( SGTS0124-RQSC )
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
           INITIALIZE                      IIBKERRO-PRM-ERRO

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
      *        PARA O MÓDULO CHAMADO e tratar na section chamadora
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
                    PERFORM 999002-TRATA-CONDITION-CICS
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

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR

           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)

      *--> Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

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

               WHEN  SGTS0081
                     INITIALIZE            SGTS0081-RPST
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR          )
                               INTO      ( SGTS0081-RPST    )
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

      *--> Obtem a quantidade de caracteres no container
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
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *-----------------------------------------------------------------
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SPACES                TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RQSC'               TO  NM-CTNR(9:5)

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Faz PUT da requisição do programa a executar
           EVALUATE  NM-PGM
               WHEN  SGTS0081
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTS0081-RQSC           )
                               FLENGTH   ( LENGTH OF SGTS0081-RQSC )
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
      *    programas que utilizam COMMAREA.*
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

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
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CICS   TO  TRUE
                     PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *--> Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE                      IIBKERRO-PRM-ERRO

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
                     SET   ERRO-CICS   TO  TRUE
                     PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *--> Grava o erro gerado no container de erro
      *-----------------------------------------------------------------

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
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
                     PERFORM 999004-TRATA-ERRO-CTNR
               WHEN  ERRO-TAMANHO-AREA
                     PERFORM 999005-TRATA-ERRO-AREA-DADOS
               WHEN  ERRO-CANAL
                     PERFORM 999006-TRATA-ERRO-CANAL
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

      *--> Adiciona o Texto de Erro na Monitoração
           MOVE '000-IIBKERRO-TX-ERRO' TO  MSGKW999-NM-VRV-MNT
           MOVE  IIBKERRO-TX-ERRO      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE 'SGTS0124-CD-CTRA-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0124-CD-CTRA-OPR-PGDR
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0124-CD-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTS0124-CD-OPR-PGDR        IS NUMERIC
               MOVE  SGTS0124-CD-OPR-PGDR
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF

           MOVE 'SGTS0124-NR-VRS-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0124-NR-VRS-OPR-PGDR
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0124-TX-PGTO-OPR-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0124-TX-PGTO-OPR-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava o erro gerado no container de erro
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

      *---------------------- FIM SGTS0124 -----------------------------