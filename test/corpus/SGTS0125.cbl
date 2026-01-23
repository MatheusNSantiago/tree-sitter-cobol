      *-----------------------------------------------------------------
      * Programa..: SGTS0125
      * Analista..: F2054109 - Cintia Ap Soares Iglesias
      * Autor.....: F8214779 - Pedro Argolo Lacerda
      * Data......: 02/06/2017
      * Objetivo..: Excluir Operação vinculada para pagamento.
      * Compilacao: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 31/01/2019 - F0950413 inclusão da sigla 'SGT' na section
      *                     972000-LER-CONTAINER-DE-ERRO
      *        25/06/2018 - F8658869 - DE 2016/145195 - Incluir carteira
      *                     para SGTS0081.
      *        02/06/2017 - F8214779 - DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0125.

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
       77  CTE-PROG                    PIC  X(18)  VALUE
                                        '*** SGTS0125 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *--> Módulos acionados
       77  SGTS0081                    PIC  X(08) VALUE 'SGTS0081'.
       77  SGTSE071                    PIC  X(08) VALUE 'SGTSE071'.

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
       01 SGTS0125-DADOS.
-INC SGTK0125

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      *--> Exclusão na tabela OPR_PG_OTR_OP
-INC SGTKE071

      *--> Consultar Disponibilidade de Atributos por Tipo de Ativo
      *    e Estado da Operação
-INC SGTK0081

       01  CTE-FINAL-SS                PIC X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.

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

      *--> Ativa a captura automática de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM 970000-LER-CANAL

      *--> Define o nome do programa p/ fins manipulação container
           MOVE  CTE-PROG(5:8)         TO  NM-PGM

      *--> Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *--> Valida campo a campo da requisição
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Operação Pagadora
           IF  SGTS0125-CD-CTRA-OPR-PGDR   EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0496              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira da operação pagadora não inform
      -              'ado.'             TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0125-CD-CTRA-OPR-PGDR   NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0497              TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira da operação pagadora inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0125-CD-OPR-PGDR        NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0498              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada pagadora não numéri
      -              'co.'             TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0125-CD-OPR-PGDR        EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0499              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada pagadora não inform
      -              'ado.'             TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0125-CD-OPR-PGDR        NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0500              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada pagadora inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0125-NR-VRS-OPR-PGDR    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0501              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação pagadora não informad
      -              'o.'              TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0125-NR-VRS-OPR-PGDR    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0502              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação pagadora inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

      *--> Valida a Operação Vinculada
           IF  SGTS0125-CD-CTRA-OPR-VCLD
                                           EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0503              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira da operação vinculada não infor
      -              'mado.'            TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0125-CD-CTRA-OPR-VCLD
                                           NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0504              TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira da operação vinculada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0125-CD-OPR-ETRD-VCLD   NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0505              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada vinculada não numér
      -              'ico.'            TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0125-CD-OPR-ETRD-VCLD   EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0506              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada vinculada não infor
      -              'mado.'            TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0125-CD-OPR-ETRD-VCLD   NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0507              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada vinculada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0125-NR-VRS-OPR-VCLD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0508              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação vinculada não informa
      -              'do.'             TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0125-NR-VRS-OPR-VCLD    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0509              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação vinculada inválido.'
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

           PERFORM  210000-CONSULTAR-PERMISSAO

           PERFORM  220000-EXCLUIR-OPR-VINCULADA

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-CONSULTAR-PERMISSAO      SECTION.
      *--> Consultar indicadores de permissão de alteração SGTS0081
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTS0081-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  'SGTS0081'            TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0125-CD-CTRA-OPR-PGDR TO SGTS0081-CD-CTRA-TIT-VL
           MOVE  SGTS0125-CD-OPR-PGDR      TO SGTS0081-CD-OPR-ETRD
           MOVE  SGTS0125-NR-VRS-OPR-PGDR  TO SGTS0081-NR-VRS-OPR-ETRD

           PERFORM  900000-EXECUTAR-MODULO

           IF  SGTS0081-IN-HBLT-MENU-PGTO  EQUAL  'N'
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0521              TO  MSG1-CD-ERRO-TS
               MOVE  'Estado da operação pagadora não permite Exclusão.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-EXCLUIR-OPR-VINCULADA        SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTSE071-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  'SGTSE071'            TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0125-CD-CTRA-OPR-PGDR
                                       TO  SGTSE071-CD-CTRA-OPR-PGDR
           MOVE  SGTS0125-CD-OPR-PGDR
                                       TO  SGTSE071-CD-OPR-PGDR
           MOVE  SGTS0125-NR-VRS-OPR-PGDR
                                       TO  SGTSE071-NR-VRS-OPR-PGDR
           MOVE  SGTS0125-CD-CTRA-OPR-VCLD
                                       TO  SGTSE071-CD-CTRA-OPR-VCLD
           MOVE  SGTS0125-CD-OPR-ETRD-VCLD
                                       TO  SGTSE071-CD-OPR-ETRD-VCLD
           MOVE  SGTS0125-NR-VRS-OPR-VCLD
                                       TO  SGTSE071-NR-VRS-OPR-VCLD

            PERFORM 900000-EXECUTAR-MODULO
           .
       220000-SAI.
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

           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE  NM-PGM
               WHEN  SGTS0081
                     EXEC CICS LINK
                               PROGRAM ( SGTS0081   )
                               CHANNEL ( NM-CNL     )
                               RESP    ( MSG1-RESP1 )
                               RESP2   ( MSG1-RESP2 )
                     END-EXEC
               WHEN  SGTSE071
                     EXEC CICS LINK
                               PROGRAM ( SGTSE071   )
                               CHANNEL ( NM-CNL     )
                               RESP    ( MSG1-RESP1 )
                               RESP2   ( MSG1-RESP2 )
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE 9006         TO MSG1-CD-ERRO-TS
                     MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                     PERFORM 999000-ERRO
           END-EVALUATE
      *
           EVALUATE TRUE
               WHEN  MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     MOVE 3004         TO MSG1-CD-ERRO-TS
                     MOVE 'Erro ao acionar o módulo.'
                                       TO MSG1-TX-ERRO-TS
                     SET ERRO-CICS     TO TRUE
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

               WHEN  SGTSE071
                     MOVE 'SGTSE071-CD-CTRA-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE071-CD-CTRA-OPR-PGDR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSE071-CD-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSE071-CD-OPR-PGDR
                                           IS NUMERIC
                         MOVE  SGTSE071-CD-OPR-PGDR
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF

                     MOVE 'SGTSE071-NR-VRS-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE071-NR-VRS-OPR-PGDR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSE071-CD-CTRA-OPR-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE071-CD-CTRA-OPR-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSE071-CD-OPR-ETRD-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSE071-CD-OPR-ETRD-VCLD
                                           IS NUMERIC
                         MOVE  SGTSE071-CD-OPR-ETRD-VCLD
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF

                     MOVE 'SGTSE071-NR-VRS-OPR-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE071-NR-VRS-OPR-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT
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
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

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
           MOVE  CTE-PROG(5:8)         TO  NM-PGM
           MOVE  'SGTS0125-RQSC'       TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      *--> Obtem o tamanho esperado da requisição
           MOVE  LENGTH OF SGTS0125-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisição X tamanho recebido
           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA   TO TRUE
               PERFORM  999000-ERRO
           END-IF

           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER ( NM-CTNR       )
                     FLENGTH   ( QT-CCT-LIDO   )
                     INTO      ( SGTS0125-RQSC )
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
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

      *--> Le o container de erro dos módulos chamados
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
                     SET   ERRO-CICS   TO  TRUE
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

      *--> Monta o nome dos container de acordo com módulo a executar
           MOVE  SPACES                TO  NM-CTNR

           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)

      *--> Recupera o tamanho do container de resposta
           PERFORM  973100-LER-CONTAINER-NODATA

           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de resposta
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Lê a RESPOSTA de acordo com o programa que a gravou

           EVALUATE  NM-PGM
               WHEN  SGTS0081
                     INITIALIZE            SGTS0081-RPST
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR          )
                               INTO      ( SGTS0081-RPST    )
                               FLENGTH   ( QT-CCT-LIDO      )
                     END-EXEC

               WHEN  SGTSE071
                     INITIALIZE            SGTSE071-RPST
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR          )
                               INTO      ( SGTSE071-RPST    )
                               FLENGTH   ( QT-CCT-LIDO      )
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE 9006         TO MSG1-CD-ERRO-TS
                     MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
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
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Obtém a quantidade de caracteres no container
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
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

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
                               CONTAINER ( NM-CTNR                )
                               FROM      ( SGTS0081-RQSC          )
                               FLENGTH   ( LENGTH OF SGTS0081-RQSC)
                     END-EXEC
               WHEN  SGTSE071
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                )
                               FROM      ( SGTSE071-RQSC          )
                               FLENGTH   ( LENGTH OF SGTSE071-RQSC)
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE 9006         TO MSG1-CD-ERRO-TS
                     MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
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
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)

           SET   DEL-CONTAINER         TO  TRUE

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
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE                      IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro

           MOVE  'PRM-ERRO'            TO  NM-CTNR
           SET   DEL-CONTAINER
                                       TO  TRUE

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

      *--> Em caso de erro, desfaz as atualizaes em banco de dados
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

      *--> Adiciona o Texto de Erro na Monitoração
           MOVE 'IIBKERRO-TX-ERRO'     TO  MSGKW999-NM-VRV-MNT
           MOVE  IIBKERRO-TX-ERRO      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE 'SGTS0125-CD-CTRA-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0125-CD-CTRA-OPR-PGDR
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0125-CD-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTS0125-CD-OPR-PGDR        IS NUMERIC
               MOVE  SGTS0125-CD-OPR-PGDR
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF

           MOVE 'SGTS0125-NR-VRS-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0125-NR-VRS-OPR-PGDR
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0125-CD-CTRA-OPR-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0125-CD-CTRA-OPR-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0125-CD-OPR-ETRD-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTS0125-CD-OPR-ETRD-VCLD   IS NUMERIC
               MOVE  SGTS0125-CD-OPR-ETRD-VCLD
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF

           MOVE 'SGTS0125-NR-VRS-OPR-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0125-NR-VRS-OPR-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

      *--> Encerra a transação
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

      *---------------------- FIM SGTS0125 -----------------------------