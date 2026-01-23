      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0294
      * ANALISTA..: F8785286 - Roseli Akemi Kawahata
      * AUTOR.....: F0950413 - António Francisco Mendonça Silva
      * DATA......: 25/10/2019
      * OBJETIVO..: Lista açoes permitidas para a ordem de venda.
      * COMPILACAO: 52 - PSOSE600 - Cobol 4.2 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS002 08.09.2020 - F9507141 - 2019/155748 - Inclusão da opção
      *                           "Cancelar Ordem de Venda" quando a
      *                           Ordem estiver no estado 5 (Concluida).
      *                           Exclusão da opção "Concluir Cadastro"
      *                           quando a Ordem estiver no estado
      *                           7 (Não Encerrada)
      *        10.08.2020 - F9507141 - 2019/155748 - Inclusão da opção
      *                           Cancelar Ordem de Venda quando a Ordem
      *                           estiver no estado 7 (Não Encerrada)
      * VRS001 25.10.2019 - F0950413 - 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0294.

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
                                       '*** S.S. Começa Aqui ***'.
       77  CTE-PROG                    PIC  X(18)  VALUE
                                       '*** SGTS0294 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *--> Módulos a serem acionados via 'CONTAINER'
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTSL078                PIC  X(08) VALUE 'SGTSL078'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Areas de trabalho
       01  IC-TAB                      PIC S9(04) COMP-5    VALUE ZEROS.
       01  IC-RESTO                    PIC S9(04) COMP-5    VALUE ZEROS.
       01  CNT-CD-TIP-EST-NEGO-1       PIC S9(04) COMP-5    VALUE ZEROS.

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC   ---------------9.

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

      *--> Book do Container de Requisicao e Resposta deste Módulo
       01  SGTS0294-DADOS.
-INC SGTK0294

      *-----------------------------------------------------------------
      *    Book das Subrotinas Acionadas
      *-----------------------------------------------------------------

      *--> Listar a Tabela DB2SGT.EST_ORD_NEGO_OPR
       01  SGTSL078-DADOS.
-INC SGTKL078

       01  CTE-FINAL-SS                PIC X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
-INC ALMKCBL2

      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999

           PERFORM  100000-INICIAR

           PERFORM  200000-LISTA-ESTADO-NEGOCIACAO

           PERFORM  980000-GRAVAR-CTNR-RESPOSTA

           PERFORM  999999-ENCERRAR

           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           MOVE  CTE-PROG(5:8)         TO  NM-PGM

           PERFORM  110000-OBTER-NOME-DO-CANAL

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM  970000-LER-REQUISICAO-RECEBIDA

           PERFORM  120000-VALIDAR-REQUISICAO

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-OBTER-NOME-DO-CANAL      SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
                CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-VALIDAR-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTS0294-CD-CTRA-TIT-VL     NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de títulos e valores mobiliário
      -              's inválido.'     TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0294-CD-CTRA-TIT-VL     EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de títulos e valores mobiliário
      -              's não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0294-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0294-CD-OPR-ETRD-MCP    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais in
      -              'válido.'         TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0294-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'    TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0294-NR-VRS-OPR-ETRD    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0294-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0294-NR-SEQL-ORD-NEGO   NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1084              TO  MSG1-CD-ERRO-TS
               MOVE  'Número sequencial da ordem de negociação inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0294-NR-SEQL-ORD-NEGO    EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0542              TO  MSG1-CD-ERRO-TS
               MOVE  'Número sequencial da ordem de negociação não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-LISTA-ESTADO-NEGOCIACAO  SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do módulo para monitoração de variáveis
           MOVE  SGTSL078              TO  NM-PGM

      *--> Movimentar os campos de entrada
           SET   SGTSL078-LISTA-SEQL-ORD
                                       TO  TRUE
           MOVE  SGTS0294-CD-CTRA-TIT-VL
                                       TO  SGTSL078-CD-CTRA-TIT-VL
           MOVE  SGTS0294-CD-OPR-ETRD-MCP
                                       TO  SGTSL078-CD-OPR-ETRD-MCP
           MOVE  SGTS0294-NR-VRS-OPR-ETRD
                                       TO  SGTSL078-NR-VRS-OPR-ETRD
           MOVE  SGTS0294-NR-SEQL-ORD-NEGO
                                       TO  SGTSL078-NR-SEQL-ORD-ULT
           MOVE  SPACES                TO  SGTSL078-TS-EST-ULT

           PERFORM  900000-PROCEDIMENTOS-CONTAINER

           PERFORM  210000-TEXTOS-A-SEREM-GRAVADOS
                    VARYING IC-TAB
                    FROM 0001 BY 0001
                    UNTIL IC-TAB           GREATER SGTSL078-QT-OCR-TAB

           IF  CNT-CD-TIP-EST-NEGO-1       EQUAL  SGTSL078-QT-OCR-TAB
               PERFORM  220000-EDICAO-NUNCA-AVANCOU
           END-IF

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-TEXTOS-A-SEREM-GRAVADOS  SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSL078-CD-TIP-EST-NEGO (IC-TAB) = 1
               ADD   0001              TO  CNT-CD-TIP-EST-NEGO-1
               IF  IC-TAB = 1
                   PERFORM VARYING IC-RESTO FROM 2 BY 1
                             UNTIL IC-RESTO > SGTSL078-QT-OCR-TAB
                      IF  SGTSL078-CD-TIP-EST-NEGO (IC-RESTO) NOT = 1
                          PERFORM 211000-EDCO-FOI-FRENTE-VOLTOU
                          MOVE  SGTSL078-QT-OCR-TAB
                                       TO  IC-TAB IC-RESTO
                      END-IF
                   END-PERFORM
               END-IF
           ELSE
               PERFORM 212000-TEXTOS-TIP-EST-2-A-7
               MOVE  SGTSL078-QT-OCR-TAB
                                       TO  IC-TAB
           END-IF
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       211000-EDCO-FOI-FRENTE-VOLTOU   SECTION.
      *--> Estado da Ordem de Negociação em Edição que já foi para
      *    frente e voltou
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           MOVE  0005                  TO  SGTS0294-QT-OCR

           MOVE  'Editar'              TO  SGTS0294-NM-ACAO (1)
           MOVE  'Detalhar'            TO  SGTS0294-NM-ACAO (2)
           MOVE  'Manter Compradores'  TO  SGTS0294-NM-ACAO (3)
           MOVE  'Finalizar Cadastro'  TO  SGTS0294-NM-ACAO (4)
           MOVE  'Cancelar Ordem Venda'
                                       TO  SGTS0294-NM-ACAO (5)
           .
       211000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       212000-TEXTOS-TIP-EST-2-A-7     SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           EVALUATE    SGTSL078-CD-TIP-EST-NEGO (IC-TAB)

               WHEN 2
      *--> Em revisão
                    MOVE 5                       TO SGTS0294-QT-OCR
                    MOVE 'Detalhar'              TO SGTS0294-NM-ACAO (1)
                    MOVE 'Manter Compradores'    TO SGTS0294-NM-ACAO (2)
                    MOVE 'Conferir Cadastro'     TO SGTS0294-NM-ACAO (3)
                    MOVE 'Cancelar Ordem Venda'  TO SGTS0294-NM-ACAO (4)
                    MOVE 'Devolver para Cadastramento'
                                                 TO SGTS0294-NM-ACAO (5)
               WHEN 3
      *--> A autorizar
                    MOVE 5                       TO SGTS0294-QT-OCR
                    MOVE 'Detalhar'              TO SGTS0294-NM-ACAO (1)
                    MOVE 'Manter Compradores'    TO SGTS0294-NM-ACAO (2)
                    MOVE 'Autorizar Venda'       TO SGTS0294-NM-ACAO (3)
                    MOVE 'Cancelar Ordem Venda'  TO SGTS0294-NM-ACAO (4)
                    MOVE 'Devolver para Cadastramento'
                                                 TO SGTS0294-NM-ACAO (5)
               WHEN 4
      *--> Autorizada
                    MOVE 4                       TO SGTS0294-QT-OCR
                    MOVE 'Detalhar'              TO SGTS0294-NM-ACAO (1)
                    MOVE 'Manter Compradores'    TO SGTS0294-NM-ACAO (2)
                    MOVE 'Cancelar Ordem Venda'  TO SGTS0294-NM-ACAO (3)
                    MOVE 'Devolver para Cadastramento'
                                                 TO SGTS0294-NM-ACAO (4)
               WHEN 5
      *--> Concluída
                    MOVE 3                       TO SGTS0294-QT-OCR
                    MOVE 'Detalhar'              TO SGTS0294-NM-ACAO (1)
                    MOVE 'Manter Compradores'    TO SGTS0294-NM-ACAO (2)
                    MOVE 'Cancelar Ordem Venda'  TO SGTS0294-NM-ACAO (3)
               WHEN 6
      *--> Cancelada
                    MOVE 1                       TO SGTS0294-QT-OCR
                    MOVE 'Detalhar'              TO SGTS0294-NM-ACAO (1)
               WHEN 7
      *--> Não encerrado
                    MOVE 2                       TO SGTS0294-QT-OCR
                    MOVE 'Detalhar'              TO SGTS0294-NM-ACAO (1)
                    MOVE 'Cancelar Ordem Venda'  TO SGTS0294-NM-ACAO (2)
               WHEN OTHER
                    SET  ERRO-PROGRAMADO         TO  TRUE
                    MOVE 1093                    TO  MSG1-CD-ERRO-TS
                    MOVE 'Código do tipo do estado de negociação inválid
      -                  'o.'                    TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .
       212000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-EDICAO-NUNCA-AVANCOU     SECTION.
      *--> Estado da Ordem de Negociação em edição que nunca avançou
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           MOVE  5                     TO  SGTS0294-QT-OCR

           MOVE  'Editar'              TO  SGTS0294-NM-ACAO (1)
           MOVE  'Detalhar'            TO  SGTS0294-NM-ACAO (2)
           MOVE  'Manter Compradores'  TO  SGTS0294-NM-ACAO (3)
           MOVE  'Finalizar Cadastro'  TO  SGTS0294-NM-ACAO (4)
           MOVE  'Excluir Cadastro'    TO  SGTS0294-NM-ACAO (5)
           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       900000-PROCEDIMENTOS-CONTAINER  SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

      *--> Grava o container de requisição do módulo a executar
           PERFORM  982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o módulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

      *--> Executa módulos com container
           PERFORM  910000-EXECUTA-MODULOS

      *--> Lê o container de erro do módulo executado
           PERFORM  972000-LER-CONTAINER-DE-ERRO

      *--> Lê o container de resposta do módulo executado
           PERFORM  973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       910000-EXECUTA-MODULOS          SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

      *--> Executar Módulos com container, sem Serviço
           EVALUATE NM-PGM

      *--> Executar Módulo SGTSL078
               WHEN SGTSL078
                    EXEC CICS LINK
                              PROGRAM ( SGTSL078   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

      *--> Execução de Módulo não prevista
               WHEN OTHER
                    SET  ERRO-PROGRAMADO
                    TO  TRUE
                    MOVE  9006         TO  MSG1-CD-ERRO-TS
                    STRING 'Não definida a execução do módulo '
                            NM-PGM      '.'
                            DELIMITED BY SIZE
                            INTO          MSG1-TX-ERRO-TS
                    END-STRING
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    PERFORM 911000-VER-ERRO-MODULO-CHAMADO
           END-EVALUATE
           .
       910000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       911000-VER-ERRO-MODULO-CHAMADO  SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

      *--> Análise do retorno das chamadas dos programas
           SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
           MOVE  3004                  TO  MSG1-CD-ERRO-TS
           STRING  'Erro ao acionar o módulo ' NM-PGM      '.'
                   'RESP  = '    MSG1-RESP1  ', '
                   'RESP2 = '    MSG1-RESP2
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
           PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           .
       911000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    Section da Faixa de 970000 a 979900 para Leitura de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       970000-LER-REQUISICAO-RECEBIDA  SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao
           MOVE  'SGTS0294-RQSC'       TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM  973100-LER-CONTAINER-NODATA

           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

      *--> Obtem o tamanho esperado da requisicao
           MOVE  LENGTH OF SGTS0294-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisicao X tamanho recebido
           IF  QT-CCT-LIDO                 NOT EQUAL QT-CCT-EPRO
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
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0294-RQSC)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       970000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *--> A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

      *--> Le o container de erro dos modulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           INITIALIZE                      IIBKERRO-PRM-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                     )
                      INTO      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      RESP      ( MSG1-RESP1                  )
                      RESP2     ( MSG1-RESP2                  )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL DFHRESP(CONTAINERERR)
                     CONTINUE
               WHEN  MSG1-RESP1            EQUAL DFHRESP(NORMAL)
                     SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                     MOVE  3004        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     MOVE  NM-PGM      TO  MSG1-NM-PGM-ACND
                     MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                     MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
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
      *--> Recupera o Container com os Dados de Resposta (Saída).
      *-----------------------------------------------------------------
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)

      *--> Trata área de resposta do programa acionado (NODATA)
           PERFORM 973100-LER-CONTAINER-NODATA

           MOVE  0017                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de resposta
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática erro CICS, deve ser previsto
      *    o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Recupera o container de resposta do programa que foi acionado
           EVALUATE NM-PGM

      *--> Lê container de resposta do SGTSL078
               WHEN SGTSL078
                    MOVE LENGTH OF SGTSL078-RPST      TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSL078-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

      *--> Módulo não previsto para leitura do container
               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9003         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na leitura da resposta do módulo '
                             NM-PGM      '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                    END-STRING
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *--> Recupera o tamanho recebido da resposta do programa acionado
      *-----------------------------------------------------------------
           MOVE  0018                  TO  MSG1-CD-SEQL-ERRO

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
       980000-GRAVAR-CTNR-RESPOSTA     SECTION.
      *-----------------------------------------------------------------
           MOVE  0019                  TO  MSG1-CD-SEQL-ERRO

      *--> Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

      *--> Monta o nome do container de resposta
           MOVE  'SGTS0294-RPST'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS
                PUT  CONTAINER ( NM-CTNR                 )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTS0294-RPST )
                     FROM      ( SGTS0294-RPST           )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       980000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *   Section da Faixa de 980000 a 989900 para Gravação de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *--> Postar Container com Dados de Requisição (Entrada)
      *-----------------------------------------------------------------
           MOVE  0020                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para gravação do container de requisição
           SET   PUT-CONTAINER         TO  TRUE

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_modulo -RQSC
           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RQSC'               TO  NM-CTNR(9:5)

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Grava container da requisição do serviço IIB a ser acionado
      *    Em caso de erro, a captura automática do CICS trata o erro
           EVALUATE NM-PGM

      *--> Grava container de requisição do programa SGTSL069
               WHEN SGTSL078
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSL078-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSL078-RQSC )
                    END-EXEC

      *--> Módulo não previsto para gravar container de requisição
               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9004         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na gravação da requisição do módulo.'
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                    END-STRING
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
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
           MOVE  0021                  TO  MSG1-CD-SEQL-ERRO

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
               WHEN  MSG1-RESP1   EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1   EQUAL  DFHRESP(NORMAL)
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
           MOVE  0022                  TO  MSG1-CD-SEQL-ERRO

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
               WHEN  MSG1-RESP1   EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1   EQUAL  DFHRESP(NORMAL)
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
      * As sections abaixo iniciadas com 999000 representam os tratamen
      * tos de erro utilizando o Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *--> Monitora as variáveis dos módulos chamados
           IF  ERRO-PROGRAMA-ACIONADO
               PERFORM  999100-MONITORAR-PGM-ACIONADO
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

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE '0294-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0294-CD-CTRA-TIT-VL
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0294-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0294-CD-OPR-ETRD-MCP    NUMERIC
               MOVE  SGTS0294-CD-OPR-ETRD-MCP
                                       TO  MCR-S9-15
               MOVE  MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0294-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0294-NR-VRS-OPR-ETRD
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0294-004-NR-SEQL-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0294-NR-SEQL-ORD-NEGO
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0294-005-IC-TAB'      TO  MSGKW999-NM-VRV-MNT
           MOVE  IC-TAB                TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT


           MOVE '0294-006-IC-RESTO'    TO  MSGKW999-NM-VRV-MNT
           MOVE  IC-RESTO              TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0294-007-CNT-CD-TIP-EST-NEGO-1'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  CNT-CD-TIP-EST-NEGO-1
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT


      *--> Grava a Ocorrencia de Erro no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava a Ocorrencia de Erro no Container de Erro
           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                  )
                      FROM      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      RESP      ( MSG1-RESP1                  )
                      RESP2     ( MSG1-RESP2                  )
           END-EXEC

      *--> Encerra a transacao
           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999100-MONITORAR-PGM-ACIONADO   SECTION.
      *--> Monitora as Variáveis dos Módulos Chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM
      *------> Monitora as Variaveis de Entrada do SGTSL078
               WHEN  SGTSL078
                     MOVE 'L078-001-CD-TIP-SRVC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL078-CD-TIP-SRVC
                                       TO  MCR-S9-15
                     MOVE  MCR-S9-15   TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L078-002-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL078-CD-CTRA-TIT-VL
                                       TO  MCR-S9-15
                     MOVE  MCR-S9-15   TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L078-003-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSL078-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTSL078-CD-OPR-ETRD-MCP
                                       TO  MCR-S9-15
                         MOVE  MCR-S9-15
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L078-004-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL078-NR-VRS-OPR-ETRD
                                       TO  MCR-S9-15
                     MOVE  MCR-S9-15   TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L078-005-NR-SEQL-ORD-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL078-NR-SEQL-ORD-ULT
                                       TO  MCR-S9-15
                     MOVE  MCR-S9-15   TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L078-006-TS-EST-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL078-TS-EST-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                     CONTINUE
           END-EVALUATE
           .
       999100-SAI.
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
      *---------------------- FIM SGTS0294 -----------------------------