      *-----------------------------------------------------------------
      * Programa..: SGTS0019
      * Analista..: F2746899 - Eduardo Pio Ribeiro Junior
      * Autor.....: F2746899 - Eduardo Pio Ribeiro Junior
      * Data......: 15/07/2015
      * Objetivo..: Consultar Lista de Categoria Bacen TVM
      * Compilacao: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS002 02/05/2019 - F8650584 - DE 2016/145195
      *                     Inclusão de Mensageria e atualização no
      *                     Tratamento de Ocorrências de Erros
      * VRS001 15/07/2015 - F2746899 - DE 2013/120821 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0019.

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
           '*** Storage Section Comeca Aqui ***'.
       77  CTE-PROG                    PIC  X(17) VALUE
                                       '*** SGTS0019 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS002'.

       77  GDA-DFHEIBLK                PIC  X(85) VALUE SPACES.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  ATCS6000                PIC  X(08) VALUE 'ATCS6000'.
           03  IIBP2000                PIC  X(08) VALUE 'IIBP2000'.
           03  SGTSL029                PIC  X(08) VALUE 'SGTSL029'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de Trabalho
       01  CD-CHV-USU                  PIC  X(08) VALUE SPACES.


      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book dos dados do usuário
-INC IIBKOPRD

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

      *--> Book do container de requisicao e resposta deste modul
       01  SGTS0019-DADOS.
-INC SGTK0019

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      *--> Book da LISTA da Tabela CTGR_BC_TVM
       01  SGTSL029-DADOS.
-INC SGTKL029

      *--> Book da ATCS6000 - Retorna Nome Funcionario
       01  ATCS6000-DADOS.
-INC ATCK6000

       01  CTE-FINAL-SS                PIC  X(40) VALUE
           '*** Storage Section Termina Aqui ***'.

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

           PERFORM  100000-BUSCA-REQUISICAO-IIB

           PERFORM  200000-BUSCA-MATRICULA-IIB

           PERFORM  300000-PROCESSA-NEGOCIO

           PERFORM  400000-GRAVA-RESPOSTA-IIB

           PERFORM  999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-BUSCA-REQUISICAO-IIB     SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTS0019-RQSC

           MOVE  'SGTS0019-RQSC'       TO NM-CTNR

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                 )
                      INTO      ( SGTS0019-RQSC           )
                      FLENGTH   ( LENGTH OF SGTS0019-RQSC )
                      RESP      ( MSG1-RESP1              )
                      RESP2     ( MSG1-RESP2              )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  3000        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro na manipulação de Container de dados.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-BUSCA-MATRICULA-IIB      SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'PRM-INF-OPRD'        TO NM-CTNR

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR              )
                      INTO      ( CD-CHV-USU           )
                      FLENGTH   ( LENGTH OF CD-CHV-USU )
                      RESP      ( MSG1-RESP1           )
                      RESP2     ( MSG1-RESP2           )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  0200        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro ao obter código do usuário.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-PROCESSA-NEGOCIO         SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  310000-VALIDAR-PARAMETROS

           PERFORM  320000-CONSULTAR-TABELA

           PERFORM  330000-MOVER-DADOS
                    VARYING SGTSL029-IC-TAB
                    FROM 0001 BY 0001
                    UNTIL SGTSL029-IC-TAB > SGTSL029-QT-OCR-TAB
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       310000-VALIDAR-PARAMETROS       SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTSL029-DADOS

           .
       310000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       320000-CONSULTAR-TABELA         SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO


           IF  SGTS0019-TS-ULT-PSQD        EQUAL SPACES
               SET   SGTSL029-LISTA-TOTAL-CD
                                       TO  TRUE
           ELSE
               SET   SGTSL029-LISTA-TOTAL-TS
                                       TO  TRUE
               MOVE  SGTS0019-TS-ULT-PSQD  TO  SGTSL029-TS-ATL-E
           END-IF

      *--> Movimenta o nome do modulo para monitoracao de variaveis
           MOVE  SGTSL029              TO  NM-PGM

           EXEC CICS  LINK
                      PROGRAM  ( NM-PGM                   )
                      COMMAREA ( SGTSL029-DADOS           )
                      LENGTH   ( LENGTH OF SGTSL029-DADOS )
           END-EXEC

      *     MOVE LENGTH OF  SGTSTL029-DADOS
      *                                TO  EIBCALEN
      *     CALL  SGTSL029          USING  DFHEIBLK
      *                                    SGTSL029-DADOS

           IF  SGTSL029-CD-RTN-PGM         NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  3004              TO  MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTSL029-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTSL029-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EVALUATE  TRUE
               WHEN  SGTSL029-QT-OCR-TAB   GREATER ZEROS
                     MOVE  SGTSL029-QT-OCR-TAB
                                       TO  SGTS0019-QT-OCR-TAB
                     MOVE  SGTSL029-IN-CNTO-PSQ
                                       TO  SGTS0019-IN-CTNC-PSQ
               WHEN  SGTSL029-QT-OCR-TAB   EQUAL  ZEROS
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  0256        TO  MSG1-CD-ERRO-TS
                     MOVE  'Categoria Bacen ativa inexistente.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           .
       320000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       330000-MOVER-DADOS              SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTSL029-CD-CTGR-TVM       (SGTSL029-IC-TAB)
             TO  SGTS0019-CD-CTGR-BC        (SGTSL029-IC-TAB)
           MOVE  SGTSL029-NM-CTGR-TVM       (SGTSL029-IC-TAB)
             TO  SGTS0019-NM-CTGR-BC        (SGTSL029-IC-TAB)
           MOVE  SGTSL029-IN-CTGR-TVM       (SGTSL029-IC-TAB)
             TO  SGTS0019-IN-CTGR-BC        (SGTSL029-IC-TAB)
           MOVE  SGTSL029-CD-USU-ATL        (SGTSL029-IC-TAB)
             TO  SGTS0019-CD-USU-RSP-ATL    (SGTSL029-IC-TAB)

           PERFORM 331000-BUSCAR-NOME-USUARIO

           MOVE  K6000-NM-USU
             TO  SGTS0019-NM-USU-RSP-ATL    (SGTSL029-IC-TAB)

           MOVE  SGTSL029-TS-ATL            (SGTSL029-IC-TAB)
             TO  SGTS0019-TS-ATL            (SGTSL029-IC-TAB)
           .
       330000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       331000-BUSCAR-NOME-USUARIO      SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  961000-BUSCAR-CANAL

           PERFORM  962000-GRAVAR-SERVICO

           INITIALIZE                      ATCS6000-DADOS

           SET   K6000-PSQ-INF-USU     TO  TRUE
           MOVE  SGTS0019-CD-USU-RSP-ATL(SGTSL029-IC-TAB)
                                       TO  K6000-CD-USU

           PERFORM  963000-GRAVAR-REQUISICAO

           PERFORM  964000-ACIONAR-SERVICO

           PERFORM  965000-BUSCAR-RESPOSTA-ERRO

           PERFORM  966000-BUSCAR-RESPOSTA

           .
       331000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       400000-GRAVA-RESPOSTA-IIB       SECTION.
      *--> Retorna os parâmetros de resposta ou dados de saída.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'SGTS0019-RPST'       TO NM-CTNR

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                 )
                      FROM      ( SGTS0019-RPST           )
                      FLENGTH   ( LENGTH OF SGTS0019-RPST )
                      RESP      ( MSG1-RESP1              )
                      RESP2     ( MSG1-RESP2              )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  3000        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro na manipulação de Container de dados.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           .
       400000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       961000-BUSCAR-CANAL             SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  ASSIGN
                      CHANNEL ( NM-CNL     )
                      RESP    ( MSG1-RESP1 )
                      RESP2   ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  3003        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro na obtenção do Canal de dados.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           .
       961000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       962000-GRAVAR-SERVICO           SECTION.
      *--> Postar container de requisição de serviço.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

      *--> Chave IIB - Servico.Versao e Operacao.Versao
           MOVE  000071739             TO  SRVC-ID-SRVC
           MOVE  0001                  TO  SRVC-VRS-SRVC
           MOVE  000292018             TO  SRVC-ID-OPR
           MOVE  0001                  TO  SRVC-VRS-OPR

           MOVE  'PRM-RQSC-SRVC'       TO  NM-CTNR

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                      )
                      FROM      ( SRVC-PRM-RQSC-SRVC           )
                      FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC )
                      RESP      ( MSG1-RESP1                   )
                      RESP2     ( MSG1-RESP2                   )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  9000        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro ao acionar o serviço.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           .
       962000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       963000-GRAVAR-REQUISICAO        SECTION.
      *--> Postar container com dados de requisição (entrada)
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'PRM-DADOS-RQSC'      TO  NM-CTNR

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                    )
                      FROM      ( ATCS6000-DADOS             )
                      FLENGTH   ( LENGTH OF ATCS6000-DADOS   )
                      RESP      ( MSG1-RESP1                 )
                      RESP2     ( MSG1-RESP2                 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  9004        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro na gravação da requisição do módulo.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           .
       963000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       964000-ACIONAR-SERVICO          SECTION.
      *--> Acionar programa IIBP2000 para requisição do serviço.
      *-----------------------------------------------------------------
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  LINK
                      PROGRAM   ( IIBP2000   )
                      CHANNEL   ( NM-CNL     )
                      RESP      ( MSG1-RESP1 )
                      RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  9000        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro ao acionar o serviço.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           .
       964000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       965000-BUSCAR-RESPOSTA-ERRO     SECTION.
      *--> A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'PRM-ERRO'            TO  NM-CTNR
           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                     )
                      INTO      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      RESP      ( MSG1-RESP1                  )
                      RESP2     ( MSG1-RESP2                  )
           END-EXEC.

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
                     CONTINUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                     MOVE  3004        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro ao acionar o módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     END-STRING
                     MOVE  NM-PGM      TO  MSG1-NM-PGM-ACND
                     MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                     MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                     PERFORM  969000-MONITORAR-VARIAVEIS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  3000        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro na manipulação de container de dados.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           .
       965000-FIM.
           EXIT.


      *-----------------------------------------------------------------
       966000-BUSCAR-RESPOSTA          SECTION.
      *--> Recupera o container com os dados devolvidos pelo provedor
      *    do serviço.
      *-----------------------------------------------------------------
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'PRM-DADOS-RPST'      TO  NM-CTNR

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                  )
                      INTO      ( ATCS6000-DADOS           )
                      FLENGTH   ( LENGTH OF ATCS6000-DADOS )
                      RESP      ( MSG1-RESP1               )
                      RESP2     ( MSG1-RESP2               )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  9003        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro na leitura da resposta do módulo .'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           .
       966000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM
      *------> Monitora as Variaveis de Entrada do SGTSL029
               WHEN  SGTSL029
                     MOVE 'L029-001-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL029-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L029-002-CD-CTGR-TVM-E'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL029-CD-CTGR-TVM-E
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L029-003-TS-ATL-E'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL029-TS-ATL-E
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L029-004-NM-CTGR-TVM-E'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL029-NM-CTGR-TVM-E
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

           .
       969000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      * As sections abaixo iniciadas com 999000 representam os tratamen
      * tos de erro utilizando o Sistema MSG (rotina MSGS0001)
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

      *--> Verifica se o tipo de erro gerado possui tratamento interno
           EVALUATE  TRUE
               WHEN  ERRO-CONTAINER
                     PERFORM  999005-TRATA-ERRO-CTNR
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

      *--> Adiciona Campos de Entrada
           MOVE  '0019-001-TS-ULT-PSQD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0019-TS-ULT-PSQD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT


      *--> Grava a Ocorrencia de Erro no Sistema MSG - Mensageria
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

      *----------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      *----------------------------------------
-INC HLPKP999

      *-----------------------------------------------------------------
       999005-TRATA-ERRO-CTNR          SECTION.
      *--> Tratamento para erro de container
      *-----------------------------------------------------------------

           SET  ERRO-CICS              TO TRUE

      *--> Movimenta variáveis de container para monitoração
           MOVE 'Erro Container 01 - Programa'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  NM-PGM                TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'Erro Container 01 - Ação'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  NM-ACAO               TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'Erro Container 03 - Container'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  NM-CTNR               TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           IF  NM-CNL                      EQUAL  SPACES
               EXEC CICS  ASSIGN
                          CHANNEL ( NM-CNL )
               END-EXEC
           END-IF

           MOVE 'Erro Container 04 - Canal'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  NM-CNL                TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'Erro Container 05 - EIBRESP'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  EIBRESP               TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'Erro Container 06 - TX-EIBRESP'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPS0201-TX-EIBRESP   TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'Erro Container 07 - EIBRESP2'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  EIBRESP2              TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'Erro Container 08 - EIBFN'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  EIBFN                 TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'Erro Container 09 - TX-EIBFN'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPS0201-TX-EIBFN     TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           .
       999005-SAI.
           EXIT.

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
      *---------------------- FIM SGTS0019 -----------------------------