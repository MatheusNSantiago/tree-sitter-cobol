      *-----------------------------------------------------------------
      * Programa..: SGTS0026
      * Analista..: F2190351 - Cristiane Bolfer Cajeron
      * Autor.....: F2190351 - Cristiane Bolfer Cajeron
      * Data......: 28/07/2015
      * Objetivo..: Exclusao de Instrucao CVM
      * Compilacao: Cobol/CICS
      *-----------------------------------------------------------------
      * VRS002 05/12/2019 - F0950413 DE 2016/145195 - Alteração  varia-
      *        vel QT-OPR-VCLD-TX para QT-OPR-VCLD-TEXTO, P/ erro 1036.
      * VRS001 14/02/2017 F8785286 DE 2016/145195 - Inclusão campo
      *              CD-USU-RSP-ATL para chave log (movimentar a partir
      *              do container operador).
      *        05/08/2015 - F2190351 - DE 2013/120821 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0026.

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
                                       '*** SGTS0026 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS002'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTSLJ04                PIC  X(08) VALUE 'SGTSLJ04'.
           03  SGTST030                PIC  X(08) VALUE 'SGTST030'.

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

       01  QT-OPR-VCLD                 PIC  X(09) VALUE ZEROS.
       01  QT-OPR-VCLD-TEXTO           PIC  X(10) VALUE SPACES.
       01  QT-ZEROS                    PIC  9(04) VALUE ZEROS.

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

      *--> Book do container de requisicao e resposta deste modulo
       01  SGTS0026-DADOS.
-INC SGTK0026

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      *--> Persistencia da tabela ITC_CVM_OPR_ETRD
       01  SGTST030-DADOS.
-INC SGTKT030

      *--> Verifica operaçoes vinculadas a uma Instrução CVM.
       01  SGTSLJ04-DADOS.
-INC SGTKLJ04


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

           PERFORM  100000-BUSCAR-REQUISICAO

           PERFORM  200000-BUSCAR-MATRICULA

           PERFORM  300000-PROCESSAR-NEGOCIO

           PERFORM  400000-GRAVAR-RESPOSTA

           PERFORM  999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       100000-BUSCAR-REQUISICAO        SECTION.
      *--> Obtem  Parametros de Requisicao ou Dados de Entrada
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTS0026-RQSC

           SET   GET-CONTAINER         TO  TRUE
           MOVE  'SGTS0026-RQSC'       TO  NM-CTNR

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                 )
                      INTO      ( SGTS0026-RQSC           )
                      FLENGTH   ( LENGTH OF SGTS0026-RQSC )
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
       200000-BUSCAR-MATRICULA         SECTION.
      *--> Obtem a Matrícula do Usuario Logado
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-INF-OPRD'        TO  NM-CTNR

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
       300000-PROCESSAR-NEGOCIO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  310000-VALIDAR-PARAMETROS

           PERFORM  320000-VALIDAR-INSTRUCAO-CVM

           PERFORM  330000-EXCLUIR-INSTRUCAO-CVM

           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       310000-VALIDAR-PARAMETROS       SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTS0026-CD-ITC-CVM-OPR-ENTD
                                           NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0014              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da instrução da comissão de valores mobiliá
      -              'rios operação inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0026-CD-ITC-CVM-OPR-ENTD
                                           EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1028              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da instrução CVM operação não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       310000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       320000-VALIDAR-INSTRUCAO-CVM    SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTSLJ04-DADOS

           SET   SGTSLJ04-LISR-OPR-VCLD-ITC-CVM
                                       TO  TRUE
           MOVE  SGTS0026-CD-ITC-CVM-OPR-ENTD
                                       TO  SGTSLJ04-CD-ITC-CVM-OPR-E

      *--> Movimenta o nome do modulo para monitoracao de variaveis
           MOVE  SGTSLJ04              TO  NM-PGM

           EXEC CICS  LINK
                      PROGRAM  ( NM-PGM                   )
                      COMMAREA ( SGTSLJ04-DADOS           )
                      LENGTH   ( LENGTH OF SGTSLJ04-DADOS )
           END-EXEC

      *     MOVE  LENGTH OF SGTSLJ04-DADOS
      *                                 TO  EIBCALEN
      *     CALL  SGTSLJ04           USING  DFHEIBLK
      *                                     SGTSLJ04-DADOS

           IF SGTSLJ04-CD-RTN-PGM          NOT EQUAL  ZEROS
               MOVE  3004              TO  MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTSLJ04-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTSLJ04-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Avaliar a Quandidade de Operações Estruturadas Vinculadas
      *    na Instrução CVM
           MOVE  SGTSLJ04-QT-OPR-VCLD  TO  SGTS0026-QT-OPR-VCLD

           IF  SGTS0026-QT-OPR-VCLD        GREATER  ZEROS
               SET ERRO-PROGRAMADO     TO  TRUE
               MOVE  1036              TO  MSG1-CD-ERRO-TS
               PERFORM  321000-FORMATAR-MSG-OPR-VCLD
               STRING  'Exclusão não permitida, existem '
                       DELIMITED BY SIZE
                       QT-OPR-VCLD-TEXTO
                       DELIMITED BY SPACES
                       ' operações estruturadas vinculadas'
                       ' na da Instrução CVM.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       320000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       321000-FORMATAR-MSG-OPR-VCLD    SECTION.
      *--> Retira os Zeros Não Significativos e formata a mensagem com
      *    a quantidade de operações vinculadas
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTS0026-QT-OPR-VCLD  TO  QT-OPR-VCLD

           INSPECT  QT-OPR-VCLD
                    TALLYING QT-ZEROS FOR LEADING '0'

           MOVE  QT-OPR-VCLD(QT-ZEROS + 1:9 - QT-ZEROS)
                                       TO  QT-OPR-VCLD-TEXTO

           STRING  QT-OPR-VCLD-TEXTO
                   DELIMITED BY SIZE
                   ' '
                   DELIMITED BY SIZE
                   INTO                    QT-OPR-VCLD-TEXTO

           .
       321000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       330000-EXCLUIR-INSTRUCAO-CVM    SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTST030-DADOS

           MOVE  SGTS0026-CD-ITC-CVM-OPR-ENTD
                                       TO  SGTST030-CD-ITC-CVM-OPR

      *--> Consultar Instrução CVM
           SET   SGTST030-CONSULTAR    TO  TRUE

           PERFORM  910000-CHAMAR-SGTST030

      *--> Excluir Instrução CVM
           SET   SGTST030-EXCLUIR      TO  TRUE

           MOVE  CD-CHV-USU            TO  SGTST030-CD-USU-ATL

           PERFORM  910000-CHAMAR-SGTST030

           IF  SGTST030-CD-RTN-PGM     EQUAL  ZEROS
               MOVE  'Exclusão da instrução CVM efetivada com sucesso.'
                                       TO  SGTS0026-TX-MSG-RTN-PRCT
           END-IF

           .
       330000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       400000-GRAVAR-RESPOSTA          SECTION.
      *--> Retorna Parametros de Resposta ou Dados de Saida.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           SET   PUT-CONTAINER         TO  TRUE
           MOVE  'SGTS0026-RPST'       TO  NM-CTNR

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                 )
                      FROM      ( SGTS0026-RPST           )
                      FLENGTH   ( LENGTH OF SGTS0026-RPST )
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
       910000-CHAMAR-SGTST030          SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimenta o nome do modulo para monitoracao de variaveis
           MOVE  SGTST030              TO  NM-PGM

           EXEC CICS  LINK
                      PROGRAM  ( NM-PGM                   )
                      COMMAREA ( SGTST030-DADOS           )
                      LENGTH   ( LENGTH OF SGTST030-DADOS )
           END-EXEC

           IF  SGTST030-CD-RTN-PGM         NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  3004              TO  MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST030-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTST030-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       910000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM
      *------> Monitora as Variaveis de Entrada do SGTSLJ04
               WHEN  SGTSLJ04
                     MOVE 'LJ04-001-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLJ04-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'LJ04-002-CD-PRM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLJ04-CD-PRM
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'LJ04-003-CD-ITC-CVM-E'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLJ04-CD-ITC-CVM-E
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'LJ04-004-CD-ITC-CVM-OPR-E'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLJ04-CD-ITC-CVM-OPR-E
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'LJ04-005-CD-TIP-COTA-TIT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLJ04-CD-TIP-COTA-TIT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'LJ04-006-NR-VRS-TIP-COTA'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLJ04-NR-VRS-TIP-COTA
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'LJ04-007-CD-CTGR-BC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLJ04-CD-CTGR-BC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'LJ04-008-DCR-CTGR-BC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLJ04-DCR-CTGR-BC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do SGTST029
               WHEN  SGTST030
                     MOVE 'T030-001-CD-CHV-USU'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST030-CD-CHV-USU
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T030-002-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST030-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T030-003-CD-ITC-CVM-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST030-CD-ITC-CVM-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T030-004-CD-ITC-CVM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST030-CD-ITC-CVM
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T030-005-NM-ITC-CVM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST030-NM-ITC-CVM
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T030-006-IN-ITC-CVM-ATV'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST030-IN-ITC-CVM-ATV
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T030-007-CD-USU-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST030-CD-USU-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T030-008-TS-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST030-TS-ATL
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
           MOVE '0026-001-CD-ITC-CVM-OPR-ENTD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0026-CD-ITC-CVM-OPR-ENTD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0026-002-QT-OPR-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0026-QT-OPR-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'QT-OPR-VCLD-TEXTO'    TO  MSGKW999-NM-VRV-MNT
           MOVE  QT-OPR-VCLD-TEXTO     TO  MSGKW999-VL-VRV-MNT
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

           SET   ERRO-CICS             TO  TRUE

      *--> Movimenta variáveis de container para monitoração
           MOVE 'Erro Container 01 - Programa'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  NM-PGM                TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'Erro Container 01 - Ação'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  NM-ACAO               TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'Erro Container 03 - Container'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  NM-CTNR               TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           IF  NM-CNL                      EQUAL  SPACES
               EXEC CICS  ASSIGN
                          CHANNEL ( NM-CNL )
               END-EXEC
           END-IF

           MOVE 'Erro Container 04 - Canal'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  NM-CNL                TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'Erro Container 05 - EIBRESP'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  EIBRESP               TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'Erro Container 06 - TX-EIBRESP'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPS0201-TX-EIBRESP   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'Erro Container 07 - EIBRESP2'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  EIBRESP2              TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'Erro Container 08 - EIBFN'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  EIBFN                 TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'Erro Container 09 - TX-EIBFN'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPS0201-TX-EIBFN     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

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
      *---------------------- FIM SGTS0026 -----------------------------