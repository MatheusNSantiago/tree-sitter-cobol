      *-----------------------------------------------------------------
      * Programa..: SGTS0022
      * Analista..: F2746899 - Eduardo Pio Ribeiro Junior
      * Autor.....: F2746899 - Eduardo Pio Ribeiro Junior
      * Data......: 15/07/2015
      * Objetivo..: Exclusao de Categoria Bacen TVM
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
       PROGRAM-ID.                     SGTS0022.

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
                                       '*** SGTS0022 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS002'.

       77  GDA-DFHEIBLK                PIC  X(85) VALUE SPACES.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTSLJ04                PIC  X(08) VALUE 'SGTSLJ04'.
           03  SGTST029                PIC  X(08) VALUE 'SGTST029'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de Trabalho
       01  CD-CHV-USU                  PIC  X(08) VALUE ' '.


       01  CTG-OPR                     PIC  9(09) VALUE ZEROS.
       01  CTG-OPR-ALFA                PIC  X(09) VALUE SPACES.
       01  PSC-INI                     PIC  9(01) VALUE ZEROS.


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
       01  SGTS0022-DADOS.
-INC SGTK0022

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------
       01  SGTST029-DADOS.
-INC SGTKT029

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

           INITIALIZE                      SGTS0022-RQSC

           SET   GET-CONTAINER         TO  TRUE
           MOVE  'SGTS0022-RQSC'       TO  NM-CTNR

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                 )
                      INTO      ( SGTS0022-RQSC           )
                      FLENGTH   ( LENGTH OF SGTS0022-RQSC )
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
       300000-PROCESSA-NEGOCIO         SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           PERFORM 310000-VALIDAR-PARAMETROS

           PERFORM 320000-CONSULT-EXIST-COD-CATG

           IF  SGTST029-EXCLUIR
               MOVE  SGTS0022-CD-CTGR-BC
                                       TO  SGTST029-CD-CTGR-TVM

               PERFORM 910000-ACESSA-TAB-CTGR-BC-TVM

               IF  SGTST029-CD-RTN-PGM     EQUAL  ZEROS
                   MOVE  'Exclusão da Categoria Bacen de TVM efetivada c
      -                  'cm sucesso.'
                                       TO  SGTS0022-TX-MSG-RTN-PRCT
               ELSE
                   SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                   MOVE  3004          TO  MSG1-CD-ERRO-TS
                   STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                           DELIMITED BY SIZE
                           INTO            MSG1-TX-ERRO-TS
                   END-STRING
                   MOVE  NM-PGM        TO  MSG1-NM-PGM-ACND
                   MOVE  SGTST029-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
                   MOVE  SGTST029-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
                   PERFORM  969000-MONITORAR-VARIAVEIS
                   PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           END-IF

           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       310000-VALIDAR-PARAMETROS       SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTS0022-CD-CTGR-BC         EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0250              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da categoria Bacen não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       310000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       320000-CONSULT-EXIST-COD-CATG   SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTST029-DADOS

           SET   SGTST029-CONSULTAR-COD-CTGR-BC
                                       TO  TRUE
           MOVE  SGTS0022-CD-CTGR-BC   TO  SGTST029-CD-CTGR-TVM

           PERFORM 910000-ACESSA-TAB-CTGR-BC-TVM

           EVALUATE  SGTST029-CD-RTN-PGM
               WHEN  ZEROS
                     CONTINUE
               WHEN  +100
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  0942        TO  MSG1-CD-ERRO-TS
                     MOVE  'Categoria TVM do BACEN não cadastrada.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  969000-MONITORAR-VARIAVEIS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               WHEN  OTHER
                     SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                     MOVE  3004        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     END-STRING
                     MOVE  NM-PGM      TO  MSG1-NM-PGM-ACND
                     MOVE  SGTST029-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
                     MOVE  SGTST029-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
                     PERFORM  969000-MONITORAR-VARIAVEIS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           PERFORM  321000-BUSCA-QTDE-OPER-COD-BC

           IF  CTG-OPR                     EQUAL  ZEROS
               SET   SGTST029-EXCLUIR  TO  TRUE
           ELSE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1008              TO  MSG1-CD-ERRO-TS
               INSPECT  CTG-OPR
                        TALLYING PSC-INI
                        FOR LEADING ZEROS
               MOVE  CTG-OPR(PSC-INI + 1:9 - PSC-INI)
                                       TO  CTG-OPR-ALFA
               STRING  'Categoria TVM do BACEN TVM usado em '
                       DELIMITED BY SIZE
                       CTG-OPR-ALFA
                       DELIMITED BY SPACES
                       ' operações e não permite exclusão.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       320000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       321000-BUSCA-QTDE-OPER-COD-BC   SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTSLJ04-DADOS CTG-OPR

           SET   SGTSLJ04-CTG-CD-CTGR-BC-QQ-EST
                                       TO  TRUE
           MOVE  SGTS0022-CD-CTGR-BC   TO  SGTSLJ04-CD-CTGR-BC

      *--> Movimenta o nome do modulo para monitoracao de variaveis
           MOVE  SGTSLJ04              TO  NM-PGM

           EXEC CICS  LINK
                      PROGRAM  ( NM-PGM                   )
                      COMMAREA ( SGTSLJ04-DADOS           )
                      LENGTH   ( LENGTH OF SGTSLJ04-DADOS )
           END-EXEC

           IF  SGTSLJ04-CD-RTN-PGM         EQUAL  ZEROS
               MOVE  ZEROS             TO  CTG-OPR
               MOVE  SGTSLJ04-IN-GERAL TO  CTG-OPR
           ELSE
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

           .
       321000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       400000-GRAVA-RESPOSTA-IIB       SECTION.
      *--> Retorna os parâmetros de resposta ou dados de saída.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           SET   PUT-CONTAINER         TO  TRUE
           MOVE  'SGTS0022-RPST'       TO  NM-CTNR

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                 )
                      FROM      ( SGTS0022-RPST           )
                      FLENGTH   ( LENGTH OF SGTS0022-RPST )
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
       910000-ACESSA-TAB-CTGR-BC-TVM   SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO


      *--> Movimenta o nome do modulo para monitoracao de variaveis
           MOVE  SGTST029              TO  NM-PGM

           EXEC CICS  LINK
                      PROGRAM  ( NM-PGM                   )
                      COMMAREA ( SGTST029-DADOS           )
                      LENGTH   ( LENGTH OF SGTST029-DADOS )
           END-EXEC

           IF  SGTST029-CD-RTN-PGM         NOT EQUAL  ZEROS  AND
                                           NOT EQUAL  +100
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  3004              TO  MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST029-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTST029-CD-RTN-PGM
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
      *------> Monitora as Variaveis de Entrada do SGTST005
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

      *------> Monitora as Variaveis de Entrada do SGTST005
               WHEN  SGTST029
                     MOVE 'T029-001-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST029-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T029-002-CD-CTGR-TVM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST029-CD-CTGR-TVM
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T029-003-NM-CTGR-TVM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST029-NM-CTGR-TVM
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T029-004-IN-CTGR-TVM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST029-IN-CTGR-TVM
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

      *--> Em caso de erro, desfaz as atualizações em banco de dados
           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC

      *--> Verifica se o tipo de erro gerado possui tratamento interno
           EVALUATE  TRUE
               WHEN  ERRO-CONTAINER
                     PERFORM  999005-TRATA-ERRO-CTNR
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

      *--> Adiciona Campos de Entrada
           MOVE '0022-001-CD-CTGR-BC'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0022-CD-CTGR-BC
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

      *---------------------- FIM SGTS0022 -----------------------------