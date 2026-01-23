      *-----------------------------------------------------------------
      * Programa..: SGTS0020
      * Analista..: F2746899 - Eduardo Pio Ribeiro Junior
      * Autor.....: F2746899 - Eduardo Pio Ribeiro Junior
      * Data......: 15/07/2015
      * Objetivo..: Incluir Categoria Bacen TVM
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
       PROGRAM-ID.                     SGTS0020.

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
                                       '*** SGTS0020 ***'.
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
       01  CD-CHV-USU                  PIC  X(08) VALUE SPACES.

       01  PSC-FIM                     PIC S9(09) COMP-5 VALUE ZEROS.

       01  IN-CTGR-BC                  PIC  X(01) VALUE SPACES.
           88  IND-CATEGORIA-ATIVA                VALUE 'S'.
           88  IND-CATEGORIA-NAO-ATIVA            VALUE 'N'.
       01  NM-CTGR-BC                  PIC  X(40) VALUE SPACES.

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
       01  SGTS0020-DADOS.
-INC SGTK0020

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

           PERFORM 100000-BUSCA-REQUISICAO-IIB

           PERFORM 200000-BUSCA-MATRICULA-IIB

           PERFORM 300000-PROCESSA-NEGOCIO

           PERFORM 400000-GRAVA-RESPOSTA-IIB

           PERFORM  999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-BUSCA-REQUISICAO-IIB     SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTS0020-RQSC

           SET   GET-CONTAINER         TO  TRUE
           MOVE  'SGTS0020-RQSC'       TO  NM-CTNR

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                 )
                      INTO      ( SGTS0020-RQSC           )
                      FLENGTH   ( LENGTH OF SGTS0020-RQSC )
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

           PERFORM  310000-VALIDAR-PARAMETROS

           PERFORM  320000-CONSULTAR-NOME-CTGR

           IF  SGTST029-INCLUIR
               PERFORM  330000-INCLUIR-CATEGORIA
           END-IF

           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       310000-VALIDAR-PARAMETROS       SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           IF SGTS0020-NM-CTGR-BC          EQUAL  SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0249              TO  MSG1-CD-ERRO-TS
               MOVE  'Nome da categoria Bacen não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0020-IN-CTGR-BC         EQUAL  SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0940              TO  MSG1-CD-ERRO-TS
               MOVE  'Indicador de Categoria Ativa TVM do Bacen não info
      -              'rmado.'          TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  FUNCTION UPPER-CASE(SGTS0020-IN-CTGR-BC)
                                       TO  IN-CTGR-BC

           IF  IN-CTGR-BC                  NOT EQUAL  'S'  AND  'N'
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0941              TO  MSG1-CD-ERRO-TS
               MOVE  'Indicador de Categoria Ativa TVM do Bacen inválido
      -              '.'               TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       310000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       320000-CONSULTAR-NOME-CTGR      SECTION.
      *--> Verificar se o Nome da Categoria Bacen já está cadastrado
      *-----------------------------------------------------------------

           PERFORM  321000-RETIRA-ESPACO-INI-DESCR

           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTSLJ04-DADOS

           SET   SGTSLJ04-VRF-EXT-DCR  TO  TRUE
           MOVE  SGTS0020-NM-CTGR-BC   TO  SGTSLJ04-DCR-CTGR-BC

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

           IF  SGTSLJ04-IN-GERAL           EQUAL  ZEROS
               INITIALIZE                  SGTST029-DADOS
               SET   SGTST029-INCLUIR  TO  TRUE
           ELSE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0943              TO  MSG1-CD-ERRO-TS
               MOVE  'Categoria TVM do Bacen já cadastrada.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       320000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       321000-RETIRA-ESPACO-INI-DESCR  SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           INSPECT  SGTS0020-NM-CTGR-BC
                    TALLYING  PSC-FIM
                    FOR LEADING SPACES

           IF  PSC-FIM                     GREATER  ZEROS
               MOVE  SPACES            TO  NM-CTGR-BC
               MOVE  SGTS0020-NM-CTGR-BC(PSC-FIM + 1:40 - PSC-FIM)
                                       TO  NM-CTGR-BC
               MOVE  SPACES            TO  SGTS0020-NM-CTGR-BC
               MOVE  NM-CTGR-BC        TO  SGTS0020-NM-CTGR-BC
           END-IF

           .
       321000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       330000-INCLUIR-CATEGORIA        SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

              MOVE  IN-CTGR-BC
                                       TO  SGTST029-IN-CTGR-TVM
              MOVE  SGTS0020-NM-CTGR-BC
                                       TO  SGTST029-NM-CTGR-TVM
              MOVE  'CURRENT'          TO  SGTST029-TS-ATL
              MOVE  CD-CHV-USU         TO  SGTST029-CD-USU-ATL

              PERFORM  910000-ACIONAR-SGTST029

               IF  SGTST029-CD-RTN-PGM     EQUAL  ZEROS
                   MOVE  'Inclusão da Categoria Bacen de TVM efetivada c
      -                  'om sucesso.'
                                       TO  SGTS0020-TX-MSG-RTN-PRCT
               END-IF

           .
       333000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       400000-GRAVA-RESPOSTA-IIB       SECTION.
      *--> Retorna os parâmetros de resposta ou dados de saída.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           SET   PUT-CONTAINER         TO  TRUE
           MOVE  'SGTS0020-RPST'       TO  NM-CTNR

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                 )
                      FROM      ( SGTS0020-RPST           )
                      FLENGTH   ( LENGTH OF SGTS0020-RPST )
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
       910000-ACIONAR-SGTST029         SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

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
           MOVE '0020-001-NM-CTGR-BC'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0020-NM-CTGR-BC
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0020-002-IN-CTGR-BC'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0020-IN-CTGR-BC
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

      *---------------------- FIM SGTS0020 -----------------------------