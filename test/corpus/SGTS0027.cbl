      *-----------------------------------------------------------------
      * Programa..: SGTS0027
      * Analista..: F8650584 - Roberto Fernandes Lima
      * Autor.....: F8650584 - Roberto Fernandes Lima
      * Data......: 28.07.2015
      * Objetivo..: Listar Especies de Debentures Cadastradas no SGT
      * COMPILACAO: COBOL/CICS.
      *-----------------------------------------------------------------
      * VRS002 07.06.2019 - F0950413 - DE 2016/145195 - Adequação das
      *        das mensagens de erro ao padrão da mensageria,
      *        Alteração da chamada de módulos com comando 'CALL' para,
      *        'EXEC CICS LINK (COMMAREA)'.
      * VRS001 28/07/2015 F8650584  P9327 DE 2015/120821 - Implantacao
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0027.

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
       77  CTE-INICIO-SS               PIC  X(35) VALUE
           '*** Storage Section Comeca Aqui ***'.
       77  CTE-PROG                    PIC  X(17) VALUE
                                       '*** SGTS0027 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS002'.

       77  NAO                         PIC  X(01) VALUE 'N'.
       77  SIM                         PIC  X(01) VALUE 'S'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  ATCS6000                PIC  X(08) VALUE 'ATCS6000'.
           03  CACS153C                PIC  X(08) VALUE 'CACS153C'.
           03  IIBP2000                PIC  X(08) VALUE 'IIBP2000'.
           03  SGTSL031                PIC  X(08) VALUE 'SGTSL031'.

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
       01  IC-TAB                      PIC S9(04) COMP-5 VALUE ZEROS.

      *-------------------------- B O O K S ----------------------------
      *--> Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Variaveis para tratamento de Abend e Condition
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      ---------------9.

      *--> Book da Subrotina ATCS6000
      *    Retorna Informacoes Basicas de um Funcionario
       01  ATCS6000-DADOS.
-INC ATCK6000

      *--> Book da Subrotina CACS153C - CRUD da Tabela TIP_GRT
       01  CACS153C-DADOS.
-INC CACK153C

      *--> Book Lista ESPC_DBTR_OPR
       01  SGTSL031-DADOS.
-INC SGTKL031

      *--> Book da Subrotina SGTS0027
       01  SGTS0027-DADOS.
-INC SGTK0027

       01  CTE-SAI-SS                  PIC  X(40) VALUE
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

           INITIALIZE                      SGTS0027-RQSC

           MOVE  'SGTS0027-RQSC'       TO  NM-CTNR

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                 )
                      INTO      ( SGTS0027-RQSC           )
                      FLENGTH   ( LENGTH OF SGTS0027-RQSC )
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

           IF  SGTS0027-ULT-CD-ESPC-DBTR   NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0016              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da espécie de debênture da operação inválid
      -              'o.'              TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           PERFORM  310000-LISTAR-GARANTIAS-SGT

           MOVE  SGTSL031-QT-OCR-TAB   TO  SGTS0027-QT-OCR-TAB
           MOVE  SGTSL031-IN-CNTO-PSQ  TO  SGTS0027-IN-CNTO-PSQ

           IF  SGTSL031-IN-CNTO-PSQ        EQUAL  SPACES
               MOVE  NAO               TO  SGTS0027-IN-CNTO-PSQ
           END-IF

           PERFORM  320000-MOVER-DADOS
                    VARYING                IC-TAB
                    FROM                   0001 BY 0001
                    UNTIL IC-TAB           > SGTS0027-QT-OCR-TAB

           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       310000-LISTAR-GARANTIAS-SGT     SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTSL031-DADOS

           SET   SGTSL031-LISTA-TOTAL-CD
                                       TO  TRUE
           MOVE  SGTS0027-ULT-CD-ESPC-DBTR
                                       TO  SGTSL031-CD-ESPC-DBTR-OPR-E
           MOVE  SGTSL031              TO  NM-PGM

           EXEC CICS  LINK
                      PROGRAM  ( NM-PGM                   )
                      COMMAREA ( SGTSL031-DADOS           )
                      LENGTH   ( LENGTH OF SGTSL031-DADOS )
           END-EXEC

           IF  SGTSL031-CD-RTN-PGM         NOT EQUAL  ZEROS  AND  +100
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  3004              TO  MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTSL031-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTSL031-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL031-QT-OCR-TAB         EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1039              TO  MSG1-CD-ERRO-TS
               MOVE  'Não existem espécies de debêntures a serem exibida
      -              's.'              TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       310000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       320000-MOVER-DADOS              SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTSL031-CD-ESPC-DBTR-OPR(IC-TAB)
             TO  SGTS0027-CD-ESPC-DBTR-OPR(IC-TAB)

           PERFORM  321000-OBTER-NOME-GARANTIA

           MOVE  CACS153C-TX-DCR-TIP-GRT
             TO  SGTS0027-TX-DCR-ESPC-DBTR(IC-TAB)
           MOVE  SGTSL031-IN-ESPC-DBTR-ATV(IC-TAB)
             TO  SGTS0027-IN-ESPC-DBTR-ATV(IC-TAB)
           MOVE  SGTSL031-CD-USU-ATL(IC-TAB)
             TO  SGTS0027-CD-USU-RSP-ATL(IC-TAB)

           PERFORM  322000-OBTER-NOME-USUARIO

           MOVE  K6000-NM-USU
             TO  SGTS0027-NM-USU-RSP-ATL(IC-TAB)
           MOVE  SGTSL031-TS-ATL(IC-TAB)
             TO  SGTS0027-TS-ATL(IC-TAB)
           .
       320000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       321000-OBTER-NOME-GARANTIA      SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           SET   CACS153C-CONSULTAR    TO  TRUE
           MOVE  SGTS0027-CD-ESPC-DBTR-OPR (IC-TAB)
                                       TO  CACS153C-CD-TIP-GRT
           MOVE  CACS153C              TO  NM-PGM

           EXEC CICS  LINK
                      PROGRAM  ( NM-PGM                   )
                      COMMAREA ( CACS153C-DADOS           )
                      LENGTH   ( LENGTH OF CACS153C-DADOS )
           END-EXEC

           IF  CACS153C-CD-RTN-PGM         NOT EQUAL  ZEROS  AND  +100
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  3004              TO  MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  CACS153C-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  CACS153C-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       321000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       322000-OBTER-NOME-USUARIO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  960001-BUSCAR-CANAL

      *--> Carregar o nº serviço / versão / operação / versão
           MOVE  000071739             TO  SRVC-ID-SRVC
           MOVE  0001                  TO  SRVC-VRS-SRVC
           MOVE  000292018             TO  SRVC-ID-OPR
           MOVE  0001                  TO  SRVC-VRS-OPR

      *--> Movimentar o nome do programa a ser executado
           MOVE  ATCS6000              TO  NM-PGM

           PERFORM  960002-GRAVAR-SERVICO

           INITIALIZE                      ATCS6000-DADOS

           SET   K6000-PSQ-INF-USU     TO  TRUE
           MOVE  SGTS0027-CD-USU-RSP-ATL  (IC-TAB)
                                       TO  K6000-CD-USU

           PERFORM  960003-GRAVAR-REQUISICAO

           PERFORM  960004-CHAMAR-SERVICO

           PERFORM  960005-BUSCAR-RESPOSTA-ERRO

           PERFORM  960006-BUSCAR-RESPOSTA

           IF  K6000-CD-RTN                NOT EQUAL  ZEROS
               IF  K6000-CD-RTN            EQUAL  0006
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0959          TO  MSG1-CD-ERRO-TS
                   MOVE  'Usuário não cadastrado.'
                                       TO  MSG1-TX-ERRO-TS
               ELSE
                  SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                  MOVE  3004           TO  MSG1-CD-ERRO-TS
                  MOVE  K6000-TX-LVRE  TO  MSG1-TX-RTN-PGM-ACND
                  STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                          DELIMITED BY SIZE
                          INTO             MSG1-TX-ERRO-TS
                  END-STRING
               END-IF
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  K6000-CD-RTN      TO  MSG1-CD-RTN-PGM-ACND
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       322000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       400000-GRAVAR-RESPOSTA          SECTION.
      *--> Retorna Parametros de Resposta ou Dados de Saida.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'SGTS0027-RPST'       TO  NM-CTNR

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                 )
                      FROM      ( SGTS0027-RPST           )
                      FLENGTH   ( LENGTH OF SGTS0027-RPST )
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
       960001-BUSCAR-CANAL             SECTION.
      *---> Obtem o canal em que estamos trabalhando com containers
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS ASSIGN
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
                     MOVE  3003        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro na obtenção do Canal de dados.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .
       960001-SAI.
           EXIT.

      *-----------------------------------------------------------------
       960002-GRAVAR-SERVICO           SECTION.
      *--> Postar container de requisição de serviço.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'PRM-RQSC-SRVC'       TO  NM-CTNR

           EXEC CICS PUT
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
       960002-SAI.
           EXIT.

      *-----------------------------------------------------------------
       960003-GRAVAR-REQUISICAO        SECTION.
      *--> Postar container com dados de requisição (entrada)
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'PRM-DADOS-RQSC'      TO  NM-CTNR

           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                  )
                     FROM      ( ATCS6000-DADOS           )
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
                     MOVE  9004        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro na gravação da requisição do módulo.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .
       960003-SAI.
           EXIT.

      *-----------------------------------------------------------------
       960004-CHAMAR-SERVICO           SECTION.
      *--> Acionar programa IIBP2000 para requisição do serviço.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS LINK
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
       960004-SAI.
           EXIT.

      *-----------------------------------------------------------------
       960005-BUSCAR-RESPOSTA-ERRO     SECTION.
      *--> A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'PRM-ERRO'            TO  NM-CTNR

           EXEC CICS GET
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
       960005-SAI.
           EXIT.

      *-----------------------------------------------------------------
       960006-BUSCAR-RESPOSTA          SECTION.
      *--> Recupera o container com os dados devolvidos pelo provedor
      *    do serviço.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'PRM-DADOS-RPST'      TO  NM-CTNR

           EXEC CICS GET
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
       960006-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           EXEC CICS
                SYNCPOINT ROLLBACK
           END-EXEC

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
           MOVE 'SGTS0027-ULT-CD-ESPC-DBTR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0027-ULT-CD-ESPC-DBTR
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           IF  ERRO-PROGRAMA-ACIONADO
               PERFORM  999100-MONITORAR-VRV-PGM-CHMD
           END-IF

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

      *-----------------------------------------------------------------
       999100-MONITORAR-VRV-PGM-CHMD   SECTION.
      *-----------------------------------------------------------------
      *--> Monitora as variáveis dos módulos chamados

           EVALUATE  NM-PGM
               WHEN  ATCS6000
                     MOVE 'K6000-CD-FUC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  K6000-CD-FUC
                                       TO  MCR-S9-15
                     MOVE  MCR-S9-15
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'K6000-CD-USU'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  K6000-CD-USU
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IIB Serviço - Operação'
                                       TO  MSGKW999-NM-VRV-MNT
                     STRING  SRVC-ID-SRVC '.' SRVC-VRS-SRVC ' - '
                             SRVC-ID-OPR  '.' SRVC-VRS-OPR
                             DELIMITED BY SIZE
                             INTO          MSGKW999-VL-VRV-MNT
                     END-STRING
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  CACS153C
                     MOVE 'CACS153C-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  CACS153C-ACAO
                                       TO  MCR-S9-15
                     MOVE  MCR-S9-15
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'CACS153C-CD-TIP-GRT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  CACS153C-CD-TIP-GRT
                                       TO  MCR-S9-15
                     MOVE  MCR-S9-15
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'CACS153C-TX-DCR-TIP-GRT'

                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  CACS153C-TX-DCR-TIP-GRT

                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSL031
                     MOVE 'L031-001-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL031-ACAO
                                       TO  MCR-S9-15
                     MOVE  MCR-S9-15
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L031-002-CD-ESPC-DBTR-OPR-E'

                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL031-CD-ESPC-DBTR-OPR-E
                                       TO  MCR-S9-15
                     MOVE  MCR-S9-15
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L031-003-TS-ATL-E'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL031-TS-ATL-E
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L031-004-QT-OCR-TAB'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL031-QT-OCR-TAB
                                       TO  MCR-S9-15
                     MOVE  MCR-S9-15
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L031-005-IN-CNTO-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL031-IN-CNTO-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L031-006-IC-TAB'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL031-IC-TAB
                                       TO  MCR-S9-15
                     MOVE  MCR-S9-15
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                     CONTINUE
           END-EVALUATE
           .
       999100-SAI.
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
      *---------------------- FIM SGTS0027 -----------------------------