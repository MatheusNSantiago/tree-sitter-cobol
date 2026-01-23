      *-----------------------------------------------------------------
      * Programa..: SGTS0023
      * Analista..: F2190351 - Cristiane Bolfer Cajeron
      * Autor.....: F2190351 - Cristiane Bolfer Cajeron
      * Data......: 13/07/2015
      * Objetivo..: Listar Instruções CVM
      * Compilacao: Cobol/CICS
      *-----------------------------------------------------------------
      * VRS001 13/07/2015 - F2190351 - DE 2013/120821 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0023.

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
                                       '*** SGTS0023 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS002'.


       77  SIM                         PIC  X(01) VALUE 'S'.
       77  NAO                         PIC  X(01) VALUE 'N'.
       77  HUM                         PIC  9(01) VALUE 0001.
       77  QUATRO-NOVES                PIC  9(04) VALUE 9999.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  ATCS6000                PIC  X(08) VALUE 'ATCS6000'.
           03  IIBP2000                PIC  X(08) VALUE 'IIBP2000'.
           03  SGTSL030                PIC  X(08) VALUE 'SGTSL030'.

      *--> Definição dos Variaveis do Cobolito
       COPY ALMKCBL1.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
       COPY HLPKE01B.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de Trabalho
       01  IC                          PIC S9(04) COMP-5 VALUE ZEROS.

       01  IN-ITC-CVM-ATV-ENTD         PIC  X(01) VALUE SPACES.

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book dos dados do usuário
       COPY IIBKOPRD.

      *--> Book do container de Serviços (para acionamento de serviços)
       COPY IIBKSRVC.

      *--> Book do Container de Erro
       COPY IIBKERRO.

      *--> Book para captura de abend cics da bloco codigo HLPKW999
       COPY HLPKW999.

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
       COPY SGTKERR1.

      *--> Book para captura de erro via MSG1
       COPY MSGKWMG1.

      *--> Book do container de requisicao e resposta deste modulo
       01  SGTS0023-DADOS.
       COPY SGTK0023.

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      *--> Listar a da Tabela DB2SGT.ITC_CVM_OPR_ETRD
       01  SGTSL030-DADOS.
       COPY SGTKL030.

      *-----> BOOK DO ATCS6000 - BUSCA NOME DO USUARIO
       01  ATCS6000-DADOS.
       COPY ATCK6000.

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
       COPY ALMKCBL2.

      *--> Habilita captura automatica de abend e erros CICS
       COPY HLPKE999.

           PERFORM  100000-BUSCAR-REQUISICAO

           PERFORM  200000-PROCESSAR-NEGOCIO

           PERFORM  300000-GRAVAR-RESPOSTA

           PERFORM  999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       100000-BUSCAR-REQUISICAO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTS0023-RQSC

           MOVE  'SGTS0023-RQSC'       TO  NM-CTNR

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                 )
                      INTO      ( SGTS0023-RQSC           )
                      FLENGTH   ( LENGTH OF SGTS0023-RQSC )
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
       200000-PROCESSAR-NEGOCIO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  210000-VALIDAR-PARAMETROS

           PERFORM  220000-PREPARAR-CONSULTA

           PERFORM  230000-CONSULTAR-TABELA

           PERFORM  240000-MOVER-DADOS
                    VARYING SGTSL030-IC-TAB
                    FROM 0001 BY 0001
                    UNTIL SGTSL030-IC-TAB > SGTSL030-QT-OCR-TAB

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-VALIDAR-PARAMETROS       SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTS0023-CD-ITC-CVM-OPR-ENTD
                                           NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0014              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da instrução da comissão de valores mobiliá
      -              'rios operação inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0023-CD-ITC-CVM-ENTD    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0222              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da instrução da comissão de valores mobiliá
      -              'rios inválido.'  TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  FUNCTION UPPER-CASE(SGTS0023-IN-ITC-CVM-ATV-ENTD)
                                       TO  IN-ITC-CVM-ATV-ENTD

           IF  IN-ITC-CVM-ATV-ENTD         NOT EQUAL  SPACES  AND
                                                      'S'  AND  'N'
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0224              TO  MSG1-CD-ERRO-TS
               MOVE  'Indicador da instrução da comissão de valores mobi
      -              'liários ativa inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-PREPARAR-CONSULTA        SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTSL030-DADOS

           MOVE  SGTS0023-CD-ITC-CVM-OPR-ENTD
                                       TO  SGTSL030-CD-ITC-CVM-OPR-E
           MOVE  SGTS0023-CD-ITC-CVM-ENTD
                                       TO  SGTSL030-CD-ITC-CVM-E
           MOVE  SGTS0023-IN-ITC-CVM-ATV-ENTD
                                       TO  SGTSL030-IN-ITC-CVM-ATV-E

           EVALUATE  TRUE
               WHEN  SGTS0023-IN-ITC-CVM-ATV-ENTD
                                           EQUAL  SPACES  AND
                     SGTS0023-CD-ITC-CVM-ENTD
                                           EQUAL  ZEROS
                     MOVE  SIM         TO  SGTSL030-VRV-IN-ITC-1
                     MOVE  NAO         TO  SGTSL030-VRV-IN-ITC-2
                     MOVE  HUM         TO  SGTSL030-VRV-CD-ITC-1
                     MOVE  QUATRO-NOVES
                                       TO  SGTSL030-VRV-CD-ITC-2
               WHEN  SGTS0023-IN-ITC-CVM-ATV-ENTD
                                           EQUAL  'S'  AND
                     SGTS0023-CD-ITC-CVM-ENTD
                                           EQUAL ZEROS
                     MOVE  SIM         TO  SGTSL030-VRV-IN-ITC-1
                     MOVE  SIM         TO  SGTSL030-VRV-IN-ITC-2
                     MOVE  HUM         TO  SGTSL030-VRV-CD-ITC-1
                     MOVE  QUATRO-NOVES
                                       TO  SGTSL030-VRV-CD-ITC-2
               WHEN  SGTS0023-IN-ITC-CVM-ATV-ENTD
                                           EQUAL  'N'  AND
                     SGTS0023-CD-ITC-CVM-ENTD
                                           EQUAL  ZEROS
                     MOVE  NAO         TO  SGTSL030-VRV-IN-ITC-1
                     MOVE  NAO         TO  SGTSL030-VRV-IN-ITC-2
                     MOVE  HUM         TO  SGTSL030-VRV-CD-ITC-1
                     MOVE  QUATRO-NOVES
                                       TO  SGTSL030-VRV-CD-ITC-2
               WHEN  SGTS0023-CD-ITC-CVM-ENTD
                                           NOT EQUAL  ZEROS
                     MOVE  SIM         TO  SGTSL030-VRV-IN-ITC-1
                     MOVE  NAO         TO  SGTSL030-VRV-IN-ITC-2
                     MOVE  SGTS0023-CD-ITC-CVM-ENTD
                                       TO  SGTSL030-VRV-CD-ITC-1
                     MOVE  QUATRO-NOVES
                                       TO  SGTSL030-VRV-CD-ITC-2
           END-EVALUATE

           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-CONSULTAR-TABELA         SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           SET   SGTSL030-LISTAR-ITC-CVM
                                       TO  TRUE

      *--> Movimenta o nome do modulo para monitoracao de variaveis
           MOVE  SGTSL030              TO  NM-PGM

           EXEC CICS  LINK
                      PROGRAM  ( NM-PGM                   )
                      COMMAREA ( SGTSL030-DADOS           )
                      LENGTH   ( LENGTH OF SGTSL030-DADOS )
           END-EXEC

      *     MOVE LENGTH OF  SGTSTL030-DADOS
      *                                TO  EIBCALEN
      *     CALL  SGTSL030          USING  DFHEIBLK
      *                                    SGTSL030-DADOS

           IF  SGTSL030-CD-RTN-PGM         NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  3004              TO  MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTSL030-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTSL030-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL030-IN-CNTO-PSQ        EQUAL  SPACES
               MOVE  NAO               TO  SGTS0023-IN-CTNC-PSQ
           END-IF

           EVALUATE  TRUE
               WHEN  SGTSL030-QT-OCR-TAB   GREATER ZEROS
                     MOVE  SGTSL030-QT-OCR-TAB
                                       TO  SGTS0023-QT-OCR-TAB
                     MOVE  SGTSL030-IN-CNTO-PSQ
                                       TO  SGTS0023-IN-CTNC-PSQ
               WHEN  SGTSL030-QT-OCR-TAB   EQUAL  ZEROS
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  1032        TO  MSG1-CD-ERRO-TS
                     MOVE  'Não foram localizadas instruções CVM relativ
      -                    'as à pesquisa.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       240000-MOVER-DADOS              SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           ADD   0001                  TO  IC

           MOVE  SGTSL030-CD-ITC-CVM-OPR (SGTSL030-IC-TAB)  TO
                SGTS0023-CD-ITC-CVM-OPR (SGTSL030-IC-TAB)
           MOVE  SGTSL030-CD-ITC-CVM     (SGTSL030-IC-TAB)  TO
                SGTS0023-CD-ITC-CVM     (SGTSL030-IC-TAB)
           MOVE  SGTSL030-NM-ITC-CVM     (SGTSL030-IC-TAB)  TO
                SGTS0023-NM-ITC-CVM     (SGTSL030-IC-TAB)
           MOVE  SGTSL030-IN-ITC-CVM-ATV (SGTSL030-IC-TAB)  TO
                SGTS0023-IN-ITC-CVM-ATV (SGTSL030-IC-TAB)
           MOVE  SGTSL030-CD-USU-RSP-ATL (SGTSL030-IC-TAB)  TO
                SGTS0023-CD-USU-RSP-ATL (SGTSL030-IC-TAB)
           MOVE  SGTSL030-TS-ATL         (SGTSL030-IC-TAB)  TO
                SGTS0023-TS-ATL         (SGTSL030-IC-TAB)

           PERFORM  241000-OBTER-NOME-USUARIO

           .
       240000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       241000-OBTER-NOME-USUARIO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  961000-OBTER-NOME-CANAL

           PERFORM  962000-GRAVAR-SERVICO

           INITIALIZE                      ATCS6000-DADOS

           SET   K6000-PSQ-INF-USU     TO  TRUE
           MOVE  SGTS0023-CD-USU-RSP-ATL(IC)
                                       TO  K6000-CD-USU

           PERFORM  963000-GRAVAR-REQUISICAO

           PERFORM  964000-ACIONAR-SERVICO

           PERFORM  965000-BUSCAR-RESPOSTA-ERRO

           PERFORM  966000-BUSCAR-RESPOSTA

           MOVE  K6000-NM-USU          TO  SGTS0023-NM-USU-RSP-ATL(IC)
           .
       241000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-GRAVAR-RESPOSTA          SECTION.
      *--> Retorna os parâmetros de resposta ou dados de saída.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'SGTS0023-RPST'       TO  NM-CTNR

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                 )
                      FROM      ( SGTS0023-RPST           )
                      FLENGTH   ( LENGTH OF SGTS0023-RPST )
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
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       961000-OBTER-NOME-CANAL         SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

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
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

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
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

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
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

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
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

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
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

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
      *------> Monitora as Variaveis de Entrada do SGTSL030
               WHEN  SGTSL030
                     MOVE 'L030-001-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL030-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L030-002-CD-ITC-CVM-OPR-E'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL030-CD-ITC-CVM-OPR-E
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L030-003-CD-ITC-CVM-E'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL030-CD-ITC-CVM-E
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L030-004-IN-ITC-CVM-ATV-E'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL030-IN-ITC-CVM-ATV-E
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L030-005-VRV-IN-ITC-1'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL030-VRV-IN-ITC-1
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L030-006-VRV-IN-ITC-2'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL030-VRV-IN-ITC-2
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L030-007-VRV-CD-ITC-1'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL030-VRV-CD-ITC-1
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L030-008-VRV-CD-ITC-2'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL030-VRV-CD-ITC-2
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
           MOVE '0023-001-CD-ITC-CVM-OPR-ENTD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0023-CD-ITC-CVM-OPR-ENTD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0023-002-CD-ITC-CVM-ENTD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0023-CD-ITC-CVM-ENTD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0023-003-IN-ITC-CVM-ATV-ENTD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0023-IN-ITC-CVM-ATV-ENTD
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
       COPY HLPKP999.

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
       COPY MSGKPMG1.

      *-----------------------------------------------------------------
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTS0023 -----------------------------
