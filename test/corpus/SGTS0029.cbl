      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0029
      * ANALISTA..: F2054109 - CINTIA IGLESIAS
      * AUTOR.....: F2020377 - CINTIA IGLESIAS
      * DATA......: 04/03/2015
      * OBJETIVO..: Incluir especies de debentures no sistema SGT
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS002 10.06.2019 - F0950413 - DE 2016/145195 - Adequação das
      *        das mensagens de erro ao padrão da mensageria,
      *        Alteração da chamada de módulos com comando 'CALL' para,
      *        'EXEC CICS LINK (COMMAREA)'.
      * VRS001 29.07.2015 F2054109  P9327 /DE 2015/120821 - IMPLANTACAO
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0029.

      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------

       77  CTE-INICIO-SS          PIC  X(035)         VALUE
           '*** STORAGE SECTION COMECA AQUI ***'.

       01  CTE-PROG               PIC X(17)  VALUE '*** SGTS0029 ***'.
       01  CTE-VERS               PIC X(06)  VALUE 'VRS002'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTST031                PIC  X(008)   VALUE 'SGTST031'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de Trabalho
       01  TAM-CNTR                    PIC S9(04) COMP-5 VALUE 0.
       01  IC-TAB                      PIC S9(04) COMP-5 VALUE 0.
       01  CT-INCL-SUCESSO             PIC  9(04)  VALUE 0.
       01  CD-CHV-USU                  PIC  X(8)   VALUE ' '.

      *--> Retirar zeros mensagem
       01  NUMERO-COM-ZEROS            PIC  X(09).
       01  NUMERO-COM-ESPACES          PIC  X(10).
       01  CONTA-ZEROS                 PIC  9(01) VALUE 0.
       01  MENSAGEM-SEM-ZEROS          PIC  X(077).

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      ---------------9.

      *-------------------------- B O O K S ----------------------------
      *--> Book do container de Erro (IIB)
-INC IIBKERRO

      *--> Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> PERSISTENCIA DA TABELA  ESPC_DBTR_OPR
       01  SGTST031-DADOS.
-INC SGTKT031

      *--> BOOK DA SUBROTINA SGTS0029
       01  SGTS0029-DADOS.
-INC SGTK0029


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

           MOVE  'SGTS0029-RQSC'       TO  NM-CTNR

           EXEC CICS GET
                     CONTAINER ( NM-CTNR    )
                     NODATA
                     FLENGTH   ( TAM-CNTR   )
                     RESP      ( MSG1-RESP1 )
                     RESP2     ( MSG1-RESP2 )
           END-EXEC

           EXEC CICS GET
                CONTAINER ( NM-CTNR       )
                INTO      ( SGTS0029-RQSC )
                FLENGTH   ( TAM-CNTR      )
                RESP      ( MSG1-RESP1    )
                RESP2     ( MSG1-RESP2    )
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
       300000-PROCESSAR-NEGOCIO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  310000-VALIDAR-PARAMETRO

           MOVE  ZEROS     TO   CT-INCL-SUCESSO

           PERFORM  320000-TRATAR-INCL-ESPC-DBTR
                    VARYING IC-TAB
                    FROM 0001 BY 0001
                    UNTIL IC-TAB > SGTS0029-QT-OCR-TAB

           IF  CT-INCL-SUCESSO             EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1049              TO  MSG1-CD-ERRO-TS
               MOVE  'Espécie(s) de debênture(s) já existente(s).'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           ELSE
               IF  CT-INCL-SUCESSO         EQUAL  0001
                   MOVE  'Foi realizada a inclusão de uma nova espécie d
      -                  'e debênture.'
                                       TO  SGTS0029-TX-MSG-RTN-PRCT
               ELSE
                   PERFORM  330000-FORMATAR-MSG-INCLUSAO
                   MOVE  MENSAGEM-SEM-ZEROS
                                       TO  SGTS0029-TX-MSG-RTN-PRCT
               END-IF
           END-IF
           .
       300000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       310000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTS0029-QT-OCR-TAB         EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1048              TO  MSG1-CD-ERRO-TS
               MOVE  'Nenhuma espécie debênture informada.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           PERFORM  311000-VALIDAR-LISTA-DEBENTURE
                    VARYING IC-TAB
                    FROM  0001 BY 0001
                    UNTIL IC-TAB > SGTS0029-QT-OCR-TAB

           .
       310000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       311000-VALIDAR-LISTA-DEBENTURE        SECTION.
      *--> Efetua validação da lista de espécies de debentures
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTS0029-CD-ESPC-DBTR(IC-TAB)
                                           EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0016              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da espécie de debênture da operação inválid
      -              'o.'              TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       311000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       320000-TRATAR-INCL-ESPC-DBTR    SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTST031-DADOS

           SET   SGTST031-INCLUIR      TO  TRUE
           MOVE  SGTS0029-CD-ESPC-DBTR(IC-TAB)
                                       TO  SGTST031-CD-ESPC-DBTR-OPR
           MOVE  'S'                   TO  SGTST031-IN-ESPC-DBTR-ATV
           MOVE  'CURRENT'             TO  SGTST031-TS-ATL
           MOVE  CD-CHV-USU            TO  SGTST031-CD-USU-ATL

           PERFORM  970101-CHAMA-SGTST031

           IF  SGTST031-CD-RTN-PGM         EQUAL  ZEROS
                ADD   0001             TO  CT-INCL-SUCESSO
           END-IF
           .
       320000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       330000-FORMATAR-MSG-INCLUSAO   SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           MOVE  CT-INCL-SUCESSO       TO  NUMERO-COM-ZEROS

           INSPECT  NUMERO-COM-ZEROS
                    TALLYING CONTA-ZEROS FOR LEADING '0'

           MOVE  NUMERO-COM-ZEROS(CONTA-ZEROS + 1:9 - CONTA-ZEROS)
                                       TO  NUMERO-COM-ESPACES

           STRING  NUMERO-COM-ESPACES
                   DELIMITED BY SIZE
                   ' '
                   DELIMITED BY SIZE
                   INTO NUMERO-COM-ESPACES

           STRING  'Foram realizadas '
                   DELIMITED BY SIZE
                   NUMERO-COM-ESPACES
                   DELIMITED BY SPACE
                   ' inclusoes de novas espécies de debenture.'
                   DELIMITED BY SIZE
                   INTO MENSAGEM-SEM-ZEROS
           .
       310000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       400000-GRAVAR-RESPOSTA          SECTION.
      *--> Retorna Parametros de Resposta ou Dados de Saida.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'SGTS0029-RPST'       TO  NM-CTNR

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                 )
                      FROM      ( SGTS0029-RPST           )
                      FLENGTH   ( LENGTH OF SGTS0029-RPST )
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
       970101-CHAMA-SGTST031                SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTST031              TO  NM-PGM

           EXEC CICS  LINK
                      PROGRAM  ( NM-PGM                   )
                      COMMAREA ( SGTST031-DADOS           )
                      LENGTH   ( LENGTH OF SGTST031-DADOS )
           END-EXEC

           IF  SGTST031-CD-RTN-PGM         NOT EQUAL  ZEROS  AND  0803
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  3004              TO  MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST031-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTST031-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       970101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  ERRO-PROGRAMA-ACIONADO
               PERFORM 999100-MONITORAR-VRV-PGM-CHMD
           END-IF

           PERFORM 999200-MONITORAR-VRV-PGM-E-ENC
           .
       999000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999100-MONITORAR-VRV-PGM-CHMD   SECTION.
      *-----------------------------------------------------------------
      *--> Monitora as variáveis dos módulos chamados

           EVALUATE  NM-PGM
               WHEN  SGTST031
                     MOVE 'SGTST031-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST031-ACAO
                                       TO  MCR-S9-15
                     MOVE  MCR-S9-15   TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST031-CD-ESPC-DBTR-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST031-CD-ESPC-DBTR-OPR
                                       TO  MCR-S9-15
                     MOVE  MCR-S9-15
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST031-IN-ESPC-DBTR-ATV'

                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST031-IN-ESPC-DBTR-ATV

                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST031-TS-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST031-TS-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST031-CD-USU-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST031-CD-USU-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                     CONTINUE
           END-EVALUATE
           .
       999100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999200-MONITORAR-VRV-PGM-E-ENC  SECTION.
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
           MOVE 'SGTS0029-QT-OCR-TAB'  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0029-QT-OCR-TAB   TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           PERFORM VARYING IC-TAB FROM 1 BY 1
                     UNTIL IC-TAB  >  SGTS0029-QT-OCR-TAB
              MOVE 'SGTS0029-CD-ESPC-DBTR'
                                       TO  MSGKW999-NM-VRV-MNT
              MOVE  SGTS0029-CD-ESPC-DBTR (IC-TAB)
                                       TO  MCR-S9-15
              MOVE  MCR-S9-15          TO  MSGKW999-VL-VRV-MNT
              PERFORM  999003-ADICIONA-VRV-MNT
           END-PERFORM

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
       999200-SAI.
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

      *---------------------- FIM SGTS0029 -----------------------------