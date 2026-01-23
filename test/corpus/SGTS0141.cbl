      *-----------------------------------------------------------------
      * Programa..: SGTS0141
      * Analista..: F2054109 - Cintia Iglesias
      * Autor.....: F2804421 - Eliana Cristina Ramos da Silva
      * Data......: 22/03/2017
      * Objetivo..: Listar Agentes Fiduciários da Operação.
      * Compilacao: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 22/03/2017 - F2804421 - DE 2016/145195 - Implantação
      * VRS002 03/09/2024 - F7464259 - Adequação 4966 - Inclusão de
      *                     tranche, adequação para NR-DVS-TIT
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0141.

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
       77  CTE-PROG                    PIC  X(18) VALUE
                                       '*** SGTS0141 ***'.
VRS002 77  CTE-VERS                    PIC  X(06) VALUE 'VRS002'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  IIBP2000                PIC  X(08) VALUE 'IIBP2000'.
           03  SGTSL500                PIC  X(08) VALUE 'SGTSL500'.
           03  MCISI074                PIC  X(08) VALUE 'MCISI074'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  IC-TAB                      PIC S9(04) COMP-5 VALUE ZEROS.
       01  IC-MCI                      PIC S9(04) COMP-5 VALUE ZEROS.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-TX.
           03  CD-OPR-ETRD             PIC  9(17).

      *-----------------------------------------------------------------
      *    Books Utilizados no Modulo
      *-----------------------------------------------------------------

      *--> Book do Container do Usuário Logado
-INC IIBKOPRD

      *--> Book do Container de Serviços
-INC IIBKSRVC

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book do container de requisição e resposta deste modulo
       01 SGTS0141-DADOS.
-INC SGTK0141

      *-----------------------------------------------------------------
      * Book das subrotinas chamadas
      *-----------------------------------------------------------------

      *--> Listar informações do Cliente Emissor e do Agente Fiduciario
       01  SGTSL500-DADOS.
-INC SGTKL500

      *--> A partir de uma lista de códigos de clientes,
      *    retorna os nomes dos clientes.
       01  MCISI074-DADOS.
-INC MCIKI074

       01  CTE-FINAL-SS                PIC  X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
-INC ALMKCBL2

      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999

           PERFORM  100000-INICIAR

           PERFORM  200000-PROCESSAR

           PERFORM  300000-FINALIZAR

           PERFORM  999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  970000-LER-CANAL

      *--> Define o nome do programa p/ fins manipulacao container
           MOVE  'SGTS0141'            TO  NM-PGM

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM  971000-LER-REQUISICAO

           PERFORM  974000-LER-DADOS-OPRD

           PERFORM  110000-VALIDAR-REQUISICAO

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *--> Valida campo a campo da requisicao
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTS0141-CD-OPR-ULT         NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0141-CD-OPR-ULT         NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0141-NR-VRS-ULT         NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0141-CD-EMT-PSQ         NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0028              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do cliente emissor inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0141-CD-AGT-FDAR-PSQ    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0032              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do cliente agente fiduciário inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  210000-LISTAR-OPERACOES

           PERFORM  220000-MONTAR-RESPOSTA
                    VARYING IC-TAB
                    FROM  0001 BY 0001
                    UNTIL  IC-TAB          GREATER  SGTSL500-QT-OCR OR
                           IC-TAB          GREATER  0100

           PERFORM  230000-OBTER-NOME-CLIENTES

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-LISTAR-OPERACOES         SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSL500              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0141-CD-OPR-ULT   TO  SGTSL500-CD-OPR-ULT
           MOVE  SGTS0141-NR-VRS-ULT   TO  SGTSL500-NR-VRS-ULT
           MOVE  SGTS0141-CD-TIT-PSQ   TO  SGTSL500-CD-TIT-PSQ
           MOVE  SGTS0141-CD-EMT-PSQ   TO  SGTSL500-CD-EMT-PSQ
           MOVE  SGTS0141-CD-AGT-FDAR-PSQ
                                       TO  SGTSL500-CD-AGT-FDAR-PSQ

           PERFORM  900000-EXECUTAR-MODULO

           MOVE  SGTSL500-QT-OCR       TO  SGTS0141-QT-OCR
           MOVE  SGTSL500-IN-CTNC-PSQ  TO  SGTS0141-IN-CTNC-PSQ

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-MONTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTSL500-CD-CTRA-TVM      (IC-TAB)  TO
                 SGTS0141-CD-CTRA-TVM      (IC-TAB)
           MOVE  SGTSL500-CD-OPR-ETRD      (IC-TAB)  TO
                 SGTS0141-CD-OPR-ETRD      (IC-TAB)
           MOVE  SGTSL500-NR-VRS-OPR       (IC-TAB)  TO
                 SGTS0141-NR-VRS-OPR       (IC-TAB)
           MOVE  SGTSL500-CD-USU-ULT-ATL   (IC-TAB)  TO
                 SGTS0141-CD-USU-ULT-ATL   (IC-TAB)
           MOVE  SGTSL500-TS-ULT-ATL       (IC-TAB)  TO
                 SGTS0141-TS-ULT-ATL       (IC-TAB)
           MOVE  SGTSL500-CD-PRD           (IC-TAB)  TO
                 SGTS0141-CD-PRD           (IC-TAB)
           MOVE  SGTSL500-CD-MDLD          (IC-TAB)  TO
                 SGTS0141-CD-MDLD          (IC-TAB)
           MOVE  SGTSL500-CD-TIT-OPR-ETRD  (IC-TAB)  TO
                 SGTS0141-CD-TIT-OPR-ETRD  (IC-TAB)
VRS002     MOVE  SGTSL500-NR-DVS-TIT       (IC-TAB)  TO
VRS002           SGTS0141-NR-DVS-TIT       (IC-TAB)
           MOVE  SGTSL500-CD-TIP-EST-OPR   (IC-TAB)  TO
                 SGTS0141-CD-TIP-EST-OPR   (IC-TAB)
           MOVE  SGTSL500-CD-EMT           (IC-TAB)  TO
                 SGTS0141-CD-EMT           (IC-TAB)
           MOVE  SGTSL500-CD-AGT-FDAR      (IC-TAB)  TO
                 SGTS0141-CD-AGT-FDAR      (IC-TAB)

           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-OBTER-NOME-CLIENTES      SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      MCISI074-DADOS

      *--> Movimentar o nome do programa a ser executado
           MOVE  MCISI074              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  KOPRD-CD-OPRD         TO  MCISI074-CD-CHV-USU
           MOVE  SGTS0141-QT-OCR       TO  MCISI074-QT-REG-ENTD

      *--> Lista de MCIs
      *     A rotina do MCI recebe lista de códigos (pf e pf), mas se
      *    vierem zerados, ignora a linha no retorno.
      *    Quando não houver cliente na operação, movimentando fixo
      *    qualquer código no PF, para não suprimir a linha e usando
      *    o nome PJ que neste caso estará em branco.
      *    Neste pgm:
      *    informa Agente Fiduciário no MCISI074-CD-CLI-PJ-ENTD  E
      *    informa Emissor           no MCISI074-CD-CLI-PF-ENTD
           PERFORM  VARYING  IC-TAB
                    FROM  0001 BY 0001
                    UNTIL  IC-TAB          GREATER  SGTS0141-QT-OCR  OR
                           IC-TAB          GREATER  0100
                    IF  SGTS0141-CD-EMT (IC-TAB)
                                           EQUAL  ZEROS
                        MOVE  0001
                          TO  MCISI074-CD-CLI-PF-ENTD           (IC-TAB)
                    ELSE
                        MOVE  SGTS0141-CD-EMT                   (IC-TAB)
                          TO  MCISI074-CD-CLI-PF-ENTD           (IC-TAB)
                    END-IF
                    IF  SGTS0141-CD-AGT-FDAR (IC-TAB)
                                           EQUAL  ZEROS
                        MOVE  0001
                          TO  MCISI074-CD-CLI-PJ-ENTD           (IC-TAB)
                    ELSE
                        MOVE  SGTS0141-CD-AGT-FDAR              (IC-TAB)
                          TO  MCISI074-CD-CLI-PJ-ENTD           (IC-TAB)
                    END-IF
           END-PERFORM

           PERFORM  910000-CHAMAR-SERVICO

      *--> Localiza nome do Emissor na lista retornada
           MOVE  0001                  TO  IC-MCI

           PERFORM  VARYING SGTS0141-IC-TAB
                    FROM 0001 BY 0001
                    UNTIL   SGTS0141-IC-TAB > SGTS0141-QT-OCR

                    IF  MCISI074-CD-CLI-PF-SAID (IC-MCI)
                                           EQUAL
                        SGTS0141-CD-EMT (SGTS0141-IC-TAB)
                        MOVE  MCISI074-NM-CLI-PF-SAID(IC-MCI)
                                       TO  SGTS0141-NM-EMT
                                           (SGTS0141-IC-TAB)
                        ADD   0001     TO  IC-MCI
                    ELSE
                        MOVE  SPACES   TO  SGTS0141-NM-EMT
                                           (SGTS0141-IC-TAB)
                    END-IF
           END-PERFORM

      *--> Localiza nome do Agente Fiduciário na lista retornada
           MOVE  0001                  TO  IC-MCI

           PERFORM  VARYING SGTS0141-IC-TAB
                    FROM 0001 BY 0001
                    UNTIL   SGTS0141-IC-TAB > SGTS0141-QT-OCR

                    IF  MCISI074-CD-CLI-PJ-SAID (IC-MCI)
                                           EQUAL
                        SGTS0141-CD-AGT-FDAR (SGTS0141-IC-TAB)
                        MOVE  MCISI074-NM-CLI-PJ-SAID(IC-MCI)
                                       TO  SGTS0141-NM-AGT-FDAR
                                              (SGTS0141-IC-TAB)
                        ADD   0001     TO  IC-MCI
                    ELSE
                        MOVE  SPACES   TO  SGTS0141-NM-AGT-FDAR
                                              (SGTS0141-IC-TAB)
                    END-IF
           END-PERFORM

           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-FINALIZAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

      *--> Exclui eventual container de erro de outros programas
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO

      *--> Gravando o container de resposta da subrotina
           PERFORM  980000-GRAVAR-CONTAINER-RPST
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

      *--> Grava o container de requisicao do modulo a executar
           PERFORM  982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE  NM-PGM
               WHEN  SGTSL500
                     EXEC CICS  LINK
                                PROGRAM ( SGTSL500   )
                                CHANNEL ( NM-CNL     )
                                RESP    ( MSG1-RESP1 )
                                RESP2   ( MSG1-RESP2 )
                     END-EXEC

               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9006        TO  MSG1-CD-ERRO-TS
                     STRING 'Não definida a execução do módulo ' NM-PGM
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CICS   TO  TRUE
                     MOVE  3004        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na execução do módulo ' NM-PGM ', '
                             'RESP  = '    MSG1-RESP1  ', '
                             'RESP2 = '    MSG1-RESP2
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     END-STRING
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM  972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a leitura do Container de Resposta
           PERFORM  973000-LER-RESPOSTA

           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       910000-CHAMAR-SERVICO           SECTION.
      *--> Acionar programa IIBP2000 para requisição do serviço.
      *-----------------------------------------------------------------

      *--> Grava o container com dados do serviço/operação a chamar
           PERFORM  981000-GRAVAR-CONTAINER-SRVC

      *--> Grava o container de requisicao do modulo a executar
           PERFORM  982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS LINK
                     PROGRAM   ( IIBP2000     )
                     CHANNEL   ( NM-CNL       )
                     RESP      ( MSG1-RESP1   )
                     RESP2     ( MSG1-RESP2   )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1             EQUAL DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CICS    TO  TRUE
                     MOVE  9000        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro ao acionar serviço: '
                             SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
                             SRVC-ID-OPR  '.' SRVC-VRS-OPR
                             'via IIBP2000, ' 'RESP=' MSG1-RESP1
                                           ', RESP-2=' MSG1-RESP2 '.'
                             DELIMITED BY SIZE
                             INTO  MSG1-TX-ERRO-TS
                     END-STRING


                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM  972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a leitura do Container de Resposta
           PERFORM  973000-LER-RESPOSTA

           .
       910000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM

               WHEN  SGTSL500
                     MOVE  'L500-001-CD-OPR-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
      *------------> Verifica se o conteudo é numerico antes de adicionar
                     IF  SGTSL500-CD-OPR-ULT
                                           NUMERIC
                         MOVE  SGTSL500-CD-OPR-ULT
                                       TO  CD-OPR-ETRD
                         MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'L500-002-NR-VRS-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL500-NR-VRS-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'L500-003-CD-TIT-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL500-CD-TIT-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'L500-004-CD-EMT-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL500-CD-EMT-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'L500-005-CD-AGT-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL500-CD-AGT-FDAR-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  MCISI074
                     MOVE  'MCISI074-CD-CHV-USU'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  MCISI074-CD-CHV-USU
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
      *
      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao
           MOVE  CTE-PROG(5:8)         TO  NM-PGM
           MOVE  'SGTS0141-RQSC'       TO  NM-CTNR

      **--> Recupera o tamanho do container de requisição
           PERFORM  973100-LER-CONTAINER-NODATA

      *--> Obtem o tamanho esperado da requisicao
           MOVE  LENGTH OF SGTS0141-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisicao X tamanho recebido
           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA
                                       TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0141-RQSC)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *    A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

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
               WHEN  MSG1-RESP1             EQUAL  DFHRESP(CONTAINERERR)
                     CONTINUE
               WHEN  MSG1-RESP1             EQUAL  DFHRESP(NORMAL)
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
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *--> Recupera o container com os dados de resposta (saída).
      *    Modulo CONTAINER  -     NOME_PGM-RPST
      *    Modulo IIB (FIXO) -     PRM-DADOS-RPST
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           EVALUATE  NM-PGM
               WHEN  MCISI074
                     MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN  OTHER
                     MOVE  NM-PGM      TO  NM-CTNR(1:8)
                     MOVE  '-RPST'     TO  NM-CTNR(9:5)
           END-EVALUATE

      *--> Recupera o tamanho do container de resposta
           PERFORM  973100-LER-CONTAINER-NODATA

           IF  NM-PGM                      EQUAL  ( MCISI074 )
               PERFORM  973200-TAMANHO-ESPERADO
           END-IF

      *--> Configura dados para leitura do container de resposta
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE


      *--> Le a RESPOSTA de acordo com o programa que a gravou
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

           EVALUATE  NM-PGM
               WHEN  SGTSL500
                     MOVE  LENGTH OF SGTSL500-RPST
                                       TO  QT-CCT-EPRO
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR       )
                                INTO      ( SGTSL500-RPST )
                                FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC

               WHEN  MCISI074
                     MOVE  LENGTH OF MCISI074-DADOS
                                       TO  QT-CCT-EPRO

                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR        )
                                INTO      ( MCISI074-DADOS )
                                FLENGTH   ( QT-CCT-LIDO    )
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
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE

           .
       973000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Obtem a quantidade de caracteres no container
           MOVE  ZEROS                 TO  QT-CCT-LIDO

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR    )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973200-TAMANHO-ESPERADO         SECTION.
      *-----------------------------------------------------------------
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM

               WHEN  MCISI074
                     MOVE  LENGTH OF MCISI074-DADOS
                                       TO  QT-CCT-EPRO
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9003        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na leitura da resposta do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     END-STRING
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-PROGRAMADO
                                       TO  TRUE
               MOVE  9001              TO  MSG1-CD-ERRO-TS
               MOVE  QT-CCT-EPRO       TO  VL-EXBR-1
               MOVE  QT-CCT-LIDO       TO  VL-EXBR-2
               STRING  'Erro no tamanho de área de requisição '
                       ', Tamanho Esperado: '  VL-EXBR-1
                       ', Tamanho Recebido: '  VL-EXBR-2
                       DELIMITED BY SIZE
                       INTO  MSG1-TX-ERRO-TS
               END-STRING
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       973200-SAI.
           EXIT.

      *-----------------------------------------------------------------
       974000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE  0017                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-INF-OPRD'        TO  NM-CTNR

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           INITIALIZE                      PRM-INF-OPRD

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                )
                      INTO      ( PRM-INF-OPRD           )
                      FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       974000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *   Section da Faixa de 980000 a 989900 para Gravacao de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------

      *--> Exclui eventual container de erro de outros programas
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO

           MOVE  0018                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de resposta
           MOVE  'SGTS0141-RPST'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                 )
                      FROM      ( SGTS0141-RPST           )
                      FLENGTH   ( LENGTH OF SGTS0141-RPST )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       980000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       981000-GRAVAR-CONTAINER-SRVC    SECTION.
      *--> Postar container de requisição de serviço.
      *-----------------------------------------------------------------
           MOVE  0019                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao do servico
           MOVE  'PRM-RQSC-SRVC'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

           EVALUATE NM-PGM
               WHEN MCISI074
                    MOVE  000019485    TO  SRVC-ID-SRVC
                    MOVE  0001         TO  SRVC-VRS-SRVC
                    MOVE  001574513    TO  SRVC-ID-OPR
                    MOVE  0001         TO  SRVC-VRS-OPR

           END-EVALUATE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                      )
                     FROM      ( SRVC-PRM-RQSC-SRVC           )
                     FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       981000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *--> Postar container com dados de requisição (entrada)
      *    Modulo CONTAINER  -     NOME_PGM-RQSC
      *    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
      *-----------------------------------------------------------------
           MOVE  0020                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_modulo -RQSC
           EVALUATE  NM-PGM
               WHEN  MCISI074
                     MOVE  'PRM-DADOS-RQSC'
                                       TO  NM-CTNR
               WHEN  OTHER
                     MOVE  NM-PGM       TO  NM-CTNR(1:8)
                     MOVE  '-RQSC'      TO  NM-CTNR(9:5)
           END-EVALUATE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Faz put da requisicao do programa a executar
           EVALUATE  NM-PGM
               WHEN  MCISI074
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                  )
                                FROM      ( MCISI074-DADOS           )
                                FLENGTH   ( LENGTH OF MCISI074-DADOS )
                     END-EXEC
               WHEN  SGTSL500
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTSL500-RQSC           )
                               FLENGTH   ( LENGTH OF SGTSL500-RQSC )
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9004        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na gravação da requisição do módulo '
                              NM-PGM '.'
                              DELIMITED BY SIZE
                              INTO          MSG1-TX-ERRO-TS
                     END-STRING
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
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
           EVALUATE  NM-PGM
               WHEN  MCISI074
                     MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN  OTHER
                     MOVE  NM-PGM      TO  NM-CTNR(1:8)
                     MOVE  '-RPST'     TO  NM-CTNR(9:5)
           END-EVALUATE

           SET   DEL-CONTAINER         TO  TRUE

      *--> Excluir o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR    )
                      RESP      ( MSG1-RESP1 )
                      RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
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
           INITIALIZE                      IIBKERRO-PRM-ERRO

      *--> Excluir o container de erro
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           SET   DEL-CONTAINER         TO  TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR    )
                      RESP      ( MSG1-RESP1 )
                      RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
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
      *    As sections abaixo iniciadas com 999000 representam os
      *    tratamentos de erro utilizando o Sistema MSG(rotina MSGS0001)
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
                     PERFORM  999004-TRATA-ERRO-CTNR
               WHEN  ERRO-TAMANHO-AREA
                     PERFORM  999005-TRATA-ERRO-AREA-DADOS
               WHEN  ERRO-CANAL
                     PERFORM  999006-TRATA-ERRO-CANAL
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE  '0141-001-CD-OPR-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0141-CD-OPR-ULT         NUMERIC
               MOVE  SGTS0141-CD-OPR-ULT
                                       TO  CD-OPR-ETRD
               MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  '0141-002-NR-VRS-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0141-NR-VRS-ULT   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  '0141-003-CD-TIT-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0141-CD-TIT-PSQ   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  '0141-004-CD-EMT-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0141-CD-EMT-PSQ   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  '0141-005-CD-AGT-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0141-CD-AGT-FDAR-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
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

      *-------------------------- FIM SGTS0141 -------------------------
