      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0613
      * ANALISTA..: F9188847 - Soila K. L. Akamatsu
      * AUTOR.....: F9188847 - Soila K. L. Akamatsu
      * DATA......: 17/04/2025
      * OBJETIVO..: Excluir Cadastro Cronograma de Evento fora da B3.
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 17/04/2025 F9188847 Historia 1852713 - Eventos Exclusos
      *                B3 -  (Amortizações, juros e etc) - Implantacao

      *-----------------------------------------------------------------

       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0613.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------

       77  CTE-INICIO-SS                   PIC  X(040)  VALUE
                    '*** S.S. COMECA AQUI ***'.

       77  CTE-PROG                        PIC  X(018)  VALUE
                    '*** SGTS0613 ***'.
       77  CTE-VERS                        PIC  X(006)  VALUE 'VRS001'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTST138                   PIC  X(08) VALUE 'SGTST138'.
           03  SGTST139                   PIC  X(08) VALUE 'SGTST139'.
           03  SGTST140                   PIC  X(08) VALUE 'SGTST140'.
           03  SGTSL139                   PIC  X(08) VALUE 'SGTSL139'.
           03  SGTSL140                   PIC  X(08) VALUE 'SGTSL140'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1
      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Areas de trabalho

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------
      * Book do container de requisição deste módulo
-INC SGTK0613

      *--> Consultar TABELA CRNG_EVT_TIT_EXCD
       01  SGTST138-DADOS.
-INC SGTKT138

      *--> CRUD EST_EVT_TIT_EXCD
       01  SGTST139-DADOS.
-INC SGTKT139

      *--> CRUD PRCT_CRNG_TIT_EXCD
       01  SGTST140-DADOS.
-INC SGTKT140

      *--> Lista EST_EVT_TIT_EXCD
       01  SGTSL139-DADOS.
-INC SGTKL139

      *--> Lista PRCT_CRNG_TIT_EXCD
       01  SGTSL140-DADOS.
-INC SGTKL140

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      * Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999

      * Book para captura de erro via MSG1
-INC MSGKWMG1


      * Book do Container de Erro
-INC IIBKERRO

      * Book dos dados do usuário
-INC IIBKOPRD

       77  CTE-FINAL-SS                  PIC X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
-INC ALMKCBL2

      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999

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

           MOVE 0001                   TO  MSG1-CD-SEQL-ERRO

      *--> Habilita tratamento automatico de Erros DB2
      * -INC HLPKE997
      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999

           PERFORM 970000-LER-CANAL

           MOVE 'SGTS0613'             TO  NM-PGM

           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 974000-LER-DADOS-OPRD

           PERFORM 971000-LER-REQUISICAO

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO  MSG1-CD-SEQL-ERRO

            IF  SGTS0613-CD-ITMT-FNCO EQUAL SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
                MOVE  1160             TO  MSG1-CD-ERRO-TS
                MOVE 'Código do Título do Evento não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0613-CD-GR-EVT-FDAR < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1161              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do Tipo de Evento inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0613-CD-GR-EVT-FDAR EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1186              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do Tipo de Evento não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0613-NR-SEQL-EVT-EXCD EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1179              TO  MSG1-CD-ERRO-TS
               MOVE 'Numero do Evento SGT  não informado.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0613-NR-SEQL-EVT-EXCD < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1180              TO  MSG1-CD-ERRO-TS
               MOVE 'Numero do Evento SGT  inválido.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO  MSG1-CD-SEQL-ERRO

      *--> Consulta estado do cronograma de eventos
           PERFORM 210000-VER-ULT-ESTADO-EVT

      *--> Consulta estado do envio do cronograma de eventos
           PERFORM 220000-VER-ULT-ESTADO-PRCT

      *--> Consistência para Excluir Evento (exclusao fisica)
      *-->  1- Em Cadastramento/ 1 - Não Enviado para processamento
      *-->  2- Finalizado/ 1 - Não Enviado para processamento
      *-->  4 = Indeferido/ 1 - Não Enviado para processamento
           IF ((SGTST139-CD-EST-EVT-EXCD EQUAL 1 AND
               SGTST140-CD-EST-PRCT-CRNG  EQUAL 1)
           OR (SGTST139-CD-EST-EVT-EXCD EQUAL 2 AND
               SGTST140-CD-EST-PRCT-CRNG  EQUAL 1)
           OR (SGTST139-CD-EST-EVT-EXCD EQUAL 4 AND
               SGTST140-CD-EST-PRCT-CRNG  EQUAL 1))
           CONTINUE
           ELSE
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 1183               TO MSG1-CD-ERRO-TS
               MOVE 'Alteração não permitida para o estado atual do even
      -               'to.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           PERFORM 230000-TRATAR-EXCLUSAO

           .
       200000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       210000-VER-ULT-ESTADO-EVT    SECTION.
      * Verifica o estado mais atual do Evento Excluso B3
      *-----------------------------------------------------------------
           MOVE 0004                   TO  MSG1-CD-SEQL-ERRO

           INITIALIZE         SGTST139-DADOS

      *--> Movimentar os campos de entrada

           SET SGTST139-CONSULTAR-EST-ATIVO TO TRUE
           MOVE SGTS0613-CD-ITMT-FNCO
                                            TO SGTST139-CD-ITMT-FNCO
           MOVE SGTS0613-CD-GR-EVT-FDAR
                                            TO SGTST139-CD-GR-EVT-FDAR
           MOVE SGTS0613-NR-SEQL-EVT-EXCD
                                            TO SGTST139-NR-SEQL-EVT-EXCD
           PERFORM 910000-CHAMA-SGTST139

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-VER-ULT-ESTADO-PRCT    SECTION.
      * * Verifica o estado mais atual do Procesamento do Cronograma
      *-----------------------------------------------------------------
           MOVE 0005                   TO  MSG1-CD-SEQL-ERRO

           INITIALIZE         SGTST140-DADOS

      *--> Movimentar os campos de entrada

           SET SGTST140-CONSULTAR-EST-ATIVO TO TRUE
           MOVE SGTS0613-CD-ITMT-FNCO
                                            TO SGTST140-CD-ITMT-FNCO
           MOVE SGTS0613-CD-GR-EVT-FDAR
                                            TO SGTST140-CD-GR-EVT-FDAR
           MOVE SGTS0613-NR-SEQL-EVT-EXCD
                                            TO SGTST140-NR-SEQL-EVT-EXCD
           PERFORM 920000-CHAMA-SGTST140

           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-TRATAR-EXCLUSAO   SECTION.
      * Exclusao dos EST_EVT_TIT_EXCD,PRCT_CRNG_TIT_EXCD e
      * CRNG_EVT_TIT_EXCD
      *-----------------------------------------------------------------
           MOVE 0006                   TO  MSG1-CD-SEQL-ERRO

           PERFORM 231000-CONSULTA-CRNG-EVT

           PERFORM 232000-TRATA-ESTADO-EVT

           PERFORM 233000-TRATA-ESTADO-CRNG

           PERFORM 234000-EXCLUI-CRNG-EVT

           PERFORM 300000-MOVIMENTAR-SAIDA


           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       231000-CONSULTA-CRNG-EVT      SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO  MSG1-CD-SEQL-ERRO

           INITIALIZE         SGTST138-DADOS
           MOVE SGTST138               TO NM-PGM

      *--> Movimentar os campos de entrada

           SET  SGTST138-CONSULTAR        TO TRUE
           MOVE SGTS0613-CD-ITMT-FNCO     TO SGTST138-CD-ITMT-FNCO
           MOVE SGTS0613-CD-GR-EVT-FDAR   TO SGTST138-CD-GR-EVT-FDAR
           MOVE SGTS0613-NR-SEQL-EVT-EXCD TO SGTST138-NR-SEQL-EVT-EXCD

           PERFORM 930000-CHAMA-SGTST138
           .
       231000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       232000-TRATA-ESTADO-EVT        SECTION.
      * Lista  EST_EVT_TIT_EXCD
      *----------------------------------------------------------------*

           MOVE 0008                   TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL139-RQSC
                                       SGTSL139-QT-OCR-TAB
                                       SGTSL139-IC-TAB
                                       SGTSL139-IN-CNTO-PSQ

      *    Movimentar os campos de entrada da lista
           SET SGTSL139-LISTA-POR-EVT TO TRUE
           MOVE SGTS0613-CD-ITMT-FNCO
                                       TO SGTSL139-CD-ITMT-FNCO
           MOVE SGTS0613-CD-GR-EVT-FDAR
                                       TO SGTSL139-CD-GR-EVT-FDAR
           MOVE SGTS0613-NR-SEQL-EVT-EXCD
                                       TO SGTSL139-NR-SEQL-EVT-EXCD
           MOVE ' '
                                       TO SGTSL139-TS-EST-EVT-EXCD-ULT


           PERFORM UNTIL SGTSL139-IN-CNTO-PSQ  EQUAL  'N'
              MOVE SGTSL139            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 232100-EXCLUIR-ESTADO-EVT
                        VARYING SGTSL139-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL139-IC-TAB > SGTSL139-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL139-QT-OCR-TAB  > ZEROS

                  MOVE SGTSL139-TS-EST-EVT-EXCD (SGTSL139-QT-OCR-TAB)
                                       TO SGTSL139-TS-EST-EVT-EXCD-ULT
              END-IF
           END-PERFORM

           .
       232000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       232100-EXCLUIR-ESTADO-EVT   SECTION.
      * Exclui EST_EVT_TIT_EXCD
      *-----------------------------------------------------------------
           MOVE 0009                   TO  MSG1-CD-SEQL-ERRO

      *--> Exclui o estado do cronograma de eventos.

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTST139-DADOS.

           MOVE SGTST139              TO NM-PGM

      *--> Movimentar os campos de entrada

           SET SGTST139-EXCLUIR        TO TRUE

           MOVE SGTSL139-CD-ITMT-FNCO  TO SGTST139-CD-ITMT-FNCO

           MOVE SGTSL139-CD-GR-EVT-FDAR
                                       TO SGTST139-CD-GR-EVT-FDAR

           MOVE SGTSL139-NR-SEQL-EVT-EXCD
                                       TO SGTST139-NR-SEQL-EVT-EXCD
           MOVE SGTSL139-TS-EST-EVT-EXCD (SGTSL139-IC-TAB)
                                       TO SGTST139-TS-EST-EVT-EXCD

           MOVE KOPRD-CD-OPRD          TO SGTST139-CD-USU-ULT-ATL

           PERFORM 910000-CHAMA-SGTST139
           .
       232100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       233000-TRATA-ESTADO-CRNG  SECTION.
      * Lista PRCT_CRNG_TIT_EXCD
      *-----------------------------------------------------------------
           MOVE 0010                   TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL140-RQSC
                                       SGTSL140-QT-OCR-TAB
                                       SGTSL140-IC-TAB
                                       SGTSL140-IN-CNTO-PSQ

      *    Movimentar os campos de entrada da lista
           SET SGTSL140-LISTA-POR-PRCT TO TRUE
           MOVE SGTS0613-CD-ITMT-FNCO
                                       TO SGTSL140-CD-ITMT-FNCO
           MOVE SGTS0613-CD-GR-EVT-FDAR
                                       TO SGTSL140-CD-GR-EVT-FDAR
           MOVE SGTS0613-NR-SEQL-EVT-EXCD
                                       TO SGTSL140-NR-SEQL-EVT-EXCD
           MOVE ' '
                                       TO SGTSL140-TS-EST-ENVO-CRNG-ULT


           PERFORM UNTIL SGTSL140-IN-CNTO-PSQ  EQUAL  'N'
              MOVE SGTSL140            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 233100-EXCLUIR-PRCT-CRNG
                        VARYING SGTSL140-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL140-IC-TAB > SGTSL140-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL140-QT-OCR-TAB  > ZEROS

                  MOVE SGTSL140-TS-EST-ENVO-CRNG(SGTSL140-QT-OCR-TAB)
                                       TO SGTSL140-TS-EST-ENVO-CRNG-ULT
              END-IF
           END-PERFORM


           .
       233000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       233100-EXCLUIR-PRCT-CRNG  SECTION.
      * Exclui PRCT_CRNG_TIT_EXCD
      *-----------------------------------------------------------------
           MOVE 0011                   TO  MSG1-CD-SEQL-ERRO

      *--> Exclui o processamento de cronograma de eventos.

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTST140-DADOS.

           MOVE SGTST140              TO NM-PGM

      *--> Movimentar os campos de entrada

           SET SGTST140-EXCLUIR        TO TRUE

           MOVE SGTSL140-CD-ITMT-FNCO  TO SGTST140-CD-ITMT-FNCO

           MOVE SGTSL140-CD-GR-EVT-FDAR
                                       TO SGTST140-CD-GR-EVT-FDAR

           MOVE SGTSL140-NR-SEQL-EVT-EXCD
                                       TO SGTST140-NR-SEQL-EVT-EXCD
           MOVE SGTSL140-TS-EST-ENVO-CRNG (SGTSL140-IC-TAB)
                                       TO SGTST140-TS-EST-ENVO-CRNG

           MOVE KOPRD-CD-OPRD          TO SGTST140-CD-USU-RSP-ULT-ATL

           PERFORM 920000-CHAMA-SGTST140
           .
       233100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       234000-EXCLUI-CRNG-EVT   SECTION.
      * Exclui CRNG_EVT_TIT_EXCD
      *-----------------------------------------------------------------
           MOVE 0012                   TO  MSG1-CD-SEQL-ERRO

      *--> Exclui o cronograma de eventos.

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTST138-DADOS.

           MOVE SGTST138             TO NM-PGM

      *--> Movimentar os campos de entrada

           SET SGTST138-EXCLUIR        TO TRUE

           MOVE SGTS0613-CD-ITMT-FNCO  TO SGTST138-CD-ITMT-FNCO

           MOVE SGTS0613-CD-GR-EVT-FDAR
                                       TO SGTST138-CD-GR-EVT-FDAR

           MOVE SGTS0613-NR-SEQL-EVT-EXCD
                                       TO SGTST138-NR-SEQL-EVT-EXCD

           MOVE KOPRD-CD-OPRD          TO SGTST138-CD-USU-RSP-ATL

           PERFORM 930000-CHAMA-SGTST138
           .
       234000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-MOVIMENTAR-SAIDA         SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'S'
                                       TO SGTS0613-IN-SCS

      *--> Gravando o container de resposta da subrotina

           PERFORM 980000-GRAVAR-CONTAINER-RPST
           .
       300000-SAI.
           EXIT.

      *----------------------------------------------------------------*
002730*                                                                *
002740*  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
002750*                                                                *
002760*----------------------------------------------------------------*
002770* Executa um modulo implementado com CONTAINER                   *
002780* Nao incluir nessa section os modulos chamados via COMMAREA     *
002790*
      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *-->  Executa um modulo implementado com CONTAINER
      *     Nao incluir nessa section os modulos chamados via COMMAREA
      *-----------------------------------------------------------------
           MOVE 0014                   TO  MSG1-CD-SEQL-ERRO

      *--> Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           INITIALIZE MSG1-RESP1 MSG1-RESP2 EIBRESP EIBRESP2

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM
               WHEN SGTSL139
                    EXEC CICS LINK
                              PROGRAM ( SGTSL139  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC
               WHEN SGTSL140
                    EXEC CICS LINK
                              PROGRAM ( SGTSL140  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9006         TO  MSG1-CD-ERRO-TS
                    STRING 'Não definida a execução do módulo '
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                   END-STRING
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           EVALUATE TRUE
               WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET   ERRO-CICS    TO  TRUE
                    MOVE  3004         TO  MSG1-CD-ERRO-TS
                    STRING 'Erro ao acionar o módulo ' NM-PGM ', '
                           'RESP  ='   ' ' MSG1-RESP1  ', '
                           'RESP2 =' ' ' MSG1-RESP2
                           DELIMITED BY SIZE
                           INTO            MSG1-TX-ERRO-TS
                    END-STRING
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       910000-CHAMA-SGTST139         SECTION.
      *-----------------------------------------------------------------
           MOVE 0015                   TO  MSG1-CD-SEQL-ERRO

      *--> Movimenta nome do módulo para monitoração de variáveis
           MOVE SGTST139               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST139)
                COMMAREA (SGTST139-DADOS)
           END-EXEC

           IF  SGTST139-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE NM-PGM                TO MSG1-NM-PGM-ACND
               MOVE SGTST139-MSG-ERRO-PGM TO MSG1-TX-RTN-PGM-ACND
                                             MSG1-TX-ERRO-TS
               MOVE SGTST139-CD-RTN-PGM   TO MSG1-CD-RTN-PGM-ACND
                                             MSG1-CD-ERRO-TS
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       910000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       920000-CHAMA-SGTST140         SECTION.
      *-----------------------------------------------------------------
           MOVE 0016                   TO  MSG1-CD-SEQL-ERRO

      *--> Movimenta nome do módulo para monitoração de variáveis
           MOVE SGTST140               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST140)
                COMMAREA (SGTST140-DADOS)
           END-EXEC

           IF  SGTST140-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE NM-PGM                TO MSG1-NM-PGM-ACND
               MOVE SGTST140-MSG-ERRO-PGM TO MSG1-TX-RTN-PGM-ACND
                                             MSG1-TX-ERRO-TS
               MOVE SGTST140-CD-RTN-PGM   TO MSG1-CD-RTN-PGM-ACND
                                             MSG1-CD-ERRO-TS
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       920000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       930000-CHAMA-SGTST138           SECTION.
      *-----------------------------------------------------------------
           MOVE 0017                   TO  MSG1-CD-SEQL-ERRO

      *--> Movimenta nome do módulo para monitoração de variáveis
           MOVE SGTST138               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST138)
                COMMAREA (SGTST138-DADOS)
           END-EXEC

           IF  SGTST138-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO  TO TRUE
               MOVE NM-PGM                 TO MSG1-NM-PGM-ACND
               MOVE SGTST138-MSG-ERRO-PGM  TO MSG1-TX-RTN-PGM-ACND
                                              MSG1-TX-ERRO-TS
               MOVE SGTST138-CD-RTN-PGM    TO MSG1-CD-RTN-PGM-ACND
                                              MSG1-CD-ERRO-TS
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       930000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*

            EVALUATE NM-PGM
               WHEN SGTST139
                    MOVE 'SGTST139-ACAO'       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST139-ACAO         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST139-CD-ITMT-FNCO'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST139-CD-ITMT-FNCO
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST139-CD-GR-EVT-FDAR'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST139-CD-GR-EVT-FDAR
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST139-NR-SEQL-EVT-EXCD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST139-NR-SEQL-EVT-EXCD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                  IF SGTST139-EXCLUIR

                    MOVE 'SGTST139-TS-EST-EVT-EXCD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST139-TS-EST-EVT-EXCD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                  END-IF

               WHEN SGTST140
                 MOVE 'SGTST140-ACAO'       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST140-ACAO         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST140-CD-ITMT-FNCO'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST140-CD-ITMT-FNCO
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST140-CD-GR-EVT-FDAR'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST140-CD-GR-EVT-FDAR
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST140-NR-SEQL-EVT-EXCD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST140-NR-SEQL-EVT-EXCD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
                    IF SGTST139-EXCLUIR

                       MOVE 'SGTST140-TS-EST-ENVO-CRNG'
                                               TO MSGKW999-NM-VRV-MNT
                       MOVE SGTST140-TS-EST-ENVO-CRNG
                                               TO MSGKW999-VL-VRV-MNT
                       PERFORM 999003-ADICIONA-VRV-MNT

                   END-IF
      *------> Monitora as Variaveis de Entrada do
               WHEN  SGTSL139
                     MOVE 'L139-E-CD-TIP-SRVC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL139-E-CD-TIP-SRVC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L139-CD-ITMT-FNCO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL139-CD-ITMT-FNCO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L139-CD-GR-EVT-FDAR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL139-CD-GR-EVT-FDAR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L139-NR-SEQL-EVT-EXCD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL139-NR-SEQL-EVT-EXCD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L139-TS-EST-EVT-EXCD-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL139-TS-EST-EVT-EXCD-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L139-QT-OCR-TAB'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL139-QT-OCR-TAB
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L139-IC-TAB '
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL139-IC-TAB
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do
               WHEN  SGTSL140
                     MOVE 'L140-E-CD-TIP-SRVC '
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL140-E-CD-TIP-SRVC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L140-CD-ITMT-FNCO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL140-CD-ITMT-FNCO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L140-CD-GR-EVT-FDAR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL140-CD-GR-EVT-FDAR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L140-NR-SEQL-EVT-EXCD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL140-NR-SEQL-EVT-EXCD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L140-TS-EST-ENVO-CRNG-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL140-TS-EST-ENVO-CRNG-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L140-QT-OCR-TAB'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL140-QT-OCR-TAB
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L140-IC-TAB '
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL140-IC-TAB
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

004180         WHEN OTHER
004190              CONTINUE
004200     END-EVALUATE
004210
004220     .
004230 969000-SAI.
004240     EXIT.
004250
      *-----------------------------------------------------------------
      * SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       970000-LER-CANAL                SECTION.
      *--> Obtem o canal corrente
      *-----------------------------------------------------------------

           MOVE 0018                   TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       970000-FIM.
           EXIT.

004500*----------------------------------------------------------------*
004510 971000-LER-REQUISICAO           SECTION.
004520*----------------------------------------------------------------*
004530
           MOVE 0019                   TO  MSG1-CD-SEQL-ERRO
004550
004560*    Monta o nome do container de requisicao
004570     MOVE 'SGTS0613'             TO NM-PGM
004580     MOVE 'SGTS0613-RQSC'        TO NM-CTNR
004590
004600*    Recupera o tamanho do container de requisição
004610     PERFORM 973100-LER-CONTAINER-NODATA
004620
004630*    Obtem o tamanho esperado da requisicao
004640     MOVE LENGTH OF SGTS0613-RQSC TO QT-CCT-EPRO
004650
004660*    Valida tamanho esperado da requisicao X tamanho recebido
004670
004680     IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
004690         SET ERRO-TAMANHO-AREA   TO TRUE
004700         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
004710     END-IF
004720
004740
004750*Configura dados para leitura do container de requisição
004760     SET  GET-CONTAINER          TO TRUE
004770
004780*Por causa da captura automática de erro CICS, deve ser previsto
004790*o erro de container
004800     SET  ERRO-CONTAINER         TO TRUE
004810
004820*    Efetua a leitura do container
004830     EXEC CICS GET
004840               CONTAINER (NM-CTNR)
004850               FLENGTH   (QT-CCT-LIDO)
004860               INTO      (SGTS0613-RQSC)
004870     END-EXEC
004880
004890*Ao chegar aqui, há certeza de que não houve erro algum
004900     SET  SEM-ERRO               TO TRUE
004910
004920     .
004930 971000-SAI.
004940     EXIT.
      *----------------------------------------------------------------*
004970 972000-LER-CONTAINER-DE-ERRO    SECTION.
004980*----------------------------------------------------------------*
004990*    A presença do container PRM-ERRO indica que houve erro de
005000*    integração, a ausência do container indica sucesso.
005010*----------------------------------------------------------------*
005020
005050*    Le o container de erro dos modulos chamados
005060     SET GET-CONTAINER           TO TRUE
005070     MOVE 'PRM-ERRO'             TO NM-CTNR
005080     INITIALIZE IIBKERRO-PRM-ERRO
005090
005100*    Não há captura automática de erro CICS por usar RESP e RESP2
005110     EXEC CICS GET
005120               CONTAINER ( NM-CTNR                     )
005130               INTO      ( IIBKERRO-PRM-ERRO           )
005140               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
005150               RESP      ( MSG1-RESP1                  )
005160               RESP2     ( MSG1-RESP2                  )
005170     END-EXEC
005180
005190     EVALUATE TRUE
005200*        Caso deseje personalizar ou não interromper o
005210*        processamento, alterar para não dar erro aqui SOMENTE
005220*        PARA O MÓDULO CHAMADO e tratar na section chamadora
005230         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
005240              CONTINUE
005250         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                    MOVE  NM-PGM       TO  MSG1-NM-PGM-ACND

                    IF NM-PGM(1:3) EQUAL 'SGT'
                       MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                                           MSG1-CD-ERRO-TS
                       MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS

                    ELSE
                       MOVE 3004          TO MSG1-CD-ERRO-TS
                       STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS

                       MOVE IIBKERRO-TX-ERRO
                                          TO MSG1-TX-RTN-PGM-ACND
                       MOVE IIBKERRO-CD-ERRO
                                          TO MSG1-CD-RTN-PGM-ACND

                    END-IF
005350*             Monitorar variáveis do módulo chamado
005360                 PERFORM 969000-MONITORAR-VARIAVEIS
005370                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
005380         WHEN OTHER
005390              SET ERRO-CONTAINER  TO TRUE
005400              PERFORM 999002-TRATA-CONDITION-CICS
005410     END-EVALUATE
005420     .
005430 972000-SAI.
005440     EXIT.
      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *-----------------------------------------------------------------
      *    Recupera o container com os dados de resposta (saída).
      *-----------------------------------------------------------------

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

      *    Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

      *Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Lê a RESPOSTA de acordo com o programa que a gravou

           EVALUATE NM-PGM

      *    Calcula a quantidade de caracteres esperado e limpa a área de
      *    dados antes de executar o GET (somentepara matrizes com dados
      *    não variáveis)

               WHEN SGTSL139
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL139-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSL140
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL140-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN OTHER
                    MOVE 9003          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro na leitura da resposta do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE

           .
       973000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       973100-LER-CONTAINER-NODATA     SECTION.
      *----------------------------------------------------------------*
      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *    Obtem a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR    )
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973100-SAI.
           EXIT.
      *----------------------------------------------------------------*
       974000-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE 0070                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           MOVE 'PRM-INF-OPRD'         TO NM-CTNR

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           INITIALIZE PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       974000-SAI.
           EXIT.
006520*----------------------------------------------------------------*
006530*                                                                *
006540* SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
006550*                                                                *
007190*----------------------------------------------------------------*
      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------

      *    Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

           MOVE 0021                   TO  MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de resposta
           MOVE 'SGTS0613-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                    )
                     FROM      (SGTS0613-RPST          )
                     FLENGTH   (LENGTH OF SGTS0613-RPST)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.
      *----------------------------------------------------------------*
007200 982000-GRAVAR-CONTAINER-RQSC    SECTION.
007210*----------------------------------------------------------------*
007220*    Postar container com dados de requisição (entrada)
007230*
007240*                        NOME DO CONTAINER REQUISICAO
007250*                        ----------------------------
007260*
007270*    Modulo CONTAINER  -     NOME_PGM-RQSC
007280*    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
007290*
007300*----------------------------------------------------------------*
007310
           MOVE 0022                   TO  MSG1-CD-SEQL-ERRO

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE
      *    Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_modulo -RQSC

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RQSC'                TO NM-CTNR(9:5)

      * Por causa da captura automática de erro CICS, deve ser previsto
      * o erro de container
      * Por causa da captura automática de erro CICS, deve ser previsto
      * o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisicao do programa a executar

           EVALUATE NM-PGM

                WHEN SGTSL139
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSL139-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSL139-RQSC )
                    END-EXEC
                WHEN SGTSL140
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSL140-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSL140-RQSC )
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9004         TO  MSG1-CD-ERRO-TS
                    STRING 'Erro na gravação da requisição do módulo'
                            NM-PGM '.' DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       982000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           MOVE NM-PGM          TO NM-CTNR(1:8)
           MOVE '-RPST'         TO NM-CTNR(9:5)

           SET DEL-CONTAINER           TO TRUE

      *    Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER (NM-CTNR)
                     RESP      (MSG1-RESP1)
                     RESP2     (MSG1-RESP2)
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *    Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *----------------------------------------------------------------*
      *    Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE 'PRM-ERRO'             TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER (NM-CTNR   )
                     RESP      (MSG1-RESP1)
                     RESP2     (MSG1-RESP2)
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *----------------------------------------------------------------*
      *    Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS PUT
                     CONTAINER ('PRM-ERRO'                 )
                     FROM      (IIBKERRO-PRM-ERRO          )
                     FLENGTH   (LENGTH OF IIBKERRO-PRM-ERRO)
                     RESP      (MSG1-RESP1                 )
                     RESP2     (MSG1-RESP2                 )
           END-EXEC
           .
       984000-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *     As sections abaixo representam os tratamentos de erro
      *    utilizando o sistema MSG de mensageria (rotina MSGS0001)
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------
      *
           IF MSG1-CD-ERRO-TS EQUAL ZEROS
              MOVE 8888  TO MSG1-CD-ERRO-TS
           END-IF

           IF MSG1-TX-ERRO-TS EQUAL SPACES
              MOVE 'Erro não especificado no módulo.' TO MSG1-TX-ERRO-TS
           END-IF

      *    Em caso de erro, desfaz as atualizações em banco de dados

           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC

      *Verifica se o tipo de erro gerado possui tratamento interno
           EVALUATE TRUE
               WHEN ERRO-CONTAINER
                    PERFORM 999004-TRATA-ERRO-CTNR
               WHEN ERRO-TAMANHO-AREA
                    PERFORM 999005-TRATA-ERRO-AREA-DADOS
               WHEN ERRO-CANAL
                    PERFORM 999006-TRATA-ERRO-CANAL
               WHEN OTHER
                    CONTINUE
           END-EVALUATE

      *Adiciona as variáveis de entrada para monitoração

               MOVE 'SGTS0613-CD-ITMT-FNCO'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0613-CD-ITMT-FNCO
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0613-CD-GR-EVT-FDAR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0613-CD-GR-EVT-FDAR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0613-NR-SEQL-EVT-EXCD'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0613-NR-SEQL-EVT-EXCD
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT
009400
009410*    Grava o erro gerado no sistema MSG - Mensageria
009420     PERFORM MSG1-NTF-MSG
009430
009440     MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
009450     MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
009460
009470*    Grava o erro gerado no container de erro
009480     PERFORM 984000-GRAVAR-CONTAINER-ERRO
009490
009500*    Encerra a transacao
009510     PERFORM 999999-ENCERRAR
009520     .
009530 999000-SAI.
009540     EXIT.
009550* --------------------------------------*
009560* 999001-TRATA-ABEND-CICS        SECTION.
009570* 999002-TRATA-CONDITION-CICS    SECTION.
009580* 999003-ADICIONA-VRV-MNT        SECTION.
009590* --------------------------------------*
-INC HLPKP999
009610* --------------------------------------*
009620* 999004-TRATA-ERRO-CTNR         SECTION.
009630* 999005-TRATA-ERRO-AREA-DADOS   SECTION.
009640* 999006-TRATA-ERRO-CANAL        SECTION.
009650*---------------------------------------*
-INC SGTKP999
009670* --------------------------------------*
009680* MSG1-NTF-MSG                   SECTION.
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
      * --------------------------------------*
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

      *---------------------- FIM SGTS0613 -----------------------------