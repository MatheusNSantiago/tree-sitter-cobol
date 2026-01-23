      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0615
      * ANALISTA..: F9188847 - Soila K. L. Akamatsu
      * AUTOR.....: F9188847 - Soila K. L. Akamatsu
      * DATA......: 23/04/2025
      * OBJETIVO..: Indeferir Cadastro Evento Excluso B3
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 23/04/2025 F9188847 Historia 1852713 - Eventos Exclusos
      *                B3 -  (Amortizações, juros e etc) - Implantacao
      *-----------------------------------------------------------------

       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0615.
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
                    '*** SGTS0615 ***'.
       77  CTE-VERS                        PIC  X(006)  VALUE 'VRS001'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTST139                   PIC  X(08) VALUE 'SGTST139'.
           03  SGTST140                   PIC  X(08) VALUE 'SGTST140'.

      *--> Definição dos Variaveis do Cobolito
       COPY ALMKCBL1.
      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------
      * Book do container de requisição deste módulo
       COPY SGTK0615.

      *--> CRUD EST_EVT_TIT_EXCD
       01  SGTST139-DADOS.
       COPY SGTKT139.

      *--> CRUD PRCT_CRNG_TIT_EXCD
       01  SGTST140-DADOS.
       COPY SGTKT140.

      * Book das variáveis auxiliares do tratamento de erro via MSG
       COPY SGTKERR1.
      * Book para captura de abend cics da bloco codigo MSGKP999
       COPY HLPKW999.

      * Book para captura de erro via MSG1
       COPY MSGKWMG1.


      * Book do Container de Erro
       COPY IIBKERRO.

      * Book dos dados do usuário
       COPY IIBKOPRD.

       77  CTE-FINAL-SS                  PIC X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
       COPY ALMKCBL2.

      *--> Habilita captura automatica de abend e erros CICS
       COPY HLPKE999.

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
      *-INC HLPKE997
      *--> Habilita captura automatica de abend e erros CICS
       COPY HLPKE999.

      *     PERFORM 970000-LER-CANAL

           MOVE 'SGTS0615'             TO  NM-PGM

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

            IF  SGTS0615-CD-ITMT-FNCO EQUAL SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
                MOVE  1160             TO  MSG1-CD-ERRO-TS
                MOVE 'Código do Título do Evento não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0615-CD-GR-EVT-FDAR < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1161              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do Tipo de Evento inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0615-CD-GR-EVT-FDAR EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1186              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do Tipo de Evento não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0615-NR-SEQL-EVT-EXCD EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1179              TO  MSG1-CD-ERRO-TS
               MOVE 'Numero do Evento SGT  não informado.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0615-NR-SEQL-EVT-EXCD < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1180              TO  MSG1-CD-ERRO-TS
               MOVE 'Numero do Evento SGT  inválido.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
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

      *--> Consistência do Operador Logado
           IF  SGTST139-CD-USU-ULT-ATL    =  KOPRD-CD-OPRD
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 1185               TO MSG1-CD-ERRO-TS
               MOVE 'Operador logado deve ser diferente do operador que
      -             'efetuou a edição do evento.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Consistência da permissão para confirmação da Finalização do
      *--> Cadastro
           IF (SGTST139-CD-EST-EVT-EXCD EQUAL 2 AND
              SGTST140-CD-EST-PRCT-CRNG  EQUAL 1)
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

           PERFORM 230000-ATUALIZA-ESTADO

           PERFORM 300000-MOVIMENTAR-SAIDA

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
           MOVE SGTS0615-CD-ITMT-FNCO
                                            TO SGTST139-CD-ITMT-FNCO
           MOVE SGTS0615-CD-GR-EVT-FDAR
                                            TO SGTST139-CD-GR-EVT-FDAR
           MOVE SGTS0615-NR-SEQL-EVT-EXCD
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
           MOVE SGTS0615-CD-ITMT-FNCO
                                            TO SGTST140-CD-ITMT-FNCO
           MOVE SGTS0615-CD-GR-EVT-FDAR
                                            TO SGTST140-CD-GR-EVT-FDAR
           MOVE SGTS0615-NR-SEQL-EVT-EXCD
                                            TO SGTST140-NR-SEQL-EVT-EXCD
           PERFORM 920000-CHAMA-SGTST140

           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-ATUALIZA-ESTADO   SECTION.
      * Inclui EST_EVT_TIT_EXCD
      *-----------------------------------------------------------------
           MOVE 0006                   TO  MSG1-CD-SEQL-ERRO

      *--> Inclui novo registro de estado do cronograma.
      *--> 4 - Indeferido

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTST139-DADOS.

           MOVE SGTST139              TO NM-PGM

      *--> Movimentar os campos de entrada

           SET SGTST139-INCLUIR        TO TRUE

           MOVE SGTS0615-CD-ITMT-FNCO  TO SGTST139-CD-ITMT-FNCO

           MOVE SGTS0615-CD-GR-EVT-FDAR
                                       TO SGTST139-CD-GR-EVT-FDAR

           MOVE SGTS0615-NR-SEQL-EVT-EXCD
                                       TO SGTST139-NR-SEQL-EVT-EXCD
           MOVE 'CURRENT'              TO SGTST139-TS-EST-EVT-EXCD

           MOVE KOPRD-CD-OPRD          TO SGTST139-CD-USU-ULT-ATL

      *--> Tipo de estado evento excluído B3 = 4 - Indeferido
           MOVE 4                      TO SGTST139-CD-EST-EVT-EXCD

           MOVE SPACES                 TO SGTST139-TX-JST-EVT-EXCD


           PERFORM 910000-CHAMA-SGTST139
           .
       230000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-MOVIMENTAR-SAIDA         SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'S'
                                       TO SGTS0615-IN-SCS

      *--> Gravando o container de resposta da subrotina

           PERFORM 980000-GRAVAR-CONTAINER-RPST
           .
       300000-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *----------------------------------------------------------------*
      * Executa um modulo implementado com CONTAINER                   *
      * Nao incluir nessa section os modulos chamados via COMMAREA     *
      *

      *-----------------------------------------------------------------
       910000-CHAMA-SGTST139         SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO  MSG1-CD-SEQL-ERRO

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
               PERFORM 999000-ERRO
           END-IF
           .
       910000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       920000-CHAMA-SGTST140         SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO  MSG1-CD-SEQL-ERRO

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
               PERFORM 999000-ERRO
           END-IF
           .
       920000-SAI.
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

                  IF SGTST139-INCLUIR

                    MOVE 'SGTST139-TS-EST-EVT-EXCD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST139-TS-EST-EVT-EXCD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST139-CD-USU-ULT-ATL'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST139-CD-USU-ULT-ATL
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST139-CD-EST-EVT-EXCD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST139-CD-EST-EVT-EXCD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST139-TX-JST-EVT-EXCD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST139-TX-JST-EVT-EXCD
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

               WHEN OTHER
                    CONTINUE
           END-EVALUATE
      
           .
       969000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
      *                                                                *
      * SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
      *                                                                *
      
      *----------------------------------------------------------------*
       971000-LER-REQUISICAO           SECTION.
      *----------------------------------------------------------------*
      
           MOVE 0010                   TO  MSG1-CD-SEQL-ERRO
      
      *    Monta o nome do container de requisicao
           MOVE 'SGTS0615'             TO NM-PGM
           MOVE 'SGTS0615-RQSC'        TO NM-CTNR
      
      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA
      
      *    Obtem o tamanho esperado da requisicao
           MOVE LENGTH OF SGTS0615-RQSC TO QT-CCT-EPRO
      
      *    Valida tamanho esperado da requisicao X tamanho recebido
      
           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA   TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
      
      
      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
      
      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE
      
      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0615-RQSC)
           END-EXEC
      
      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
      
           .
       971000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       973100-LER-CONTAINER-NODATA     SECTION.
      *----------------------------------------------------------------*
      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
      
      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE
      
      *    Obtem a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
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
           MOVE 0011                   TO  MSG1-CD-SEQL-ERRO
      
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
      *----------------------------------------------------------------*
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
      *                                                                *
      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------

      *    Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

           MOVE 0012                   TO  MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de resposta
           MOVE 'SGTS0615-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                    )
                     FROM      (SGTS0615-RPST          )
                     FLENGTH   (LENGTH OF SGTS0615-RPST)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
      
           MOVE 0013                   TO  MSG1-CD-SEQL-ERRO
      
      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

                    MOVE NM-PGM        TO NM-CTNR(1:8)
                    MOVE '-RPST'       TO NM-CTNR(9:5)

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
      *
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
           .
       983100-SAI.
           EXIT.
      *----------------------------------------------------------------*
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *----------------------------------------------------------------*
      *    Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS PUT
                     CONTAINER ( 'PRM-ERRO'                  )
                     FROM      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      (MSG1-RESP1                   )
                     RESP2     (MSG1-RESP2                   )
           END-EXEC
      
           .
       984000-SAI.
           EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      * As sections abaixo iniciadas com 999000 representam os tratamen*
      * tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *
      *                                                                *
      *----------------------------------------------------------------*
       999000-ERRO                     SECTION.
      *----------------------------------------------------------------*
      
           IF MSG1-CD-ERRO-TS EQUAL ZEROS
              MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF
           IF MSG1-TX-ERRO-TS EQUAL SPACES
              MOVE 'ERRO NAO ESPECIFICADO NO MODULO' TO MSG1-TX-ERRO-TS
           END-IF
           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-ERRO-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
      *     As sections abaixo representam os tratamentos de erro
      *    utilizando o sistema MSG de mensageria (rotina MSGS0001)
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------
      
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

               MOVE 'SGTS0615-CD-ITMT-FNCO'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0615-CD-ITMT-FNCO
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0615-CD-GR-EVT-FDAR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0615-CD-GR-EVT-FDAR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0615-NR-SEQL-EVT-EXCD'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0615-NR-SEQL-EVT-EXCD
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT
      
      *    Grava o erro gerado no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG
      
           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
      
      *    Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO
      
      *    Encerra a transacao
           PERFORM 999999-ENCERRAR
           .
       999000-SAI.
           EXIT.
      * --------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * --------------------------------------*
       COPY HLPKP999.
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
       COPY SGTKP999.
      * --------------------------------------*
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
      * --------------------------------------*
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
      *---------------------- FIM SGTS0615 -----------------------------
