      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0616
      * ANALISTA..: F9188847 - Soila K. L. Akamatsu
      * AUTOR.....: F9188847 - Soila K. L. Akamatsu
      * DATA......: 23/04/2025
      * OBJETIVO..:  Aprovar Cadastro Evento Excluso B3
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 24/04/2025 F9188847 Historia 1852713 - Eventos Exclusos
      *                B3 -  (Amortizações, juros e etc) - Implantacao
      *-----------------------------------------------------------------

       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0616.
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
                    '*** SGTS0616 ***'.
       77  CTE-VERS                        PIC  X(006)  VALUE 'VRS001'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTST139                   PIC  X(08) VALUE 'SGTST139'.
           03  SGTST140                   PIC  X(08) VALUE 'SGTST140'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1
      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------
      * Book do container de requisição deste módulo
-INC SGTK0616

      *--> CRUD EST_EVT_TIT_EXCD
       01  SGTST139-DADOS.
-INC SGTKT139

      *--> CRUD PRCT_CRNG_TIT_EXCD
       01  SGTST140-DADOS.
-INC SGTKT140

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

      *     PERFORM 970000-LER-CANAL

           MOVE 'SGTS0616'             TO  NM-PGM

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

            IF  SGTS0616-CD-ITMT-FNCO EQUAL SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
                MOVE  1160             TO  MSG1-CD-ERRO-TS
                MOVE 'Código do Título do Evento não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0616-CD-GR-EVT-FDAR < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1161              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do Tipo de Evento inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0616-CD-GR-EVT-FDAR EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1186              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do Tipo de Evento não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0616-NR-SEQL-EVT-EXCD EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1179              TO  MSG1-CD-ERRO-TS
               MOVE 'Numero do Evento SGT  não informado.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0616-NR-SEQL-EVT-EXCD < ZEROS
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

           PERFORM 240000-ATUALIZA-ESTADO-CRNG

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
           MOVE SGTS0616-CD-ITMT-FNCO
                                            TO SGTST139-CD-ITMT-FNCO
           MOVE SGTS0616-CD-GR-EVT-FDAR
                                            TO SGTST139-CD-GR-EVT-FDAR
           MOVE SGTS0616-NR-SEQL-EVT-EXCD
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
           MOVE SGTS0616-CD-ITMT-FNCO
                                            TO SGTST140-CD-ITMT-FNCO
           MOVE SGTS0616-CD-GR-EVT-FDAR
                                            TO SGTST140-CD-GR-EVT-FDAR
           MOVE SGTS0616-NR-SEQL-EVT-EXCD
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
      *--> 3 - Aprovar

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTST139-DADOS.

           MOVE SGTST139              TO NM-PGM

      *--> Movimentar os campos de entrada

           SET SGTST139-INCLUIR        TO TRUE

           MOVE SGTS0616-CD-ITMT-FNCO  TO SGTST139-CD-ITMT-FNCO

           MOVE SGTS0616-CD-GR-EVT-FDAR
                                       TO SGTST139-CD-GR-EVT-FDAR

           MOVE SGTS0616-NR-SEQL-EVT-EXCD
                                       TO SGTST139-NR-SEQL-EVT-EXCD
           MOVE 'CURRENT'              TO SGTST139-TS-EST-EVT-EXCD

           MOVE KOPRD-CD-OPRD          TO SGTST139-CD-USU-ULT-ATL

      *--> Tipo de estado evento excluído B3 = 3 - Aprovar
           MOVE 3                      TO SGTST139-CD-EST-EVT-EXCD

           MOVE SPACES                 TO SGTST139-TX-JST-EVT-EXCD


           PERFORM 910000-CHAMA-SGTST139
           .
       230000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       240000-ATUALIZA-ESTADO-CRNG   SECTION.
      * Inclui EST_PRCT_CRNG
      *-----------------------------------------------------------------
           MOVE 0007                   TO  MSG1-CD-SEQL-ERRO

      *--> Inclui novo registro de estado do envio do cronograma.
      *--> 2- liberado para processamento do CAC

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTST139-DADOS.

           MOVE SGTST140              TO NM-PGM

      *--> Movimentar os campos de entrada

           SET SGTST140-INCLUIR        TO TRUE

           MOVE SGTS0616-CD-ITMT-FNCO  TO SGTST140-CD-ITMT-FNCO

           MOVE SGTS0616-CD-GR-EVT-FDAR
                                       TO SGTST140-CD-GR-EVT-FDAR

           MOVE SGTS0616-NR-SEQL-EVT-EXCD
                                       TO SGTST140-NR-SEQL-EVT-EXCD
           MOVE 'CURRENT'              TO SGTST140-TS-EST-ENVO-CRNG

           MOVE KOPRD-CD-OPRD          TO SGTST140-CD-USU-RSP-ULT-ATL

      *--> Tipo de estado processamento do cronograma = 2- liberado para
      *--> processamento do CAC
           MOVE 2                      TO SGTST140-CD-EST-PRCT-CRNG

           MOVE SPACES                 TO SGTST140-TS-PRCT-RTN-EVT
                                          SGTST140-TX-PRCT-EVT


           PERFORM 920000-CHAMA-SGTST140
           .
       240000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-MOVIMENTAR-SAIDA         SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'S'
                                       TO SGTS0616-IN-SCS

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
       910000-CHAMA-SGTST139         SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO  MSG1-CD-SEQL-ERRO

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
           MOVE 0010                   TO  MSG1-CD-SEQL-ERRO

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
                  IF SGTST140-INCLUIR

                    MOVE 'SGTST140-TS-EST-ENVO-CRNG'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST140-TS-EST-ENVO-CRNG
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST140-CD-USU-RSP-ULT-ATL'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST140-CD-USU-RSP-ULT-ATL
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST140-CD-EST-PRCT-CRNG'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST140-CD-EST-PRCT-CRNG
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST140-TS-PRCT-RTN-EVT'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST140-TS-PRCT-RTN-EVT
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST140-TX-PRCT-EVT'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST140-TX-PRCT-EVT
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
                  END-IF

004180         WHEN OTHER
004190              CONTINUE
004200     END-EVALUATE
004210
004220     .
004230 969000-SAI.
004240     EXIT.
004250
004260*----------------------------------------------------------------*
004270*                                                                *
004280* SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
004290*                                                                *
004490
004500*----------------------------------------------------------------*
004510 971000-LER-REQUISICAO           SECTION.
004520*----------------------------------------------------------------*
004530
           MOVE 0011                   TO  MSG1-CD-SEQL-ERRO
004550
004560*    Monta o nome do container de requisicao
004570     MOVE 'SGTS0616'             TO NM-PGM
004580     MOVE 'SGTS0616-RQSC'        TO NM-CTNR
004590
004600*    Recupera o tamanho do container de requisição
004610     PERFORM 973100-LER-CONTAINER-NODATA
004620
004630*    Obtem o tamanho esperado da requisicao
004640     MOVE LENGTH OF SGTS0616-RQSC TO QT-CCT-EPRO
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
004860               INTO      (SGTS0616-RQSC)
004870     END-EXEC
004880
004890*Ao chegar aqui, há certeza de que não houve erro algum
004900     SET  SEM-ERRO               TO TRUE
004910
004920     .
004930 971000-SAI.
004940     EXIT.
004950
006260*----------------------------------------------------------------*
006270 973100-LER-CONTAINER-NODATA     SECTION.
006280*----------------------------------------------------------------*
006320*Configura dados para leitura do container de requisição
006330     SET  GET-CONTAINER          TO TRUE
006340
006350*Por causa da captura automática de erro CICS, deve ser previsto
006360*o erro de container
006370     SET  ERRO-CONTAINER         TO TRUE
006380
006390*    Obtem a quantidade de caracteres no container
006400     MOVE ZEROS                  TO QT-CCT-LIDO
006410     EXEC CICS GET
006420               CONTAINER (NM-CTNR)
006430               NODATA
006440               FLENGTH   (QT-CCT-LIDO)
006450     END-EXEC
006460
006470*    Ao chegar aqui, há certeza de que não houve erro algum
006480     SET  SEM-ERRO               TO TRUE
006490     .
006500 973100-SAI.
006510     EXIT.
      *----------------------------------------------------------------*
008170 974000-LER-DADOS-OPRD           SECTION.
008180*----------------------------------------------------------------*
           MOVE 0012                   TO  MSG1-CD-SEQL-ERRO
008200
008210*Configura dados para leitura do container de requisição
008220     SET  GET-CONTAINER          TO TRUE
008230     MOVE 'PRM-INF-OPRD'         TO NM-CTNR
008240
008250*Por causa da captura automática de erro CICS, deve ser previsto
008260*o erro de container
008270     SET  ERRO-CONTAINER         TO TRUE
008280
008290     INITIALIZE PRM-INF-OPRD
008300     EXEC CICS GET
008310          CONTAINER ( NM-CTNR                )
008320          INTO      ( PRM-INF-OPRD           )
008330          FLENGTH   ( LENGTH OF PRM-INF-OPRD )
008340     END-EXEC.
008350
008360*    Ao chegar aqui, há certeza de que não houve erro algum
008370     SET  SEM-ERRO               TO TRUE
008380     .
008390 974000-SAI.
008400     EXIT.
006520*----------------------------------------------------------------*
006530*                                                                *
006540* SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
006550*                                                                *
      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------

      *    Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

           MOVE 0013                   TO  MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de resposta
           MOVE 'SGTS0616-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                    )
                     FROM      (SGTS0616-RPST          )
                     FLENGTH   (LENGTH OF SGTS0616-RPST)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.
007790*----------------------------------------------------------------*
007800 983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
007810*----------------------------------------------------------------*
007820* Exclui o container de resposta e de erro para evitar problemas *
007830* em modulos executados mais de uma vez dentro da mesma sessao do*
007840* CICS. Equivale ao initialize na area de resposta dos programas *
007850* que utilizam COMMAREA.                                         *
007860*----------------------------------------------------------------*
007870
           MOVE 0014                   TO  MSG1-CD-SEQL-ERRO
007890
007900*    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
007910*    nome_modulo -RPST

007970              MOVE NM-PGM        TO NM-CTNR(1:8)
007980              MOVE '-RPST'       TO NM-CTNR(9:5)

008010     SET DEL-CONTAINER           TO TRUE
008020
008030*    Exclui o container de resposta
008040*    Não há captura automática de erro CICS por usar RESP e RESP2
008050     EXEC CICS DELETE
008060               CONTAINER (NM-CTNR)
008070               RESP      (MSG1-RESP1)
008080               RESP2     (MSG1-RESP2)
008090     END-EXEC
008100
008110     EVALUATE TRUE
008120         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
008130         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
008140              CONTINUE
008150         WHEN OTHER
008160              SET ERRO-CONTAINER TO TRUE
008170              PERFORM 999002-TRATA-CONDITION-CICS
008180     END-EVALUATE
008190
008200*    Excluir o container de erro
008210     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
008220     .
008230 983000-SAI.
008240     EXIT.
008250*
008260*----------------------------------------------------------------*
008270 983100-EXCLUIR-CONTAINER-ERRO   SECTION.
008280*----------------------------------------------------------------*
008290
008300*    Limpar variaveis container de erro
008310     INITIALIZE IIBKERRO-PRM-ERRO
008320
008330*    Exclui o container de erro
008350     MOVE 'PRM-ERRO'             TO NM-CTNR
008360     SET DEL-CONTAINER           TO TRUE
008370
008380*    Não há captura automática de erro CICS por usar RESP e RESP2
008390     EXEC CICS DELETE
008400               CONTAINER (NM-CTNR)
008410               RESP      (MSG1-RESP1)
008420               RESP2     (MSG1-RESP2)
008430     END-EXEC
008440
008450     EVALUATE TRUE
008460         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
008470         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
008480              CONTINUE
008490         WHEN OTHER
008500              SET ERRO-CONTAINER TO TRUE
008510              PERFORM 999002-TRATA-CONDITION-CICS
008520     END-EVALUATE
008530     .
008540 983100-SAI.
008550     EXIT.
008560*----------------------------------------------------------------*
008570 984000-GRAVAR-CONTAINER-ERRO    SECTION.
008580*----------------------------------------------------------------*
008610*    Grava o erro gerado no container de erro
008620*    Não há captura automática de erro CICS por usar RESP e RESP2
008630     EXEC CICS PUT
008640               CONTAINER ( 'PRM-ERRO'                  )
008650               FROM      ( IIBKERRO-PRM-ERRO           )
008660               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
008670               RESP      (MSG1-RESP1                   )
008680               RESP2     (MSG1-RESP2                   )
008690     END-EXEC
008700
008710     .
008720 984000-SAI.
008730     EXIT.
008860*----------------------------------------------------------------*
008870*                                                                *
008880* As sections abaixo iniciadas com 999000 representam os tratamen*
008890* tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *
008900*                                                                *
008910*----------------------------------------------------------------*
008920 999000-ERRO                     SECTION.
008930*----------------------------------------------------------------*
008940
008950     IF MSG1-CD-ERRO-TS EQUAL ZEROS
008960        MOVE 8888                TO MSG1-CD-ERRO-TS
008970     END-IF
008980     IF MSG1-TX-ERRO-TS EQUAL SPACES
008990        MOVE 'ERRO NAO ESPECIFICADO NO MODULO' TO MSG1-TX-ERRO-TS
009000     END-IF
009010     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
009020     .
009030 999000-ERRO-SAI.
009040     EXIT.
009050
009060*----------------------------------------------------------------*
009070*     As sections abaixo representam os tratamentos de erro
009080*    utilizando o sistema MSG de mensageria (rotina MSGS0001)
009090*-----------------------------------------------------------------
009100 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
009110*-----------------------------------------------------------------
009120
009130*    Em caso de erro, desfaz as atualizações em banco de dados
009140
009150     EXEC CICS SYNCPOINT
009160               ROLLBACK
009170     END-EXEC
009180
009190*Verifica se o tipo de erro gerado possui tratamento interno
009200     EVALUATE TRUE
009210         WHEN ERRO-CONTAINER
009220              PERFORM 999004-TRATA-ERRO-CTNR
009230         WHEN ERRO-TAMANHO-AREA
009240              PERFORM 999005-TRATA-ERRO-AREA-DADOS
009250         WHEN ERRO-CANAL
009260              PERFORM 999006-TRATA-ERRO-CANAL
009270         WHEN OTHER
009280              CONTINUE
009290     END-EVALUATE
009300
009310*Adiciona as variáveis de entrada para monitoração

               MOVE 'SGTS0616-CD-ITMT-FNCO'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0616-CD-ITMT-FNCO
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0616-CD-GR-EVT-FDAR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0616-CD-GR-EVT-FDAR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0616-NR-SEQL-EVT-EXCD'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0616-NR-SEQL-EVT-EXCD
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
009690* --------------------------------------*
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
009710*---------------------- FIM SGTS0616 -----------------------------