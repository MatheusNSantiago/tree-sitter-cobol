      *----------------------------------------------------------------*
      * PROGRAMA..: SGTS0298
      * ANALISTA..: F9158937 - Silvia Aguiar de Oliveira
      * AUTOR.....: F9158937 - Silvia Aguiar de Oliveira
      * DATA......: 13.07.2023
      * OBJETIVO..: Negócio  - Inclusão Tabela DB2SGT.ACSS_CTRA_TVM
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS001 13.07.2023 - F9158937 - IMPLANTAÇÃO                     *
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS0298.
      *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
      *
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

       77  CTE-INICIO-SS               PIC  X(40)  VALUE
                                            '*** S.S. COMEÇA AQUI ***'.
       77  CTE-PROG                    PIC  X(18)  VALUE
                                            '*** SGTS0298 ***'.
       77  CTE-VERS                    PIC  X(06)  VALUE 'VRS001'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTSI126                    PIC  X(08) VALUE 'SGTSI126'.
           03  IIBP2000                    PIC  X(08) VALUE 'IIBP2000'.
           03  MSTS8042                    PIC  X(08) VALUE 'MSTS8042'.


      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1
      *----------------------------------------------------------------*
      *                     Containers utilizados
      *----------------------------------------------------------------*

      * Container de erro
       77 CTNR-PRM-ERRO                PIC X(16) VALUE 'PRM-ERRO'.

      * Container de requisição desta subrotina
       77 CTNR-SGTS0298-RQSC           PIC X(16) VALUE 'SGTS0298-RQSC'.

      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*

       01  CARTEIRA                    PIC 9(06) VALUE ZEROS.
       01  UOR                         PIC 9(06) VALUE ZEROS.
       01  NM-PGM-CHAMADO              PIC X(08) VALUE SPACES.

      *----------------------------------------------------------------*
      * Books dos Containers e das subrotinas chamadas
      *----------------------------------------------------------------*

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      * Book para captura de abend cics do bloco código HLPKW999
-INC HLPKW999

      * Book para captura de erro via MSG1
-INC MSGKWMG1

      * Book do container de requisição deste módulo
-INC SGTK0298

      * Book do container de requisição e resposta do SGTSI126
-INC SGTKI126

      *-----> Checa UOR
-INC MSTK8042

      * Book do container de Servicos (para acionamento de servicos)
-INC IIBKSRVC

      * Book do Container de Erro
-INC IIBKERRO
      * Book do Container do Usuário
-INC IIBKOPRD

       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*
      *--> Habilita Testes no Cobolito
-INC ALMKCBL2

           PERFORM 100000-INICIAR

           PERFORM 200000-PROCESSAR

           PERFORM 999999-ENCERRAR
           .
       000000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       100000-INICIAR                  SECTION.
      *----------------------------------------------------------------*
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *    Ativa a captura automática de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM 970000-LER-CANAL

      *    Define o nome do programa p/ fins manipulação container
           MOVE CTE-PROG(5:8)          TO NM-PGM

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

           PERFORM 110000-VALIDAR-REQUISICAO

           .
       100000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       110000-VALIDAR-REQUISICAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      * ??   Validação tamanho esperado da requisição X tamanho recebido
      * ??   na section de leitura de container de requisição 971000

      *    Valida campo a campo da requisição

           IF  SGTS0298-CD-CTRA-TVM LESS OR EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0298-CD-UOR-ACSS-CTRA LESS ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE  604               TO  MSG1-CD-ERRO-TS
               MOVE  'Código UOR informado inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           ELSE
               IF  SGTS0298-CD-UOR-ACSS-CTRA  EQUAL ZEROS
                   SET ERRO-PROGRAMADO TO TRUE
                   MOVE  604           TO  MSG1-CD-ERRO-TS
                   MOVE  'Código UOR informado inválido.'
                                       TO  MSG1-TX-ERRO-TS
                   SET MNT-VRV         TO TRUE
                   PERFORM 999000-ERRO
               END-IF
           END-IF

      *    Verifica se UOR existe no MST
           PERFORM 900002-CHECA-UOR

           .
       110000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

      *    Movimenta o nome do programa a ser executado
           MOVE SGTSI126               TO NM-PGM

           MOVE SGTS0298-CD-CTRA-TVM   TO SGTSI126-CD-CTRA-TVM
                                          CARTEIRA
           MOVE SGTS0298-CD-UOR-ACSS-CTRA
                                       TO SGTSI126-CD-UOR-ACSS-CTRA
                                          UOR

           PERFORM 900000-EXECUTAR-MODULO-CTNR

      * MOCK-POINT MOCKMODULO

           IF  IIBKERRO-CD-ERRO        EQUAL 803
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE   0476            TO MSG1-CD-ERRO-TS
               STRING 'Atualização não realizada para Carteira '
                       CARTEIRA
                       '- UOR '
                       UOR
                       ', existem registros duplicados.'
                  DELIMITED BY SIZE    INTO MSG1-TX-ERRO-TS
               END-STRING
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF
           .
       200000-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *----------------------------------------------------------------*
      * Executa um módulo implementado com CONTAINER                   *
      *----------------------------------------------------------------*
       900000-EXECUTAR-MODULO-CTNR     SECTION.
      *----------------------------------------------------------------*
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

      *    Grava o container de requisição do módulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM
               WHEN SGTSI126
                    EXEC CICS LINK
                              PROGRAM ( SGTSI126   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN OTHER
                    MOVE 9006          TO MSG1-CD-ERRO-TS
                    MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE
      *
           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    MOVE 3004          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro ao acionar o módulo.'
                                       TO MSG1-TX-ERRO-TS
                    SET ERRO-CICS      TO TRUE
                    PERFORM 999000-ERRO
           END-EVALUATE

      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO
           IF IIBKERRO-CD-ERRO  = ZEROS
      *       Efetua a leitura do Container de Resposta
              PERFORM 973000-LER-RESPOSTA
           END-IF
           .
       900000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       900002-CHECA-UOR            SECTION.
      *----------------------------------------------------------------*
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO
           MOVE  MSTS8042              TO  NM-PGM-CHAMADO
004110*    carregar o nº serviço / versão / operação / versão
004120
004130     MOVE ZEROS                  TO  SRVC-ID-SRVC
004140     MOVE ZEROS                  TO  SRVC-VRS-SRVC
004150     MOVE 1998646                TO  SRVC-ID-OPR
004160     MOVE 1                      TO  SRVC-VRS-OPR

004180     PERFORM 900003-GRAVAR-SERVICO

           INITIALIZE   MSTS8042-DADOS
                           REPLACING NUMERIC BY  ZEROS
           MOVE SGTS0298-CD-UOR-ACSS-CTRA
                                         TO MSTS8042-CD-UOR
004240
004250     PERFORM 900004-GRAVAR-REQUISICAO
004260     PERFORM 900005-CHAMAR-SERVICO
004270     PERFORM 900006-BUSCAR-RESPOSTA-ERRO
004280     PERFORM 900007-BUSCAR-RESPOSTA

           IF MSTS8042-NM-UOR-RDZ    EQUAL SPACES
              SET ERRO-PROGRAMADO     TO TRUE
              MOVE  604               TO  MSG1-CD-ERRO-TS
              MOVE  'Código UOR informado inválido.'
                                      TO  MSG1-TX-ERRO-TS
              PERFORM  900008-MONITORAR-VARIAVEIS
              SET MNT-VRV             TO TRUE
              PERFORM 999000-ERRO
           END-IF

004290     .
004310 900002-SAI.
004320     EXIT.
      *----------------------------------------------------------------*
004630 900003-GRAVAR-SERVICO           SECTION.
004640*----------------------------------------------------------------*
004650*    Postar container de requisição de serviço.
004660*----------------------------------------------------------------*
004670     MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'PRM-RQSC-SRVC'       TO  NM-CTNR

           EXEC CICS  PUT
                     CONTAINER ( NM-CTNR                      )
004710               FROM      ( SRVC-PRM-RQSC-SRVC           )
004720               FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC )
004730               RESP      ( MSG1-RESP1                   )
004740               RESP2     ( MSG1-RESP2                   )
004750     END-EXEC
004760
004770     EVALUATE TRUE
004780         WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
004790              CONTINUE
004800         WHEN OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  9000        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro ao acionar o serviço.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
004860     END-EVALUATE
004870
004880     .
004890 900003-FIM.
004900     EXIT.
004910*----------------------------------------------------------------*
004930 900004-GRAVAR-REQUISICAO        SECTION.
004940*----------------------------------------------------------------*
004950*    Postar container com dados de requisição (entrada)
004960*----------------------------------------------------------------*
004970     MOVE  0007                  TO  MSG1-CD-SEQL-ERRO
004980
           MOVE  'PRM-DADOS-RQSC'      TO  NM-CTNR

           EVALUATE NM-PGM-CHAMADO
           WHEN MSTS8042
004990         EXEC CICS PUT
005000               CONTAINER ( NM-CTNR                    )
005010               FROM      ( MSTS8042-RQSC              )
005020               FLENGTH   ( LENGTH OF MSTS8042-RQSC    )
005030               RESP      ( MSG1-RESP1                 )
005040               RESP2     ( MSG1-RESP2                 )
005050         END-EXEC
           END-EVALUATE

005070     EVALUATE TRUE
005080         WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
005090              CONTINUE
005100         WHEN OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  9004        TO  MSG1-CD-ERRO-TS
                     STRING 'Erro na gravação da requisição do módulo.'
                             NM-PGM-CHAMADO '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
005160     END-EVALUATE
005170
005180     .
005190 900004-FIM.
005200     EXIT.
005220*----------------------------------------------------------------*
005230 900005-CHAMAR-SERVICO           SECTION.
005240*----------------------------------------------------------------*
005250*    Acionar programa IIBP2000 para requisição do serviço.
005260*----------------------------------------------------------------*
005270     MOVE  0008                  TO  MSG1-CD-SEQL-ERRO
005280
005290     EXEC CICS LINK
005300               PROGRAM   ( IIBP2000     )
005310               CHANNEL   ( NM-CNL       )
005320               RESP      ( MSG1-RESP1   )
005330               RESP2     ( MSG1-RESP2   )
005340     END-EXEC
005350
005360     EVALUATE TRUE
005370         WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
005380              CONTINUE
005390         WHEN OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  9000        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro ao acionar o serviço.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
005450     END-EVALUATE
005460
005470     .
005480 900005-FIM.
005490     EXIT.
      *----------------------------------------------------------------*
005520 900006-BUSCAR-RESPOSTA-ERRO     SECTION.
005530*----------------------------------------------------------------*
005540*    A presença do container PRM-ERRO indica que houve erro de
005550*    integração, a ausência do container indica sucesso.
005560*----------------------------------------------------------------*
005570     MOVE  0009                  TO  MSG1-CD-SEQL-ERRO
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           EXEC CICS  GET
                     CONTAINER ( NM-CTNR                     )
005610               INTO      ( IIBKERRO-PRM-ERRO           )
005620               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
005630               RESP      ( MSG1-RESP1                  )
005640               RESP2     ( MSG1-RESP2                  )
005650     END-EXEC.
005660
005670     EVALUATE TRUE
005680         WHEN MSG1-RESP1         EQUAL DFHRESP(CONTAINERERR)
005690              CONTINUE
005700         WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
                     SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                     MOVE  3004        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro ao acionar o módulo '
                             NM-PGM-CHAMADO '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     END-STRING
                     MOVE  NM-PGM-CHAMADO  TO  MSG1-NM-PGM-ACND
                     MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                     MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                     PERFORM  900008-MONITORAR-VARIAVEIS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  3000        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro na manipulação de container de dados.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
005780     END-EVALUATE
005800     .
005810 900006-FIM.
005820     EXIT.
005840*----------------------------------------------------------------*
005850 900007-BUSCAR-RESPOSTA          SECTION.
005860*----------------------------------------------------------------*
005870*    Recupera o container com os dados devolvidos pelo provedor
005880*    do serviço.
005890*----------------------------------------------------------------*
005900     MOVE  0010                  TO  MSG1-CD-SEQL-ERRO
           MOVE  'PRM-DADOS-RPST'      TO  NM-CTNR

           EVALUATE NM-PGM-CHAMADO
           WHEN MSTS8042
               EXEC CICS  GET
                     CONTAINER ( NM-CTNR                  )
005940               INTO      ( MSTS8042-RPST            )
005950               FLENGTH   ( LENGTH OF MSTS8042-RPST  )
005960               RESP      ( MSG1-RESP1               )
005970               RESP2     ( MSG1-RESP2               )
               END-EXEC
           END-EVALUATE
005990
006000     EVALUATE TRUE
006010         WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
006020              CONTINUE
006030         WHEN OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  9003        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro na leitura da resposta do módulo .'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
006090     END-EVALUATE
006110     .
006120 900007-FIM.
006130     EXIT.
006140*-----------------------------------------------------------------
       900008-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM-CHAMADO
      *------> Monitora as Variaveis de Programas chamados
               WHEN  MSTS8042
                     MOVE 'MSTS8042-CD-UOR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  MSTS8042-CD-UOR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                     CONTINUE
           END-EVALUATE
           .
       900008-SAI.
           EXIT.

      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*

      * Variáveis comuns a todos os programas chamados
           STRING NM-PGM '-CD-CTRA-TVM'  DELIMITED BY SIZE
                                       INTO MSGKW999-NM-VRV-MNT
           EVALUATE NM-PGM
             WHEN SGTSI126
                  MOVE SGTSI126-CD-CTRA-TVM
                                       TO MSGKW999-VL-VRV-MNT

           END-EVALUATE
           PERFORM 999003-ADICIONA-VRV-MNT


      * Variáveis exclusivas de cada programa acionado
           EVALUATE NM-PGM

             WHEN SGTSI126
                  MOVE 'SGTSI126-CD-UOR-ACSS-CTRA'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI126-CD-UOR-ACSS-CTRA
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
       970000-LER-CANAL                SECTION.
      *    Obtem o canal corrente
      *----------------------------------------------------------------*
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       970000-FIM.
           EXIT.

      *----------------------------------------------------------------*
       971000-LER-REQUISICAO           SECTION.
      *----------------------------------------------------------------*
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisição
           MOVE CTE-PROG(5:8)          TO NM-PGM
           MOVE CTNR-SGTS0298-RQSC     TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER ( NM-CTNR       )
                     FLENGTH   ( QT-CCT-LIDO   )
                     INTO      ( SGTS0298-RQSC )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       971000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *----------------------------------------------------------------*
      *    A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *----------------------------------------------------------------*
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      *    Lê o container de erro dos módulos chamados
           SET GET-CONTAINER           TO TRUE
           MOVE CTNR-PRM-ERRO          TO NM-CTNR
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS GET
                     CONTAINER ( NM-CTNR                     )
                     INTO      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC

           EVALUATE TRUE
      *        Caso deseje personalizar ou não interromper o
      *        processamento, alterar para não dar erro aqui SOMENTE
      *        PARA O módulo CHAMADO e tratar na section chamadora
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
      *        IF  NM-PGM            EQUAL SGTSI126  AND
      *             IIBKERRO-CD-ERRO  EQUAL 803
      *             CONTINUE
      *         ELSE
      *--> Trata o erro de programa acionado por ser módulo interno
                SET ERRO-PROGRAMA-ACIONADO   TO  TRUE
                MOVE NM-PGM                  TO  MSG1-NM-PGM-ACND
                IF NM-PGM(1:3) EQUAL 'SGT'
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    MOVE IIBKERRO-CD-ERRO    TO  MSG1-CD-RTN-PGM-ACND
                                                 MSG1-CD-ERRO-TS
                    MOVE IIBKERRO-TX-ERRO    TO  MSG1-TX-RTN-PGM-ACND
                                                 MSG1-TX-ERRO-TS
                ELSE
                    MOVE  3004               TO  MSG1-CD-ERRO-TS
                    STRING  'Erro ao acionar o módulo ' NM-PGM      '.'
                            DELIMITED BY SIZE
                            INTO        MSG1-TX-ERRO-TS
                    END-STRING
                    MOVE IIBKERRO-CD-ERRO     TO  MSG1-CD-RTN-PGM-ACND
                    MOVE IIBKERRO-TX-ERRO     TO  MSG1-TX-RTN-PGM-ACND
      *          END-IF
      *-->      Monitorar variáveis do módulo chamado
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-ERRO
               END-IF
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       973000-LER-RESPOSTA             SECTION.
      *----------------------------------------------------------------*
      *    Recupera o container com os dados de resposta (saída).
      *----------------------------------------------------------------*
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

      *    Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

      *    Obtém o tamanho esperado da requisição, quando é fixo
           EVALUATE NM-PGM
               WHEN SGTSI126
                    MOVE LENGTH OF SGTSI126-RPST
                                       TO QT-CCT-EPRO
           END-EVALUATE

      *    Valida tamanho esperado da requisição X tamanho recebido

           IF  QT-CCT-LIDO   NOT EQUAL QT-CCT-EPRO
               AND (NM-PGM   EQUAL SGTSI126)
               SET   ERRO-TAMANHO-AREA TO TRUE
               PERFORM 999000-ERRO
           END-IF

      *Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Lê a RESPOSTA de acordo com o programa que a gravou

           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM

      *    Calcula a quantidade de caracteres esperado e limpa a área de
      *    dados antes de executar o GET (somentepara matrizes com dados
      *    não variáveis)


               WHEN SGTSI126
                    INITIALIZE SGTSI126-RPST
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSI126-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN OTHER
                    MOVE 9006          TO MSG1-CD-ERRO-TS
                    MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       973100-LER-CONTAINER-NODATA     SECTION.
      *----------------------------------------------------------------*
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Obtém a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER ( NM-CTNR     )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973100-SAI.
           EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
      *                                                                *
      *----------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *----------------------------------------------------------------*
      *    Postar container com dados de requisição (entrada)
      *----------------------------------------------------------------*
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RQSC'                TO NM-CTNR(9:5)

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisição do programa a executar
           EVALUATE NM-PGM
               WHEN SGTSI126
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSI126-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSI126-RQSC )
                    END-EXEC
               WHEN OTHER
                    MOVE 9006          TO MSG1-CD-ERRO-TS
                    MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
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
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

      *    Padrão: nome_modulo-RPST
           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

           SET DEL-CONTAINER           TO TRUE

      *    Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR    )
                     RESP      ( MSG1-RESP1 )
                     RESP2     ( MSG1-RESP2 )
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

      *    Limpar variáveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO
           MOVE CTNR-PRM-ERRO          TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR    )
                     RESP      ( MSG1-RESP1 )
                     RESP2     ( MSG1-RESP2 )
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
           MOVE 0020                   TO MSG1-CD-SEQL-ERRO

      *    Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS PUT
                     CONTAINER ( CTNR-PRM-ERRO               )
                     FROM      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
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

           IF  MSG1-CD-ERRO-TS         EQUAL ZEROS
               MOVE 8888               TO MSG1-CD-ERRO-TS
           END-IF
           IF  MSG1-TX-ERRO-TS         EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                       TO MSG1-TX-ERRO-TS
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
           IF  MNT-VRV
               MOVE 'SGTS0298-CD-CTRA-TVM '
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0298-CD-CTRA-TVM
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0298-CD-UOR-ACSS-CTRA'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0298-CD-UOR-ACSS-CTRA
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'CARTEIRA'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  CARTEIRA
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'UOR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  UOR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT
            END-IF

      *    Grava o erro gerado no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG

           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO

      *    Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

      *    Encerra a transação
           PERFORM 999999-ENCERRAR
           .
       999000-SAI.
           EXIT.
      * --------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      * --------------------------------------*
-INC HLPKP999
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999
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
-INC MSGKPMG1
      *----------------------------------------------------------------*
       999999-ENCERRAR                 SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *---------------------- Fim SGTS0298 ----------------------------*