      *----------------------------------------------------------------*
      * PROGRAMA..: SGTS0200
      * ANALISTA..: F9507141 - Vicente de Paulo Tavares de Souza
      * AUTOR.....: F9507141 - Vicente de Paulo Tavares de Souza
      * DATA......: 15/05/2018
      * OBJETIVO..: Listar Segmentos das UOR's.
      * COMPILACAO: 52 - PSOSE600 - Cobol 4.2 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS002 30/06/2020 F0950413  DE 2020/145195 -Alteração das mensa-
      *        gens de erro de 001 para 535 na SECTION  200000-PROCESSAR
      *        E 0000 para 1112, na SECTION 210000-CONSULTA-SEGMENTO.
      * VRS001 08/01/2019 F2804421  DE 2016/145195 -Inclusão do comando
      *                             'GOBACK'.
      *        11/05/2018 F9507141  DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS0200.
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
                                            '*** SGTS0200 ***'.
       77  CTE-VERS                    PIC  X(06)  VALUE 'VRS002'.

       77  SBSORT                      PIC  X(08) VALUE 'SBSORT  '.

      *--> Módulos acionados / Padrão para Cobolito
       01  TAB-PROGRAMAS-CHAMADOS.
           03  IIBP2000                PIC  X(08) VALUE 'IIBP2000'.
           03  IORS0004                PIC  X(08) VALUE 'IORS0004'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *----------------------------------------------------------------*
      *                     Containers utilizados
      *----------------------------------------------------------------*

      * Container de erro
       77 CTNR-PRM-ERRO                PIC X(16) VALUE 'PRM-ERRO'.

      * Container de requisição desta subrotina
       77 CTNR-SGTS0200-RQSC           PIC X(16) VALUE 'SGTS0200-RQSC'.

      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*

      * Áreas de trabalho

       01  IC-TAB                      PIC S9(04)  COMP-5 VALUE    0.

       01  IC-SEL                      PIC S9(04)  COMP-5 VALUE    0.

      *--> Lista dos Segmentos das UOR's.
       01  SEGMENTOS.
           03  TAB-PRM                 OCCURS  5  TIMES.
               05  CD-DET-PRM          PIC S9(04)  COMP-5  VALUE   0.
               05  NM-DET-PRM          PIC  X(40)          VALUE ' '.
               05  TX-DET-PRM          PIC  X(65)          VALUE ' '.

      *--> Area de Parametros da Rotina SBSORT
       01  TAB-PRM-QT-REGISTRO         PIC S9(08)  COMP    VALUE   0.
       01  TAB-PRM-TAMANHO             PIC S9(08)  COMP    VALUE 107.
       01  TAB-PRM-CHAVE.
           03  TAB-PRM-QT-CHAVES       PIC S9(08)  COMP    VALUE   1.
      *--> Chave 01 - Código do Segmento da UOR.
           03  TAB-PRM-01-INICIO       PIC S9(08)  COMP    VALUE   1.
           03  TAB-PRM-01-TAMANHO      PIC S9(08)  COMP    VALUE   2.
           03  TAB-PRM-01-FORMATO      PIC S9(08)  COMP    VALUE  +4.

      *----------------------------------------------------------------*
      * Books dos Containers e das subrotinas chamadas
      *----------------------------------------------------------------*

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      * Book para captura de abend cics do bloco código HLPKW999
-INC HLPKW999

      * Book para captura de erro via MSG1
-INC MSGKWMG1

      * Book do container de requisição e resposta deste módulo
       01 SGTS0200-DADOS.
-INC SGTK0200

       01 IORS0004-DADOS.
-INC IORK0004

      * Book do Container de Erro
-INC IIBKERRO

      * Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC

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

           GOBACK
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

           .
       100000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  IORS0004-DADOS

      *    Movimentar o nome do programa a ser executado
           MOVE  IORS0004              TO  NM-PGM

      *    Movimentar os campos de entrada
           MOVE  2                     TO  IORS0004-CD-FUNCAO
           MOVE  1                     TO  IORS0004-CD-IOR-E
           MOVE  'A'                   TO  IORS0004-IN-ORDEM-LEITURA

      *    Zera o contador.
           MOVE  0                     TO  IC-SEL

           PERFORM  210000-CONSULTA-SEGMENTO
              UNTIL IC-SEL  =  5

      *    Ordena a lista
           MOVE  IC-SEL                TO  TAB-PRM-QT-REGISTRO
           CALL  SBSORT             USING  SEGMENTOS
                                           TAB-PRM-QT-REGISTRO
                                           TAB-PRM-TAMANHO
                                           TAB-PRM-CHAVE

           IF  RETURN-CODE                 NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0535              TO  MSG1-CD-ERRO-TS
               MOVE  'Erro na execução da ordenação da lista de entrada.
      -              ''                TO  MSG1-TX-ERRO-TS
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  IC-SEL                TO  SGTS0200-QT-OCR-TAB

           PERFORM VARYING IC-SEL FROM 1 BY 1
              UNTIL IC-SEL  >  5
              MOVE  CD-DET-PRM(IC-SEL) TO  SGTS0200-CD-DET-PRM(IC-SEL)
              MOVE  NM-DET-PRM(IC-SEL) TO  SGTS0200-NM-DET-PRM(IC-SEL)
              MOVE  TX-DET-PRM(IC-SEL) TO  SGTS0200-TX-DET-PRM(IC-SEL)
           END-PERFORM

           PERFORM  980000-GRAVAR-CONTAINER-RPST

           .
       200000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       210000-CONSULTA-SEGMENTO        SECTION.
      *----------------------------------------------------------------*
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO


           PERFORM 910000-CHAMAR-SERVICO

           PERFORM VARYING IC-TAB FROM 1 BY 1
              UNTIL IC-TAB  >  IORS0004-NR-OCR-SAI
              OR    IC-SEL  =  5
              IF  IORS0004-CD-ITEM-AQTT-S(IC-TAB)  =  47  OR  50  OR  51
                                                          OR 202  OR 214
                  ADD  1               TO  IC-SEL
                  MOVE  IORS0004-CD-ITEM-AQTT-S(IC-TAB)
                                       TO  CD-DET-PRM(IC-SEL)
                  MOVE  IORS0004-NM-ITEM-AQTT-S(IC-TAB)
                                       TO  NM-DET-PRM(IC-SEL)
                  MOVE  IORS0004-TX-ITEM-AQTT-S(IC-TAB)
                                       TO  TX-DET-PRM(IC-SEL)
              END-IF
           END-PERFORM

           IF  IC-SEL   NOT EQUAL   5
               IF  IORS0004-IN-RECHAMADA   EQUAL   'S'
                   INITIALIZE          IORS0004-CONTROLE
                   INITIALIZE          IORS0004-SAIDA
                   MOVE  'N'           TO  IORS0004-IN-RECHAMADA
               ELSE
                   SET ERRO-PROGRAMA-ACIONADO TO TRUE
                   MOVE 1112           TO  MSG1-CD-ERRO-TS
                   MOVE 'Erro na busca dos segmentos das uors.'
                                       TO  MSG1-TX-ERRO-TS
                   PERFORM 999000-ERRO
               END-IF
           END-IF

           .
       210000-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *----------------------------------------------------------------*
      * Executa um módulo implementado com CONTAINER                   *
      *-----------------------------------------------------------------
       910000-CHAMAR-SERVICO           SECTION.
      *--> Acionar programa IIBP2000 para requisição do serviço.
      *-----------------------------------------------------------------

      *--> Grava o container com dados do serviço/operação a chamar
           PERFORM 981000-GRAVAR-CONTAINER-SRVC

      *--> Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO


      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS LINK
                     PROGRAM   ( IIBP2000     )
                     CHANNEL   ( NM-CNL       )
                     RESP      ( MSG1-RESP1   )
                     RESP2     ( MSG1-RESP2   )
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1             EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET   ERRO-CICS    TO  TRUE
                    MOVE  9000         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro ao acionar serviço: '
                            SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
                            SRVC-ID-OPR  '.' SRVC-VRS-OPR
                            'via IIBP2000, ' 'RESP=' MSG1-RESP1
                                           ', RESP-2=' MSG1-RESP2 '.'
                            DELIMITED BY SIZE
                            INTO  MSG1-TX-ERRO-TS
                    END-STRING

                    PERFORM 999000-ERRO
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA

           .
       910000-SAI.
           EXIT.


      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*

      * Variáveis exclusivas de cada programa acionado
           EVALUATE NM-PGM
               WHEN IORS0004
                    MOVE 'IORS0004-CD-IOR-E'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE  IORS0004-CD-IOR-E
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IORS0004-IN-ORDEM-LEITURA'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE  IORS0004-IN-ORDEM-LEITURA
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IORS0004-IN-RECHAMADA'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE  IORS0004-IN-RECHAMADA
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IORS0004-CD-ITEM-ANT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE  IORS0004-CD-ITEM-ANT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IORS0004-NM-ITEM-ANT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE  IORS0004-NM-ITEM-ANT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IORS0004-CD-ITEM-PRX'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE  IORS0004-CD-ITEM-PRX
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IORS0004-NM-ITEM-PRX'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE  IORS0004-NM-ITEM-PRX
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
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

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
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *----------------------------------------------------------------*
      *    A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *----------------------------------------------------------------*

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
                    SET ERRO-PROGRAMA-ACIONADO TO TRUE
                    MOVE  NM-PGM       TO  MSG1-NM-PGM-ACND

                    IF  NM-PGM(1:3) EQUAL 'SGT'
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

                       MOVE IIBKERRO-TX-ERRO TO MSG1-TX-RTN-PGM-ACND
                       MOVE IIBKERRO-CD-ERRO TO MSG1-CD-RTN-PGM-ACND
                    END-IF

      *             Monitorar variáveis do módulo chamado
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-ERRO
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

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           EVALUATE NM-PGM
               WHEN IORS0004
                    MOVE 'PRM-DADOS-RPST'
                                       TO NM-CTNR
               WHEN OTHER
                    MOVE NM-PGM        TO NM-CTNR(1:8)
                    MOVE '-RPST'       TO NM-CTNR(9:5)
           END-EVALUATE

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

      *             INITIALIZE IORS0004-DADOS


               WHEN IORS0004
                    MOVE LENGTH OF IORS0004-DADOS     TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (IORS0004-DADOS   )
                              FLENGTH   (QT-CCT-LIDO      )
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

      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

      *--> Exclui eventual container de erro de outros programas
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO


      *--> Monta o nome do container de resposta
           MOVE  'SGTS0200-RPST'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                              )
                      FROM      ( SGTS0200-RPST                        )
                      FLENGTH   ( LENGTH OF SGTS0200-RPST              )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       980000-SAI.
           EXIT.

      *---------------------------------------------------------------*
       981000-GRAVAR-CONTAINER-SRVC    SECTION.
      *---------------------------------------------------------------*
      *    Postar container de requisição de serviço.
      *---------------------------------------------------------------*

      *    Monta o nome do container de requisicao do servico
           MOVE 'PRM-RQSC-SRVC'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

           EVALUATE NM-PGM

               WHEN IORS0004
                    MOVE 101620        TO SRVC-ID-SRVC
                    MOVE 1             TO SRVC-VRS-SRVC
                    MOVE 105931        TO SRVC-ID-OPR
                    MOVE 1             TO SRVC-VRS-OPR

           END-EVALUATE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                     )
                     FROM      ( SRVC-PRM-RQSC-SRVC          )
                     FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       981000-SAI.
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

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_modulo -RQSC
           EVALUATE NM-PGM
               WHEN IORS0004
                    MOVE 'PRM-DADOS-RQSC'
                                       TO NM-CTNR
               WHEN OTHER
                    MOVE NM-PGM        TO NM-CTNR(1:8)
                    MOVE '-RQSC'       TO NM-CTNR(9:5)
           END-EVALUATE


      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisição do programa a executar
           EVALUATE NM-PGM
               WHEN IORS0004
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (IORS0004-DADOS   )
                              FLENGTH   (LENGTH OF IORS0004-DADOS   )
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

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           EVALUATE NM-PGM
               WHEN IORS0004
                    MOVE 'PRM-DADOS-RPST'
                                       TO NM-CTNR
               WHEN OTHER
                    MOVE NM-PGM        TO NM-CTNR(1:8)
                    MOVE '-RPST'       TO NM-CTNR(9:5)
           END-EVALUATE

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

      *    Verifica se o tipo de erro gerado possui tratamento interno
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

      *    O SGTS0200 não posui variáveis de entrada

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
      *---------------------- Fim SGTS0200 ----------------------------*