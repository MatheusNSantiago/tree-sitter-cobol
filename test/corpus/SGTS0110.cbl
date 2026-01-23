      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0110
      * ANALISTA..: F2054109 - Cintia Ap Soares Iglesias
      * AUTOR.....: F4535118 - Ivone de Melo Costa
      * DATA......: 28/12/2016
      * OBJETIVO..: Consultar precificação da operação estruturada
      * Tipo de comunicação: Container
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 30/01/2019 - F0950413 inclusão da sigla 'SGT' na section
      *                     972000-LER-CONTAINER-DE-ERRO
      *        03/10/2016 - F4535118 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS0110.
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

       77  CTE-INICIO-SS                 PIC  X(040)  VALUE
                    '*** S.S. COMECA AQUI ***'.

       77  CTE-PROG                      PIC  X(018)  VALUE
                    '*** SGTS0110 ***'.
       77  CTE-VERS                      PIC  X(006)  VALUE 'VRS001'.

      * Mdulos acionados
       77  SGTST005                      PIC  X(008) VALUE 'SGTST005'.
       77  SGTSC063                      PIC  X(008) VALUE 'SGTSC063'.

      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*

      * Areas de trabalho
       01  CD-OPR-ETRD-MCP-DISPLAY       PIC  S9(011) VALUE 0.

      *----------------------------------------------------------------*
      * Book dos Containers e das subrotinas chamadas
      *----------------------------------------------------------------*

      * Book das variáveis auxiliares do tratamento de erro via MSG
       COPY SGTKERR1.

      * Book para captura de abend cics da bloco codigo HLPKW999
       COPY HLPKW999.

      * Book para captura de erro via MSG1
       COPY MSGKWMG1.

      * Book do container de requisição e resposta deste modulo
       01 SGTS0110-DADOS.
       COPY SGTK0110.

      * CRUD OPR_ETRD_MCP
       01 SGTST005-DADOS.
       COPY SGTKT005.

      * CRUD IN_OPR_ETRD
       COPY SGTKC063.

      * Book do Container de Erro
       COPY IIBKERRO.

      * Book dos dados do usuário
       COPY IIBKOPRD.

       01  CTE-FINAL-SS                  PIC X(40)      VALUE
                    '          *** S.S. TERMINA AQUI ***'.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-BUSCA-REQUISICAO

           PERFORM 200000-PROCESSAR

           PERFORM 990000-ENCERRAR
           .
       000000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       100000-BUSCA-REQUISICAO         SECTION.
      *----------------------------------------------------------------*

           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *    Ativa a captura automatica de abend do CICS.
      *    WHENEVER CICS
       COPY HLPKE999.

           PERFORM 970000-LER-CANAL

      *    Define o nome do programa p/ fins manipulação container
           MOVE 'SGTS0110'                 TO NM-PGM
           MOVE 'SGTS0110-RPST'            TO NM-CTNR

      *    Limpa a resposta e erro antes de executar o modulo
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

      *    Validação tamanho esperado da requisição X tamanho recebido
      *    na section de leitura de container de requisição 971000

      *    Valida campo a campo da requisição

           IF  SGTS0110-CD-CTRA-TIT-VL <= ZEROS
               SET ERRO-PROGRAMADO         TO TRUE
               MOVE 0001                   TO MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'          TO MSG1-TX-ERRO-TS
               SET MNT-VRV                 TO TRUE
               PERFORM 998900-ERRO
           END-IF

           IF  SGTS0110-CD-OPR-ETRD-MCP IS NOT NUMERIC
               SET ERRO-PROGRAMADO         TO TRUE
               MOVE 0003                   TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'           TO MSG1-TX-ERRO-TS
               SET MNT-VRV                 TO TRUE
               PERFORM 998900-ERRO
           END-IF

           IF  SGTS0110-CD-OPR-ETRD-MCP <= ZEROS
               SET ERRO-PROGRAMADO         TO TRUE
               MOVE 0004                   TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais inv
      -             'álido.'               TO MSG1-TX-ERRO-TS
               SET MNT-VRV                 TO TRUE
               PERFORM 998900-ERRO
           END-IF


           IF  SGTS0110-NR-VRS-OPR-ETRD  <= ZEROS
               SET ERRO-PROGRAMADO         TO TRUE
               MOVE 0006                   TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                           TO MSG1-TX-ERRO-TS
               SET MNT-VRV                 TO TRUE
               PERFORM 998900-ERRO
           END-IF

           .
       110000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           PERFORM 210000-CONSULTAR-OPERACAO

           PERFORM 220000-CONSULTAR-IND-FAC

           PERFORM 230000-CONSULTAR-IND-RMNR

           PERFORM 980000-GRAVAR-CONTAINER-RPST
           .
       200000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       210000-CONSULTAR-OPERACAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTST005-CHAVE.

      *    Movimentar o nome do programa a ser executado
           MOVE 'SGTST005'                TO NM-PGM

      *    Movimentar os campos de entrada

           MOVE  4                         TO  SGTST005-ACAO

           MOVE  SGTS0110-CD-CTRA-TIT-VL   TO  SGTST005-CD-CTRA-TIT-VL
           MOVE  SGTS0110-CD-OPR-ETRD-MCP  TO  SGTST005-CD-OPR-ETRD-MCP
           MOVE  SGTS0110-NR-VRS-OPR-ETRD  TO  SGTST005-NR-VRS-OPR-ETRD

           PERFORM   920000-CHAMAR-SGTST005

           MOVE CORR SGTST005-DT-EMS-PREF-R TO  SGTS0110-DT-EMS-PREF-R

           MOVE SGTST005-QT-DD-PZ-VLD-PREF TO SGTS0110-QT-DD-PZ-VLD-PREF
           MOVE  SGTST005-PC-TARF-GRT-TAXA TO  SGTS0110-PC-TARF-GRT-TAXA
           MOVE  SGTST005-QT-MM-CARE-JUR   TO  SGTS0110-QT-MM-CARE-JUR
           MOVE  SGTST005-QT-MM-CARE-OPR   TO  SGTS0110-QT-MM-CARE-OPR
           .
       210000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       220000-CONSULTAR-IND-FAC        SECTION.
      *----------------------------------------------------------------*
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSC063-DADOS.

      *    Movimentar o nome do programa a ser executado
           MOVE 'SGTSC063'                 TO NM-PGM

      *    Movimentar os campos de entrada

           MOVE  SGTS0110-CD-CTRA-TIT-VL   TO  SGTSC063-CD-CTRA-TIT-VL
           MOVE  SGTS0110-CD-OPR-ETRD-MCP  TO  SGTSC063-CD-OPR-ETRD-MCP
           MOVE  SGTS0110-NR-VRS-OPR-ETRD  TO  SGTSC063-NR-VRS-OPR-ETRD
           MOVE   17                       TO  SGTSC063-CD-TIP-IN

           PERFORM  900000-EXECUTAR-MODULO

           MOVE  SGTSC063-IN-EXT-TIP       TO  SGTS0110-IN-FCTR
           .
       220000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       230000-CONSULTAR-IND-RMNR   SECTION.
      *----------------------------------------------------------------*
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSC063-DADOS.

      *    Movimentar o nome do programa a ser executado
           MOVE 'SGTSC063'                 TO NM-PGM

      *    Movimentar os campos de entrada

           MOVE  SGTS0110-CD-CTRA-TIT-VL   TO  SGTSC063-CD-CTRA-TIT-VL
           MOVE  SGTS0110-CD-OPR-ETRD-MCP  TO  SGTSC063-CD-OPR-ETRD-MCP
           MOVE  SGTS0110-NR-VRS-OPR-ETRD  TO  SGTSC063-NR-VRS-OPR-ETRD
           MOVE   16                       TO  SGTSC063-CD-TIP-IN

           PERFORM  900000-EXECUTAR-MODULO

           IF  SGTSC063-IN-EXT-TIP  = 'N'
               MOVE  'N'                   TO  SGTS0110-IN-RMNC-VRV-PREF
           ELSE
               MOVE  'S'                   TO  SGTS0110-IN-RMNC-VRV-PREF
           END-IF
           .
       230000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       900000-EXECUTAR-MODULO          SECTION.
      *----------------------------------------------------------------*

           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

      *    Grava o container de requisição do modulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM
               WHEN SGTSC063
                   EXEC CICS LINK
                             PROGRAM ( SGTSC063)
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN OTHER
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 9006             TO MSG1-CD-ERRO-TS
                 MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    MOVE 3004          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro ao acionar o módulo.'
                                       TO MSG1-TX-ERRO-TS
                    SET ERRO-CICS      TO TRUE
      *             Monitorar variáveis do módulo chamado                     VR
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO


      *    Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA

           .
       900000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       920000-CHAMAR-SGTST005          SECTION.
      *----------------------------------------------------------------*
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE  SGTST005              TO  NM-PGM

           EXEC CICS LINK
                PROGRAM    ( SGTST005                 )
                COMMAREA   ( SGTST005-DADOS           )
                LENGTH     ( LENGTH OF SGTST005-DADOS )
           END-EXEC

           IF SGTST005-CD-RTN-PGM  NOT EQUAL  0  AND  100
              SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
              MOVE 3004                TO MSG1-CD-ERRO-TS
              MOVE 'Erro ao acionar o módulo.'
                                       TO MSG1-TX-ERRO-TS
              MOVE SGTST005            TO MSG1-NM-PGM-ACND
              MOVE SGTST005-CD-RTN-PGM TO MSG1-CD-RTN-PGM-ACND
              MOVE SGTST005-MSG-ERRO   TO MSG1-TX-RTN-PGM-ACND
      *       Monitorar variáveis do módulo chamado                     VRS001
              PERFORM 969000-MONITORAR-VARIAVEIS
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       920000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM

               WHEN SGTST005
                    MOVE 'SGTST005-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST005-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST005-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF  SGTST005-CD-OPR-ETRD-MCP IS NUMERIC
                        MOVE SGTST005-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                        MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE 'Código de Operação Estruturada deve ser nú
      -                      'mero.'   TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST005-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST005-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST005-DT-EMS-PREF'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST005-DT-EMS-PREF
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST005-QT-DD-PZ-VLD-PREF'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST005-QT-DD-PZ-VLD-PREF
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST005-IN-RMNC-VRV'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST005-IN-RMNC-VRV
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST005-QT-MM-CARE-JUR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST005-QT-MM-CARE-JUR
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST005-QT-MM-CARE-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST005-QT-MM-CARE-OPR
                                       TO MSGKW999-VL-VRV-MNT

                    PERFORM 999003-ADICIONA-VRV-MNT


               WHEN SGTSC063

                    MOVE 'SGTSC063-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSC063-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSC063-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF  SGTSC063-CD-OPR-ETRD-MCP IS NUMERIC
                        MOVE SGTSC063-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                        MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE 'Código de Operação Estruturada deve ser nú
      -                       'mero.'  TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSC063-NR-VRS-OPR-ETRDD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSC063-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSC063-CD-TIP-IN'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSC063-CD-TIP-IN
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSC063-IN-EXT-TIP'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSC063-IN-EXT-TIP
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
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

      *    Por causa da captura automática de erro CICS,
      *    deve ser previsto o erro de container.
           SET  ERRO-CANAL                 TO TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO                   TO TRUE
           .
       970000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       971000-LER-REQUISICAO           SECTION.
      *----------------------------------------------------------------*

           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisição
           MOVE 'SGTS0110'                 TO NM-PGM
           MOVE 'SGTS0110-RQSC'            TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      * Caso a requisição seja variável (cláusula DEPENDING ON), o
      * tratamento abaixo não deve ser efetuado, senão, ocasionará erro
      *    -->> Início
      *    Obtem o tamanho esperado da requisição
           MOVE LENGTH OF SGTS0110-RQSC    TO QT-CCT-EPRO

      *    Valida tamanho esperado da requisição X tamanho recebido

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
      *    -->> Fim

           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

      *    Configura dados para leitura do container de requisição
           SET  GET-CONTAINER              TO TRUE

      *    Por causa da captura automática de erro CICS,
      *    deve ser previsto o erro de container.
           SET  ERRO-CONTAINER             TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0110-RQSC)
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO                   TO TRUE

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

      *    Le o container de erro dos modulos chamados
           SET GET-CONTAINER               TO TRUE
           MOVE 'PRM-ERRO'                 TO NM-CTNR

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
      *        PARA O MÓDULO CHAMADO e tratar na section chamadora
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
      *--> Trata o erro de programa acionado por ser módulo interno
                SET ERRO-PROGRAMA-ACIONADO   TO  TRUE
                MOVE NM-PGM                  TO  MSG1-NM-PGM-ACND
                IF NM-PGM(1:3) EQUAL 'SGT'
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
                END-IF
      *-->          Monitorar variáveis do módulo chamado
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 998900-ERRO

               WHEN OTHER
                    SET ERRO-CONTAINER  TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       973000-LER-RESPOSTA             SECTION.
      *----------------------------------------------------------------*
      *     Recupera o container com os dados de resposta (sada).

      *                        NOME DO CONTAINER RESPOSTA
      *                        --------------------------

      *    Modulo CONTAINER  -     NOME_PGM-RPST
      *    Modulo IIB (FIXO) -     PRM-DADOS-RPST

      *---------------------------------------------------------------*

           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome dos container de acordo com módulo a executar
           MOVE SPACES                     TO NM-CTNR

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           MOVE NM-PGM                     TO NM-CTNR(1:8)
           MOVE '-RPST'                    TO NM-CTNR(9:5)

      *    Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

      *    Configura dados para leitura do container de resposta
           SET  GET-CONTAINER              TO TRUE

      *    Por causa da captura automática de erro CICS,
      *    deve ser previsto o erro de container
           SET  ERRO-CONTAINER             TO TRUE

           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

      *    Le a RESPOSTA de acordo com o programa que a gravou
           EVALUATE NM-PGM

      *        Calcular a quantidade de caracteres esperado e limpar
      *        a área de dados antes de executar o GET
      *        Para matrizes com dados variáveis (clausula depending
      *        on) não há como utilizar o comando INITIALIZE

                WHEN SGTSC063
                     INITIALIZE SGTSC063-RPST
                     MOVE LENGTH OF SGTSC063-RPST
                                       TO QT-CCT-EPRO
                     EXEC CICS GET
                               CONTAINER (NM-CTNR          )
                               INTO      (SGTSC063-RPST    )
                               FLENGTH   (QT-CCT-LIDO      )
                     END-EXEC

                WHEN OTHER
                     SET ERRO-PROGRAMADO
                                       TO TRUE
                     MOVE 9006         TO MSG1-CD-ERRO-TS
                     MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       973100-LER-CONTAINER-NODATA     SECTION.
      *----------------------------------------------------------------*

           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

      *    Configura dados para leitura do container de requisição
           SET  GET-CONTAINER              TO TRUE

      *    Por causa da captura automática de erro CICS,
      *    deve ser previsto o erro de container
           SET  ERRO-CONTAINER             TO TRUE

      *    Obtem a quantidade de caracteres no container
           MOVE ZEROS                      TO QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO                   TO TRUE
           .
       973100-SAI.
           EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
      *                                                                *
      *----------------------------------------------------------------*
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *----------------------------------------------------------------*

           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

      *    Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

      *    Monta o nome do container de resposta
           MOVE 'SGTS0110-RPST'            TO NM-CTNR
           SET PUT-CONTAINER               TO TRUE

      *    Por causa da captura automática de erro CICS,
      *    deve ser previsto o erro de container
           SET  ERRO-CONTAINER             TO TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                    )
                     FROM      (SGTS0110-RPST          )
                     FLENGTH   (LENGTH OF SGTS0110-RPST)
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO                   TO TRUE
           .
       980000-SAI.
           EXIT.
      *---------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *----------------------------------------------------------------*
      *    Postar container com dados de requisição (entrada)
      *
      *                        NOME DO CONTAINER REQUISIÇÃO
      *                        ----------------------------
      *
      *    Modulo CONTAINER  -     NOME_PGM-RQSC
      *    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
      *
      *----------------------------------------------------------------*

           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                     TO NM-CTNR
           SET PUT-CONTAINER               TO TRUE

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_modulo -RQSC
           MOVE NM-PGM                     TO NM-CTNR(1:8)
           MOVE '-RQSC'                    TO NM-CTNR(9:5)

      *    Por causa da captura automática de erro CICS,
      *    deve ser previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisição do programa a executar
           EVALUATE NM-PGM

                WHEN SGTSC063
                     EXEC CICS PUT
                               CONTAINER (NM-CTNR)
                               FROM      (SGTSC063-RQSC  )
                               FLENGTH   (LENGTH OF SGTSC063-RQSC  )
                     END-EXEC
                WHEN OTHER
                     SET ERRO-PROGRAMADO
                                       TO TRUE
                     MOVE 9006         TO MSG1-CD-ERRO-TS
                     MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE

           .
       982000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em módulos executados mais de uma vez dentro da mesma sessão do*
      * CICS. Equivale ao initialize na área de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*

           MOVE 0019                   TO MSG1-CD-SEQL-ERRO

           MOVE NM-PGM                     TO NM-CTNR(1:8)
           MOVE '-RPST'                    TO NM-CTNR(9:5)

           SET DEL-CONTAINER               TO TRUE

      *    Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e
      *    RESP2   VRS001
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
                    SET ERRO-CONTAINER     TO TRUE
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
           MOVE 0020                   TO MSG1-CD-SEQL-ERRO

      *    Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE 'PRM-ERRO'                 TO NM-CTNR
           SET DEL-CONTAINER               TO TRUE

      *    Não há captura automática de erro CICS por usar RESP e
      *    RESP2   VRS001
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
                    SET ERRO-CONTAINER     TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.
      *----------------------------------------------------------------*
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0021                   TO MSG1-CD-SEQL-ERRO

      *    Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e
      *    RESP2   VRS001
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
       990000-ENCERRAR                 SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                RETURN
           END-EXEC
           .
       990000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
      *                                                                *
      * As sections abaixo iniciadas com 999000 representam os         *
      * tratamentos de erro utilizando o Sistema MSG (rotina MSGS0001) *
      *                                                                *
      *----------------------------------------------------------------*
       998900-ERRO                     SECTION.
      *----------------------------------------------------------------*
           MOVE 0022                   TO MSG1-CD-SEQL-ERRO

           IF MSG1-CD-ERRO-TS EQUAL ZEROS
              MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF

           IF MSG1-TX-ERRO-TS EQUAL SPACES
              MOVE 'Erro não especificado no módulo '
                                       TO MSG1-TX-ERRO-TS
           END-IF

           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       998900-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *     As sections abaixo representam os tratamentos de erro
      *    utilizando o sistema MSG de mensageria (rotina MSGS0001)
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------
           MOVE 0023                   TO MSG1-CD-SEQL-ERRO

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

      *    Adiciona as variáveis de entrada para monitoração
           IF  MNT-VRV
               MOVE 'SGTS0110-CD-CTRA-TIT-VL '
                                           TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0110-CD-CTRA-TIT-VL
                                           TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0110-CD-OPR-ETRD-MCP'
                                           TO MSGKW999-NM-VRV-MNT
               IF  SGTS0110-CD-OPR-ETRD-MCP IS NUMERIC
                  MOVE SGTS0110-CD-OPR-ETRD-MCP
                                           TO CD-OPR-ETRD-MCP-DISPLAY
                  MOVE CD-OPR-ETRD-MCP-DISPLAY
                                           TO MSGKW999-VL-VRV-MNT
               ELSE
                  MOVE 'Código de Operação Estruturada deve ser número.'
                                           TO MSGKW999-VL-VRV-MNT
               END-IF
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0110-NR-VRS-OPR-ETRD'
                                           TO MSGKW999-NM-VRV-MNT
               MOVE SGTS0110-NR-VRS-OPR-ETRD
                                           TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

           END-IF

      *    Grava o erro gerado no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG

           MOVE MSG1-CD-ERRO-TS            TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS            TO IIBKERRO-TX-ERRO

      *    Em caso de abend da MSGS001, adiciona o nome da rotina,
      *    código do erro, retornos cics e nível execução
           IF MSG1-CD-ERRO NOT EQUAL ZEROS
              STRING
                 '(MSG1/' MSG1-CD-ERRO '/' MSG1-RET-RESP '/'
                   MSG1-RET-RESP2  '/' MSG1-NVL-EXEC ') '
                   MSG1-TX-ERRO-TS
                         DELIMITED BY SIZE INTO IIBKERRO-TX-ERRO
              END-STRING
           END-IF

      *    Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

      *    Encerra a transacao
           PERFORM 990000-ENCERRAR
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
      * --------------------------------------*
       COPY MSGKPMG1.
      *---------------------- FIM SGTS0110 -----------------------------
