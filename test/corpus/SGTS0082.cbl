      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0082
      * ANALISTA..: F0175702 ADRIANO SEIJI NAKAMURA
      * AUTOR.....: F0175702 ADRIANO SEIJI NAKAMURA
      * DATA......: 16/11/2016
      * OBJETIVO..: Listar Anexos de uma Operação Estruturada de Mercado
      *             de Capitais
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 16/11/2016 F0175702 DE 2015/137233 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS0082.
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

       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

       77  CTE-INICIO-SS                 PIC  X(040)  VALUE
                    '*** S.S. COMEÇA AQUI ***'.

       77  CTE-PROG                      PIC  X(018)  VALUE
                    '*** SGTS0082 ***'.
       77  CTE-VERS                      PIC  X(006)  VALUE 'VRS001'.

      * Módulos acionados
       77  IIBP2000                      PIC  X(008) VALUE 'IIBP2000'.
       77  SGTSL060                      PIC  X(008) VALUE 'SGTSL060'.
       77  ARHPI40D                      PIC  X(008) VALUE 'ARHPI40D'.
      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*

      * Areas de trabalho

       01  CD-CHAVE-FUN                PIC  X(08) VALUE ' '.
       01  FILLER                      REDEFINES  CD-CHAVE-FUN.
           05  FILLER                  PIC  X(01).
           05  CD-CHAVE-FUN-NR         PIC  9(07).

       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *----------------------------------------------------------------*
      * Book dos Containers e das subrotinas chamadas
      *----------------------------------------------------------------*

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1                                                           00000610
      * Book para captura de abend cics da bloco código MSGKP999
      * -INC MSGKW999 (não localizado na compilação)
-INC HLPKW999                                                           00000630
      * Book para captura de erro via MSG1
-INC MSGKWMG1                                                           00000660

      * Book do container de requisição e resposta deste módulo
-INC SGTK0082

      * Book para consulta do nome do funcionário
-INC ARHKI40D

      * Listar Anexo da Operação
-INC SGTKL060
      * Book do Container de Erro
-INC IIBKERRO
      * Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC
      * Book dos dados do usuário                                         VRS001

       01  CTE-FINAL-SS                  PIC X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*

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
-INC HLPKE999                                                           00001140

           PERFORM 970000-LER-CANAL

      *    Define o nome do programa p/ fins manipulação container
           MOVE 'SGTS0082'             TO NM-PGM

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

      *    Valida campo a campo da requisição

           IF  SGTS0082-CD-CTRA-TIT-VL < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'inválido.'        TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0082-CD-CTRA-TIT-VL EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0002               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'   TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0082-CD-OPR-ETRD-MCP NOT NUMERIC
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0082-CD-OPR-ETRD-MCP LESS ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0004               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais inv
      -             'álido.'           TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0082-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0005               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' informado.'      TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0082-NR-VRS-OPR-ETRD LESS ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0006               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0082-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0007               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF
           .
       110000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-PESQ-ANEXOS-OPR

           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           MOVE  SGTSL060-IN-CTNC-PSQ  TO SGTS0082-IN-CTNC-PSQ
           MOVE  SGTSL060-QT-OCR-TAB   TO SGTS0082-QT-OCR-TAB

           IF  SGTS0082-QT-OCR-TAB  GREATER ZEROS
               PERFORM 215000-CARREGAR-MATRICULA
                 VARYING SGTSL060-IC-TAB FROM 1 BY 1
                 UNTIL SGTSL060-IC-TAB GREATER SGTSL060-QT-OCR-TAB

               PERFORM 216000-CONSULTAR-NOME

               PERFORM 220000-PROCESSAR-LISTAR-ANEXOS
                 VARYING SGTSL060-IC-TAB FROM 1 BY 1
                 UNTIL SGTSL060-IC-TAB GREATER SGTSL060-QT-OCR-TAB
           END-IF

           PERFORM 980000-GRAVAR-CONTAINER-RPST
           .
       200000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       215000-CARREGAR-MATRICULA    SECTION.
      *----------------------------------------------------------------*
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTS0082-CD-USU-ULT-ATL(SGTSL060-IC-TAB)
                                       TO CD-CHAVE-FUN
           MOVE CD-CHAVE-FUN-NR        TO
                 ARHPI40D-CD-MTC-FUN(SGTSL060-IC-TAB)
           .
       215000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       216000-CONSULTAR-NOME    SECTION.
      *----------------------------------------------------------------*
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar o nome do programa a ser executado
            MOVE ARHPI40D               TO NM-PGM

            PERFORM 910000-CHAMAR-SERVICO
           .
       216000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       210000-PESQ-ANEXOS-OPR       SECTION.
      *----------------------------------------------------------------*
      * Pesquisa se há vinculação do Arquivo com outra Oper Estrut
      *----------------------------------------------------------------*
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSL060                    TO NM-PGM

           MOVE  SGTS0082-CD-CTRA-TIT-VL    TO SGTSL060-CD-CTRA-TIT-VL
           MOVE  SGTS0082-CD-TIP-ARQ        TO SGTSL060-CD-TIP-ARQ
           MOVE  SGTS0082-CD-OPR-ETRD-MCP   TO SGTSL060-CD-OPR-ETRD-MCP
           MOVE  SGTS0082-NR-VRS-OPR-ETRD   TO SGTSL060-NR-VRS-OPR-ETRD
           MOVE  SGTS0082-E-CD-ARQ-OPR-ETRD TO SGTSL060-CD-ARQ-OPR-ULT

           PERFORM 900000-EXECUTAR-MODULO
           .
       210000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       220000-PROCESSAR-LISTAR-ANEXOS        SECTION.
      *----------------------------------------------------------------*
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE  SGTSL060-CD-ARQ-OPR-ETRD(SGTSL060-IC-TAB)  TO
                             SGTS0082-CD-ARQ-OPR-ETRD(SGTSL060-IC-TAB)
           MOVE  SGTSL060-CD-USU-ULT-ATL(SGTSL060-IC-TAB)    TO
                             SGTS0082-CD-USU-ULT-ATL(SGTSL060-IC-TAB)
           MOVE  SGTSL060-TX-DCR-ANX(SGTSL060-IC-TAB)  TO
                             SGTS0082-TX-DCR-ANX(SGTSL060-IC-TAB)
           MOVE  SGTSL060-TS-ULT-ATL(SGTSL060-IC-TAB)  TO
                             SGTS0082-TS-ULT-ATL(SGTSL060-IC-TAB)
           MOVE  SGTSL060-TX-DCR-ANX(SGTSL060-IC-TAB)  TO
                             SGTS0082-TX-DCR-ANX(SGTSL060-IC-TAB)
           MOVE  SGTSL060-TX-DCR-TIP-ARQ(SGTSL060-IC-TAB)  TO
                             SGTS0082-TX-DCR-TIP-ARQ(SGTSL060-IC-TAB)
           MOVE ARHPI40D-NM-FUN(SGTSL060-IC-TAB) TO
                             SGTS0082-NM-USU-ULT-ATL(SGTSL060-IC-TAB)
           .
       220000-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *----------------------------------------------------------------*
      * Executa um modulo implementado com CONTAINER                   *
      * Nao incluir nessa section os modulos chamados via COMMAREA     *
      *                                                                *
      *----------------------------------------------------------------*
       900000-EXECUTAR-MODULO          SECTION.
      *----------------------------------------------------------------*
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

      *    Grava o container de requisição do módulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM
               WHEN SGTSL060
                   EXEC CICS LINK
                             PROGRAM ( SGTSL060  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN OTHER
                   MOVE 9006           TO  MSG1-CD-ERRO-TS
                   STRING
                       'Não definida a execução do módulo ' NM-PGM
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                   END-STRING
                   PERFORM 999000-ERRO
           END-EVALUATE
      *
           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    STRING
                          'Erro na execução do módulo ' NM-PGM ', '
                          'RESP  ='   ' ' MSG1-RESP1  ', '
                          'RESP2 =' ' ' MSG1-RESP2
                          DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    END-STRING
                    MOVE 3004          TO MSG1-CD-ERRO-TS
                    SET ERRO-CICS      TO TRUE
                    PERFORM 999000-ERRO
           END-EVALUATE

      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *    Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.

      *---------------------------------------------------------------*   VRS001
       910000-CHAMAR-SERVICO           SECTION.                           VRS001
      *---------------------------------------------------------------*   VRS001
      *    Acionar programa IIBP2000 para requisição do serviço.          VRS001
      *---------------------------------------------------------------*   VRS001
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *    Grava o container com dados do serviço/operação a chamar       VRS001
           PERFORM 981000-GRAVAR-CONTAINER-SRVC                           VRS001
                                                                          VRS001
      *    Grava o container de requisição do módulo a executar           VRS001
           PERFORM 982000-GRAVAR-CONTAINER-RQSC                           VRS001
                                                                          VRS001
      *    Limpa a resposta e erro antes de executar o módulo             VRS001
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO                           VRS001
                                                                          VRS001
      *    Não há captura automática de erro CICS por usar RESP e RESP2   VRS001
           EXEC CICS LINK                                                 VRS001
                     PROGRAM   ( IIBP2000     )                           VRS001
                     CHANNEL   ( NM-CNL       )                           VRS001
                     RESP      ( MSG1-RESP1   )                           VRS001
                     RESP2     ( MSG1-RESP2   )                           VRS001
           END-EXEC                                                       VRS001
                                                                          VRS001
           EVALUATE TRUE                                                  VRS001
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)               VRS001
                    CONTINUE                                              VRS001
               WHEN OTHER
                    MOVE 9000          TO MSG1-CD-ERRO-TS                 VRS001
                    STRING 'Erro ao acionar serviço: '                    VRS001
                           SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'             VRS001
                           SRVC-ID-OPR  '.' SRVC-VRS-OPR                  VRS001
                           'via IIBP2000, '                               VRS001
                           'RESP=' MSG1-RESP1 ', RESP-2=' MSG1-RESP2      VRS001
                           '.'  DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS    VRS001
                    END-STRING
                    SET ERRO-CICS      TO TRUE                            VRS001
                    PERFORM 999000-ERRO                                   VRS001
           END-EVALUATE                                                   VRS001
                                                                          VRS001
      *    Efetua a leitura do Container de Erro                          VRS001
           PERFORM 972000-LER-CONTAINER-DE-ERRO                           VRS001
                                                                          VRS001
      *    Efetua a leitura do Container de Resposta                      VRS001
           PERFORM 973000-LER-RESPOSTA                                    VRS001
           .                                                              VRS001
       910000-SAI.                                                        VRS001
           EXIT.

      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM
           WHEN SGTSL060

                MOVE 'SGTSL060-CD-CTRA-TIT-VL'   TO MSGKW999-NM-VRV-MNT
                MOVE SGTSL060-CD-CTRA-TIT-VL     TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSL060-CD-OPR-ETRD-MCP'  TO MSGKW999-NM-VRV-MNT
                IF  SGTSL060-CD-OPR-ETRD-MCP     IS NUMERIC
                    MOVE  SGTSL060-CD-OPR-ETRD-MCP  TO  CD-OPR-ETRD-MCP
                    MOVE  CD-OPR-ETRD-MCP-TX     TO  MSGKW999-VL-VRV-MNT
                ELSE
                    MOVE  'Valor não numérico.'  TO  MSGKW999-VL-VRV-MNT
                END-IF
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSL060-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSL060-NR-VRS-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSL060-CD-ARQ-OPR-ULT'   TO MSGKW999-NM-VRV-MNT
                MOVE SGTSL060-CD-ARQ-OPR-ULT     TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSL060-CD-TIP-ARQ'       TO MSGKW999-NM-VRV-MNT
                MOVE SGTSL060-CD-TIP-ARQ         TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT


           WHEN ARHPI40D
               IF  SGTSL060-IC-TAB GREATER ZEROS AND
                   SGTSL060-IC-TAB NOT GREATER SGTSL060-QT-OCR-TAB
                   MOVE 'SGTSL060-IC-TAB'        TO MSGKW999-NM-VRV-MNT
                   MOVE SGTSL060-IC-TAB          TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT

                   MOVE 'ARHPI40D-CD-MTC-FUN'    TO MSGKW999-NM-VRV-MNT
                   MOVE ARHPI40D-CD-MTC-FUN(SGTSL060-IC-TAB)
                                                 TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT
               END-IF

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
           MOVE 'SGTS0082'             TO NM-PGM
           MOVE 'SGTS0082-RQSC'        TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      *    Obtem o tamanho esperado da requisição
           MOVE LENGTH OF SGTS0082-RQSC TO QT-CCT-EPRO

      *    Valida tamanho esperado da requisição X tamanho recebido

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA   TO TRUE
               PERFORM 999000-ERRO
           END-IF

           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR      )
                     FLENGTH   (QT-CCT-LIDO  )
                     INTO      (SGTS0082-RQSC)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       971000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *----------------------------------------------------------------*
      *    A presenca do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *----------------------------------------------------------------*
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

      *    Lê o container de erro dos módulos chamados
           SET GET-CONTAINER           TO TRUE
           MOVE 'PRM-ERRO'             TO NM-CTNR
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
                    SET   ERRO-PROGRAMA-ACIONADO TO  TRUE
                    MOVE  NM-PGM      TO  MSG1-NM-PGM-ACND
                    IF  NM-PGM(1:3) EQUAL 'SGT'
                        MOVE  IIBKERRO-CD-ERRO TO  MSG1-CD-RTN-PGM-ACND
                                                   MSG1-CD-ERRO-TS
                        MOVE  IIBKERRO-TX-ERRO TO  MSG1-TX-RTN-PGM-ACND
                                                   MSG1-TX-ERRO-TS
                    ELSE
                        MOVE 3004              TO MSG1-CD-ERRO-TS
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
                    SET ERRO-CONTAINER  TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       973000-LER-RESPOSTA             SECTION.
      *----------------------------------------------------------------*
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome dos container de acordo com módulo a executar
           MOVE SPACES                     TO NM-CTNR

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           EVALUATE NM-PGM
               WHEN ARHPI40D
                    MOVE 'PRM-DADOS-RPST'  TO NM-CTNR
               WHEN OTHER
                    MOVE NM-PGM            TO NM-CTNR(1:8)
                    MOVE '-RPST'           TO NM-CTNR(9:5)
           END-EVALUATE

      *    Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

      *Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Lê a RESPOSTA de acordo com o programa que a gravou

           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM

      *        Calcular a quantidade de caracteres esperado e limpar
      *        a área de dados antes de executar o GET
      *        Para matrizes com dados variáveis (clausula depending
      *        on) não há como utilizar o comando INITIALIZE

               WHEN ARHPI40D
                   EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (ARHPI40D-RPST )
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL060
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL060-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN OTHER
                    MOVE  9003         TO  MSG1-CD-ERRO-TS
                    STRING
                        'Erro na leitura da resposta do módulo '
                        NM-PGM DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    END-STRING
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
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

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
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
      *                                                                *
      *----------------------------------------------------------------*
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *----------------------------------------------------------------*

      *    Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de resposta
           MOVE 'SGTS0082-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                )
                     FROM      (SGTS0082-RPST          )
                     FLENGTH   (LENGTH OF SGTS0082-RPST)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.

      *---------------------------------------------------------------*   VRS001
       981000-GRAVAR-CONTAINER-SRVC    SECTION.                           VRS001
      *---------------------------------------------------------------*   VRS001
      *    Postar container de requisição de serviço.                     VRS001
      *---------------------------------------------------------------*   VRS001
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *    Monta o nome do container de requisição do serviço             VRS001
           MOVE 'PRM-RQSC-SRVC'        TO NM-CTNR                         VRS001
           SET PUT-CONTAINER           TO TRUE                            VRS001

      *    Serviço do programa ARHS0882                                   VRS001
           MOVE 0             TO SRVC-ID-SRVC                             VRS001
           MOVE 0             TO SRVC-VRS-SRVC                            VRS001
           MOVE 2418135        TO SRVC-ID-OPR                              VRS00
           MOVE 1             TO SRVC-VRS-OPR                             VRS001
                                                                          VRS001
      *Por causa da captura automática de erro CICS, deve ser previsto    VRS001
      *o erro de container                                                VRS001
           SET  ERRO-CONTAINER         TO TRUE                            VRS001
                                                                          VRS001
           EXEC CICS PUT                                                  VRS001
                     CONTAINER ( NM-CTNR                     )            VRS001
                     FROM      ( SRVC-PRM-RQSC-SRVC          )            VRS001
                     FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC)            VRS001
           END-EXEC                                                       VRS001
                                                                          VRS001
      *Ao chegar aqui, há certeza de que não houve erro algum             VRS001
           SET  SEM-ERRO               TO TRUE                            VRS001
           .                                                              VRS001
       981000-SAI.                                                        VRS001
           EXIT.

      *----------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *----------------------------------------------------------------*
           MOVE 0020                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome dos container de acordo com módulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RQSC
           EVALUATE NM-PGM
                WHEN ARHPI40D
                     MOVE 'PRM-DADOS-RQSC'  TO NM-CTNR
                WHEN OTHER
                    MOVE NM-PGM             TO NM-CTNR(1:8)
                    MOVE '-RQSC'            TO NM-CTNR(9:5)
            END-EVALUATE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisição do programa a executar
           EVALUATE NM-PGM
               WHEN ARHPI40D
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (ARHPI40D-RQSC           )
                              FLENGTH   (LENGTH OF ARHPI40D-RQSC )
                    END-EXEC

               WHEN SGTSL060
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL060-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL060-RQSC )
                    END-EXEC

               WHEN OTHER
                    MOVE  9004              TO  MSG1-CD-ERRO-TS
                    STRING
                      'Erro na gravação da requisição do módulo '
                       NM-PGM  DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    END-STRING
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
      * em módulos executados mais de uma vez dentro da mesma sessão do*
      * CICS. Equivale ao initialize na área de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
           MOVE 0021                   TO MSG1-CD-SEQL-ERRO

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           EVALUATE NM-PGM
               WHEN ARHPI40D
                    MOVE 'PRM-DADOS-RPST'  TO NM-CTNR
               WHEN OTHER
                    MOVE NM-PGM            TO NM-CTNR(1:8)
                    MOVE '-RPST'           TO NM-CTNR(9:5)
           END-EVALUATE

           SET DEL-CONTAINER           TO TRUE

      *    Exclui o container de resposta
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

      *    Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *----------------------------------------------------------------*

      *    Limpar variáveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE 0022                   TO MSG1-CD-SEQL-ERRO
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

      *----------------------------------------------------------------*
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0023                   TO MSG1-CD-SEQL-ERRO

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
      *
      *----------------------------------------------------------------*
      *                                                                *
      * As sections abaixo iniciadas com 999000 representam os tratamen*
      * tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *
      *                                                                *
      *----------------------------------------------------------------*
       999000-ERRO                     SECTION.
      *----------------------------------------------------------------*

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS EQUAL SPACES
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
           MOVE  'SGTS0082-CD-CTRA-TIT-VL'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0082-CD-CTRA-TIT-VL      TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0082-CD-OPR-ETRD-MCP'    TO MSGKW999-NM-VRV-MNT
           IF  SGTS0082-CD-OPR-ETRD-MCP       IS NUMERIC
               MOVE SGTS0082-CD-OPR-ETRD-MCP  TO CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX       TO MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Valor não numérico.'     TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0082-NR-VRS-OPR-ETRD'    TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0082-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0082-CD-TIP-ARQ'         TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0082-CD-TIP-ARQ          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0082-E-CD-ARQ-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0082-E-CD-ARQ-OPR-ETRD   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

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
-INC HLPKP999                                                           00009600
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999                                                           00009660
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

           GOBACK
           .
       999999-SAI.
           EXIT.                                                        00009700
      *---------------------- FIM SGTS0082 -----------------------------