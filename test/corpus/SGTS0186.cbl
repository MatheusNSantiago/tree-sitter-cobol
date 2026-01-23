      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0186
      * ANALISTA..: F2054109 CINTIA IGLESIAS
      * AUTOR.....: F2054109 CINTIA IGLESIAS
      * DATA......: 11/09/2017
      * OBJETIVO..: Validar Taxas Precificadas
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS002 03/01/2018 F9188847 Acerto LER-CONTAINER-DE-ERRO inclusao
      *        tratamento erro para prg SGT.
      *        20/12/2018 F8090555 Permite taxa zerada.
      * VRS001 11/09/2017 F2054109 DE 2015/137233 - IMPLANTACAO
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS0186.
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
                    '*** S.S. COMEÇA AQUI ***'.

       77  CTE-PROG                      PIC  X(018)  VALUE
                    '*** SGTS0186 ***'.
       77  CTE-VERS                      PIC  X(006)  VALUE 'VRS001'.

      * Modulos acionados
       77  SGTSL508                      PIC  X(008) VALUE 'SGTSL508'.
      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*

       01  VL-4V10                 PIC  --.--9,9999999999.

       01  SGTS0186-IC-TAB             PIC S9(4) COMP-5 VALUE 0.

       01  IC-TAB-PER                  PIC S9(4) COMP-5 VALUE 0.

       01  IDX                         PIC S9(4) COMP-5 VALUE 0.

       01  ERRO-TAB                    PIC  X(01) VALUE SPACES.

      * Book das variáveis auxiliares do tratamento de erro via MSG
       COPY SGTKERR1.                                                           0000
      * Book para captura de abend cics da bloco codigo MSGKP999
       COPY HLPKW999.                                                           0000
      * Book para captura de erro via MSG1
       COPY MSGKWMG1.                                                           0000

      * Book do container de requisicao e resposta deste modulo
       COPY SGTK0186.

       COPY SGTKL508.
      * Book do Container de Erro
       COPY IIBKERRO.
                                                                         VRS001
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
       COPY HLPKE999.                                                           0000

           PERFORM 970000-LER-CANAL

      *    Define o nome do programa p/ fins manipulação container
           MOVE 'SGTS0186'             TO NM-PGM

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

           IF  SGTS0186-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0002                TO MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -        ' não informado.'        TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0186-CD-CTRA-TIT-VL LESS ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0001                TO MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -        ' inválido.'             TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0186-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0003                TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturado mercado de capitais não
      -        ' numérico.'             TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0186-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0005                TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturado mercado de capitais não
      -        ' informado.'            TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0186-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0007                TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0186-NR-VRS-OPR-ETRD LESS ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0006                TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0186-CD-TIP-TAXA NOT EQUAL 1 AND 2
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0167                TO MSG1-CD-ERRO-TS
               MOVE 'Código do tipo da taxa inválido.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           PERFORM VARYING IDX FROM 1 BY 1
                   UNTIL IDX GREATER SGTS0186-QT-PER
               IF  SGTS0186-DT-INC-TAXA (IDX)  EQUAL  SPACES
                   SET  ERRO-PROGRAMADO     TO TRUE
                   MOVE 0469                TO MSG1-CD-ERRO-TS
                   MOVE 'Data início da taxa não informada.'
                                            TO MSG1-TX-ERRO-TS
                   MOVE 'S'                 TO ERRO-TAB
                   PERFORM 999000-ERRO
              END-IF

              IF  SGTS0186-DT-FIM-TAXA (IDX)  EQUAL  SPACES
                   SET  ERRO-PROGRAMADO     TO TRUE
                   MOVE 0470                TO MSG1-CD-ERRO-TS
                   MOVE 'Data fim da taxa não informada.'
                                            TO MSG1-TX-ERRO-TS
                   MOVE 'S'                 TO ERRO-TAB
                   PERFORM 999000-ERRO
              END-IF

              IF  SGTS0186-PC-TAXA (IDX)  NOT NUMERIC
                   SET  ERRO-PROGRAMADO     TO TRUE
                   MOVE 0471                TO MSG1-CD-ERRO-TS
                   MOVE 'Percentual da taxa não numérico.'
                                            TO MSG1-TX-ERRO-TS
                   MOVE 'S'                 TO ERRO-TAB
                   PERFORM 999000-ERRO
              END-IF

      *        IF  SGTS0186-PC-TAXA (IDX)  EQUAL  ZEROS
      *             SET  ERRO-PROGRAMADO     TO TRUE
      *             MOVE 0472                TO MSG1-CD-ERRO-TS
      *             MOVE 'Percentual da taxa não informado.'
      *                                      TO MSG1-TX-ERRO-TS
      *             MOVE 'S'                 TO ERRO-TAB
      *             PERFORM 999000-ERRO
      *        END-IF

              IF  SGTS0186-PC-TAXA (IDX)  LESS  ZEROS
                   SET  ERRO-PROGRAMADO     TO TRUE
                   MOVE 0472                TO MSG1-CD-ERRO-TS
                   MOVE 'Percentual da taxa inválido.'
                                            TO MSG1-TX-ERRO-TS
                   MOVE 'S'                 TO ERRO-TAB
                   PERFORM 999000-ERRO
              END-IF
           END-PERFORM
           .
       110000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS TO   IC-TAB-PER
           PERFORM 210000-VALIDAR-TAXAS
                   VARYING IC-TAB-PER FROM 1 BY 1
                   UNTIL IC-TAB-PER > SGTS0186-QT-PER

           MOVE   SGTS0186-IC-TAB TO SGTS0186-QT-OCR-TAB
           PERFORM 980000-GRAVAR-CONTAINER-RPST
           .
       200000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       210000-VALIDAR-TAXAS       SECTION.
      *----------------------------------------------------------------*
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           PERFORM 211000-LISTAR-PREF

           PERFORM 212000-TRATAR-PREF
                   VARYING SGTSL508-IC-TAB FROM 1 BY 1
                   UNTIL SGTSL508-IC-TAB > SGTSL508-QT-OCR-TAB
           .
       210000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       211000-LISTAR-PREF       SECTION.
      *----------------------------------------------------------------*
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSL508               TO NM-PGM

           MOVE SGTS0186-CD-CTRA-TIT-VL     TO SGTSL508-CD-CTRA-TIT-VL
           MOVE SGTS0186-CD-OPR-ETRD-MCP    TO SGTSL508-CD-OPR-ETRD-MCP
           MOVE SGTS0186-NR-VRS-OPR-ETRD    TO SGTSL508-NR-VRS-OPR-ETRD
           MOVE SGTS0186-CD-TIP-TAXA        TO SGTSL508-CD-TIP-TAXA
           MOVE CORR SGTS0186-DT-INC-TAXA-R(IC-TAB-PER)
                                            TO SGTSL508-DT-INC-PSQ-R
           MOVE CORR SGTS0186-DT-FIM-TAXA-R(IC-TAB-PER)
                                            TO SGTSL508-DT-FIM-PSQ-R
           MOVE SPACES                      TO SGTSL508-DT-ULT-INC

           PERFORM 900000-EXECUTAR-MODULO

           IF  SGTSL508-QT-OCR-TAB     EQUAL ZEROS AND
               SGTS0186-CD-TIP-CBT (IC-TAB-PER) NOT EQUAL 4
                ADD 1  TO  SGTS0186-IC-TAB
                MOVE CORR SGTS0186-DT-INC-TAXA-R(IC-TAB-PER)
                        TO SGTS0186-DT-INC-CND-TAXA-R(SGTS0186-IC-TAB)

                MOVE CORR SGTS0186-DT-FIM-TAXA-R(IC-TAB-PER)
                        TO SGTS0186-DT-FIM-CND-TAXA-R(SGTS0186-IC-TAB)

                MOVE ZEROS TO SGTS0186-PC-TAXA(SGTS0186-IC-TAB)
                MOVE 'Não existe precificação para o período.' TO
                                 SGTS0186-TX-DCR-DFRC(SGTS0186-IC-TAB)
           END-IF
           .
       211000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       212000-TRATAR-PREF       SECTION.
      *----------------------------------------------------------------*
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           IF SGTS0186-CD-TIP-CBT(IC-TAB-PER) EQUAL 4
               MOVE ZEROS TO SGTS0186-PC-TAXA(IC-TAB-PER)
           END-IF
           IF  SGTSL508-PC-MIN-TAXA(SGTSL508-IC-TAB) >
                             SGTS0186-PC-TAXA(IC-TAB-PER)
                ADD 1  TO  SGTS0186-IC-TAB
                MOVE    'Taxa precificada maior que taxa informada.'
                       TO SGTS0186-TX-DCR-DFRC(SGTS0186-IC-TAB)
                MOVE CORR SGTSL508-DT-INC-CND-TAXA-R(SGTSL508-IC-TAB)
                       TO SGTS0186-DT-INC-CND-TAXA-R(SGTS0186-IC-TAB)
                MOVE CORR SGTSL508-DT-FIM-CND-TAXA-R(SGTSL508-IC-TAB)
                       TO SGTS0186-DT-FIM-CND-TAXA-R(SGTS0186-IC-TAB)
                MOVE SGTSL508-PC-MIN-TAXA(SGTSL508-IC-TAB)  TO
                             SGTS0186-PC-CND-TAXA(SGTS0186-IC-TAB)
           END-IF

           .
       212000-SAI.
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
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

      *    Grava o container de requisição do módulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM
               WHEN SGTSL508
                   EXEC CICS LINK
                             PROGRAM ( SGTSL508  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN OTHER
                   MOVE 9006          TO MSG1-CD-ERRO-TS
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

      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*

           EVALUATE NM-PGM
           WHEN SGTSL508
                MOVE 'SGTSL508-CD-CTRA-TIT-VL'   TO MSGKW999-NM-VRV-MNT
                MOVE SGTSL508-CD-CTRA-TIT-VL     TO MSGKW999-NM-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSL508-CD-OPR-ETRD-MCP'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSL508-CD-OPR-ETRD-MCP    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSL508-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSL508-NR-VRS-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSL508-CD-TIP-TAXA'      TO MSGKW999-NM-VRV-MNT
                MOVE SGTSL508-CD-TIP-TAXA        TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSL508-DT-INC-PSQ'       TO MSGKW999-NM-VRV-MNT
                MOVE SGTSL508-DT-INC-PSQ         TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSL508-DT-FIM-PSQ'       TO MSGKW999-NM-VRV-MNT
                MOVE SGTSL508-DT-FIM-PSQ         TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSL508-DT-ULT-INC'       TO MSGKW999-NM-VRV-MNT
                MOVE SGTSL508-DT-ULT-INC         TO MSGKW999-VL-VRV-MNT
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
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisicao
           MOVE 'SGTS0186'             TO NM-PGM
           MOVE 'SGTS0186-RQSC'        TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisicao
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0186-RQSC)
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
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

      *    Le o container de erro dos módulos chamados
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
      *        PARA O MODULO CHAMADO e tratar na section chamadora
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
                      SET ERRO-PROGRAMA-ACIONADO TO TRUE
                      MOVE 3004               TO MSG1-CD-ERRO-TS
                      MOVE 'Erro ao acionar o módulo.'
                                              TO MSG1-TX-ERRO-TS
                      MOVE IIBKERRO-TX-ERRO   TO MSG1-TX-RTN-PGM-ACND
                      MOVE IIBKERRO-CD-ERRO   TO MSG1-CD-RTN-PGM-ACND
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
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome dos container de acordo com módulo a executar
           MOVE SPACES                 TO NM-CTNR

           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RPST'       TO NM-CTNR(9:5)

      *    Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

      *Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Lê a RESPOSTA de acordo com o programa que a gravou

           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM

      *        Calcular a quantidade de caracteres esperado e limpar
      *        a area de dados antes de executar o GET
      *        Para matrizes com dados variaveis (clausula depending
      *        on) nao ha como utilizar o comando INITIALIZE

               WHEN SGTSL508
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL508-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN OTHER
                    MOVE 9006          TO MSG1-CD-ERRO-TS
                    STRING
                        'Leitura não definida para resposta do módulo '
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
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

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

      *----------------------------------------------------------------*  VRS001
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
      *                                                                *
      *----------------------------------------------------------------*
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *----------------------------------------------------------------*

      *    Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de resposta
           MOVE 'SGTS0186-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                )
                     FROM      (SGTS0186-RPST          )
                     FLENGTH   (LENGTH OF SGTS0186-RPST)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *----------------------------------------------------------------*
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RQSC'       TO NM-CTNR(9:5)

      *Por causa da captura automatica de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisção do programa a executar
           EVALUATE NM-PGM
               WHEN SGTSL508
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL508-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL508-RQSC )
                   END-EXEC
               WHEN OTHER
                    MOVE 9006          TO MSG1-CD-ERRO-TS
                    STRING
                      'Gravação não definida para requisição do módulo '
                      NM-PGM   DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    END-STRING
                    PERFORM 999000-ERRO
           END-EVALUATE

      *Ao chegar aqui, ha certeza de que nao houve erro algum
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
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RPST'       TO NM-CTNR(9:5)

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
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO
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
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO

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
           IF  MNT-VRV
               MOVE 'SGTS0186-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0186-CD-CTRA-TIT-VL     TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0186-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0186-CD-OPR-ETRD-MCP    TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0186-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0186-NR-VRS-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0186-CD-TIP-TAXA'       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0186-CD-TIP-TAXA        TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0186-QT-PER'            TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0186-QT-PER             TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               IF  ERRO-TAB  EQUAL  'S'
                   MOVE 'IDX'                    TO MSGKW999-NM-VRV-MNT
                   MOVE  IDX                     TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT

                   MOVE 'SGTS0186-DT-INC-TAXA'   TO MSGKW999-NM-VRV-MNT
                   MOVE  SGTS0186-DT-INC-TAXA(IDX)
                                                 TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT

                   MOVE 'SGTS0186-DT-FIM-TAXA'   TO MSGKW999-NM-VRV-MNT
                   MOVE  SGTS0186-DT-FIM-TAXA(IDX)
                                                 TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT

                   MOVE 'SGTS0186-PC-TAXA'       TO MSGKW999-NM-VRV-MNT
                   MOVE  SGTS0186-PC-TAXA(IDX)   TO VL-4V10
                   MOVE VL-4V10                  TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT
               END-IF
           END-IF

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
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      * --------------------------------------*
       COPY HLPKP999.                                                           0000
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
       COPY SGTKP999.                                                           0000
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
      *----------------------------------------------------------------*
       999999-ENCERRAR                 SECTION.
      *----------------------------------------------------------------*

           GOBACK
           .
       999999-SAI.
           EXIT.                                                             000
      *---------------------- FIM SGTS0186 -----------------------------
