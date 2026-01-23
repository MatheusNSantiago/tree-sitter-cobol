      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0192
      * ANALISTA..: F2054109 CINTIA IGLESIAS
      * AUTOR.....: F2054109 CINTIA IGLESIAS
      * DATA......: 23/10/2017
      * OBJETIVO..: Programa de negócio para desfazimento de exclusão de
      *             anexo - Utilizado quando ocorre problema na integra-
      *             ção com GED
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 25/06/2018 F8658869 DE 2016/145195 - Incluir carteira
      *                   para SGTS0081
      *        23/10/2017 F2054109 DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS0192.
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
                    '*** SGTS0192 ***'.
       77  CTE-VERS                      PIC  X(006)  VALUE 'VRS001'.

      * Modulos acionados
       77  SGTSI060                      PIC  X(008) VALUE 'SGTSI060'.

       77  SGTS0081                      PIC  X(008) VALUE 'SGTS0081'.


      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*
      * Areas de trabalho
       01 IN-EXITENCIA-REGISTRO            PIC X(1).
          88 REGISTRO-NAO-EXISTE           VALUE  'N'.
          88 REGISTRO-EXISTE               VALUE  'S'.

       01  QT-REG-SEL                      PIC S9(4) COMP-5 VALUE 0.

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      * Book dos Containers e das subrotinas chamadas
      *----------------------------------------------------------------*

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1                                                           00000610
      * Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999                                                           00000630
      * Book para captura de erro via MSG1
-INC MSGKWMG1                                                           00000660

      * Book do container de requisicao e resposta deste modulo
       01 SGTS0192-DADOS.
-INC SGTK0192

-INC SGTK0081

-INC SGTKI060

      * Book do Container de Erro
-INC IIBKERRO
      * Book do container de Serviços (para acionamento de serviços)

      * Book dos dados do usuário
-INC IIBKOPRD

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

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

      *    Ativa a captura automatica de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999                                                           00001140

           PERFORM 970000-LER-CANAL

      *    Define o nome do programa p/ fins manipulação container
           MOVE 'SGTS0192'             TO NM-PGM

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

           PERFORM 974000-LER-DADOS-OPRD

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       110000-VALIDAR-REQUISICAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           IF  SGTS0192-CD-CTRA-TIT-VL     LESS ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 001                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'inválido.'            TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0192-CD-CTRA-TIT-VL     EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 002                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0192-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 003                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturado mercado de capitais não
      -        ' numérico.'                TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0192-CD-OPR-ETRD-MCP    LESS  ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 004                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturado mercado de capitais inv
      -        'álido.'                    TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0192-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 005                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturado mercado de capitais não
      -        ' informado.'               TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0192-NR-VRS-OPR-ETRD    LESS ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 006                    TO  MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0192-NR-VRS-OPR-ETRD    EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 007                    TO  MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0192-CD-ARQ-OPR-ETRD-ANT    LESS ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 150                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código arquivo da operação estruturada inválido.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0192-CD-ARQ-OPR-ETRD-ANT    EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 149                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código arquivo da operação estruturada não informad
      -             'o.'                   TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0192-CD-TIP-ARQ-ANT    LESS ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 322                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código do tipo de arquivo inválido.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0192-CD-TIP-ARQ-ANT    EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 323                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código do tipo de arquivo não informado.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           .
       110000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-VERIFICAR-PMS-ALT

           PERFORM 220000-CONSULTA-ARQUIVO

           IF REGISTRO-EXISTE
              PERFORM 230000-CONSULTA-ANEXO-OPR
              IF REGISTRO-EXISTE
                 SET  ERRO-PROGRAMADO        TO  TRUE
                 MOVE 467                    TO  MSG1-CD-ERRO-TS
                 MOVE 'Erro no desfazimento de exclusão de anexo para re
      -             'gistro já existente.'   TO  MSG1-TX-ERRO-TS
                 PERFORM 999000-ERRO
              ELSE
                PERFORM 250000-GRAVA-ANEXO-OPR
              END-IF
           ELSE

      *Não deve ser tratada a inclusão da ARQ_OPR_ETRD pois
      *no processo de desfazimento da exclusão do anexo,
      *deve ser mantido o mesmo CD_ARQ_OPR_ETRD

              SET  ERRO-PROGRAMADO        TO  TRUE
              MOVE 468                    TO  MSG1-CD-ERRO-TS
              MOVE 'Desfazimento não permitido para arquivo não encontra
      -            'do.'                  TO  MSG1-TX-ERRO-TS
              PERFORM 999000-ERRO

           END-IF
           .
       200000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       210000-VERIFICAR-PMS-ALT        SECTION.
      *Verificar se Anexo Pode ser alterado.
      *----------------------------------------------------------------*
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE         SGTS0081-DADOS

            MOVE SGTS0081              TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTS0192-CD-CTRA-TIT-VL    TO SGTS0081-CD-CTRA-TIT-VL
           MOVE SGTS0192-CD-OPR-ETRD-MCP   TO SGTS0081-CD-OPR-ETRD
           MOVE SGTS0192-NR-VRS-OPR-ETRD   TO SGTS0081-NR-VRS-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO

           IF SGTS0081-IN-HBLT-MENU-ANX EQUAL 'N'
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0415               TO MSG1-CD-ERRO-TS
               MOVE 'Exclusão não permitida para o estado da operação.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF
           .
       210000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       220000-CONSULTA-ARQUIVO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           SET REGISTRO-NAO-EXISTE TO TRUE

           EXEC SQL
                 SELECT 1
                   INTO  :QT-REG-SEL
                   FROM DB2SGT.ARQ_OPR_ETRD
                WHERE CD_ARQ_OPR_ETRD    = :SGTS0192-CD-ARQ-OPR-ETRD-ANT
           END-EXEC

           IF  QT-REG-SEL GREATER ZEROS
               SET REGISTRO-EXISTE TO TRUE
           END-IF
           .
       220000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       230000-CONSULTA-ANEXO-OPR       SECTION.
      *----------------------------------------------------------------*
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           SET REGISTRO-NAO-EXISTE TO  TRUE
           MOVE ZEROS              TO  QT-REG-SEL

           EXEC SQL
                 SELECT 1
                   INTO  :QT-REG-SEL
                       FROM DB2SGT.ANX_OPR_ETRD
                WHERE CD_CTRA_TIT_VL     = :SGTS0192-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP    = :SGTS0192-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD    = :SGTS0192-NR-VRS-OPR-ETRD
                  AND CD_ARQ_OPR_ETRD    = :SGTS0192-CD-ARQ-OPR-ETRD-ANT
           END-EXEC

           IF  QT-REG-SEL GREATER ZEROS
               SET REGISTRO-EXISTE TO TRUE
           END-IF
           .
       230000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       250000-GRAVA-ANEXO-OPR       SECTION.
      *----------------------------------------------------------------*
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSI060                    TO NM-PGM

           MOVE SGTS0192-CD-CTRA-TIT-VL     TO SGTSI060-CD-CTRA-TIT-VL
           MOVE SGTS0192-CD-OPR-ETRD-MCP    TO SGTSI060-CD-OPR-ETRD-MCP
           MOVE SGTS0192-NR-VRS-OPR-ETRD    TO SGTSI060-NR-VRS-OPR-ETRD
           MOVE SGTS0192-CD-ARQ-OPR-ETRD-ANT
                                      TO SGTSI060-CD-ARQ-OPR-ETRD
           MOVE SGTS0192-TX-DCR-ANX-ANT     TO SGTSI060-TX-DCR-ANX
           MOVE KOPRD-CD-OPRD               TO SGTSI060-CD-USU-ULT-ATL
           MOVE 'CURRENT'                   TO SGTSI060-TS-ULT-ATL

           PERFORM 900000-EXECUTAR-MODULO
           .
       250000-SAI.
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

           MOVE SPACES TO IN-EXITENCIA-REGISTRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM

               WHEN SGTSI060
                   EXEC CICS LINK
                             PROGRAM ( SGTSI060  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC


               WHEN SGTS0081
                   EXEC CICS LINK
                             PROGRAM ( SGTS0081  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN OTHER
                   MOVE  9006  TO  MSG1-CD-ERRO-TS
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
                    MOVE  3004  TO  MSG1-CD-ERRO-TS
                    STRING
                         'Erro ao acionar o módulo ' NM-PGM ', '
                         'RESP  ='   ' ' MSG1-RESP1  ', '
                         'RESP2 =' ' ' MSG1-RESP2
                          DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    END-STRING
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
      * Monitora as variaveis dos modulos chamados
      *----------------------------------------------------------------*

           EVALUATE NM-PGM

           WHEN SGTSI060
                MOVE 'SGTSI060-CD-CTRA-TIT-VL'   TO MSGKW999-NM-VRV-MNT
                MOVE SGTSI060-CD-CTRA-TIT-VL     TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSI060-CD-OPR-ETRD-MCP'  TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                IF  SGTSI060-CD-OPR-ETRD-MCP     IS NUMERIC
                    MOVE  SGTSI060-CD-OPR-ETRD-MCP  TO  CD-OPR-ETRD-MCP
                    MOVE  CD-OPR-ETRD-MCP-TX     TO  MSGKW999-VL-VRV-MNT
                ELSE
                    MOVE  'Valor não numérico.'  TO  MSGKW999-VL-VRV-MNT
                END-IF
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSI060-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSI060-NR-VRS-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSI060-CD-ARQ-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSI060-CD-ARQ-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSI060-CD-USU-ULT-ATL'   TO MSGKW999-NM-VRV-MNT
                MOVE SGTSI060-CD-USU-ULT-ATL     TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSI060-TS-ULT-ATL'       TO MSGKW999-NM-VRV-MNT
                MOVE SGTSI060-TS-ULT-ATL         TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSI060-TX-DCR-ANX'       TO MSGKW999-NM-VRV-MNT
                MOVE SGTSI060-TX-DCR-ANX         TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

           WHEN SGTS0081
                MOVE 'SGTS0081-CD-CTRA-TIT-VL'   TO MSGKW999-NM-VRV-MNT
                MOVE SGTS0081-CD-CTRA-TIT-VL     TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTS0081-CD-OPR-ETRD'      TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                IF  SGTS0081-CD-OPR-ETRD     IS NUMERIC
                    MOVE  SGTS0081-CD-OPR-ETRD   TO  CD-OPR-ETRD-MCP
                    MOVE  CD-OPR-ETRD-MCP-TX     TO  MSGKW999-VL-VRV-MNT
                ELSE
                    MOVE  'Valor não numérico.'  TO  MSGKW999-VL-VRV-MNT
                END-IF
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTS0081-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTS0081-NR-VRS-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
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

      *    Monta o nome do container de requisição
           MOVE 'SGTS0192'             TO NM-PGM
           MOVE 'SGTS0192-RQSC'        TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      *    Obtem o tamanho esperado da requisição
           MOVE LENGTH OF SGTS0192-RQSC TO QT-CCT-EPRO

      *    Valida tamanho esperado da requisição X tamanho recebido
      *    MSG1-CD-ERRO-TS=3001
           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA       TO TRUE
               PERFORM 999000-ERRO
           END-IF

           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR      )
                     FLENGTH   (QT-CCT-LIDO  )
                     INTO      (SGTS0192-RQSC)
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
                    MOVE NM-PGM            TO MSG1-NM-PGM-ACND

                    MOVE IIBKERRO-TX-ERRO  TO MSG1-TX-RTN-PGM-ACND
                                              MSG1-TX-ERRO-TS

                    MOVE IIBKERRO-CD-ERRO  TO MSG1-CD-RTN-PGM-ACND
                                              MSG1-CD-ERRO-TS

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

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

      *    Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

           EVALUATE NM-PGM

               WHEN SGTS0081
                    MOVE LENGTH OF SGTS0081-RPST TO QT-CCT-EPRO

               WHEN SGTSI060
                    MOVE LENGTH OF SGTSI060-RPST TO QT-CCT-EPRO

           END-EVALUATE

      *    MSG1-CD-ERRO-TS=3001
           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA       TO TRUE
               PERFORM 999000-ERRO
           END-IF

      *Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Le a RESPOSTA de acordo com o programa que a gravou

           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM

      *        Calcular a quantidade de caracteres esperado e limpar
      *        a area de dados antes de executar o GET
      *        Para matrizes com dados variáveis (clausula depending
      *        on) não há como utilizar o comando INITIALIZE


               WHEN SGTSI060
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSI060-RPST )
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTS0081
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTS0081-RPST )
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC


               WHEN OTHER
                    MOVE  9003  TO  MSG1-CD-ERRO-TS
                    STRING
                        'Erro na leitura da resposta do módulo. '
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
       974000-LER-DADOS-OPRD           SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *Configura dados para leitura do container de requisição            VRS001
           SET  GET-CONTAINER          TO TRUE                            VRS001
           MOVE 'PRM-INF-OPRD'         TO NM-CTNR                         VRS001
                                                                          VRS001
      *Por causa da captura automática de erro CICS, deve ser previsto    VRS001
      *o erro de container                                                VRS001
           SET  ERRO-CONTAINER         TO TRUE                            VRS001
                                                                          VRS001
           INITIALIZE PRM-INF-OPRD                                        VRS001
           EXEC CICS GET                                                  VRS001
                CONTAINER ( NM-CTNR                )                      VRS001
                INTO      ( PRM-INF-OPRD           )                      VRS001
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )                      VRS001
           END-EXEC.                                                      VRS001
                                                                          VRS001
      *    Ao chegar aqui, há certeza de que não houve erro algum         VRS001
           SET  SEM-ERRO               TO TRUE                            VRS001
           .                                                              VRS001
       974000-SAI.                                                        VRS001
           EXIT.


      *----------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *----------------------------------------------------------------*
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE
           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RQSC'                TO NM-CTNR(9:5)

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisicao do programa a executar
           EVALUATE NM-PGM

               WHEN SGTSI060
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI060-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI060-RQSC )
                    END-EXEC

               WHEN SGTS0081
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTS0081-RQSC           )
                              FLENGTH   (LENGTH OF SGTS0081-RQSC )
                    END-EXEC

               WHEN OTHER
                    MOVE  9004  TO  MSG1-CD-ERRO-TS
                    STRING
                      'Erro na gravação da requisição do módulo.'
                       NM-PGM DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
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
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)
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

           MOVE 'SGTS0192-CD-CTRA-TIT-VL'        TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0192-CD-CTRA-TIT-VL         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0192-CD-OPR-ETRD-MCP'       TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0192-CD-OPR-ETRD-MCP      IS NUMERIC
               MOVE  SGTS0192-CD-OPR-ETRD-MCP    TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX          TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0192-NR-VRS-OPR-ETRD'       TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0192-NR-VRS-OPR-ETRD        TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0192-CD-ARQ-OPR-ETRD-ANT'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTS0192-CD-ARQ-OPR-ETRD-ANT     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0192-CD-TIP-ARQ-ANT'        TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0192-CD-TIP-ARQ-ANT         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0192-TX-DCR-ANX-ANT'        TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0192-TX-DCR-ANX-ANT         TO MSGKW999-VL-VRV-MNT
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
      *---------------------- FIM SGTS0192 -----------------------------