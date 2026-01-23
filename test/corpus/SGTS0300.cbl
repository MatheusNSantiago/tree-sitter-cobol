      *----------------------------------------------------------------*
      * PROGRAMA..: SGTS0300
      * ANALISTA..: F9507141 - Vicente de Paulo Tavares de Souza
      * AUTOR.....: F9507141 - Vicente de Paulo Tavares de Souza
      * DATA......: 05.10.2023
      * OBJETIVO..: Negócio  - Listar Taxonomias disponíveis para uma
      *             determinada Carteira Drive e Produto/Modalidade.
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS002 05.06.2024 - F9158937 - Substituição da chamada do      *
      *        programa RSMS013D pelo programa RSMS015A                *
      * VRS001 05.10.2023 - F9507141 - IMPLANTAÇÃO                     *
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS0300.
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
                                            '*** SGTS0300 ***'.
       77  CTE-VERS                    PIC  X(06)  VALUE 'VRS002'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  IIBP2000                    PIC  X(08) VALUE 'IIBP2000'.
           03  RSMS015A                    PIC  X(08) VALUE 'RSMS015A'.


      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1
      *----------------------------------------------------------------*
      *                     Containers utilizados
      *----------------------------------------------------------------*

      * Container de erro
       77 CTNR-PRM-ERRO                PIC X(16) VALUE 'PRM-ERRO'.

      * Container de requisição desta subrotina
       77 CTNR-SGTS0300-RQSC           PIC X(16) VALUE 'SGTS0300-RQSC'.

      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*

       01  NM-PGM-CHAMADO              PIC  X(08) VALUE SPACES.
       01  IC-RSM                      PIC S9(04) COMP-5 VALUE ZEROS.
       01  IC-SGT                      PIC S9(04) COMP-5 VALUE ZEROS.

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
-INC SGTK0300

      *-----> Book do RSMS015A que busca a lista de Taxonomias
       01 RSMS015A-DADOS.
-INC RSMK015A

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

      *    Valida campo a campo da requisição

           IF  SGTS0300-CD-PRD LESS ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE  0010              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do Produto inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           ELSE
               IF  SGTS0300-CD-PRD  EQUAL ZEROS
                   SET ERRO-PROGRAMADO TO TRUE
                   MOVE  0068          TO  MSG1-CD-ERRO-TS
                   MOVE  'Código do Produto não informado.'
                                       TO  MSG1-TX-ERRO-TS
                   SET MNT-VRV         TO TRUE
                   PERFORM 999000-ERRO
               END-IF
           END-IF

           IF  SGTS0300-CD-MDLD LESS ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE  0011              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da Modalidade do Produto inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           ELSE
               IF  SGTS0300-CD-MDLD  EQUAL ZEROS
                   SET ERRO-PROGRAMADO TO TRUE
                   MOVE  0069          TO  MSG1-CD-ERRO-TS
                   MOVE 'Código da Modalidade do Produto não informado.'
                                       TO  MSG1-TX-ERRO-TS
                   SET MNT-VRV         TO TRUE
                   PERFORM 999000-ERRO
               END-IF
           END-IF

           IF  SGTS0300-CD-CMCP-ENTD LESS ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE  1115              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da Carteira de Mercado de Capitais inválido
      -              '.'               TO  MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           ELSE
               IF  SGTS0300-CD-CMCP-ENTD  EQUAL ZEROS
                   SET ERRO-PROGRAMADO TO TRUE
                   MOVE  1116          TO  MSG1-CD-ERRO-TS
                   MOVE  'Código da Carteira de Mercado de Capitais não
      -                  ' informado.' TO  MSG1-TX-ERRO-TS
                   SET MNT-VRV         TO TRUE
                   PERFORM 999000-ERRO
               END-IF
           END-IF

           IF  SGTS0300-CD-GR-MCP-ENTD LESS ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE  1117              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do Grupo de Mercado de Capitais inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           ELSE
               IF  SGTS0300-CD-GR-MCP-ENTD  EQUAL ZEROS
                   SET ERRO-PROGRAMADO TO TRUE
                   MOVE  1118          TO  MSG1-CD-ERRO-TS
                   MOVE  'Código do Grupo de Mercado de Capitais não inf
      -                  'ormado.'     TO  MSG1-TX-ERRO-TS
                   SET MNT-VRV         TO TRUE
                   PERFORM 999000-ERRO
               END-IF
           END-IF
           .
       110000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

      *    Busca a lista de Taxonomias no RSM
           PERFORM 900002-LISTA-TAXONOMIA

           MOVE ZEROS TO IC-RSM
           MOVE ZEROS TO IC-SGT

           PERFORM  VARYING IC-RSM  FROM 1 BY 1
                      UNTIL IC-RSM  GREATER  K015A-QT-REG
               IF  SGTS0300-CD-CMCP-ENTD   EQUAL K015A-CD-CMCP(IC-RSM)
                 AND
                   SGTS0300-CD-GR-MCP-ENTD EQUAL K015A-CD-GR-MCP(IC-RSM)
                   ADD 1               TO IC-SGT
                       ON SIZE ERROR
                       SET ERRO-PROGRAMADO  TO TRUE
                       MOVE 0706            TO MSG1-CD-ERRO-TS
                       MOVE 'Erro em operação aritmética.'
                                            TO MSG1-TX-ERRO-TS
                       PERFORM 999000-GRAVA-ERRO-E-ENCERRA
                   END-ADD
                   PERFORM 210000-CARREGAR-LISTA
               END-IF
           END-PERFORM

           IF  IC-SGT  EQUAL ZEROS
              SET ERRO-PROGRAMADO      TO TRUE
              MOVE   379               TO  MSG1-CD-ERRO-TS
              MOVE  'Não encontrado nenhum registro com os parâmetros in
      -             'formados.'        TO  MSG1-TX-ERRO-TS
              PERFORM  900008-MONITORAR-VARIAVEIS
              SET MNT-VRV              TO TRUE
              PERFORM 999000-ERRO
           END-IF

           MOVE  IC-SGT                TO SGTS0300-QT-OCR-TAB

           PERFORM 980000-GRAVAR-CONTAINER-RPST

      * MOCK-POINT MOCKMODULO
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-CARREGAR-LISTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE K015A-CD-CMCP(IC-RSM)  TO SGTS0300-CD-CMCP-SAID(IC-SGT)
           MOVE K015A-TX-CMCP(IC-RSM)  TO SGTS0300-TX-CMCP(IC-SGT)
           MOVE K015A-CD-GR-MCP(IC-RSM)
                                      TO SGTS0300-CD-GR-MCP-SAID(IC-SGT)
           MOVE K015A-TX-GR-MCP(IC-RSM)
                                       TO SGTS0300-TX-GR-MCP(IC-SGT)
           MOVE K015A-CD-LMCP(IC-RSM)  TO SGTS0300-CD-LMCP(IC-SGT)
           MOVE K015A-TX-LMCP(IC-RSM)  TO SGTS0300-TX-LMCP(IC-SGT)
           MOVE K015A-CD-SMCP(IC-RSM)  TO SGTS0300-CD-SMCP(IC-SGT)
           MOVE K015A-TX-SMCP(IC-RSM)  TO SGTS0300-TX-SMCP(IC-SGT)
           MOVE K015A-CD-TIP-CLSC-CTB(IC-RSM)
                                       TO SGTS0300-CD-TIP-CLSC-CTB
                                          (IC-SGT)
           MOVE K015A-NM-TIP-CLSC-CTB(IC-RSM)
                                       TO SGTS0300-NM-TIP-CLSC-CTB
                                          (IC-SGT)
           .
       210000-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * Executa um módulo implementado com CONTAINER                   *
      *----------------------------------------------------------------*
       900002-LISTA-TAXONOMIA          SECTION.
      *----------------------------------------------------------------*
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE  RSMS015A              TO  NM-PGM-CHAMADO
      *    carregar o nº serviço / versão / operação / versão

           MOVE ZEROS                  TO  SRVC-ID-SRVC
           MOVE ZEROS                  TO  SRVC-VRS-SRVC
           MOVE 2064788                TO  SRVC-ID-OPR
           MOVE  3                     TO  SRVC-VRS-OPR

           PERFORM 900003-GRAVAR-SERVICO

           INITIALIZE   RSMS015A-RQSC
                           REPLACING NUMERIC BY  ZEROS

           MOVE SGTS0300-CD-PRD        TO K015A-CD-OBJ-PSQ
           MOVE SGTS0300-CD-MDLD       TO K015A-CD-MDLD-PSQ
           MOVE 0                      TO K015A-NR-ULT-REG-PSQ

           PERFORM 900004-GRAVAR-REQUISICAO
           PERFORM 900005-CHAMAR-SERVICO
           PERFORM 900006-BUSCAR-RESPOSTA-ERRO
           PERFORM 900007-BUSCAR-RESPOSTA

           IF K015A-QT-REG  EQUAL  0
              SET ERRO-PROGRAMADO     TO TRUE
              MOVE  379               TO  MSG1-CD-ERRO-TS
              MOVE  'Não encontrado nenhum registro com os parâmetros in
      -             'formados.'       TO  MSG1-TX-ERRO-TS
              PERFORM  900008-MONITORAR-VARIAVEIS
              SET MNT-VRV             TO TRUE
              PERFORM 999000-ERRO
           END-IF
           .
       900002-SAI.
           EXIT.
      *----------------------------------------------------------------*
       900003-GRAVAR-SERVICO           SECTION.
      *----------------------------------------------------------------*
      *    Postar container de requisição de serviço.
      *----------------------------------------------------------------*
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE  'PRM-RQSC-SRVC'       TO  NM-CTNR

           EXEC CICS  PUT
                     CONTAINER ( NM-CTNR                      )
                     FROM      ( SRVC-PRM-RQSC-SRVC           )
                     FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC )
                     RESP      ( MSG1-RESP1                   )
                     RESP2     ( MSG1-RESP2                   )
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .
       900003-FIM.
           EXIT.
      *----------------------------------------------------------------*
       900004-GRAVAR-REQUISICAO        SECTION.
      *----------------------------------------------------------------*
      *    Postar container com dados de requisição (entrada)
      *----------------------------------------------------------------*
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE  'PRM-DADOS-RQSC'      TO  NM-CTNR

           EVALUATE NM-PGM-CHAMADO
           WHEN RSMS015A
               EXEC CICS PUT
                     CONTAINER ( NM-CTNR                    )
                     FROM      ( RSMS015A-RQSC              )
                     FLENGTH   ( LENGTH OF RSMS015A-RQSC    )
                     RESP      ( MSG1-RESP1                 )
                     RESP2     ( MSG1-RESP2                 )
               END-EXEC
           END-EVALUATE

           EVALUATE TRUE
               WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .
       900004-FIM.
           EXIT.
      *----------------------------------------------------------------*
       900005-CHAMAR-SERVICO           SECTION.
      *----------------------------------------------------------------*
      *    Acionar programa IIBP2000 para requisição do serviço.
      *----------------------------------------------------------------*
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS LINK
                     PROGRAM   ( IIBP2000     )
                     CHANNEL   ( NM-CNL       )
                     RESP      ( MSG1-RESP1   )
                     RESP2     ( MSG1-RESP2   )
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .
       900005-FIM.
           EXIT.
      *----------------------------------------------------------------*
       900006-BUSCAR-RESPOSTA-ERRO     SECTION.
      *----------------------------------------------------------------*
      *    A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *----------------------------------------------------------------*
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           MOVE 'PRM-ERRO'             TO NM-CTNR
           EXEC CICS  GET
                     CONTAINER ( NM-CTNR                     )
                     INTO      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC.

           EVALUATE TRUE
               WHEN MSG1-RESP1         EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
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
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .
       900006-FIM.
           EXIT.
      *----------------------------------------------------------------*
       900007-BUSCAR-RESPOSTA          SECTION.
      *----------------------------------------------------------------*
      *    Recupera o container com os dados devolvidos pelo provedor
      *    do serviço.
      *----------------------------------------------------------------*
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           MOVE 'PRM-DADOS-RPST'       TO NM-CTNR

      *    Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

      *    Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *    Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EVALUATE NM-PGM-CHAMADO
           WHEN RSMS015A
               EXEC CICS  GET
                     CONTAINER ( NM-CTNR                  )
                     INTO      ( RSMS015A-RPST            )
                     FLENGTH   ( QT-CCT-LIDO              )
                     RESP      ( MSG1-RESP1               )
                     RESP2     ( MSG1-RESP2               )
               END-EXEC
           END-EVALUATE

           EVALUATE TRUE
               WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .
       900007-FIM.
           EXIT.
      *-----------------------------------------------------------------
       900008-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------
           EVALUATE  NM-PGM-CHAMADO
      *------> Monitora as Variaveis de Programas chamados
               WHEN  RSMS015A
                     MOVE 'K015A-CD-OBJ-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  K015A-CD-OBJ-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'K015A-CD-MDLD-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  K015A-CD-MDLD-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE
           .
       900008-SAI.
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
           MOVE CTNR-SGTS0300-RQSC     TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER ( NM-CTNR       )
                     FLENGTH   ( QT-CCT-LIDO   )
                     INTO      ( SGTS0300-RQSC )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       971000-SAI.
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
      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

      *--> Exclui eventual container de erro de outros programas
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO

      *--> Monta o nome do container de resposta
           MOVE  'SGTS0300-RPST'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                 )
                      CHANNEL   ( NM-CNL                  )
                      FLENGTH   ( LENGTH OF SGTS0300-RPST )
                      FROM      ( SGTS0300-RPST           )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
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
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

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
               MOVE 'SGTS0300-CD-PRD'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0300-CD-PRD
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0300-CD-MDLD'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0300-CD-MDLD
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0300-CD-CMCP-ENTD'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0300-CD-CMCP-ENTD
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0300-CD-GR-MCP-ENTD'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0300-CD-GR-MCP-ENTD
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
      *---------------------- Fim SGTS0300 ----------------------------*