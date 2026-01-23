      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0237
      * ANALISTA..: F8785286 - Roseli Akemi Kawahata
      * AUTOR.....: F8785286 - Roseli Akemi Kawahata
      * DATA......: 13/11/2018
      * OBJETIVO..: Negócio - Devolução de solicitação de compra.
      * COMPILACAO: COBOL/CICS/DB2 (On-line)
      *-----------------------------------------------------------------
      * VRS004 28.11.2023 - F9158937 - Incluir chamada SGTS005B -
      *                     Cancelamento Reserva OPR
      * VRS003 11.11.2019 - F2804421 - 2016/145195 - Incluir atualização
      *          do estado da ordem de negociação(SGTSLB75 e SGTSI078).
      *
      * VRS002 16.04.2019 - F2804421 - 2016/145195 -Incluir o código de
      *              ação SGTSL063-CD-ACAO = 1 (indicadores vigentes).
      *             - Substituir exclusão física por exclusão lógica do
      *               indicador = 2.
      * VRS001 31.01.2019 - F0950413 - ACERTO NA SECTION 971000-LER-ERRO
      * VRS001 13.11.2018 - F8785286 - 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0237.

      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-WS               PIC X(40) VALUE
                                       '*** WSS COMECA AQUI ***'.
       77  CTE-PROG                    PIC X(18) VALUE
               '*** SGTS0237 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS004'.

       01  TAB-PROGRAMAS-CHAMADOS.

      *--> Módulos a serem acionados via 'EXEC CICS LINK (COMMAREA)'
           03  SGTST009                PIC X(08) VALUE 'SGTST009'.
           03  SGTST005                PIC X(08) VALUE 'SGTST005'.

      *--> Módulos a serem acionados via 'CONTAINER'
           03  SGTSA063                PIC X(08) VALUE 'SGTSA063'.
           03  SGTSI069                PIC X(08) VALUE 'SGTSI069'.
           03  SGTSI078                PIC X(08) VALUE 'SGTSI078'.
           03  SGTSI094                PIC X(08) VALUE 'SGTSI094'.
           03  SGTSL009                PIC X(08) VALUE 'SGTSL009'.
           03  SGTSL063                PIC X(08) VALUE 'SGTSL063'.
           03  SGTSLB75                PIC X(08) VALUE 'SGTSLB75'.
           03  SGTSL094                PIC X(08) VALUE 'SGTSL094'.
           03  SGTS005B                PIC X(08) VALUE 'SGTS005B'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *--> Texto destaque para mensagem dinamica.
       77  TX-DTQ-MSG                  PIC  X(30)     VALUE
                                           'devolução para autorização'.
      *--> CONTAINER do usuário logado
       77  CTNR-PRM-INF-OPRD           PIC X(16) VALUE 'PRM-INF-OPRD'.

       01  CTE-FIM-WS                  PIC X(32)  VALUE
                                             '*** WSS TERMINA AQUI ***'.
      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  CTE-INICIO-LS               PIC  X(32)  VALUE
                                             '*** LS COMECA AQUI ***'.
      *--> Variáveis de trabalho
       01  AUX-CD-TIP-EST-NEGO         PIC  9999.
       01  IC-TAB                      PIC S9(04)     COMP-5  VALUE   0.

      *--> Book do container de erro
-INC IIBKERRO
      *--> Book para captura de erro via MSG1
-INC MSGKWMG1
      *--> Variaveis para tratamento de Abend e Condition
-INC HLPKW999
      *--> Variaveis para tratamento de CONTAINER
-INC SGTKERR1
      *--> Book dos dados do usuário
-INC IIBKOPRD

      *--> Container de erro
       01  CTNR-PRM-ERRO               PIC X(16) VALUE 'PRM-ERRO'.

      *--> Areas de Controle de Programa Chamado (CONTAINER)
       01  CTL-PGM-CHMD.
           03  NM-PGM-CHMD             PIC  X(08)         VALUE ' '.

       01  WS-ERRO-CICS.
           03  WS-RESP1                PIC  9(008).
           03  WS-RESP2                PIC  9(008).

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      ---------------9.

      *----------------------------------------------------------------
      *--> Manutenção da tabela 'DB2SGT.EST_OPR_ETRD'
       01  SGTST009-DADOS.
-INC SGTKT009

      *--> Pesquisa tabela OPR_ETRD_MCP
       01  SGTST005-DADOS.
-INC SGTKT005

      *--> Recupera estado e versão do estado mais atual SGTSL009
       01  SGTSL009-DADOS.
-INC SGTKL009

      *--> Inclusão na tabela 'DB2SGT.EST_VRS_OPR_ETRD'
       01  SGTSI069-DADOS.
-INC SGTKI069

      *--> Inclusão na Tabela EST_ORD_NEGO_OPR'
       01  SGTSI078-DADOS.
-INC SGTKI078

      *--> Inclusão na tabela 'DB2SGT.IFLC_MNL_LIM_EXCO'
-INC SGTKI094

      *--> Consulta tabela 'DB2SGT.IFLC_MNL_LIM_EXCO'
-INC SGTKL094

      *--> Consulta tabela 'DB2SGT.IN_OPR_ETRD'
-INC SGTKL063

      *--> Exclusão lógica da tabela 'DB2SGT.IN_OPR_ETRD'
-INC SGTKA063

      *--> Consulta lista  da tabela 'DB2SGT.ORD_NEGO_OPR X MOD_ORD_OPR'
      *    e EST_ORD_NEGO_OPR e EST_MOD_ORD_NEGO.
-INC SGTKLB75

      *--> Programa que faz Cancelamento Reserva OPR
       01  SGTS005B-DADOS.
-INC SGTK005B

      *--> Book do programa
       01  SGTS0237-DADOS.
-INC SGTK0237

       01  CTE-FIM-LS                  PIC  X(32)  VALUE
                                             '*** LS TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *--> Habilita Testes no Cobolito
-INC ALMKCBL2

      *--> Habilita tratamento automatico de Erros DB2
      *-INC HLPKE997
      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999
           MOVE CTE-PROG(5:8)          TO NM-PGM
                                          NM-PGM-CHMD

           PERFORM 100000-OBTER-NOME-DO-CANAL
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
           PERFORM 970000-LER-REQUISICAO-RECEBIDA

           PERFORM 140000-VALIDAR-PARAMETROS
           PERFORM 974000-LER-DADOS-OPRD

           PERFORM 160000-VALOR-INICIAL-VARIAVEIS
           PERFORM 150000-VER-USARIO-E-DEVOLUCAO
           PERFORM 155000-VER-ORDEM-COMPRA
           PERFORM 170000-VER-IFLC-MNL
           PERFORM 180000-VER-FLEXB-MANUAL

           PERFORM 300000-FINALIZA-CADASTRAMENTO
           PERFORM 400000-ATUALIZA-ESTADO-VERSAO
           PERFORM 500000-ATUALIZA-ESTADO-ORDEM
           PERFORM 700000-CANCELA-RESERVA-OPR

           PERFORM 999999-ENCERRAR
           GOBACK
           .
       000000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       100000-OBTER-NOME-DO-CANAL      SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
                CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETROS       SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           IF  SGTS0237-CD-CTRA-TIT-VL  NOT > ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de títulos e valores mobiliários
      -             ' inválido.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0237-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
            IF SGTS0237-CD-OPR-ETRD-MCP = 0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0005               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' informado.'      TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
            END-IF
           END-IF

           IF  SGTS0237-NR-VRS-OPR-ETRD NOT > ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0006               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0237-TX-JST-EST     EQUAL SPACES
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0777               TO MSG1-CD-ERRO-TS
               MOVE 'Texto de observação não informado.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       150000-VER-USARIO-E-DEVOLUCAO   SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE 2                      TO SGTSL009-E-CD-TIP-SRVC
           MOVE SGTSL009               TO NM-PGM-CHMD
           PERFORM 900000-PROCEDIMENTOS-CONTAINER

      *--> Consistência da permissão da devolução
           IF (SGTSL009-CD-TIP-EST-OPR(1)     NOT = 3       AND
               SGTSL009-CD-TIP-EST-OPR(1)     NOT = 4)      OR
              (SGTSL009-CD-TIP-EST-VRS(1)     NOT = 3)

               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0866               TO MSG1-CD-ERRO-TS
               MOVE 'Estado da operação não permite devolução.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       150000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       155000-VER-ORDEM-COMPRA         SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

      *--> Consulta dados da ordem de compra.
           MOVE SGTS0237-CD-CTRA-TIT-VL
                                       TO SGTSLB75-E-CD-CTRA-TIT-VL
           MOVE SGTS0237-CD-OPR-ETRD-MCP
                                       TO SGTSLB75-E-CD-OPR-ETRD-MCP
           MOVE SGTS0237-NR-VRS-OPR-ETRD
                                       TO SGTSLB75-E-NR-VRS-OPR-ETRD
           MOVE ZEROS                  TO SGTSLB75-E-NR-SEQL-ORD-PAG

           MOVE SGTSLB75               TO NM-PGM-CHMD
           PERFORM 900000-PROCEDIMENTOS-CONTAINER

      *--> Consistência dos dados da ordem de venda em relação a ação
      *    de devolução do cadastramento da operação.

           IF  SGTSLB75-QT-OCR-TAB      EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0650               TO MSG1-CD-ERRO-TS
               MOVE 'Ordem de negociação da operação inexistente.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSLB75-CD-TIP-EST-NEGO(1)     NOT = 4     AND
               SGTSLB75-CD-TIP-EST-NEGO(1)     NOT = 5
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 1106               TO MSG1-CD-ERRO-TS
               MOVE SGTSLB75-CD-TIP-EST-NEGO(1)
                                       TO AUX-CD-TIP-EST-NEGO
               STRING 'Estado da ordem não permite '
                       DELIMITED BY SIZE
                       TX-DTQ-MSG
                       DELIMITED BY SPACE
                     ' da ordem de compra. Estado atual = '
                       DELIMITED BY SIZE
                       AUX-CD-TIP-EST-NEGO
                       DELIMITED BY SIZE
                       INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF SGTSLB75-IN-MOD-ORD-ATI( 1) NOT = 'S'
               SET  ERRO-PROGRAMADO       TO TRUE
               MOVE 0917                  TO MSG1-CD-ERRO-TS
               MOVE 'Modelo de ordem de compra deve estar ativo.'
                                          TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       155000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       160000-VALOR-INICIAL-VARIAVEIS  SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           SET  SGTST009-INCLUIR       TO TRUE

           MOVE SGTS0237-CD-CTRA-TIT-VL     TO SGTST009-CD-CTRA-TIT-VL
                                               SGTSI069-CD-CTRA-TIT-VL
                                               SGTSL009-CD-CTRA-TIT-VL

           MOVE SGTS0237-CD-OPR-ETRD-MCP    TO SGTST009-CD-OPR-ETRD-MCP
                                               SGTSI069-CD-OPR-ETRD-MCP
                                               SGTSL009-CD-OPR-ETRD-MCP

           MOVE SGTS0237-NR-VRS-OPR-ETRD    TO SGTST009-NR-VRS-OPR-ETRD
                                               SGTSI069-NR-VRS-OPR-ETRD
                                               SGTSL009-NR-VRS-OPR-ETRD

           MOVE 0                      TO SGTST009-CD-MTV-ALT

           MOVE KOPRD-CD-OPRD          TO SGTST009-CD-USU-RSP-EST
                                          SGTST009-CD-USU-RSP-ATL

           MOVE 'CURRENT'              TO SGTST009-TS-EST-OPR-ETRD

           MOVE 2                      TO SGTSI069-CD-TIP-EST-VRS
                                          SGTST009-CD-TIP-EST-OPR

           MOVE SGTS0237-TX-JST-EST    TO SGTSI069-TX-OBS-EST-VRS
           .
       160000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       170000-VER-IFLC-MNL             SECTION.
      * Verifica se há impacto manual vigente cadastrado p/ operação.
      *  Caso positivo, deve-se gravar outro registro inativando o
      *   impacto
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           PERFORM 171000-PESQ-OPR
           MOVE 1                         TO SGTSL094-E-CD-TIP-SRVC
           MOVE SGTS0237-CD-CTRA-TIT-VL   TO SGTSL094-E-CD-CTRA-TIT-VL
           MOVE SGTST005-CD-TIT-OPR-ETRD  TO SGTSL094-E-CD-TIT-OPR-ETRD
           MOVE SGTST005-NR-DVS-TIT       TO SGTSL094-E-NR-DVS-TIT
           MOVE 'CURRENT'                 TO SGTSL094-E-TS-ATL-IFLC-MNL

           MOVE SGTSL094                  TO NM-PGM-CHMD
           PERFORM 900000-PROCEDIMENTOS-CONTAINER

      *--> Consistência de existência de impacto manual vigente. (se
      *     existir, criar novo registro com IN_IFLC_MNL_EXCO = 'N')
           IF SGTSL094-QT-OCR-TAB  >  0
              MOVE SGTS0237-CD-CTRA-TIT-VL  TO SGTSI094-CD-CTRA-TIT-VL
              MOVE SGTST005-CD-TIT-OPR-ETRD TO SGTSI094-CD-TIT-OPR-ETRD
              MOVE SGTST005-NR-DVS-TIT      TO SGTSI094-NR-DVS-TIT
              MOVE 'CURRENT'                TO SGTSI094-TS-ATL-IFLC-MNL
              MOVE 'N'                      TO SGTSI094-IN-IFLC-MNL-EXCO
              MOVE KOPRD-CD-OPRD         TO SGTSI094-CD-USU-RSP-ULT-ATL

              MOVE SGTSI094                 TO NM-PGM-CHMD
              PERFORM 900000-PROCEDIMENTOS-CONTAINER
           END-IF
           .
       170000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       171000-PESQ-OPR             SECTION.
      * A partir da carteira/oper/versão, recupera título da operação
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           SET  SGTST005-CONSULTAR         TO TRUE
           MOVE SGTS0237-CD-CTRA-TIT-VL    TO SGTST005-CD-CTRA-TIT-VL
           MOVE SGTS0237-CD-OPR-ETRD-MCP   TO SGTST005-CD-OPR-ETRD-MCP
           MOVE SGTS0237-NR-VRS-OPR-ETRD   TO SGTST005-NR-VRS-OPR-ETRD

           PERFORM 951000-PESQ-OPERACAO

           IF SGTST005-CD-RTN-PGM NOT EQUAL ZEROS
              SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
              MOVE SGTST005-CD-RTN-PGM TO MSG1-CD-ERRO-TS
              MOVE SGTST005-MSG-ERRO   TO MSG1-TX-ERRO-TS
              MOVE SGTST005            TO MSG1-NM-PGM-ACND
                                          NM-PGM-CHMD
              MOVE SGTST005-CD-RTN-PGM TO MSG1-CD-RTN-PGM-ACND
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       171000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       180000-VER-FLEXB-MANUAL             SECTION.
      *  Verifica se há flexibilização manual cadastrada para a oper.
      *  Se existir, deletar o registro.
      *-----------------------------------------------------------------
           MOVE 0009                      TO MSG1-CD-SEQL-ERRO

      *-->  Lista indicadores vigentes.
           MOVE 1                         TO SGTSL063-CD-ACAO

           MOVE SGTS0237-CD-CTRA-TIT-VL   TO SGTSL063-CD-CTRA-TIT-VL
           MOVE SGTS0237-CD-OPR-ETRD-MCP  TO SGTSL063-CD-OPR-ETRD-MCP
           MOVE SGTS0237-NR-VRS-OPR-ETRD  TO SGTSL063-NR-VRS-OPR-ETRD
           MOVE 2                         TO SGTSL063-CD-TIP-IN-E
           MOVE ZEROS                     TO SGTSL063-NR-IN-OPR-ETRD-E

           MOVE SGTSL063                  TO NM-PGM-CHMD
           PERFORM 900000-PROCEDIMENTOS-CONTAINER

           PERFORM VARYING  SGTSL063-IC-TAB FROM 1 BY  1
                  UNTIL SGTSL063-IC-TAB  >   SGTSL063-QT-OCR-TAB
              IF SGTSL063-CD-TIP-IN (SGTSL063-IC-TAB)  EQUAL 2
                 MOVE SGTS0237-CD-CTRA-TIT-VL
                                       TO SGTSA063-CD-CTRA-TIT-VL
                 MOVE SGTS0237-CD-OPR-ETRD-MCP
                                       TO SGTSA063-CD-OPR-ETRD-MCP
                 MOVE SGTS0237-NR-VRS-OPR-ETRD
                                       TO SGTSA063-NR-VRS-OPR-ETRD
                 MOVE 2                TO SGTSA063-CD-TIP-IN
                 MOVE SGTSL063-NR-IN-OPR-ETRD (SGTSL063-IC-TAB)
                                       TO SGTSA063-NR-IN-OPR-ETRD

                 MOVE SGTSA063         TO NM-PGM-CHMD
                 PERFORM 900000-PROCEDIMENTOS-CONTAINER
              END-IF
           END-PERFORM
           .
       180000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-FINALIZA-CADASTRAMENTO   SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTS0237-TX-JST-EST    TO SGTST009-TX-JST-EST
           PERFORM 950000-ATUALIZA-EST-OPERACAO

           IF SGTST009-CD-RTN-PGM NOT EQUAL ZEROS AND +100
              SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
              MOVE SGTST009-CD-RTN-PGM TO MSG1-CD-ERRO-TS
              MOVE SGTST009-MSG-ERRO   TO MSG1-TX-ERRO-TS
              MOVE SGTST009            TO MSG1-NM-PGM-ACND
                                          NM-PGM-CHMD
              MOVE SGTST009-CD-RTN-PGM TO MSG1-CD-RTN-PGM-ACND
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       400000-ATUALIZA-ESTADO-VERSAO   SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSI069               TO NM-PGM-CHMD
           PERFORM 900000-PROCEDIMENTOS-CONTAINER
           .
       400000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       500000-ATUALIZA-ESTADO-ORDEM    SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

      *--> Inclui estado da ordem com tipo = 3 - A autorizar.
           MOVE 3                      TO  SGTSI078-CD-TIP-EST-NEGO
           MOVE SGTSI078               TO NM-PGM-CHMD

           MOVE SGTS0237-CD-CTRA-TIT-VL
                                       TO  SGTSI078-CD-CTRA-TIT-VL
           MOVE SGTS0237-CD-OPR-ETRD-MCP
                                       TO  SGTSI078-CD-OPR-ETRD-MCP
           MOVE SGTS0237-NR-VRS-OPR-ETRD
                                       TO  SGTSI078-NR-VRS-OPR-ETRD
           MOVE SGTSLB75-NR-SEQL-ORD-NEGO(1)
                                       TO  SGTSI078-NR-SEQL-ORD-NEGO
           MOVE 'CURRENT'              TO  SGTSI078-TS-EST-ORD-NEGO
           MOVE KOPRD-CD-OPRD          TO  SGTSI078-CD-USU-ULT-ATL

           PERFORM 900000-PROCEDIMENTOS-CONTAINER
           .
       500000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       700000-CANCELA-RESERVA-OPR    SECTION.
      *-----------------------------------------------------------------
           MOVE 0027                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTS005B               TO NM-PGM-CHMD

           MOVE SGTS0237-CD-CTRA-TIT-VL
                                       TO  SGTS005B-CD-CTRA-TIT-VL
           MOVE SGTS0237-CD-OPR-ETRD-MCP
                                       TO  SGTS005B-CD-OPR-ETRD-MCP
           MOVE SGTS0237-NR-VRS-OPR-ETRD
                                       TO  SGTS005B-NR-VRS-OPR-ETRD

           PERFORM 900000-PROCEDIMENTOS-CONTAINER
           .
       700000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       900000-PROCEDIMENTOS-CONTAINER  SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      *--> Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVA-REQUISICAO

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *--> Executa a operação acionada via IIB
           PERFORM 910000-EXECUTA-MODULOS

      *--> Lê o container de erro da operação acionada
           PERFORM 971000-LER-ERRO

           IF NM-PGM-CHMD NOT EQUAL SGTS005B
      *--> Lê o container de resposta da operação acionada
              PERFORM 972000-LER-RESPOSTA
           END-IF
           .
       900000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       910000-EXECUTA-MODULOS          SECTION.
      *-----------------------------------------------------------------
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

      *    MOCK-POINT MCKV01

           EVALUATE NM-PGM-CHMD

      *--> Executar Módulo SGTSL009, container sem Serviço
               WHEN SGTSL009
                 EXEC CICS LINK
                           PROGRAM ( SGTSL009  )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC

      *--> Executar Módulo SGTSL063, container sem Serviço
               WHEN SGTSL063
                 EXEC CICS LINK
                           PROGRAM ( SGTSL063  )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC

      *--> Executar Módulo SGTSL094, container sem Serviço
               WHEN SGTSL094
                 EXEC CICS LINK
                           PROGRAM ( SGTSL094  )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC

      *--> Executar Módulo SGTSA063, container sem Serviço
               WHEN SGTSA063
                 EXEC CICS LINK
                           PROGRAM ( SGTSA063  )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC

      *--> Executar Módulo SGTSI094, container sem Serviço
               WHEN SGTSI094
                 EXEC CICS LINK
                           PROGRAM ( SGTSI094  )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC

      *--> Executar Módulo SGTSI069, container sem Serviço
               WHEN SGTSI069
                 EXEC CICS LINK
                           PROGRAM ( SGTSI069  )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC

      *--> Executar Módulo SGTSI078, container sem Serviço
               WHEN SGTSI078
                 EXEC CICS LINK
                           PROGRAM ( SGTSI078  )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC

      *--> Executar Módulo SGTSLB75, container sem Serviço
               WHEN SGTSLB75
                 EXEC CICS LINK
                           PROGRAM ( SGTSLB75  )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC
      *--> Executar Módulo SGTS005B, container sem Serviço
               WHEN SGTS005B
                 EXEC CICS LINK
                           PROGRAM ( SGTS005B  )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC

      *--> Execução de Módulo não prevista
               WHEN OTHER
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 9006             TO MSG1-CD-ERRO-TS
                 STRING 'Não definida a execução do módulo '
                         NM-PGM-CHMD '.'
                         DELIMITED BY SIZE
                         INTO         MSG1-TX-ERRO-TS
                 END-STRING
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *    MOCK-POINT MCKV02


           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    PERFORM 911000-VER-ERRO-MODULO-CHAMADO
           END-EVALUATE
           .
       910000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       911000-VER-ERRO-MODULO-CHAMADO  SECTION.
      *----------------------------------------------------------------*
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

      *--> Análise do retorno das chamadas dos programas
           SET ERRO-PROGRAMA-ACIONADO  TO  TRUE
           MOVE 3004          TO MSG1-CD-ERRO-TS
           STRING
              'Erro ao acionar o módulo ' NM-PGM-CHMD '.'
              'RESP  ='   ' ' MSG1-RESP1  ', '
              'RESP2 =' ' ' MSG1-RESP2
               DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
           END-STRING
           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       911000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       950000-ATUALIZA-EST-OPERACAO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS LINK
                        PROGRAM ( SGTST009       )
                        COMMAREA( SGTST009-DADOS )
                        RESP    ( WS-RESP1)
                        RESP2   ( WS-RESP2)
           END-EXEC

           IF  WS-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo SGTST009.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       950000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       951000-PESQ-OPERACAO            SECTION.
      *----------------------------------------------------------------*
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS LINK
                        PROGRAM ( SGTST005       )
                        COMMAREA( SGTST005-DADOS )
                        RESP    ( WS-RESP1)
                        RESP2   ( WS-RESP2)
           END-EXEC

           IF  WS-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo SGTST005.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       951000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970000-LER-REQUISICAO-RECEBIDA  SECTION.
      *-----------------------------------------------------------------
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisição
           MOVE 'SGTS0237'             TO NM-PGM
           MOVE 'SGTS0237-RQSC'        TO NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM 970100-LER-CONTAINER-NODATA

      *--> Obtem o tamanho esperado da requisição
           MOVE LENGTH OF SGTS0237-RQSC TO QT-CCT-EPRO

      *--> Valida tamanho esperado da requisição X tamanho recebido

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTS0237-RQSC' )
              CHANNEL   ( NM-CNL          )
              FLENGTH   ( QT-CCT-LIDO     )
              INTO      ( SGTS0237-RQSC   )
           END-EXEC
           .
       970000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       970100-LER-CONTAINER-NODATA     SECTION.
      *----------------------------------------------------------------*
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Obtem a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR    )
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       970100-SAI.
           EXIT.
      *----------------------------------------------------------------*
       971000-LER-ERRO                 SECTION.
      *----------------------------------------------------------------*
           MOVE 0020                   TO MSG1-CD-SEQL-ERRO

      *----------------------------------------------------------------*
      *    A presença do container PRM-ERRO indica que houve erro de   *
      *    integração, a ausência do container indica sucesso.         *
      *----------------------------------------------------------------*

      *--> Configura dados para leitura de container de erro
           MOVE CTNR-PRM-ERRO          TO NM-CTNR
           SET  GET-CONTAINER          TO TRUE
           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Leitura do container de erro
      *--> Não há captura automática de erro CICS
           EXEC CICS GET
                     CONTAINER ( NM-CTNR                     )
                     INTO      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     NOHANDLE
           END-EXEC

           EVALUATE TRUE
              WHEN EIBRESP             EQUAL DFHRESP(CONTAINERERR)
                   CONTINUE
              WHEN EIBRESP             EQUAL DFHRESP(NORMAL)
      *--> Trata o erro de programa acionado por ser módulo interno
               SET ERRO-PROGRAMA-ACIONADO   TO  TRUE
               IF NM-PGM-CHMD(1:3) EQUAL 'SGT'
                  MOVE IIBKERRO-CD-ERRO     TO  MSG1-CD-RTN-PGM-ACND
                                                MSG1-CD-ERRO-TS
                  MOVE IIBKERRO-TX-ERRO     TO  MSG1-TX-RTN-PGM-ACND
                                                MSG1-TX-ERRO-TS
               ELSE
                  MOVE  3004                TO  MSG1-CD-ERRO-TS
                  STRING  'Erro ao acionar o módulo ' NM-PGM-CHMD '.'
                            DELIMITED BY SIZE
                            INTO        MSG1-TX-ERRO-TS
                  END-STRING
                  MOVE NM-PGM-CHMD          TO  MSG1-NM-PGM-ACND
                  MOVE IIBKERRO-CD-ERRO     TO  MSG1-CD-RTN-PGM-ACND
                  MOVE IIBKERRO-TX-ERRO     TO  MSG1-TX-RTN-PGM-ACND
               END-IF
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              WHEN OTHER
                   SET ERRO-CONTAINER       TO  TRUE
                   PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       971000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       972000-LER-RESPOSTA             SECTION.
      *-----------------------------------------------------------------
           MOVE 0021                   TO MSG1-CD-SEQL-ERRO

      *--> Inicializa quantidade de caracteres lidos na área de resposta
           MOVE ZEROS                  TO QT-CCT-LIDO

           MOVE NM-PGM-CHMD            TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

      *--> Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *--> Por causa da captura automática erro CICS, deve ser previsto
      *--> o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Trata área de resposta do programa acionado (NODATA)
           PERFORM 972100-TRATA-AREA-RESPOSTA

      *--> Recupera o container de resposta do programa que foi acionado
           EVALUATE NM-PGM-CHMD

      *--> Lê container de resposta do SGTSL009
               WHEN SGTSL009
                 MOVE LENGTH OF SGTSL009-RPST TO QT-CCT-EPRO
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSL009-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC

      *--> Lê container de resposta do SGTSA063
               WHEN SGTSA063
                 MOVE LENGTH OF SGTSA063-RPST TO QT-CCT-EPRO
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSA063-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC

      *--> Lê container de resposta do SGTSI094
               WHEN SGTSI094
                 MOVE LENGTH OF SGTSI094-RPST TO QT-CCT-EPRO
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSI094-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC

      *--> Lê container de resposta do SGTSL063
               WHEN SGTSL063
                 MOVE LENGTH OF SGTSL063-RPST TO QT-CCT-EPRO
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSL063-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC

      *--> Lê container de resposta do SGTSL094
               WHEN SGTSL094
                 MOVE LENGTH OF SGTSL094-RPST TO QT-CCT-EPRO
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSL094-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC

      *--> Lê container de resposta do SGTSI069
               WHEN SGTSI069
                 MOVE LENGTH OF SGTSI069-RPST TO QT-CCT-EPRO
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSI069-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC

      *--> Lê container de resposta do SGTSI078
               WHEN SGTSI078
                 MOVE LENGTH OF SGTSI078-RPST TO QT-CCT-EPRO
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSI078-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC

      *--> Lê container de resposta do SGTSLB75
               WHEN SGTSLB75
                 MOVE LENGTH OF SGTSLB75-RPST TO QT-CCT-EPRO
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSLB75-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC

               WHEN OTHER
                 SET SEM-ERRO        TO TRUE
                 SET ERRO-PROGRAMADO TO TRUE
                 MOVE 9003           TO MSG1-CD-ERRO-TS
                 STRING
                    'Erro na leitura da resposta do módulo '
                     NM-PGM-CHMD '.'
                     DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                 END-STRING
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       972000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       972100-TRATA-AREA-RESPOSTA      SECTION.
      *-----------------------------------------------------------------
           MOVE 0022                   TO MSG1-CD-SEQL-ERRO

      *--> Recupera o tamanho previsto e o tamanho recebido da resposta
      *--> do programa que foi acionado.

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Obtém a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO

           EXEC CICS GET
                CONTAINER (NM-CTNR    )
                NODATA
                FLENGTH   (QT-CCT-LIDO)
           END-EXEC
           .
       972100-SAI.
           EXIT.
      *----------------------------------------------------------------*
       974000-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE 0023                   TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           MOVE CTNR-PRM-INF-OPRD      TO NM-CTNR

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto erro de container
           SET  ERRO-CONTAINER         TO TRUE

           INITIALIZE PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       974000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       982000-GRAVA-REQUISICAO         SECTION.
      *-----------------------------------------------------------------
           MOVE 0024                   TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para gravação do container de requisição
           SET  PUT-CONTAINER          TO TRUE

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *--> nome_modulo -RQSC

           MOVE NM-PGM-CHMD            TO NM-CTNR(1:8)
           MOVE '-RQSC'                TO NM-CTNR(9:5)

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Grava container da requisição do serviço IIB a ser acionado
      *--> Em caso de erro, a captura automática do CICS trata o erro

           EVALUATE NM-PGM-CHMD
      *--> Grava container de requisição do programa SGTSL009
               WHEN SGTSL009
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTSL009-RQSC           )
                            FLENGTH   ( LENGTH OF SGTSL009-RQSC )
                  END-EXEC

      *--> Grava container de requisição do programa SGTSA063
               WHEN SGTSA063
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTSA063-RQSC           )
                            FLENGTH   ( LENGTH OF SGTSA063-RQSC )
                  END-EXEC

      *--> Grava container de requisição do programa SGTSI094
               WHEN SGTSI094
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTSI094-RQSC           )
                            FLENGTH   ( LENGTH OF SGTSI094-RQSC )
                  END-EXEC

      *--> Grava container de requisição do programa SGTSL063
               WHEN SGTSL063
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTSL063-RQSC           )
                            FLENGTH   ( LENGTH OF SGTSL063-RQSC )
                  END-EXEC

      *--> Grava container de requisição do programa SGTSL094
               WHEN SGTSL094
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTSL094-RQSC           )
                            FLENGTH   ( LENGTH OF SGTSL094-RQSC )
                  END-EXEC

      *--> Grava container de requisição do programa SGTSI069
               WHEN SGTSI069
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTSI069-RQSC           )
                            FLENGTH   ( LENGTH OF SGTSI069-RQSC )
                  END-EXEC

      *--> Grava container de requisição do programa SGTSI078
               WHEN SGTSI078
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTSI078-RQSC           )
                            FLENGTH   ( LENGTH OF SGTSI078-RQSC )
                  END-EXEC

      *--> Grava container de requisição do programa SGTSLB75
               WHEN SGTSLB75
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTSLB75-RQSC           )
                            FLENGTH   ( LENGTH OF SGTSLB75-RQSC )
                  END-EXEC

      *--> Grava container de requisição do programa SGTS005B
               WHEN SGTS005B
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTS005B-RQSC           )
                            FLENGTH   ( LENGTH OF SGTS005B-RQSC )
                  END-EXEC
      *--> Módulo não previsto para gravar container de requisição
               WHEN OTHER
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 9004             TO MSG1-CD-ERRO-TS
                 STRING
                    'Erro na gravação da requisição do módulo.'
                    NM-PGM-CHMD '.'
                     DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                 END-STRING
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       982000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0025                   TO MSG1-CD-SEQL-ERRO
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *--> nome_modulo -RPST

           MOVE NM-PGM-CHMD            TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

           SET DEL-CONTAINER           TO TRUE

      *--> Exclui o container de resposta
      *--> Não há captura automática de erro CICS por usar RESP e RESP2
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

      *--> Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *----------------------------------------------------------------*
           MOVE 0026                   TO MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE CTNR-PRM-ERRO          TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
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
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  ERRO-PROGRAMA-ACIONADO
               PERFORM 999100-MONITORAR-VRV-PGM-CHMD
           END-IF

           PERFORM 999200-MONITORAR-VRV-PGM-E-ENC
           .
       999000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999100-MONITORAR-VRV-PGM-CHMD   SECTION.
      *-----------------------------------------------------------------
      *--> Monitora as variáveis dos módulos chamados

           EVALUATE  NM-PGM-CHMD

      *-->>SGTST009<<
               WHEN SGTST009
                    MOVE 'SGTST009-ACAO'         TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST009-ACAO          TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST009-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST009-CD-CTRA-TIT-VL
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST009-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                    IF  SGTST009-CD-OPR-ETRD-MCP
                                                 IS NUMERIC
                        MOVE  SGTST009-CD-OPR-ETRD-MCP
                                                 TO MCR-S9-15
                        MOVE  MCR-S9-15          TO MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE  'Valor não numérico'
                                                 TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST009-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST009-NR-VRS-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST009-TS-EST-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST009-TS-EST-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST009-CD-USU-RSP-ATL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST009-CD-USU-RSP-ATL
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTSL009<<
               WHEN SGTSL009
                    MOVE  'SGTSL009-E-CD-TIP-SRVC'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL009-E-CD-TIP-SRVC TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL009-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL009-CD-CTRA-TIT-VL
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL009-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                    IF  SGTSL009-CD-OPR-ETRD-MCP
                                                 IS NUMERIC
                        MOVE  SGTSL009-CD-OPR-ETRD-MCP
                                                 TO MCR-S9-15
                        MOVE  MCR-S9-15          TO MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE  'Valor não numérico'
                                                 TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL009-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL009-NR-VRS-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL009-TS-EST-OPR-ETRD-ULT'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL009-TS-EST-OPR-ETRD-ULT
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTST005<<
               WHEN SGTST005
                    MOVE 'SGTST005-ACAO'         TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST005-ACAO          TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST005-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST005-CD-CTRA-TIT-VL
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST005-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                    IF  SGTST005-CD-OPR-ETRD-MCP
                                                 IS NUMERIC
                        MOVE  SGTST005-CD-OPR-ETRD-MCP
                                                 TO MCR-S9-15
                        MOVE  MCR-S9-15          TO MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE  'Valor não numérico'
                                                 TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST005-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST005-NR-VRS-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST005-CD-RTN-PGM'  TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST005-CD-RTN-PGM    TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST005-MSG-ERRO-PGM'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST005-MSG-ERRO-PGM  TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTSI069<<
               WHEN SGTSI069
                    MOVE  'SGTSI069-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI069-CD-CTRA-TIT-VL
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI069-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                    IF  SGTSI069-CD-OPR-ETRD-MCP
                                                 IS NUMERIC
                        MOVE  SGTSI069-CD-OPR-ETRD-MCP
                                                 TO MCR-S9-15
                        MOVE  MCR-S9-15          TO MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE  'Valor não numérico'
                                                 TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI069-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI069-NR-VRS-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI069-CD-TIP-EST-VRS'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI069-CD-TIP-EST-VRS
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI069-TX-OBS-EST-VRS'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI069-TX-OBS-EST-VRS
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTSI094<<
               WHEN SGTSI094
                    MOVE  'SGTSI094-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI094-CD-CTRA-TIT-VL
                                                 TO MCR-S9-15
                    MOVE  MCR-S9-15              TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI094-CD-TIT-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI094-CD-TIT-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI094-NR-DVS-TIT'  TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI094-NR-DVS-TIT    TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI094-TS-ATL-IFLC-MNL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI094-TS-ATL-IFLC-MNL
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI094-IN-IFLC-MNL-EXCO'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI094-IN-IFLC-MNL-EXCO
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI094-CD-USU-RSP-ULT-ATL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI094-CD-USU-RSP-ULT-ATL
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTSA063<<
               WHEN SGTSA063
                    MOVE  'SGTSA063-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSA063-CD-CTRA-TIT-VL
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSA063-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                    IF  SGTSA063-CD-OPR-ETRD-MCP IS NUMERIC
                        MOVE  SGTSA063-CD-OPR-ETRD-MCP
                                                 TO MCR-S9-15
                        MOVE  MCR-S9-15          TO MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE  'Valor não numérico'
                                                 TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSA063-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSA063-NR-VRS-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSA063-CD-TIP-IN'   TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSA063-CD-TIP-IN     TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSA063-NR-IN-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSA063-NR-IN-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTSL063<<
               WHEN SGTSL063
                    MOVE  'SGTSL063-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL063-CD-CTRA-TIT-VL
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL063-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                    IF  SGTSL063-CD-OPR-ETRD-MCP IS NUMERIC
                        MOVE  SGTSL063-CD-OPR-ETRD-MCP
                                                 TO MCR-S9-15
                        MOVE  MCR-S9-15          TO MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE  'Valor não numérico'
                                                 TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL063-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL063-NR-VRS-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL063-CD-TIP-IN-E' TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL063-CD-TIP-IN-E   TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL063-NR-IN-OPR-ETRD-E'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL063-NR-IN-OPR-ETRD-E
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTSL094<<
               WHEN SGTSL094
                    MOVE  'SGTSL094-E-CD-TIP-SRVC'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL094-E-CD-TIP-SRVC TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL094-E-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL094-E-CD-CTRA-TIT-VL
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL094-E-CD-TIT-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL094-E-CD-TIT-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL094-E-NR-DVS-TIT'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL094-E-NR-DVS-TIT  TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSL094-E-TS-ATL-IFLC-MNL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL094-E-TS-ATL-IFLC-MNL
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT
      *-->>SGTSI078<<
               WHEN SGTSI078
                    MOVE  'SGTSI078-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI078-CD-CTRA-TIT-VL TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

      *-----------> Verifica se o conteudo e numerico antes de adicionar
                    IF  SGTSI078-CD-OPR-ETRD-MCP IS NUMERIC
                        MOVE  SGTSI078-CD-OPR-ETRD-MCP
                                                 TO MCR-S9-15
                        MOVE  MCR-S9-15          TO MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE  'Valor não numérico'
                                                 TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI078-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI078-NR-VRS-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI078-NR-SEQL-ORD-NEGO'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI078-NR-SEQL-ORD-NEGO
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSI078-TS-EST-ORD-NEGO'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSI078-TS-EST-ORD-NEGO
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT
      *-->>SGTSLB75<<
               WHEN SGTSLB75
                    MOVE  'SGTSLB75-E-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLB75-E-CD-CTRA-TIT-VL
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

      *-----------> Verifica se o conteudo e numerico antes de adicionar
                    IF  SGTSLB75-E-CD-OPR-ETRD-MCP IS NUMERIC
                        MOVE  SGTSLB75-E-CD-OPR-ETRD-MCP
                                                 TO MCR-S9-15
                        MOVE  MCR-S9-15          TO MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE  'Valor não numérico'
                                                 TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLB75-E-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLB75-E-NR-VRS-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLB75-E-NR-SEQL-ORD-PAG'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLB75-E-NR-SEQL-ORD-PAG
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT
      *-->>SGTS005B<<
               WHEN SGTS005B
                    MOVE  'SGTS005B-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTS005B-CD-CTRA-TIT-VL TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

      *-----------> Verifica se o conteudo e numerico antes de adicionar
                    MOVE  'SGTS005B-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
                    IF  SGTS005B-CD-OPR-ETRD-MCP IS NUMERIC
                        MOVE  SGTS005B-CD-OPR-ETRD-MCP
                                                 TO MCR-S9-15
                        MOVE  MCR-S9-15          TO MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE  'Valor não numérico'
                                                 TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTS005B-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTS005B-NR-VRS-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                    CONTINUE
           END-EVALUATE
           .
       999100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999200-MONITORAR-VRV-PGM-E-ENC  SECTION.
      *-----------------------------------------------------------------
      *--> Monitorar variàveis do pgm, grava e encerra processamento

      *--> Em caso de erro, desfaz as atualizaçoes em banco de dados
           EXEC CICS
                   SYNCPOINT  ROLLBACK
           END-EXEC

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Verifica se o tipo de erro gerado possui tratamento interno
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

      *--> Adiciona as variáveis para monitoração
           MOVE 'SGTS0237-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0237-CD-CTRA-TIT-VL     TO MCR-S9-15
           MOVE  MCR-S9-15                   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTS0237-CD-OPR-ETRD-MCP'  TO MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF SGTS0237-CD-OPR-ETRD-MCP  NUMERIC
              MOVE SGTS0237-CD-OPR-ETRD-MCP  TO MCR-S9-15
              MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
           ELSE
              MOVE  'Valor não numérico'     TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0237-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0237-NR-VRS-OPR-ETRD    TO MCR-S9-15
           MOVE  MCR-S9-15                   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0237-TX-JST-EST'        TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0237-TX-JST-EST         TO MCR-S9-15
           MOVE  MCR-S9-15                   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *--> Monitora variáveis dinamicas de mensagem.
           MOVE 'TX-DTQ-MSG'                     TO  MSGKW999-NM-VRV-MNT
           MOVE  TX-DTQ-MSG                      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'AUX-CD-TIP-EST-NEGO'            TO  MSGKW999-NM-VRV-MNT
           MOVE  AUX-CD-TIP-EST-NEGO             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO

           EXEC CICS PUT
              CONTAINER ( CTNR-PRM-ERRO               )
              CHANNEL   ( NM-CNL                      )
              FROM      ( IIBKERRO-PRM-ERRO           )
              FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
              NOHANDLE
           END-EXEC

           PERFORM 999999-ENCERRAR
           .
       999200-SAI.
           EXIT.
      *---------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      *---------------------------------------*
-INC HLPKP999

      *---------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999

      *----------------------------------------
      * 999010-TRATA-ERRO-DB2          SECTION.
      *----------------------------------------
      * -INC HLPKP997

      *---------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
      *---------------------------------------*
-INC MSGKPMG1

      *---------------------------------------*
       999999-ENCERRAR                 SECTION.
      *---------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTS0237 ----------------------------