       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0236
      * ANALISTA..: F8785286 - Roseli Akemi Kawahata
      * AUTOR.....: F0950413 - António Francisco Mendonça Silva
      * DATA......: 06/11/2018
      * OBJETIVO..: Negócio - Confirmar a Revisão do Cadastro da Opera-
      *             ção Estruturada de Mercado de Capitais.
      * COMPILACAO: COBOL/CICS/DB2 (On-line)
      *-----------------------------------------------------------------
      * VRS002 26.11.2019 F2804421 DE 2016/145195 - Monitorar variáveis
      *        dinamicas da mensagem 1106.
      *        14.11.2019 - F9188847 - 2016/145195 - Inclusao Sections:
      *  160000-CONSISTE-ORD-NEGO e 500000-ATUALIZA-ESTADO-ORDEM
      * VRS001 02.01.2019 - F8785286 - 2016/145195 - Inclusão chamada
      *             dos módulos SGTSA083 e SGTSL083.
      *             Exclusão chamada do módulo SGTSI089.
      *        27.12.2018 - F2804421 - 2016/145195 - Inclusão chamada
      *                                do módulo SGTSI089.
      * VRS001 06.11.2018 - F0950413 - 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0236.

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
               '*** SGTS0236 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS002'.

      *--> Módulos a serem acionados via 'EXEC CICS LINK (COMMAREA)'
       77  SGTST009                    PIC X(08) VALUE 'SGTST009'.

      *--> Módulos a serem acionados via 'CONTAINER'
       77  SGTSI069                    PIC X(08) VALUE 'SGTSI069'.
       77  SGTSA083                    PIC X(08) VALUE 'SGTSA083'.
       77  SGTSI083                    PIC X(08) VALUE 'SGTSI083'.
       77  SGTSL083                    PIC X(08) VALUE 'SGTSL083'.
      *77  SGTSI089                    PIC X(08) VALUE 'SGTSI089'.
       77  SGTSI095                    PIC X(08) VALUE 'SGTSI095'.
       77  SGTS0163                    PIC X(08) VALUE 'SGTS0163'.
       77  SGTSLB75                    PIC X(08) VALUE 'SGTSLB75'.
       77  SGTSI078                    PIC X(08) VALUE 'SGTSI078'.

      *--> Texto destaque para mensagem dinamica.
       77  TX-DTQ-MSG                  PIC  X(30)     VALUE
                                       'confirmar revisão do cadastro'.
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

      *--> Variaveis utilizados para leitura
       01  REG-WS.
           03 CD-USU-RSP-EST           PIC  X(08)     VALUE ' '.
           03 CD-TIP-EST-OPR           PIC S9(09)     COMP-5 VALUE 0.
           03 TX-JST-EST               PIC  X(100)    VALUE ' '.
           03 CD-MTV-ALT               PIC S9(04)     COMP-5 VALUE 0.
           03 CD-TIP-EST-VRS           PIC S9(04)     COMP-5 VALUE 0.

      *--> Indicadores de nulidades
       01  CD-TIP-EST-OPR-N            PIC S9(04)     COMP-5 VALUE 0.
       01  TX-JST-EST-N                PIC S9(04)     COMP-5 VALUE 0.
       01  CD-MTV-ALT-N                PIC S9(04)     COMP-5 VALUE 0.

      *--> Variáveis de trabalho
       01  IC-TAB                      PIC S9(04)     COMP-5  VALUE   0.
       01  AUX-CD-TIP-EST-NEGO         PIC  9999.
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
       01  MCR-S9-15                   PIC  ---------------9.
       01  MCR-S9-15-V9-04             PIC  ----.---.---.---.--9,9999.

      *-----------------------------------------------------------------
      *--> Manutenção da tabela 'DB2SGT.EST_OPR_ETRD'
       01  SGTST009-DADOS.
-INC SGTKT009

      *--> Inclusão na tabela 'DB2SGT.EST_VRS_OPR_ETRD'
       01  SGTSI069-DADOS.
-INC SGTKI069

      *--> Alteração Tabela 'DB2SGT.EXCO_PRFL_SELD'
-INC SGTKA083

      *--> Inclusão Tabela 'DB2SGT.EXCO_PRFL_SELD'
-INC SGTKI083

      *--> Lista Tabela 'DB2SGT.EXCO_PRFL_SELD'
-INC SGTKL083

      *--> Inclusão Tabela 'DB2SGT.ITC_RGR_VLDC_CRCT'
      *01  SGTSI089-DADOS.
      *      -INC SGTKI089

      *--> Inclusão da tabela 'DB2SGT.ITEM_EXCO_OPR'
-INC SGTKI095

      * Lista da tabela DB2SGT.ORD_NEGO_OPR e DB2SGT.MOD_ORD_NEGO
-INC SGTKLB75

      *--> Inclusão na Tabela EST_ORD_NEGO_OPR.
       01  SGTSI078-DADOS.
-INC SGTKI078

      *--> Negocial verifica se operação entra ou não em trat. exceção
-INC SGTK0163

      *--> Book do programa
       01  SGTS0236-DADOS.
-INC SGTK0236

      *--> Area de trabalho SQL/ DB2.
           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

       01  CTE-FIM-LS                  PIC  X(32)  VALUE
                                             '*** LS TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997
      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999
           MOVE CTE-PROG(5:8)          TO NM-PGM
                                          NM-PGM-CHMD

           PERFORM 100000-OBTER-NOME-DO-CANAL
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
           PERFORM 970000-LER-REQUISICAO-RECEBIDA

           PERFORM 140000-VALIDAR-PARAMETROS
           PERFORM 974000-LER-DADOS-OPRD
           PERFORM 150000-CONFIRMACAO-ESTADO-OPR

           PERFORM 160000-VALOR-INICIAL-VARIAVEIS

           PERFORM 170000-CONSISTE-ORD-NEGO
           PERFORM 200000-VER-EXCECAO-ATIVA-OPR
           PERFORM 210000-VER-EXCECAO-OPR-REVISAO
           PERFORM 300000-FINALIZA-CADASTRAMENTO
           PERFORM 400000-ATUALIZA-ESTADO-VERSAO
           PERFORM 500000-ATUALIZA-ESTADO-ORDEM

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

           IF  SGTS0236-CD-CTRA-TIT-VL  NOT > ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de títulos e valores mobiliários
      -             ' inválido.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0236-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
            IF SGTS0236-CD-OPR-ETRD-MCP = 0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0005               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' informado.'      TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
            END-IF
           END-IF

           IF  SGTS0236-NR-VRS-OPR-ETRD NOT > ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0006               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       150000-CONFIRMACAO-ESTADO-OPR   SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                SELECT A.CD_USU_RSP_EST
                     , A.CD_TIP_EST_OPR
                     , A.TX_JST_EST
                     , A.CD_MTV_ALT
                     , B.CD_TIP_EST_VRS
                  INTO  :CD-USU-RSP-EST
                     ,  :CD-TIP-EST-OPR
                        :CD-TIP-EST-OPR-N
                     ,  :TX-JST-EST
                        :TX-JST-EST-N
                     ,  :CD-MTV-ALT
                        :CD-MTV-ALT-N
                     ,  :CD-TIP-EST-VRS
                FROM DB2SGT.EST_OPR_ETRD  A
                     JOIN DB2SGT.EST_VRS_OPR_ETRD  B
                       ON   B.CD_CTRA_TIT_VL  =  A.CD_CTRA_TIT_VL
                       AND  B.CD_OPR_ETRD_MCP =  A.CD_OPR_ETRD_MCP
                       AND  B.NR_VRS_OPR_ETRD =  A.NR_VRS_OPR_ETRD
                WHERE A.CD_CTRA_TIT_VL    = :SGTS0236-CD-CTRA-TIT-VL
                  AND A.CD_OPR_ETRD_MCP   = :SGTS0236-CD-OPR-ETRD-MCP
                  AND A.NR_VRS_OPR_ETRD   = :SGTS0236-NR-VRS-OPR-ETRD
                  AND B.TS_EST_VRS = (SELECT MAX (C.TS_EST_VRS)
                FROM DB2SGT.EST_VRS_OPR_ETRD C
                     WHERE C.CD_CTRA_TIT_VL  =  A.CD_CTRA_TIT_VL
                       AND C.CD_OPR_ETRD_MCP =  A.CD_OPR_ETRD_MCP
                       AND C.NR_VRS_OPR_ETRD =  A.NR_VRS_OPR_ETRD)
                ORDER BY A.TS_EST_OPR_ETRD DESC
                FETCH FIRST 1 ROW ONLY
           END-EXEC

      *--> Consistência do Operador Logado
           IF  CD-USU-RSP-EST              =  KOPRD-CD-OPRD
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0865               TO MSG1-CD-ERRO-TS
               MOVE 'Operador logado deve ser diferente do operador que
      -             'efetuou a edição da operação estruturada.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Consistência da permissão para confirmação de revisão
           IF (CD-TIP-EST-OPR     NOT = 1      OR
               CD-TIP-EST-VRS     NOT = 1      OR
               CD-MTV-ALT         NOT = 999)
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0871               TO MSG1-CD-ERRO-TS
               MOVE 'Estado da operação não permite confirmação de revis
      -             'ão.'              TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       150000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       160000-VALOR-INICIAL-VARIAVEIS  SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           SET SGTSL083-LISTA-EXCO-ATV      TO TRUE
           SET  SGTST009-INCLUIR       TO TRUE

           MOVE SGTS0236-CD-CTRA-TIT-VL     TO SGTSI069-CD-CTRA-TIT-VL
                                               SGTSA083-CD-CTRA-TIT-VL
                                               SGTSI083-CD-CTRA-TIT-VL
                                               SGTSL083-CD-CTRA-TIT-VL
                                               SGTSI095-CD-CTRA-TIT-VL
                                               SGTST009-CD-CTRA-TIT-VL
                                               SGTS0163-CD-CTRA-TIT-VL
                                               SGTSI078-CD-CTRA-TIT-VL

           MOVE SGTS0236-CD-OPR-ETRD-MCP    TO SGTSI069-CD-OPR-ETRD-MCP
                                               SGTSA083-CD-OPR-ETRD-MCP
                                               SGTSI083-CD-OPR-ETRD-MCP
                                               SGTSL083-CD-OPR-ETRD-MCP
                                               SGTSI095-CD-OPR-ETRD-MCP
                                               SGTST009-CD-OPR-ETRD-MCP
                                               SGTS0163-CD-OPR-ETRD-MCP
                                               SGTSI078-CD-OPR-ETRD-MCP

           MOVE SGTS0236-NR-VRS-OPR-ETRD    TO SGTSI069-NR-VRS-OPR-ETRD
                                               SGTSA083-NR-VRS-OPR-ETRD
                                               SGTSI083-NR-VRS-OPR-ETRD
                                               SGTSL083-NR-VRS-OPR-ETRD
                                               SGTSI095-NR-VRS-OPR-ETRD
                                               SGTST009-NR-VRS-OPR-ETRD
                                               SGTS0163-NR-VRS-OPR-ETRD
                                               SGTSI078-NR-VRS-OPR-ETRD

           MOVE KOPRD-CD-OPRD          TO SGTSA083-CD-USU-ULT-ATL
                                          SGTSI083-CD-USU-ULT-ATL
                                          SGTST009-CD-USU-RSP-ATL
                                          SGTST009-CD-USU-RSP-EST
                                          SGTSI078-CD-USU-ULT-ATL

           MOVE 'CURRENT'              TO SGTSA083-TS-ULT-ATL
                                          SGTSI083-TS-ULT-ATL
                                          SGTST009-TS-EST-OPR-ETRD
                                          SGTSI078-TS-EST-ORD-NEGO

           MOVE 0                      TO SGTSI083-NR-SEQL-EXCO-PRFL
                                          SGTSL083-E-NR-SEQL-EXCO-PRFL
                                          SGTSL083-CD-RGR-VLDC
                                          SGTSL083-NR-VRS-RGR-VLDC
                                          SGTSI095-NR-SEQL-ITEM-EXCO
                                          SGTS0163-QT-OCR-TAB-E
                                          SGTST009-CD-MTV-ALT

           MOVE 2                      TO SGTSI069-CD-TIP-EST-VRS
                                          SGTST009-CD-TIP-EST-OPR

           MOVE SPACES                 TO SGTSI069-TX-OBS-EST-VRS
                                          SGTSI083-TX-OBS-EXCO-PRFL
      *                                   SGTSI089-TX-DCR-ITC
                                          SGTSI095-TX-OBS-ITEM-EXCO
                                          SGTSI078-TX-JST-EST-ORD

           MOVE 'S'                    TO SGTSI083-IN-EXCO-ATV

           MOVE 'N'                    TO SGTSA083-IN-EXCO-ATV

           MOVE 'Inativado por reavaliação de exceção - conferir cadastr
      -           'o.'                 TO SGTSA083-TX-OBS-EXCO-PRFL

           .
       160000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       170000-CONSISTE-ORD-NEGO        SECTION.
      *    Consulta se existe vinculo com modelo de ordem de compra,
      *    se está ativo e se o estado da última ordem está Em revisão
      *
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSLB75-RQSC

001530     MOVE SGTSLB75               TO  NM-PGM-CHMD

001530     MOVE SGTS0236-CD-CTRA-TIT-VL
001530                                 TO  SGTSLB75-E-CD-CTRA-TIT-VL
001530     MOVE SGTS0236-CD-OPR-ETRD-MCP
001530                                 TO  SGTSLB75-E-CD-OPR-ETRD-MCP
001530     MOVE SGTS0236-NR-VRS-OPR-ETRD
001530                                 TO  SGTSLB75-E-NR-VRS-OPR-ETRD

           MOVE ZEROES                 TO  SGTSLB75-E-NR-SEQL-ORD-PAG

           PERFORM 900000-PROCEDIMENTOS-CONTAINER

           IF SGTSLB75-QT-OCR-TAB     <=   ZEROS
              SET   ERRO-PROGRAMA-ACIONADO   TO  TRUE
              MOVE  0932              TO  MSG1-CD-ERRO-TS
              MOVE  'Não existe modelo de compra vinculado a esta operaç
      -              'ção.'           TO  MSG1-TX-ERRO-TS
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE

      *        CD-TIP-EST-NEGO = 2 - Em revisão
              IF  SGTSLB75-CD-TIP-EST-NEGO(1)     NOT EQUAL 2
               SET  ERRO-PROGRAMA-ACIONADO    TO TRUE
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

              IF SGTSLB75-CD-TIP-EST-DCS(1)  NOT EQUAL 2
                 SET   ERRO-PROGRAMA-ACIONADO   TO  TRUE
                 MOVE  0916              TO  MSG1-CD-ERRO-TS
                 MOVE  'Modelo vinculado à ordem de negociação deve esta
      -                 'r aprovado.'       TO  MSG1-TX-ERRO-TS
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-IF

              IF SGTSLB75-IN-MOD-ORD-ATI(1)  NOT EQUAL 'S'
                 SET   ERRO-PROGRAMA-ACIONADO   TO  TRUE
                 MOVE  0917              TO  MSG1-CD-ERRO-TS
                 MOVE  'Modelo de ordem de compra deve estar ativo.'
                                         TO  MSG1-TX-ERRO-TS
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-IF
           END-IF
           .
       170000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-VER-EXCECAO-ATIVA-OPR    SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSL083               TO NM-PGM-CHMD
           PERFORM 900000-PROCEDIMENTOS-CONTAINER

      *--> Desativa a exceção ativa para a operação
      *--> SGTSL083-QT-OCR-TAB > 0
           PERFORM VARYING IC-TAB FROM 1 BY 1
                        UNTIL IC-TAB > SGTSL083-QT-OCR-TAB
                   MOVE SGTSL083-NR-SEQL-EXCO-PRFL (IC-TAB)
                                       TO SGTSA083-NR-SEQL-EXCO-PRFL
                   MOVE SGTSA083       TO NM-PGM-CHMD
                   PERFORM 900000-PROCEDIMENTOS-CONTAINER
           END-PERFORM
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       210000-VER-EXCECAO-OPR-REVISAO  SECTION.
      *-----------------------------------------------------------------
      *--> Consultar se gera exceção p/ operação que está sendo revisada
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTS0163               TO NM-PGM-CHMD
           PERFORM 900000-PROCEDIMENTOS-CONTAINER

           IF SGTS0163-QT-OCR-TAB > 0
      *--> Gravar exceção ativa para a operação
              MOVE SGTSI083            TO NM-PGM-CHMD
              PERFORM 900000-PROCEDIMENTOS-CONTAINER

              PERFORM 211000-GRAVAR-ITENS-EXCO
                        VARYING IC-TAB FROM 1 BY 1
                          UNTIL IC-TAB > SGTS0163-QT-OCR-TAB
           END-IF
           .
       210000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       211000-GRAVAR-ITENS-EXCO  SECTION.
      *-----------------------------------------------------------------
      *--> Gravar itens da exceção ativa para a operação
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSI083-NR-SEQL-EXCO-PRFL-S
                                       TO SGTSI095-NR-SEQL-EXCO-PRFL
      *    MOVE SGTS0163-CD-RGR-PPL         (IC-TAB)
           MOVE SGTS0163-CD-RGR-SCDR        (IC-TAB)
                                       TO SGTSI095-CD-RGR-VLDC
      *    MOVE SGTS0163-NR-VRS-RGR-PPL     (IC-TAB)
           MOVE SGTS0163-NR-VRS-RGR-SCDR     (IC-TAB)
                                       TO SGTSI095-NR-VRS-RGR-VLDC
      *    MOVE SGTS0163-NR-SEQL-ITC        (IC-TAB)
           MOVE SGTS0163-NR-SEQL-OGNL        (IC-TAB)
                                       TO SGTSI095-NR-SEQL-ITC
           MOVE SGTS0163-IN-LNH-EXCO        (IC-TAB)
                                       TO SGTSI095-IN-ITEM-CRIO-EXCO

           IF SGTS0163-VL-ITC-RLZD (IC-TAB)     NOT NUMERIC
              MOVE ZEROES              TO SGTSI095-VL-RLZD-ITEM-EXCO
           ELSE
              MOVE SGTS0163-VL-ITC-RLZD        (IC-TAB)
                                       TO SGTSI095-VL-RLZD-ITEM-EXCO
           END-IF

           MOVE SGTS0163-TX-ITC-RLZD        (IC-TAB)
                                       TO SGTSI095-TX-RLZD-ITEM-EXCO
           MOVE SGTS0163-NR-ITC-RLZD        (IC-TAB)
                                       TO SGTSI095-NR-RLZD-ITEM-EXCO

           MOVE SGTSI095               TO NM-PGM-CHMD
           PERFORM 900000-PROCEDIMENTOS-CONTAINER
           .
       2110000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-FINALIZA-CADASTRAMENTO   SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           PERFORM 950000-ATUALIZA-EST-OPERACAO

           IF SGTST009-CD-RTN-PGM NOT EQUAL ZEROS AND +100
              SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
              MOVE 0099                TO MSG1-CD-ERRO-TS
              MOVE 'Código do tipo do estado  da operação inválido.'
                                       TO MSG1-TX-ERRO-TS
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

           MOVE SGTSI078               TO NM-PGM-CHMD

           MOVE SGTSLB75-NR-SEQL-ORD-NEGO(1)
                                       TO SGTSI078-NR-SEQL-ORD-NEGO
      ***   3 - Autorizar
           MOVE 3                      TO SGTSI078-CD-TIP-EST-NEGO

           PERFORM 900000-PROCEDIMENTOS-CONTAINER
           .
       500000-SAI.
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

      *--> Lê o container de resposta da operação acionada
           PERFORM 972000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       910000-EXECUTA-MODULOS          SECTION.
      *-----------------------------------------------------------------
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM-CHMD
      *--> Executar Módulo SGTSI069, container sem Serviço
               WHEN SGTSI069
                 EXEC CICS LINK
                           PROGRAM ( SGTSI069  )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC

      *--> Executar Módulo SGTSA083, container sem Serviço
               WHEN SGTSA083
                 EXEC CICS LINK
                           PROGRAM ( SGTSA083  )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC

      *--> Executar Módulo SGTSI083, container sem Serviço
               WHEN SGTSI083
                 EXEC CICS LINK
                           PROGRAM ( SGTSI083  )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC

      *--> Executar Módulo SGTSL083, container sem Serviço
               WHEN SGTSL083
                 EXEC CICS LINK
                           PROGRAM ( SGTSL083  )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC

      *--> Executar Módulo SGTSI089, container sem Serviço
      *        WHEN SGTSI089
      *          EXEC CICS LINK
      *                    PROGRAM ( SGTSI089  )
      *                    CHANNEL ( NM-CNL    )
      *                    RESP    ( MSG1-RESP1)
      *                    RESP2   ( MSG1-RESP2)
      *          END-EXEC
      *
      *--> Executar Módulo SGTSI095, container sem Serviço
               WHEN SGTSI095
                 EXEC CICS LINK
                           PROGRAM ( SGTSI095  )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC

      *--> Executar Módulo SGTS0163, container sem Serviço
               WHEN SGTS0163
                 EXEC CICS LINK
                           PROGRAM ( SGTS0163  )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC
      *--> Executar Módulo SGTSLB75 container sem Serviço
               WHEN SGTSLB75
                 EXEC CICS LINK
                           PROGRAM ( SGTSLB75  )
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

               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE

               MOVE NM-PGM             TO MSG1-NM-PGM-ACND
               MOVE SGTST009-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE SGTST009-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS

               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       950000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970000-LER-REQUISICAO-RECEBIDA  SECTION.
      *-----------------------------------------------------------------
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisição
           MOVE 'SGTS0236'             TO NM-PGM
           MOVE 'SGTS0236-RQSC'        TO NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM 970100-LER-CONTAINER-NODATA

      *--> Obtem o tamanho esperado da requisição
           MOVE LENGTH OF SGTS0236-RQSC TO QT-CCT-EPRO

      *--> Valida tamanho esperado da requisição X tamanho recebido

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTS0236-RQSC' )
              CHANNEL   ( NM-CNL          )
              FLENGTH   ( QT-CCT-LIDO     )
              INTO      ( SGTS0236-RQSC   )
           END-EXEC
           .
       970000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       970100-LER-CONTAINER-NODATA     SECTION.
      *----------------------------------------------------------------*
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO

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
               MOVE NM-PGM-CHMD          TO  MSG1-NM-PGM-ACND

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
           MOVE 0020                   TO MSG1-CD-SEQL-ERRO

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

      *--> Lê container de resposta do SGTSI069
               WHEN SGTSI069
                 MOVE LENGTH OF SGTSI069-RPST TO QT-CCT-EPRO
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSI069-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC

      *--> Lê container de resposta do SGTSA083
               WHEN SGTSA083
                 MOVE LENGTH OF SGTSA083-RPST TO QT-CCT-EPRO
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSA083-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC

      *--> Lê container de resposta do SGTSI083
               WHEN SGTSI083
                 MOVE LENGTH OF SGTSI083-RPST TO QT-CCT-EPRO
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSI083-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC

      *--> Lê container de resposta do SGTSL083
               WHEN SGTSL083
                 MOVE LENGTH OF SGTSL083-RPST TO QT-CCT-EPRO
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSL083-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC

      *--> Lê container de resposta do SGTSI089
      *        WHEN SGTSI089
      *          MOVE LENGTH OF SGTSI089-RPST TO QT-CCT-EPRO
      *          EXEC CICS GET
      *                    CONTAINER ( NM-CTNR       )
      *                    INTO      ( SGTSI089-RPST )
      *                    FLENGTH   ( QT-CCT-LIDO   )
      *          END-EXEC
      *
      *--> Lê container de resposta do SGTSI095
               WHEN SGTSI095
                 MOVE LENGTH OF SGTSI095-RPST TO QT-CCT-EPRO
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSI095-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC

      *--> Lê container de resposta do SGTS0163
               WHEN SGTS0163
                 MOVE LENGTH OF SGTS0163-RPST TO QT-CCT-EPRO
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTS0163-RPST )
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

      *--> Lê container de resposta do SGTSI078
               WHEN SGTSI078
                 MOVE LENGTH OF SGTSI078-RPST TO QT-CCT-EPRO
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSI078-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC
      *--> Módulo não previsto para leitura do container
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
           MOVE 0021                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0022                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0023                   TO MSG1-CD-SEQL-ERRO

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
      *--> Grava container de requisição do programa SGTSI069
               WHEN SGTSI069
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTSI069-RQSC           )
                            FLENGTH   ( LENGTH OF SGTSI069-RQSC )
                  END-EXEC

      *--> Grava container de requisição do programa SGTSA083
               WHEN SGTSA083
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTSA083-RQSC           )
                            FLENGTH   ( LENGTH OF SGTSA083-RQSC )
                  END-EXEC

      *--> Grava container de requisição do programa SGTSI083
               WHEN SGTSI083
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTSI083-RQSC           )
                            FLENGTH   ( LENGTH OF SGTSI083-RQSC )
                  END-EXEC

      *--> Grava container de requisição do programa SGTSL083
               WHEN SGTSL083
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTSL083-RQSC           )
                            FLENGTH   ( LENGTH OF SGTSL083-RQSC )
                  END-EXEC

      *--> Grava container de requisição do programa SGTSI089
      *        WHEN SGTSI089
      *           EXEC CICS PUT
      *                     CONTAINER ( NM-CTNR                 )
      *                     FROM      ( SGTSI089-RQSC           )
      *                     FLENGTH   ( LENGTH OF SGTSI089-RQSC )
      *           END-EXEC
      *
      *--> Grava container de requisição do programa SGTSI095
               WHEN SGTSI095
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTSI095-RQSC           )
                            FLENGTH   ( LENGTH OF SGTSI095-RQSC )
                  END-EXEC

      *--> Grava container de requisição do programa SGTS0163
               WHEN SGTS0163
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTS0163-RQSC           )
                            FLENGTH   ( LENGTH OF SGTS0163-RQSC )
                  END-EXEC

      *--> Grava container de requisição do programa SGTSLB75
               WHEN SGTSLB75
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTSLB75-RQSC           )
                            FLENGTH   ( LENGTH OF SGTSLB75-RQSC )
                  END-EXEC
      *--> Grava container de requisição do programa SGTSI078
               WHEN SGTSI078
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTSI078-RQSC           )
                            FLENGTH   ( LENGTH OF SGTSI078-RQSC )
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
           MOVE 0024                   TO MSG1-CD-SEQL-ERRO
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
           MOVE 0025                   TO MSG1-CD-SEQL-ERRO

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

      *-->>SGTSI069<<
             WHEN SGTSI069
                  MOVE  'SGTSI069-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI069-CD-CTRA-TIT-VL  TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI069-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                  IF  SGTSI069-CD-OPR-ETRD-MCP   NUMERIC
                      MOVE  SGTSI069-CD-OPR-ETRD-MCP
                                                 TO MCR-S9-15
                      MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                  ELSE
                      MOVE  'Valor não numérico' TO MSGKW999-VL-VRV-MNT
                  END-IF
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI069-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI069-NR-VRS-OPR-ETRD TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI069-CD-TIP-EST-VRS'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI069-CD-TIP-EST-VRS  TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI069-TX-OBS-EST-VRS'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI069-TX-OBS-EST-VRS  TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTSA083<<
             WHEN SGTSI083
                  MOVE  'SGTSA083-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSA083-CD-CTRA-TIT-VL  TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSA083-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                  IF  SGTSA083-CD-OPR-ETRD-MCP   NUMERIC
                      MOVE  SGTSA083-CD-OPR-ETRD-MCP
                                                 TO MCR-S9-15
                      MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                  ELSE
                      MOVE  'Valor não numérico' TO MSGKW999-VL-VRV-MNT
                  END-IF
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSA083-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSA083-NR-VRS-OPR-ETRD TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSA083-NR-SEQL-EXCO-PRFL'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSA083-NR-SEQL-EXCO-PRFL
                                                 TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSA083-CD-USU-ULT-ATL'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSA083-CD-USU-ULT-ATL  TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSA083-TS-ULT-ATL'    TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSA083-TS-ULT-ATL      TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSA083-IN-EXCO-ATV'   TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSA083-IN-EXCO-ATV     TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSA083-TX-OBS-EXCO-PRFL'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSA083-TX-OBS-EXCO-PRFL
                                                 TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTSI083<<
             WHEN SGTSI083
                  MOVE  'SGTSI083-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI083-CD-CTRA-TIT-VL  TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI083-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                  IF  SGTSI083-CD-OPR-ETRD-MCP NUMERIC
                      MOVE  SGTSI083-CD-OPR-ETRD-MCP
                                                 TO MCR-S9-15
                      MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                  ELSE
                      MOVE  'Valor não numérico' TO MSGKW999-VL-VRV-MNT
                  END-IF
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI083-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI083-NR-VRS-OPR-ETRD TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI083-NR-SEQL-EXCO-PRFL'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI083-NR-SEQL-EXCO-PRFL
                                                 TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI083-CD-USU-ULT-ATL'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI083-CD-USU-ULT-ATL  TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI083-TS-ULT-ATL'    TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI083-TS-ULT-ATL      TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI083-IN-EXCO-ATV'   TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI083-IN-EXCO-ATV     TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI083-TX-OBS-EXCO-PRFL'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI083-TX-OBS-EXCO-PRFL
                                                 TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTSL083<<
             WHEN SGTSL083
                  MOVE  'SGTSL083-E-CD-TIP-SRVC' TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSL083-E-CD-TIP-SRVC   TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSL083-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSL083-CD-CTRA-TIT-VL  TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSL083-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                  IF  SGTSL083-CD-OPR-ETRD-MCP   NUMERIC
                      MOVE  SGTSL083-CD-OPR-ETRD-MCP
                                                 TO MCR-S9-15
                      MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                  ELSE
                      MOVE  'Valor não numérico' TO MSGKW999-VL-VRV-MNT
                  END-IF
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSL083-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSL083-NR-VRS-OPR-ETRD TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSL083-E-NR-SEQL-EXCO-PRFL'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSL083-E-NR-SEQL-EXCO-PRFL
                                                 TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSL083-CD-RGR-VLDC'   TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSL083-CD-RGR-VLDC     TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSL083-NR-VRS-RGR-VLDC'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSL083-NR-VRS-RGR-VLDC TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTSI089<<
      *      WHEN SGTSI089
      *           MOVE  'SGTSI089-CD-RGR-VLDC'   TO MSGKW999-NM-VRV-MNT
      *           MOVE  SGTSI089-CD-RGR-VLDC     TO MCR-S9-15
      *           MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
      *           PERFORM  999003-ADICIONA-VRV-MNT
      *
      *           MOVE  'SGTSI089-NR-VRS-RGR-VLDC'
      *                                          TO MSGKW999-NM-VRV-MNT
      *           MOVE  SGTSI089-NR-VRS-RGR-VLDC TO MCR-S9-15
      *           MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
      *           PERFORM  999003-ADICIONA-VRV-MNT
      *
      *           MOVE  'SGTSI089-NR-SEQL-ITC'   TO MSGKW999-NM-VRV-MNT
      *           MOVE   SGTSI089-NR-SEQL-ITC    TO MCR-S9-15
      *           MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
      *           PERFORM  999003-ADICIONA-VRV-MNT
      *
      *           MOVE  'SGTSI089-CD-TIP-CMDO-VLDC'
      *                                          TO MSGKW999-NM-VRV-MNT
      *           MOVE   SGTSI089-CD-TIP-CMDO-VLDC
      *                                          TO MCR-S9-15
      *           MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
      *           PERFORM  999003-ADICIONA-VRV-MNT
      *
      *           MOVE  'SGTSI089-NR-GR-CND-ITC' TO MSGKW999-NM-VRV-MNT
      *           MOVE   SGTSI089-NR-GR-CND-ITC  TO MCR-S9-15
      *           MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
      *           PERFORM  999003-ADICIONA-VRV-MNT
      *
      *           MOVE  'SGTSI089-CD-ATB-VLDC'   TO MSGKW999-NM-VRV-MNT
      *           MOVE   SGTSI089-CD-ATB-VLDC    TO MCR-S9-15
      *           MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
      *           PERFORM  999003-ADICIONA-VRV-MNT
      *
      *           MOVE  'SGTSI089-CD-TIP-CFT-LGC'
      *                                          TO MSGKW999-NM-VRV-MNT
      *           MOVE   SGTSI089-CD-TIP-CFT-LGC TO MCR-S9-15
      *           MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
      *           PERFORM  999003-ADICIONA-VRV-MNT
      *
      *           MOVE  'SGTSI089-CD-TIP-CFT-ITEM'
      *                                          TO MSGKW999-NM-VRV-MNT
      *           MOVE   SGTSI089-CD-TIP-CFT-ITEM
      *                                          TO MCR-S9-15
      *           MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
      *           PERFORM  999003-ADICIONA-VRV-MNT
      *
      *           MOVE  'SGTSI089-CD-RGR-VLDC-EXAR'
      *                                          TO MSGKW999-NM-VRV-MNT
      *           MOVE   SGTSI089-CD-RGR-VLDC-EXAR
      *                                          TO MCR-S9-15
      *           MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
      *           PERFORM  999003-ADICIONA-VRV-MNT
      *
      *           MOVE  'SGTSI089-NR-VRS-RGR-EXAR'
      *                                          TO MSGKW999-NM-VRV-MNT
      *           MOVE   SGTSI089-NR-VRS-RGR-EXAR
      *                                          TO MCR-S9-15
      *           MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
      *           PERFORM  999003-ADICIONA-VRV-MNT
      *
      *           MOVE  'SGTSI089-VL-ITC-RGR-VLDC'
      *                                          TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
      *           IF  SGTSI089-VL-ITC-RGR-VLDC   NUMERIC
      *               MOVE  SGTSI089-VL-ITC-RGR-VLDC
      *                                          TO MCR-S9-15-V9-04
      *               MOVE  MCR-S9-15-V9-04      TO MSGKW999-VL-VRV-MNT
      *           ELSE
      *               MOVE  'Valor não numérico' TO MSGKW999-VL-VRV-MNT
      *           END-IF
      *           PERFORM  999003-ADICIONA-VRV-MNT
      *
      *           MOVE  'SGTSI089-NR-ITC-RGR-VLDC'
      *                                          TO MSGKW999-NM-VRV-MNT
      *           MOVE   SGTSI089-NR-ITC-RGR-VLDC
      *                                          TO MCR-S9-15
      *           MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
      *           PERFORM  999003-ADICIONA-VRV-MNT
      *
      *           MOVE  'SGTSI089-TX-ITC-RGR-VLDC'
      *                                          TO MSGKW999-NM-VRV-MNT
      *           MOVE  SGTSI089-TX-ITC-RGR-VLDC TO MSGKW999-VL-VRV-MNT
      *           PERFORM  999003-ADICIONA-VRV-MNT
      *
      *           MOVE  'SGTSI089-TX-DCR-ITC'    TO MSGKW999-NM-VRV-MNT
      *           MOVE  SGTSI089-TX-DCR-ITC      TO MSGKW999-VL-VRV-MNT
      *           PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTSI095<<
             WHEN SGTSI095
                  MOVE  'SGTSI095-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI095-CD-CTRA-TIT-VL  TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI095-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                  IF  SGTSI095-CD-OPR-ETRD-MCP   NUMERIC
                      MOVE  SGTSI095-CD-OPR-ETRD-MCP
                                                 TO MCR-S9-15
                      MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                  ELSE
                      MOVE  'Valor não numérico' TO MSGKW999-VL-VRV-MNT
                  END-IF
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI095-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI095-NR-VRS-OPR-ETRD TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI095-NR-SEQL-EXCO-PRFL'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI095-NR-SEQL-EXCO-PRFL
                                                 TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI095-NR-SEQL-ITEM-EXCO'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI095-NR-SEQL-ITEM-EXCO
                                                 TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI095-CD-RGR-VLDC'   TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI095-CD-RGR-VLDC     TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI095-NR-VRS-RGR-VLDC'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI095-NR-VRS-RGR-VLDC TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI095-NR-SEQL-ITC'   TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI095-NR-SEQL-ITC     TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI095-IN-ITEM-CRIO-EXCO'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI095-IN-ITEM-CRIO-EXCO
                                                 TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI095-VL-RLZD-ITEM-EXCO'
                                                 TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                  IF  SGTSI095-VL-RLZD-ITEM-EXCO NUMERIC
                      MOVE  SGTSI095-VL-RLZD-ITEM-EXCO
                                                 TO MCR-S9-15-V9-04
                      MOVE  MCR-S9-15-V9-04      TO MSGKW999-VL-VRV-MNT
                  ELSE
                      MOVE  'Valor não numérico' TO MSGKW999-VL-VRV-MNT
                  END-IF
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI095-TX-RLZD-ITEM-EXCO'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI095-TX-RLZD-ITEM-EXCO
                                                 TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI095-NR-RLZD-ITEM-EXCO'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI095-NR-RLZD-ITEM-EXCO
                                                 TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTSI095-TX-OBS-ITEM-EXCO'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSI095-TX-OBS-ITEM-EXCO
                                                 TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTST009<<
             WHEN SGTST009
                  MOVE 'SGTST009-ACAO'           TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTST009-ACAO            TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTST009-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTST009-CD-CTRA-TIT-VL  TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTST009-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                  IF  SGTST009-CD-OPR-ETRD-MCP NUMERIC
                      MOVE  SGTST009-CD-OPR-ETRD-MCP
                                                 TO MCR-S9-15
                      MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                  ELSE
                      MOVE  'Valor não numérico' TO MSGKW999-VL-VRV-MNT
                  END-IF
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTST009-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTST009-NR-VRS-OPR-ETRD TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTST009-TS-EST-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTST009-TS-EST-OPR-ETRD TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTST009-CD-USU-RSP-ATL'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTST009-CD-USU-RSP-ATL  TO MSGKW999-VL-VRV-MNT
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
      *-->>SGTSI078<<
             WHEN SGTSI078
                    MOVE 'SGTSI078-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI078-CD-CTRA-TIT-VL
                                                 TO MCR-S9-15
025700              MOVE  MCR-S9-15              TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                    MOVE 'SGTSI078-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
                    IF   SGTSI078-CD-OPR-ETRD-MCP   IS NUMERIC
                        MOVE  SGTSI078-CD-OPR-ETRD-MCP
                                                 TO MCR-S9-15
025700                  MOVE  MCR-S9-15          TO MSGKW999-VL-VRV-MNT
                    ELSE
025700                  MOVE  'Valor não numérico'
025700                                           TO MSGKW999-VL-VRV-MNT
025700              END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI078-NR-VRS-OPR-ETRD'
                                                TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI078-NR-VRS-OPR-ETRD
                                                TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI078-NR-SEQL-ORD-NEGO'
                                                TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI078-NR-SEQL-ORD-NEGO
                                                TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI078-TS-EST-ORD-NEGO'
                                                TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI078-TS-EST-ORD-NEGO
                                                TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI078-CD-USU-ULT-ATL'
                                                TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI078-CD-USU-ULT-ATL
                                                TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI078-CD-TIP-EST-NEGO'
                                                TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI078-CD-TIP-EST-NEGO
                                                TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI078-TX-JST-EST-ORD'
                                                TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI078-TX-JST-EST-ORD
                                                TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

      *-->>SGTS0163<<
             WHEN SGTS0163
                  MOVE  'SGTS0163-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTS0163-CD-CTRA-TIT-VL  TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTS0163-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                  IF  SGTS0163-CD-OPR-ETRD-MCP NUMERIC
                      MOVE  SGTS0163-CD-OPR-ETRD-MCP
                                                 TO MCR-S9-15
                      MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                  ELSE
                      MOVE  'Valor não numérico' TO MSGKW999-VL-VRV-MNT
                  END-IF
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTS0163-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTS0163-NR-VRS-OPR-ETRD TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE  'SGTS0163-QT-OCR-TAB-E'  TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTS0163-QT-OCR-TAB-E    TO MCR-S9-15
                  MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  PERFORM VARYING IC-TAB FROM 1 BY 1
                      UNTIL IC-TAB > SGTS0163-QT-OCR-TAB-E

                      MOVE  'SGTS0163-CD-RGR-VLDC-E'
                                                 TO MSGKW999-NM-VRV-MNT
                      MOVE  SGTS0163-CD-RGR-VLDC-E (IC-TAB)
                                                 TO MCR-S9-15
                      MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                      PERFORM  999003-ADICIONA-VRV-MNT

                      MOVE  'SGTS0163-NR-VRS-RGR-VLDC-E'
                                                 TO MSGKW999-NM-VRV-MNT
                      MOVE  SGTS0163-NR-VRS-RGR-VLDC-E (IC-TAB)
                                                 TO MCR-S9-15
                      MOVE  MCR-S9-15            TO MSGKW999-VL-VRV-MNT
                      PERFORM  999003-ADICIONA-VRV-MNT
                  END-PERFORM

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
           MOVE 'SGTS0236-CD-CTRA-TIT-VL'        TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0236-CD-CTRA-TIT-VL         TO MCR-S9-15
           MOVE  MCR-S9-15                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTS0236-CD-OPR-ETRD-MCP'      TO MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF SGTS0236-CD-OPR-ETRD-MCP  NUMERIC
              MOVE SGTS0236-CD-OPR-ETRD-MCP      TO MCR-S9-15
              MOVE  MCR-S9-15                    TO MSGKW999-VL-VRV-MNT
           ELSE
              MOVE  'Valor não numérico'         TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0236-NR-VRS-OPR-ETRD'       TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0236-NR-VRS-OPR-ETRD        TO MCR-S9-15
           MOVE  MCR-S9-15                       TO MSGKW999-VL-VRV-MNT
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
-INC HLPKP997

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
      *---------------------- FIM SGTS0236 ----------------------------