       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0203
      * ANALISTA..: F2804421 - Eliana Cristina Ramos da Silva
      * AUTOR.....: F2804421 - Eliana Cristina Ramos da Silva
      * DATA......: 12/11/2018
      * OBJETIVO..: Negócio  - Salvar dados da finalização do cadastra-
      *                        mento da operação.
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS003  08/11/2019 F9188847 DE 2016/145195 - Inclusao Sections:
      *         211000-VERIFICAR-MOD-ORD-ATI e Inclusao EST_ORD_NEGO_OPR
      * VRS002  07/03/2019 F2804421 DE 2016/145195 -Acerto rotina de mo-
      *         nitoração de variáveis.
      *         20/02/2019 F2804421 DE 2016/145195 -Acerto mensageria
      *         para os códigos 9003 e 9004.
      * VRS001 15/01/2019 F2804421 - Retirar descrição na mudança de
      *                   estado da operação - SGTST009-TX-JST-EST.
      *        12/11/2018 F2804421  DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0203.
      *
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
      *
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------

       77  CTE-INICIO-SS               PIC  X(40)  VALUE
                                            '*** S.S. COMEÇA AQUI ***'.
       77  CTE-PROG                    PIC  X(18)  VALUE
                                            '*** SGTS0203 ***'.
       77  CTE-VERS                    PIC  X(06)  VALUE 'VRS003'.

      * Módulos acionados
       77  SGTST009                    PIC  X(08) VALUE 'SGTST009'.
       77  SGTSI069                    PIC  X(08) VALUE 'SGTSI069'.
       77  SGTSI117                    PIC  X(08) VALUE 'SGTSI117'.
       77  SGTSE117                    PIC  X(08) VALUE 'SGTSE117'.
       77  SGTSL117                    PIC  X(08) VALUE 'SGTSL117'.
       77  SGTSLB75                    PIC  X(08) VALUE 'SGTSLB75'.
       77  SGTSI078                    PIC  X(08) VALUE 'SGTSI078'.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      * ?reas de trabalho

      *--> Tabela auxiliar para validar códigos de parâmetros informados
      *TB-CD-PRM    Descrição:
      *  1         'Aviso de Liquidação Financeira'
      *  2         'Contrato de Garantia'
      *  3         'Declaração de Conformidade'
      *  4         'Instrumento de Formalização Registrado'
      *  5         'Limite de Crédito'
      *  6         'Súmula da Agência'
      *  7         'Precificação Válida'
      *  8         'Tarifa'
      *  9         'Operação com Garantia'
      * 10         'Nota Técnica Aprovação'
      * 11         'Agente Fiduciário'
      * 12         'Limite Financeiro da Carteira'
      * 13         'Exceção'
      ***
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

       01  IC-TAB                      PIC S9(04)     COMP-5 VALUE 0.
       01  IC-DISP                     PIC S9(04)     COMP-5 VALUE 0.

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      ---------------9.

      **** mensagens de erro emitidas neste módulo - mensageria *******
      *
      * 001 'Código da carteira de título valores mobiliários inválido!
      * 002 'Código da carteira de título e valores mobiliários
      *              ' não informado.'
      * 003 'Código operação estruturada mercado de capitais não
      *     numérico.'
      * 004 'Código da operação estruturada do mercado de capit
      *              'ais inválido.'
      * 005 'Código operação estruturada mercado de capitais nã
      *              'o informado.'
      * 006 'Número versão operação estruturada inválido.'
      * 007 'Número da versão da operação estruturada não informado.'
      * 860 'Estado da operação não permite finalização. '
      *3004 'Erro ao acionar o módulo '
      *
      *
      *-----------------------------------------------------------------
      * Books dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      * Book para captura de abend cics do bloco código HLPKW999
-INC HLPKW999
      * Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Manutenção da tabela 'DB2SGT.OPR_ETRD_MCP'
       01  SGTST009-DADOS.
-INC SGTKT009

      *--> Inclusão na tabela 'DB2SGT.EST_VRS_OPR_ETRD'
       01  SGTSI069-DADOS.
-INC SGTKI069

      * Book dos dados do usuário
-INC IIBKOPRD

      *--> Inclusão da tabela 'JST-VLDC-OPR-ETRD'.
-INC SGTKI117

      *--> Exclusão da tabela 'JST-VLDC-OPR-ETRD'.
-INC SGTKE117

      *--> Lista das tabelas 'JST-VLDC-OPR-ETRD', e 'TIP-JST-VLDC-OPR'
       01  SGTSL117-DADOS.
-INC SGTKL117

      *--> Consulta tabelas ORD_NEGO_OPR e MOD_ORD_NEGO
-INC SGTKLB75

      *--> Inclusão na Tabela EST_ORD_NEGO_OPR.
       01  SGTSI078-DADOS.
-INC SGTKI078

      * Book do Container de Erro
-INC IIBKERRO

      * Book do container de requisição e resposta deste módulo
       01 SGTS0203-DADOS.
-INC SGTK0203

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

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
           MOVE 001                    TO MSG1-CD-SEQL-ERRO

      *--> WHENEVER DB2
-INC HLPKE997

      *    Ativa a captura automática de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM 970000-LER-CANAL

      *    Define o nome do programa p/ fins manipulação container
           MOVE CTE-PROG(5:8)          TO NM-PGM

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

           PERFORM 974000-LER-DADOS-OPRD

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 002                    TO MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada
           IF  SGTS0203-CD-CTRA-TIT-VL     LESS ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0203-CD-CTRA-TIT-VL     EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0203-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0203-CD-OPR-ETRD-MCP    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0203-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'    TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0203-NR-VRS-OPR-ETRD    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número versão operação estruturada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0203-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------

           PERFORM 210000-CONSULTA-ESTADOS

           PERFORM 211000-VERIFICAR-MOD-ORD-ATI

           PERFORM 220000-ATUALIZA-ESTADOS

           PERFORM 230000-TRATA-JUSTIFICATIVAS

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-CONSULTA-ESTADOS         SECTION.
      *-----------------------------------------------------------------
           MOVE 003                    TO MSG1-CD-SEQL-ERRO

      * Busca os últimos estados da operação e versão;

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
                 WHERE A.CD_CTRA_TIT_VL    = :SGTS0203-CD-CTRA-TIT-VL
                   AND A.CD_OPR_ETRD_MCP   = :SGTS0203-CD-OPR-ETRD-MCP
                   AND A.NR_VRS_OPR_ETRD   = :SGTS0203-NR-VRS-OPR-ETRD
                 AND A.TS_EST_OPR_ETRD = (SELECT MAX (C.TS_EST_OPR_ETRD)
                      FROM DB2SGT.EST_OPR_ETRD C
                      WHERE C.CD_CTRA_TIT_VL  =  A.CD_CTRA_TIT_VL
                        AND C.CD_OPR_ETRD_MCP =  A.CD_OPR_ETRD_MCP
                        AND C.NR_VRS_OPR_ETRD =  A.NR_VRS_OPR_ETRD)
                      ORDER BY B.TS_EST_VRS DESC
                      FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  CD-TIP-EST-OPR       NOT EQUAL 1 OR
               CD-MTV-ALT           NOT EQUAL 0 OR
               CD-TIP-EST-VRS       NOT EQUAL 1
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 860                TO MSG1-CD-ERRO-TS
               MOVE 'Estado da operação não permite finalização.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       211000-VERIFICAR-MOD-ORD-ATI         SECTION.
      *-----------------------------------------------------------------
           MOVE 004                    TO MSG1-CD-SEQL-ERRO

      * Verfifica se o Modelo da Ordem de Compra esta ativo;
           INITIALIZE                  SGTSLB75-RQSC

           MOVE SGTSLB75               TO NM-PGM

           MOVE SGTS0203-CD-CTRA-TIT-VL
                                       TO SGTSLB75-E-CD-CTRA-TIT-VL
           MOVE SGTS0203-CD-OPR-ETRD-MCP
                                       TO SGTSLB75-E-CD-OPR-ETRD-MCP

           MOVE SGTS0203-NR-VRS-OPR-ETRD
                                       TO SGTSLB75-E-NR-VRS-OPR-ETRD

           MOVE ZEROS
                                       TO SGTSLB75-E-NR-SEQL-ORD-PAG

           PERFORM 900000-EXECUTAR-MODULO

           IF  SGTSLB75-QT-OCR-TAB      EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0932               TO MSG1-CD-ERRO-TS
               MOVE 'Não existe modelo de compra vinculado a esta operaç
      -              'ção.'            TO  MSG1-TX-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSLB75-IN-MOD-ORD-ATI(1)       NOT EQUAL 'S'
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0917                TO MSG1-CD-ERRO-TS
               MOVE 'Modelo da Ordem de Compra deve estar ativo.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       211000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       220000-ATUALIZA-ESTADOS         SECTION.
      *-----------------------------------------------------------------
           MOVE 005                    TO MSG1-CD-SEQL-ERRO

      ***    Incluir estado da operação:
           SET SGTST009-INCLUIR        TO TRUE

           MOVE SGTS0203-CD-CTRA-TIT-VL
                                       TO SGTST009-CD-CTRA-TIT-VL
           MOVE SGTS0203-CD-OPR-ETRD-MCP
                                       TO SGTST009-CD-OPR-ETRD-MCP

           MOVE SGTS0203-NR-VRS-OPR-ETRD
                                       TO SGTST009-NR-VRS-OPR-ETRD
           MOVE 'CURRENT'              TO SGTST009-TS-EST-OPR-ETRD
           MOVE KOPRD-CD-OPRD          TO SGTST009-CD-USU-RSP-ATL
                                          SGTST009-CD-USU-RSP-EST
           MOVE 1                      TO SGTST009-CD-TIP-EST-OPR
           MOVE ZEROS                  TO SGTST009-CD-MTV-ALT

           PERFORM 985000-ATUALIZA-EST-OPERACAO

      ***    Incluir estado da versão da operação:
           MOVE SGTSI069               TO NM-PGM

           MOVE SGTS0203-CD-CTRA-TIT-VL
                                       TO SGTSI069-CD-CTRA-TIT-VL
           MOVE SGTS0203-CD-OPR-ETRD-MCP
                                       TO SGTSI069-CD-OPR-ETRD-MCP
           MOVE SGTS0203-NR-VRS-OPR-ETRD
                                       TO SGTSI069-NR-VRS-OPR-ETRD
           MOVE 1                      TO SGTSI069-CD-TIP-EST-VRS

           PERFORM 900000-EXECUTAR-MODULO

      ***    Incluir estado da ordem de negociação da compra:
           MOVE SGTSI078               TO NM-PGM

           MOVE SGTS0203-CD-CTRA-TIT-VL
                                       TO SGTSI078-CD-CTRA-TIT-VL
           MOVE SGTS0203-CD-OPR-ETRD-MCP
                                       TO SGTSI078-CD-OPR-ETRD-MCP
           MOVE SGTS0203-NR-VRS-OPR-ETRD
                                       TO SGTSI078-NR-VRS-OPR-ETRD
           MOVE SGTSLB75-NR-SEQL-ORD-NEGO(1)
                                       TO SGTSI078-NR-SEQL-ORD-NEGO
      ***    1 - Em edição
           MOVE 1                      TO SGTSI078-CD-TIP-EST-NEGO

           MOVE KOPRD-CD-OPRD          TO SGTSI078-CD-USU-ULT-ATL

           MOVE 'CURRENT'              TO SGTSI078-TS-EST-ORD-NEGO

           PERFORM 900000-EXECUTAR-MODULO

           .
       220000-SAI.

      *-----------------------------------------------------------------
       230000-TRATA-JUSTIFICATIVAS     SECTION.
      *-----------------------------------------------------------------
           MOVE 006                    TO MSG1-CD-SEQL-ERRO

           SET SGTSL117-LS-JST-VLDC-OPR-ETRD     TO TRUE
           MOVE SGTS0203-CD-CTRA-TIT-VL
                                       TO SGTSL117-CD-CTRA-TIT-VL
           MOVE SGTS0203-CD-OPR-ETRD-MCP
                                       TO SGTSL117-CD-OPR-ETRD-MCP
           MOVE SGTS0203-NR-VRS-OPR-ETRD
                                       TO SGTSL117-NR-VRS-OPR-ETRD

           MOVE SGTSL117               TO NM-PGM

           PERFORM 900000-EXECUTAR-MODULO

      ***  Se existir algum registro de justificativa na tabela.
           IF  SGTSL117-QT-OCR-TAB > 0
               MOVE SGTSE117           TO NM-PGM
               MOVE SGTSL117-CD-CTRA-TIT-VL
                                       TO SGTSE117-CD-CTRA-TIT-VL
               MOVE SGTSL117-CD-OPR-ETRD-MCP
                                       TO SGTSE117-CD-OPR-ETRD-MCP
               MOVE SGTSL117-NR-VRS-OPR-ETRD
                                       TO SGTSE117-NR-VRS-OPR-ETRD

      ***      Efetua a exclusão de todos os registros encontrados.
               PERFORM VARYING IC-TAB FROM 1 BY 1
                     UNTIL IC-TAB > SGTSL117-QT-OCR-TAB
                     MOVE IC-TAB       TO IC-DISP

                     MOVE SGTSL117-CD-TIP-VLDC-OPR (IC-TAB)
                                       TO SGTSE117-CD-TIP-VLDC-OPR

                     PERFORM 900000-EXECUTAR-MODULO
               END-PERFORM
           END-IF

      ***  Se existir algum registro de justificativa informado  na tela
      ***     efetuar a sua inclusão na tabela.

           IF  SGTS0203-QT-OCR-PRM-JST  > 0
               MOVE SGTS0203-CD-CTRA-TIT-VL
                                       TO  SGTSI117-CD-CTRA-TIT-VL
               MOVE SGTS0203-CD-OPR-ETRD-MCP
                                       TO  SGTSI117-CD-OPR-ETRD-MCP
               MOVE SGTS0203-NR-VRS-OPR-ETRD
                                       TO  SGTSI117-NR-VRS-OPR-ETRD
               MOVE KOPRD-CD-OPRD      TO  SGTSI117-CD-USU-ULT-ATL
               MOVE 'CURRENT'          TO  SGTSI117-TS-ULT-ATL

               MOVE SGTSI117           TO   NM-PGM

      ***      Efetua a inclusão das justificativas informadas na tela.
               PERFORM VARYING IC-TAB FROM 1 BY 1
                       UNTIL IC-TAB > SGTS0203-QT-OCR-PRM-JST

                   IF SGTS0203-CD-TIP-VLDC-OPR (IC-TAB) > 0     AND
                      SGTS0203-TX-JST-VLDC-OPR (IC-TAB) NOT = SPACES

                      MOVE SGTS0203-CD-TIP-VLDC-OPR (IC-TAB)
                                           TO SGTSI117-CD-TIP-VLDC-OPR
                      MOVE SGTS0203-TX-JST-VLDC-OPR (IC-TAB)
                                           TO SGTSI117-TX-JST-VLDC-OPR

                      PERFORM 900000-EXECUTAR-MODULO
                   END-IF
               END-PERFORM
           END-IF
           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *-----------------------------------------------------------------
      * Executa um módulo implementado com CONTAINER                   *
      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *-----------------------------------------------------------------

      *    Grava o container de requisição do módulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM
               WHEN SGTSI069
                    EXEC CICS LINK
                              PROGRAM ( SGTSI069   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTSI117
                    EXEC CICS LINK
                              PROGRAM ( SGTSI117   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTSE117
                    EXEC CICS LINK
                              PROGRAM ( SGTSE117   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTSL117
                    EXEC CICS LINK
                              PROGRAM ( SGTSL117   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTSI078
                    EXEC CICS LINK
                              PROGRAM ( SGTSI078   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTSLB75
                    EXEC CICS LINK
                              PROGRAM ( SGTSLB75   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN OTHER
                    MOVE 9006          TO MSG1-CD-ERRO-TS
                    MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
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
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *    Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *-----------------------------------------------------------------
      * Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------
      * Variáveis exclusivas de cada programa acionado
           EVALUATE NM-PGM
             WHEN SGTST009
               MOVE 'SGTST009-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTST009-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTST009-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
               MOVE SGTST009-CD-OPR-ETRD-MCP   TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTST009-NR-VRS-OPR-ETRD' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTST009-NR-VRS-OPR-ETRD TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTST009-ACAO' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTST009-ACAO  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

              WHEN SGTSI069
               MOVE 'SGTSI069-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSI069-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSI069-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
               MOVE SGTSI069-CD-OPR-ETRD-MCP   TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSI069-NR-VRS-OPR-ETRD' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSI069-NR-VRS-OPR-ETRD TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSI069-CD-TIP-EST-VRS'  TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSI069-CD-TIP-EST-VRS   TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

             WHEN SGTSI117
               MOVE 'SGTSI117-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSI117-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSI117-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
               MOVE SGTSI117-CD-OPR-ETRD-MCP   TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSI117-NR-VRS-OPR-ETRD' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSI117-NR-VRS-OPR-ETRD  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSI117-CD-TIP-VLDC-OPR' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSI117-CD-TIP-VLDC-OPR  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

             WHEN SGTSE117
               MOVE 'SGTSE117-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSE117-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSE117-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
               MOVE SGTSE117-CD-OPR-ETRD-MCP   TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSE117-NR-VRS-OPR-ETRD' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSE117-NR-VRS-OPR-ETRD  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSE117-CD-TIP-VLDC-OPR' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSE117-CD-TIP-VLDC-OPR  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

             WHEN SGTSL117
               MOVE 'SGTSL117-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSL117-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSL117-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
               MOVE SGTSL117-CD-OPR-ETRD-MCP   TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSL117-NR-VRS-OPR-ETRD' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSL117-NR-VRS-OPR-ETRD  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               IF   IC-DISP        > 0
                    MOVE 'IC-DISP'             TO MSGKW999-NM-VRV-MNT
                    MOVE  IC-DISP              TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
              END-IF

             WHEN SGTSLB75
               MOVE 'SGTSLB75-E-CD-CTRA-TIT-VL' TO  MSGKW999-NM-VRV-MNT
               MOVE SGTSLB75-E-CD-CTRA-TIT-VL   TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'SGTSLB75-E-CD-OPR-ETRD-MCP' TO  MSGKW999-NM-VRV-MNT
               MOVE SGTSLB75-E-CD-OPR-ETRD-MCP   TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

              MOVE 'SGTSLB75-E-NR-VRS-OPR-ETRD' TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSLB75-E-NR-VRS-OPR-ETRD   TO  MSGKW999-VL-VRV-MNT
              PERFORM  999003-ADICIONA-VRV-MNT

              MOVE 'SGTSLB75-E-NR-SEQL-ORD-PAG' TO  MSGKW999-NM-VRV-MNT
              MOVE SGTSLB75-E-NR-SEQL-ORD-PAG   TO  MSGKW999-VL-VRV-MNT
              PERFORM  999003-ADICIONA-VRV-MNT

            WHEN SGTSI078
              MOVE 'SGTSI078-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
              MOVE SGTSI078-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSI078-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
              MOVE  SGTSI078-CD-OPR-ETRD-MCP    TO  MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSI078-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
              MOVE SGTSI078-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSI078-NR-SEQL-ORD-NEGO'  TO MSGKW999-NM-VRV-MNT
              MOVE SGTSI078-NR-SEQL-ORD-NEGO    TO MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSI078-TS-EST-ORD-NEGO'   TO MSGKW999-NM-VRV-MNT
              MOVE SGTSI078-TS-EST-ORD-NEGO     TO MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSI078-CD-USU-ULT-ATL'    TO MSGKW999-NM-VRV-MNT
              MOVE SGTSI078-CD-USU-ULT-ATL      TO MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSI078-CD-TIP-EST-NEGO'   TO MSGKW999-NM-VRV-MNT
              MOVE SGTSI078-CD-TIP-EST-NEGO     TO MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              MOVE 'SGTSI078-TX-JST-EST-ORD'    TO MSGKW999-NM-VRV-MNT
              MOVE SGTSI078-TX-JST-EST-ORD      TO MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT

              WHEN OTHER
                  CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *                                                                *
      * SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
      *                                                                *
      *-----------------------------------------------------------------
       970000-LER-CANAL                SECTION.
      *    Obtem o canal corrente
      *-----------------------------------------------------------------
           MOVE 007                    TO MSG1-CD-SEQL-ERRO

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

      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------
           MOVE 008                    TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisição
           MOVE CTE-PROG(5:8)          TO NM-PGM
           MOVE 'SGTS0203-RQSC'        TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      *--> Obtem o tamanho esperado da requisição
           MOVE  LENGTH OF SGTS0203-RQSC
                                       TO  QT-CCT-EPRO

      *--> Não valida tamanho esperado da requisição X tamanho recebido
      *--> pois possui tabela com depending on na entrada.

           MOVE 009                    TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER ( NM-CTNR       )
                     FLENGTH   ( QT-CCT-LIDO   )
                     INTO      ( SGTS0203-RQSC )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *-----------------------------------------------------------------
      *    A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------

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
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *-----------------------------------------------------------------
      *    Recupera o container com os dados de resposta (saída).
      *-----------------------------------------------------------------

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

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

               WHEN SGTSI069
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI069-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSI117
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSI117-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE117
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSE117-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL117
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSL117-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSLB75
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSLB75-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSI078
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSI078-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN OTHER
                    MOVE 9003          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro na leitura da resposta do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------

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
       974000-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE 010                    TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           MOVE 'PRM-INF-OPRD'         TO NM-CTNR

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           INITIALIZE PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       974000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
      *                                                                *
      *----------------------------------------------------------------*

       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *-----------------------------------------------------------------
      *    Postar container com dados de requisição (entrada)
      *-----------------------------------------------------------------

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_modulo -RQSC

           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RQSC'       TO NM-CTNR(9:5)


      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisição do programa a executar
           EVALUATE NM-PGM
               WHEN SGTSI069
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSI069-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSI069-RQSC )
                    END-EXEC
      *
               WHEN SGTSI117
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSI117-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSI117-RQSC )
                    END-EXEC
      *
               WHEN SGTSE117
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSE117-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSE117-RQSC )
                    END-EXEC
      *
               WHEN SGTSL117
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSL117-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSL117-RQSC )
                    END-EXEC

      *
               WHEN SGTSLB75
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSLB75-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSLB75-RQSC )
                    END-EXEC

      *
               WHEN SGTSI078
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSI078-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSI078-RQSC )
                    END-EXEC

               WHEN OTHER
                    MOVE 9004          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro na gravação da requisição do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       982000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *-----------------------------------------------------------------
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *-----------------------------------------------------------------


      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RPST'       TO NM-CTNR(9:5)


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

      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------

      *    Limpar variáveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE 011                    TO MSG1-CD-SEQL-ERRO
           MOVE 'PRM-ERRO'             TO NM-CTNR
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

      *-----------------------------------------------------------------
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *-----------------------------------------------------------------
           MOVE 012                    TO MSG1-CD-SEQL-ERRO

      *    Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           MOVE 'PRM-ERRO'             TO NM-CTNR
           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                     )
                     FROM      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC
           .
       984000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       985000-ATUALIZA-EST-OPERACAO    SECTION.
      *----------------------------------------------------------------*
           MOVE 013                    TO MSG1-CD-SEQL-ERRO

           EXEC CICS LINK
                        PROGRAM ( SGTST009       )
                        COMMAREA( SGTST009-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  SGTST009-CD-RTN-PGM     NOT EQUAL ZEROS AND +100
               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND
               MOVE SGTST009-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS
               MOVE SGTST009-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       985000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    As sections abaixo iniciadas com 999000 representam os
      *    tratamentos de erro utilizando o Sistema MSG
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF
           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Em caso de erro, desfaz as atualizações em banco de dados
           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC

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

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE 'SGTS0203-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0203-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0203-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTS0203-CD-OPR-ETRD-MCP    NUMERIC
               MOVE  SGTS0203-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0203-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0203-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *-->  Monitora tabela de entrada.
           PERFORM VARYING IC-TAB FROM 1 BY 1
                     UNTIL IC-TAB > SGTS0203-QT-OCR-PRM-JST
              IF   IC-TAB              > 0
                   MOVE 'IC-TAB'       TO MSGKW999-NM-VRV-MNT
                   MOVE  IC-TAB        TO MCR-S9-15
                   MOVE  MCR-S9-15     TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT

                   MOVE 'SGTS0203-TX-JST-VLDC-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                   MOVE  SGTS0203-TX-JST-VLDC-OPR (IC-TAB)
                                       TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT
              END-IF
           END-PERFORM

      *--> Grava a Ocorrência de Erro no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava a Ocorrência de Erro no Container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

      *--> Encerra a transacao
           PERFORM 999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      *----------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      *----------------------------------------
-INC HLPKP999
      *----------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
-INC SGTKP999
      *----------------------------------------
      * 999010-TRATA-ERRO-DB2          SECTION.
      *----------------------------------------
-INC HLPKP997
      *----------------------------------------
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
      *----------------------------------------
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
      *---------------------- Fim SGTS0203 ----------------------------*