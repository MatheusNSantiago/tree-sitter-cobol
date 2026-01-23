      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0252
      * ANALISTA..: F2804421 - Eliana Cristina Ramos da Silva
      * AUTOR.....: F2804421 - Eliana Cristina Ramos da Silva
      * DATA......: 07/02/2019
      * OBJETIVO..: Efetuar atualização do código do título da operação.
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS002 17/04/2019 - F2804421 - DE 2016/145195 - Tratamento do
      *             número máximo de NR-DVS-TIT para consulta e atuali-
      *             zação nas tabelas IN_OPR_ETRD e OPR_ETRD_MCP.
      * VRS001 07/02/2019 - F2804421 - DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0252.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------

       77  CTE-INICIO-SS                   PIC  X(040)  VALUE
                    '*** S.S. COMECA AQUI ***'.

       77  CTE-PROG                        PIC  X(018)  VALUE
                    '*** SGTS0252 ***'.
       77  CTE-VERS                        PIC  X(006)  VALUE 'VRS002'.

      *--> Módulos acionados
       77  SGTST002                        PIC  X(008) VALUE 'SGTST002'.
       77  SGTST005                        PIC  X(008) VALUE 'SGTST005'.
       77  SGTS0211                        PIC  X(008) VALUE 'SGTS0211'.
      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

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


      *--> Areas de trabalho
      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).
      *--> Área para receber o valor máximo existente na tabela.
       01  NR-DVS-TIT                  PIC S9(04) COMP-5 VALUE ZEROS.

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------
      * Book do container de requisição deste módulo
       01 SGTS0252-DADOS.
-INC SGTK0252

      * Consulta de vínculos do título informado.
       01 SGTS0211-DADOS.
-INC SGTK0211

      *--> CRUD da tabela TIT_OPR_ETRD_MCP
       01 SGTST002-DADOS.
-INC SGTKT002

      * Book SGT -  Lista da tabela DB2SGT.OPR_ETRD_MCP
       01  SGTST005-DADOS.
-INC SGTKT005

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      * Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999

      * Book para captura de erro via MSG1
-INC MSGKWMG1

      * Book do Container de Erro
-INC IIBKERRO

      * Book dos dados do usuário
-INC IIBKOPRD

      *--> Area de trabalho SQL/ DB2.
           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

       01  CTE-FIM-LS                  PIC  X(32)  VALUE
                                             '*** LS TERMINA AQUI ***'.
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

           MOVE 0001                   TO  MSG1-CD-SEQL-ERRO

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997
      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999

           PERFORM 970000-LER-CANAL

           MOVE 'SGTS0252'             TO  NM-PGM

           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 974000-LER-DADOS-OPRD

           PERFORM 971000-LER-REQUISICAO

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO  MSG1-CD-SEQL-ERRO

           IF  SGTS0252-CD-CTRA-TIT-VL LESS ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0001               TO  MSG1-CD-ERRO-TS
               MOVE 'Código carteira operação estruturada inválido.'
      -                                TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0252-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0002               TO  MSG1-CD-ERRO-TS
               MOVE 'Código Carteira Operação Estruturada não informado.
      -         ''                     TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0252-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0003               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada de mercado de capita
      -             'is não numérico.' TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0252-CD-OPR-ETRD-MCP    LESS ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 0004                   TO  MSG1-CD-ERRO-TS
               MOVE 'Codigo Operação Estruturada Mercado de Capitais inv
      -             'álido.'               TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0252-CD-OPR-ETRD-MCP    EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 0005                   TO  MSG1-CD-ERRO-TS
               MOVE 'Codigo Operação Estruturada Mercado de Capitais não
      -             'informado'            TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0252-NR-VRS-OPR-ETRD    LESS ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 0006                   TO  MSG1-CD-ERRO-TS
               MOVE 'Numero Versão Operação Estruturado inválido.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0252-NR-VRS-OPR-ETRD    EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 0007                   TO  MSG1-CD-ERRO-TS
               MOVE 'Numero Versão Operação Estruturado deve ser informa
      -        'do.'                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0252-CD-TIT-OPR-ETRD    EQUAL SPACES
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 0231                TO  MSG1-CD-ERRO-TS
               MOVE 'Código do título da operação estruturada não inform
      -        'ado.'                      TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO  MSG1-CD-SEQL-ERRO

           PERFORM 210000-CONSULTA-ESTADO

           PERFORM 220000-CONSULTA-OPR

           PERFORM 230000-TRATA-TITULO

           PERFORM 240000-ATUALIZA-OPR

           .
       200000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       210000-CONSULTA-ESTADO          SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO  MSG1-CD-SEQL-ERRO

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
                WHERE A.CD_CTRA_TIT_VL    = :SGTS0252-CD-CTRA-TIT-VL
                  AND A.CD_OPR_ETRD_MCP   = :SGTS0252-CD-OPR-ETRD-MCP
                  AND A.NR_VRS_OPR_ETRD   = :SGTS0252-NR-VRS-OPR-ETRD
                  AND B.TS_EST_VRS = (SELECT MAX (C.TS_EST_VRS)
                FROM DB2SGT.EST_VRS_OPR_ETRD C
                     WHERE C.CD_CTRA_TIT_VL  =  A.CD_CTRA_TIT_VL
                       AND C.CD_OPR_ETRD_MCP =  A.CD_OPR_ETRD_MCP
                       AND C.NR_VRS_OPR_ETRD =  A.NR_VRS_OPR_ETRD)
                ORDER BY A.TS_EST_OPR_ETRD DESC
                FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  SQLCODE                     EQUAL +100
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0133               TO MSG1-CD-ERRO-TS
               MOVE 'Estado da versão da operação estruturada inexistent
      -             'e.'               TO MSG1-TX-ERRO-TS

               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Consistência da permissão da atualização do título
      *--> Permitido somente para Operação nos estados:
      *    1.1 (Em edição com motivo = 999 (conferir cadastro)
      *    2.2 (Autorizar compra)
      *    3.3 (Disparar Ordem de Compra)
      *    4.3 (A Encarteirar)

           IF (CD-TIP-EST-OPR     = 1    AND
               CD-TIP-EST-VRS     = 1    AND
               CD-MTV-ALT         = 999)
                                               OR
              (CD-TIP-EST-OPR     = 2    AND
               CD-TIP-EST-VRS     = 2)
                                               OR
              (CD-TIP-EST-OPR     = 2    AND
               CD-TIP-EST-VRS     = 3)
                                               OR
              (CD-TIP-EST-OPR     = 3    AND
               CD-TIP-EST-VRS     = 3)
                                               OR
              (CD-TIP-EST-OPR     = 4   AND
               CD-TIP-EST-VRS     = 3)

              CONTINUE
           ELSE
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0098               TO MSG1-CD-ERRO-TS
               MOVE 'Alteração não permitida para o estado da operação.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-CONSULTA-OPR            SECTION.
      *  Efetua busca de dados da operação;
      *-----------------------------------------------------------------
           MOVE 0005                   TO  MSG1-CD-SEQL-ERRO

           MOVE SGTST005                   TO  NM-PGM
           SET  SGTST005-CONSULTAR         TO  TRUE

           MOVE SGTS0252-CD-CTRA-TIT-VL    TO  SGTST005-CD-CTRA-TIT-VL
           MOVE SGTS0252-CD-OPR-ETRD-MCP   TO  SGTST005-CD-OPR-ETRD-MCP
           MOVE SGTS0252-NR-VRS-OPR-ETRD   TO  SGTST005-NR-VRS-OPR-ETRD

           PERFORM 930000-CHAMAR-SGTST005

           IF  SGTST005-CD-RTN-PGM     EQUAL 100
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0348               TO MSG1-CD-ERRO-TS
               MOVE 'Operação estruturada de mercado de capitais inexist
      -             'ente.'            TO MSG1-TX-ERRO-TS

               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-TRATA-TITULO             SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO  MSG1-CD-SEQL-ERRO

      *--> Verifica se título será incluído na tabela de títulos.
      *--> TIT_OPT_ETRD_MCP

           EXEC SQL
                SELECT VALUE (MAX(NR_DVS_TIT) , 0 )
                INTO :NR-DVS-TIT
                FROM DB2SGT.TIT_OPR_ETRD_MCP
                WHERE CD_TIT_OPR_ETRD   = :SGTS0252-CD-TIT-OPR-ETRD
           END-EXEC

      *--> Título não existe, incluir.
           IF  NR-DVS-TIT  EQUAL 0
               SET SGTST002-INCLUIR    TO TRUE
               MOVE SGTS0252-CD-TIT-OPR-ETRD
                                       TO SGTST002-CD-TIT-OPR-ETRD
               MOVE 1                  TO SGTST002-NR-DVS-TIT
                                          NR-DVS-TIT
               MOVE SGTS0252-CD-TIT-OPR-ETRD
                                       TO SGTST002-NM-TIT-OPR-ETRD
               MOVE SPACES             TO SGTST002-CD-ISIN-TIT-OPR
               MOVE SPACES             TO SGTST002-CD-EPC-TIT-OPR
               MOVE 'CURRENT'          TO SGTST002-TS-ULT-ATL
               MOVE  KOPRD-CD-OPRD     TO SGTST002-CD-USU-RSP-ATL

               PERFORM 920000-CHAMAR-SGTST002
           ELSE
      *--> Se título já existe, verificar se está vinculado a outra
      *--> operação ativa.
               MOVE SGTS0211         TO NM-PGM
               MOVE SGTS0252-CD-TIT-OPR-ETRD
                                     TO SGTS0211-CD-TIT-OPR-ETRD
               MOVE NR-DVS-TIT       TO SGTS0211-NR-DVS-TIT

               PERFORM 900000-EXECUTAR-MODULO

               IF  SGTS0211-CD-CTRA-TIT-VL  NOT EQUAL ZEROS  OR
                   SGTS0211-CD-OPR-ETRD-MCP NOT EQUAL ZEROS  OR
                   SGTS0211-NR-VRS-OPR-ETRD NOT EQUAL ZEROS

                   SET ERRO-PROGRAMADO      TO  TRUE
                   MOVE 0423                TO  MSG1-CD-ERRO-TS
                   MOVE 'Título já existente, atualização não efetua
      -                 'da.'               TO MSG1-TX-ERRO-TS

                   PERFORM 969000-MONITORAR-VARIAVEIS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           END-IF

           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       240000-ATUALIZA-OPR            SECTION.
      *  Efetua atualização do campo título da operação;
      *-----------------------------------------------------------------
           MOVE 0008                   TO  MSG1-CD-SEQL-ERRO

           MOVE SGTST005               TO  NM-PGM
           SET  SGTST005-ALTERAR       TO  TRUE

           MOVE 'CURRENT'              TO  SGTST005-TS-ULT-ATL
           MOVE KOPRD-CD-OPRD          TO  SGTST005-CD-USU-ULT-ATL

           MOVE SGTS0252-CD-TIT-OPR-ETRD
                                       TO  SGTST005-CD-TIT-OPR-ETRD
           MOVE NR-DVS-TIT             TO  SGTST005-NR-DVS-TIT

           PERFORM 930000-CHAMAR-SGTST005

           IF  SGTST005-CD-RTN-PGM     EQUAL 100
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0348               TO MSG1-CD-ERRO-TS
               MOVE 'Operação estruturada de mercado de capitais inexist
      -             'ente.'            TO MSG1-TX-ERRO-TS

               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       240000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *   Section da Faixa de 900000 a 969900 para Demais Rotinas
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

               WHEN SGTS0211
                    EXEC CICS LINK
                              PROGRAM ( SGTS0211   )
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
       920000-CHAMAR-SGTST002          SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE SGTST002               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST002)
                COMMAREA (SGTST002-DADOS)
           END-EXEC

           IF  SGTST002-CD-RTN-PGM     NOT EQUAL ZEROS AND 100
               SET ERRO-PROGRAMA-ACIONADO  TO TRUE
               MOVE NM-PGM                 TO MSG1-NM-PGM-ACND
               MOVE SGTST002-MSG-ERRO-PGM  TO MSG1-TX-RTN-PGM-ACND
                                              MSG1-TX-ERRO-TS
               MOVE SGTST002-CD-RTN-PGM    TO MSG1-CD-RTN-PGM-ACND
                                              MSG1-CD-ERRO-TS

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       920000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       930000-CHAMAR-SGTST005          SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE SGTST005               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST005)
                COMMAREA (SGTST005-DADOS)
           END-EXEC

           IF  SGTST005-CD-RTN-PGM     NOT EQUAL ZEROS AND 100
               SET ERRO-PROGRAMA-ACIONADO  TO TRUE
               MOVE NM-PGM                 TO MSG1-NM-PGM-ACND
               MOVE SGTST005-MSG-ERRO-PGM  TO MSG1-TX-RTN-PGM-ACND
                                              MSG1-TX-ERRO-TS
               MOVE SGTST005-CD-RTN-PGM    TO MSG1-CD-RTN-PGM-ACND
                                              MSG1-CD-ERRO-TS

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       930000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE NM-PGM
               WHEN SGTST005
                 MOVE 'SGTST005-CD-CTRA-TIT-VL' TO  MSGKW999-NM-VRV-MNT
                 MOVE SGTST005-CD-CTRA-TIT-VL   TO  MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTST005-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
                 IF SGTST005-CD-OPR-ETRD-MCP     IS NUMERIC
                    MOVE  SGTST005-CD-OPR-ETRD-MCP  TO CD-OPR-ETRD-MCP
                    MOVE CD-OPR-ETRD-MCP-TX      TO MSGKW999-VL-VRV-MNT
                 ELSE
                    MOVE 'Valor não numérico.'   TO MSGKW999-VL-VRV-MNT
                 END-IF
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTST005-NR-VRS-OPR-ETRD' TO  MSGKW999-NM-VRV-MNT
                 MOVE SGTST005-NR-VRS-OPR-ETRD   TO  MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTST002
                 MOVE 'SGTST002-CD-TIT-OPR-ETRD' TO  MSGKW999-NM-VRV-MNT
                 MOVE SGTST002-CD-TIT-OPR-ETRD   TO  MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTST002-NR-DVS-TIT'      TO  MSGKW999-NM-VRV-MNT
                 MOVE SGTST002-NR-DVS-TIT        TO  MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTS0211
                 MOVE 'SGTS0211-CD-TIT-OPR-ETRD' TO  MSGKW999-NM-VRV-MNT
                 MOVE SGTS0211-CD-TIT-OPR-ETRD   TO  MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0211-NR-DVS-TIT'      TO  MSGKW999-NM-VRV-MNT
                 MOVE SGTS0211-NR-DVS-TIT        TO  MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

               WHEN OTHER
                    CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    Section da Faixa de 970000 a 979900 para Leitura de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       970000-LER-CANAL                SECTION.
      *--> Obtem o canal corrente
      *-----------------------------------------------------------------

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CANAL             TO  TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       970000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao
           MOVE 'SGTS0252'             TO  NM-PGM
           MOVE 'SGTS0252-RQSC'        TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      * A requisição é variável (cláusula DEPENDING ON), não haverá a
      * comparação de tamanho lido e esperado

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0252-RQSC)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
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

               WHEN SGTS0211
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTS0211-RPST )
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

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO  TRUE

      *--> Obtem a quantidade de caracteres no container
           MOVE ZEROS                  TO  QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       973100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       974000-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE 0012                   TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           MOVE 'PRM-INF-OPRD'         TO NM-CTNR

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
      *   Section da Faixa de 980000 a 989900 para Gravação de Dados
      *-----------------------------------------------------------------
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
               WHEN SGTS0211
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTS0211-RQSC           )
                              FLENGTH   ( LENGTH OF SGTS0211-RQSC )
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
      *--> Exclui o container de resposta e de erro para evitar
      *    problemas em módulos executados mais de uma vez dentro
      *    da mesma sessão do CICS.
      *    Equivale ao initialize na área de resposta dos
      *    programas que utilizam COMMAREA.
      *-----------------------------------------------------------------
      *--> Monta o nome do container de resposta
           MOVE NM-PGM        TO  NM-CTNR(1:8)
           MOVE '-RPST'       TO  NM-CTNR(9:5)

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET ERRO-CONTAINER          TO  TRUE
           SET DEL-CONTAINER           TO  TRUE

      *    Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
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
                    SET ERRO-CONTAINER TO  TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *--> Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------

      *    Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE 'PRM-ERRO'             TO  NM-CTNR
           SET ERRO-CONTAINER          TO  TRUE
           SET DEL-CONTAINER           TO  TRUE

      *    Não há captura automática de erro CICS por usar RESP e RESP2
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
                    SET ERRO-CONTAINER TO  TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE

           .
       983100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *--> Grava o erro gerado no container de erro
      *-----------------------------------------------------------------

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS PUT
                     CONTAINER ( 'PRM-ERRO'                  )
                     FROM      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC
           .
       984000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    As sections abaixo iniciadas com 999000 representam os
      *    tratamentos de erro através do Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *--> Em caso de erro, desfaz as atualizações em banco de dados
           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC
      *

           IF  MSG1-CD-ERRO-TS             EQUAL ZEROS
               MOVE 8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL SPACES
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

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE 'SGTS0252-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0252-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0252-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
           IF SGTS0252-CD-OPR-ETRD-MCP     NUMERIC
              MOVE  SGTS0252-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
              MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
              MOVE 'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0252-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE SGTS0252-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0252-CD-TIT-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE SGTS0252-CD-TIT-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT


      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

      *--> Encerra a transacao
           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      *----------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
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
      *---------------------- FIM SGTS0252 -----------------------------