      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0253
      * ANALISTA..: F2804421 - Eliana Cristina Ramos da Silva
      * AUTOR.....: F2804421 - Eliana Cristina Ramos da Silva
      * DATA......: 13/02/2019
      * OBJETIVO..: Incluir o Código ISIN do Título da Operação Estrutu-
      *             rada.
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS002 17/04/2019 - F2804421 - DE 2016/145195 - Utilizar o cam-
      *                   SGTST005-NR-DVS-TIT para consulta e atualiza-
      *                   ção.
      * VRS001 13/02/2019 - F2804421 - DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0253.
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
                    '*** SGTS0253 ***'.
       77  CTE-VERS                        PIC  X(006)  VALUE 'VRS002'.

      *--> Módulos acionados
       77  SGTST002                        PIC  X(008) VALUE 'SGTST002'.
       77  SGTST005                        PIC  X(008) VALUE 'SGTST005'.
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

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------
      * Book do container de requisição deste módulo
       01 SGTS0253-DADOS.
-INC SGTK0253

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

           MOVE 'SGTS0253'             TO  NM-PGM

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

           IF  SGTS0253-CD-CTRA-TIT-VL LESS ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0001               TO  MSG1-CD-ERRO-TS
               MOVE 'Código carteira operação estruturada inválido.'
      -                                TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0253-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0002               TO  MSG1-CD-ERRO-TS
               MOVE 'Código Carteira Operação Estruturada não informado.
      -         ''                     TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0253-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0003               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada de mercado de capita
      -             'is não numérico.' TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0253-CD-OPR-ETRD-MCP    LESS ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 0004                   TO  MSG1-CD-ERRO-TS
               MOVE 'Codigo Operação Estruturada Mercado de Capitais inv
      -             'álido.'               TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0253-CD-OPR-ETRD-MCP    EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 0005                   TO  MSG1-CD-ERRO-TS
               MOVE 'Codigo Operação Estruturada Mercado de Capitais não
      -             'informado'            TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0253-NR-VRS-OPR-ETRD    LESS ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 0006                   TO  MSG1-CD-ERRO-TS
               MOVE 'Numero Versão Operação Estruturado inválido.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0253-NR-VRS-OPR-ETRD    EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 0007                   TO  MSG1-CD-ERRO-TS
               MOVE 'Numero Versão Operação Estruturado deve ser informa
      -        'do.'                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0253-CD-ISIN-TIT-OPR    EQUAL SPACES
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 0896                  TO  MSG1-CD-ERRO-TS
               MOVE 'Código do ISIN do título não informado.'
                                           TO  MSG1-TX-ERRO-TS
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

           PERFORM 230000-TRATA-TITULO-ISIN

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
                WHERE A.CD_CTRA_TIT_VL    = :SGTS0253-CD-CTRA-TIT-VL
                  AND A.CD_OPR_ETRD_MCP   = :SGTS0253-CD-OPR-ETRD-MCP
                  AND A.NR_VRS_OPR_ETRD   = :SGTS0253-NR-VRS-OPR-ETRD
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
      *    4.3 (A Encarteirar) .

           IF (CD-TIP-EST-OPR     = 1    AND
               CD-TIP-EST-VRS     = 1    AND
               CD-MTV-ALT         = 999)
                                               OR
              (CD-TIP-EST-OPR     = 2    AND
               CD-TIP-EST-VRS     = 3)
                                               OR
              (CD-TIP-EST-OPR     = 3    AND
               CD-TIP-EST-VRS     = 3)
                                               OR
              (CD-TIP-EST-OPR     = 4    AND
               CD-TIP-EST-VRS     = 3)
                                               OR
              (CD-TIP-EST-OPR     = 5    AND
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
      *  Efetua busca de dados da operação ;
      *-----------------------------------------------------------------
           MOVE 0005                   TO  MSG1-CD-SEQL-ERRO

           MOVE SGTST005               TO  NM-PGM
           SET  SGTST005-CONSULTAR     TO  TRUE

           MOVE SGTS0253-CD-CTRA-TIT-VL    TO  SGTST005-CD-CTRA-TIT-VL
           MOVE SGTS0253-CD-OPR-ETRD-MCP   TO  SGTST005-CD-OPR-ETRD-MCP
           MOVE SGTS0253-NR-VRS-OPR-ETRD   TO  SGTST005-NR-VRS-OPR-ETRD

           PERFORM 930000-CHAMAR-SGTST005

           IF  SGTST005-CD-RTN-PGM     EQUAL 100
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0061               TO MSG1-CD-ERRO-TS
               MOVE 'Operação estruturada inexistente.'
                                       TO MSG1-TX-ERRO-TS

               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA

           END-IF

           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-TRATA-TITULO-ISIN        SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO  MSG1-CD-SEQL-ERRO

      *--> Consulta existencia de título tabela TIT_OPT_ETRD_MCP,
      *--> para fazer a inclusão do código ISIN.

           MOVE SGTST002               TO NM-PGM
           SET  SGTST002-CONSULTAR     TO TRUE
           MOVE SGTST005-CD-TIT-OPR-ETRD
                                       TO SGTST002-CD-TIT-OPR-ETRD
           MOVE SGTST005-NR-DVS-TIT
                                      TO SGTST002-NR-DVS-TIT

           PERFORM 920000-CHAMAR-SGTST002

           MOVE 0007                   TO  MSG1-CD-SEQL-ERRO

           IF  SGTST002-CD-RTN-PGM     EQUAL 100
               SET ERRO-PROGRAMADO     TO  TRUE
               MOVE 0234               TO  MSG1-CD-ERRO-TS
               MOVE 'Título da operação estruturada inexistente.'
                                       TO MSG1-TX-ERRO-TS

               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA

           ELSE
               SET SGTST002-ALTERAR    TO TRUE

               MOVE SGTST005-CD-TIT-OPR-ETRD
                                       TO SGTST002-CD-TIT-OPR-ETRD
               MOVE SGTST005-NR-DVS-TIT
                                       TO SGTST002-NR-DVS-TIT
               MOVE SGTST005-CD-TIT-OPR-ETRD
                                       TO SGTST002-NM-TIT-OPR-ETRD
               MOVE SGTS0253-CD-ISIN-TIT-OPR
                                       TO SGTST002-CD-ISIN-TIT-OPR
               MOVE SPACES             TO SGTST002-CD-EPC-TIT-OPR
               MOVE 'CURRENT'          TO SGTST002-TS-ULT-ATL
               MOVE  KOPRD-CD-OPRD     TO SGTST002-CD-USU-RSP-ATL

               PERFORM 920000-CHAMAR-SGTST002
           END-IF

           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *   Section da Faixa de 900000 a 969900 para Demais Rotinas
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       920000-CHAMAR-SGTST002          SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO  MSG1-CD-SEQL-ERRO

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE SGTST002               TO  NM-PGM
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
           MOVE 0010                   TO  MSG1-CD-SEQL-ERRO

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE SGTST005               TO  NM-PGM
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
           MOVE 'SGTS0253'             TO  NM-PGM
           MOVE 'SGTS0253-RQSC'        TO  NM-CTNR

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
                     INTO      (SGTS0253-RQSC)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       971000-SAI.
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
           MOVE 'SGTS0253-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0253-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0253-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
           IF SGTS0253-CD-OPR-ETRD-MCP     NUMERIC
              MOVE  SGTS0253-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
              MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
              MOVE 'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0253-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE SGTS0253-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0253-CD-ISIN-TIT-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE SGTS0253-CD-ISIN-TIT-OPR
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
      *---------------------- FIM SGTS0253 -----------------------------