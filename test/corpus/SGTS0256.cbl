      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0256
      * ANALISTA..: F2804421 - Eliana Cristina Ramos da Silva
      * AUTOR.....: F2804421 - Eliana Cristina Ramos da Silva
      * DATA......: 29/04/2019
      * OBJETIVO..: Consulta dados do responsável pela finalização da
      *             edição da versão.
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 29/04/2019 - F2804421 - DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0256.
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
                    '*** SGTS0256 ***'.
       77  CTE-VERS                        PIC  X(006)  VALUE 'VRS001'.


      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Variaveis utilizados para leitura
       01  REG-WS.
           03 CD-USU-ULT-ATL           PIC  X(08)     VALUE ' '.
           03 CD-TIP-EST-OPR           PIC S9(04)     COMP-5 VALUE 0.
           03 CD-TIP-EST-VRS           PIC S9(04)     COMP-5 VALUE 0.
           03 TS-EST-VRS               PIC  X(26)     VALUE ' '.

      *--> Indicadores de nulidades
       01  CD-TIP-EST-OPR-N            PIC S9(04)     COMP-5 VALUE 0.

      *--> Areas de trabalho
      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------
      * Book do container de requisição deste módulo
       01 SGTS0256-DADOS.
-INC SGTK0256

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

           PERFORM 300000-GRAVAR-RESPOSTA

           PERFORM 999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.
      *-----------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------

           MOVE 0001                   TO  MSG1-CD-SEQL-ERRO

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997
      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999

           PERFORM 970000-LER-CANAL

           MOVE 'SGTS0256'             TO  NM-PGM

           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO  MSG1-CD-SEQL-ERRO

           IF  SGTS0256-CD-CTRA-TIT-VL LESS ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0001               TO  MSG1-CD-ERRO-TS
               MOVE 'Código carteira operação estruturada inválido.'
      -                                TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0256-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0986               TO  MSG1-CD-ERRO-TS
               MOVE 'Operação Estruturada já possuí regra de exceção ati
      -            'va.'               TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0256-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0003               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada de mercado de capita
      -             'is não numérico.' TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0256-CD-OPR-ETRD-MCP    LESS ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 0004                   TO  MSG1-CD-ERRO-TS
               MOVE 'Codigo Operação Estruturada Mercado de Capitais inv
      -             'álido.'               TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0256-CD-OPR-ETRD-MCP    EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 0005                   TO  MSG1-CD-ERRO-TS
               MOVE 'Codigo Operação Estruturada Mercado de Capitais não
      -             'informado'            TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0256-NR-VRS-OPR-ETRD    LESS ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 0006                   TO  MSG1-CD-ERRO-TS
               MOVE 'Numero Versão Operação Estruturado inválido.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0256-NR-VRS-OPR-ETRD    EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 0007                   TO  MSG1-CD-ERRO-TS
               MOVE 'Numero Versão Operação Estruturado deve ser informa
      -        'do.'                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO  MSG1-CD-SEQL-ERRO

      *--> Verifica último estado da operação e versão e usuário
      *-- responsável e considera o estado operação = 5 - Encarteirado
      *-- e estado da versão igual a 2- A Avaliar.

           EXEC SQL
                SELECT A.CD_TIP_EST_OPR
                     , B.CD_TIP_EST_VRS
                     , B.CD_USU_ULT_ATL
                     , B.TS_EST_VRS
                  INTO  :CD-TIP-EST-OPR
                        :CD-TIP-EST-OPR-N
                     ,  :CD-TIP-EST-VRS
                     ,  :CD-USU-ULT-ATL
                     ,  :TS-EST-VRS
                FROM DB2SGT.EST_OPR_ETRD  A
                     JOIN DB2SGT.EST_VRS_OPR_ETRD  B
                       ON   B.CD_CTRA_TIT_VL  =  A.CD_CTRA_TIT_VL
                       AND  B.CD_OPR_ETRD_MCP =  A.CD_OPR_ETRD_MCP
                       AND  B.NR_VRS_OPR_ETRD =  A.NR_VRS_OPR_ETRD
                WHERE A.CD_CTRA_TIT_VL    = :SGTS0256-CD-CTRA-TIT-VL
                  AND A.CD_OPR_ETRD_MCP   = :SGTS0256-CD-OPR-ETRD-MCP
                  AND A.NR_VRS_OPR_ETRD   = :SGTS0256-NR-VRS-OPR-ETRD
                 AND A.TS_EST_OPR_ETRD = (SELECT MAX (C.TS_EST_OPR_ETRD)
                FROM DB2SGT.EST_OPR_ETRD C
                     WHERE C.CD_CTRA_TIT_VL  =  A.CD_CTRA_TIT_VL
                       AND C.CD_OPR_ETRD_MCP =  A.CD_OPR_ETRD_MCP
                       AND C.NR_VRS_OPR_ETRD =  A.NR_VRS_OPR_ETRD)
                     ORDER  BY  B.TS_EST_VRS  DESC
                     FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  CD-TIP-EST-OPR          =  5    AND
               CD-TIP-EST-VRS          =  2
               MOVE CD-TIP-EST-OPR     TO SGTS0256-CD-TIP-EST-OPR
               MOVE CD-TIP-EST-VRS     TO SGTS0256-CD-TIP-EST-VRS
               MOVE CD-USU-ULT-ATL     TO SGTS0256-CD-USU-ULT-ATL
               MOVE TS-EST-VRS         TO SGTS0256-TS-EST-VRS
           ELSE
               MOVE SPACES             TO SGTS0256-CD-USU-ULT-ATL
                                          SGTS0256-TS-EST-VRS
           END-IF

           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-GRAVAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO  MSG1-CD-SEQL-ERRO

      *--> Grava container de resposta;

           SET  PUT-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE
           MOVE 'SGTS0256-RPST'        TO NM-CTNR

           EXEC CICS PUT
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTS0256-RPST )
              FROM      ( SGTS0256-RPST           )
           END-EXEC

           SET  SEM-ERRO               TO TRUE
            .
       300000-SAI.
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
           MOVE 0005                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao
           MOVE 'SGTS0256'             TO  NM-PGM
           MOVE 'SGTS0256-RQSC'        TO  NM-CTNR

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
                     INTO      (SGTS0256-RQSC)
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
           MOVE 'SGTS0256-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0256-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0256-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
           IF SGTS0256-CD-OPR-ETRD-MCP     NUMERIC
              MOVE  SGTS0256-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
              MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
              MOVE 'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0256-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE SGTS0256-NR-VRS-OPR-ETRD
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
      *---------------------- FIM SGTS0256 -----------------------------
