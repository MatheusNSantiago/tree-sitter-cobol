      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0101
      * ANALISTA..: F2054109 - Cintia A.Iglesias
      * AUTOR.....: F9188847 - Soila K. L. Akamatsu
      * DATA......: 30/11/2016
      * OBJETIVO..: Consultar Dados do Mandato da Operação Estruturada
      *             de Mercado de Capitais
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 10/05/2018 - F8658869 2016/145195 - Inclusão da carteira
      *        30/11/2016 - F9188847 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0101.

      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------

       77  CTE-INICIO-SS               PIC  X(40) VALUE
               '*** Storage Section comeca aqui ***'.
       77  CTE-PROG                    PIC  X(18) VALUE
                                       '*** SGTS0101 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

       01 DT-MNDT-OPR  PIC  X(10) VALUE ' '.
       01 DT-MNDT-OPR-R REDEFINES
                                DT-MNDT-OPR.
              03 DIA                PIC  9(2).
              03 FILLER             PIC  X(1).
              03 MES                PIC  9(2).
              03 FILLER             PIC  X(1).
              03 ANO                PIC  9(4).

       01 VL-PU-EMS-TIT-OPR         PIC S9(9)V9(8) COMP-3 VALUE 0.
       01 QT-PAPL-TIT-EMS           PIC S9(9) COMP-5 VALUE 0.

       01  VL-MNDT-OPR-N               PIC S9(04) COMP-5 VALUE 0.
       01  VL-APVD-BB-OPR-N            PIC S9(04) COMP-5 VALUE 0.
       01  VL-GRT-BB-OPR-N             PIC S9(04) COMP-5 VALUE 0.
       01  VL-PRV-ENCT-N               PIC S9(04) COMP-5 VALUE 0.
       01  DT-MNDT-OPR-N               PIC S9(04) COMP-5 VALUE 0.


      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book do container de requisição e resposta deste modulo
      *    Consultar Mandato da Operação Estruturada
-INC SGTK0101

      *--> Book do Container de Erro
-INC IIBKERRO

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC  X(40) VALUE
               '*** Storage Section termina aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

           PERFORM  100000-INICIAR

           PERFORM  200000-PROCESSAR

           PERFORM  999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO


      *--> WHENEVER DB2
-INC HLPKE997

      *--> Ativa a captura automatica de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM  970000-LER-CANAL

      *--> Define o nome do programa p/ fins manipulacao container
           MOVE  'SGTS0101'            TO  NM-PGM

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM  971000-LER-REQUISICAO

           PERFORM  110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

      *--> Valida campo a campo da requisicao

           IF  SGTS0101-CD-CTRA-TIT-VL < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'inválido.'        TO MSG1-TX-ERRO-TS
               PERFORM 998000-ERRO
           END-IF

           IF  SGTS0101-CD-CTRA-TIT-VL EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0002               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'   TO MSG1-TX-ERRO-TS
               PERFORM 998000-ERRO
           END-IF

           IF  SGTS0101-CD-OPR-ETRD        NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  998000-ERRO
           END-IF

           IF  SGTS0101-CD-OPR-ETRD        < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais in
      -              'válido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  998000-ERRO
           END-IF

           IF  SGTS0101-NR-VRS-OPR-ETRD    < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da Operação Estruturada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  998000-ERRO
           END-IF
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  210000-CONSULTAR-OPERACOES

           PERFORM  980000-GRAVAR-CONTAINER-RPST
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-CONSULTAR-OPERACOES      SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           EXEC SQL
                 SELECT
				        VL_MNDT_OPR
                      , DT_MNDT_OPR
                      , VL_APVD_BB_OPR
                      , VL_GRT_BB_OPR
                      , VL_PRV_ENCT
                      , VL_CPR_OPR
                      , VL_PU_EMS_TIT_OPR
                      , QT_PAPL_TIT_EMS

                   INTO  :SGTS0101-VL-MNDT-OPR
                         :VL-MNDT-OPR-N
                       , :DT-MNDT-OPR
                         :DT-MNDT-OPR-N
                       , :SGTS0101-VL-APVD-BB-OPR
                         :VL-APVD-BB-OPR-N
                       , :SGTS0101-VL-GRT-BB-OPR
                         :VL-GRT-BB-OPR-N
                       , :SGTS0101-VL-PRV-ENCT
                         :VL-PRV-ENCT-N
                       , :SGTS0101-VL-CPR-OPR
                       , :VL-PU-EMS-TIT-OPR
                       , :QT-PAPL-TIT-EMS
				   FROM DB2SGT.OPR_ETRD_MCP
                WHERE CD_CTRA_TIT_VL     = :SGTS0101-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP    = :SGTS0101-CD-OPR-ETRD
                  AND NR_VRS_OPR_ETRD    = :SGTS0101-NR-VRS-OPR-ETRD
           END-EXEC.

           IF  SQLCODE                     EQUAL +100
                 SET   ERRO-PROGRAMADO          TO  TRUE
                 MOVE  100                      TO  MSG1-CD-ERRO-TS
                 MOVE  'Registro inexistente.'  TO  MSG1-TX-ERRO-TS
                 PERFORM  998000-ERRO
           END-IF

           MOVE  CORR  DT-MNDT-OPR-R      TO  SGTS0101-DT-MNDT-OPR-R
           PERFORM  211000-CALCULA-VL-EMS-TIT-OPR
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       211000-CALCULA-VL-EMS-TIT-OPR   SECTION.
      *--> Calcula o valor de emissão do título da operação
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           MULTIPLY  VL-PU-EMS-TIT-OPR
                     BY  QT-PAPL-TIT-EMS
                     GIVING SGTS0101-VL-EMS-TIT-OPR
                     ON SIZE ERROR
                 SET   ERRO-PROGRAMADO TO  TRUE
                 MOVE  0062            TO  MSG1-CD-ERRO-TS
                 MOVE  'Erro no cálculo do valor de emissão do título.'
                                       TO  MSG1-TX-ERRO-TS
                 PERFORM  998000-ERRO
           END-MULTIPLY
           .
       211000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       970000-LER-CANAL                SECTION.
      *--> Obtem o canal corrente
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       970000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------

      *--> Monta o nome do container de requisicao
           MOVE  'SGTS0101'            TO  NM-PGM
           MOVE  'SGTS0101-RQSC'       TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM  973100-LER-CONTAINER-NODATA

           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

      *--> Obtem o tamanho esperado da requisicao
           MOVE  LENGTH OF  SGTS0101-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisicao X tamanho recebido
           IF  QT-CCT-LIDO             NOT EQUAL  QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA TO TRUE
               PERFORM  998000-ERRO
           END-IF

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto  o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER ( NM-CTNR       )
                     FLENGTH   ( QT-CCT-LIDO   )
                     INTO      ( SGTS0101-RQSC )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Obtem a quantidade de caracteres no container
           MOVE  ZEROS                 TO  QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER ( NM-CTNR     )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973100-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------

      *--> Exclui eventual container de erro de outros programas
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO

           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de resposta
           MOVE  'SGTS0101-RPST'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                 )
                     FROM      ( SGTS0101-RPST           )
                     FLENGTH   ( LENGTH OF SGTS0101-RPST )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       980000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)


           SET   DEL-CONTAINER         TO  TRUE

      *--> Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR    )
                     RESP      ( MSG1-RESP1 )
                     RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1             EQUAL  DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1             EQUAL  DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET   ERRO-CONTAINER
                                       TO  TRUE
                    PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *--> Excluir o container de erro
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE                      IIBKERRO-PRM-ERRO

      *--> Excluir o container de erro
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           SET   DEL-CONTAINER         TO  TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR    )
                     RESP      ( MSG1-RESP1 )
                     RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1             EQUAL  DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1             EQUAL  DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET   ERRO-CONTAINER
                                       TO  TRUE
                    PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *-----------------------------------------------------------------

      *--> Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
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
      * As sections abaixo iniciadas com 999000 representam os
      * tratamentos de erro através do Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       998000-ERRO                     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não específicado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

           PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           .
       998000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *--> Verifica se o tipo de erro gerado possui tratamento interno
           EVALUATE TRUE
               WHEN ERRO-CONTAINER
                    PERFORM  999004-TRATA-ERRO-CTNR
               WHEN ERRO-TAMANHO-AREA
                    PERFORM  999005-TRATA-ERRO-AREA-DADOS
               WHEN ERRO-CANAL
                    PERFORM  999006-TRATA-ERRO-CANAL
               WHEN OTHER
                    CONTINUE
           END-EVALUATE

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE  'SGTS0101-CD-CTRA-TIT-VL'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0101-CD-CTRA-TIT-VL       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT


           MOVE 'SGTS0101-CD-OPR-ETRD'        TO  MSGKW999-NM-VRV-MNT
           IF  SGTS0101-CD-OPR-ETRD    IS NUMERIC
               MOVE  SGTS0101-CD-OPR-ETRD     TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'    TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0101-NR-VRS-OPR-ETRD'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0101-NR-VRS-OPR-ETRD     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava o erro gerado no container de erro
           PERFORM  984000-GRAVAR-CONTAINER-ERRO

      *--> Encerra a transacao
           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      * ---------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      * ---------------------------------------
-INC HLPKP999
      * ---------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
-INC SGTKP999
      * ---------------------------------------
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
      * ---------------------------------------
-INC MSGKPMG1
      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997

      *-----------------------------------------------------------------
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTS0101 -----------------------------