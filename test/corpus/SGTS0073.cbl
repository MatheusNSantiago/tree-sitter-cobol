      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0073
      * ANALISTA..: F2054109 - CINTIA IGLESIAS
      * AUTOR.....: F8785286 - Roseli Akemi Kawahata
      * DATA......: 30/11/2016
      * OBJETIVO..: Consultar possível vinculação contato externo da
      *              operação estruturada.
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 30/11/2016 - F8785286 - DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0073.

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
                                       '*** S.S. Comeca Aqui ***'.
       77  CTE-PROG                    PIC  X(18) VALUE
                                       '*** SGTS0073 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTS014G                PIC  X(08) VALUE 'SGTS014G'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Areas de Trabalho
       01  AREA-TRABALHO.
           03  CD-USU-RSP-ATL          PIC  X(08) VALUE SPACES.
           03  TS-ATL-CTT-EXNO         PIC  X(26) VALUE SPACES.
           03  IN-CTT-EXNO-ATI         PIC  X(01) VALUE SPACES.

       01  CD-PSS-CTT-EXNO             PIC  S9(04) COMP-5 VALUE 0.
       01  CD-PSS                      PIC   9(08) VALUE 0.
       01  CD-PSS-R REDEFINES CD-PSS.
           03  CD-PSS-INI-4            PIC   9(04).
           03  CD-PSS-FIM-4            PIC   9(04).

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book do container de requisição e resposta deste modulo
       01 SGTS0073-DADOS.
-INC SGTK0073

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

        77  CTE-FINAL-SS                PIC X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

      *--> Habilita Testes no Cobolito
-INC ALMKCBL2

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997

      *--> Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

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
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

           PERFORM 970000-LER-CANAL

           MOVE  'SGTS0073'            TO  NM-PGM

           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

           PERFORM 110000-VALIDAR-REQUISICAO

           MOVE  'N'                   TO  SGTS0073-IN-VCLC-CTT-EXNO-OPR
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *--> Valida campo a campo da requisicao
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada
           IF  SGTS0073-CD-CTRA-TVM        LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0073-CD-CTRA-TVM        EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0073-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0073-CD-OPR-ETRD-MCP    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'   TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0073-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0073-NR-VRS-OPR-ETRD    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0073-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validar Pessoa de Contato Externo
           IF  SGTS0073-CD-PSS-CTT-EXNO    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0058              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da pessoa de contato externo não informado.
      -              ' '               TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0073-CD-PSS-CTT-EXNO    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0059              TO  MSG1-CD-ERRO-TS
               MOVE  'Código pessoa contato externo inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTS0073-CD-PSS-CTT-EXNO
                                       TO  CD-PSS
           MOVE  CD-PSS-FIM-4          TO  CD-PSS-CTT-EXNO

           PERFORM  210000-LISTA-PESS-CTT-EXT-ATV

           PERFORM  220000-PES-CTT-EXT-A

           MOVE  'S'                   TO  SGTS0073-IN-VCLC-CTT-EXNO-OPR

           PERFORM  980000-GRAVAR-CONTAINER-RPST
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-LISTA-PESS-CTT-EXT-ATV   SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           EXEC SQL
                SELECT IN_CTT_EXNO_ATI
                  INTO
                       :IN-CTT-EXNO-ATI
                  FROM DB2SGT.PSS_CTT_EXNO
                WHERE CD_PSS_CTT_EXNO    = :CD-PSS-CTT-EXNO
           END-EXEC

           IF  SQLCODE                     EQUAL  ZEROS
               IF  IN-CTT-EXNO-ATI         EQUAL  'N'
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0184          TO  MSG1-CD-ERRO-TS
                   MOVE  'Vinculação não permitida. Contato Externo não
      -                  'ativo para vinculação a operação.'
                                       TO  MSG1-TX-ERRO-TS
                  PERFORM  999000-ERRO
               END-IF
           ELSE
               IF  SQLCODE                 NOT EQUAL  +100
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0529          TO  MSG1-CD-ERRO-TS
                   MOVE  'Erro na consulta das pessoas de contato extern
      -                  'o.'          TO  MSG1-TX-ERRO-TS
                   PERFORM 999000-ERRO
               END-IF
           END-IF
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-PES-CTT-EXT-A            SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           EXEC SQL
                 SELECT CD_USU_RSP_ATL
                      , TS_ATL_CTT_EXNO
                  INTO  :CD-USU-RSP-ATL
                      , :TS-ATL-CTT-EXNO
                       FROM DB2SGT.CTT_EXNO_OPR_ETRD
                WHERE CD_CTRA_TIT_VL     = :SGTS0073-CD-CTRA-TVM
                  AND CD_OPR_ETRD_MC     = :SGTS0073-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD    = :SGTS0073-NR-VRS-OPR-ETRD
                  AND CD_PSS_CTT_EXNO    = :CD-PSS-CTT-EXNO
           END-EXEC

           IF  SQLCODE                     EQUAL ZEROS
               IF  CD-USU-RSP-ATL          NOT EQUAL  SPACES
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0185          TO  MSG1-CD-ERRO-TS
                   MOVE  'Vinculação não permitida. Contato externo já e
      -                  'xistente na operação estruturada.'
                                       TO  MSG1-TX-ERRO-TS
                  PERFORM 999000-ERRO
               END-IF
           ELSE
               IF  SQLCODE                 NOT EQUAL  +100
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0530          TO  MSG1-CD-ERRO-TS
                   MOVE  'Erro na consulta do contato externo da operaçã
      -                  'o estruturada. '
                                       TO  MSG1-TX-ERRO-TS
                   PERFORM  999000-ERRO
               END-IF
           END-IF
           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    Section da Faixa de 970000 a 979900 para Leitura de Dados
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

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       970000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------

           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisicao
           MOVE  'SGTS0073'            TO  NM-PGM
           MOVE  'SGTS0073-RQSC'       TO  NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      * Caso a requisição seja variável (cláusula DEPENDING ON), o
      * tratamento abaixo não deve ser efetuado, senão, ocasionará erro
      * -->> Início

      *--> Obtem o tamanho esperado da requisicao
           MOVE  LENGTH OF SGTS0073-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisicao X tamanho recebido

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA
                                       TO  TRUE
               PERFORM 999000-ERRO
           END-IF
      * -->> Fim

           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR      )
                     FLENGTH   (QT-CCT-LIDO  )
                     INTO      (SGTS0073-RQSC)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *-----------------------------------------------------------------
      *--> A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------

           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

      *--> Le o container de erro dos modulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           INITIALIZE                      IIBKERRO-PRM-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS GET
                     CONTAINER ( NM-CTNR                     )
                     INTO      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC

           EVALUATE  TRUE
      *        Caso deseje personalizar ou não interromper o
      *        processamento, alterar para não dar erro aqui SOMENTE
      *        PARA O MÓDULO CHAMADO e tratar na section chamadora
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                    SET   ERRO-CONTAINER
                                       TO  TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *    Obtem a quantidade de caracteres no container
           MOVE  ZEROS                 TO  QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973100-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *   Section da Faixa de 980000 a 989900 para Gravação de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------

      *--> Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de resposta
           MOVE  'SGTS0073-RPST'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                )
                     FROM      (SGTS0073-RPST          )
                     FLENGTH   (LENGTH OF SGTS0073-RPST)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       980000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *--> Exclui o container de resposta e de erro para evitar
      *    problemas em módulos executados mais de uma vez dentro
      *    da mesma sessão do CICS.
      *--> Equivale ao initialize na área de resposta dos
      *    programas que utilizam COMMAREA.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)

           SET   DEL-CONTAINER         TO  TRUE

      *--> Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR    )
                      RESP      ( MSG1-RESP1 )
                      RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
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
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE                      IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           SET   DEL-CONTAINER         TO  TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR    )
                      RESP      ( MSG1-RESP1 )
                      RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *--> Grava o erro gerado no container de erro
      *-----------------------------------------------------------------

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  PUT
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
       999000-ERRO                     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não específicado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

           PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *--> Em caso de erro, desfaz as atualizações em banco de dados
           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC

      *--> Verifica se o tipo de erro gerado possui tratamento interno
           EVALUATE  TRUE
               WHEN  ERRO-CONTAINER
                     PERFORM  999004-TRATA-ERRO-CTNR
               WHEN  ERRO-TAMANHO-AREA
                     PERFORM  999005-TRATA-ERRO-AREA-DADOS
               WHEN  ERRO-CANAL
                     PERFORM  999006-TRATA-ERRO-CANAL
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE '0073-001-CD-CTRA-TVM'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0073-CD-CTRA-TVM
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0078-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0073-CD-OPR-ETRD-MCP    NUMERIC
               MOVE  SGTS0073-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0073-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0073-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0073-004-CD-PSS-CTT-EXNO'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE SGTS0073-CD-PSS-CTT-EXNO
                                       TO  MSGKW999-VL-VRV-MNT
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

      *---------------------------------------
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
      *---------------------- FIM SGTS0073 -----------------------------