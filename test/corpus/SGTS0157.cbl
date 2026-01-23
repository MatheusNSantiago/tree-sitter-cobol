      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0157
      * ANALISTA..: F2054109 Cintia Ap Soares Iglesias
      * AUTOR.....: F0950413 António Francisco Mendonça Silva
      * DATA......: 01/08/2017
      * OBJETIVO..: Calcular Indicador de não Conversibilidade para uma
      *             Operação Estruturada.
      * COMPILACAO: COBOL/CICS (On-line)
      *-----------------------------------------------------------------
      * VRS001 23.01.2019 - F0950413 inclusão da sigla 'SGT' na section
      *                     971000-LER-ERRO
      *        07.08.2018 - F8785286 - 2015/137233 - Somente recompila-
      *           ção por alteração no book SGTSL087.
      *        01.08.2017 - F0950413 - 2015/137233 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0157.

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
       77  CTE-INICIO-SS               PIC X(35) VALUE
               '*** STORAGE SECTION COMECA AQUI ***'.
       77  CTE-PROG                    PIC X(18) VALUE
               '*** SGTS0157 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS001'.

      *--> Módulos a serem acionados via 'CONTAINER'
       77  SGTSL063                    PIC X(08) VALUE 'SGTSL063'.

      *--> CONTAINER do PARÂMETRO do SERVIÇO IIB acionado
       77 CTNR-PRM-RQSC-SRVC           PIC X(16) VALUE 'PRM-RQSC-SRVC'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
       COPY HLPKE01B.
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------

      *--> Book do container de erro
       COPY IIBKERRO.
      *--> Book para captura de erro via MSG1
       COPY MSGKWMG1.
      *--> Variaveis para tratamento de Abend e Condition
       COPY HLPKW999.
      *--> Variaveis para tratamento de CONTAINER
       COPY SGTKERR1.

      *--> Container de erro
       01  CTNR-PRM-ERRO               PIC X(16) VALUE 'PRM-ERRO'.

      *--> Areas de Controle de Programa Chamado (CONTAINER)
       01  CTL-PGM-CHMD.
           03  NM-PGM-CHMD             PIC  X(08)         VALUE ' '.
               88 NM-PGM-COM-RQSC      VALUE 'SGTSL063'.
               88 NM-PGM-COM-SRVC      VALUE '        '.
           03  FILLER                  PIC  X(01)         VALUE ' '.
           03  MSG-PGM-CHMD            PIC  X(111)        VALUE ' '.

       01  WS-ERRO-CICS.
           03  WS-RESP1                   PIC  9(008).
           03  WS-RESP2                   PIC  9(008).

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-04                   PIC                 ----9.
       01  MCR-S9-09                   PIC            ---------9.
       01  MCR-S9-11                   PIC          -----------9.

      *-----------------------------------------------------------------
      *--> Book pgm de lista da tabela IN_OPR_ETRD
       COPY SGTKL063.

      *--> Book do programa
       01  SGTS0157-DADOS.
       COPY SGTK0157.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      * Habilita captura automatica de abend e erros CICS
       COPY HLPKE999.
           MOVE CTE-PROG(5:8)          TO NM-PGM
                                          NM-PGM-CHMD

           PERFORM 100000-OBTER-NOME-DO-CANAL
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
           PERFORM 970000-LER-REQUISICAO-RECEBIDA

           PERFORM 140000-VALIDAR-PARAMETROS
           PERFORM 200000-IN-CONVERSIBILIDADE

           PERFORM 300000-POSTA-RESPOSTA
           PERFORM 999999-ENCERRAR
           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-OBTER-NOME-DO-CANAL      SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS ASSIGN
                CHANNEL ( NM-CNL )
           END-EXEC
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETROS       SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           IF  SGTS0157-CD-CTRA-TIT-VL <= 0
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0001               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de títulos de valores monetários
      -        ' inválido.'            TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0157-CD-OPR-ETRD-MCP NOT NUMERIC OR
               SGTS0157-CD-OPR-ETRD-MCP NOT > 0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0002               TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada, inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0157-NR-VRS-OPR-ETRD <= 0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO  MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturado inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-IN-CONVERSIBILIDADE      SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

      *--> Recuperar código tipo indicador
           MOVE SGTS0157-CD-CTRA-TIT-VL
                                       TO SGTSL063-CD-CTRA-TIT-VL
           MOVE SGTS0157-CD-OPR-ETRD-MCP
                                       TO SGTSL063-CD-OPR-ETRD-MCP
           MOVE SGTS0157-NR-VRS-OPR-ETRD
                                       TO SGTSL063-NR-VRS-OPR-ETRD
           MOVE 3                      TO SGTSL063-CD-TIP-IN-E
           MOVE SGTSL063               TO NM-PGM-CHMD
           PERFORM 900000-PROCEDIMENTOS-CONTAINER

           MOVE 'N'                    TO SGTS0157-IN-N-CNV
           IF SGTSL063-QT-OCR-TAB > 0
            IF SGTSL063-CD-TIP-IN (1) = 4
               MOVE 'S'                TO SGTS0157-IN-N-CNV
            END-IF
           END-IF
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
                CONTAINER ( 'SGTS0157-RPST'         )
                CHANNEL   ( NM-CNL                   )
                FLENGTH   ( LENGTH OF SGTS0157-RPST )
                FROM      ( SGTS0157-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       900000-PROCEDIMENTOS-CONTAINER  SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM-CHMD
      *--> Executar Módulo SGTSL063, container sem Serviço
               WHEN SGTSL063
                 EXEC CICS LINK
                           PROGRAM ( SGTSL063 )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC

      *--> Execução de Módulo não prevista
               WHEN OTHER
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 0004             TO MSG1-CD-ERRO-TS
                 MOVE 'Módulo a ser acionado não previsto.'
                                       TO MSG1-TX-ERRO-TS
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    PERFORM 911000-VER-ERRO-MODULO-OU-SRVC
           END-EVALUATE
           .
       910000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       911000-VER-ERRO-MODULO-OU-SRVC  SECTION.
      *----------------------------------------------------------------*
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

      *--> Análise do retorno das chamadas dos programas
      *--> Módulos chamados com CONTAINER, sem serviço
           STRING
              'Erro na execução do módulo ' NM-PGM-CHMD ', '
              'RESP  ='   ' ' MSG1-RESP1  ', '
              'RESP2 =' ' ' MSG1-RESP2
              DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
              MOVE 3004          TO MSG1-CD-ERRO-TS
              SET ERRO-CICS      TO TRUE
           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       911000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970000-LER-REQUISICAO-RECEBIDA  SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS GET
              CONTAINER ( 'SGTS0157-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTS0157-RQSC )
              INTO      ( SGTS0157-RQSC           )
           END-EXEC
           .
       970000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       971000-LER-ERRO                 SECTION.
      *----------------------------------------------------------------*
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

      *----------------------------------------------------------------*
      *    A presença do container PRM-ERRO indica que houve erro de   *
      *    integração, a ausência do container indica sucesso.         *
      *----------------------------------------------------------------*

      *--> Configura dados para leitura de container de erro
           MOVE CTNR-PRM-ERRO          TO NM-CTNR
           SET  GET-CONTAINER          TO TRUE

      *--> Lê o container de erro
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
                MOVE NM-PGM-CHMD             TO  MSG1-NM-PGM-ACND
                IF NM-PGM-CHMD(1:3) EQUAL 'SGT'
                    MOVE IIBKERRO-CD-ERRO    TO  MSG1-CD-RTN-PGM-ACND
                                                 MSG1-CD-ERRO-TS
                    MOVE IIBKERRO-TX-ERRO    TO  MSG1-TX-RTN-PGM-ACND
                                                 MSG1-TX-ERRO-TS
                ELSE
                    MOVE  3004               TO  MSG1-CD-ERRO-TS
                    STRING  'Erro ao acionar o módulo ' NM-PGM-CHMD '.'
                            DELIMITED BY SIZE
                            INTO        MSG1-TX-ERRO-TS
                    END-STRING
                    MOVE IIBKERRO-CD-ERRO     TO  MSG1-CD-RTN-PGM-ACND
                    MOVE IIBKERRO-TX-ERRO     TO  MSG1-TX-RTN-PGM-ACND
                END-IF
                PERFORM 999000-GRAVA-ERRO-E-ENCERRA

              WHEN OTHER
                   SET ERRO-CONTAINER TO TRUE
                   PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-RESPOSTA             SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

      *--> Inicializa quantidade de caracteres lidos na área de resposta
           MOVE ZEROS                  TO QT-CCT-LIDO

           MOVE SPACES                 TO NM-CTNR

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

      *--> Lê container de resposta do SGTSL063
               WHEN SGTSL063
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSL063-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC

      *--> Módulo não previsto para leitura do container
               WHEN OTHER
                 SET SEM-ERRO        TO TRUE
                 SET ERRO-PROGRAMADO TO TRUE
                 MOVE 0006           TO MSG1-CD-ERRO-TS
                 STRING                                                   VRS001
                    'Leitura do container de resposta, não prevista para  VRS001
      -             'o módulo.' NM-PGM-CHMD                               VRS001
                 DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS                   VRS001
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
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

      *--> Recupera o tamanho previsto e o tamanho recebido da resposta
      *--> do programa que foi acionado.

           EVALUATE NM-PGM-CHMD

      *--> Lê container de resposta do SGTSL063
               WHEN SGTSL063
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR            )
                           NODATA
                           FLENGTH   ( QT-CCT-LIDO )
                 END-EXEC

      *--> Módulo não previsto para leitura do container de resposta
               WHEN OTHER
                 SET SEM-ERRO          TO TRUE
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 0007             TO MSG1-CD-ERRO-TS
                 STRING                                                   VRS001
                    'Tratamento de área de resposta, não prevista para o  VRS001
      -             ' módulo.' NM-PGM-CHMD                                VRS001
                 DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS                   VRS001
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .
       972100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       982000-GRAVA-REQUISICAO         SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para gravação do container de requisição
           SET  PUT-CONTAINER          TO TRUE

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *--> nome_modulo -RPST

           EVALUATE NM-PGM-CHMD
               WHEN '        '
                    MOVE 'PRM-DADOS-RQSC'
                                       TO NM-CTNR
               WHEN OTHER
                    MOVE NM-PGM-CHMD   TO NM-CTNR(1:8)
                    MOVE '-RQSC'       TO NM-CTNR(9:5)
           END-EVALUATE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Grava container da requisição do serviço IIB a ser acionado
      *--> Em caso de erro, a captura automática do CICS trata o erro

           EVALUATE NM-PGM-CHMD
      *--> Grava container de requisição do programa SGTSL087
               WHEN SGTSL063
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR       )
                            FROM      ( SGTSL063-RQSC )
                            FLENGTH   ( LENGTH OF
                                        SGTSL063-RQSC )
                  END-EXEC

      *--> Módulo não previsto para gravar container de requisição
               WHEN OTHER
                 SET SEM-ERRO          TO TRUE
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 0008             TO MSG1-CD-ERRO-TS
                 STRING                                                   VRS001
                    'Gravação do container de requisição, não definida p
      -             'ara o módulo.' NM-PGM-CHMD
                 DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS                   VRS001
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
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *--> nome_modulo -RPST
           EVALUATE NM-PGM-CHMD
               WHEN '        '
                    MOVE 'PRM-DADOS-RPST'
                                       TO NM-CTNR
               WHEN OTHER
                    MOVE NM-PGM-CHMD   TO NM-CTNR(1:8)
                    MOVE '-RPST'       TO NM-CTNR(9:5)
           END-EVALUATE

           SET DEL-CONTAINER           TO TRUE

      *--> Exclui o container de resposta
      *--> Não há captura automática de erro CICS por usar RESP e RESP2
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
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE 'PRM-ERRO'             TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
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
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

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

           MOVE 'SGTS0157-CD-CTRA-TIT-VL'   TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0157-CD-CTRA-TIT-VL    TO MCR-S9-09
           MOVE  MCR-S9-09                  TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0157-CD-OPR-ETRD-MCP'  TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0157-CD-OPR-ETRD-MCP   TO MCR-S9-11
           MOVE  MCR-S9-11                  TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0157-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0157-NR-VRS-OPR-ETRD   TO MCR-S9-04
           MOVE  MCR-S9-04                  TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *--> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO

           EXEC CICS PUT
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   ( NM-CNL      )
              FROM      ( IIBKERRO-PRM-ERRO)
              FLENGTH ( LENGTH OF IIBKERRO-PRM-ERRO )
              NOHANDLE
           END-EXEC

           PERFORM 999999-ENCERRAR
           .
       999000-SAI.
           EXIT.
      * --------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * --------------------------------------*
       COPY HLPKP999.
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
       COPY SGTKP999.
      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
      * -INC HLPKP997
      * --------------------------------------*

      * MSG1-NTF-MSG                   SECTION.
       COPY MSGKPMG1.
      * --------------------------------------*
       999999-ENCERRAR                 SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC.
       999999-SAI.
           EXIT.
