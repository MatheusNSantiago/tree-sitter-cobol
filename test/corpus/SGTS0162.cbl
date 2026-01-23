      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0162
      * ANALISTA..: F2054109 Cintia Ap Soares Iglesias
      * AUTOR.....: F0950413 António Francisco Mendonça Silva
      * DATA......: 12/05/2017
      * OBJETIVO..: Negócio - Validar Demonstrativo de Caixa.
      * COMPILACAO: COBOL/CICS (On-line)
      *-----------------------------------------------------------------
      * VRS001 31.01.2019 - F0950413 Inclusão da sigla 'SGT' na section
      *                     971000-LER-ERRO
      *        08.08.2018 - F8785286 - 2015/137233 - Somente recompila-
      *           ção por alteração no book SGTSL087.
      *        12.05.2017 - F0950413  DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0162.

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
               '*** SGTS0162 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS001'.

      *--> Subrotina para classificação de tabela interna
       77  SBSORT                      PIC X(08) VALUE 'SBSORT'.

      *--> Módulos a serem acionados via 'EXEC CICS LINK (COMMAREA)'
       77  SGTST005                    PIC X(08) VALUE 'SGTST005'.

      *--> Módulos a serem acionados via 'CONTAINER'
       77  SGTSC086                    PIC X(08) VALUE 'SGTSC086'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      *--> Book do container de erro
-INC IIBKERRO
      *--> Book para captura de erro via MSG1
-INC MSGKWMG1
      *--> Variaveis para tratamento de Abend e Condition
-INC HLPKW999
      *--> Variaveis para tratamento de CONTAINER
-INC SGTKERR1

      *--> Container de erro
       01  CTNR-PRM-ERRO               PIC X(16) VALUE 'PRM-ERRO'.

      *--> Areas de Controle de Programa Chamado (CONTAINER)
       01  CTL-PGM-CHMD.
           03  NM-PGM-CHMD             PIC  X(08)         VALUE ' '.
               88 NM-PGM-COM-RQSC      VALUE 'SGTSC086'.
               88 NM-PGM-COM-SRVC      VALUE '        '.
           03  FILLER                  PIC  X(01)         VALUE ' '.
           03  MSG-PGM-CHMD            PIC  X(111)        VALUE ' '.

      *--> Timestamp para teste de quebra
       01  AUX-TS-ANT                  PIC  X(26)     VALUE ' '.

      *--> Subscritores
       01  IC-TAB                      PIC S9(04)     COMP-5  VALUE 0.
       01  IC-TAB-TS                   PIC S9(04)     COMP-5  VALUE 0.

      *--> Areas auxiliares p/ descompactar dados COMP-3 p/ monitoração
       01  AUX-CD-OPR.
           03 AUX-CD-OPR-ETRD-MCP      PIC S9(11)             VALUE 0.
       01  AUX-VL-R.
           03 AUX-VL                   PIC S9(15)V99          VALUE 0.

      *--> Lista dos Pagamentos Vinculados sumarizados por TIMESTAMP
       01  VL-VINCULADOS-POR-TS.
           03  TAB-VL-RLZD-VCLD-POR-TS OCCURS 0 TO 1500 TIMES
                                   DEPENDING ON IC-TAB-TS.
              05  TAB-TS-SUMARIZADO    PIC  X(26).
              05  TAB-VL-SUMARIZADO    PIC S9(15)V9(2) COMP-3.

      *--> Acumuladores de valores
       01  ACR-VL-PREVISTO             PIC S9(015)V99  COMP-3 VALUE 0.

      *--> Lista dos Pagamentos Vinculados para classificação com SBSORT
       01  PAGAMENTOS-VINCULADOS.
           03  TAB-LISTA-VINCULOS      OCCURS 1500 TIMES.
              05  TAB-TS-REG-PGTO      PIC  X(26)             VALUE ' '.
              05  TAB-VL-RLZD-VCLD     PIC S9(15)V9(2) COMP-3 VALUE 0.

      *--> Area de Parametros da Rotina SBSORT
       01  TAB-PRVT-QT-REGISTRO        PIC S9(08) COMP VALUE ZEROS.
       01  TAB-PRVT-TAMANHO            PIC S9(08) COMP VALUE 0039.
       01  TAB-PRVT-CHAVE.
           03  TAB-PRVT-QT-CHAVES      PIC S9(08) COMP VALUE 0001.
      *--> Chave 01 - TS Recebimento Realizado
           03  TAB-PRVT-01-INICIO      PIC S9(08) COMP VALUE 0005.
           03  TAB-PRVT-01-TAMANHO     PIC S9(08) COMP VALUE 0026.
           03  TAB-PRVT-01-FORMATO     PIC S9(08) COMP VALUE +001.

      *-----------------------------------------------------------------
      *--> Book tabela OPR_ETRD_MCP
       01  SGTST005-DADOS.
-INC SGTKT005

      *--> Book tabela DMTV_CX_TIT
       01  SGTSC086-DADOS.
-INC SGTKC086
      *--> Book do programa
-INC SGTK0162
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

      * Habilita captura automatica de abend e erros CICS
-INC HLPKE999
           MOVE CTE-PROG(5:8)          TO NM-PGM

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 110000-EXCLUIR-CTNR-RPST
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO

           PERFORM 140000-VALIDAR-PARAMETROS
           PERFORM 150000-VALOR-INICIAL-VARIAVEIS
           PERFORM 160000-CLASSIFICAR-LS-VINCULOS

           PERFORM 180000-SUMARIZAR-VL-RLZD-VCLD
                   VARYING IC-TAB FROM 1 BY 1
                     UNTIL IC-TAB > SGTS0162-QT-OCR-TAB-PGTO

           PERFORM 200000-VALIDACAO-VL-PRVT-VCLD

           PERFORM 300000-POSTA-RESPOSTA
           PERFORM 999999-ENCERRAR
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      * Por causa da captura automática de erro CICS, deve ser previsto
      * o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
                CONTAINER ( 'SGTS0162-RPST' )
                CHANNEL   ( NM-CNL           )
                NOHANDLE
           END-EXEC
           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
               EIBRESP EQUAL DFHRESP(CONTAINERERR) )
               PERFORM 999002-TRATA-CONDITION-CICS
           END-IF
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
                CONTAINER ( 'PRM-ERRO' )
                CHANNEL   ( NM-CNL     )
                NOHANDLE
           END-EXEC
           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
               EIBRESP EQUAL DFHRESP(CONTAINERERR) )
               PERFORM 999002-TRATA-CONDITION-CICS
           END-IF
           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE 'SGTS0162-RQSC'        TO NM-CTNR

           PERFORM 972100-TRATA-AREA-RESPOSTA

           EXEC CICS GET
              CONTAINER ( 'SGTS0162-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( QT-CCT-LIDO             )
              INTO      ( SGTS0162-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETROS       SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           IF  SGTS0162-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0001               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de títulos de valores monetários
      -        ' inválido.'            TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0162-CD-OPR-ETRD-MCP     NOT NUMERIC
               SET ERRO-PROGRAMADO          TO TRUE
               MOVE 0004                    TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais inv
      -             'álido.'                TO MSG1-TX-ERRO-TS
               SET MNT-VRV                  TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
               IF  SGTS0162-CD-OPR-ETRD-MCP EQUAL ZEROS
                   SET  ERRO-PROGRAMADO          TO TRUE
                   MOVE 0005                     TO  MSG1-CD-ERRO-TS
                   MOVE 'Código da operação estruturada de mercado de ca
      -                 'pitais não informado.'  TO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           END-IF

           IF  SGTS0162-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0006               TO  MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0162-QT-OCR-TAB-PGTO   EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0534               TO  MSG1-CD-ERRO-TS
               MOVE 'Lista vínculos não informada.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validação da lista vínculos
           PERFORM  VARYING IC-TAB FROM 1 BY 1
                      UNTIL IC-TAB > SGTS0162-QT-OCR-TAB-PGTO
              IF  SGTS0162-TS-REG-PGTO      (IC-TAB)  EQUAL SPACES
                  SET  ERRO-PROGRAMADO TO TRUE
                  MOVE 0316            TO  MSG1-CD-ERRO-TS
                  MOVE 'Timestamp registro pagamento não informado.'
                                       TO MSG1-TX-ERRO-TS
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-IF
              IF  SGTS0162-VL-RLZD-VCLD     (IC-TAB)  EQUAL ZEROS
                  SET  ERRO-PROGRAMADO TO TRUE
                  MOVE 0477            TO  MSG1-CD-ERRO-TS
                  MOVE 'Valor realizado vinculado não informado.'
                                       TO MSG1-TX-ERRO-TS
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-IF
           END-PERFORM
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALOR-INICIAL-VARIAVEIS  SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

      *--> Inicializar áreas de dados dos pgms a serem acionados:
           INITIALIZE SGTSC086-DADOS
                      SGTST005-DADOS

      *--> Inicializar a açao dos pgms a serem acionados:
           SET SGTST005-CONSULTAR                TO  TRUE

      *--> Informar as chaves de acesso, dos pgms a serem acionados:
           MOVE SGTS0162-CD-CTRA-TIT-VL
                                       TO SGTST005-CD-CTRA-TIT-VL
           MOVE SGTS0162-CD-OPR-ETRD-MCP
                                       TO SGTST005-CD-OPR-ETRD-MCP
           MOVE SGTS0162-NR-VRS-OPR-ETRD
                                       TO SGTST005-NR-VRS-OPR-ETRD

      *--> Obter Código Titular Operação Estruturada, 'CD-TIT-OPR-ETRD'
      *--> Obter Numero Divisão Estruturada         , 'NR-DVS-TIT'
      *--> Obter Valor eventual operação            , 'VL-EVT-OPR'
           PERFORM 950000-CHAMAR-SGTST005
           IF  SGTST005-CD-RTN-PGM  EQUAL +100
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0353               TO  MSG1-CD-ERRO-TS
               MOVE 'Título não vinculado a operação estruturada.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE SGTST005-CD-TIT-OPR-ETRD
                                       TO SGTSC086-CD-TIT-OPR-ETRD
           MOVE SGTST005-NR-DVS-TIT    TO SGTSC086-NR-DVS-TIT
           .
       150000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       160000-CLASSIFICAR-LS-VINCULOS  SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           PERFORM VARYING IC-TAB FROM 1 BY 1
                     UNTIL IC-TAB > SGTS0162-QT-OCR-TAB-PGTO
                   MOVE SGTS0162-TS-REG-PGTO       (IC-TAB)     TO
                        TAB-TS-REG-PGTO      (IC-TAB)
                   MOVE SGTS0162-VL-RLZD-VCLD      (IC-TAB)     TO
                        TAB-VL-RLZD-VCLD     (IC-TAB)
           END-PERFORM

           MOVE SGTS0162-QT-OCR-TAB-PGTO    TO TAB-PRVT-QT-REGISTRO
           CALL  SBSORT             USING  PAGAMENTOS-VINCULADOS
                                           TAB-PRVT-QT-REGISTRO
                                           TAB-PRVT-TAMANHO
                                           TAB-PRVT-CHAVE

           IF  RETURN-CODE                 NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0535              TO  MSG1-CD-ERRO-TS
               MOVE  'Erro na execução da ordenação da lista de entrada.
      -              ''                TO  MSG1-TX-ERRO-TS
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       160000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       180000-SUMARIZAR-VL-RLZD-VCLD   SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           IF  TAB-TS-REG-PGTO (IC-TAB) EQUAL AUX-TS-ANT
               ADD TAB-VL-RLZD-VCLD (IC-TAB)
                                       TO TAB-VL-SUMARIZADO (IC-TAB-TS)
           ELSE
              ADD 1                    TO IC-TAB-TS
              MOVE TAB-TS-REG-PGTO (IC-TAB) TO AUX-TS-ANT
                                          TAB-TS-SUMARIZADO (IC-TAB-TS)
              MOVE TAB-VL-RLZD-VCLD (IC-TAB)
                                       TO TAB-VL-SUMARIZADO (IC-TAB-TS)
           END-IF
           .
       180000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-VALIDACAO-VL-PRVT-VCLD   SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

      *--> Validação do valor pagamento vínculado sumarizado por "TS"
           MOVE SGTSC086               TO NM-PGM-CHMD
           PERFORM 210000-VALIDAR-PAGTOS-ACUM-TS
                   VARYING IC-TAB FROM 1 BY 1
                     UNTIL IC-TAB > IC-TAB-TS
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-VALIDAR-PAGTOS-ACUM-TS   SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           MOVE TAB-TS-SUMARIZADO (IC-TAB)
                                       TO SGTSC086-TS-REG-PGTO
           PERFORM 900000-PROCEDIMENTOS-CONTAINER

           IF  SGTSC086-VL-EFT-MVTC NOT EQUAL TAB-VL-SUMARIZADO (IC-TAB)
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0536               TO  MSG1-CD-ERRO-TS
               MOVE 'A soma dos valores vinculados não atinge o valor to
      -             'tal efetivo da movimentação.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           MOVE 'Validação do demonstrativo de caixa realizada com suces
      -         'so.'                  TO SGTS0162-TX-RTN-PRCT

           EXEC CICS PUT
                CONTAINER ( 'SGTS0162-RPST'         )
                CHANNEL   ( NM-CNL                   )
                FLENGTH   ( LENGTH OF SGTS0162-RPST )
                FROM      ( SGTS0162-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       900000-PROCEDIMENTOS-CONTAINER  SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

      *--> Grava container de requisição da operação a ser acionada
           IF  NM-PGM-COM-RQSC
               PERFORM 982000-GRAVA-REQUISICAO
           END-IF

      *--> Executa a operação acionada via IIB
           PERFORM 910000-EXECUTA-MODULO

      *--> Lê o container de erro da operação acionada
           PERFORM 971000-LER-ERRO

      *--> Lê o container de resposta da operação acionada
           PERFORM 972000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       910000-EXECUTA-MODULO           SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      * Executa o módulo chamado
      * Em caso de erro, a captura automática do CICS trata o erro
      * ---------------------------------------
           EVALUATE NM-PGM-CHMD
      * ---------------------------------------
               WHEN SGTSC086
                 EXEC CICS LINK
                           PROGRAM ( SGTSC086  )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC
      * ---------------------------------------
               WHEN OTHER
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 9005             TO MSG1-CD-ERRO-TS
                 STRING 'Não definida a execução do módulo '
                         NM-PGM-CHMD '.' DELIMITED BY SIZE
                                       INTO MSG1-TX-ERRO-TS
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    STRING
                    'Erro na execução do módulo ' NM-PGM-CHMD ', '
                    'RESP  ='   ' ' MSG1-RESP1  ', '
                    'RESP2 =' ' ' MSG1-RESP2
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    MOVE 3004          TO MSG1-CD-ERRO-TS
                    SET ERRO-CICS      TO TRUE
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .
       910000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       950000-CHAMAR-SGTST005          SECTION.
      *----------------------------------------------------------------*
           MOVE 0014                   TO  MSG1-CD-SEQL-ERRO

           EXEC CICS LINK
                        PROGRAM ( SGTST005       )
                        COMMAREA( SGTST005-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               MOVE 'Erro na chamada ao módulo SGTST005.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF SGTST005-CD-RTN-PGM NOT EQUAL ZEROS AND NOT EQUAL +100
              SET ERRO-PROGRAMADO      TO TRUE
              MOVE 0018                TO MSG1-CD-ERRO-TS
              MOVE SGTST005-RTN-PGM    TO MSG1-TX-ERRO-TS
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       950000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       971000-LER-ERRO                 SECTION.
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO
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
                   SET ERRO-CONTAINER  TO TRUE
                   PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-RESPOSTA             SECTION.
      *-----------------------------------------------------------------
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

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
      *-----------------------------------------------------------------
      *--> Lê container de resposta do SGTSC086
               WHEN SGTSC086
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSC086-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC
      *-----------------------------------------------------------------
               WHEN OTHER
                 SET SEM-ERRO          TO TRUE
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 0537             TO MSG1-CD-ERRO-TS
                 STRING
                    'Leitura do container de resposta não prevista para
      -             'o módulo ' NM-PGM-CHMD '.'
                 DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
      *-----------------------------------------------------------------

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972100-TRATA-AREA-RESPOSTA      SECTION.
      *-----------------------------------------------------------------
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Obtém a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       972100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       982000-GRAVA-REQUISICAO         SECTION.
      *-----------------------------------------------------------------
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para gravação do container de requisição
           SET  PUT-CONTAINER          TO TRUE

           MOVE NM-PGM-CHMD            TO NM-CTNR(1:8)
           MOVE '-RQSC'                TO NM-CTNR(9:5)

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Grava container da requisição do serviço IIB a ser acionado
      *--> Em caso de erro, a captura automática do CICS trata o erro
           EVALUATE NM-PGM-CHMD
      *-----------------------------------------------------------------
      *--> Grava container de requisição do programa SGTSL087
               WHEN SGTSC086
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR       )
                            FROM      ( SGTSC086-RQSC )
                            FLENGTH   ( LENGTH OF
                                        SGTSC086-RQSC )
                  END-EXEC
      *-----------------------------------------------------------------
               WHEN OTHER
                 SET SEM-ERRO          TO TRUE
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 0538             TO MSG1-CD-ERRO-TS
                 STRING
                    'Gravação do container de requisição não definida pa
      -             'ra o módulo ' NM-PGM-CHMD '.'
                 DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
      *-----------------------------------------------------------------

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       982000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO

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

           MOVE 'SGTS0162-CD-CTRA-TIT-VL'        TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0162-CD-CTRA-TIT-VL         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0162-CD-OPR-ETRD-MCP'       TO MSGKW999-NM-VRV-MNT
           IF  SGTS0162-CD-OPR-ETRD-MCP          IS  NUMERIC
               MOVE SGTS0162-CD-OPR-ETRD-MCP     TO AUX-CD-OPR-ETRD-MCP
               MOVE  AUX-CD-OPR                  TO MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Código não numérico.'       TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0162-NR-VRS-OPR-ETRD'       TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0162-NR-VRS-OPR-ETRD        TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0162-QT-OCR-TAB-PGTO'       TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0162-QT-OCR-TAB-PGTO        TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           IF IC-TAB < SGTS0162-QT-OCR-TAB-PGTO

                   MOVE 'SGTS0162-TS-REG-PGTO'   TO MSGKW999-NM-VRV-MNT
                   MOVE  SGTS0162-TS-REG-PGTO    (IC-TAB)
                                                 TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT

                   MOVE 'SGTS0162-VL-RLZD-VCLD'  TO MSGKW999-NM-VRV-MNT
                   MOVE  SGTS0162-VL-RLZD-VCLD   (IC-TAB)
                                                 TO AUX-VL
                   MOVE  AUX-VL-R                TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT
           END-IF

      * --> grava o erro no msg
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
-INC HLPKP999
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999
      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
      * -INC HLPKP997
      * --------------------------------------*

      * MSG1-NTF-MSG                   SECTION.
-INC MSGKPMG1
      * --------------------------------------*
       999999-ENCERRAR                 SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC.
      *------------------------- Fim SGTS0162 -------------------------*