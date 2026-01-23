      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0228
      * ANALISTA..: F8785286 - Roseli Akemi Kawahata
      * AUTOR.....: F0950413 - António Francisco Mendonça Silva
      * DATA......: 09/10/2018
      * OBJETIVO..: Negócio - Listar Justificativa de Validação da Ope -
      *             ração Estruturada de Mercado de Capitais.
      * COMPILACAO: 52 - PSOSE600 - Cobol 4.2 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS002 04.12.2019 - F0950413 - 2016/145195 - Monitorar variavel
      *                     DCR-CMPT-TX da mensagem 0852.
      * VRS001 09.10.2018 - F0950413 - 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0228.

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
               '*** SGTS0228 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS002'.

      *--> Tabela Descrição das variáveis de complemento das inconsis-
      *--> tências dos indicadores dos códigos dos parâmetros informados
       01 TAB-TX-MSG.
          03  PIC X(38) VALUE 'AVS-LQDC  aviso de liquidação  '.
          03  PIC X(38) VALUE 'CTR-GRT   contrato de garantia '.
          03  PIC X(38) VALUE 'DCL-CFMD  declaração de conformidade '.
          03  PIC X(38) VALUE 'ITMT-FRMZ instrumento de formalização'.
          03  PIC X(38) VALUE 'LIM-CRD   limite de crédito '.
          03  PIC X(38) VALUE 'PREF-VLDC precificação de validação '.
          03  PIC X(38) VALUE 'SUMULA-AG sumula de agência '.
          03  PIC X(38) VALUE 'PRM-TARF  tarifa '.
          03  PIC X(38) VALUE 'OPR-GRT   operação de garantia '.
          03  PIC X(38) VALUE 'NOTA-TCN  nota técnica '.
          03  PIC X(38) VALUE 'AGT-FDAR  agente fiduciário '.
          03  PIC X(38) VALUE 'LIM-FNCO  limite financeiro '.
          03  PIC X(38) VALUE 'PRM-EXCO  exceção '.
       01 REDEFINES TAB-TX-MSG.
          03 TAB-TX-MSG-DIM-1          OCCURS 13 TIMES.
             05 TX-CMP                 PIC X(10).
             05 TX-MSG-ERRO            PIC X(28).

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
       01  IC-TAB                      PIC S9(04)     COMP-5  VALUE   0.
       01  IC-TAB-PRM                  PIC S9(04)     COMP-5  VALUE   0.
       01  DCR-CMPT-TX                 PIC  X(28)             VALUE ' '.
       01  AUX-CD-TIP-VLDC-OPR         PIC S9(04)     COMP-5  VALUE   0.

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

       01  WS-ERRO-CICS.
           03  WS-RESP1                PIC  9(008).
           03  WS-RESP2                PIC  9(008).

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      ---------------9.

      *--> Variaveis utilizados para leitura
       01  REG-WS.
           03 CD-TIP-VLDC-OPR          PIC S9(04)     COMP-5 VALUE 0.
           03 CD-USU-ULT-ATL           PIC  X(08)     VALUE ' '.
           03 TX-JST-VLDC-OPR          PIC  X(200)    VALUE ' '.
           03 TS-ULT-ATL               PIC  X(26)     VALUE ' '.
           03 TX-TIP-VLDC-OPR          PIC  X(50)     VALUE ' '.

      *--> Tabela para consistência códigos parâmetro x indicador
       01  DADOS-PRM-IN.
           03 TAB-PRM-IN               PIC  X(39)         VALUE ' '.
           03 REDEFINES TAB-PRM-IN.
              05 LS-PRM-IN             OCCURS 13 TIMES.
                 07 CTU-PRM            PIC  S9(04)    COMP-5.
                 07 CTU-IN             PIC   X(01).

      *-----------------------------------------------------------------

      *--> Book do programa
       01  SGTS0228-DADOS.
-INC SGTK0228

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

           PERFORM 200000-LISTAR-JUSTIFICATIVA
                   VARYING IC-TAB-PRM FROM 1 BY 1
                     UNTIL IC-TAB-PRM > 13

           PERFORM 980000-GRAVAR-CTNR-RESPOSTA
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

           IF  SGTS0228-CD-CTRA-TIT-VL  NOT > ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de títulos e valores mobiliários
      -             ' inválido.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0228-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
            IF SGTS0228-CD-OPR-ETRD-MCP = 0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0005               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' informado.'      TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
            END-IF
           END-IF

           IF  SGTS0228-NR-VRS-OPR-ETRD NOT > ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0006               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Consistência dos conteudos informados dos parâmetros e
      *--> indicadores
           MOVE SGTS0228-RQSC(13:39)   TO  TAB-PRM-IN

           PERFORM VARYING IC-TAB FROM 1 BY 1
                     UNTIL IC-TAB > 13
              IF  CTU-PRM (IC-TAB)     >  0
               IF CTU-IN  (IC-TAB)     NOT EQUAL 'S' AND 'N'
                  MOVE TX-MSG-ERRO (IC-TAB)    TO DCR-CMPT-TX
                  PERFORM 141000-MENSAGEN-ERRO-INDICADOR
               END-IF
              ELSE
               IF CTU-IN  (IC-TAB)     NOT EQUAL ' '
                  MOVE TX-MSG-ERRO (IC-TAB)    TO DCR-CMPT-TX
                  PERFORM 141000-MENSAGEN-ERRO-INDICADOR
               END-IF
              END-IF
           END-PERFORM
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       141000-MENSAGEN-ERRO-INDICADOR  SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           SET  ERRO-PROGRAMADO    TO TRUE
           MOVE 0852               TO MSG1-CD-ERRO-TS

           STRING 'Indicador do parâmetro de '  DCR-CMPT-TX
                  ' inválido.'
                  DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
           END-STRING

           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       141000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-LISTAR-JUSTIFICATIVA     SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           IF CTU-IN (IC-TAB-PRM) = 'N'
              PERFORM 971000-RECUPERAR-JUSTIFICATIVA
              IF SQLCODE               EQUAL  0
                 ADD 1                 TO SGTS0228-QT-OCR-TAB
                 MOVE CD-TIP-VLDC-OPR  TO
                      SGTS0228-CD-TIP-VLDC-JST (SGTS0228-QT-OCR-TAB)
                 MOVE CD-USU-ULT-ATL   TO
                      SGTS0228-CD-USU-ULT-ATL  (SGTS0228-QT-OCR-TAB)
                 MOVE TX-JST-VLDC-OPR  TO
                      SGTS0228-TX-JST          (SGTS0228-QT-OCR-TAB)
                 MOVE TS-ULT-ATL       TO
                      SGTS0228-TS-ULT-ATL      (SGTS0228-QT-OCR-TAB)
                 MOVE TX-TIP-VLDC-OPR  TO
                      SGTS0228-TX-TIP-JST      (SGTS0228-QT-OCR-TAB)
              END-IF
           END-IF
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970000-LER-REQUISICAO-RECEBIDA  SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisição
           MOVE 'SGTS0228'             TO NM-PGM
           MOVE 'SGTS0228-RQSC'        TO NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM 970100-LER-CONTAINER-NODATA

      *--> Obtem o tamanho esperado da requisição
           MOVE LENGTH OF SGTS0228-RQSC TO QT-CCT-EPRO

      *--> Valida tamanho esperado da requisição X tamanho recebido

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTS0228-RQSC' )
              CHANNEL   ( NM-CNL          )
              FLENGTH   ( QT-CCT-LIDO     )
              INTO      ( SGTS0228-RQSC   )
           END-EXEC
           .
       970000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       970100-LER-CONTAINER-NODATA     SECTION.
      *----------------------------------------------------------------*
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

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

      *-----------------------------------------------------------------
       971000-RECUPERAR-JUSTIFICATIVA  SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS
           MOVE CTU-PRM (IC-TAB-PRM)   TO AUX-CD-TIP-VLDC-OPR

           EXEC SQL
                SELECT A.CD_TIP_VLDC_OPR
                     , A.CD_USU_ULT_ATL
                     , A.TX_JST_VLDC_OPR
                     , A.TS_ULT_ATL
                     , B.TX_TIP_VLDC_OPR
                  INTO :CD-TIP-VLDC-OPR
                     , :CD-USU-ULT-ATL
                     , :TX-JST-VLDC-OPR
                     , :TS-ULT-ATL
                     , :TX-TIP-VLDC-OPR
                FROM  DB2SGT.JST_VLDC_OPR_ETRD      A
                INNER JOIN DB2SGT.TIP_JST_VLDC_OPR  B
                   ON   B.CD_TIP_VLDC_OPR  =  A.CD_TIP_VLDC_OPR
                WHERE A.CD_CTRA_TIT_VL  = :SGTS0228-CD-CTRA-TIT-VL
                  AND A.CD_OPR_ETRD_MCP = :SGTS0228-CD-OPR-ETRD-MCP
                  AND A.NR_VRS_OPR_ETRD = :SGTS0228-NR-VRS-OPR-ETRD
                  AND A.CD_TIP_VLDC_OPR = :AUX-CD-TIP-VLDC-OPR
           END-EXEC
           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980000-GRAVAR-CTNR-RESPOSTA     SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

      *--> Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

      *--> Monta o nome do container de resposta
           MOVE 'SGTS0228-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                CONTAINER ( NM-CTNR                 )
                CHANNEL   ( NM-CNL                  )
                FLENGTH   ( LENGTH OF SGTS0228-RPST )
                FROM      ( SGTS0228-RPST           )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO
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
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

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
      *--> Monitorar variàveis do pgm, grava e encerra processamento

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
           MOVE 'SGTS0228-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0228-CD-CTRA-TIT-VL     TO MCR-S9-15
           MOVE  MCR-S9-15                   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTS0228-CD-OPR-ETRD-MCP'  TO MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF SGTS0228-CD-OPR-ETRD-MCP  NUMERIC
              MOVE SGTS0228-CD-OPR-ETRD-MCP  TO MCR-S9-15
              MOVE  MCR-S9-15                TO MSGKW999-VL-VRV-MNT
           ELSE
              MOVE  'Valor não numérico'     TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0228-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0228-NR-VRS-OPR-ETRD    TO MCR-S9-15
           MOVE  MCR-S9-15                   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *--> Movimentação das informaçoes recebidas referentes a
      *--> a parâmetros e indicadores
           PERFORM VARYING IC-TAB FROM 1 BY 1
                     UNTIL IC-TAB > 13
      *--> Movimentação campo parâmetro
              STRING 'SGTS0228-CD-PRM-' TX-CMP (IC-TAB)
                      DELIMITED BY SIZE
                      INTO         MSGKW999-NM-VRV-MNT
              END-STRING
              MOVE  CTU-PRM (IC-TAB)   TO MCR-S9-15
              MOVE  MCR-S9-15          TO MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT
      *--> Movimentação campo indicador
              STRING 'SGTS0228-IN-PRM-' TX-CMP (IC-TAB)
                      DELIMITED BY SIZE
                      INTO         MSGKW999-NM-VRV-MNT
              END-STRING
              MOVE  CTU-IN  (IC-TAB)   TO MSGKW999-VL-VRV-MNT
              PERFORM 999003-ADICIONA-VRV-MNT
           END-PERFORM

           MOVE 'DCR-CMPT-TX'                TO MSGKW999-NM-VRV-MNT
           MOVE  DCR-CMPT-TX                 TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

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
       999000-SAI.
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
      *---------------------- FIM SGTS0228 ----------------------------