      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0111
      * ANALISTA..: F9507141 Vicente de Paulo Tavares de Souza
      * AUTOR.....: F0950413 António Francisco Mendonça Silva
      * DATA......: 18/01/2017
      * OBJETIVO..: Negócio - Listar taxas de quebra de fundo
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS003 09.02.2023 - F9188847 - Inclusão do campo: CD-FMA-APRC
      * VRS002 11.07.2019 - F8090555 - Monitora variáveis
      * VRS001 18.01.2017 - F0950413 - 2015/137233 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0111.

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
               '*** SGTS0111 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS003'.
       77  SBDATAF                     PIC X(08) VALUE 'SBDATAF'.
       77  SGTSL079                    PIC X(08) VALUE 'SGTSL079'.
       77  SGTST005                    PIC X(08) VALUE 'SGTST005'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informações
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

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *--> Container de erro
       01  CTNR-PRM-ERRO               PIC X(16) VALUE 'PRM-ERRO'.

      *--> Areas de Controle de Programa Chamado (CONTAINER)
       01  CTL-PGM-CHMD.
           03  NM-PGM-CHMD             PIC  X(08)         VALUE ' '.
               88 NM-PGM-COM-RQSC      VALUE 'SGTSL079'.
               88 NM-PGM-COM-SRVC      VALUE '        '.
           03  FILLER                  PIC  X(01)         VALUE ' '.
           03  MSG-PGM-CHMD            PIC  X(111)        VALUE ' '.

      *--> Areas do SBDATAF
       01  AREA-SBDATAF.
          03  FUNCAO                   PIC  X(003) VALUE 'F01'.
          03  ARG01-F01                PIC  9(008).
          03  ARG02-F01                PIC  9(001) VALUE 0.
          03  ARG01-F03                PIC  9(008).
          03  ARG02-F03                PIC  9(008).
          03  ARG03-F03                PIC  9(005).
          03  ARG01-F13                PIC  9(008).
          03  ARG02-F13                PIC  9(008).
          03  ARG03-F13                PIC S9(005) VALUE -1.
       01  WORKAREA                    PIC X(4095).
       01  AREA-RED REDEFINES WORKAREA.
           05  AREA-TAM                PIC 9(0008) COMP.
           05  FILLER                  PIC X(4071).
           05  AREA-FIM                PIC X(0020).

       01  AUX-CD-RETORNO              PIC -9(004) VALUE ZEROS.

       01  WS-ERRO-CICS.
           03  WS-RESP1                   PIC  9(008).
           03  WS-RESP2                   PIC  9(008).

       01  SGTS0111-IC-TAB       PIC S9(04) COMP-5 VALUE 0.

      *-----------------------------------------------------------------
-INC SGTKL079

      *--> Book tabela OPR_ETRD_MCP
       01 SGTST005-DADOS.
-INC SGTKT005

      *--> Book do programa
-INC SGTK0111
      *-----------------------------------------------------------------
       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------

      * Habilita captura automatica de abend e erros CICS
-INC HLPKE999
           MOVE CTE-PROG(5:8)          TO NM-PGM

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 110000-EXCLUIR-CTNR-RPST
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDAR-PARAMETRO

           PERFORM 200000-PROCESSAR-SOLICITACAO

           PERFORM 300000-POSTA-RESPOSTA
           PERFORM 999999-ENCERRAR
           .

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS ASSIGN
                CHANNEL ( NM-CNL )
           END-EXEC
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
                CONTAINER ( 'SGTS0111-RPST' )
                CHANNEL   ( NM-CNL          )
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

           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTS0111-RQSC     TO QT-CCT-EPRO

           EXEC CICS GET
                CONTAINER ( 'SGTS0111-RQSC' )
                CHANNEL   ( NM-CNL          )
                NODATA
                FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA         TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTS0111-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTS0111-RQSC )
              INTO      ( SGTS0111-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO SGTS0111-IN-CNTO-PSQ

           IF  SGTS0111-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0002               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -        'não informado.'        TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0111-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada de mercado de capita
      -        'is não numérico.'      TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0111-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0005               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada de mercado de capita
      -        'is não informado.'     TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0111-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0007               TO  MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturado não inform
      -             'ado.'             TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0111-CD-TIP-CND-NEGD EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0210               TO  MSG1-CD-ERRO-TS
               MOVE 'Código do tipo de condição negociada não informado.
      -        ' '                     TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0111-DT-INC-TAXA-QBRA NOT EQUAL SPACES AND ZEROS
               PERFORM 141000-VALIDAR-DATA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       141000-VALIDAR-DATA             SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           IF DIA OF SGTS0111-DT-INC-TAXA-QBRA-R NOT NUMERIC OR
              MES OF SGTS0111-DT-INC-TAXA-QBRA-R NOT NUMERIC OR
              ANO OF SGTS0111-DT-INC-TAXA-QBRA-R NOT NUMERIC
              SET  ERRO-PROGRAMADO    TO TRUE
              MOVE 0495               TO MSG1-CD-ERRO-TS
              MOVE  'Data início da taxa de quebra de fundo não numérica
      -             '.'               TO MSG1-TX-ERRO-TS
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE SGTS0111-DT-INC-TAXA-QBRA   TO ARG01-F01
      *  Validação no formato Gregoriano
           MOVE 4                           TO ARG02-F01
           MOVE 'F01'                       TO FUNCAO
           MOVE 4095                        TO AREA-TAM
           MOVE 'SBDATAF -WRKAREA-FIM'      TO AREA-FIM

           CALL SBDATAF  USING  FUNCAO
                                WORKAREA
                                ARG01-F01
                                ARG02-F01

           IF  ARG01-F01     EQUAL 99999999
               SET  ERRO-PROGRAMA-ACIONADO    TO TRUE
               MOVE 0494               TO  MSG1-CD-ERRO-TS
               MOVE 'Data início da taxa da quebra de fundo inválida.'
                                       TO MSG1-TX-ERRO-TS
                                          MSG1-TX-RTN-PGM-ACND
               MOVE SBDATAF                TO MSG1-NM-PGM-ACND
               MOVE RETURN-CODE            TO MSG1-CD-RTN-PGM-ACND

               MOVE 'FUNCAO'               TO  MSGKW999-NM-VRV-MNT
               MOVE  FUNCAO                TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG01-F01'             TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG01-F01              TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG02-F01'                TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG02-F01                 TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
            IF  ARG01-F01     EQUAL 88888888
                SET  ERRO-PROGRAMA-ACIONADO    TO TRUE
                MOVE 0051                      TO MSG1-CD-ERRO-TS
                MOVE SBDATAF                   TO MSG1-NM-PGM-ACND
                MOVE RETURN-CODE               TO MSG1-CD-RTN-PGM-ACND
                MOVE 'Erro no tratamento de datas.'
                                               TO MSG1-TX-ERRO-TS
                                                  MSG1-TX-RTN-PGM-ACND

               MOVE 'FUNCAO'               TO  MSGKW999-NM-VRV-MNT
               MOVE  FUNCAO                TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG01-F01'             TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG01-F01              TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG02-F01'                TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG02-F01                 TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
            END-IF
           END-IF
           .
       141000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR-SOLICITACAO    SECTION.
      *-----------------------------------------------------------------

           PERFORM 210000-OBTER-QT-MESES-CARENCIA

           PERFORM 220000-OBTER-LISTA-SOLICITADA

           IF  SGTSL079-QT-OCR-TAB > 0
               MOVE SGTSL079-QT-OCR-TAB    TO SGTS0111-QT-OCR-TAB
               MOVE SGTSL079-IN-CNTO-PSQ   TO SGTS0111-IN-CNTO-PSQ

               PERFORM 230000-FORMATA-PESQUISA-SAIDA
                       VARYING SGTS0111-IC-TAB FROM 1 BY 1
                         UNTIL SGTS0111-IC-TAB > SGTS0111-QT-OCR-TAB
           END-IF
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-OBTER-QT-MESES-CARENCIA  SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE SGTST005-DADOS
           SET SGTST005-CONSULTAR          TO  TRUE
           MOVE  SGTS0111-CD-CTRA-TIT-VL   TO  SGTST005-CD-CTRA-TIT-VL
           MOVE  SGTS0111-CD-OPR-ETRD-MCP  TO  SGTST005-CD-OPR-ETRD-MCP
           MOVE  SGTS0111-NR-VRS-OPR-ETRD  TO  SGTST005-NR-VRS-OPR-ETRD

           PERFORM 970000-CHAMAR-SGTST005

           IF  SGTST005-CD-RTN-PGM  EQUAL +100
               SET  ERRO-PROGRAMA-ACIONADO     TO TRUE
               MOVE 353                        TO MSG1-CD-ERRO-TS
               MOVE 'Título não vinculado a operação estruturada.'
                                               TO MSG1-TX-ERRO-TS
               MOVE SGTST005-CD-RTN-PGM        TO MSG1-CD-RTN-PGM-ACND
               MOVE SGTST005-MSG-ERRO          TO MSG1-TX-RTN-PGM-ACND

               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Recuperar quantidade de meses de carência
           EVALUATE SGTS0111-CD-TIP-CND-NEGD
               WHEN 1
                   MOVE SGTST005-QT-MM-CARE-RSGT
                                       TO SGTS0111-QT-MM-CARE
               WHEN 2
                   MOVE SGTST005-QT-MM-CARE-AMTZ
                                       TO SGTS0111-QT-MM-CARE
               WHEN 3
                   MOVE SGTST005-QT-MM-CARE-AQS
                                       TO SGTS0111-QT-MM-CARE
               WHEN OTHER
                   SET ERRO-PROGRAMADO TO TRUE
                   MOVE 0493           TO  MSG1-CD-ERRO-TS
                   MOVE 'Código do tipo de condição negociada, não previ
      -                 'sto.'         TO  MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Inicializar informações referente à lista a ser recuperada
           MOVE 0                      TO SGTS0111-QT-OCR-TAB
           MOVE ' '                    TO SGTS0111-IN-CNTO-PSQ
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-OBTER-LISTA-SOLICITADA   SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSL079                    TO NM-PGM-CHMD
           MOVE 0                           TO SGTSL079-QT-OCR-TAB
           MOVE ' '                         TO SGTSL079-IN-CNTO-PSQ

           MOVE SGTS0111-CD-CTRA-TIT-VL     TO SGTSL079-CD-CTRA-TIT-VL
           MOVE SGTS0111-CD-OPR-ETRD-MCP    TO SGTSL079-CD-OPR-ETRD-MCP
           MOVE SGTS0111-NR-VRS-OPR-ETRD    TO SGTSL079-NR-VRS-OPR-ETRD
           MOVE SGTS0111-CD-TIP-CND-NEGD    TO SGTSL079-CD-TIP-CND-NEGD
           MOVE SGTS0111-DT-INC-TAXA-QBRA   TO SGTSL079-DT-INC-VLD-CND

           PERFORM 900000-ACIONA-PROGRAMA
           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-FORMATA-PESQUISA-SAIDA   SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           MOVE CORR SGTSL079-DT-INC-VLD-CND-S-R  (SGTS0111-IC-TAB)  TO
                SGTS0111-DT-INC-TAXA-QBRA-S-R  (SGTS0111-IC-TAB)
           MOVE SGTSL079-CD-USU-ULT-ATL-S    (SGTS0111-IC-TAB)  TO
                SGTS0111-CD-USU-ULT-ATL-S    (SGTS0111-IC-TAB)
           MOVE SGTSL079-TS-ULT-ATL-S        (SGTS0111-IC-TAB)  TO
                SGTS0111-TS-ULT-ATL-S        (SGTS0111-IC-TAB)
           MOVE SGTSL079-CD-TIP-CBT-S        (SGTS0111-IC-TAB)  TO
                SGTS0111-CD-TIP-CBT-S        (SGTS0111-IC-TAB)
           MOVE CORR SGTSL079-DT-FIM-VLD-CND-S-R  (SGTS0111-IC-TAB)  TO
                SGTS0111-DT-FIM-TAXA-QBRA-S-R  (SGTS0111-IC-TAB)
           MOVE SGTSL079-IN-OBGD-CND-S       (SGTS0111-IC-TAB)  TO
                SGTS0111-IN-OBGD-CND-S       (SGTS0111-IC-TAB)
           MOVE SGTSL079-PC-TAXA-QBRA-FNDO-S (SGTS0111-IC-TAB)  TO
                SGTS0111-PC-TAXA-QBRA-FNDO-S (SGTS0111-IC-TAB)
           MOVE SGTSL079-CD-FMA-APRC-S       (SGTS0111-IC-TAB)  TO
                SGTS0111-CD-FMA-APRC-S       (SGTS0111-IC-TAB)
           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
                CONTAINER ( 'SGTS0111-RPST'         )
                CHANNEL   ( NM-CNL                  )
                FLENGTH   ( LENGTH OF SGTS0111-RPST )
                FROM      ( SGTS0111-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       900000-ACIONA-PROGRAMA      SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

      * Executa o módulo chamado
      * Em caso de erro, a captura automática do CICS trata o erro
      * ---------------------------------------
           EVALUATE NM-PGM-CHMD
      * ---------------------------------------
               WHEN SGTSL079
                 EXEC CICS LINK
                           PROGRAM ( SGTSL079  )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC
      * ---------------------------------------
               WHEN OTHER
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 9006             TO MSG1-CD-ERRO-TS
                 MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    MOVE 3004          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro ao acionar o módulo.'
                                       TO MSG1-TX-ERRO-TS
                    SET ERRO-CICS      TO TRUE
      *        Monitorar variáveis do módulo chamado                     VRS001
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .
       910000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*

           EVALUATE NM-PGM
             WHEN SGTST005

               MOVE 'SGTST005-ACAO'              TO MSGKW999-NM-VRV-MNT
               MOVE SGTST005-ACAO                TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTST005-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
               MOVE SGTST005-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTST005-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
               IF  SGTST005-CD-OPR-ETRD-MCP    IS NUMERIC
                   MOVE  SGTST005-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD-MCP
                   MOVE  CD-OPR-ETRD-MCP-TX     TO  MSGKW999-VL-VRV-MNT
               ELSE
                   MOVE  'Valor não numérico'   TO  MSGKW999-VL-VRV-MNT
               END-IF
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTST005-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT   VRS001
               MOVE SGTST005-NR-VRS-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

             WHEN SGTSL079

               MOVE 'SGTSL079-CD-CTRA-TIT-VL'   TO MSGKW999-NM-VRV-MNT
               MOVE SGTSL079-CD-CTRA-TIT-VL     TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSL079-CD-OPR-ETRD-MCP'  TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
               IF  SGTSL079-CD-OPR-ETRD-MCP    IS NUMERIC
                   MOVE  SGTSL079-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD-MCP
                   MOVE  CD-OPR-ETRD-MCP-TX     TO  MSGKW999-VL-VRV-MNT
               ELSE
                   MOVE  'Valor não numérico'   TO  MSGKW999-VL-VRV-MNT
               END-IF
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSL079-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
               MOVE SGTSL079-NR-VRS-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSL079-CD-TIP-CND-NEGD'  TO MSGKW999-NM-VRV-MNT
               MOVE SGTSL079-CD-TIP-CND-NEGD    TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSL079-CD-ULT-TIP-CND-NEGD'
                                                 TO MSGKW999-NM-VRV-MNT
               MOVE SGTSL079-CD-ULT-TIP-CND-NEGD TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSL079-DT-INC-VLD-CND'   TO MSGKW999-NM-VRV-MNT
               MOVE SGTSL079-DT-INC-VLD-CND     TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

           WHEN OTHER
                CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       970000-CHAMAR-SGTST005          SECTION.
      *----------------------------------------------------------------*
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS LINK
                        PROGRAM ( SGTST005       )
                        COMMAREA( SGTST005-DADOS )
                        RESP    ( WS-RESP1       )
                        RESP2   ( WS-RESP2       )
           END-EXEC

           IF  WS-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMA-ACIONADO     TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               MOVE 'Erro ao acionar o módulo.'
                                       TO  MSG1-TX-ERRO-TS
      *        Monitorar variáveis do módulo chamado                     VRS001
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF SGTST005-CD-RTN-PGM NOT EQUAL ZEROS AND 100
              SET ERRO-PROGRAMA-ACIONADO        TO TRUE
              MOVE 3004                         TO MSG1-CD-ERRO-TS
              MOVE 'Erro ao acionar o módulo.'  TO MSG1-TX-ERRO-TS
              MOVE SGTST005                     TO MSG1-NM-PGM-ACND
              MOVE SGTST005-CD-RTN-PGM          TO MSG1-CD-RTN-PGM-ACND
              MOVE SGTST005-MSG-ERRO            TO MSG1-TX-RTN-PGM-ACND

      *        Monitorar variáveis do módulo chamado                     VRS001
              PERFORM 969000-MONITORAR-VARIAVEIS
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       970000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       971000-LER-ERRO                 SECTION.
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO
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
                   SET ERRO-PROGRAMA-ACIONADO       TO TRUE
                   MOVE 3004                        TO MSG1-CD-ERRO-TS
                   MOVE 'Erro ao acionar o módulo.' TO MSG1-TX-ERRO-TS
                   MOVE NM-PGM-CHMD                 TO MSG1-NM-PGM-ACND
                   MOVE IIBKERRO-CD-ERRO        TO MSG1-CD-RTN-PGM-ACND
                   MOVE IIBKERRO-TX-ERRO        TO MSG1-TX-RTN-PGM-ACND
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              WHEN OTHER
                   SET ERRO-CONTAINER TO TRUE
                   PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       971000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-RESPOSTA             SECTION.
      *-----------------------------------------------------------------
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

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
      *--> Lê container de resposta do SGTSL079
               WHEN SGTSL079
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSL079-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC
      *-----------------------------------------------------------------
               WHEN OTHER
                 SET SEM-ERRO        TO TRUE
                 SET ERRO-PROGRAMADO TO TRUE
                 MOVE 9006             TO MSG1-CD-ERRO-TS
                 MOVE  'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS                VRS001

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
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

      *--> Recupera o tamanho previsto e o tamanho recebido da resposta
      *--> do programa que foi acionado.
           EVALUATE NM-PGM-CHMD
      *-----------------------------------------------------------------
      *--> Lê container de resposta do SGTSL079
               WHEN SGTSL079
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR     )
                           NODATA
                           FLENGTH   ( QT-CCT-LIDO )
                 END-EXEC
      *-----------------------------------------------------------------
               WHEN OTHER
                 SET SEM-ERRO          TO TRUE
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 9006             TO MSG1-CD-ERRO-TS
                 MOVE  'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS                VRS001

                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
      *-----------------------------------------------------------------
           .
       972100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       982000-GRAVA-REQUISICAO         SECTION.
      *-----------------------------------------------------------------
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

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
      *--> Grava container de requisição do programa SGTSL079
               WHEN SGTSL079
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTSL079-RQSC           )
                            FLENGTH   ( LENGTH OF SGTSL079-RQSC )
                  END-EXEC
      *-----------------------------------------------------------------
               WHEN OTHER
                 SET SEM-ERRO          TO TRUE
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 9006             TO MSG1-CD-ERRO-TS
                 MOVE  'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS                VRS001

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
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888               TO MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                       TO MSG1-TX-ERRO-TS
           END-IF

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


           MOVE 'SGTS0111-CD-CTRA-TIT-VL'   TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0111-CD-CTRA-TIT-VL    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0111-CD-OPR-ETRD-MCP'  TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0111-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE  SGTS0111-CD-OPR-ETRD-MCP  TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX        TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'      TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0111-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0111-NR-VRS-OPR-ETRD   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0111-CD-TIP-CND-NEGD'  TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0111-CD-TIP-CND-NEGD   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0111-DT-INC-TAXA-QBRA' TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0111-DT-INC-TAXA-QBRA  TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO

           EXEC CICS PUT
              CONTAINER ( 'PRM-ERRO'                  )
              CHANNEL   ( NM-CNL                      )
              FROM      ( IIBKERRO-PRM-ERRO           )
              FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
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
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      * --------------------------------------*
-INC HLPKP999
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999
      * --------------------------------------*

      * --------------------------------------*
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
-INC MSGKPMG1

      * --------------------------------------*
       999999-ENCERRAR                 SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.