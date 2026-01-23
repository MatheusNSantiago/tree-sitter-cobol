      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0140
      * ANALISTA..: F2054109 Cintia Ap Soares Iglesias
      * AUTOR.....: F0950413 António Francisco Mendonça Silva
      * DATA......: 09/03/2017
      * OBJETIVO..: Negócio - Listar demonstrativo de caixa disponivel
      *             para vinculação.
      * COMPILACAO: COBOL/CICS (On-line)
      *-----------------------------------------------------------------
      * VRS002 23.04.2019 - F2634012 Inclusão do campo SGTSL08-CD-CTRA-
      *                     TIT-VL na chamada do pgm    sgtsl086
      * VRS001 31.01.2019 - F0950413 Inclusão da sigla 'SGT' na section
      *                     971000-LER-ERRO
      *        17.08.2018 - F8785286 - Inclusão movimentação 1 para
      *          SGTSL086-CD-TIP-SRVC.
      *        09.08.2018 - F9158937 - Inclusão campo TX-DCR-MVTC
      *        09.03.2017 - F0950413 - 2015/137233 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0140.

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
               '*** SGTS0140 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS001'.
       77  SBDATAF                     PIC X(08) VALUE 'SBDATAF'.
       77  SGTST005                    PIC X(08) VALUE 'SGTST005'.
       77  SGTSL086                    PIC X(08) VALUE 'SGTSL086'.

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

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *--> Container de erro
       01  CTNR-PRM-ERRO               PIC X(16) VALUE 'PRM-ERRO'.

      *--> Areas de Controle de Programa Chamado (CONTAINER)
       01  CTL-PGM-CHMD.
           03  NM-PGM-CHMD             PIC  X(08)         VALUE ' '.
               88 NM-PGM-COM-RQSC      VALUE 'SGTSL086'.
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

      *--> Timestamp de trabalho
       01  AUX-TS.
           03  ANO                     PIC  X(04)     VALUE '0001'.
           03  FILLER                  PIC  X(01)     VALUE '-'.
           03  MES                     PIC  X(02)     VALUE '01'.
           03  FILLER                  PIC  X(01)     VALUE '-'.
           03  DIA                     PIC  X(02)     VALUE '01'.
           03  FILLER                  PIC  X(01)     VALUE '-'.
           03  HORA                    PIC  X(02)     VALUE '00'.
           03  FILLER                  PIC  X(01)     VALUE '.'.
           03  MINUTO                  PIC  X(02)     VALUE '00'.
           03  FILLER                  PIC  X(01)     VALUE '.'.
           03  SEGUNDO                 PIC  X(02)     VALUE '00'.
           03  FILLER                  PIC  X(01)     VALUE '.'.
           03  MICRO-SEGUNDO           PIC  X(06)     VALUE '000000'.

      *--> Data unica de trabalho para consistência de datas diferentes.
       01  AUX-DT-RCBT                 PIC  X(08)     VALUE ' '.
       01  AUX-DT-RCBT-R               REDEFINES AUX-DT-RCBT.
            03  DIA                    PIC 9(02).
            03  MES                    PIC 9(02).
            03  ANO                    PIC 9(04).

      *--> Mensagens de inconstência das datas a serem consistidas.
       01  AUX-MSG1-TX-ERRO-TS-1       PIC X(120)     VALUE
           'Data do recebimento previsto da vinculação, inválida.'.
       01  AUX-MSG1-TX-ERRO-TS-2       PIC X(120)     VALUE
           'Data do recebimento previsto, inválida.'.

      *--> Indicador para definir a mensagem de inconsistência a ser
      *--> informada para o usuário.
       01  IND-MSG                     PIC 9(01)      VALUE 0.
           88 IND-MSG-TS-1             VALUE 1.
           88 IND-MSG-TS-2             VALUE 2.

      *-----------------------------------------------------------------
      *--> Tabela para sumarização do VL-RLZD-VCLD, por timestamp
       01  TAB-VL-RLZD.
           03 TAB-VL-RLZD-OCR          OCCURS 600 TIMES .
              05 TAB-TIMESTAMP         PIC  X(026).
              05 TAB-VL-RLZD-VCLD      PIC S9(015)V99 COMP-3.

       01  AUX-TS-CHV-ANT              PIC  X(26)             VALUE ' '.
       01  IC-TAB                      PIC S9(04)     COMP-5  VALUE 0.
       01  IC-TAB-S                    PIC S9(04)     COMP-5  VALUE 0.
       01  IC-TAB-TS                   PIC S9(04)     COMP-5  VALUE 0.
       01  IC-TAB-LER-SUMARIZADO       PIC S9(04)     COMP-5  VALUE 0.
       01  IC-TAB-LER-DMTV             PIC S9(04)     COMP-5  VALUE 0.

      *--> Area auxiliar para descompactar valor COMP-3 para monitoração
       01  AUX-VL-R.
           03 AUX-VL                   PIC ----.---.---.---.--9,99.

      *--> Campo para definição de estado de erro em tabela.
       01  CD-ERRO-TAB                 PIC  9(01)             VALUE 0.
           88 COM-ERRO-TAB                                    VALUE 1.

      *-----------------------------------------------------------------
      *--> Book tabela OPR_ETRD_MCP
       01  SGTST005-DADOS.
-INC SGTKT005
      *--> Book tabela DMTV_CX_TIT
-INC SGTKL086
      *--> Book do programa
-INC SGTK0140
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
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
           PERFORM 140000-VALIDAR-PARAMETRO

           PERFORM 200000-PROCESSAR-LISTA-DMTV-CX

           PERFORM 300000-POSTA-RESPOSTA
           PERFORM 999999-ENCERRAR
           GOBACK
           .
       000000-SAI.
           EXIT.
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
                CONTAINER ( 'SGTS0140-RPST' )
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
                CHANNEL   ( NM-CNL      )
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

           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
              CONTAINER ( 'SGTS0140-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           EXEC CICS GET
              CONTAINER ( 'SGTS0140-RQSC' )
              CHANNEL   ( NM-CNL          )
              FLENGTH   ( QT-CCT-LIDO     )
              INTO      ( SGTS0140-RQSC   )
           END-EXEC
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO SGTS0140-IN-CNTO-PSQ

           IF  SGTS0140-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0002               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de títulos de valores mobiliário
      -        's não informado.'      TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0140-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada de mercado de capita
      -        'is não numérico.'      TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0140-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0005               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada de mercado de capita
      -        'is não informado.'     TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0140-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0007               TO  MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturado não inform
      -             'ado.'             TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0140-DT-RCBT-PRVT-VCLC NOT EQUAL SPACES AND ZEROS
               MOVE SGTS0140-DT-RCBT-PRVT-VCLC
                                       TO AUX-DT-RCBT
               MOVE 1                  TO IND-MSG
               PERFORM 920000-VALIDAR-DATA
           ELSE
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0119               TO  MSG1-CD-ERRO-TS
               MOVE 'Data do recebimento previsto não informada.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0140-CD-TIP-RCBT-PRVT-VCLC EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0475               TO  MSG1-CD-ERRO-TS
               MOVE 'Código tipo recebimento previsto em vinculação não
      -             'informado.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0140-QT-OCR-TAB     NOT EQUAL ZEROS
               PERFORM 142000-CONSISTIR-OCORRENCIAS
                       VARYING IC-TAB FROM 1 BY 1
                         UNTIL IC-TAB > SGTS0140-QT-OCR-TAB

               INITIALIZE TAB-VL-RLZD
               PERFORM 143000-SUMARIZA-VL-RLZD-POR-TS
                       VARYING IC-TAB FROM 1 BY 1
                         UNTIL IC-TAB > SGTS0140-QT-OCR-TAB
           END-IF
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       142000-CONSISTIR-OCORRENCIAS    SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           IF  SGTS0140-DT-RCBT-PRVT (IC-TAB) NOT EQUAL SPACES AND ZEROS
               MOVE SGTS0140-DT-RCBT-PRVT (IC-TAB)
                                       TO AUX-DT-RCBT
               MOVE 2                  TO IND-MSG
               PERFORM 920000-VALIDAR-DATA
           ELSE
               SET  ERRO-PROGRAMADO    TO TRUE
               SET  COM-ERRO-TAB       TO TRUE
               MOVE 0119               TO  MSG1-CD-ERRO-TS
               MOVE 'Data do recebimento previsto não informada.'
                                      TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0140-CD-TIP-MVTC-RCBT-PRVT (IC-TAB) EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               SET  COM-ERRO-TAB       TO TRUE
               MOVE 0476               TO  MSG1-CD-ERRO-TS
               MOVE 'Código tipo movimentação recebimento previsto não i
      -        'nformado.'             TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0140-TS-REG-PGTO (IC-TAB) EQUAL SPACES
               SET  ERRO-PROGRAMADO    TO TRUE
               SET  COM-ERRO-TAB       TO TRUE
               MOVE 0316               TO  MSG1-CD-ERRO-TS
               MOVE 'Timestamp do registro de pagamento não informado.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0140-VL-RLZD-VCLD (IC-TAB) EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               SET  COM-ERRO-TAB       TO TRUE
               MOVE 0477               TO  MSG1-CD-ERRO-TS
               MOVE 'Valor realizado vinculado não informado.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       142000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       143000-SUMARIZA-VL-RLZD-POR-TS  SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           IF  SGTS0140-TS-REG-PGTO (IC-TAB)   NOT EQUAL AUX-TS-CHV-ANT
               ADD 1                   TO IC-TAB-TS
               MOVE SGTS0140-TS-REG-PGTO (IC-TAB)
                                       TO AUX-TS-CHV-ANT
                                          TAB-TIMESTAMP (IC-TAB-TS)
           END-IF

           ADD SGTS0140-VL-RLZD-VCLD (IC-TAB)
                                       TO TAB-VL-RLZD-VCLD (IC-TAB-TS)
           .
       143000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSAR-LISTA-DMTV-CX  SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           PERFORM 210000-OBTER-TITULAR-E-DIVISAO

      *--> Recuperar Demonstrativo de caixa
           MOVE SGTSL086               TO NM-PGM-CHMD
           MOVE 1                      TO SGTSL086-CD-TIP-SRVC
           MOVE 0                      TO SGTSL086-QT-OCR-TAB
           MOVE SGTST005-CD-CTRA-TIT-VL
                                       TO SGTSL086-CD-CTRA-TIT-VL
           MOVE SGTST005-CD-TIT-OPR-ETRD
                                       TO SGTSL086-CD-TIT-OPR-ETRD
           MOVE SGTST005-NR-DVS-TIT
                                       TO SGTSL086-NR-DVS-TIT
           IF SGTS0140-ULT-TS-REG-PGTO EQUAL SPACES
              MOVE AUX-TS              TO SGTSL086-ULT-TS-REG-PGTO
           ELSE
              MOVE SGTS0140-ULT-TS-REG-PGTO
                                       TO SGTSL086-ULT-TS-REG-PGTO
           END-IF

           PERFORM 900000-PROCEDIMENTOS-CONTAINER

           IF  SGTSL086-QT-OCR-TAB > 0
               MOVE 0                  TO IC-TAB-S
               IF  SGTS0140-QT-OCR-TAB     EQUAL ZEROS
                   PERFORM 220000-PROCESSA-DMTV-CX-TIT
                   PERFORM UNTIL SGTS0140-QT-OCR-TAB-S = 500
                              OR NOT SGTSL086-TEM-MAIS-REGISTRO
                        PERFORM 900000-PROCEDIMENTOS-CONTAINER
                        PERFORM 220000-PROCESSA-DMTV-CX-TIT
                   END-PERFORM
               ELSE
                   MOVE 1              TO IC-TAB-LER-SUMARIZADO
                                          IC-TAB-LER-DMTV

                   PERFORM 230000-BATIMENTO-RLZD-C-DMTV
                           UNTIL IC-TAB-LER-DMTV > SGTSL086-QT-OCR-TAB

                   PERFORM UNTIL SGTS0140-QT-OCR-TAB-S = 500
                              OR NOT SGTSL086-TEM-MAIS-REGISTRO
                        MOVE SGTSL086-TS-REG-PGTO (SGTSL086-QT-OCR-TAB)
                                       TO SGTSL086-ULT-TS-REG-PGTO
                        PERFORM 900000-PROCEDIMENTOS-CONTAINER
                        MOVE 1         TO IC-TAB-LER-DMTV
                        PERFORM 230000-BATIMENTO-RLZD-C-DMTV
                           UNTIL IC-TAB-LER-DMTV > SGTSL086-QT-OCR-TAB
                              OR SGTS0140-QT-OCR-TAB-S = 500
                   END-PERFORM
               END-IF
           END-IF
           MOVE SGTSL086-IN-CNTO-PSQ   TO SGTS0140-IN-CNTO-PSQ
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       210000-OBTER-TITULAR-E-DIVISAO  SECTION.
      *-----------------------------------------------------------------
      *--> Recuperar 'CD-TIT-OPR-ETRD' e 'NR-DVS-TIT'
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE SGTST005-DADOS
           SET SGTST005-CONSULTAR      TO  TRUE
           MOVE SGTS0140-CD-CTRA-TIT-VL
                                       TO  SGTST005-CD-CTRA-TIT-VL
           MOVE SGTS0140-CD-OPR-ETRD-MCP
                                       TO  SGTST005-CD-OPR-ETRD-MCP
           MOVE SGTS0140-NR-VRS-OPR-ETRD
                                       TO  SGTST005-NR-VRS-OPR-ETRD

           PERFORM 970000-CHAMAR-SGTST005

           IF  SGTST005-CD-RTN-PGM  EQUAL +100
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0353               TO  MSG1-CD-ERRO-TS
               MOVE 'Título não vinculado a operação estruturada.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Inicializar informaçoes referente à lista a ser recuperada
           MOVE 0                      TO SGTS0140-QT-OCR-TAB-S
           MOVE ' '                    TO SGTS0140-IN-CNTO-PSQ
           .
       210000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       220000-PROCESSA-DMTV-CX-TIT     SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           PERFORM 221000-FORMATA-SAIDA-COM-DMTV
                   VARYING IC-TAB FROM 1 BY 1
                     UNTIL IC-TAB > SGTSL086-QT-OCR-TAB
                        OR SGTS0140-QT-OCR-TAB-S = 500

           MOVE SGTSL086-TS-REG-PGTO (SGTSL086-QT-OCR-TAB)
                                       TO SGTSL086-ULT-TS-REG-PGTO
           .
       220000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       221000-FORMATA-SAIDA-COM-DMTV   SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           ADD 1                       TO IC-TAB-S
                                          SGTS0140-QT-OCR-TAB-S

           MOVE CORR SGTSL086-DT-EFT-MVTC-R (IC-TAB)  TO
                SGTS0140-DT-EFTC-MVTC-S-R    (IC-TAB-S)
           MOVE SGTSL086-CD-TIP-MVTC-TIT    (IC-TAB)  TO
                SGTS0140-CD-TIP-MVTC-S      (IC-TAB-S)
           MOVE SGTSL086-TS-REG-PGTO        (IC-TAB)  TO
                SGTS0140-TS-REG-PGTO-S      (IC-TAB-S)
           MOVE SGTSL086-VL-EFT-MVTC        (IC-TAB)  TO
                SGTS0140-VL-EFT-MVTC-S      (IC-TAB-S)
                SGTS0140-VL-DSPN-VCL-S      (IC-TAB-S)
           MOVE 0                                     TO
                SGTS0140-VL-VCLD-S          (IC-TAB-S)
           MOVE SGTSL086-TX-DCR-MVTC        (IC-TAB)  TO
                SGTS0140-TX-DCR-MVTC        (IC-TAB-S)
           .
       221000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       230000-BATIMENTO-RLZD-C-DMTV    SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

           IF TAB-TIMESTAMP (IC-TAB-LER-SUMARIZADO)
                             < SGTSL086-TS-REG-PGTO (IC-TAB-LER-DMTV)
              PERFORM 231000-LER-TABELA-SUMARIZADA
              ADD 1                   TO IC-TAB-LER-DMTV
           ELSE
             ADD 1                     TO IC-TAB-S
                                          SGTS0140-QT-OCR-TAB-S
             MOVE CORR SGTSL086-DT-EFT-MVTC-R (IC-TAB-LER-DMTV) TO
                       SGTS0140-DT-EFTC-MVTC-S-R (IC-TAB-S)
             MOVE SGTSL086-CD-TIP-MVTC-TIT (IC-TAB-LER-DMTV)    TO
                  SGTS0140-CD-TIP-MVTC-S (IC-TAB-S)
             MOVE SGTSL086-TX-DCR-MVTC   (IC-TAB-LER-DMTV)      TO
                  SGTS0140-TX-DCR-MVTC   (IC-TAB-S)
             MOVE SGTSL086-TS-REG-PGTO (IC-TAB-LER-DMTV)        TO
                  SGTS0140-TS-REG-PGTO-S (IC-TAB-S)
             MOVE SGTSL086-VL-EFT-MVTC (IC-TAB-LER-DMTV)        TO
                  SGTS0140-VL-EFT-MVTC-S (IC-TAB-S)
             MOVE 0                    TO SGTS0140-VL-VCLD-S (IC-TAB-S)
             IF  TAB-TIMESTAMP (IC-TAB-LER-SUMARIZADO)
                              > SGTSL086-TS-REG-PGTO (IC-TAB-LER-DMTV)
                  MOVE SGTSL086-VL-EFT-MVTC (IC-TAB-LER-DMTV)   TO
                       SGTS0140-VL-DSPN-VCL-S (IC-TAB-S)
                  ADD 1                TO IC-TAB-LER-DMTV
             ELSE
      *--> Cálculo do SGTS0140-VL-DSPN-VCL-S
               SUBTRACT TAB-VL-RLZD-VCLD (IC-TAB-LER-SUMARIZADO)
                   FROM SGTSL086-VL-EFT-MVTC (IC-TAB-LER-DMTV)
                        GIVING SGTS0140-VL-DSPN-VCL-S (IC-TAB-S)
      *--> Cálculo do SGTS0140-VL-VCLD-S
               PERFORM VARYING IC-TAB FROM 1 BY 1
                         UNTIL IC-TAB > SGTS0140-QT-OCR-TAB
                  IF SGTS0140-TS-REG-PGTO (IC-TAB) EQUAL
                     TAB-TIMESTAMP (IC-TAB-LER-SUMARIZADO)         AND

                     SGTS0140-DT-RCBT-PRVT (IC-TAB) EQUAL
                     SGTS0140-DT-RCBT-PRVT-VCLC                    AND

                     SGTS0140-CD-TIP-MVTC-RCBT-PRVT (IC-TAB) EQUAL
                     SGTS0140-CD-TIP-RCBT-PRVT-VCLC

                     MOVE SGTS0140-VL-RLZD-VCLD (IC-TAB)    TO
                          SGTS0140-VL-VCLD-S (IC-TAB-S)
                     MOVE SGTS0140-QT-OCR-TAB    TO IC-TAB
                  END-IF
               END-PERFORM
               PERFORM 231000-LER-TABELA-SUMARIZADA
               ADD 1                   TO IC-TAB-LER-DMTV
            END-IF
           END-IF
           .
       230000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       231000-LER-TABELA-SUMARIZADA    SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

           ADD 1                       TO IC-TAB-LER-SUMARIZADO
           IF  IC-TAB-LER-SUMARIZADO   > IC-TAB-TS
               MOVE HIGH-VALUES        TO
                    TAB-TIMESTAMP (IC-TAB-LER-SUMARIZADO)
           END-IF
           .
       231000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
                CONTAINER ( 'SGTS0140-RPST'         )
                CHANNEL   ( NM-CNL                   )
                FLENGTH   ( LENGTH OF SGTS0140-RPST )
                FROM      ( SGTS0140-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       900000-PROCEDIMENTOS-CONTAINER  SECTION.
      *-----------------------------------------------------------------
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

      * Executa o módulo chamado
      * Em caso de erro, a captura automática do CICS trata o erro
      * ---------------------------------------
           EVALUATE NM-PGM-CHMD
      * ---------------------------------------
               WHEN SGTSL086
                 EXEC CICS LINK
                           PROGRAM ( SGTSL086 )
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
      *-----------------------------------------------------------------
       920000-VALIDAR-DATA              SECTION.
      *-----------------------------------------------------------------
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

           IF DIA OF AUX-DT-RCBT-R     NOT NUMERIC OR
              MES OF AUX-DT-RCBT-R     NOT NUMERIC OR
              ANO OF AUX-DT-RCBT-R     NOT NUMERIC
              SET  ERRO-PROGRAMADO     TO TRUE
              MOVE 0120                TO  MSG1-CD-ERRO-TS
              IF IND-MSG-TS-1
                 MOVE AUX-MSG1-TX-ERRO-TS-1
                                       TO MSG1-TX-ERRO-TS
              ELSE
                 MOVE AUX-MSG1-TX-ERRO-TS-2
                                       TO MSG1-TX-ERRO-TS
              END-IF
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE AUX-DT-RCBT            TO ARG01-F01
           MOVE 4                      TO ARG02-F01
           MOVE 'F01'                  TO FUNCAO
           MOVE 4095                   TO AREA-TAM
           MOVE 'SBDATAF -WRKAREA-FIM' TO AREA-FIM

           CALL SBDATAF  USING  FUNCAO
                                WORKAREA
                                ARG01-F01
                                ARG02-F01

           IF  ARG01-F01     EQUAL 99999999
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE 0120               TO  MSG1-CD-ERRO-TS
               IF IND-MSG-TS-1
                  MOVE AUX-MSG1-TX-ERRO-TS-1
                                       TO MSG1-TX-ERRO-TS
                                         MSG1-TX-RTN-PGM-ACND
               ELSE
                  MOVE AUX-MSG1-TX-ERRO-TS-2
                                       TO MSG1-TX-ERRO-TS
                                         MSG1-TX-RTN-PGM-ACND
               END-IF

               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
                                          NM-PGM
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
            IF  ARG01-F01     EQUAL 88888888
                SET  ERRO-PROGRAMA-ACIONADO    TO TRUE
                MOVE SBDATAF            TO  MSG1-NM-PGM-ACND
                                            NM-PGM
                MOVE 051                TO  MSG1-CD-ERRO-TS
                MOVE RETURN-CODE        TO  MSG1-CD-RTN-PGM-ACND
                MOVE  'Erro no tratamento de datas.'
                                        TO MSG1-TX-ERRO-TS
                                           MSG1-TX-RTN-PGM-ACND
      *         Monitorar variáveis do módulo chamado
                PERFORM 969000-MONITORAR-VARIAVEIS
                PERFORM 999000-GRAVA-ERRO-E-ENCERRA
            END-IF
           END-IF
           .
       920000-SAI.
           EXIT.
      *----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM

               WHEN SBDATAF
                    MOVE 'FUNCAO'      TO MSGKW999-NM-VRV-MNT
                    MOVE FUNCAO        TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG01-F01'   TO MSGKW999-NM-VRV-MNT
                    MOVE ARG01-F01     TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG02-F01'   TO MSGKW999-NM-VRV-MNT
                    MOVE ARG02-F01     TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTST005
                    MOVE 'SGTST005-ACAO'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST005-ACAO
                                       TO MSGKW999-VL-VRV-MNT

                    MOVE 'SGTST005-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST005-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT

                    MOVE 'SGTST005-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF  SGTST005-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTST005-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP
                       MOVE CD-OPR-ETRD-MCP-TX
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código de Operação Estruturada deve ser núm
      -                     'ero.'
                                       TO MSGKW999-VL-VRV-MNT
                    END-IF
                    MOVE 'SGTST005-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST005-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL086
                    MOVE 'SGTSL086-CD-TIP-SRVC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL086-CD-TIP-SRVC
                                       TO MSGKW999-VL-VRV-MNT

                    MOVE 'SGTSL086-CD-TIT-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL086-CD-TIT-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT

                    MOVE 'SGTSL086-NR-DVS-TIT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL086-NR-DVS-TIT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL086-ULT-TS-REG-PGTO'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL086-ULT-TS-REG-PGTO
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL086-E-DT-EFT-MVTC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL086-E-DT-EFT-MVTC
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL086-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL086-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
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
           MOVE 0019                   TO  MSG1-CD-SEQL-ERRO

           EXEC CICS LINK
                        PROGRAM ( SGTST005       )
                        COMMAREA( SGTST005-DADOS )
                        RESP    ( WS-RESP1)
                        RESP2   ( WS-RESP2)
           END-EXEC

           IF  WS-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               MOVE 'Erro ao acionar o módulo.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF SGTST005-CD-RTN-PGM NOT EQUAL ZEROS AND NOT EQUAL +100
              SET ERRO-PROGRAMA-ACIONADO        TO TRUE
              MOVE SGTST005                     TO MSG1-NM-PGM-ACND
              MOVE 3004                         TO MSG1-CD-ERRO-TS
              MOVE 'Erro ao acionar o módulo.'  TO MSG1-TX-ERRO-TS
              MOVE SGTST005-CD-RTN-PGM          TO MSG1-CD-RTN-PGM-ACND
              MOVE SGTST005-MSG-ERRO            TO MSG1-TX-RTN-PGM-ACND
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       970000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       971000-LER-ERRO                 SECTION.
           MOVE 0020                   TO MSG1-CD-SEQL-ERRO
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
           MOVE 0021                   TO MSG1-CD-SEQL-ERRO

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
      *--> Lê container de resposta do SGTSL086
               WHEN SGTSL086
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSL086-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC
      *-----------------------------------------------------------------
               WHEN OTHER
                 SET SEM-ERRO        TO TRUE
                 SET ERRO-PROGRAMADO TO TRUE
                 MOVE 9006           TO MSG1-CD-ERRO-TS
                 MOVE 'Não definida a execução do módulo.'
                                     TO MSG1-TX-ERRO-TS
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
           MOVE 0022                   TO MSG1-CD-SEQL-ERRO

      *--> Recupera o tamanho previsto e o tamanho recebido da resposta
      *--> do programa que foi acionado.
           EVALUATE NM-PGM-CHMD
      *-----------------------------------------------------------------
      *--> Lê container de resposta do SGTSL086
               WHEN SGTSL086
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR            )
                           NODATA
                           FLENGTH   ( QT-CCT-LIDO )
                 END-EXEC
      *-----------------------------------------------------------------
               WHEN OTHER
                 SET SEM-ERRO          TO TRUE
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 9006           TO MSG1-CD-ERRO-TS
                 MOVE 'Não definida a execução do módulo.'
                                     TO MSG1-TX-ERRO-TS                   VRS001
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
      *-----------------------------------------------------------------
           .
       972100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       982000-GRAVA-REQUISICAO         SECTION.
      *-----------------------------------------------------------------
           MOVE 0023                   TO MSG1-CD-SEQL-ERRO

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
      *--> Grava container de requisição do programa SGTSL086
               WHEN SGTSL086
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR       )
                            FROM      ( SGTSL086-RQSC )
                            FLENGTH   ( LENGTH OF
                                        SGTSL086-RQSC )
                  END-EXEC
      *-----------------------------------------------------------------
               WHEN OTHER
                 SET SEM-ERRO          TO TRUE
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 9006           TO MSG1-CD-ERRO-TS
                 MOVE 'Não definida a execução do módulo.'
                                     TO MSG1-TX-ERRO-TS
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
           MOVE 0024                   TO MSG1-CD-SEQL-ERRO

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


           MOVE 'SGTS0140-CD-CTRA-TIT-VL'   TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0140-CD-CTRA-TIT-VL    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0140-CD-OPR-ETRD-MCP'  TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0140-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE  SGTS0140-CD-OPR-ETRD-MCP  TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX        TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'      TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0140-ULT-TS-REG-PGTO'  TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0140-ULT-TS-REG-PGTO   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0140-DT-RCBT-PRVT-VCLC' TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0140-DT-RCBT-PRVT-VCLC  TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0140-CD-TIP-RCBT-PRVT-VCLC' TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0140-CD-TIP-RCBT-PRVT-VCLC  TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           IF  COM-ERRO-TAB
               MOVE 'IC-TAB'                     TO MSGKW999-NM-VRV-MNT
               MOVE  IC-TAB                      TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0140-DT-RCBT-PRVT'      TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0140-DT-RCBT-PRVT (IC-TAB)
                                                 TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0140-CD-TIP-MVTC-RCBT-PRVT'
                                                 TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0140-CD-TIP-MVTC-RCBT-PRVT (IC-TAB)
                                                 TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0140-TS-REG-PGTO'       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0140-TS-REG-PGTO (IC-TAB)
                                                 TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0140-VL-RLZD-VCLD'      TO MSGKW999-NM-VRV-MNT
               MOVE SGTS0140-VL-RLZD-VCLD (IC-TAB)
                                                 TO AUX-VL
               MOVE  AUX-VL-R                    TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT
           END-IF

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
      * 999010-TRATA-ERRO-DB2          SECTION.
      * -INC HLPKP997
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