      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0134
      * ANALISTA..: F2054109 Cintia Ap Soares Iglesias
      * AUTOR.....: F0950413 António Francisco Mendonça Silva
      * DATA......: 13/04/2017
      * OBJETIVO..: Negócio - Listar recebimentos previstos, Capital,
      *             Extraordinário e Juros.
      * COMPILACAO: COBOL/CICS (On-line)
      *-----------------------------------------------------------------
      * VRS001 31.01.2019 F0950413 Inclusão da sigla 'SGT' na section
      *              971000-LER-ERRO
      *        23.01.2019 F8785286 DE 2016/145195 - Inclusão monitoração
      *              variáveis para rotina SBDATA
      *        08.08.2018 F8785286 - 2015/137233 - Recompilação por
      *            alteração no book SGTKL087
      *        13.04.2017 F0950413 - 2015/137233 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0134.

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
               '*** SGTS0134 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS001'.
       77  SBDATAF                     PIC X(08) VALUE 'SBDATAF'.

      *--> Módulos a serem acionados via 'EXEC CICS LINK (COMMAREA)'
       77  SGTST005                    PIC X(08) VALUE 'SGTST005'.
       77  SGTST011                    PIC X(08) VALUE 'SGTST011'.

      *--> Módulos a serem acionados via 'CONTAINER'
       77  SGTSL087                    PIC X(08) VALUE 'SGTSL087'.
       77  SGTSL503                    PIC X(08) VALUE 'SGTSL503'.
       77  SGTS0136                    PIC X(08) VALUE 'SGTS0136'.
       77  SGTSPE07                    PIC X(08) VALUE 'SGTSPE07'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).
       01  PC-TAXA-RMNC-OPR-TX.
           03  PC-TAXA-RMNC-OPR        PIC S9(04)V9(10).

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
               88 NM-PGM-COM-RQSC      VALUE 'SGTSL087'
                                             'SGTSL503'
                                             'SGTSPE07'
                                             'SGTS0136'.
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

      *--> Subscritores
       01  IC-TAB-1                    PIC S9(04)     COMP-5  VALUE 0.
       01  IC-TAB-2                    PIC S9(04)     COMP-5  VALUE 0.
       01  IC-TAB-300                  PIC S9(04)     COMP-5  VALUE 0.
       01  IC-TAB-800                  PIC S9(04)     COMP-5  VALUE 0.

      *--> Area auxiliar para descompactar valor COMP-3 para monitoração
       01  AUX-CD-OPR.
           03 AUX-CD-OPR-ETRD-MCP      PIC S9(11)             VALUE 0.

      *--> Tabela auxiliar para carga na tabela do BOOK SGTK0134
       01  AUX-QT-OCR-TAB-PGTO         PIC S9(04)      COMP-5 VALUE 0.
       01  AUX-LS-TAB-PGTO-VCLD.
           03  AUX-LS-TAB-OCR-PGTO OCCURS 0 TO 800 TIMES
                                       DEPENDING ON AUX-QT-OCR-TAB-PGTO.
              05 AUX-NR-SEQL-ITEM-CRNG
                                       PIC S9(09)      COMP-5.
              05 AUX-TS-REG-PGTO
                                       PIC  X(26).
              05 AUX-VL-RLZD-VCLD
                                       PIC S9(15)V9(2) COMP-3.

      *-----------------------------------------------------------------
      *--> Book tabela OPR_ETRD_MCP
       01  SGTST005-DADOS.
-INC SGTKT005

      *--> Book tabela RMNC_OPR_ETRD
       01  SGTST011-DADOS.
-INC SGTKT011

      *--> Book tabela RCBT_RLZD_OPR
       01  SGTSL087-DADOS.
-INC SGTKL087

      *--> Book tabelas RCBT_PRVT_OPR_ETRD, DMTV_CX_TIT
-INC SGTKL503

      *--> Book PGM, para gerar texto de Remuneração p/ CD-TIP-MVTC '1'
       01  SGTS0136-DADOS.
-INC SGTK0136

       01  SGTSPE07-DADOS.
-INC SGTKPE07


      *--> Book do programa
-INC SGTK0134
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      * Habilita captura automatica de abend e erros CICS
-INC HLPKE999
           MOVE CTE-PROG(5:8)          TO NM-PGM

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 110000-EXCLUIR-CTNR-RPST
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO

           PERFORM 140000-VALIDAR-PARAMETROS
           PERFORM 150000-VALOR-INICIAL-VARIAVEIS

           PERFORM 200000-LISTA-VALORES-PREVISTOS

           PERFORM 300000-POSTA-RESPOSTA
           PERFORM 999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS ASSIGN
                CHANNEL ( NM-CNL )
           END-EXEC
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
                CONTAINER ( 'SGTS0134-RPST' )
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
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS GET
              CONTAINER ( 'SGTS0134-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTS0134-RQSC )
              INTO      ( SGTS0134-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETROS       SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           IF  SGTS0134-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0002               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'   TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0134-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0134-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0005               TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' informado.'      TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0134-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0007               TO  MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0134-DT-RCBT-PRVT-ULT  NOT EQUAL SPACES AND ZEROS
               PERFORM 920000-VALIDAR-DATA
           ELSE
               MOVE '01010001'         TO SGTS0134-DT-RCBT-PRVT-ULT
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALOR-INICIAL-VARIAVEIS  SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO


           PERFORM 151000-CONSULTAR-SEQL-RCBT

      *--> Inicializar áreas de dados dos pgms a serem acionados:
           INITIALIZE                      SGTSL087-RQSC
                                           SGTSL503-RQSC
                                           SGTST005-DADOS
                                           SGTST011-DADOS
                                           SGTS0136-DADOS

      *--> Inicializar a açao dos pgms a serem acionados:
           SET  SGTSL087-LISTA-TIPO-E-DATA
                                       TO  TRUE
           SET  SGTST005-CONSULTAR
                                       TO  TRUE
           SET  SGTST011-CSUL-TX-RMNC-DT-INFD
                                       TO  TRUE

      *--> Inicializar informaçoes das listas a serem recuperadas:
           MOVE  ZEROS                 TO  SGTSL087-QT-OCR
                                           SGTSL503-QT-OCR-TAB
                                           SGTS0134-QT-OCR-TAB-RCBT

           MOVE  SPACES                TO  SGTSL087-IN-CTNC-PSQ
                                           SGTSL503-IN-CNTC-PSQ
                                           SGTS0134-IN-CNTC-PSQ

      *--> Informar as chaves de acesso, dos pgms a serem acionados:
           MOVE SGTS0134-CD-CTRA-TIT-VL
                                       TO  SGTSL503-CD-CTRA-TIT-VL
                                           SGTSL087-CD-CTRA-TIT-VL
                                           SGTST005-CD-CTRA-TIT-VL
                                           SGTST011-CD-CTRA-TIT-VL
           MOVE SGTS0134-CD-OPR-ETRD-MCP
                                       TO  SGTSL503-CD-OPR-ETRD-MCP
                                           SGTSL087-CD-OPR-ETRD-MCP
                                           SGTST005-CD-OPR-ETRD-MCP
                                           SGTST011-CD-OPR-ETRD-MCP
           MOVE SGTS0134-NR-VRS-OPR-ETRD
                                       TO SGTSL503-NR-VRS-OPR-ETRD
                                          SGTSL087-NR-VRS-OPR-ETRD
                                          SGTST005-NR-VRS-OPR-ETRD
                                          SGTST011-NR-VRS-OPR-ETRD
           MOVE SGTSPE07-NR-SEQL-RCBT-PRVT
                                       TO SGTSL503-NR-SEQL-RCBT-PRVT
                                          SGTSL087-NR-SEQL-RCBT-PRVT

           MOVE SGTS0134-CD-TIP-RCBT-PRVT-ULT
                                       TO SGTSL503-CD-TIP-RCBT-PRVT-ULT
           MOVE SGTS0134-DT-RCBT-PRVT-ULT
                                       TO SGTSL503-DT-RCBT-PRVT-ULT
           .
       150000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       151000-CONSULTAR-SEQL-RCBT       SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

      *--> Consulta o Ultimo Sequencial do Cronograma (Sequencial Atual)

           MOVE SGTS0134-CD-CTRA-TIT-VL   TO SGTSPE07-CD-CTRA-TIT
           MOVE SGTS0134-CD-OPR-ETRD-MCP
                                       TO SGTSPE07-CD-OPR-ETRD-MCP
           MOVE SGTS0134-NR-VRS-OPR-ETRD
                                       TO SGTSPE07-NR-VRS-OPR-ETRD

           MOVE  SGTSPE07              TO  NM-PGM-CHMD
           PERFORM  900000-PROCEDIMENTOS-CONTAINER

           .
       151000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-LISTA-VALORES-PREVISTOS  SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

      *--> Obter Código Titular Operação Estruturada, 'CD-TIT-OPR-ETRD'
      *--> Obter Numero Divisão Estruturada         , 'NR-DVS-TIT'
           PERFORM 950000-CHAMAR-SGTST005
           IF  SGTST005-CD-RTN-PGM  EQUAL +100
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0353               TO  MSG1-CD-ERRO-TS
               MOVE 'Título não vinculado a operação estruturada.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE SGTST005-CD-TIT-OPR-ETRD
                                       TO SGTSL087-CD-TIT-OPR-ETRD
                                          SGTSL503-CD-TIT-OPR-ETRD
           MOVE SGTST005-NR-DVS-TIT    TO SGTSL087-NR-DVS-TIT
                                          SGTSL503-NR-DVS-TIT

           PERFORM 220000-OBTER-RECBTOS-PREVISTOS

           IF SGTSL503-QT-OCR-TAB > 0
              PERFORM 230000-MOVER-RCBTOS-PREVISTOS
              PERFORM UNTIL SGTS0134-QT-OCR-TAB-RCBT = 300
                                 OR NOT SGTS0134-TEM-MAIS-REGISTRO
                      PERFORM 220000-OBTER-RECBTOS-PREVISTOS
                      PERFORM 230000-MOVER-RCBTOS-PREVISTOS
              END-PERFORM

              MOVE AUX-QT-OCR-TAB-PGTO TO SGTS0134-QT-OCR-TAB-PGTO
              PERFORM 231400-FORMATA-TAB-PGTOS-VCLD
                      VARYING IC-TAB-2 FROM 1 BY 1
                        UNTIL IC-TAB-2 > SGTS0134-QT-OCR-TAB-PGTO

           END-IF
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-OBTER-RECBTOS-PREVISTOS  SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

      *--> Listar Recebimentos Previstos da Operação Estruturada.
           MOVE SGTSL503               TO NM-PGM-CHMD
           MOVE 0                      TO SGTSL503-QT-OCR-TAB

           PERFORM 900000-PROCEDIMENTOS-CONTAINER
           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-MOVER-RCBTOS-PREVISTOS   SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           PERFORM 231000-FORMATA-SAIDA-RCBTOS
                   VARYING IC-TAB-1 FROM 1 BY 1
                     UNTIL IC-TAB-1 > SGTSL503-QT-OCR-TAB

           MOVE SGTSL503-CD-TIP-RCBT-PRVT (SGTSL503-QT-OCR-TAB)
                                       TO SGTSL503-CD-TIP-RCBT-PRVT-ULT
           MOVE SGTSL503-DT-RCBT-PRVT (SGTSL503-QT-OCR-TAB)
                                       TO SGTSL503-DT-RCBT-PRVT-ULT
           MOVE SGTSL503-IN-CNTC-PSQ   TO SGTS0134-IN-CNTC-PSQ
           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       231000-FORMATA-SAIDA-RCBTOS     SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

           ADD 1                       TO IC-TAB-300
                                          SGTS0134-QT-OCR-TAB-RCBT

           MOVE SGTSL503-DT-RCBT-PRVT       (IC-TAB-1)  TO
                SGTS0134-DT-RCBT-PRVT       (IC-TAB-300)
           MOVE SGTSL503-CD-TIP-RCBT-PRVT   (IC-TAB-1)  TO
                SGTS0134-CD-TIP-RCBT-PRVT   (IC-TAB-300)
           MOVE SGTSL503-PC-RCBT-PRVT       (IC-TAB-1)  TO
                SGTS0134-PC-RCBT-PRVT       (IC-TAB-300)
           MOVE SGTSL503-VL-RCBT-PRVT       (IC-TAB-1)  TO
                SGTS0134-VL-RCBT-PRVT       (IC-TAB-300)
           MOVE SGTSL503-DT-EFTC-MVTC       (IC-TAB-1)  TO
                SGTS0134-DT-EFTC-MVTC       (IC-TAB-300)
           MOVE SGTSL503-VL-RLZD-VCLD       (IC-TAB-1)  TO
                SGTS0134-VL-RLZD            (IC-TAB-300)

      *----------> No caso de Juros Normal ou Extraordinário
      *----------> Obter remuneração na tabela de Remuneração e
      *----------> montar texto para exibição na tela
           IF SGTS0134-CD-TIP-RCBT-PRVT   (IC-TAB-300) = 1 OR 3
              PERFORM 231100-OBTER-ACESSO-TEXTO
           ELSE
              MOVE SPACES              TO
                   SGTS0134-TX-DCR-JUROS-PRVT  (IC-TAB-300)
           END-IF

           MOVE AUX-TS                 TO SGTSL087-TS-REG-PGTO-ULT

           PERFORM 231200-LISTA-VINCULOS-PAGTO

           IF SGTSL087-QT-OCR             GREATER  ZEROS
              PERFORM 231300-MOVE-VL-VINCULOS-PAGTO
              PERFORM UNTIL IC-TAB-800 = 800
                                 OR NOT SGTSL087-TEM-MAIS-REGISTRO
                     PERFORM 231200-LISTA-VINCULOS-PAGTO
                     PERFORM 231300-MOVE-VL-VINCULOS-PAGTO
              END-PERFORM
           END-IF
           .
       231000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       231100-OBTER-ACESSO-TEXTO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

           MOVE '  .  .    '           TO  SGTST011-DT-INC-TAXA-RMNC
           MOVE CORR SGTS0134-DT-RCBT-PRVT (IC-TAB-300)
                                       TO  SGTST011-DT-INC-TAXA-RMNC-R

           PERFORM 960000-CHAMAR-SGTST011

           IF  SGTST011-CD-RTN-PGM  EQUAL +100
              MOVE SPACES              TO
                   SGTS0134-TX-DCR-JUROS-PRVT  (IC-TAB-300)
           ELSE
               PERFORM 231110-OBTER-TEXTO-REMUNERACAO
           END-IF
           .
       231100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       231110-OBTER-TEXTO-REMUNERACAO  SECTION.
      *-----------------------------------------------------------------
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTS0136               TO NM-PGM-CHMD
           MOVE SGTST011-PC-TAXA-RMNC-OPR
                                       TO SGTS0136-PC-TAXA-RMNC-OPR
           MOVE SGTST011-CD-TIP-FMA-APL
                                       TO SGTS0136-CD-TIP-FMA-APL
           MOVE SGTST011-CD-IEC-RMNC   TO SGTS0136-CD-IEC-RMNC

           PERFORM 900000-PROCEDIMENTOS-CONTAINER

           MOVE SGTS0136-TX-DCR-RMNC-OPR    TO
                SGTS0134-TX-DCR-JUROS-PRVT (IC-TAB-300)
           .
       231110-SAI.
           EXIT.

      *-----------------------------------------------------------------
       231200-LISTA-VINCULOS-PAGTO     SECTION.
      *--> Acionar o SGTSL087 e Listar Valores Realizados vinculados
      *-----------------------------------------------------------------
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSL087               TO NM-PGM-CHMD
           MOVE  ZEROS                 TO SGTSL087-QT-OCR
           MOVE SGTS0134-CD-TIP-RCBT-PRVT (IC-TAB-300)
                                       TO SGTSL087-CD-TIP-RCBT-PRVT-ULT
           MOVE SGTS0134-DT-RCBT-PRVT     (IC-TAB-300)
                                       TO SGTSL087-DT-RCBT-PRVT-ULT

           PERFORM 900000-PROCEDIMENTOS-CONTAINER
           .
       231200-SAI.
           EXIT.

      *-----------------------------------------------------------------
       231300-MOVE-VL-VINCULOS-PAGTO   SECTION.
      *-----------------------------------------------------------------
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

           PERFORM 231310-FORMATA-VALORES-VCLD
                   VARYING IC-TAB-2 FROM 1 BY 1
                     UNTIL IC-TAB-2 > SGTSL087-QT-OCR

           MOVE SGTSL087-TS-REG-PGTO (SGTSL087-QT-OCR)
                                       TO SGTSL087-TS-REG-PGTO-ULT
           .
       231300-SAI.
           EXIT.

      *-----------------------------------------------------------------
       231310-FORMATA-VALORES-VCLD     SECTION.
      *-----------------------------------------------------------------
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

           ADD 1                       TO IC-TAB-800
                                          AUX-QT-OCR-TAB-PGTO

           MOVE IC-TAB-300     TO
                               AUX-NR-SEQL-ITEM-CRNG (IC-TAB-800)
           MOVE SGTSL087-TS-REG-PGTO  (IC-TAB-2)
                               TO AUX-TS-REG-PGTO    (IC-TAB-800)
           MOVE SGTSL087-VL-RLZD-VCLD (IC-TAB-2)
                               TO AUX-VL-RLZD-VCLD   (IC-TAB-800)
           .
       231310-SAI.
           EXIT.

      *-----------------------------------------------------------------
       231400-FORMATA-TAB-PGTOS-VCLD   SECTION.
      *-----------------------------------------------------------------
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

           MOVE AUX-NR-SEQL-ITEM-CRNG (IC-TAB-2)      TO
                SGTS0134-NR-SEQL-ITEM-CRNG (IC-TAB-2)

           MOVE AUX-TS-REG-PGTO (IC-TAB-2)            TO
                SGTS0134-TS-REG-PGTO (IC-TAB-2)

           MOVE AUX-VL-RLZD-VCLD (IC-TAB-2)           TO
                SGTS0134-VL-VCLD (IC-TAB-2)
           .
       231400-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
                CONTAINER ( 'SGTS0134-RPST'         )
                CHANNEL   ( NM-CNL                   )
                FLENGTH   ( LENGTH OF SGTS0134-RPST )
                FROM      ( SGTS0134-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       900000-PROCEDIMENTOS-CONTAINER  SECTION.
      *-----------------------------------------------------------------
           MOVE 0020                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0021                   TO MSG1-CD-SEQL-ERRO

      * Executa o módulo chamado
      * Em caso de erro, a captura automática do CICS trata o erro
      * ---------------------------------------
           EVALUATE NM-PGM-CHMD
      * ---------------------------------------
               WHEN SGTSL087
                 EXEC CICS LINK
                           PROGRAM ( SGTSL087 )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC
               WHEN SGTSL503
                 EXEC CICS LINK
                           PROGRAM ( SGTSL503 )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC
               WHEN SGTS0136
                 EXEC CICS LINK
                           PROGRAM ( SGTS0136 )
                           CHANNEL ( NM-CNL    )
                           RESP    ( MSG1-RESP1)
                           RESP2   ( MSG1-RESP2)
                 END-EXEC
               WHEN SGTSPE07
                 EXEC CICS LINK
                           PROGRAM ( SGTSPE07 )
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
           MOVE 0022                   TO MSG1-CD-SEQL-ERRO

           IF DIA OF SGTS0134-DT-RCBT-PRVT-ULT NOT NUMERIC OR
              MES OF SGTS0134-DT-RCBT-PRVT-ULT NOT NUMERIC OR
              ANO OF SGTS0134-DT-RCBT-PRVT-ULT NOT NUMERIC
              SET  ERRO-PROGRAMADO     TO TRUE
              MOVE 0120                TO  MSG1-CD-ERRO-TS
              MOVE 'Data de recebimento previsto inválida.'
                                      TO MSG1-TX-ERRO-TS
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  SGTS0134-DT-RCBT-PRVT-ULT
                                       TO ARG01-F01
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
               MOVE 'Data de recebimento previsto inválida.'
                                      TO MSG1-TX-ERRO-TS
                                         MSG1-TX-RTN-PGM-ACND

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

      *----------------------------------------------------------------*
       950000-CHAMAR-SGTST005          SECTION.
      *----------------------------------------------------------------*
           MOVE 0023                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS LINK
                        PROGRAM ( SGTST005       )
                        COMMAREA( SGTST005-DADOS )
                        RESP    ( WS-RESP1)
                        RESP2   ( WS-RESP2)
           END-EXEC

           IF  WS-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMA-ACIONADO  TO TRUE
               MOVE SGTST005               TO MSG1-NM-PGM-ACND
               MOVE 3004                   TO MSG1-CD-ERRO-TS
               MOVE 'Erro ao acionar o módulo.'
                                           TO MSG1-TX-ERRO-TS
                                           MSG1-TX-RTN-PGM-ACND
               MOVE WS-RESP1               TO MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF SGTST005-CD-RTN-PGM NOT EQUAL ZEROS AND NOT EQUAL +100
              SET ERRO-PROGRAMA-ACIONADO   TO TRUE
              MOVE SGTST005                TO MSG1-NM-PGM-ACND
              MOVE 3004                    TO MSG1-CD-ERRO-TS
              MOVE 'Erro ao acionar o módulo.'
                                           TO MSG1-TX-ERRO-TS
              MOVE SGTST005-CD-RTN-PGM     TO MSG1-CD-RTN-PGM-ACND
              MOVE SGTST005-MSG-ERRO       TO MSG1-TX-RTN-PGM-ACND
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       950000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       960000-CHAMAR-SGTST011          SECTION.
      *----------------------------------------------------------------*
           MOVE 0024                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS LINK
                        PROGRAM ( SGTST011       )
                        COMMAREA( SGTST011-DADOS )
                        RESP    ( WS-RESP1)
                        RESP2   ( WS-RESP2)
           END-EXEC

           IF  WS-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE SGTST011              TO MSG1-NM-PGM-ACND
               MOVE 3004                  TO MSG1-CD-ERRO-TS
               MOVE 'Erro ao acionar o módulo.'
                                          TO MSG1-TX-ERRO-TS
                                             MSG1-TX-RTN-PGM-ACND
               MOVE WS-RESP1              TO MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF SGTST011-CD-RTN-PGM NOT EQUAL ZEROS AND NOT EQUAL +100
              SET ERRO-PROGRAMA-ACIONADO   TO TRUE
              MOVE SGTST011                TO MSG1-NM-PGM-ACND
              MOVE 3004                    TO MSG1-CD-ERRO-TS
              MOVE 'Erro ao acionar o módulo.'
                                           TO MSG1-TX-ERRO-TS
              MOVE SGTST011-CD-RTN-PGM     TO MSG1-CD-RTN-PGM-ACND
              MOVE SGTST011-MSG-ERRO       TO MSG1-TX-RTN-PGM-ACND
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       960000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*

           EVALUATE NM-PGM

                WHEN SGTST005
                    MOVE 'SGTST005-ACAO'          TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST005-ACAO            TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST005-CD-CTRA-TIT-VL'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST005-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST005-CD-OPR-ETRD-MCP'
                                                  TO MSGKW999-NM-VRV-MNT
                    IF   SGTST005-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTST005-CD-OPR-ETRD-MCP TO CD-OPR-ETRD-MCP
                       MOVE CD-OPR-ETRD-MCP-TX    TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código de operação estruturada de mercado d
      -                     'e capitais não numérico.'
                                                  TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST005-NR-VRS-OPR-ETRD'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST005-NR-VRS-OPR-ETRD TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTST011
                    MOVE 'SGTST011-ACAO'          TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST011-ACAO            TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST011-CD-CTRA-TIT-VL'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST011-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST011-CD-OPR-ETRD-MCP'
                                                  TO MSGKW999-NM-VRV-MNT
                    IF   SGTST011-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTST011-CD-OPR-ETRD-MCP TO CD-OPR-ETRD-MCP
                       MOVE CD-OPR-ETRD-MCP-TX    TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código de operação estruturada de mercado d
      -                     'e capitais não numérico.'
                                                  TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST011-NR-VRS-OPR-ETRD'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST011-NR-VRS-OPR-ETRD TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST011-DT-INC-TAXA-RMNC'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST011-DT-INC-TAXA-RMNC
                                                  TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSL087
                    MOVE 'SGTSL087-CD-TIP-SRVC'   TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL087-CD-TIP-SRVC     TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL087-CD-CTRA-TIT-VL'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL087-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL087-CD-OPR-ETRD-MCP'
                                                  TO MSGKW999-NM-VRV-MNT
                    IF   SGTSL087-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSL087-CD-OPR-ETRD-MCP TO CD-OPR-ETRD-MCP
                       MOVE CD-OPR-ETRD-MCP-TX    TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código de operação estruturada de mercado d
      -                     'e capitais não numérico.'
                                                  TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL087-NR-VRS-OPR-ETRD'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL087-NR-VRS-OPR-ETRD TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL087-NR-SEQL-RCBT-PRVT'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL087-NR-SEQL-RCBT-PRVT
                                                  TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL087-CD-TIT-OPR-ETRD'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL087-CD-TIT-OPR-ETRD TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL087-NR-DVS-TIT'    TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL087-NR-DVS-TIT      TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL087-NR-SEQL-RCBT-PRVT-ULT'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL087-NR-SEQL-RCBT-PRVT-ULT
                                                  TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL087-CD-TIP-RCBT-PRVT-ULT'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL087-CD-TIP-RCBT-PRVT-ULT
                                                  TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL087-DT-RCBT-PRVT-ULT'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL087-DT-RCBT-PRVT-ULT
                                                  TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL087-TS-REG-PGTO-ULT'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL087-TS-REG-PGTO-ULT TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSL503
                    MOVE 'SGTSL503-CD-CTRA-TIT-VL'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL503-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL503-CD-OPR-ETRD-MCP'
                                                  TO MSGKW999-NM-VRV-MNT
                    IF   SGTSL503-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSL503-CD-OPR-ETRD-MCP TO CD-OPR-ETRD-MCP
                       MOVE CD-OPR-ETRD-MCP-TX    TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código de operação estruturada de mercado d
      -                     'e capitais não numérico.'
                                                  TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL503-NR-VRS-OPR-ETRD'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL503-NR-VRS-OPR-ETRD TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL503-NR-SEQL-RCBT-PRVT'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL503-NR-SEQL-RCBT-PRVT
                                                  TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL503-CD-TIT-OPR-ETRD'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL503-CD-TIT-OPR-ETRD TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL503-CD-TIP-RCBT-PRVT-ULT'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL503-CD-TIP-RCBT-PRVT-ULT
                                                  TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL503-NR-DVS-TIT'    TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL503-NR-DVS-TIT      TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL503-DT-RCBT-PRVT-ULT'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL503-DT-RCBT-PRVT-ULT
                                                  TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTS0136
                    MOVE 'SGTS0136-PC-TAXA-RMNC-OPR'
                                                  TO MSGKW999-NM-VRV-MNT
                    IF SGTS0136-PC-TAXA-RMNC-OPR IS NUMERIC
                       MOVE SGTS0136-PC-TAXA-RMNC-OPR TO
                                             PC-TAXA-RMNC-OPR
                       MOVE PC-TAXA-RMNC-OPR-TX   TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Percentual taxa remuneração da operação não
      -                     ' numérico.'
                                                  TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTS0136-CD-TIP-FMA-APL'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTS0136-CD-TIP-FMA-APL  TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTS0136-CD-IEC-RMNC'   TO MSGKW999-NM-VRV-MNT
                    MOVE SGTS0136-CD-IEC-RMNC     TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSPE07
                    MOVE 'SGTSPE07-CD-CTRA-TIT'   TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSPE07-CD-CTRA-TIT     TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSPE07-CD-OPR-ETRD-MCP'
                                                  TO MSGKW999-NM-VRV-MNT
                    IF   SGTSPE07-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSPE07-CD-OPR-ETRD-MCP TO CD-OPR-ETRD-MCP
                       MOVE CD-OPR-ETRD-MCP-TX    TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código de operação estruturada de mercado d
      -                     'e capitais não numérico.'
                                                  TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSPE07-NR-VRS-OPR-ETRD'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSPE07-NR-VRS-OPR-ETRD TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SBDATAF
                    MOVE 'FUNCAO'                 TO MSGKW999-NM-VRV-MNT
                    MOVE FUNCAO                   TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG01-F01'              TO MSGKW999-NM-VRV-MNT
                    MOVE ARG01-F01                TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG02-F01'              TO MSGKW999-NM-VRV-MNT
                    MOVE ARG01-F01                TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN OTHER
                    CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       971000-LER-ERRO                 SECTION.
           MOVE 0025                   TO MSG1-CD-SEQL-ERRO
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
                MOVE NM-PGM                  TO  MSG1-NM-PGM-ACND
                IF NM-PGM(1:3) EQUAL 'SGT'
                    MOVE IIBKERRO-CD-ERRO    TO  MSG1-CD-RTN-PGM-ACND
                                                 MSG1-CD-ERRO-TS
                    MOVE IIBKERRO-TX-ERRO    TO  MSG1-TX-RTN-PGM-ACND
                                                 MSG1-TX-ERRO-TS
                ELSE
                    MOVE  3004               TO  MSG1-CD-ERRO-TS
                    STRING  'Erro ao acionar o módulo ' NM-PGM '.'
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
           MOVE 0026                   TO MSG1-CD-SEQL-ERRO

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
      *--> Lê container de resposta do SGTSL087
               WHEN SGTSL087
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSL087-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC
      *--> Lê container de resposta do SGTSL503
               WHEN SGTSL503
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSL503-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC
      *--> Lê container de resposta do SGTS0136
               WHEN SGTS0136
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTS0136-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC
               WHEN SGTSPE07
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSPE07-RPST )
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
           MOVE 0027                   TO MSG1-CD-SEQL-ERRO

      *--> Recupera o tamanho previsto e o tamanho recebido da resposta
      *--> do programa que foi acionado.
           EVALUATE NM-PGM-CHMD
      *-----------------------------------------------------------------
      *--> Lê container de resposta do SGTSL087
               WHEN SGTSL087
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR            )
                           NODATA
                           FLENGTH   ( QT-CCT-LIDO )
                 END-EXEC
      *--> Lê container de resposta do SGTSL503
               WHEN SGTSL503
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR            )
                           NODATA
                           FLENGTH   ( QT-CCT-LIDO )
                 END-EXEC
      *--> Lê container de resposta do SGTS0136
               WHEN SGTS0136
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR            )
                           NODATA
                           FLENGTH   ( QT-CCT-LIDO )
                 END-EXEC
               WHEN SGTSPE07
                 EXEC CICS GET
                           CONTAINER ( NM-CTNR            )
                           NODATA
                           FLENGTH   ( QT-CCT-LIDO )
                 END-EXEC
      *-----------------------------------------------------------------
               WHEN OTHER
                 SET SEM-ERRO          TO TRUE
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 9006             TO MSG1-CD-ERRO-TS
                 MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
      *-----------------------------------------------------------------
           .
       972100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       982000-GRAVA-REQUISICAO         SECTION.
      *-----------------------------------------------------------------
           MOVE 0028                   TO MSG1-CD-SEQL-ERRO

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
               WHEN SGTSL087
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR       )
                            FROM      ( SGTSL087-RQSC )
                            FLENGTH   ( LENGTH OF
                                        SGTSL087-RQSC )
                  END-EXEC
      *--> Grava container de requisição do programa SGTSL503
               WHEN SGTSL503
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR       )
                            FROM      ( SGTSL503-RQSC )
                            FLENGTH   ( LENGTH OF
                                        SGTSL503-RQSC )
                  END-EXEC
      *--> Grava container de requisição do programa SGTS0136
               WHEN SGTS0136
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR       )
                            FROM      ( SGTS0136-RQSC )
                            FLENGTH   ( LENGTH OF
                                        SGTS0136-RQSC )
                  END-EXEC
               WHEN SGTSPE07
                  EXEC CICS PUT
                            CONTAINER ( NM-CTNR       )
                            FROM      ( SGTSPE07-RQSC )
                            FLENGTH   ( LENGTH OF
                                        SGTSPE07-RQSC )
                  END-EXEC
      *-----------------------------------------------------------------
               WHEN OTHER
                 SET SEM-ERRO          TO TRUE
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 9006             TO MSG1-CD-ERRO-TS
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
           MOVE 0029                   TO MSG1-CD-SEQL-ERRO

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

           MOVE 'SGTS0134-CD-CTRA-TIT-VL'   TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0134-CD-CTRA-TIT-VL    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0134-CD-OPR-ETRD-MCP'  TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0134-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE  SGTS0134-CD-OPR-ETRD-MCP  TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX    TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'  TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0134-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0134-NR-VRS-OPR-ETRD   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0134-NR-SEQL-RCBT-PRVT' TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0134-NR-SEQL-RCBT-PRVT  TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0134-CD-TIP-RCBT-PRVT-ULT' TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0134-CD-TIP-RCBT-PRVT-ULT  TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0134-DT-RCBT-PRVT-ULT' TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0134-DT-RCBT-PRVT-ULT  TO MSGKW999-VL-VRV-MNT
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
           END-EXEC.
       999999-SAI.
           EXIT.