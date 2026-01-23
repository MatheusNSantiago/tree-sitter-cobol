      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0262
      * ANALISTA..: F0950413 António Francisco Mendonça Silva
      * AUTOR.....: F0950413 António Francisco Mendonça Silva
      * DATA......: 12/08/2019
      * OBJETIVO..: Listar operaçoes em exceção.
      * COMPILACAO: COBOL/CICS (On-line)
      *-----------------------------------------------------------------
      * VRS001 09.08.2019 F0950413 DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0262.

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
       77  CTE-INICIO-WS               PIC X(30) VALUE
                                       '*** WSS COMECA AQUI ***'.
       77  CTE-PROG                    PIC X(18) VALUE
           '*** SGTS0262 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS001'.

      *--> Módulos a serem acionados via 'CONTAINER'
       77  SGTSC003                    PIC X(08) VALUE 'SGTSC003'.
       77  SGTS0263                    PIC X(08) VALUE 'SGTS0263'.
       77  SGTS0264                    PIC X(08) VALUE 'SGTS0264'.
       77  SGTS0265                    PIC X(08) VALUE 'SGTS0265'.

      *--> Subrotinas Utilizadas
       77  SBDATAF                     PIC X(08) VALUE 'SBDATAF '.

      *-----------------------------------------------------------------
      *                     CONTAINERS UTILIZADOS
      *-----------------------------------------------------------------
      *--> CONTAINER DE ERRO
       77 CTNR-PRM-ERRO                PIC X(16) VALUE 'PRM-ERRO'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
      *-----------------------------------------------------------------
-INC HLPKE01B

       01  CTE-FIM-WS                  PIC  X(30)  VALUE
                                       '*** WSS TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  CTE-INICIO-LS               PIC X(30)  VALUE
                                       '*** LS COMECA AQUI   ***'.

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC   ---------------9.

      *--> Variáveis de trabalho
       01  IC-TAB                      PIC S9(04)     COMP-5  VALUE 0.
       01  AUX-IN-FLZ                  PIC  X(01)             VALUE ' '.
       01  AUX-DT-PSC                  PIC  X(08)             VALUE ' '.
       01  AUX-CD-CTRA-TIT-VL          PIC S9(09)     COMP-5  VALUE  0.
       01  AUX-CD-OPR-ETRD-MCP         PIC S9(11)     COMP-3  VALUE 0.
       01  AUX-CD-TIT-OPR-ETRD         PIC  X(12)             VALUE ' '.
       01  AUX-CD-CLI                  PIC S9(09)     COMP-5  VALUE 0.
       01  AUX-CD-RGR-VLDC-ULT         PIC S9(09)     COMP-5  VALUE 0.
       01  AUX-CD-CTRA-TIT-VL-ULT      PIC S9(09)     COMP-5  VALUE  0.
       01  AUX-CD-OPR-ETRD-MCP-ULT     PIC S9(11)     COMP-3  VALUE 0.

      *--> Variáveis para leitura das listas
       01  IC-TAB-0263                 PIC S9(04)     COMP-5  VALUE 0.
       01  IC-TAB-0264                 PIC S9(04)     COMP-5  VALUE 0.
       01  IC-TAB-0265                 PIC S9(04)     COMP-5  VALUE 0.

      *--> Chaves para controle do balance-line
       01  CH-CTRA-OPR-0263.
           03 CD-CTRA-TIT-VL-0263      PIC S9(09)     COMP-5  VALUE 0.
           03 CD-OPR-ETRD-MCP-0263     PIC S9(11)     COMP-3  VALUE 0.

       01  CH-CTRA-OPR-0264.
           03 CD-CTRA-TIT-VL-0264       PIC S9(09)    COMP-5  VALUE 0.
           03 CD-OPR-ETRD-MCP-0264      PIC S9(11)    COMP-3  VALUE 0.

       01  CH-CTRA-OPR-0265.
           03 CD-CTRA-TIT-VL-0265       PIC S9(09)    COMP-5  VALUE 0.
           03 CD-OPR-ETRD-MCP-0265      PIC S9(11)    COMP-3  VALUE 0.

      *--> Chaves para controle da quebra para gravação de regras
       01  CH-CTRA-OPR-0263-ANT.
           03 CD-CTRA-0263-ANT         PIC S9(09)     COMP-5  VALUE 0.
           03 CD-OPR-0263-ANT          PIC S9(11)     COMP-3  VALUE 0.

      *--> Area da subrotina SBDATAF
       01  FUNCAO                        PIC  X(3) VALUE 'F01'.
       01  AREA-WORK                     PIC  X(4099).
       01  AREA-WORK-RED  REDEFINES  AREA-WORK.
           03 A-TAM                      PIC  9(0008) COMP.
           03 FILLER                     PIC  X(4071).
           03 A-FIM                      PIC  X(0020).
       01  ARG01                         PIC  9(8) VALUE 0.
       01  ARG01-R REDEFINES ARG01.
           03  DIA                       PIC  9(2).
           03  MES                       PIC  9(2).
           03  ANO                       PIC  9(4).
       01  ARG02                         PIC  9(1) VALUE 4.

      *--> Tabela de trabalho
       01 AUX-SGTS0262-CD-RGR-VLDC.
          03 AUX-TAB-CD-RGR-VLDC       PIC S9(09) COMP-5  VALUE 0
                                       OCCURS  3000 TIMES.


      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book do container de erro
-INC IIBKERRO
      *--> Book para captura de erro via MSG1
-INC MSGKWMG1
      *--> Variaveis para tratamento de Abend e Condition
-INC HLPKW999
      *--> Variaveis para tratamento de CONTAINER
-INC SGTKERR1

      *-----------------------------------------------------------------
      *    Book das Subrotinas Acionadas
      *-----------------------------------------------------------------
      *--> Consulta data para pesquisa Tabela PSC_DRIA_CTRA_TIT
       01  SGTSC003-DADOS.
-INC SGTKC003

      *--> Lista das operaçoes que contêm regras ativas gravadas.
       01  SGTS0263-DADOS.
-INC SGTK0263

      *--> Lista das operaçoes com indicador de impacto manual
       01  SGTS0264-DADOS.
-INC SGTK0264

      *--> Lista das operaçoes com indicador de flexibilização
       01  SGTS0265-DADOS.
-INC SGTK0265

      *--> Book do container de requisição e resposta deste módulo
       01 SGTS0262-DADOS.
-INC SGTK0262

       01  CTE-FIM-LS                  PIC X(30)  VALUE
                                       '*** LS TERMINA AQUI  ***'.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999

      *--> Define o nome do programa p/ fins manipulação container
           MOVE CTE-PROG(5:8)          TO NM-PGM

           PERFORM 100000-OBTER-NOME-DO-CANAL
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
           PERFORM 970000-LER-REQUISICAO-RECEBIDA

           PERFORM 140000-VALIDAR-REQUISICAO
           PERFORM 200000-PRIMEIRO-ACESSO-LISTAS

           PERFORM 300000-GRAVA-OPERACOES-EXCECAO
                   UNTIL (CH-CTRA-OPR-0263 = HIGH-VALUES
                     AND  NOT SGTS0263-TEM-MAIS-REGISTRO
                     AND  CH-CTRA-OPR-0264 = HIGH-VALUES
                     AND  NOT SGTS0264-TEM-MAIS-REGISTRO)
                      OR  SGTS0262-QT-OCR-TAB = 100

           PERFORM  VARYING IC-TAB  FROM  1  BY  1
                      UNTIL IC-TAB > SGTS0262-QT-OCR-CD-RGR
                    MOVE AUX-TAB-CD-RGR-VLDC  (IC-TAB)     TO
                         SGTS0262-CD-RGR-VLDC (IC-TAB)
           END-PERFORM

           IF  SGTS0262-QT-OCR-TAB = 100 AND
              (CH-CTRA-OPR-0263 NOT EQUAL HIGH-VALUES OR
               CH-CTRA-OPR-0264 NOT EQUAL HIGH-VALUES)
               SET SGTS0262-TEM-MAIS-REGISTRO TO TRUE
           END-IF

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

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
                CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           IF  SGTS0262-DT-PSC-E       EQUAL SPACES
               MOVE SGTSC003           TO NM-PGM
               PERFORM 900000-PROCEDIMENTOS-CONTAINER
               MOVE CORR SGTSC003-DT-PSC-R  TO SGTS0262-DT-PSC-E-R
           ELSE
               PERFORM 920000-VALIDAR-DATA-INFORMADA
           END-IF

           IF  SGTS0262-CD-CTRA-TIT-VL-E <  0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0001               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de títulos e valores mobiliários
      -        ' inválido.'            TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0262-CD-OPR-ETRD-MCP-E NOT NUMERIC OR
               SGTS0262-CD-OPR-ETRD-MCP-E < 0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0004               TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais inv
      -             'álido.'           TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> SGTS0262-CD-TIT-OPR-ETRD-E (Campo alfa, não obrigatório)

           IF  SGTS0262-CD-CLI-E < 0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0028               TO  MSG1-CD-ERRO-TS
               MOVE 'Código cliente emissor inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *---------  Valida campos de paginacao  ------------------------
      *---------- Carteira-paginacao----------------------------------
           IF   SGTS0262-CD-CTRA-TIT-VL-ULT  LESS ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *----------  Operação-paginacao--------------------------------
           IF   SGTS0262-CD-OPR-ETRD-MCP-ULT   NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF   SGTS0262-CD-OPR-ETRD-MCP-ULT    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
      *
           MOVE 'N'                    TO SGTS0262-IN-CTNC-PSQ
           MOVE SGTS0262-DT-PSC-E      TO SGTS0262-DT-PSC
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PRIMEIRO-ACESSO-LISTAS   SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTS0262-DT-PSC-E           TO AUX-DT-PSC
           MOVE SGTS0262-CD-CTRA-TIT-VL-E   TO AUX-CD-CTRA-TIT-VL
           MOVE SGTS0262-CD-OPR-ETRD-MCP-E  TO AUX-CD-OPR-ETRD-MCP
           MOVE SGTS0262-CD-TIT-OPR-ETRD-E  TO AUX-CD-TIT-OPR-ETRD
           MOVE SGTS0262-CD-CLI-E           TO AUX-CD-CLI
           MOVE SGTS0262-CD-CTRA-TIT-VL-ULT TO AUX-CD-CTRA-TIT-VL-ULT
           MOVE SGTS0262-CD-OPR-ETRD-MCP-ULT
                                            TO AUX-CD-OPR-ETRD-MCP-ULT
      *-- Verifica se está sendo feita paginação e carrega campo
      *-- código de regra com 999999 para buscar a próxima operação
      *-- da lista de regras, pois todas as regras desta operação já
      *-- foi apresentada na tela.
           IF   SGTS0262-CD-CTRA-TIT-VL-ULT > ZEROS
                MOVE 99999999               TO AUX-CD-RGR-VLDC-ULT
           ELSE
                MOVE 0                      TO AUX-CD-RGR-VLDC-ULT
           END-IF

           PERFORM 976000-RECUPERA-LISTA-RGR
           PERFORM 977000-RECUPERA-LISTA-IFLC-MNL
           PERFORM 978000-RECUPERA-LISTA-FLZ

      *--> Leitura do primeiro item da lista regras ativas gravadas
           IF SGTS0263-QT-OCR-TAB = 0
              MOVE HIGH-VALUES         TO  CH-CTRA-OPR-0263
           ELSE
              PERFORM 973000-LER-LISTA-REGRAS-0263
           END-IF

      *--> Leitura do primeiro item da lista indicadores impacto manual
           IF SGTS0264-QT-OCR-TAB = 0
              MOVE HIGH-VALUES         TO  CH-CTRA-OPR-0264
           ELSE
              PERFORM 974000-LER-LISTA-IFLC-MNL-0264
           END-IF

      *--> Leitura do primeiro item da lista indicadores flexibilização
           IF SGTS0265-QT-OCR-TAB = 0
              MOVE HIGH-VALUES         TO  CH-CTRA-OPR-0265
           ELSE
              PERFORM 975000-LER-LISTA-FLZ-0265
           END-IF
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-GRAVA-OPERACOES-EXCECAO  SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           IF  CH-CTRA-OPR-0263    <  CH-CTRA-OPR-0264
               PERFORM 310000-GRV-RGR-ATIVAS-SEM-IFLC
           ELSE
             IF CH-CTRA-OPR-0263   >  CH-CTRA-OPR-0264
                PERFORM 320000-IFLC-MNL-X-FLZ    UNTIL
                        CH-CTRA-OPR-0263 NOT > CH-CTRA-OPR-0264
             ELSE
                PERFORM 330000-GRAVA-RGR-ATIVAS-C-IFLC
             END-IF
           END-IF
           .
       300000-SAI.
           EXIT.

      *---------------------(0263  <  0264)-----------------------------
      *-----------------------------------------------------------------
       310000-GRV-RGR-ATIVAS-SEM-IFLC  SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           ADD 1                       TO SGTS0262-QT-OCR-TAB
           MOVE CH-CTRA-OPR-0263       TO CH-CTRA-OPR-0263-ANT

           MOVE SGTS0262-QT-OCR-CD-RGR TO
                SGTS0262-OCR-INC-CD-RGR     (SGTS0262-QT-OCR-TAB)
           ADD 1                       TO
                SGTS0262-OCR-INC-CD-RGR     (SGTS0262-QT-OCR-TAB)

           MOVE CD-CTRA-TIT-VL-0263    TO
                SGTS0262-CD-CTRA-TIT-VL     (SGTS0262-QT-OCR-TAB)

           MOVE CD-OPR-ETRD-MCP-0263   TO
                SGTS0262-CD-OPR-ETRD-MCP     (SGTS0262-QT-OCR-TAB)

           MOVE SGTS0263-NR-VRS-OPR-ETRD-S   (IC-TAB-0263) TO
                SGTS0262-NR-VRS-OPR-ETRD     (SGTS0262-QT-OCR-TAB)

           MOVE SGTS0263-CD-TIT-OPR-ETRD-S   (IC-TAB-0263) TO
                SGTS0262-CD-TIT-OPR-ETRD     (SGTS0262-QT-OCR-TAB)

           MOVE SGTS0263-CD-CLI-S            (IC-TAB-0263) TO
                SGTS0262-CD-CLI              (SGTS0262-QT-OCR-TAB)

           MOVE SGTS0263-VL-CPR-OPR-S        (IC-TAB-0263) TO
                SGTS0262-VL-CPR              (SGTS0262-QT-OCR-TAB)

           MOVE SGTS0263-VL-SDO-TIT-DT-PSC-S (IC-TAB-0263) TO
                SGTS0262-VL-SDO-TIT-DT-PSC   (SGTS0262-QT-OCR-TAB)

           MOVE ' '                    TO
                SGTS0262-IN-IFLC-MNL         (SGTS0262-QT-OCR-TAB)

           PERFORM 311000-VER-FLEXIBILIZACAO
           MOVE AUX-IN-FLZ             TO
                SGTS0262-IN-FLZ              (SGTS0262-QT-OCR-TAB)

      *--> Gravar regras até quebra da CH-CTRA-OPR-0263
           PERFORM 312000-GRAVA-REGRAS-0263
                   UNTIL CH-CTRA-OPR-0263 NOT < CH-CTRA-OPR-0264
                      OR CH-CTRA-OPR-0263 NOT = CH-CTRA-OPR-0263-ANT

           MOVE SGTS0262-QT-OCR-CD-RGR TO
                SGTS0262-OCR-FIM-CD-RGR      (SGTS0262-QT-OCR-TAB)
           .
       310000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       311000-VER-FLEXIBILIZACAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE ' '                    TO AUX-IN-FLZ
           IF CH-CTRA-OPR-0263    >    CH-CTRA-OPR-0265
              PERFORM 975000-LER-LISTA-FLZ-0265
                 PERFORM UNTIL
                         CH-CTRA-OPR-0263 NOT > CH-CTRA-OPR-0265
                         PERFORM 975000-LER-LISTA-FLZ-0265
                END-PERFORM
           END-IF

           IF CH-CTRA-OPR-0263    =    CH-CTRA-OPR-0265
              MOVE 'S'                 TO AUX-IN-FLZ
           END-IF
           .
       311000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       312000-GRAVA-REGRAS-0263        SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           ADD 1                       TO SGTS0262-QT-OCR-CD-RGR

           MOVE SGTS0263-CD-RGR-VLDC-S (IC-TAB-0263)  TO
                AUX-TAB-CD-RGR-VLDC    (SGTS0262-QT-OCR-CD-RGR)

           PERFORM 973000-LER-LISTA-REGRAS-0263
           .
       312000-SAI.
           EXIT.

      *---------------------(0263  >  0264)-----------------------------
      *-----------------------------------------------------------------
       320000-IFLC-MNL-X-FLZ           SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           MOVE ' '                    TO AUX-IN-FLZ

           IF CH-CTRA-OPR-0264    <  CH-CTRA-OPR-0265
      *-----> MARCA IMPACTO MANUAL P/ CH-CTRA-OPR-0264
              PERFORM 3210000-GRAVA-IFLC-MNL-0264
              PERFORM 974000-LER-LISTA-IFLC-MNL-0264
           ELSE
      *-----> Despreza CH-CTRA-OPR-0265
             IF CH-CTRA-OPR-0264  >  CH-CTRA-OPR-0265
                PERFORM 975000-LER-LISTA-FLZ-0265
             ELSE
      *-------> MARCA IMPACTO MANUAL P/ CH-CTRA-OPR-0264
      *-------> MARCA FLEXIBILIZAÇAO P/ CH-CTRA-OPR-0265
                MOVE 'S'               TO AUX-IN-FLZ
                PERFORM 3210000-GRAVA-IFLC-MNL-0264
                PERFORM 974000-LER-LISTA-IFLC-MNL-0264
                PERFORM 975000-LER-LISTA-FLZ-0265
             END-IF
           END-IF
           .
       320000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       3210000-GRAVA-IFLC-MNL-0264     SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           ADD 1                       TO SGTS0262-QT-OCR-TAB

           MOVE CD-CTRA-TIT-VL-0264    TO
                SGTS0262-CD-CTRA-TIT-VL      (SGTS0262-QT-OCR-TAB)

           MOVE CD-OPR-ETRD-MCP-0264   TO
                SGTS0262-CD-OPR-ETRD-MCP     (SGTS0262-QT-OCR-TAB)

           MOVE SGTS0264-NR-VRS-OPR-ETRD     (IC-TAB-0264) TO
                SGTS0262-NR-VRS-OPR-ETRD     (SGTS0262-QT-OCR-TAB)

           MOVE SGTS0264-CD-TIT-OPR-ETRD     (IC-TAB-0264) TO
                SGTS0262-CD-TIT-OPR-ETRD     (SGTS0262-QT-OCR-TAB)

           MOVE SGTS0264-CD-CLI              (IC-TAB-0264) TO
                SGTS0262-CD-CLI              (SGTS0262-QT-OCR-TAB)

           MOVE 0                      TO
                SGTS0262-OCR-INC-CD-RGR      (SGTS0262-QT-OCR-TAB)

           MOVE 0                      TO
                SGTS0262-OCR-FIM-CD-RGR      (SGTS0262-QT-OCR-TAB)

           MOVE SGTS0264-VL-CPR              (IC-TAB-0264) TO
                SGTS0262-VL-CPR              (SGTS0262-QT-OCR-TAB)

           MOVE SGTS0264-VL-SDO-TIT-DT-PSC   (IC-TAB-0264) TO
                SGTS0262-VL-SDO-TIT-DT-PSC   (SGTS0262-QT-OCR-TAB)

           MOVE 'S'                    TO
                SGTS0262-IN-IFLC-MNL         (SGTS0262-QT-OCR-TAB)

           MOVE AUX-IN-FLZ             TO
                SGTS0262-IN-FLZ              (SGTS0262-QT-OCR-TAB)
           .
       321000-SAI.
           EXIT.

      *---------------------(0263  =  0264)-----------------------------
      *-----------------------------------------------------------------
       330000-GRAVA-RGR-ATIVAS-C-IFLC      SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           ADD 1                       TO SGTS0262-QT-OCR-TAB

           MOVE SGTS0262-QT-OCR-CD-RGR TO
                SGTS0262-OCR-INC-CD-RGR      (SGTS0262-QT-OCR-TAB)
           ADD 1                       TO
                SGTS0262-OCR-INC-CD-RGR      (SGTS0262-QT-OCR-TAB)

           MOVE CD-CTRA-TIT-VL-0263    TO
                SGTS0262-CD-CTRA-TIT-VL      (SGTS0262-QT-OCR-TAB)

           MOVE CD-OPR-ETRD-MCP-0263   TO
                SGTS0262-CD-OPR-ETRD-MCP     (SGTS0262-QT-OCR-TAB)

           MOVE SGTS0263-NR-VRS-OPR-ETRD-S     (IC-TAB-0263) TO
                SGTS0262-NR-VRS-OPR-ETRD     (SGTS0262-QT-OCR-TAB)

           MOVE SGTS0263-CD-TIT-OPR-ETRD-S   (IC-TAB-0263) TO
                SGTS0262-CD-TIT-OPR-ETRD     (SGTS0262-QT-OCR-TAB)

           MOVE SGTS0263-CD-CLI-S            (IC-TAB-0263) TO
                SGTS0262-CD-CLI              (SGTS0262-QT-OCR-TAB)

           MOVE SGTS0263-VL-CPR-OPR-S        (IC-TAB-0263) TO
                SGTS0262-VL-CPR              (SGTS0262-QT-OCR-TAB)

           MOVE SGTS0263-VL-SDO-TIT-DT-PSC-S (IC-TAB-0263) TO
                SGTS0262-VL-SDO-TIT-DT-PSC   (SGTS0262-QT-OCR-TAB)

           MOVE 'S'                    TO
                SGTS0262-IN-IFLC-MNL         (SGTS0262-QT-OCR-TAB)

           MOVE ' '                    TO
              SGTS0262-IN-FLZ                (SGTS0262-QT-OCR-TAB)

      *--->Verificar se as regras ativas com impacto, têm flexibilização
           IF CH-CTRA-OPR-0264    >  CH-CTRA-OPR-0265
              PERFORM 975000-LER-LISTA-FLZ-0265
              PERFORM UNTIL
                      CH-CTRA-OPR-0264  NOT > CH-CTRA-OPR-0265
                      PERFORM 975000-LER-LISTA-FLZ-0265
              END-PERFORM
           END-IF

           IF CH-CTRA-OPR-0264    =  CH-CTRA-OPR-0265
              MOVE 'S'                 TO
                   SGTS0262-IN-FLZ           (SGTS0262-QT-OCR-TAB)
              PERFORM 975000-LER-LISTA-FLZ-0265
           END-IF

           PERFORM 331000-GRAVA-REGRAS-0263
                   UNTIL CH-CTRA-OPR-0263 NOT = CH-CTRA-OPR-0264

           MOVE SGTS0262-QT-OCR-CD-RGR TO
                SGTS0262-OCR-FIM-CD-RGR      (SGTS0262-QT-OCR-TAB)

           PERFORM 974000-LER-LISTA-IFLC-MNL-0264
           .
       330000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       331000-GRAVA-REGRAS-0263        SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

           ADD 1                       TO SGTS0262-QT-OCR-CD-RGR

           MOVE SGTS0263-CD-RGR-VLDC-S (IC-TAB-0263)  TO
                AUX-TAB-CD-RGR-VLDC    (SGTS0262-QT-OCR-CD-RGR)

           PERFORM 973000-LER-LISTA-REGRAS-0263
           .
       331000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       900000-PROCEDIMENTOS-CONTAINER  SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

      *--> Executar Módulos com container, sem Serviço
           EVALUATE NM-PGM
      *--> Executar Módulo SGTSC003
                  WHEN SGTSC003
                    EXEC CICS
                         LINK PROGRAM ( SGTSC003  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC

      *--> Executar Módulo SGTS0263
                  WHEN SGTS0263
                    EXEC CICS
                         LINK PROGRAM ( SGTS0263  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC

      *--> Executar Módulo SGTS0264
                  WHEN SGTS0264
                    EXEC CICS
                         LINK PROGRAM ( SGTS0264  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC

      *--> Executar Módulo SGTS0265
                  WHEN SGTS0265
                    EXEC CICS
                         LINK PROGRAM ( SGTS0265  )
                              CHANNEL ( NM-CNL    )
                              RESP    ( MSG1-RESP1)
                              RESP2   ( MSG1-RESP2)
                    END-EXEC

      *--> Execução de Módulo não prevista
                  WHEN OTHER
                    SET ERRO-PROGRAMADO     TO TRUE
                    MOVE 9006               TO MSG1-CD-ERRO-TS
                    STRING 'Não definida a execução do módulo '
                            NM-PGM      '.'
                            DELIMITED BY SIZE
                            INTO          MSG1-TX-ERRO-TS
                    END-STRING
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           EVALUATE TRUE
               WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    PERFORM 911000-VER-ERRO-MODULO-CHAMADO
           END-EVALUATE
           .
       910000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       911000-VER-ERRO-MODULO-CHAMADO  SECTION.
      *----------------------------------------------------------------*
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

      *--> Análise do retorno das chamadas dos programas
      *--> Demais Módulos chamados com CONTAINER, sem serviço
           SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
           MOVE  3004                  TO  MSG1-CD-ERRO-TS
           STRING  'Erro ao acionar o módulo ' NM-PGM      '.'
                   'RESP  = '    MSG1-RESP1  ', '
                   'RESP2 = '    MSG1-RESP2
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
           PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           .
       911000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       920000-VALIDAR-DATA-INFORMADA   SECTION.
      *-----------------------------------------------------------------
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

           MOVE 4095                   TO A-TAM
           MOVE 'SBDATAF -WRKAREA-FIM' TO A-FIM

           MOVE  CORR SGTS0262-DT-PSC-E-R
                                       TO ARG01-R

           CALL SBDATAF                USING FUNCAO AREA-WORK ARG01
                                             ARG02
           IF  RETURN-CODE             NOT EQUAL  ZEROS  AND
                                       0022  AND  0024   AND 0046
               SET  ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               MOVE SBDATAF            TO NM-PGM
               MOVE 0051               TO MSG1-CD-ERRO-TS
               MOVE 'Erro no tratamento de datas.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  RETURN-CODE          EQUAL 022 OR 0024 OR 0046
               SET  ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               MOVE SBDATAF            TO NM-PGM
               MOVE 0049               TO MSG1-CD-ERRO-TS
               MOVE 'Data informada inválida.'
                                       TO MSG1-TX-ERRO-TS
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       920000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970000-LER-REQUISICAO-RECEBIDA  SECTION.
      *-----------------------------------------------------------------
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

           MOVE 'SGTS0262-RQSC'        TO  NM-CTNR

           PERFORM 972100-LER-CONTAINER-NODATA

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO  TRUE
      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS
                GET CONTAINER ( NM-CTNR       )
                    FLENGTH   ( QT-CCT-LIDO   )
                    INTO      ( SGTS0262-RQSC )
           END-EXEC

      *-->Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       970000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       971000-LER-ERRO                 SECTION.
      *----------------------------------------------------------------*
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

      *----------------------------------------------------------------*
      *    A presença do container PRM-ERRO indica que houve erro de   *
      *    integração, a ausência do container indica sucesso.         *
      *----------------------------------------------------------------*

      *--> Configura dados para leitura de container de erro
           MOVE CTNR-PRM-ERRO          TO NM-CTNR
           SET  GET-CONTAINER          TO TRUE
           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Não há captura automática de erro CICS
      *--> Leitura do container de erro
           EXEC CICS GET
                     CONTAINER ( NM-CTNR                     )
                     INTO      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC

      *==> Observação <=================================================
      *        Caso deseje personalizar ou não interromper o
      *        processamento, alterar para não dar erro aqui SOMENTE
      *        PARA O MÓDULO CHAMADO e tratar na section chamadora
      *=================================================================
           EVALUATE  TRUE
               WHEN  MSG1-RESP1           EQUAL  DFHRESP(CONTAINERERR)
                     CONTINUE
               WHEN  MSG1-RESP1           EQUAL  DFHRESP(NORMAL)
                     SET   ERRO-PROGRAMA-ACIONADO
                                          TO  TRUE
                IF NM-PGM(1:3)            EQUAL 'SGT'
                  MOVE  IIBKERRO-CD-ERRO  TO  MSG1-CD-RTN-PGM-ACND
                                              MSG1-CD-ERRO-TS
                  MOVE  IIBKERRO-TX-ERRO  TO  MSG1-TX-RTN-PGM-ACND
                                              MSG1-TX-ERRO-TS
                ELSE
                  MOVE 3004               TO  MSG1-CD-ERRO-TS
                  STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                          DELIMITED BY SIZE
                               INTO           MSG1-TX-ERRO-TS
                   MOVE IIBKERRO-TX-ERRO  TO  MSG1-TX-RTN-PGM-ACND
                   MOVE IIBKERRO-CD-ERRO  TO  MSG1-CD-RTN-PGM-ACND
                END-IF
                     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               WHEN  OTHER
                     SET   ERRO-CONTAINER TO  TRUE
                     PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-RESPOSTA             SECTION.
      *-----------------------------------------------------------------
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

      *--> Trata área de resposta do programa acionado (NODATA)
           PERFORM 972100-LER-CONTAINER-NODATA

      *--> Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *--> Por causa da captura automática erro CICS, deve ser previsto
      *--> o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Recupera o container de resposta do programa que foi acionado
           EVALUATE NM-PGM

      *--> Lê container de resposta do SGTSC003
               WHEN SGTSC003
                 MOVE LENGTH OF SGTSC003-RPST TO QT-CCT-EPRO
                 EXEC CICS
                      GET  CONTAINER ( NM-CTNR       )
                           INTO      ( SGTSC003-RPST )
                           FLENGTH   ( QT-CCT-LIDO   )
                 END-EXEC

      *--> Lê container de resposta do SGTS0263
               WHEN SGTS0263
                 MOVE LENGTH OF SGTS0263-RPST TO QT-CCT-EPRO
                 EXEC CICS
                      GET  CONTAINER ( NM-CTNR        )
                           INTO      ( SGTS0263-RPST  )
                           FLENGTH   ( QT-CCT-LIDO    )
                 END-EXEC

      *--> Lê container de resposta do SGTS0264
               WHEN SGTS0264
                 MOVE LENGTH OF SGTS0264-RPST TO QT-CCT-EPRO
                 EXEC CICS
                      GET  CONTAINER ( NM-CTNR        )
                           INTO      ( SGTS0264-RPST  )
                           FLENGTH   ( QT-CCT-LIDO    )
                 END-EXEC

      *--> Lê container de resposta do SGTS0265
               WHEN SGTS0265
                 MOVE LENGTH OF SGTS0265-RPST TO QT-CCT-EPRO
                 EXEC CICS
                      GET  CONTAINER ( NM-CTNR        )
                           INTO      ( SGTS0265-RPST  )
                           FLENGTH   ( QT-CCT-LIDO    )
                 END-EXEC

      *--> Módulo não previsto para leitura do container
               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9003         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na leitura da resposta do módulo '
                             NM-PGM      '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                    END-STRING
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------
           MOVE 0020                   TO MSG1-CD-SEQL-ERRO

      *--> Recupera o tamanho previsto e o tamanho recebido da resposta
      *--> do programa que foi acionado.

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Obtém a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO

           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       972100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973000-LER-LISTA-REGRAS-0263    SECTION.
      *-----------------------------------------------------------------
           MOVE 0021                   TO MSG1-CD-SEQL-ERRO

           ADD 1                       TO IC-TAB-0263

           IF  IC-TAB-0263 > SGTS0263-QT-OCR-TAB
               IF SGTS0263-TEM-MAIS-REGISTRO
                  PERFORM 973100-PAGINA-REGRAS
               ELSE
                  MOVE HIGH-VALUES     TO  CH-CTRA-OPR-0263
               END-IF
           ELSE
               MOVE SGTS0263-CD-CTRA-TIT-VL-S  (IC-TAB-0263)
                                       TO CD-CTRA-TIT-VL-0263
               MOVE SGTS0263-CD-OPR-ETRD-MCP-S (IC-TAB-0263)
                                       TO CD-OPR-ETRD-MCP-0263
           END-IF
           .
       973000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973100-PAGINA-REGRAS            SECTION.
      *-----------------------------------------------------------------
           MOVE 0022                   TO MSG1-CD-SEQL-ERRO

           SUBTRACT 1 FROM IC-TAB-0263

      *-- Campos do filtro permanecem os mesmos da 1a chamada da lista.

           MOVE SGTS0263-CD-CTRA-TIT-VL-S   (IC-TAB-0263)
                                            TO AUX-CD-CTRA-TIT-VL-ULT
           MOVE SGTS0263-CD-OPR-ETRD-MCP-S  (IC-TAB-0263)
                                            TO AUX-CD-OPR-ETRD-MCP-ULT
           MOVE SGTS0263-CD-RGR-VLDC-S      (IC-TAB-0263)
                                            TO AUX-CD-RGR-VLDC-ULT
           PERFORM 976000-RECUPERA-LISTA-RGR

           MOVE 1                      TO   IC-TAB-0263
           MOVE SGTS0263-CD-CTRA-TIT-VL-S  (IC-TAB-0263)
                                       TO CD-CTRA-TIT-VL-0263
           MOVE SGTS0263-CD-OPR-ETRD-MCP-S (IC-TAB-0263)
                                       TO CD-OPR-ETRD-MCP-0263
           .
       973100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       974000-LER-LISTA-IFLC-MNL-0264  SECTION.
      *-----------------------------------------------------------------
           MOVE 0023                   TO MSG1-CD-SEQL-ERRO

           ADD 1                       TO IC-TAB-0264

           IF  IC-TAB-0264 > SGTS0264-QT-OCR-TAB
               IF SGTS0264-TEM-MAIS-REGISTRO
                  PERFORM 974100-PAGINA-IFLC-MNL
               ELSE
                  MOVE HIGH-VALUES     TO  CH-CTRA-OPR-0264
               END-IF
           ELSE
               MOVE SGTS0264-CD-CTRA-TIT-VL  (IC-TAB-0264)
                                       TO CD-CTRA-TIT-VL-0264
               MOVE SGTS0264-CD-OPR-ETRD-MCP (IC-TAB-0264)
                                       TO CD-OPR-ETRD-MCP-0264
           END-IF
           .
       974000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       974100-PAGINA-IFLC-MNL          SECTION.
      *-----------------------------------------------------------------
           MOVE 0024                   TO MSG1-CD-SEQL-ERRO

           SUBTRACT 1 FROM IC-TAB-0264

      *-- Campos do filtro permanecem os mesmos da 1a chamada da lista.

           MOVE SGTS0264-CD-CTRA-TIT-VL     (IC-TAB-0264)
                                            TO AUX-CD-CTRA-TIT-VL-ULT
           MOVE SGTS0264-CD-OPR-ETRD-MCP    (IC-TAB-0264)
                                            TO AUX-CD-OPR-ETRD-MCP-ULT

           PERFORM 977000-RECUPERA-LISTA-IFLC-MNL

           MOVE 1                      TO IC-TAB-0264
           MOVE SGTS0264-CD-CTRA-TIT-VL  (IC-TAB-0264)
                                       TO CD-CTRA-TIT-VL-0264
           MOVE SGTS0264-CD-OPR-ETRD-MCP (IC-TAB-0264)
                                       TO CD-OPR-ETRD-MCP-0264
           .
       974100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       975000-LER-LISTA-FLZ-0265       SECTION.
      *-----------------------------------------------------------------
           MOVE 0025                   TO MSG1-CD-SEQL-ERRO

           ADD 1                       TO IC-TAB-0265

           IF  IC-TAB-0265 > SGTS0265-QT-OCR-TAB
               IF SGTS0265-TEM-MAIS-REGISTRO
                  PERFORM 975100-PAGINA-FLZ
               ELSE
                   MOVE HIGH-VALUES    TO  CH-CTRA-OPR-0265
               END-IF
           ELSE
               MOVE SGTS0265-CD-CTRA-TIT-VL-S  (IC-TAB-0265)
                                       TO CD-CTRA-TIT-VL-0265
               MOVE SGTS0265-CD-OPR-ETRD-MCP-S (IC-TAB-0265)
                                       TO CD-OPR-ETRD-MCP-0265
           END-IF
           .
       975000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       975100-PAGINA-FLZ               SECTION.
      *-----------------------------------------------------------------
           MOVE 0026                   TO MSG1-CD-SEQL-ERRO

           SUBTRACT     1    FROM      IC-TAB-0265

      *-- Campos do filtro permanecem os mesmos da 1a chamada da lista.

           MOVE SGTS0265-CD-CTRA-TIT-VL-S   (IC-TAB-0265)
                                            TO AUX-CD-CTRA-TIT-VL-ULT
           MOVE SGTS0265-CD-OPR-ETRD-MCP-S  (IC-TAB-0265)
                                            TO AUX-CD-OPR-ETRD-MCP-ULT

           PERFORM 978000-RECUPERA-LISTA-FLZ

           MOVE 1                      TO IC-TAB-0265
           MOVE SGTS0265-CD-CTRA-TIT-VL-S   (IC-TAB-0265)
                                       TO CD-CTRA-TIT-VL-0265
           MOVE SGTS0265-CD-OPR-ETRD-MCP-S  (IC-TAB-0265)
                                       TO CD-OPR-ETRD-MCP-0265
           .
       975100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       976000-RECUPERA-LISTA-RGR       SECTION.
      *-----------------------------------------------------------------
      *--> Lista das operaçoes que contêm regras ativas gravadas
           MOVE 0027                   TO MSG1-CD-SEQL-ERRO

      *-- Campos do filtro permanecem os mesmos da 1a chamada da lista.
           MOVE AUX-DT-PSC             TO SGTS0263-DT-PSC-E
           MOVE AUX-CD-CTRA-TIT-VL     TO SGTS0263-CD-CTRA-TIT-VL-E
           MOVE AUX-CD-OPR-ETRD-MCP    TO SGTS0263-CD-OPR-ETRD-MCP-E
           MOVE AUX-CD-TIT-OPR-ETRD    TO SGTS0263-CD-TIT-OPR-ETRD-E
           MOVE AUX-CD-CLI             TO SGTS0263-CD-CLI-E

           MOVE AUX-CD-RGR-VLDC-ULT    TO SGTS0263-CD-RGR-VLDC-ULT
           MOVE AUX-CD-CTRA-TIT-VL-ULT TO SGTS0263-CD-CTRA-TIT-VL-ULT
           MOVE AUX-CD-OPR-ETRD-MCP-ULT TO SGTS0263-CD-OPR-ETRD-MCP-ULT

           MOVE SGTS0263               TO NM-PGM
           PERFORM 900000-PROCEDIMENTOS-CONTAINER
           .
       976000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       977000-RECUPERA-LISTA-IFLC-MNL  SECTION.
      *-----------------------------------------------------------------
      *--> Lista das operaçoes com indicador de impacto manual
           MOVE 0028                   TO MSG1-CD-SEQL-ERRO

      *-- Campos do filtro permanecem os mesmos da 1a chamada da lista.
           MOVE AUX-DT-PSC             TO SGTS0264-DT-PSC-E
           MOVE AUX-CD-CTRA-TIT-VL     TO SGTS0264-CD-CTRA-TIT-VL-E
           MOVE AUX-CD-OPR-ETRD-MCP    TO SGTS0264-CD-OPR-ETRD-MCP-E
           MOVE AUX-CD-TIT-OPR-ETRD    TO SGTS0264-CD-TIT-OPR-ETRD-E
           MOVE AUX-CD-CLI             TO SGTS0264-CD-CLI-E

           MOVE AUX-CD-CTRA-TIT-VL-ULT TO SGTS0264-CD-CTRA-TIT-VL-ULT
           MOVE AUX-CD-OPR-ETRD-MCP-ULT TO SGTS0264-CD-OPR-ETRD-MCP-ULT

           MOVE SGTS0264               TO NM-PGM
           PERFORM 900000-PROCEDIMENTOS-CONTAINER
           .
       977000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       978000-RECUPERA-LISTA-FLZ       SECTION.
      *-----------------------------------------------------------------
      *--> Lista das operaçoes com indicador de flexibilização
           MOVE 0029                   TO MSG1-CD-SEQL-ERRO

      *-- Campos do filtro permanecem os mesmos da 1a chamada da lista.
           MOVE AUX-DT-PSC             TO SGTS0265-DT-PSC-E
           MOVE AUX-CD-CTRA-TIT-VL     TO SGTS0265-CD-CTRA-TIT-VL-E
           MOVE AUX-CD-OPR-ETRD-MCP    TO SGTS0265-CD-OPR-ETRD-MCP-E
           MOVE AUX-CD-TIT-OPR-ETRD    TO SGTS0265-CD-TIT-OPR-ETRD-E
           MOVE AUX-CD-CLI             TO SGTS0265-CD-CLI-E

           MOVE AUX-CD-CTRA-TIT-VL-ULT TO SGTS0265-CD-CTRA-TIT-VL-ULT
           MOVE AUX-CD-OPR-ETRD-MCP-ULT TO SGTS0265-CD-OPR-ETRD-MCP-ULT

           MOVE SGTS0265               TO NM-PGM
           PERFORM 900000-PROCEDIMENTOS-CONTAINER
           .
       978000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980000-GRAVAR-CTNR-RESPOSTA     SECTION.
      *-----------------------------------------------------------------
           MOVE 0030                   TO MSG1-CD-SEQL-ERRO

      *--> Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

      *--> Monta o nome do container de resposta
           MOVE 'SGTS0262-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS
                PUT  CONTAINER ( NM-CTNR                 )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTS0262-RPST )
                     FROM      ( SGTS0262-RPST           )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       982000-GRAVA-REQUISICAO         SECTION.
      *-----------------------------------------------------------------
           MOVE 0031                   TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para gravação do container de requisição
           SET  PUT-CONTAINER          TO TRUE

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *--> nome_modulo -RQSC
           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RQSC'                TO NM-CTNR(9:5)

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Grava container da requisição do serviço IIB a ser acionado
      *--> Em caso de erro, a captura automática do CICS trata o erro

           EVALUATE NM-PGM
      *--> SGTSC003 não tem container de requisição
               WHEN SGTSC003
                    CONTINUE

      *--> Grava container de requisição do programa SGTS0263
               WHEN SGTS0263
                  EXEC CICS
                       PUT  CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTS0263-RQSC           )
                            FLENGTH   ( LENGTH OF SGTS0263-RQSC )
                  END-EXEC

      *--> Grava container de requisição do programa SGTS0264
               WHEN SGTS0264
                  EXEC CICS
                       PUT  CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTS0264-RQSC           )
                            FLENGTH   ( LENGTH OF SGTS0264-RQSC )
                  END-EXEC

      *--> Grava container de requisição do programa SGTS0265
               WHEN SGTS0265
                  EXEC CICS
                       PUT  CONTAINER ( NM-CTNR                 )
                            FROM      ( SGTS0265-RQSC           )
                            FLENGTH   ( LENGTH OF SGTS0265-RQSC )
                  END-EXEC

      *--> Módulo não previsto para gravar container de requisição
               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9004         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na gravação da requisição do módulo.'
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                    END-STRING
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
           MOVE 0032                   TO MSG1-CD-SEQL-ERRO
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *--> nome_modulo -RPST

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

           SET DEL-CONTAINER           TO TRUE

      *--> Exclui o container de resposta
      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS
                DELETE CONTAINER (NM-CTNR   )
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
           MOVE 0033                   TO MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE CTNR-PRM-ERRO          TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS
                DELETE CONTAINER (NM-CTNR   )
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

           IF  ERRO-PROGRAMA-ACIONADO
               PERFORM 999100-MONITORAR-VRV-PGM-CHMD
           END-IF

           PERFORM 999200-MONITORAR-VRV-PGM-E-ENC
           .
       999000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999100-MONITORAR-VRV-PGM-CHMD   SECTION.
      *-----------------------------------------------------------------
      *--> Monitora as variáveis dos módulos chamados

           EVALUATE  NM-PGM

      *-->>SGTSC003<<
               WHEN  SGTSC003
               MOVE 'SGTSC003-DT-PSC'            TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTSC003-DT-PSC             TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTS0263<<
               WHEN  SGTS0263
               MOVE 'SGTS0263-DT-PSC-E'          TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0263-DT-PSC-E           TO MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0263-CD-CTRA-TIT-VL-E'  TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0263-CD-CTRA-TIT-VL-E   TO MCR-S9-15
               MOVE  MCR-S9-15                   TO MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0263-CD-OPR-ETRD-MCP-E' TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
               IF  SGTS0263-CD-OPR-ETRD-MCP-E    NUMERIC
                   MOVE  SGTS0263-CD-OPR-ETRD-MCP-E
                                       TO  MCR-S9-15
                   MOVE  MCR-S9-15     TO  MSGKW999-VL-VRV-MNT
               ELSE
                   MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
               END-IF
               PERFORM  999003-ADICIONA-VRV-MNT

      *        MOVE 'SGTS0263-NR-VRS-OPR-ETRD-E' TO  MSGKW999-NM-VRV-MNT
      *        MOVE  SGTS0263-NR-VRS-OPR-ETRD-E  TO MCR-S9-15
      *        MOVE  MCR-S9-15                   TO  MSGKW999-VL-VRV-MNT
      *        PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0263-CD-TIT-OPR-ETRD-E' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0263-CD-TIT-OPR-ETRD-E  TO MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0263-CD-CLI-E'          TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS0263-CD-CLI-E           TO MCR-S9-15
               MOVE  MCR-S9-15                   TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0263-CD-RGR-VLDC-ULT'   TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS0263-CD-RGR-VLDC-ULT    TO MCR-S9-15
               MOVE  MCR-S9-15                   TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

      *--> Paginacao
               MOVE 'SGTS0263-CD-CTRA-TIT-VL-ULT' TO MSGKW999-NM-VRV-MNT
               MOVE SGTS0263-CD-CTRA-TIT-VL-ULT   TO MCR-S9-15
               MOVE MCR-S9-15                     TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0263-CD-OPR-ETRD-MCP-ULT'
                                                  TO MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
               IF  SGTS0263-CD-OPR-ETRD-MCP-ULT IS NUMERIC
                   MOVE  SGTS0263-CD-OPR-ETRD-MCP-ULT TO  MCR-S9-15
                   MOVE  MCR-S9-15      TO  MSGKW999-VL-VRV-MNT
               ELSE
                   MOVE 'Valor não numérico'     TO  MSGKW999-VL-VRV-MNT
               END-IF
                   PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTS0264<<
               WHEN  SGTS0264
               MOVE 'SGTS0264-DT-PSC-E'          TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0264-DT-PSC-E           TO MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0264-CD-CTRA-TIT-VL-E'  TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0264-CD-CTRA-TIT-VL-E   TO MCR-S9-15
               MOVE  MCR-S9-15                   TO MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0264-CD-OPR-ETRD-MCP-E' TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
               IF  SGTS0264-CD-OPR-ETRD-MCP-E    NUMERIC
                   MOVE  SGTS0264-CD-OPR-ETRD-MCP-E
                                       TO  MCR-S9-15
                   MOVE  MCR-S9-15     TO  MSGKW999-VL-VRV-MNT
               ELSE
                   MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
               END-IF
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0264-CD-TIT-OPR-ETRD-E' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0264-CD-TIT-OPR-ETRD-E  TO MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0264-CD-CLI-E'          TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS0264-CD-CLI-E           TO MCR-S9-15
               MOVE  MCR-S9-15                   TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT
      *--> Paginacao
               MOVE 'SGTS0264-CD-CTRA-TIT-VL-ULT' TO MSGKW999-NM-VRV-MNT
               MOVE SGTS0264-CD-CTRA-TIT-VL-ULT   TO MCR-S9-15
               MOVE MCR-S9-15                     TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0264-CD-OPR-ETRD-MCP-ULT'
                                                  TO MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
               IF  SGTS0264-CD-OPR-ETRD-MCP-ULT IS NUMERIC
                   MOVE  SGTS0264-CD-OPR-ETRD-MCP-ULT TO  MCR-S9-15
                   MOVE  MCR-S9-15      TO  MSGKW999-VL-VRV-MNT
               ELSE
                   MOVE 'Valor não numérico'     TO  MSGKW999-VL-VRV-MNT
               END-IF
                   PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTS0265<<
               WHEN  SGTS0265
               MOVE 'SGTS0265-DT-PSC-E'          TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0265-DT-PSC-E           TO MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0265-CD-CTRA-TIT-VL-E'  TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0265-CD-CTRA-TIT-VL-E   TO MCR-S9-15
               MOVE  MCR-S9-15                   TO MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0265-CD-OPR-ETRD-MCP-E' TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
               IF  SGTS0265-CD-OPR-ETRD-MCP-E    NUMERIC
                   MOVE  SGTS0265-CD-OPR-ETRD-MCP-E
                                       TO  MCR-S9-15
                   MOVE  MCR-S9-15     TO  MSGKW999-VL-VRV-MNT
               ELSE
                   MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
               END-IF
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0265-CD-TIT-OPR-ETRD-E' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0265-CD-TIT-OPR-ETRD-E  TO MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0265-CD-CLI-E'          TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS0265-CD-CLI-E           TO MCR-S9-15
               MOVE  MCR-S9-15                   TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

      *--> Paginacao
               MOVE 'SGTS0265-CD-CTRA-TIT-VL-ULT' TO MSGKW999-NM-VRV-MNT
               MOVE SGTS0265-CD-CTRA-TIT-VL-ULT   TO MCR-S9-15
               MOVE MCR-S9-15                     TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0265-CD-OPR-ETRD-MCP-ULT'
                                                  TO MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
               IF  SGTS0265-CD-OPR-ETRD-MCP-ULT IS NUMERIC
                   MOVE  SGTS0265-CD-OPR-ETRD-MCP-ULT TO  MCR-S9-15
                   MOVE  MCR-S9-15      TO  MSGKW999-VL-VRV-MNT
               ELSE
                   MOVE 'Valor não numérico'     TO  MSGKW999-VL-VRV-MNT
               END-IF
                   PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SBDATAF<<
             WHEN SBDATAF
               MOVE 'ARG01'  TO MSGKW999-NM-VRV-MNT
               MOVE ARG01    TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG02'  TO MSGKW999-NM-VRV-MNT
               MOVE ARG02    TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'FUNCAO'                    TO  MSGKW999-NM-VRV-MNT
               MOVE  FUNCAO                      TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'A-TAM'                     TO  MSGKW999-NM-VRV-MNT
               MOVE   A-TAM                      TO  MCR-S9-15
               MOVE  MCR-S9-15                   TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'A-FIM'                     TO  MSGKW999-NM-VRV-MNT
               MOVE   A-FIM
               TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                    CONTINUE
           END-EVALUATE
           .
       999100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999200-MONITORAR-VRV-PGM-E-ENC  SECTION.
      *-----------------------------------------------------------------
      *--> Monitora variàveis do pgm, grava o erro e encerra

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Em caso de erro, desfaz as atualizaçoes em banco de dados
                 EXEC CICS SYNCPOINT
                           ROLLBACK
                 END-EXEC

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

      *==> Observação <=================================================
      *    Normalmente se Monitora apenas as Variáveis da Requisição
      *    para Monitoração da Mensageria.
      *    O Analista que especificou o módulo e o Analista Programador
      *    tem liberdade para Monitorar outras Variáveis que forem
      *    relevantes ao Suporte do Sistema.
      *=================================================================

      *--> Adiciona as variáveis para monitoração
           MOVE 'SGTS0262-DT-PSC-E'              TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0262-DT-PSC-E               TO MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0262-CD-CTRA-TIT-VL-E'      TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0262-CD-CTRA-TIT-VL-E       TO MCR-S9-15
           MOVE  MCR-S9-15                       TO MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0262-CD-OPR-ETRD-MCP-E'     TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0262-CD-OPR-ETRD-MCP-E  NUMERIC
               MOVE  SGTS0262-CD-OPR-ETRD-MCP-E  TO MCR-S9-15
               MOVE  MCR-S9-15                   TO MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'        TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0262-CD-TIT-OPR-ETRD-E'     TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0262-CD-TIT-OPR-ETRD-E      TO MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0262-CD-CLI-E'              TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0262-CD-CLI-E               TO MCR-S9-15
           MOVE  MCR-S9-15                       TO MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Paginacao
           MOVE 'SGTS0262-CD-CTRA-TIT-VL-ULT' TO MSGKW999-NM-VRV-MNT
           MOVE SGTS0262-CD-CTRA-TIT-VL-ULT   TO MCR-S9-15
           MOVE MCR-S9-15                     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0262-CD-OPR-ETRD-MCP-ULT'
                                              TO MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0262-CD-OPR-ETRD-MCP-ULT IS NUMERIC
               MOVE  SGTS0262-CD-OPR-ETRD-MCP-ULT TO  MCR-S9-15
               MOVE  MCR-S9-15      TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Valor não numérico'     TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO

           EXEC CICS PUT
              CONTAINER ( CTNR-PRM-ERRO               )
              CHANNEL   ( NM-CNL                      )
              FROM      ( IIBKERRO-PRM-ERRO           )
              FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
              NOHANDLE
           END-EXEC

           PERFORM 999999-ENCERRAR
           .
       999200-SAI.
           EXIT.

      *---------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      *---------------------------------------*
-INC HLPKP999

      *---------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999

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
      *---------------------- Fim SGTS0262 -----------------------------