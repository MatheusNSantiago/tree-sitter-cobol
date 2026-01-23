      *-----------------------------------------------------------------
      * Programa..: SGTS0113
      * Analista..: F2054109 - Cintia Ap Soares Iglesias
      * Autor.....: F4535118 - Ivone de Melo Costa
      * Data......: 06/02/2017
      * Objetivo..: Alterar Precificação da operação estruturada
      * Compilacao: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS002 09/09/2019 - F8090555 - Aceita Fee de garantia e Duration
      *                     com valores zerados.
      * VRS001 29/01/2019 - F9188847 - Acerto código mensageria
      *        22/01/2019 - F0950413 inclusão da sigla 'SGT' na section
      *                     972000-LER-CONTAINER-DE-ERRO
      *        21/01/2019 F8785286 DE 2016/145195 - Inclusão monitoração
      *              variáveis para rotina SBDATA
      *        09/10/2018 - F9158937 - Incluir consistência data inicio
      *                     Precificação >= que início da Remuneração
      *        22/08/2018 - Incluir Duration Inicial
      *        11/05/2018 - F8658869 DE 2015/137233 - Alterar 300000
      *                     para SGTS0113-CD-CTRA-TIT-VL
      *        06/02/2017 - F4535118 DE 2015/137233 - Implantacao
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0113.

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
                                       '*** S.S. Começa Aqui ***'.
       77  CTE-PROG                    PIC  X(18)  VALUE
                                       '*** SGTS0113***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS002'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTST005                PIC  X(08) VALUE 'SGTST005'.
           03  SGTSE068                PIC  X(08) VALUE 'SGTSE068'.
           03  SGTSI068                PIC  X(08) VALUE 'SGTSI068'.
           03  SGTSL068                PIC  X(08) VALUE 'SGTSL068'.
           03  SGTS0081                PIC  X(08) VALUE 'SGTS0081'.
           03  SGTS0128                PIC  X(08) VALUE 'SGTS0128'.
           03  SGTS0184                PIC  X(08) VALUE 'SGTS0184'.
           03  SGTS0185                PIC  X(08) VALUE 'SGTS0185'.
           03  SGTS0219                PIC  X(08) VALUE 'SGTS0219'.
           03  SGTS0220                PIC  X(08) VALUE 'SGTS0220'.
           03  SGTS0222                PIC  X(08) VALUE 'SGTS0222'.
           03  SBTIMEDT                PIC  X(08) VALUE 'SBTIMEDT'.
           03  SBDATAF                 PIC  X(08) VALUE 'SBDATAF'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Áreas de trabalho
       01  IC                          PIC S9(04) VALUE ZEROS.
       01  IC-JUR                      PIC S9(04) VALUE ZEROS.
       01  IC-BFF                      PIC S9(04) VALUE ZEROS.
       01  IC-ANTERIOR                 PIC S9(04) VALUE ZEROS.

       01  ULT-DT-INC-VLD              PIC X(08) VALUE SPACES.
       01  ULT-DT-INC-VLD-R            REDEFINES ULT-DT-INC-VLD.
           03  DIA                     PIC  9(02).
           03  MES                     PIC  9(02).
           03  ANO                     PIC  9(04).

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *--> Datas Auxiliares
       01  DT-VNCT                     PIC  X(08) VALUE SPACES.
       01  DT-VNCT-R                   REDEFINES  DT-VNCT.
           03 ANO                      PIC  9(04).
           03 MES                      PIC  9(02).
           03 DIA                      PIC  9(02).

       01  DT-EMS-TIT                  PIC  X(08) VALUE SPACES.
       01  DT-EMS-TIT-R                REDEFINES  DT-EMS-TIT.
           03 ANO                      PIC  9(04).
           03 MES                      PIC  9(02).
           03 DIA                      PIC  9(02).

       01  AUX-DT-INC                  PIC  X(08) VALUE SPACES.
       01  AUX-DT-INC-R                REDEFINES  AUX-DT-INC.
           03 ANO                      PIC  9(04).
           03 MES                      PIC  9(02).
           03 DIA                      PIC  9(02).

       01  AUX-DT-FIM                  PIC  X(08) VALUE SPACES.
       01  AUX-DT-FIM-R                REDEFINES  AUX-DT-FIM.
           03 ANO                      PIC  9(04).
           03 MES                      PIC  9(02).
           03 DIA                      PIC  9(02).

       01  AUX-DT                      PIC  X(08) VALUE SPACES.
       01  AUX-DT-R                    REDEFINES  AUX-DT.
           03 ANO                      PIC  9(04).
           03 MES                      PIC  9(02).
           03 DIA                      PIC  9(02).

       01  DT-POS-EMS                  PIC  X(08) VALUE SPACES.
       01  DT-POS-EMS-R                REDEFINES  DT-POS-EMS.
           03 ANO                      PIC  9(04).
           03 MES                      PIC  9(02).
           03 DIA                      PIC  9(02).

       01  DT-ANT-VNCT                 PIC  X(08) VALUE SPACES.
       01  DT-ANT-VNCT-R               REDEFINES  DT-ANT-VNCT.
           03 ANO                      PIC  9(04).
           03 MES                      PIC  9(02).
           03 DIA                      PIC  9(02).

       01  DT-INI-RMNC                 PIC  X(08) VALUE ' '.
       01  DT-INI-RMNC-R               REDEFINES  DT-INI-RMNC.
           03 ANO                      PIC  9(4).
           03 MES                      PIC  9(2).
           03 DIA                      PIC  9(2).

       01  DT-INI-PREF                 PIC  X(08) VALUE ' '.
       01  DT-INI-PREF-R               REDEFINES  DT-INI-PREF.
           03 ANO                      PIC  9(4).
           03 MES                      PIC  9(2).
           03 DIA                      PIC  9(2).

       01  AUX-QT-MM                   PIC  9(07)V9(2) VALUE ZEROS.
       01  AUX-QT-MM-R                 REDEFINES   AUX-QT-MM.
           03 AUX-INT                  PIC  9(07).
           03 AUX-DEC                  PIC  9(02).

       01  AUX-QT-MM-CARE-JUR          PIC  9(09) COMP-5 VALUE ZEROS.

       01  AUX-QT-MM-CARE-OPR          PIC  9(09) COMP-5 VALUE ZEROS.

       01  QT-DIAS-CORRIDOS            PIC  9(09) COMP-5 VALUE ZEROS.

       01  DT-EMS-PREF                 PIC  X(08) VALUE SPACES.
       01  DT-EMS-PREF-R               REDEFINES  DT-EMS-PREF.
           03 ANO                      PIC  9(04).
           03 MES                      PIC  9(02).
           03 DIA                      PIC  9(02).

       01  DT-VNCT-PREF                PIC  X(08) VALUE SPACES.
       01  DT-VNCT-PREF-R              REDEFINES  DT-VNCT-PREF.
           03 ANO                      PIC  9(04).
           03 MES                      PIC  9(02).
           03 DIA                      PIC  9(02).

       01  DT-CPR-OPR                  PIC  X(08) VALUE SPACES.
       01  DT-CPR-OPR-R                REDEFINES  DT-CPR-OPR.
           03 ANO                      PIC  9(04).
           03 MES                      PIC  9(02).
           03 DIA                      PIC  9(02).

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-TX.
           03  CD-OPR-ETRD             PIC  9(17).
       01  VL-04V10                    PIC  -.--9,9999999999.

      *--> Campo para definição de estado de erro em tabela.
       01  CD-ERRO-TABX                PIC  9(01) VALUE ZEROS.
           88  COM-ERRO-TABX                      VALUE 0001.
       01  CD-ERRO-TABY                PIC  9(01) VALUE ZEROS.
           88  COM-ERRO-TABY                      VALUE 0001.

       01 IC-TAB                       PIC S9(04) COMP-5 VALUE ZEROS.

      *--> São necessários 2 campos de índice, para cada tabela. Um para
      *    movimentar o nome do campo e outro para o valor do índice.
       01  NM-IC-TABX                  PIC  X(30) VALUE SPACES.
       01  VL-IC-TABX                  PIC S9(04) COMP-5 VALUE ZEROS.
       01  NM-IC-TABY                  PIC  X(30) VALUE SPACES.
       01  VL-IC-TABY                  PIC S9(04) COMP-5 VALUE ZEROS.

000800*-----------------------------------------------------------------
000810* AREA PARA RECUPERAR TIMESTAMP E DATA
000820*-----------------------------------------------------------------
000830 01  DADOS-SBTIMEDT.
000840     03  HLPKE001-CD-FUC              PIC X(03)      VALUE 'F01'.
000850     03  HLPKE001-AREA-TRB.
000860         05  HLPKE001-AREA-TRB-TAM    PIC S9(9) COMP VALUE 1024.
000870         05  HLPKE001-AREA-TRB-DADOS  PIC  X(1024)   VALUE ' '.
000880     03  HLPKE001-FMT-TS              PIC S9(4) COMP VALUE 1.
000890     03  HLPKE001-TAM-TS              PIC S9(4) COMP VALUE 30.
000900     03  HLPKE001-TS-FMTD.
000910         05  ANO                      PIC  9(4).
000920         05  FILLER                   PIC  X(1).
000930         05  MES                      PIC  9(2).
000940         05  FILLER                   PIC  X(1).
000950         05  DIA                      PIC  9(2).
000960         05  FILLER                   PIC  X(1).
000970         05  HORA                     PIC  9(2).
000980         05  FILLER                   PIC  X(1).
000990         05  MINUTO                   PIC  9(2).
001000         05  FILLER                   PIC  X(1).
001010         05  SEGUNDO                  PIC  9(2).
001020         05  FILLER                   PIC  X(1).
001030         05  MICRO-SEGUNDO            PIC  9(6).
001040         05  FILLER                   PIC  X(4).

       01  HOJE                             PIC  X(08).
       01  HOJE-R  REDEFINES                HOJE.
               03  ANO                      PIC  9(4).
000930         03  MES                      PIC  9(2).
000950         03  DIA                      PIC  9(2).

      *-----------------------------------------------------------------
      *    Areas Utilizadas pelas Funcoes do SBDATAF
      *-----------------------------------------------------------------

      *--> SBDATAF - F01 - Validar Data
       01  AREA-SBDATAF-F01.
           03  FUNCAO-F01              PIC  X(03) VALUE 'F01'.
           03  ARG01-F01               PIC  9(08) VALUE ZEROS.
           03  ARG01-F01-R             REDEFINES ARG01-F01.
               05  DIA                 PIC  9(02).
               05  MES                 PIC  9(02).
               05  ANO                 PIC  9(04).
           03  ARG02-F01               PIC  9(01) VALUE ZEROS.

      *--> SBDATAF - F03 -  Dias Corridos Entre Datas
       01  AREA-SBDATAF-F03.
           03  FUNCAO-F03              PIC  X(03) VALUE 'F03'.
           03  ARG01-F03               PIC  9(08) VALUE ZEROS.
           03  ARG01-F03-R             REDEFINES ARG01-F03.
               05  DIA                 PIC  9(02).
               05  MES                 PIC  9(02).
               05  ANO                 PIC  9(04).
           03  ARG02-F03               PIC  9(08) VALUE ZEROS.
           03  ARG02-F03-R             REDEFINES ARG02-F03.
               05  DIA                 PIC  9(02).
               05  MES                 PIC  9(02).
               05  ANO                 PIC  9(04).
           03  ARG03-F03               PIC  9(05) VALUE ZEROS.

      *--> SBDATAF - F05 - Calcular Data Final
      *              Data Final = Data Base +/- (Quantidade de Dias)
       01  AREA-SBDATA-F05.
           03  FUNCAO-F05              PIC  X(03) VALUE 'F05'.
           03  ARG01-F05               PIC  9(08) VALUE ZEROS.
           03  ARG01-F05-R             REDEFINES ARG01-F05.
               05  DIA                 PIC  9(02).
               05  MES                 PIC  9(02).
               05  ANO                 PIC  9(04).
           03  ARG02-F05               PIC  9(05) VALUE ZEROS.
           03  ARG03-F05               PIC  9(01) VALUE ZEROS.
           03  ARG04-F05               PIC  9(08) VALUE ZEROS.
           03  ARG04-F05-R             REDEFINES ARG04-F05.
               05  DIA                 PIC  9(02).
               05  MES                 PIC  9(02).
               05  ANO                 PIC  9(04).

       01  WKSBDATA                    PIC  X(4099).
       01  AREA-RED                    REDEFINES WKSBDATA.
           03  AREA-TAM                PIC  9(0008) COMP.
           03  FILLER                  PIC  X(4071).
           03  AREA-FIM                PIC  X(0020).


      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book dos dados do usuário
-INC IIBKOPRD

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book do container de requisição e resposta deste módulo
-INC SGTK0113

      *-----------------------------------------------------------------
      *    Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------


      *--> CRUD da Operação Estruturada - DB2SGT.OPR_ETRD_MCP
       01 SGTST005-DADOS.
-INC SGTKT005

      *--> Inclusão Precificação - DB2SGT.PREF_TAXA_OPR
-INC SGTKI068

      *--> Exclusão Precificação - DB2SGT.PREF_TAXA_OPR
-INC SGTKE068

      *--> Listar Precificação - DB2SGT.PREF_TAXA_OPR
-INC SGTKL068

      *--> Book do container de requisição e resposta deste módulo
-INC SGTK0081

      *--> Atualizar Indicador da Operação
-INC SGTK0128

      *--> Listar Remuneração
       01  SGTS0184-DADOS.
-INC SGTK0184

      *--> Comparar taxas de precificação com taxas remuneração
       01  SGTS0185-DADOS.
-INC SGTK0185

      *--> Incluir Duration Inicial
       01  SGTS0219-DADOS.
-INC SGTK0219

      *--> Excluir Duration Inicial
       01  SGTS0220-DADOS.
-INC SGTK0220

      *--> Consultar Duration Inicial
       01  SGTS0222-DADOS.
-INC SGTK0222

       77  CTE-FINAL-SS               PIC X(40) VALUE
                                      '*** S.S. Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

      *--> Habilita Testes no Cobolito
-INC ALMKCBL2

      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999

           PERFORM  100000-INICIAR

           PERFORM  200000-TRATAR-DATAS-CARENCIAS

           PERFORM  300000-PROCESSAR

           IF  SGTS0113-DURATION-INCIAL
               PERFORM  400000-TRATAR-DURATION
           END-IF

           PERFORM  500000-FINALIZAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  970000-LER-CANAL

      *--> Define o nome do programa p/ fins manipulação container
           MOVE  'SGTS0113'            TO  NM-PGM

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM  971000-LER-REQUISICAO

           PERFORM  110000-VALIDAR-REQUISICAO

           IF  SGTS0113-DURATION-INCIAL
               PERFORM  120000-VALIDAR-DURATION
           END-IF

           PERFORM  130000-VALIDAR-HABILITACAO

           PERFORM  140000-BUSCA-TS-DT

           PERFORM  974000-LER-DADOS-OPRD
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *--> Valida campo a campo da requisicao
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada
           IF  SGTS0113-CD-CTRA-TIT-VL     LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0113-CD-CTRA-TIT-VL     EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0113-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0113-CD-OPR-ETRD-MCP    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0113-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'    TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0113-NR-VRS-OPR-ETRD    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número versão operação estruturada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0113-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

      *--> Validar Outros Campos de Entrada
           IF  SGTS0113-DT-EMS-PREF        EQUAL  SPACES
               SET ERRO-PROGRAMADO     TO  TRUE
               MOVE  0557              TO  MSG1-CD-ERRO-TS
               MOVE  'Data de emissão da precificação não informada.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           ELSE
               MOVE  CORR SGTS0113-DT-EMS-PREF-R
                                       TO  ARG01-F01-R
      *------> Validação no formato Gregoriano
               PERFORM  9610000-VALIDAR-DATA

               MOVE  CORR SGTS0113-DT-EMS-PREF-R TO  DT-EMS-PREF-R

           END-IF

           IF  SGTS0113-QT-DD-PZ-VLD-PREF  LESS  ZEROS
               SET ERRO-PROGRAMADO     TO  TRUE
               MOVE  0558              TO  MSG1-CD-ERRO-TS
               MOVE  'Quantidade de dias do prazo de validade inválida.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0113-PC-TARF-GRT-TAXA   NOT NUMERIC
               SET ERRO-PROGRAMADO     TO  TRUE
               MOVE  0793              TO  MSG1-CD-ERRO-TS
               MOVE  'Percentual da tarifa de garantia da taxa não numér
      -              'ico.'            TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0113-PC-TARF-GRT-TAXA   NEGATIVE
               SET ERRO-PROGRAMADO     TO  TRUE
               MOVE  0794              TO  MSG1-CD-ERRO-TS
               MOVE  'Percentual da tarifa de garantia da taxa inválido.
      -              '.'               TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

      *    IF  SGTS0113-PC-TARF-GRT-TAXA   EQUAL  ZEROS
      *        SET ERRO-PROGRAMADO     TO  TRUE
      *        MOVE  0795              TO  MSG1-CD-ERRO-TS
      *        MOVE  'Percentual da tarifa de garantia da taxa não infor
      *              'mado.'           TO  MSG1-TX-ERRO-TS
      *        PERFORM  999000-ERRO
      *    END-IF

           PERFORM  VARYING  IC-JUR
                    FROM  0001 BY 0001
                    UNTIL  IC-JUR          GREATER  SGTS0113-QT-OCR-JUR

                    MOVE 'IC-JUR'      TO  NM-IC-TABX
                    MOVE  IC-JUR       TO  VL-IC-TABX

                IF  SGTS0113-DT-INC-CND-JUR (IC-JUR)
                                           EQUAL  SPACES
                    SET ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  0559         TO  MSG1-CD-ERRO-TS
                    MOVE 'Data início condição juros não informada.'
                                       TO  MSG1-TX-ERRO-TS
                    SET  COM-ERRO-TABX
                                       TO  TRUE
                    PERFORM  999000-ERRO
                ELSE
                    MOVE CORR SGTS0113-DT-INC-CND-JUR-R  (IC-JUR)
                                       TO  ARG01-F01-R
      *-----------> Validação no formato Gregoriano
                    PERFORM  9610000-VALIDAR-DATA
                END-IF

                IF  SGTS0113-DT-FIM-CND-JUR (IC-JUR)
                                           EQUAL  SPACES
                    SET ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  0560         TO  MSG1-CD-ERRO-TS
                    MOVE  'Data fim condição juros não informada.'
                                       TO  MSG1-TX-ERRO-TS
                    SET   COM-ERRO-TABX
                                       TO  TRUE
                    PERFORM  999000-ERRO
                ELSE
                    MOVE  CORR SGTS0113-DT-FIM-CND-JUR-R  (IC-JUR)
                                       TO  ARG01-F01-R
      *-----------> Validação no formato Gregoriano
                    PERFORM  9610000-VALIDAR-DATA
                END-IF

                IF  SGTS0113-PC-MIN-JUR (IC-JUR)
                NOT NUMERIC
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  0172         TO  MSG1-CD-ERRO-TS
                    MOVE 'Percentual mínimo da taxa de juros não numéric
      -                   'o.'         TO  MSG1-TX-ERRO-TS
                    SET   COM-ERRO-TABX
                                       TO  TRUE
                    PERFORM  999000-ERRO
                END-IF

                IF  SGTS0113-PC-MIN-JUR (IC-JUR)
                                           <=  ZEROS
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  0174         TO  MSG1-CD-ERRO-TS
                    MOVE 'Percentual mínimo da taxa de juros inválido.'
                                       TO  MSG1-TX-ERRO-TS
                    SET   COM-ERRO-TABX
                                       TO  TRUE
                    PERFORM  999000-ERRO
                END-IF

                IF  SGTS0113-CD-IEC-PREF (IC-JUR)
                                           <=  ZEROS
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  0180         TO  MSG1-CD-ERRO-TS
                    MOVE 'Código do indicador econômico inválido.'
                                       TO  MSG1-TX-ERRO-TS
                    SET  COM-ERRO-TABX
                                       TO  TRUE
                    PERFORM  999000-ERRO
                END-IF

                IF  SGTS0113-CD-PERC-APL-PREF (IC-JUR)
                                           EQUAL  ZEROS
                    MOVE     1     TO SGTS0113-CD-PERC-APL-PREF (IC-JUR)
                END-IF

                IF  SGTS0113-CD-PERC-APL-PREF (IC-JUR)
                                           <=  ZEROS
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  0178         TO  MSG1-CD-ERRO-TS
                    MOVE 'Código da periodicidade da condição inválido.'
                                       TO  MSG1-TX-ERRO-TS
                    SET   COM-ERRO-TABX
                                       TO  TRUE
                    PERFORM  999000-ERRO
                END-IF

                IF  SGTS0113-CD-IEC-PREF (IC-JUR)
                                           EQUAL  1313
                    MOVE  3    TO    SGTS0113-CD-TIP-APL-PREF (IC-JUR)
                ELSE
                    IF  SGTS0113-CD-TIP-APL-PREF (IC-JUR)
                                           <=  ZEROS
                    OR  SGTS0113-CD-TIP-APL-PREF (IC-JUR)
                                           GREATER  0002
                       SET   ERRO-PROGRAMADO
                                       TO  TRUE
                       MOVE  0014      TO  MSG1-CD-ERRO-TS
                    MOVE 'Código tipo de forma de aplicação taxa inválid
      -                      'o.'      TO  MSG1-TX-ERRO-TS
                       SET  COM-ERRO-TABX
                                       TO  TRUE
                      PERFORM  999000-ERRO
                   END-IF
                 END-IF

           END-PERFORM

           PERFORM
                VARYING IC-BFF         FROM 1 BY 1
                UNTIL  IC-BFF              GREATER  SGTS0113-QT-OCR-GRT

                MOVE 'IC-BFF'          TO  NM-IC-TABY
                MOVE  IC-BFF           TO  VL-IC-TABY

                IF  SGTS0113-DT-INC-CND-GRT(IC-BFF)
                                           EQUAL  SPACES
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  0168         TO  MSG1-CD-ERRO-TS
                    MOVE 'Data início condição BBF não informada.'
                                       TO  MSG1-TX-ERRO-TS
                    SET   COM-ERRO-TABY
                                       TO  TRUE
                    PERFORM  999000-ERRO
                ELSE
                    MOVE  CORR SGTS0113-DT-INC-CND-GRT-R  (IC-BFF)
                                       TO  ARG01-F01-R
      *-----------> Validação no formato Gregoriano
                    PERFORM  9610000-VALIDAR-DATA
                END-IF

                IF  SGTS0113-DT-FIM-CND-GRT (IC-BFF)
                                           EQUAL  SPACES
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  0170         TO  MSG1-CD-ERRO-TS
                    MOVE  'Data fim condição BBF não informada.'
                                       TO  MSG1-TX-ERRO-TS
                    SET   COM-ERRO-TABY
                                       TO  TRUE
                    PERFORM  999000-ERRO
                ELSE
                    MOVE  CORR SGTS0113-DT-FIM-CND-GRT-R (IC-BFF)
                                       TO  ARG01-F01-R
      *-----------> Validação no formato Gregoriano
                    PERFORM  9610000-VALIDAR-DATA
                END-IF

                IF  SGTS0113-PC-MIN-GRT (IC-BFF)
                NOT NUMERIC
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  0172         TO  MSG1-CD-ERRO-TS
                    MOVE 'Percentual mínimo da taxa de BFF não numérico.
      -                  ' '           TO  MSG1-TX-ERRO-TS
                    SET  COM-ERRO-TABY
                                       TO  TRUE
                    PERFORM  999000-ERRO
                END-IF

                IF  SGTS0113-PC-MIN-GRT (IC-BFF)
                                           <  ZEROS
                    SET ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  0173         TO  MSG1-CD-ERRO-TS
                    MOVE 'Percentual mínimo da taxa de BFF não informado
      -                   '.'          TO  MSG1-TX-ERRO-TS
                    SET  COM-ERRO-TABY
                                       TO  TRUE
                    PERFORM  999000-ERRO
                END-IF
           END-PERFORM

      **  Compara data inicial Precificação com data inicial Remuneração
      **----------------------------------------------------------------

           PERFORM  210000-CONSULTAR-OPERACAO

           IF  SGTST005-IN-RMNC-VRV  = 'S'
      *    Remuneração é Escalonada
              MOVE     0003                  TO  MSG1-CD-SEQL-ERRO
              MOVE     SGTS0184              TO  NM-PGM
              MOVE SGTS0113-CD-CTRA-TIT-VL   TO SGTS0184-CD-CTRA-TIT-VL
              MOVE SGTS0113-CD-OPR-ETRD-MCP  TO SGTS0184-CD-OPR-ETRD-MCP
              MOVE SGTS0113-NR-VRS-OPR-ETRD  TO SGTS0184-NR-VRS-OPR-ETRD
              MOVE        01010001           TO SGTS0184-DT-INC-TAXA-ULT

              PERFORM  900000-EXECUTAR-MODULO

              MOVE  CORR SGTS0184-DT-INC-TAXA-RMNC-R (1)
                                             TO  DT-INI-RMNC-R
              MOVE  CORR SGTS0113-DT-INC-CND-JUR-R (1)
                                             TO  DT-INI-PREF-R
              IF   DT-INI-RMNC  GREATER   DT-INI-PREF
                   SET  ERRO-PROGRAMADO TO  TRUE
                   MOVE  0846            TO  MSG1-CD-ERRO-TS
                   MOVE  'Data início da Remuneração deve ser menor ou i
      -                  'gual ao início da Precificação.'
                                         TO  MSG1-TX-ERRO-TS
                   PERFORM  969000-MONITORAR-VARIAVEIS
                   PERFORM  999000-ERRO
              END-IF
           END-IF

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-VALIDAR-DURATION         SECTION.
      *--> Validar Campos da Duraton Inicial
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTS0113-DT-PSC-CLC         EQUAL  SPACES  OR
               SGTS0113-DT-PSC-CLC         EQUAL  '00000000'
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0769              TO  MSG1-CD-ERRO-TS
               MOVE  'Data da posição de cálculo não informada.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0113-QT-AA-PZ-RTN-INVS  NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0773              TO  MSG1-CD-ERRO-TS
               MOVE  'Quantidade de anos do prazo retorno investimento n
      -              'ão numérica.'    TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0113-QT-AA-PZ-RTN-INVS  NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0774              TO  MSG1-CD-ERRO-TS
               MOVE  'Quantidade de anos do prazo retorno investimento i
      -              'nválida.'        TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *    IF  SGTS0113-QT-AA-PZ-RTN-INVS  EQUAL  ZEROS
      *        SET   ERRO-PROGRAMADO   TO  TRUE
      *        MOVE  0775              TO  MSG1-CD-ERRO-TS
      *        MOVE  'Quantidade de anos do prazo retorno investimento n
      *              'ão informada.'   TO  MSG1-TX-ERRO-TS
      *        PERFORM  999000-GRAVA-ERRO-E-ENCERRA
      *    END-IF

           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-VALIDAR-HABILITACAO      SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTS0081-DADOS

      *--> Movimentar o nome do programa a ser executado
           MOVE  'SGTS0081'            TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0113-CD-CTRA-TIT-VL
                                       TO  SGTS0081-CD-CTRA-TIT-VL
           MOVE  SGTS0113-CD-OPR-ETRD-MCP
                                       TO  SGTS0081-CD-OPR-ETRD
           MOVE  SGTS0113-NR-VRS-OPR-ETRD
                                       TO  SGTS0081-NR-VRS-OPR-ETRD

           PERFORM  900000-EXECUTAR-MODULO

           IF  SGTS0081-IN-HBLT-MENU-PREF  EQUAL  'N'

               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0098              TO  MSG1-CD-ERRO-TS
               MOVE 'Alteração não permitida para o estado da operação.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-BUSCA-TS-DT              SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           CALL  SBTIMEDT           USING  HLPKE001-CD-FUC
                                           HLPKE001-AREA-TRB
                                           HLPKE001-FMT-TS
                                           HLPKE001-TAM-TS
                                           HLPKE001-TS-FMTD

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  SBTIMEDT          TO  MSG1-NM-PGM-ACND
               MOVE  0050              TO  MSG1-CD-ERRO-TS
               MOVE  'Erro ao obter data e hora atual.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           MOVE  CORR HLPKE001-TS-FMTD TO HOJE-R

           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-TRATAR-DATAS-CARENCIAS   SECTION.
      *-----------------------------------------------------------------

           PERFORM  220000-CALCULAR-DATAS-LIMITES

           PERFORM  230000-CALCULAR-CARENCIAS

           PERFORM  240000-VALIDAR-DATAS-LIMITES

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-CONSULTAR-OPERACAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTST005-CHAVE.

      *--> Movimentar o nome do programa a ser executado
           MOVE  'SGTST005'            TO  NM-PGM

      *--> Movimentar os campos de entrada
      *--> Validação no formato Gregoriano
           MOVE  0004                  TO  SGTST005-ACAO

           MOVE  SGTS0113-CD-CTRA-TIT-VL
                                       TO  SGTST005-CD-CTRA-TIT-VL
           MOVE  SGTS0113-CD-OPR-ETRD-MCP
                                       TO  SGTST005-CD-OPR-ETRD-MCP
           MOVE  SGTS0113-NR-VRS-OPR-ETRD
                                       TO  SGTST005-NR-VRS-OPR-ETRD

           PERFORM  920000-CHAMAR-SGTST005

           IF  SGTST005-DT-EMS-TIT-OPR     EQUAL  SPACES
                SET   ERRO-PROGRAMADO  TO  TRUE
                MOVE  0354             TO  MSG1-CD-ERRO-TS
                MOVE  'Data de emissão do título não informada.'
                                       TO  MSG1-TX-ERRO-TS
                PERFORM  999000-ERRO

           END-IF

           IF  SGTST005-DT-VNCT-OPR        EQUAL  SPACES
                SET   ERRO-PROGRAMADO  TO  TRUE
                MOVE  0239             TO  MSG1-CD-ERRO-TS
                MOVE  'Data de vencimento da operação não informada.'
                                       TO  MSG1-TX-ERRO-TS
                PERFORM  999000-ERRO
           END-IF

           MOVE  CORR SGTST005-DT-EMS-TIT-OPR-R
                                       TO  DT-EMS-TIT-R

           MOVE  CORR SGTST005-DT-VNCT-OPR-R
                                       TO  DT-VNCT-R

           MOVE  CORR SGTST005-DT-CPR-OPR-R
                                       TO  DT-CPR-OPR-R
           IF  DT-EMS-PREF-R  GREATER DT-CPR-OPR-R
               SET   ERRO-PROGRAMADO  TO  TRUE
               MOVE  0838             TO  MSG1-CD-ERRO-TS
               MOVE  'Data de emissão da precificação deve ser menor ou
      -              'igual a data de compra.'
                                      TO  MSG1-TX-ERRO-TS

               MOVE 'SGTST005-DT-CPR-OPR-R' TO  MSGKW999-NM-VRV-MNT
               MOVE SGTST005-DT-CPR-OPR-R   TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               PERFORM  999000-ERRO
           END-IF
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-CALCULAR-DATAS-LIMITES   SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

      *--> Calcular DT-EMS-R  +  1
           MOVE  CORR DT-EMS-TIT-R     TO  ARG01-F05-R
      *    Quantidade de dias
           MOVE  0001                  TO  ARG02-F05
      *    Adição  ARG03-F05 = 1
           MOVE  0001                  TO  ARG03-F05

           PERFORM  962000-CALCULAR-DIA

           MOVE  CORR ARG04-F05-R      TO  DT-POS-EMS-R

           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

      *--> Calcular DT-VNCT-R  -  1
           MOVE  CORR DT-VNCT-R        TO  ARG01-F05-R
      *    Quantidade de dias
           MOVE  0001                  TO  ARG02-F05
      *    Subtração  ARG03-F05 = 2
           MOVE  0002                  TO  ARG03-F05

           PERFORM  962000-CALCULAR-DIA

           MOVE  CORR ARG04-F05-R      TO  DT-ANT-VNCT-R


      *--> Calcular DT-EMS-PREF + X
           MOVE  CORR DT-EMS-PREF-R         TO  ARG01-F05-R
      *    Quantidade de dias
           MOVE SGTS0113-QT-DD-PZ-VLD-PREF  TO  ARG02-F05
      *    Adição  ARG03-F05 = 1
           MOVE  0001                       TO  ARG03-F05

           PERFORM  962000-CALCULAR-DIA

           MOVE  CORR ARG04-F05-R           TO  DT-VNCT-PREF-R
           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-CALCULAR-CARENCIAS       SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

      *--> Cálculo Carência de Juros
           MOVE  CORR SGTS0113-DT-INC-CND-JUR-R (1)
                                       TO  ARG01-F03-R
           MOVE  CORR DT-POS-EMS-R     TO  ARG02-F03-R

           PERFORM  963000-CALCULAR-DIAS-CORRIDOS

      *--> Cálculo dos meses
           DIVIDE  QT-DIAS-CORRIDOS    BY  0030
                   GIVING                  AUX-QT-MM
                   ON SIZE ERROR
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0561          TO  MSG1-CD-ERRO-TS
                   MOVE  'Erro no cálculo da carência de juros.'
                                       TO  MSG1-TX-ERRO-TS
                   PERFORM  999000-ERRO
           END-DIVIDE

           MOVE  AUX-INT               TO  AUX-QT-MM-CARE-JUR

      *--> Cálculo Carência da Taxa de Quebra da Garantia
           MOVE  CORR  SGTS0113-DT-INC-CND-GRT-R (1)
                                       TO  ARG01-F03-R
           MOVE  CORR  DT-POS-EMS-R    TO  ARG02-F03-R

           PERFORM  963000-CALCULAR-DIAS-CORRIDOS

      *--> Cálculo dos meses
           DIVIDE  QT-DIAS-CORRIDOS    BY  0030
                   GIVING                  AUX-QT-MM
                   ON SIZE ERROR
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0562          TO  MSG1-CD-ERRO-TS
                   MOVE 'Erro no cálculo da carência da taxa de quebra d
      -                 'a garantia.'  TO  MSG1-TX-ERRO-TS
                   SET   COM-ERRO-TABY
                                       TO  TRUE
                   PERFORM  999000-ERRO
           END-DIVIDE

           MOVE  AUX-INT               TO  AUX-QT-MM-CARE-OPR
           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       240000-VALIDAR-DATAS-LIMITES    SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           MOVE  CORR DT-ANT-VNCT-R    TO  AUX-DT-R

           IF  SGTS0113-QT-OCR-JUR         GREATER  ZEROS
               MOVE  CORR SGTS0113-DT-FIM-CND-JUR-R(SGTS0113-QT-OCR-JUR)
                                       TO  AUX-DT-FIM-R
               IF  AUX-DT-FIM              GREATER  DT-VNCT
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0563          TO  MSG1-CD-ERRO-TS
                   MOVE 'Datas informadas para juros fora do período da
      -                  'operação.'   TO  MSG1-TX-ERRO-TS
                   PERFORM  999000-ERRO
               END-IF

               IF  AUX-DT-FIM              LESS  AUX-DT
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0564          TO  MSG1-CD-ERRO-TS
                   MOVE 'Datas informadas para juros não completam o per
      -                  'íodo da operação.'
                                       TO  MSG1-TX-ERRO-TS
                   PERFORM  999000-ERRO
               END-IF
            END-IF

           IF  SGTS0113-QT-OCR-GRT         GREATER  ZEROS
              MOVE  CORR SGTS0113-DT-FIM-CND-GRT-R (SGTS0113-QT-OCR-GRT)
                                       TO  AUX-DT-FIM-R
               IF  AUX-DT-FIM              GREATER  DT-VNCT
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0565          TO  MSG1-CD-ERRO-TS
                   MOVE  'Datas informadas para taxa de quebra da garant
      -                  'ia fora do período da operação.'
                                       TO  MSG1-TX-ERRO-TS
                   PERFORM  999000-ERRO
               END-IF

               IF  AUX-DT-FIM              LESS  AUX-DT
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0566          TO  MSG1-CD-ERRO-TS
                   MOVE  'Datas informadas para taxa de quebra da garant
      -                  'ia não completam o período da operação.'
                                       TO  MSG1-TX-ERRO-TS
                   PERFORM  999000-ERRO
               END-IF
           END-IF
           .
       240000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------

           PERFORM  310000-TRATAR-PERIODO-JUR
                    VARYING IC-JUR FROM 1 BY 1
                    UNTIL  IC-JUR          GREATER  SGTS0113-QT-OCR-JUR

           PERFORM  320000-TRATAR-PERIODO-BFF
                     VARYING IC-BFF         FROM 1 BY 1
                     UNTIL  IC-BFF         GREATER  SGTS0113-QT-OCR-GRT

           PERFORM  330000-TRATAR-IND-FACILITY

           PERFORM  340000-TRATAR-IND-REMUNERACAO

           PERFORM  350000-GRAVAR-TABELAS

           PERFORM  360000-VERIFICA-DFRC-PREF

           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       310000-TRATAR-PERIODO-JUR       SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'IC-JUR'              TO  NM-IC-TABX
           MOVE   IC-JUR               TO  VL-IC-TABX

           MOVE CORR SGTS0113-DT-INC-CND-JUR-R (IC-JUR)
                                       TO  AUX-DT-INC-R

           MOVE CORR SGTS0113-DT-FIM-CND-JUR-R (IC-JUR)
                                       TO  AUX-DT-FIM-R

           IF  AUX-DT-INC                  LESS  DT-EMS-TIT  OR
               AUX-DT-INC                  GREATER  DT-VNCT
              SET ERRO-PROGRAMADO      TO  TRUE
              MOVE  0443               TO  MSG1-CD-ERRO-TS
              MOVE  'Data inicial fora do período de vigência.'
                                       TO  MSG1-TX-ERRO-TS
              SET  COM-ERRO-TABX       TO  TRUE
              PERFORM  999000-ERRO
           END-IF

           IF  AUX-DT-FIM                  LESS  DT-EMS-TIT  OR
               AUX-DT-FIM                  GREATER  DT-VNCT-R
              SET ERRO-PROGRAMADO      TO  TRUE
              MOVE  0444               TO  MSG1-CD-ERRO-TS
              MOVE  'Data final fora do período de vigência.'
                                       TO  MSG1-TX-ERRO-TS
              SET   COM-ERRO-TABX      TO  TRUE
              PERFORM  999000-ERRO
           END-IF

           IF  AUX-DT-INC                  GREATER  AUX-DT-FIM
              SET   ERRO-PROGRAMADO    TO  TRUE
              MOVE  0445               TO  MSG1-CD-ERRO-TS
              MOVE 'Data inicial juros do período maior que a data final
      -             '.'                TO  MSG1-TX-ERRO-TS
              SET   COM-ERRO-TABX      TO  TRUE
              PERFORM  999000-ERRO
           END-IF

           IF  IC-JUR                      GREATER  0001
      *------> Calcular  período anterior
               SUBTRACT  0001        FROM  IC-JUR
                         GIVING            IC-ANTERIOR

               MOVE  CORR SGTS0113-DT-FIM-CND-JUR-R (IC-ANTERIOR)
                                       TO  AUX-DT-FIM-R

               IF  AUX-DT-INC              <=   AUX-DT-FIM
                   SET  ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0569          TO  MSG1-CD-ERRO-TS
                   MOVE  'Ocorreu sobreposição de períodos no escaloname
      -                  'nto da precificação dos juros.'
                                       TO  MSG1-TX-ERRO-TS
                   SET   COM-ERRO-TABX
                                       TO  TRUE
                   PERFORM  999000-ERRO
               END-IF

      *------> Calcular SGTS0113-DT-FIM-CND-JUR-R (IC-ANTERIOR) + 1
               MOVE  CORR AUX-DT-FIM-R
                                       TO  ARG01-F05-R
               MOVE  0001              TO  ARG02-F05
      *------> Soma  ARG03-F05 + 1
               MOVE  0001              TO  ARG03-F05

               PERFORM  962000-CALCULAR-DIA

               MOVE  CORR ARG04-F05-R  TO  AUX-DT-FIM-R

               IF  AUX-DT-INC              NOT EQUAL  AUX-DT-FIM
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0570          TO  MSG1-CD-ERRO-TS
                   MOVE  'Foi encontrado período descoberto na precifica
      -                  'ção de juros.'
                                       TO  MSG1-TX-ERRO-TS
                   SET   COM-ERRO-TABX
                                       TO  TRUE
                   PERFORM 999000-ERRO
               END-IF
           END-IF
           .
       310000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       320000-TRATAR-PERIODO-BFF       SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

           MOVE 'IC-BFF'               TO  NM-IC-TABY
           MOVE  IC-BFF                TO  VL-IC-TABY

           MOVE  ZEROS                 TO  IC-ANTERIOR

           MOVE  CORR SGTS0113-DT-INC-CND-GRT-R (IC-BFF)
                                       TO  AUX-DT-INC-R
           MOVE  CORR SGTS0113-DT-FIM-CND-GRT-R (IC-BFF)
                                       TO  AUX-DT-FIM-R


           IF  AUX-DT-INC                  LESS  DT-EMS-TIT  OR
               AUX-DT-INC                  GREATER  DT-VNCT
              SET   ERRO-PROGRAMADO    TO  TRUE
              MOVE  0571               TO  MSG1-CD-ERRO-TS
              MOVE 'Data inicial da taxa de quebra da garantia fora do p
      -            'eríodo de validade do título.'
                                       TO  MSG1-TX-ERRO-TS
              SET   COM-ERRO-TABY      TO  TRUE
              PERFORM  999000-ERRO
           END-IF

           IF  AUX-DT-FIM                  LESS  DT-EMS-TIT  OR
               AUX-DT-FIM                  GREATER  DT-VNCT
              SET   ERRO-PROGRAMADO    TO  TRUE
              MOVE  0572               TO  MSG1-CD-ERRO-TS
              MOVE 'Data final da taxa de quebra da garantia fora do per
      -            'íodo de validade do título.'
                                       TO  MSG1-TX-ERRO-TS
              SET   COM-ERRO-TABY      TO  TRUE
              PERFORM  999000-ERRO
           END-IF

           IF  AUX-DT-INC                  GREATER  AUX-DT-FIM
              SET   ERRO-PROGRAMADO    TO  TRUE
              MOVE  0573               TO  MSG1-CD-ERRO-TS
              MOVE 'Data inicial da taxa de quebra da garantia do períod
      -            'o maior que a data final.'
                                       TO  MSG1-TX-ERRO-TS
              SET  COM-ERRO-TABY       TO  TRUE
              PERFORM  999000-ERRO
           END-IF

           IF  IC-BFF                      GREATER  0001
               SUBTRACT  0001        FROM  IC-BFF
                         GIVING            IC-ANTERIOR

               MOVE  CORR SGTS0113-DT-FIM-CND-GRT-R (IC-ANTERIOR)
                                       TO  AUX-DT-FIM-R

               IF  AUX-DT-INC              <=  AUX-DT-FIM
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0574          TO  MSG1-CD-ERRO-TS
                   MOVE  'Ocorreu sobreposição de períodos no escaloname
      -                  'nto da precificação da taxa de quebra da garan
      -                  'tia.'        TO  MSG1-TX-ERRO-TS
                  SET   COM-ERRO-TABY
                                       TO  TRUE
                  PERFORM 999000-ERRO
               END-IF

      *-----> Calcular SGTS0113-DT-FIM-CND-GRT-R (IC-ANTERIOR) +  1
               MOVE  CORR AUX-DT-FIM-R
                                       TO  ARG01-F05-R
               MOVE  0001              TO  ARG02-F05
      *------> Soma  ARG03-F05 + 1
               MOVE  0001              TO  ARG03-F05

               PERFORM  962000-CALCULAR-DIA

               MOVE  CORR ARG04-F05-R  TO  AUX-DT-FIM-R

               IF  AUX-DT-INC              NOT EQUAL  AUX-DT-FIM
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0575          TO  MSG1-CD-ERRO-TS
                   MOVE  'Foi encontrado período descoberto na precifica
      -                  'ção da taxa de quebra da garantia.'
                                       TO  MSG1-TX-ERRO-TS
                   SET   COM-ERRO-TABY
                                       TO  TRUE
                   PERFORM 999000-ERRO
               END-IF
           END-IF
           .
       320000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       330000-TRATAR-IND-FACILITY      SECTION.
      *-----------------------------------------------------------------
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTS0128-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTS0128              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0113-CD-CTRA-TIT-VL
                                       TO  SGTS0128-CD-CTRA-TIT-VL
           MOVE  SGTS0113-CD-OPR-ETRD-MCP
                                       TO  SGTS0128-CD-OPR-ETRD-MCP
           MOVE  SGTS0113-NR-VRS-OPR-ETRD
                                       TO  SGTS0128-NR-VRS-OPR-ETRD
           MOVE  0017                  TO  SGTS0128-CD-TIP-IN
           MOVE  SGTS0113-IN-FCTR      TO  SGTS0128-IN-OPR

           PERFORM  900000-EXECUTAR-MODULO
           .
       330000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       340000-TRATAR-IND-REMUNERACAO   SECTION.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTS0128-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTS0128             TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0113-CD-CTRA-TIT-VL
                                       TO  SGTS0128-CD-CTRA-TIT-VL
           MOVE  SGTS0113-CD-OPR-ETRD-MCP
                                       TO  SGTS0128-CD-OPR-ETRD-MCP
           MOVE  SGTS0113-NR-VRS-OPR-ETRD
                                       TO  SGTS0128-NR-VRS-OPR-ETRD
           MOVE  0016                  TO  SGTS0128-CD-TIP-IN
           MOVE  SGTS0113-IN-RMNC-VRV  TO  SGTS0128-IN-OPR

           PERFORM  900000-EXECUTAR-MODULO
           .
       340000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       350000-GRAVAR-TABELAS           SECTION.
      *-----------------------------------------------------------------
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  IC-JUR

           MOVE  ZEROS                 TO  IC-BFF

           PERFORM  351000-GRAVAR-OPERACAO

           IF  SGTS0113-IN-ATL-JUR         EQUAL  'S'
               MOVE  ZEROS             TO  IC
               PERFORM  352000-TRATAR-EXCLUSAO-JUROS
               PERFORM  353000-GRAVAR-CONDICAO-JUR
                        VARYING IC-JUR
                        FROM 0001 BY 0001
                        UNTIL  IC-JUR      GREATER  SGTS0113-QT-OCR-JUR
           END-IF

           IF  SGTS0113-IN-ATL-GRT         EQUAL  'S'
               MOVE  ZEROS             TO  IC
               PERFORM  354000-TRATAR-EXCLUSAO-BFF
               PERFORM  355000-GRAVAR-CONDICAO-BFF
                        VARYING IC-BFF
                        FROM 0001 BY 0001
                        UNTIL  IC-BFF      GREATER  SGTS0113-QT-OCR-GRT
           END-IF
           .
       350000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       351000-GRAVAR-OPERACAO          SECTION.
      *-----------------------------------------------------------------
           MOVE  0017                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTST005-CHAVE.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTST005              TO  NM-PGM

      *--> Movimentar os campos de entrada
      *    Alterar
           MOVE  0002                  TO  SGTST005-ACAO

           MOVE  SGTS0113-CD-CTRA-TIT-VL
                                       TO  SGTST005-CD-CTRA-TIT-VL
           MOVE  SGTS0113-CD-OPR-ETRD-MCP
                                       TO  SGTST005-CD-OPR-ETRD-MCP
           MOVE  SGTS0113-NR-VRS-OPR-ETRD
                                       TO  SGTST005-NR-VRS-OPR-ETRD
           MOVE  KOPRD-CD-OPRD         TO  SGTST005-CD-USU-ULT-ATL
           MOVE  'CURRENT'             TO  SGTST005-TS-ULT-ATL

           MOVE '  .  .    '           TO  SGTST005-DT-EMS-PREF
           MOVE  CORR SGTS0113-DT-EMS-PREF-R
                                       TO  SGTST005-DT-EMS-PREF-R
           MOVE  SGTS0113-QT-DD-PZ-VLD-PREF
                                       TO  SGTST005-QT-DD-PZ-VLD-PREF

           MOVE  AUX-QT-MM-CARE-JUR    TO  SGTST005-QT-MM-CARE-JUR
           MOVE  AUX-QT-MM-CARE-OPR    TO  SGTST005-QT-MM-CARE-OPR
           MOVE  SGTS0113-PC-TARF-GRT-TAXA
                                       TO  SGTST005-PC-TARF-GRT-TAXA

           PERFORM  920000-CHAMAR-SGTST005
           .
       351000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       352000-TRATAR-EXCLUSAO-JUROS    SECTION.
      *-----------------------------------------------------------------
           MOVE  0018                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTSL068-RQSC.

           IF  IC                          EQUAL  ZEROS
               MOVE  SPACES            TO  ULT-DT-INC-VLD
           END-IF


      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSL068              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0113-CD-CTRA-TIT-VL
                                       TO  SGTSL068-CD-CTRA-TIT-VL
           MOVE  SGTS0113-CD-OPR-ETRD-MCP
                                       TO  SGTSL068-CD-OPR-ETRD-MCP
           MOVE  SGTS0113-NR-VRS-OPR-ETRD
                                       TO  SGTSL068-NR-VRS-OPR-ETRD
      *    Tipo da Taxa = 1 - Juros
           MOVE  0001                  TO  SGTSL068-CD-TIP-TAXA
           MOVE  ULT-DT-INC-VLD        TO  SGTSL068-DT-ULT-INC-VLD

           PERFORM  900000-EXECUTAR-MODULO

           PERFORM  964000-EXCLUIR-CONDICAO
                    VARYING IC
                    FROM 0001 BY 0001
                    UNTIL  IC              GREATER  SGTSL068-QT-OCR-TAB
           .
       352000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       353000-GRAVAR-CONDICAO-JUR      SECTION.
      *-----------------------------------------------------------------
           MOVE  0019                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTSI068-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSI068              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0113-CD-CTRA-TIT-VL
                                       TO  SGTSI068-CD-CTRA-TIT-VL
           MOVE  SGTS0113-CD-OPR-ETRD-MCP
                                       TO  SGTSI068-CD-OPR-ETRD-MCP
           MOVE  SGTS0113-NR-VRS-OPR-ETRD
                                       TO  SGTSI068-NR-VRS-OPR-ETRD
           MOVE  0001                  TO  SGTSI068-CD-TIP-TAXA

           MOVE  '  .  .    '          TO  SGTSI068-DT-INC-CND-TAXA

           MOVE  CORR SGTS0113-DT-INC-CND-JUR-R (IC-JUR)
                                       TO  SGTSI068-DT-INC-CND-TAXA-R
           MOVE  '  .  .    '          TO  SGTSI068-DT-FIM-CND-TAXA
           MOVE  CORR  SGTS0113-DT-FIM-CND-JUR-R (IC-JUR)
                                       TO  SGTSI068-DT-FIM-CND-TAXA-R

           MOVE  SGTS0113-CD-TIP-APL-PREF (IC-JUR)
                                       TO  SGTSI068-CD-TIP-APL-PREF

           MOVE  SGTS0113-CD-PERC-APL-PREF (IC-JUR)
                                       TO  SGTSI068-CD-PERC-APL-PREF

           MOVE  SGTS0113-CD-IEC-PREF (IC-JUR)
                                       TO  SGTSI068-CD-IEC-PREF

           MOVE  KOPRD-CD-OPRD         TO  SGTSI068-CD-USU-ULT-ATL
           MOVE  'CURRENT'             TO  SGTSI068-TS-ULT-ATL

           MOVE  SGTS0113-PC-MIN-JUR  (IC-JUR)
                                       TO  SGTSI068-PC-MIN-TAXA

           PERFORM  900000-EXECUTAR-MODULO
           .
       353000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       354000-TRATAR-EXCLUSAO-BFF      SECTION.
      *-----------------------------------------------------------------
           MOVE  0020                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTSL068-RQSC.

           MOVE  SPACES                TO  ULT-DT-INC-VLD

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSL068              TO  NM-PGM

      *--> Movimentar os campos de entrada

           MOVE  SGTS0113-CD-CTRA-TIT-VL
                                       TO  SGTSL068-CD-CTRA-TIT-VL
           MOVE  SGTS0113-CD-OPR-ETRD-MCP
                                       TO  SGTSL068-CD-OPR-ETRD-MCP
           MOVE  SGTS0113-NR-VRS-OPR-ETRD
                                       TO  SGTSL068-NR-VRS-OPR-ETRD
      *    Tipo da Taxa = 2 - BFF
           MOVE  0002                  TO  SGTSL068-CD-TIP-TAXA
           MOVE  ULT-DT-INC-VLD        TO  SGTSL068-DT-ULT-INC-VLD

           PERFORM  900000-EXECUTAR-MODULO

           PERFORM  964000-EXCLUIR-CONDICAO
                    VARYING IC
                    FROM 0001 BY 0001
                    UNTIL  IC              GREATER  SGTSL068-QT-OCR-TAB
           .
       354000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       355000-GRAVAR-CONDICAO-BFF      SECTION.
      *-----------------------------------------------------------------
           MOVE  0021                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTSI068-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSI068              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0113-CD-CTRA-TIT-VL
                                       TO  SGTSI068-CD-CTRA-TIT-VL
           MOVE  SGTS0113-CD-OPR-ETRD-MCP
                                       TO  SGTSI068-CD-OPR-ETRD-MCP
           MOVE  SGTS0113-NR-VRS-OPR-ETRD
                                       TO  SGTSI068-NR-VRS-OPR-ETRD

           MOVE  0002                  TO  SGTSI068-CD-TIP-TAXA
           MOVE  '  .  .    '          TO  SGTSI068-DT-INC-CND-TAXA

           MOVE  CORR SGTS0113-DT-INC-CND-GRT-R (IC-BFF)
                                       TO  SGTSI068-DT-INC-CND-TAXA-R
           MOVE  KOPRD-CD-OPRD         TO  SGTSI068-CD-USU-ULT-ATL
           MOVE  'CURRENT'             TO  SGTSI068-TS-ULT-ATL
           MOVE  '  .  .    '          TO  SGTSI068-DT-FIM-CND-TAXA
           MOVE  CORR  SGTS0113-DT-FIM-CND-GRT-R (IC-BFF)
                                       TO  SGTSI068-DT-FIM-CND-TAXA-R
           MOVE  SGTS0113-PC-MIN-GRT  (IC-BFF)
                                       TO  SGTSI068-PC-MIN-TAXA

           PERFORM  900000-EXECUTAR-MODULO
           .
       355000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       360000-VERIFICA-DFRC-PREF       SECTION.
      *-----------------------------------------------------------------
           MOVE  0022                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTS0185              TO  NM-PGM

           MOVE  SGTS0113-CD-CTRA-TIT-VL
                                       TO  SGTS0185-CD-CTRA-TIT-VL
           MOVE  SGTS0113-CD-OPR-ETRD-MCP
                                       TO  SGTS0185-CD-OPR-ETRD-MCP
           MOVE  SGTS0113-NR-VRS-OPR-ETRD
                                       TO  SGTS0185-NR-VRS-OPR-ETRD
           SET   SGTS0185-LS-RMNC      TO  TRUE

           PERFORM 900000-EXECUTAR-MODULO

           MOVE  SGTS0185-QT-OCR-TAB   TO  SGTS0113-QT-OCR-DFRC

           IF  SGTS0185-QT-OCR-TAB         GREATER  ZEROS
               PERFORM  361000-CARREGA-DFRC-PREF
                        VARYING  IC-TAB
                        FROM  0001 BY 0001
                        UNTIL  IC-TAB      GREATER  SGTS0185-QT-OCR-TAB
           END-IF
           .
       360000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       361000-CARREGA-DFRC-PREF        SECTION.
      *-----------------------------------------------------------------
           MOVE  0023                  TO  MSG1-CD-SEQL-ERRO

           MOVE  CORR SGTS0185-DT-INC-CND-TAXA-R(IC-TAB)
             TO  SGTS0113-DT-INC-CND-TAXA-R(IC-TAB)
           MOVE  CORR SGTS0185-DT-FIM-CND-TAXA-R(IC-TAB)
             TO  SGTS0113-DT-FIM-CND-TAXA-R(IC-TAB)
           MOVE  SGTS0185-TX-DCR-TAXA-OPR(IC-TAB)
             TO  SGTS0113-TX-DCR-TAXA-OPR(IC-TAB)
           MOVE  SGTS0185-TX-DCR-DFRC(IC-TAB)
             TO  SGTS0113-TX-DCR-DFRC(IC-TAB)
           .
       361000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       400000-TRATAR-DURATION          SECTION.
      *-----------------------------------------------------------------
           MOVE  0024                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  420000-CONSULTAR-DURATION

           EVALUATE  TRUE
               WHEN  SGTS0222-QT-AA-PZ-RTN-INVS
                                           EQUAL  ZEROS  AND
                     SGTS0222-DT-PSC-CLC   EQUAL  SPACES
                     PERFORM  440000-INCLUIR-DURATION
               WHEN  SGTS0222-QT-AA-PZ-RTN-INVS
                                           NOT EQUAL
                                           SGTS0113-QT-AA-PZ-RTN-INVS OR
                     SGTS0222-DT-PSC-CLC   NOT EQUAL
                                           SGTS0113-DT-PSC-CLC
                     PERFORM  430000-EXCLUIR-DURATION
                     PERFORM  440000-INCLUIR-DURATION
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

           .
       400000-SAI.
           EXIT.



      *-----------------------------------------------------------------
       420000-CONSULTAR-DURATION       SECTION.
      *-----------------------------------------------------------------
           MOVE  0025                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTS0222              TO  NM-PGM

           MOVE  SGTS0113-CD-CTRA-TIT-VL
                                       TO  SGTS0222-CD-CTRA-TIT-VL
           MOVE  SGTS0113-CD-OPR-ETRD-MCP
                                       TO  SGTS0222-CD-OPR-ETRD-MCP
           MOVE  SGTS0113-NR-VRS-OPR-ETRD
                                       TO  SGTS0222-NR-VRS-OPR-ETRD
           MOVE  SPACES                TO  SGTS0222-TS-EXEA-CLC

           PERFORM  900000-EXECUTAR-MODULO

           .
       420000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       430000-EXCLUIR-DURATION         SECTION.
      *-----------------------------------------------------------------
           MOVE  0026                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTS0220              TO  NM-PGM

           MOVE  SGTS0222-CD-CTRA-TIT-VL
                                       TO  SGTS0220-CD-CTRA-TIT-VL
           MOVE  SGTS0222-CD-OPR-ETRD-MCP
                                       TO  SGTS0220-CD-OPR-ETRD-MCP
           MOVE  SGTS0222-NR-VRS-OPR-ETRD
                                       TO  SGTS0220-NR-VRS-OPR-ETRD
           MOVE  SGTS0222-TS-EXEA-CLC-MIN
                                       TO  SGTS0220-TS-EXEA-CLC

           PERFORM  900000-EXECUTAR-MODULO

           .
       430000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       440000-INCLUIR-DURATION         SECTION.
      *-----------------------------------------------------------------
           MOVE  0027                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTS0219              TO  NM-PGM

           MOVE  SGTS0113-CD-CTRA-TIT-VL
                                       TO  SGTS0219-CD-CTRA-TIT-VL
           MOVE  SGTS0113-CD-OPR-ETRD-MCP
                                       TO  SGTS0219-CD-OPR-ETRD-MCP
           MOVE  SGTS0113-NR-VRS-OPR-ETRD
                                       TO  SGTS0219-NR-VRS-OPR-ETRD
           MOVE  SGTS0113-DT-PSC-CLC
                                       TO  SGTS0219-DT-PSC-CLC
           SET   SGTS0219-DURATION-INCIAL
                                       TO  TRUE
           MOVE  ZEROS                 TO  SGTS0219-PC-TIR
           MOVE  SGTS0113-QT-AA-PZ-RTN-INVS
                                       TO  SGTS0219-QT-AA-PZ-RTN-INVS

           PERFORM  900000-EXECUTAR-MODULO

           .
       440000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       500000-FINALIZAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0028                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  980000-GRAVAR-CONTAINER-RPST

           PERFORM  999999-ENCERRAR

           .
       500000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    Section da Faixa de 900000 à 969900 para Demais Rotinas
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *-----------------------------------------------------------------

      *--> Grava o container de requisição do módulo a executar
           PERFORM  982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o módulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

           MOVE  0029                  TO  MSG1-CD-SEQL-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE  NM-PGM

               WHEN  SGTSI068
                     EXEC CICS LINK
                               PROGRAM ( SGTSI068  )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  SGTSE068
                     EXEC CICS LINK
                               PROGRAM ( SGTSE068  )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  SGTSL068
                     EXEC CICS LINK
                               PROGRAM ( SGTSL068  )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  SGTS0128
                     EXEC CICS LINK
                               PROGRAM ( SGTS0128  )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  SGTS0081
                     EXEC CICS LINK
                               PROGRAM ( SGTS0081  )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  SGTS0184
                     EXEC CICS LINK
                               PROGRAM ( SGTS0184  )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  SGTS0185
                     EXEC CICS LINK
                               PROGRAM ( SGTS0185  )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  SGTS0219
                     EXEC CICS LINK
                               PROGRAM ( SGTS0219  )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  SGTS0220
                     EXEC CICS LINK
                               PROGRAM ( SGTS0220  )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  SGTS0222
                     EXEC CICS LINK
                               PROGRAM ( SGTS0222  )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9006        TO  MSG1-CD-ERRO-TS
                     STRING 'Não definida a execução do módulo ' NM-PGM
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                     PERFORM  999000-ERRO
           END-EVALUATE

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CICS   TO  TRUE
                     MOVE  3004        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na execução do módulo ' NM-PGM ', '
                             'RESP  = '    MSG1-RESP1  ', '
                             'RESP2 = '    MSG1-RESP2
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     END-STRING
                     PERFORM  999000-ERRO
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM  972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a leitura do Container de Resposta
           EVALUATE  NM-PGM
               WHEN  SGTS0219
               WHEN  SGTS0220
                     CONTINUE
               WHEN  OTHER
                     PERFORM  973000-LER-RESPOSTA
           END-EVALUATE

           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       920000-CHAMAR-SGTST005          SECTION.
      *-----------------------------------------------------------------
           MOVE  0030                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTST005              TO  NM-PGM
           EXEC CICS LINK
                PROGRAM    ( SGTST005                 )
                COMMAREA   ( SGTST005-DADOS           )
                LENGTH     ( LENGTH OF SGTST005-DADOS )
           END-EXEC

           IF  SGTST005-CD-RTN-PGM         NOT EQUAL  ZEROS  AND 100
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  SGTST005-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
                                              MSG1-TX-ERRO-TS
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST005-CD-RTN-PGM
                                       TO  MSG1-CD-ERRO-TS
                                              MSG1-CD-RTN-PGM-ACND

      *        Monitorar variáveis do módulo chamado
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF
           .
       920000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       9610000-VALIDAR-DATA            SECTION.
      *--> Aciona a Função 01 da Subrotina SBDATAF para validar datas
      *    no Formato Gregoriano
      *-----------------------------------------------------------------
           MOVE  0031                  TO  MSG1-CD-SEQL-ERRO

      *--> ARG02 - 0004 - Formato DDMMAAAA
           MOVE  0004                  TO  ARG02-F01
           MOVE  4095                  TO  AREA-TAM.
           MOVE  'SBDATAF -WRKAREA-FIM'
                                       TO  AREA-FIM.

           CALL  SBDATAF            USING  FUNCAO-F01
                                           WKSBDATA
                                           ARG01-F01
                                           ARG02-F01.

      *--> Return Code
      *    0022 - Conteúdo Não Numérico
      *    0024 - Data Inválida
      *    0046 - Data Fora da Faixa de 01/01/0001 a 31/12/9999
           IF  RETURN-CODE                 NOT EQUAL ZEROS AND 24 AND 22
                                                       AND 0046
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  0067              TO  MSG1-CD-ERRO-TS
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND
               MOVE 'Erro na rotina que valida data.'
                                       TO  MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE  SBDATAF           TO  MSG1-NM-PGM-ACND

      *        Monitorar variáveis do módulo chamado
               MOVE 'FUNCAO-F01'               TO  MSGKW999-NM-VRV-MNT
               MOVE  FUNCAO-F01                TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG01-F01'                TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG01-F01                 TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG02-F01'                TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG02-F01                 TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               PERFORM  999000-ERRO

           END-IF

           IF  RETURN-CODE                 EQUAL 24  OR  22
               SET   ERRO-PROGRAMA-ACIONADO    TO  TRUE
               MOVE  0049              TO  MSG1-CD-ERRO-TS
               MOVE 'Data informada inválida.'
                                       TO  MSG1-TX-ERRO-TS
                                           MSG1-TX-RTN-PGM-ACND
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SBDATAF           TO  MSG1-NM-PGM-ACND

      *        Monitorar variáveis do módulo chamado
               MOVE 'FUNCAO-F01'       TO  MSGKW999-NM-VRV-MNT
               MOVE  FUNCAO-F01        TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG01-F01'        TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG01-F01         TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG02-F01'        TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG02-F01         TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               PERFORM  999000-ERRO
           END-IF
           .
       961000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       962000-CALCULAR-DIA             SECTION.
      *-----------------------------------------------------------------
           MOVE  0032                  TO  MSG1-CD-SEQL-ERRO

           MOVE  4095                  TO  AREA-TAM.
           MOVE  'SBDATAF -WRKAREA-FIM'
                                       TO  AREA-FIM.

           CALL  SBDATAF            USING  FUNCAO-F05
                                           WKSBDATA
                                           ARG01-F05 ARG02-F05
                                           ARG03-F05 ARG04-F05.

      *--> Return Code
      *    0022 - Conteúdo Não Numérico
      *    0024 - Data Inválida
      *    0046 - Data Fora da Faixa de 01/01/0001 a 31/12/9999
           IF  RETURN-CODE                 NOT EQUAL ZEROS AND 24 AND 22
                                                       AND 0046
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               MOVE  0567              TO  MSG1-CD-ERRO-TS
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND
               MOVE 'Erro ao calcular dia.'
                                       TO  MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE  SBDATAF           TO  MSG1-NM-PGM-ACND
      *        Monitorar variáveis do módulo chamado
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF

           IF  RETURN-CODE                 EQUAL  0024
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0049              TO  MSG1-CD-ERRO-TS
               MOVE 'Data informada inválida.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF
           .
       962000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       963000-CALCULAR-DIAS-CORRIDOS   SECTION.
      *--> Aciona a Função 03 da Subrotina SBDATAF para Calcular
      *    Dias Corridos entre Duas Datas (máximo 99999 dias)
      *-----------------------------------------------------------------
           MOVE  0033                  TO  MSG1-CD-SEQL-ERRO

           MOVE  4095                  TO  AREA-TAM.
           MOVE  'SBDATAF -WRKAREA-FIM'
                                       TO  AREA-FIM.
           CALL  SBDATAF            USING  FUNCAO-F03
                                           WKSBDATA
                                           ARG01-F03
                                           ARG02-F03
                                           ARG03-F03.

      *--> Return Code
      *    0022 - Conteúdo Não Numérico
      *    0024 - Data Inválida
      *    0046 - Data Fora da Faixa de 01/01/0001 a 31/12/9999
             IF  RETURN-CODE               NOT EQUAL ZEROS AND 24 AND 22
                                           AND 0046
                  SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                  MOVE  0568           TO  MSG1-CD-ERRO-TS
                  MOVE  RETURN-CODE    TO  MSG1-CD-RTN-PGM-ACND
                  MOVE 'Erro ao calcular dias corridos.'
                                       TO  MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
                  MOVE  SBDATAF        TO  MSG1-NM-PGM-ACND
      *           Monitorar variáveis do módulo chamado
                  PERFORM  969000-MONITORAR-VARIAVEIS
                  PERFORM  999000-ERRO
             END-IF

             IF  RETURN-CODE               EQUAL  0024
                  SET   ERRO-PROGRAMADO
                                       TO  TRUE
                  MOVE  0049           TO  MSG1-CD-ERRO-TS
                  MOVE  'Data informada inválida.'
                                       TO  MSG1-TX-ERRO-TS
                  PERFORM  999000-ERRO
             END-IF

             MOVE  ARG03-F03           TO  QT-DIAS-CORRIDOS
           .
       963000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       964000-EXCLUIR-CONDICAO         SECTION.
      *-----------------------------------------------------------------
           MOVE  0034                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTSE068-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSE068              TO  NM-PGM

      *--> Movimentar os campos de entrada

           MOVE  SGTSL068-CD-CTRA-TIT-VL
                                       TO  SGTSE068-CD-CTRA-TIT-VL
           MOVE  SGTSL068-CD-OPR-ETRD-MCP
                                       TO  SGTSE068-CD-OPR-ETRD-MCP
           MOVE  SGTSL068-NR-VRS-OPR-ETRD
                                       TO  SGTSE068-NR-VRS-OPR-ETRD
           MOVE  SGTSL068-CD-TIP-TAXA
                                       TO  SGTSE068-CD-TIP-TAXA
           MOVE  '  .  .    '          TO  SGTSE068-DT-INC-CND-TAXA
           MOVE  CORR  SGTSL068-DT-INC-CND-TAXA-R(IC)
                                       TO  SGTSE068-DT-INC-CND-TAXA-R

           PERFORM  900000-EXECUTAR-MODULO

           IF  SGTSL068-IN-CNTO-PSQ        EQUAL  'S'
               MOVE  CORR SGTSL068-DT-INC-CND-TAXA-R (IC)
                                       TO  ULT-DT-INC-VLD-R
           END-IF
           .
       964000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM
               WHEN  SGTST005
                     MOVE 'T005001-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T005002-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE SGTST005-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T005003-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------------> Verifica se o Conteudo é Numérico
                     IF  SGTST005-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTST005-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T005004-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T005005-DT-EMS-PREF'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-DT-EMS-PREF
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T005006-DT-VNCT-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-DT-VNCT-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T005007-QT-DD-PZ-VLD-PREF'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-QT-DD-PZ-VLD-PREF
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T005008-IN-RMNC-VRV'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-IN-RMNC-VRV
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T005009-QT-MM-CARE-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-QT-MM-CARE-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTS0128
                     MOVE '0128-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0128-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0128-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------------> Verifica se o Conteudo é Numérico
                     IF  SGTS0128-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTS0128-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0128-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0128-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0128-004-CD-TIP-IN '
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0128-CD-TIP-IN
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0128-005-IN-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0128-IN-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSI068
                     MOVE 'I068-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI068-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I068-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------------> Verifica se o Conteudo é Numérico
                     IF  SGTSI068-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTSI068-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I068-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI068-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I068-004-CD-TIP-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI068-CD-TIP-TAXA
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I068-005-DT-INC-CND-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI068-DT-INC-CND-TAXA
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I068-006-CD-USU-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI068-CD-USU-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I068-007-TS-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI068-TS-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I068-008-DT-FIM-CND-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI068-DT-FIM-CND-TAXA
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I068-009-PC-MIN-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
      *------------> Verifica se o Conteudo é Numérico
                     IF  SGTSI068-PC-MIN-TAXA
                                           NUMERIC
                         MOVE  SGTSI068-PC-MIN-TAXA
                                       TO  VL-04V10
                         MOVE  VL-04V10
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I068-010-CD-TIP-APL-PREF'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI068-CD-TIP-APL-PREF
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I068-011-CD-PERC-APL-PREF'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI068-CD-PERC-APL-PREF
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I068-012-CD-IEC-PREF'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI068-CD-IEC-PREF
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSE068
                     MOVE 'E068-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE068-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'E068-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------------> Verifica se o Conteudo é Numérico
                     IF  SGTSE068-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTSE068-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'E068-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE068-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'E068-004-CD-TIP-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE068-CD-TIP-TAXA
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'E068-005-DT-INC-CND-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE068-DT-INC-CND-TAXA
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSL068
                     MOVE 'L068-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL068-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L068-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------------> Verifica se o Conteudo é Numérico
                     IF  SGTSL068-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTSL068-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L068-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL068-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L068-004-CD-TIP-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL068-CD-TIP-TAXA
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L068-005-DT-ULT-INC-VLD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL068-DT-ULT-INC-VLD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTS0081
                     MOVE '0081-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0081-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0081-002-CD-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
      *------------> Verifica se o Conteudo é Numérico
                     IF  SGTS0081-CD-OPR-ETRD
                                           NUMERIC
                         MOVE  SGTS0081-CD-OPR-ETRD
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0081-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0081-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0081-004-IN-HBLT-MENU-PREF '
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0081-IN-HBLT-MENU-PREF
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTS0185
                     MOVE '0185-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0185-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0185-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------------> Verifica se o Conteudo é Numérico
                     IF  SGTS0185-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTS0185-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                    ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0185-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0185-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0185-004-CD-LS-RTN'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0185-CD-LS-RTN
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTS0184
                     MOVE '0184-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0184-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0184-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------------> Verifica se o Conteudo é Numérico
                     IF  SGTS0184-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTS0184-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                    ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0184-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0184-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0184-004-IN-RMNC-VRV'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-IN-RMNC-VRV
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0184-005-DT-INI-RMNC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  DT-INI-RMNC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0184-006-DT-INI-PREF'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  DT-INI-PREF
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTS0219
                     MOVE '0219-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0219-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0219-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------------> Verifica se o Conteudo é Numérico
                     IF  SGTS0219-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTS0219-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0219-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0219-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0219-004-DT-PSC-CLC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0219-DT-PSC-CLC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0219-005-IN-CLC-INC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0219-IN-CLC-INC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0219-006-PC-TIR'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTS0219-PC-TIR   NUMERIC
                         MOVE  SGTS0219-PC-TIR
                                       TO  VL-04V10
                         MOVE  VL-04V10
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0219-007-QT-AA-PZ-RTN-INVS'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTS0219-QT-AA-PZ-RTN-INVS
                                           NUMERIC
                         MOVE  SGTS0219-QT-AA-PZ-RTN-INVS
                                       TO  VL-04V10
                         MOVE  VL-04V10
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTS0220
                     MOVE '0220-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0220-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0220-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------------> Verifica se o Conteudo é Numérico
                     IF  SGTS0220-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTS0220-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0220-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0220-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0220-004-TS-EXEA-CLC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0220-TS-EXEA-CLC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTS0222
                     MOVE '0222-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0222-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0222-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------------> Verifica se o Conteudo é Numérico
                     IF  SGTS0222-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTS0222-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0222-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0222-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0222-004-TS-EXEA-CLC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0220-TS-EXEA-CLC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                     CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    Section da Faixa de 970000 a 979900 para Leitura de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       970000-LER-CANAL                SECTION.
      *--> Obtem o canal corrente
      *-----------------------------------------------------------------
           MOVE  0035                  TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS  ASSIGN
                      CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       970000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------
           MOVE  0036                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisição
           MOVE  'SGTS0113'            TO  NM-PGM
           MOVE  'SGTS0113-RQSC'       TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM  973100-LER-CONTAINER-NODATA

           MOVE  0037                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER ( NM-CTNR       )
                     FLENGTH   ( QT-CCT-LIDO   )
                     INTO      ( SGTS0113-RQSC )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *    A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------
           MOVE  0038                  TO  MSG1-CD-SEQL-ERRO

      *--> Le o container de erro dos modulos chamados
           SET   GET-CONTAINER         TO  TRUE

           MOVE  'PRM-ERRO'            TO  NM-CTNR
           INITIALIZE                      IIBKERRO-PRM-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                     )
                      INTO      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      RESP      ( MSG1-RESP1                  )
                      RESP2     ( MSG1-RESP2                  )
           END-EXEC

      *--> Caso deseje personalizar ou não interromper o
      *    processamento, alterar para não dar erro aqui SOMENTE
      *    para o Modulo Chamado e tratar na section chamadora
           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
                     CONTINUE

               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                IF  NM-PGM            EQUAL  'SGTS0128'  AND
                    IIBKERRO-CD-ERRO  EQUAL  9002
                    CONTINUE
                ELSE
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
                    STRING  'Erro ao acionar o módulo ' NM-PGM      '.'
                            DELIMITED BY SIZE
                            INTO        MSG1-TX-ERRO-TS
                    END-STRING
                    MOVE IIBKERRO-CD-ERRO     TO  MSG1-CD-RTN-PGM-ACND
                    MOVE IIBKERRO-TX-ERRO     TO  MSG1-TX-RTN-PGM-ACND
                 END-IF
      *-->          Monitorar variáveis do módulo chamado
                         PERFORM 969000-MONITORAR-VARIAVEIS
                         PERFORM 999000-ERRO
                END-IF
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *--> Recupera o container com os dados de resposta (saída).
      *    Modulo CONTAINER  -     NOME_PGM-RPST
      *    Modulo IIB (FIXO) -     PRM-DADOS-RPST
      *-----------------------------------------------------------------
           MOVE  0039                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com módulo a executar
           MOVE  SPACES                TO  NM-CTNR

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)

      *--> Recupera o tamanho do container de resposta
           PERFORM  973100-LER-CONTAINER-NODATA

           MOVE  0040                  TO  MSG1-CD-SEQL-ERRO

           IF  NM-PGM                      NOT EQUAL  'SGTSL068'  AND
                                                      'SGTS0184'  AND
                                                      'SGTS0185'
      *------> Obtem o tamanho esperado da resposta
               EVALUATE  NM-PGM
                   WHEN  SGTST005
                         MOVE  LENGTH OF SGTST005-DADOS
                                       TO  QT-CCT-EPRO
                   WHEN  SGTSI068
                         MOVE  LENGTH OF SGTSI068-RPST
                                       TO  QT-CCT-EPRO
                   WHEN  SGTSE068
                         MOVE  LENGTH OF SGTSE068-RPST
                                       TO  QT-CCT-EPRO
                   WHEN  SGTS0081
                         MOVE  LENGTH OF SGTS0081-RPST
                                       TO  QT-CCT-EPRO
                   WHEN  SGTS0128
                         MOVE  LENGTH OF SGTS0128-RPST
                                       TO  QT-CCT-EPRO
                   WHEN  SGTS0222
                         MOVE  LENGTH OF SGTS0222-RPST
                                       TO  QT-CCT-EPRO
               END-EVALUATE
      *------> Valida tamanho esperado da resposta X tamanho recebido
               IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
                   SET   ERRO-TAMANHO-AREA
                                       TO  TRUE
                   PERFORM  999000-ERRO
               END-IF
           END-IF

      *--> Configura dados para leitura do container de resposta
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Le a RESPOSTA de acordo com o programa que a gravou
           MOVE  0041                  TO  MSG1-CD-SEQL-ERRO

           EVALUATE  NM-PGM
               WHEN  SGTSI068
                     INITIALIZE            SGTSI068-RPST
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR       )
                               INTO      ( SGTSI068-RPST )
                               FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC
               WHEN  SGTSE068
                     INITIALIZE            SGTSE068-RPST
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR       )
                               INTO      ( SGTSE068-RPST )
                               FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC
               WHEN  SGTSL068
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR       )
                               INTO      ( SGTSL068-RPST )
                               FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC
               WHEN  SGTS0128
                     INITIALIZE            SGTS0128-RPST
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR       )
                               INTO      ( SGTS0128-RPST )
                               FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC
               WHEN  SGTS0081
                     INITIALIZE            SGTS0081-RPST
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR       )
                               INTO      ( SGTS0081-RPST )
                               FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC
               WHEN  SGTS0184
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR       )
                               INTO      ( SGTS0184-RPST )
                               FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC
               WHEN  SGTS0185
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR       )
                               INTO      ( SGTS0185-RPST )
                               FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC
               WHEN  SGTS0222
                     INITIALIZE            SGTS0222-RPST
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR       )
                               INTO      ( SGTS0222-RPST )
                               FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9006        TO  MSG1-CD-ERRO-TS
                     MOVE  'Não definida a execução do módulo.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-ERRO
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------
           MOVE  0042                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Obtem a quantidade de caracteres no container
           MOVE  ZEROS                 TO  QT-CCT-LIDO
           EXEC CICS  GET
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
       974000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE  0043                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-INF-OPRD'        TO  NM-CTNR

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           INITIALIZE                      PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       974000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *   Section da Faixa de 980000 a 989900 para Gravacao de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------

      *--> Exclui eventual container de erro de outros programas
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO

           MOVE  0044                  TO  MSG1-CD-SEQL-ERRO

           IF DT-VNCT-PREF-R  LESS  DT-CPR-OPR-R
             MOVE  'Sucesso no processamento, mas Precificação Vencida.'
                                       TO  SGTS0113-TX-RTN-PRCT
           ELSE
             MOVE  'Sucesso no processamento da Precificação.'
                                       TO  SGTS0113-TX-RTN-PRCT
           END-IF

      *--> Monta o nome do container de resposta
           MOVE  'SGTS0113-RPST'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                 )
                      FROM      ( SGTS0113-RPST           )
                      FLENGTH   ( LENGTH OF SGTS0113-RPST )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       980000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *--> Postar container com dados de requisição (entrada)
      *    Modulo CONTAINER  -     NOME_PGM-RQSC
      *    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
      *-----------------------------------------------------------------
           MOVE  0045                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com módulo a executar
           MOVE  SPACES                TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_modulo -RQSC
           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RQSC'               TO  NM-CTNR(9:5)

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Faz PUT da requisiçcão do programa a executar
           EVALUATE  NM-PGM
               WHEN  SGTSI068
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTSI068-RQSC           )
                               FLENGTH   ( LENGTH OF SGTSI068-RQSC )
                     END-EXEC
               WHEN  SGTSE068
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTSE068-RQSC           )
                               FLENGTH   ( LENGTH OF SGTSE068-RQSC )
                     END-EXEC
               WHEN  SGTSL068
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTSL068-RQSC           )
                               FLENGTH   ( LENGTH OF SGTSL068-RQSC )
                     END-EXEC
               WHEN  SGTS0128
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTS0128-RQSC           )
                               FLENGTH   ( LENGTH OF SGTS0128-RQSC )
                     END-EXEC
               WHEN  SGTS0081
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTS0081-RQSC           )
                               FLENGTH   ( LENGTH OF SGTS0081-RQSC )
                     END-EXEC
               WHEN  SGTS0184
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTS0184-RQSC           )
                               FLENGTH   ( LENGTH OF SGTS0184-RQSC )
                     END-EXEC
               WHEN  SGTS0185
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTS0185-RQSC           )
                               FLENGTH   ( LENGTH OF SGTS0185-RQSC )
                     END-EXEC
               WHEN  SGTS0219
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTS0219-RQSC           )
                               FLENGTH   ( LENGTH OF SGTS0219-RQSC )
                     END-EXEC
               WHEN  SGTS0220
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTS0220-RQSC           )
                               FLENGTH   ( LENGTH OF SGTS0220-RQSC )
                     END-EXEC
               WHEN  SGTS0222
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTS0222-RQSC           )
                               FLENGTH   ( LENGTH OF SGTS0222-RQSC )
                     END-EXEC
               WHEN  OTHER
                     MOVE  9006         TO  MSG1-CD-ERRO-TS
                     MOVE  'Não definida a execução do módulo.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-ERRO
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       982000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *--> Exclui o container de resposta e de erro para evitar
      *    problemas em modulos executados mais de uma vez
      *    dentro da mesma sessao do CICS.
      *    Equivale ao INITIALIZE na area de resposta dos programas
      *    que utilizam COMMAREA.
      *-----------------------------------------------------------------
           MOVE  0046                  TO  MSG1-CD-SEQL-ERRO

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
           MOVE  0047                  TO  MSG1-CD-SEQL-ERRO

      *--> Limpar variáveis container de erro
           INITIALIZE                      IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           SET   DEL-CONTAINER         TO  TRUE

      *--> Não há captura automática de erro CICS por usar RESP e
      *    RESP2
           EXEC CICS DELETE
                      CONTAINER ( NM-CTNR    )
                      RESP      ( MSG1-RESP1 )
                      RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE TRUE
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
      * As sections abaixo iniciadas com 999000 representam os tratamen
      * tos de erro utilizando o Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-ERRO                     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

           PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-ERRO-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *--> Em caso de erro, desfaz as atualizações em banco de dados
           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC

      *--> Verifica se o tipo de erro gerado possui tratamento interno
           EVALUATE TRUE
               WHEN  ERRO-CONTAINER
                    PERFORM  999004-TRATA-ERRO-CTNR
               WHEN  ERRO-TAMANHO-AREA
                    PERFORM  999005-TRATA-ERRO-AREA-DADOS
               WHEN  ERRO-CANAL
                    PERFORM  999006-TRATA-ERRO-CANAL
               WHEN  OTHER
                    CONTINUE
           END-EVALUATE

      *--> Adiciona Chave da Operação Estruturada
           MOVE '0113-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0113-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0113-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o Conteudo é Numérico Antes de Adicionar
           IF  SGTS0113-CD-OPR-ETRD-MCP    NUMERIC
               MOVE  SGTS0113-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0113-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0113-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Adiciona Campos de Entrada da Precificação
           MOVE '0113-004-DT-EMS-PREF'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0113-DT-EMS-PREF
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0113-005-QT-DD-PZ-VLD-PREF'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0113-QT-DD-PZ-VLD-PREF
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0113-006-PC-TARF-GRT-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o Conteudo é Numérico Antes de Adicionar
           IF  SGTS0113-PC-TARF-GRT-TAXA   NUMERIC
               MOVE  SGTS0113-PC-TARF-GRT-TAXA
                                       TO  VL-04V10
               MOVE  VL-04V10
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0113-007-IN-FCTR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0113-IN-FCTR
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0113-008-IN-RMNC-VRV'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0113-IN-RMNC-VRV
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Adiciona Campos de Entrada da Duration Inicial
           MOVE '0113-009-IN-CLC-INC'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0113-IN-CLC-INC
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0113-010-DT-PSC-CLC'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0113-DT-PSC-CLC
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT


           MOVE '0113-011-QT-AA-PZ-RTN-INVS'
                                       TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o Conteudo é Numérico Antes de Adicionar
           IF  SGTS0113-QT-AA-PZ-RTN-INVS  NUMERIC
               MOVE  SGTS0113-QT-AA-PZ-RTN-INVS
                                       TO  VL-04V10
               MOVE  VL-04V10
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT


      *--> Verifica se o erro foi em registro de Tabela
           IF  COM-ERRO-TABX
               PERFORM  999100-ADICIONA-OCR-TABX
           END-IF

           IF  COM-ERRO-TABY
               PERFORM  999200-ADICIONA-OCR-TABY
           END-IF

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

      *-----------------------------------------------------------------
       999100-ADICIONA-OCR-TABX        SECTION.
      *--> Adiciona Ocorrencia da Tabela 01 para Monitoracao
      *-----------------------------------------------------------------

           MOVE  NM-IC-TABX            TO  MSGKW999-NM-VRV-MNT
           MOVE  VL-IC-TABX            TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0113-012-DT-INC-CND-JUR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0113-DT-INC-CND-JUR (VL-IC-TABX)
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0113-013-DT-FIM-CND-JUR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0113-DT-FIM-CND-JUR (VL-IC-TABX)
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0113-014-PC-MIN-JUR'  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0113-PC-MIN-JUR (VL-IC-TABX)
                                       TO  VL-04V10
           MOVE  VL-04V10              TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0113-015-CD-TIP-APL-PREF'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0113-CD-TIP-APL-PREF (VL-IC-TABX)
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0113-016-CD-IEC-PREF'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0113-CD-IEC-PREF (VL-IC-TABX)
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0113-017-CD-PERC-APL-PREF'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0113-CD-PERC-APL-PREF (VL-IC-TABX)
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT
           .
       999100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999200-ADICIONA-OCR-TABY        SECTION.
      *--> Adiciona Ocorrencia da Tabela 01 para Monitoracao
      *-----------------------------------------------------------------

           MOVE  NM-IC-TABY            TO  MSGKW999-NM-VRV-MNT
           MOVE  VL-IC-TABY            TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0113-018-DT-INC-CND-GRT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0113-DT-INC-CND-GRT (VL-IC-TABY)
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0113-019-DT-FIM-CND-GRT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0113-DT-FIM-CND-GRT (VL-IC-TABY)
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0113-020-PC-MIN-GRT'  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0113-PC-MIN-GRT (VL-IC-TABY)
                                       TO  VL-04V10
           MOVE  VL-04V10              TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT
           .
       999200-SAI.
           EXIT.

      *----------------------------------------
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
      *---------------------- FIM SGTS0113 -----------------------------