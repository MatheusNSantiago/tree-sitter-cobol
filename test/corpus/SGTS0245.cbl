      *-----------------------------------------------------------------
      * Programa..: SGTS0245
      * Analista..: F8785286 - Roseli Akemi Kawahata
      * Autor.....: F8785286 - Roseli Akemi Kawahata
      * Data......: 12/04/2019
      * Objetivo..: Formatar Ordem de Negociação Compra - Dados Operação
      * Compilacao: Cobol/CICS
      *-----------------------------------------------------------------
      * VRS005 03/07/2024 - C1334842 - Recompilação devido a alteração
      *                     do book SGTK0083
      * VRS004 05/12/2023 F9507141 - Recompilado devido a alteração no
      *        book SGTK0083 - Inclusão dos campos: TX-CMCP, TX-GR-MCP,
      *        TX-LVRO-MC-CPTL, TX-SMCP, CD-TIP-VRS-OPR.
      * VRS003 22/04/2020 - F0950413 DE 2016/145195 - Inclusão da forma-
      *        tação dos campos referente à Categoria BACEN.
      * VRS002 05/12/2019 - F0950413 DE 2016/145195 - Alteração  varia-
      *        vel CD-TIP-APL-PREF-EDIT de PIC ZZ.ZZ9, para PIC 99.999.
      *        19/09/2019 F8785286 DE 2016/145195 - Re-compilação por
      *            alteração no book SGTKLA75. Sem alter neste pgm.
      * VRS001 15/07/2019 - F8785286 - Acerto na formatação campo
      *                PC-TAXA-RMNC-EDIT.
      *        26/06/2019 - F8090555 - Inibe validação de estado da OPR.
      *        12/04/2019 - F8785286 - DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0245.
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

      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-SS               PIC  X(40) VALUE
                                       '*** S.S. Comeca Aqui ***'.
       77  CTE-PROG                    PIC  X(18) VALUE
                                       '*** SGTS0245 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS005'.

      *--> Definição dos Nomes dos Programas Acionados
      *--> Módulos acionados / Padrão para Cobolito
       01  TAB-PROGRAMAS-CHAMADOS.
           03  IIBP2000                PIC  X(08)     VALUE  'IIBP2000'.
           03  SGTS0083                PIC  X(08)     VALUE  'SGTS0083'.
           03  SGTS0092                PIC  X(08)     VALUE  'SGTS0092'.
           03  SGTS0155                PIC  X(08)     VALUE  'SGTS0155'.
           03  SGTS0156                PIC  X(08)     VALUE  'SGTS0156'.
           03  SGTS0158                PIC  X(08)     VALUE  'SGTS0158'.
           03  SGTSL052                PIC  X(08)     VALUE  'SGTSL052'.
           03  SGTSL068                PIC  X(08)     VALUE  'SGTSL068'.
           03  SGTSL074                PIC  X(08)     VALUE  'SGTSL074'.
           03  SGTSLA75                PIC  X(08)     VALUE  'SGTSLA75'.
      *    03  SGTST009                PIC  X(08)     VALUE  'SGTST009'.
           03  ATCS6000                PIC  X(08)     VALUE  'ATCS6000'.
           03  SBDATAF                 PIC  X(08)     VALUE  'SBDATAF'.
           03  SBTIMEDT                PIC  X(08)     VALUE  'SBTIMEDT'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho

      *--> Auxiliares para movimentação na monitoração de variáveis     .
       01  CD-CTRA-TVM-TX.
           03  CD-CTRA-TVM-DISP        PIC  9(9).

       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP-DISP    PIC  9(11).

       01  CPOS-AUX.
           03  TX-TIP-APL-PREF         PIC  X(15).
           03  CD-BCO-CRD-DISP         PIC  9(5).
           03  CD-DEPE-CRD-DISP        PIC  9(5).
           03  NR-CC-CRD-DISP          PIC  9(11).
           03  CD-CLI-CDT-DISP         PIC  9(10).
           03  CD-CLI-EMT-DISP         PIC  9(10).
           03  NR-EMS-TIT-DISP         PIC  9(10).
           03  NR-SRE-EMS-TIT-DISP     PIC  9(10).
           03  CD-ITC-CVM-DISP         PIC  9(5).
           03  CD-TIP-COTA-DISP        PIC  9(5).
           03  AUX-NM-USU              PIC  X(80).
           03  VL-CPR-OPR              PIC  9(15)V99.
           03  VL-PU-CPR               PIC  9(15)V99.
           03  QT-MM-DRCO-OPR-DISP     PIC  9(4)V99.
           03  QT-AA-DRCO-OPR-DISP     PIC  9(4)V99.
           03  PC-TARF-OPR-ETRO-DISP   PIC  9(4)V9(10).
           03  PC-MIN-TAXA-DISP        PIC  9(4)V9(10).
           03  QT-PAPL-TIT-CPR         PIC  9(10).
           03  CD-TIP-APL-PREF         PIC  9(5).

           03  NR-CPF-DISP             PIC  9(11).
           03  NR-CPF-DISP-R           REDEFINES NR-CPF-DISP.
               05  NR-CPF-P1-DISP      PIC  9(3).
               05  NR-CPF-P2-DISP      PIC  9(3).
               05  NR-CPF-P3-DISP      PIC  9(3).
               05  NR-CPF-P4-DISP      PIC  9(2).

           03  NR-CNPJ-DISP            PIC  9(14).
           03  NR-CNPJ-DISP-R          REDEFINES NR-CNPJ-DISP.
               05  NR-CNPJ-P1-DISP     PIC  9(2).
               05  NR-CNPJ-P2-DISP     PIC  9(3).
               05  NR-CNPJ-P3-DISP     PIC  9(3).
               05  NR-CNPJ-P4-DISP     PIC  9(4).
               05  NR-CNPJ-P5-DISP     PIC  9(2).

       01  CD-BCO-CRD-EDIT             PIC  ZZ.ZZ9.
       01  CD-DEPE-CRD-EDIT            PIC  ZZ.ZZ9.
       01  NR-CC-CRD-EDIT              PIC  ZZ.ZZZ.ZZZ.ZZ9.
       01  CD-CLI-EMT-EDIT             PIC  Z.ZZZ.ZZZ.ZZ9.
       01  NR-EMS-TIT-EDIT             PIC  Z.ZZZ.ZZZ.ZZ9.
       01  NR-SRE-EMS-EDIT             PIC  Z.ZZZ.ZZZ.ZZ9.
       01  CD-ITC-CVM-EDIT             PIC  ZZ.ZZ9.
       01  CD-TIP-COTA-EDIT            PIC  ZZ.ZZ9.
       01  PC-TAXA-RMNC-EDIT           PIC  Z.ZZ9,9999999999.
       01  PC-MIN-TAXA-EDIT            PIC  Z.ZZ9,9999999999.
       01  QT-MM-DRCO-OPR-EDIT         PIC  Z.ZZ9,99.
       01  QT-AA-DRCO-OPR-EDIT         PIC  Z.ZZ9,99.
       01  VL-CPR-OPR-EDIT             PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       01  VL-PU-CPR-EDIT              PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       01  PC-TARF-OPR-ETRO-EDIT       PIC  Z.ZZ9,9999999999.
       01  QT-PAPL-TIT-CPR-EDIT        PIC  Z.ZZZ.ZZZ.ZZ9.
       01  CD-TIP-APL-PREF-EDIT        PIC  99.999.

       01  NR-CPF-EDIT.
           03  NR-CPF-P1-EDIT          PIC  9(3).
           03  NR-CPF-PTO1             PIC  X(1).
           03  NR-CPF-P2-EDIT          PIC  9(3).
           03  NR-CPF-PTO2             PIC  X(1).
           03  NR-CPF-P3-EDIT          PIC  9(3).
           03  NR-CPF-TRO1             PIC  X(1).
           03  NR-CPF-P4-EDIT          PIC  9(2).

       01  NR-CNPJ-EDIT.
           03  NR-CNPJ-P1-EDIT         PIC  9(2).
           03  NR-CNPJ-PTO1            PIC  X(1).
           03  NR-CNPJ-P2-EDIT         PIC  9(3).
           03  NR-CNPJ-PTO2            PIC  X(1).
           03  NR-CNPJ-P3-EDIT         PIC  9(3).
           03  NR-CNPJ-TRO1            PIC  X(1).
           03  NR-CNPJ-P4-EDIT         PIC  9(4).
           03  NR-CNPJ-TRO2            PIC  X(1).
           03  NR-CNPJ-P5-EDIT         PIC  9(2).

       01  QT-BCO                      PIC S9(4) COMP-5 VALUE 0.
       01  TAM-CD-BCO-CRD              PIC S9(4) COMP-5 VALUE 0.
       01  TAM-CD-DEPE-CRD             PIC S9(4) COMP-5 VALUE 0.
       01  TAM-NR-CC-CRD               PIC S9(4) COMP-5 VALUE 0.
       01  TAM-CD-CLI-EMT              PIC S9(4) COMP-5 VALUE 0.
       01  TAM-NR-EMS-TIT              PIC S9(4) COMP-5 VALUE 0.
       01  TAM-NR-SRE-EMS              PIC S9(4) COMP-5 VALUE 0.
       01  TAM-CD-ITC-CVM              PIC S9(4) COMP-5 VALUE 0.
       01  TAM-CD-TIP-COTA             PIC S9(4) COMP-5 VALUE 0.
       01  TAM-PC-TARF-OPR             PIC S9(4) COMP-5 VALUE 0.

       01  INI-CD-BCO-CRD              PIC S9(4) COMP-5 VALUE 0.
       01  INI-CD-DEPE-CRD             PIC S9(4) COMP-5 VALUE 0.
       01  INI-NR-CC-CRD               PIC S9(4) COMP-5 VALUE 0.
       01  INI-CD-CLI-EMT              PIC S9(4) COMP-5 VALUE 0.
       01  INI-NR-EMS-TIT              PIC S9(4) COMP-5 VALUE 0.
       01  INI-NR-SRE-EMS              PIC S9(4) COMP-5 VALUE 0.
       01  INI-CD-ITC-CVM              PIC S9(4) COMP-5 VALUE 0.
       01  INI-CD-TIP-COTA             PIC S9(4) COMP-5 VALUE 0.
       01  INI-PC-TARF-OPR             PIC S9(4) COMP-5 VALUE 0.

       01  WS-POS                      PIC S9(04)  COMP-5 VALUE 1.
       01  IDX-MSG                     PIC S9(4)   COMP-5 VALUE 0.
       01  AUX-ACHOU                   PIC  X(1)   VALUE ' '.
       01  IC-TAB                      PIC S9(4)   COMP-5 VALUE 0.

      *Período de Remuneração
       01 PER-RMNC-OPR.
          03 DT-INC-RMNC-OPR           PIC  X(10) VALUE '  /  /    '.
          03 DT-INC-RMNC-OPR-R         REDEFINES  DT-INC-RMNC-OPR.
             05 DIA                    PIC  9(2).
             05                        PIC  X(01).
             05 MES                    PIC  9(2).
             05                        PIC  X(01).
             05 ANO                    PIC  9(4).

          03 TX-ESP                    PIC  X(5).

          03 DT-FIM-RMNC-OPR           PIC  X(10) VALUE '  /  /    '.
          03 DT-FIM-RMNC-OPR-R         REDEFINES  DT-FIM-RMNC-OPR.
             05 DIA                    PIC  9(2).
             05                        PIC  X(01).
             05 MES                    PIC  9(2).
             05                        PIC  X(01).
             05 ANO                    PIC  9(4).

      *Período de BFF
       01 PER-CND-TAXA.
          03 DT-INC-CND-TAXA           PIC  X(10) VALUE '  /  /    '.
          03 DT-INC-CND-TAXA-R         REDEFINES  DT-INC-CND-TAXA.
             05 DIA                    PIC  9(2).
             05                        PIC  X(01).
             05 MES                    PIC  9(2).
             05                        PIC  X(01).
             05 ANO                    PIC  9(4).

          03 TX-ESP-BFF                PIC  X(5).

          03 DT-FIM-CND-TAXA           PIC  X(10) VALUE '  /  /    '.
          03 DT-FIM-CND-TAXA-R         REDEFINES  DT-FIM-CND-TAXA.
             05 DIA                    PIC  9(2).
             05                        PIC  X(01).
             05 MES                    PIC  9(2).
             05                        PIC  X(01).
             05 ANO                    PIC  9(4).

      *--> Campo mensagem
       01  IN-TB-MSG                   PIC X(1).

       01  TX-MSG-ORD-NEGO             PIC X(600).
       01  TX-MSG-ORD-NEGO-R           REDEFINES TX-MSG-ORD-NEGO.
           03 LOCAL                    PIC  X(50).
           03 QT-TAB-ITEM              PIC  9(03).
           03                          PIC  X(01).
           03 TAB-OCR                  OCCURS 100 TIMES.
              05 TAB-ITEM              PIC  9(03).
              05                       PIC  X(01).
      *   Tabela BFF
       01 TABELAS.
          03 TAB-OCR-BFF               OCCURS 0100 TIMES.
             05 PER-CND-TAXA-BFF       PIC X(25).
             05 TX-TIP-APL-PREF-BFF    PIC X(40) VALUE SPACES.

      *   Tabela Remununeração
          03 TAB-OCR-RMNC              OCCURS  0100 TIMES.
             05 PER-RMNC-OPR-RMNC      PIC  X(25).
             05 TAXA-RMNC-OPR          PIC  X(40).

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B

      *--> Book dos dados do usuário
-INC IIBKOPRD

      *--> Book do Container de Erro
-INC IIBKERRO

      * Book do container de Servicos (para acionamento de servicos)
-INC IIBKSRVC

      *--> Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book do container de requisição e resposta deste modulo
       01 SGTS0245-DADOS.
-INC SGTK0245

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------
      * Consultar Dados Básicos da Operação Estruturada de Mercado de
      *               Capitais - DB2SGT.OPR_ETRD_MCP
-INC SGTK0083

      * Consultar tarifas - DB2SGT.IN_OPR_ETRD
       01  SGTS0092-DADOS.
-INC SGTK0092

      * Recuperar UOR Gestora Carteira TVM - DB2SGT.CRCT_CTRA_TVM
-INC SGTK0155

      * Calcula Prazo de Duração da Operação em Meses.
-INC SGTK0156

      * Recuperar Indicador de Remuneração Escalonado -
      *                              DB2SGT.RMNC_OPR_ETRD
-INC SGTK0158

      * Listar Itens vinculados a modelo de ordem
      *                                DB2SGT.ITEM_VCLD_MOD_ORD
       01  SGTSL052-DADOS.
-INC SGTKL052

      * Listar Precificação de Taxa de Operação - DB2SGT.PREF_TAXA_OPR
-INC SGTKL068
      *
      * Consulta Nota Classificação Operação - DB2SGT.NOTA_CLSC_OPR
-INC SGTKL074

      * Lista Ordens de Negociação - DB2SGT.ORD_NEGO_OPR
-INC SGTKLA75

      * Verificar em qual estado está a operação -  SGTST009
      *01 SGTST009-DADOS.
      *-INC SGTKT009

      * Consultar dados de usuário - via IIB
       01  ATCS6000-DADOS.
-INC ATCK6000

       77  CTE-FINAL-SS                PIC X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.
      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
-INC ALMKCBL2

      *--> Ativa a captura automática de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

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

           PERFORM  970000-LER-CANAL

      *--> Define o nome do programa p/ fins manipulacao container
           MOVE  'SGTS0245'            TO  NM-PGM

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM  971000-LER-REQUISICAO

           PERFORM  974000-LER-DADOS-OPRD

           PERFORM  110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *--> Valida campo a campo da requisicao
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada
           IF  SGTS0245-CD-CTRA-TVM        LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0245-CD-CTRA-TVM        EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0245-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0245-CD-OPR-ETRD-MCP    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'   TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0245-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0245-NR-VRS-OPR-ETRD    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0245-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       110000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

      *    INITIALIZE SGTS0245-RPST
           PERFORM  210000-CONSISTE-MODELO
      *    PERFORM  220000-CONSISTE-EST-OPR
           PERFORM  230000-CONSULTA-OPR
           MOVE ' '                    TO  SGTSL052-IN-CNTO-PSQ
           PERFORM  UNTIL SGTSL052-IN-CNTO-PSQ  =  'N'
               PERFORM  240000-BUSCA-ITENS-MODELO
               PERFORM  250000-FORMATA-CAMPOS
                    VARYING   SGTSL052-IC-TAB  FROM 1 BY 1
                    UNTIL  SGTSL052-IC-TAB >   SGTSL052-QT-OCR-TAB
           END-PERFORM

           PERFORM  260000-CARREGA-TAB
           PERFORM  980000-GRAVAR-CONTAINER-RSPT
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       210000-CONSISTE-MODELO          SECTION.
      *    Consiste se o modelo que está associado à  oper informada
      *    existe e está ativo.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           MOVE SGTSLA75                   TO NM-PGM
           SET  SGTSLA75-LISTA-ORD-ATV     TO TRUE
           MOVE SGTS0245-CD-CTRA-TVM       TO SGTSLA75-E-CD-CTRA-TIT-VL
           MOVE SGTS0245-CD-OPR-ETRD-MCP   TO SGTSLA75-E-CD-OPR-ETRD-MCP
           MOVE SGTS0245-NR-VRS-OPR-ETRD   TO SGTSLA75-E-NR-VRS-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO

           IF SGTSLA75-QT-OCR-TAB      EQUAL  ZEROES
              SET   ERRO-PROGRAMADO    TO  TRUE
              MOVE  0932               TO  MSG1-CD-ERRO-TS
              MOVE  'Favor vincular um modelo de negociação a esta opera
      -              'ção estruturada.'
                                       TO  MSG1-TX-ERRO-TS
              PERFORM  969000-MONITORAR-VARIAVEIS
              PERFORM  999000-ERRO
           END-IF

           MOVE SGTSLA75-TX-MSG-ORD-NEGO(1) TO TX-MSG-ORD-NEGO
           .
       210000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      *220000-CONSISTE-EST-OPR         SECTION.
      *    Consulta estado atual da ordem de operação.
      *    Caso o estado não seja mais de edição (estado 1, motivo 0),
      *    não pode mais vincular ordem de compra á oper. estruturada.
      *-----------------------------------------------------------------
      *    MOVE  0005                  TO  MSG1-CD-SEQL-ERRO
      *
      *    MOVE SGTST009              TO NM-PGM
      *    SET  SGTST009-CONSULTAR-EST-ATIVO  TO TRUE
      *    MOVE SGTS0245-CD-CTRA-TVM
      *                                TO SGTST009-CD-CTRA-TIT-VL
      *    MOVE SGTS0245-CD-OPR-ETRD-MCP
      *                                TO SGTST009-CD-OPR-ETRD-MCP
      *    MOVE SGTS0245-NR-VRS-OPR-ETRD
      *                                TO SGTST009-NR-VRS-OPR-ETRD
      *
      *    PERFORM 920000-CHAMAR-COMMAREA
      *
      *    IF  SGTST009-CD-RTN-PGM     NOT EQUAL ZEROS
      *        SET ERRO-PROGRAMA-ACIONADO TO TRUE
      *        MOVE NM-PGM             TO MSG1-NM-PGM-ACND
      *        MOVE SGTST009-MSG-ERRO-PGM
      *                                TO MSG1-TX-RTN-PGM-ACND
      *                                   MSG1-TX-ERRO-TS
      *        MOVE SGTST009-CD-RTN-PGM
      *                                TO MSG1-CD-RTN-PGM-ACND
      *                                   MSG1-CD-ERRO-TS
      *
      *        Monitorar variáveis do módulo chamado
      *        PERFORM 969000-MONITORAR-VARIAVEIS
      *        PERFORM 999000-ERRO
      *    END-IF
      *
      *    IF ((SGTST009-CD-TIP-EST-OPR  >  1)   OR
      *        (SGTST009-CD-TIP-EST-OPR  =  1    AND
      *         SGTST009-CD-MTV-ALT      =  999))
      *        SET   ERRO-PROGRAMADO   TO  TRUE
      *        MOVE  1009              TO  MSG1-CD-ERRO-TS
      *        MOVE  'Estado da operação não permite vinculação de Ordem
      *-              'de Negociação de Compra.'
      *                                TO  MSG1-TX-ERRO-TS
      *        PERFORM  969000-MONITORAR-VARIAVEIS
      *        PERFORM  999000-ERRO
      *    END-IF
      *    .
      *220000-SAI.
      *    EXIT.
      *-----------------------------------------------------------------
       230000-CONSULTA-OPR             SECTION.
      *    Busca dados da operação estruturada mercado capitais.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           MOVE SGTS0083                   TO NM-PGM
           MOVE SGTS0245-CD-CTRA-TVM       TO SGTS0083-CD-CTRA-TIT-VL
           MOVE SGTS0245-CD-OPR-ETRD-MCP   TO SGTS0083-CD-OPR-ETRD
           MOVE SGTS0245-NR-VRS-OPR-ETRD   TO SGTS0083-NR-VRS-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO
           .
       230000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       240000-BUSCA-ITENS-MODELO       SECTION.
      *    Busca os itens que compõem o modelo de dados associado á 
      *    operação informada.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           MOVE SGTSL052                   TO NM-PGM
           MOVE SGTSLA75-CD-MOD-ORD-NEGO (1)
                                           TO SGTSL052-CD-MOD-ORD-NEGO

           PERFORM 900000-EXECUTAR-MODULO

           IF SGTSL052-QT-OCR-TAB     EQUAL  ZEROES
              SET   ERRO-PROGRAMADO   TO  TRUE
              MOVE  0933              TO  MSG1-CD-ERRO-TS
              MOVE  'Modelo vinculado a esta operação estruturada não co
      -              'ntém itens associados.'
                                      TO  MSG1-TX-ERRO-TS
              PERFORM  969000-MONITORAR-VARIAVEIS
              PERFORM  999000-ERRO
           END-IF
           .
       240000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       250000-FORMATA-CAMPOS           SECTION.
      *    Verifica qual campo (item vinculado) deve ser formatado
      *    enviando as informações para compor a Ordem de Negociação    .
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           IF  TX-MSG-ORD-NEGO EQUAL SPACES
               MOVE 'N'                TO  IN-TB-MSG
           ELSE
               MOVE 'S'                TO  IN-TB-MSG
           END-IF

           EVALUATE SGTSL052-CD-ITEM-MOD-ORD (SGTSL052-IC-TAB)
           WHEN 8
               PERFORM  250100-FORMATA-EMISSOR
           WHEN 9
               PERFORM  250200-FORMATA-AG-CC
           WHEN 10
               PERFORM  250300-FORMATA-CNPJ
           WHEN 12
               PERFORM  250400-FORMATA-NRO-TIT
           WHEN 15
               PERFORM  250500-FORMATA-DT-EMS
           WHEN 16
               PERFORM  250600-FORMATA-PRAZO
           WHEN 17
               PERFORM  250700-FORMATA-DT-VNCT
           WHEN 18
               PERFORM  250800-FORMATA-DT-TRAN
           WHEN 25
               PERFORM  250810-FORMATA-CATEGORIA-BACEN
           WHEN 26
               PERFORM  250900-FORMATA-CD-TIT
           WHEN 27
               PERFORM  251000-FORMATA-VL-NMNL
           WHEN 28
               PERFORM  251100-FORMATA-VL-NMNL-PU
           WHEN 29
               PERFORM  251200-FORMATA-QTDE
           WHEN 30
               PERFORM  251300-FORMATA-TAXA-RMNC
           WHEN 31
               PERFORM  251400-FORMATA-TARF
           WHEN 32
               PERFORM  251500-FORMATA-RATING
           WHEN 35
               PERFORM  251700-FORMATA-CDT
           WHEN 48
               PERFORM  252000-FORMATA-BFF
           END-EVALUATE
           .
       250000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       250100-FORMATA-EMISSOR          SECTION.
      *    Formata cliente emissor da Operação Estruturada.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           IF IN-TB-MSG  EQUAL 'S'
              MOVE 'N'                 TO  AUX-ACHOU
              PERFORM VARYING IDX-MSG FROM 1 BY 1
              UNTIL IDX-MSG > QT-TAB-ITEM
                 IF TAB-ITEM(IDX-MSG)  EQUAL 8
                    MOVE 'S'           TO  AUX-ACHOU
                    MOVE QT-TAB-ITEM   TO  IDX-MSG
                 END-IF
              END-PERFORM
           END-IF

      *   SGTS0245-IN-ITEM-SELD-CLI com 'S' -> O item foi selecionado
      *           previamente ou não tem nenhum item selecionado.
      *   SGTS0245-IN-ITEM-SELD-CLI com 'N'-> O item não foi selecionado
      *           previamente e tem pelo menos 1 item gravado como selec
           IF IN-TB-MSG  EQUAL 'S'
              IF AUX-ACHOU  =  'S'
                 MOVE 'S'              TO SGTS0245-IN-ITEM-SELD-CLI
              ELSE
                 MOVE 'N'              TO SGTS0245-IN-ITEM-SELD-CLI
              END-IF
           ELSE
              MOVE 'S'                 TO SGTS0245-IN-ITEM-SELD-CLI
           END-IF

           MOVE 8                      TO  SGTS0245-CD-ITEM-MOD-CLI
           MOVE SGTSL052-NR-SEQ-EXB-ITEM(SGTSL052-IC-TAB)
                                       TO SGTS0245-NR-SEQ-EXB-CLI
           MOVE SGTS0083-NM-CLI-EMT    TO  SGTS0245-NM-CLI-EMT
           .
       250100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       250200-FORMATA-AG-CC            SECTION.
      *    Formata Agência/Conta Crédito da Operação Estruturada.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           IF IN-TB-MSG  EQUAL 'S'
              MOVE 'N'                 TO  AUX-ACHOU
              PERFORM VARYING IDX-MSG FROM 1 BY 1
              UNTIL IDX-MSG > QT-TAB-ITEM
                 IF TAB-ITEM(IDX-MSG)  EQUAL 9
                    MOVE 'S'           TO  AUX-ACHOU
                    MOVE QT-TAB-ITEM   TO  IDX-MSG
                 END-IF
              END-PERFORM
           END-IF

      *   SGTS0245-IN-ITEM-SELD-AG-CC com 'S' -> O item foi selecionado
      *           previamente ou não tem nenhum item selecionado.
      * SGTS0245-IN-ITEM-SELD-AG-CC com 'N'-> O item não foi selecionado
      *           previamente e tem pelo menos 1 item gravado como selec
           IF IN-TB-MSG  EQUAL 'S'
              IF AUX-ACHOU  =  'S'
                 MOVE 'S'              TO SGTS0245-IN-ITEM-SELD-AG-CC
              ELSE
                 MOVE 'N'              TO SGTS0245-IN-ITEM-SELD-AG-CC
              END-IF
           ELSE
              MOVE 'S'                 TO SGTS0245-IN-ITEM-SELD-AG-CC
           END-IF

           MOVE 9  TO  SGTS0245-CD-ITEM-MOD-AG-CC
           MOVE SGTSL052-NR-SEQ-EXB-ITEM(SGTSL052-IC-TAB)
                                       TO SGTS0245-NR-SEQ-EXB-AG-CC

           MOVE SGTS0083-CD-BCO-CRD-VL-OPR     TO CD-BCO-CRD-DISP
           MOVE CD-BCO-CRD-DISP                TO CD-BCO-CRD-EDIT
           MOVE SGTS0083-CD-DEPE-CRD-VL-OPR    TO CD-DEPE-CRD-DISP
           MOVE CD-DEPE-CRD-DISP               TO CD-DEPE-CRD-EDIT
           MOVE SGTS0083-NR-CC-CRD-VL-OPR      TO NR-CC-CRD-DISP
           MOVE NR-CC-CRD-DISP                 TO NR-CC-CRD-EDIT

           MOVE ZEROES TO QT-BCO
           INSPECT CD-BCO-CRD-EDIT
                   TALLYING  QT-BCO FOR LEADING SPACES
           ADD 1 TO QT-BCO GIVING INI-CD-BCO-CRD
           COMPUTE TAM-CD-BCO-CRD = 6 - QT-BCO
                 ON SIZE ERROR
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 0143             TO MSG1-CD-ERRO-TS
                 MOVE 'Erro no cálculo do tamanho do texto.'
                                       TO MSG1-TX-ERRO-TS
                 PERFORM 999000-ERRO
           END-COMPUTE

           MOVE ZEROES TO QT-BCO
           INSPECT CD-DEPE-CRD-EDIT
                   TALLYING  QT-BCO FOR LEADING SPACES
           ADD 1 TO QT-BCO GIVING INI-CD-DEPE-CRD
           COMPUTE TAM-CD-DEPE-CRD = 6 - QT-BCO
                 ON SIZE ERROR
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 0143             TO MSG1-CD-ERRO-TS
                 MOVE 'Erro no cálculo do tamanho do texto.'
                                       TO MSG1-TX-ERRO-TS
                 PERFORM 999000-ERRO
           END-COMPUTE

           MOVE ZEROES TO QT-BCO
           INSPECT NR-CC-CRD-EDIT
                   TALLYING  QT-BCO FOR LEADING SPACES
           ADD 1 TO QT-BCO GIVING INI-NR-CC-CRD
           COMPUTE TAM-NR-CC-CRD = 14 - QT-BCO
                 ON SIZE ERROR
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 0143             TO MSG1-CD-ERRO-TS
                 MOVE 'Erro no cálculo do tamanho do texto.'
                                       TO MSG1-TX-ERRO-TS
                 PERFORM 999000-ERRO
           END-COMPUTE

           STRING
              'Bco: '   DELIMITED BY SIZE
              CD-BCO-CRD-EDIT(INI-CD-BCO-CRD:TAM-CD-BCO-CRD)
                                           DELIMITED BY SIZE
              ' - '  DELIMITED BY SIZE
              SGTS0083-NM-BCO-CRD-VL-OPR  DELIMITED BY '  '
              '  Ag. '  DELIMITED BY SIZE
              CD-DEPE-CRD-EDIT(INI-CD-DEPE-CRD:TAM-CD-DEPE-CRD)
                                           DELIMITED BY SIZE
              ' / '  DELIMITED BY SIZE
              NR-CC-CRD-EDIT(INI-NR-CC-CRD:TAM-NR-CC-CRD)
                                           DELIMITED BY SIZE
                                       INTO SGTS0245-TX-BCO-CRD-VL-OPR
           .
       250200-SAI.
           EXIT.
      *-----------------------------------------------------------------
       250300-FORMATA-CNPJ             SECTION.
      *  Formata CPF/CNPJ do cliente emissor da Operação Estruturada.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           IF IN-TB-MSG  EQUAL 'S'
              MOVE 'N'                 TO  AUX-ACHOU
              PERFORM VARYING IDX-MSG FROM 1 BY 1
              UNTIL IDX-MSG > QT-TAB-ITEM
                 IF TAB-ITEM(IDX-MSG)  EQUAL 10
                    MOVE 'S'           TO  AUX-ACHOU
                    MOVE QT-TAB-ITEM   TO  IDX-MSG
                 END-IF
              END-PERFORM
           END-IF

      *   SGTS0245-IN-ITEM-SELD-CNPJ com 'S' -> O item foi selecionado
      *           previamente ou não tem nenhum item selecionado.
      *  SGTS0245-IN-ITEM-SELD-CNPJ com 'N'-> O item não foi selecionado
      *           previamente e tem pelo menos 1 item gravado como selec
           IF IN-TB-MSG  EQUAL 'S'
              IF AUX-ACHOU  =  'S'
                 MOVE 'S'              TO SGTS0245-IN-ITEM-SELD-CNPJ
              ELSE
                 MOVE 'N'              TO SGTS0245-IN-ITEM-SELD-CNPJ
              END-IF
           ELSE
              MOVE 'S'                 TO SGTS0245-IN-ITEM-SELD-CNPJ
           END-IF

           MOVE 10                     TO  SGTS0245-CD-ITEM-MOD-CNPJ
           MOVE SGTSL052-NR-SEQ-EXB-ITEM(SGTSL052-IC-TAB)
                                       TO SGTS0245-NR-SEQ-EXB-CNPJ

           MOVE SGTS0083-CD-CLI-EMT              TO CD-CLI-EMT-DISP
           MOVE CD-CLI-EMT-DISP                  TO CD-CLI-EMT-EDIT

           MOVE ZEROES TO QT-BCO
           INSPECT CD-CLI-EMT-EDIT
                   TALLYING  QT-BCO FOR LEADING SPACES
           ADD 1 TO QT-BCO GIVING INI-CD-CLI-EMT
           COMPUTE TAM-CD-CLI-EMT = 13 - QT-BCO
                 ON SIZE ERROR
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 0143             TO MSG1-CD-ERRO-TS
                 MOVE 'Erro no cálculo do tamanho do texto.'
                                       TO MSG1-TX-ERRO-TS
                 PERFORM 999000-ERRO
           END-COMPUTE

      *    SGTS0083-CD-TIP-PSS-EMT = 1   -->  PF
      *                            = 2   -->  PJ
           IF SGTS0083-CD-TIP-PSS-EMT  EQUAL 1
              MOVE SGTS0083-NR-CPF-CNPJ-EMT      TO NR-CPF-DISP
      *      Foi necessário formatar o CPF e CNPJ por pedaços, pois
      *      a máscara 99.999.999-99 e 99.999.999a99 não foi aceita pelo
      *      compilador. O mesmo vale pra CNPJ.
      *      99.999.999a99 seria combinado com INPECT REPLACE
              MOVE NR-CPF-P1-DISP      TO  NR-CPF-P1-EDIT
              MOVE NR-CPF-P2-DISP      TO  NR-CPF-P2-EDIT
              MOVE NR-CPF-P3-DISP      TO  NR-CPF-P3-EDIT
              MOVE NR-CPF-P4-DISP      TO  NR-CPF-P4-EDIT
              MOVE '.'  TO     NR-CPF-PTO1 NR-CPF-PTO2
              MOVE '-'  TO     NR-CPF-TRO1

              STRING
                   NR-CPF-EDIT   DELIMITED BY SIZE
                   ' / '  DELIMITED BY SIZE
                   CD-CLI-EMT-EDIT(INI-CD-CLI-EMT:TAM-CD-CLI-EMT)
                                           DELIMITED BY SIZE
                                       INTO SGTS0245-TX-CPF-CNPJ-EMT
           ELSE
              IF SGTS0083-CD-TIP-PSS-EMT  EQUAL  2
                 MOVE SGTS0083-NR-CPF-CNPJ-EMT   TO NR-CNPJ-DISP
                 MOVE NR-CNPJ-P1-DISP  TO  NR-CNPJ-P1-EDIT
                 MOVE NR-CNPJ-P2-DISP  TO  NR-CNPJ-P2-EDIT
                 MOVE NR-CNPJ-P3-DISP  TO  NR-CNPJ-P3-EDIT
                 MOVE NR-CNPJ-P4-DISP  TO  NR-CNPJ-P4-EDIT
                 MOVE NR-CNPJ-P5-DISP  TO  NR-CNPJ-P5-EDIT
                 MOVE '.'              TO  NR-CNPJ-PTO1 NR-CNPJ-PTO2
                 MOVE '/'              TO  NR-CNPJ-TRO1
                 MOVE '-'              TO  NR-CNPJ-TRO2

                 STRING
                       NR-CNPJ-EDIT   DELIMITED BY SIZE
                       ' / '  DELIMITED BY SIZE
                       CD-CLI-EMT-EDIT(INI-CD-CLI-EMT:TAM-CD-CLI-EMT)
                                           DELIMITED BY SIZE
                                       INTO SGTS0245-TX-CPF-CNPJ-EMT
              END-IF
           END-IF
           .
       250300-SAI.
           EXIT.
      *-----------------------------------------------------------------
       250400-FORMATA-NRO-TIT          SECTION.
      *  Formata Número do Título associado à Operação Estruturada.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           IF IN-TB-MSG  EQUAL 'S'
              MOVE 'N'                 TO  AUX-ACHOU
              PERFORM VARYING IDX-MSG FROM 1 BY 1
              UNTIL IDX-MSG > QT-TAB-ITEM
                 IF TAB-ITEM(IDX-MSG)  EQUAL 12
                    MOVE 'S'           TO  AUX-ACHOU
                    MOVE QT-TAB-ITEM   TO  IDX-MSG
                 END-IF
              END-PERFORM
           END-IF

      *   SGTS0245-IN-ITEM-SELD-EMS com 'S' -> O item foi selecionado
      *           previamente ou não tem nenhum item selecionado.
      *  SGTS0245-IN-ITEM-SELD-EMS com 'N'-> O item não foi selecionado
      *           previamente e tem pelo menos 1 item gravado como selec
           IF IN-TB-MSG  EQUAL 'S'
              IF AUX-ACHOU  =  'S'
                 MOVE 'S'              TO SGTS0245-IN-ITEM-SELD-EMS
              ELSE
                 MOVE 'N'              TO SGTS0245-IN-ITEM-SELD-EMS
              END-IF
           ELSE
              MOVE 'S'                 TO SGTS0245-IN-ITEM-SELD-EMS
           END-IF

           MOVE 12                      TO SGTS0245-CD-ITEM-MOD-EMS
           MOVE SGTSL052-NR-SEQ-EXB-ITEM(SGTSL052-IC-TAB)
                                       TO SGTS0245-NR-SEQ-EXB-EMS

           MOVE SGTS0083-NR-EMS-TIT-OPR        TO NR-EMS-TIT-DISP
           MOVE NR-EMS-TIT-DISP                TO NR-EMS-TIT-EDIT
           MOVE SGTS0083-NR-SRE-EMS-TIT-OPR    TO NR-SRE-EMS-TIT-DISP
           MOVE NR-SRE-EMS-TIT-DISP            TO NR-SRE-EMS-EDIT
           MOVE SGTS0083-CD-ITC-CVM            TO CD-ITC-CVM-DISP
           MOVE CD-ITC-CVM-DISP                TO CD-ITC-CVM-EDIT

           MOVE ZEROES TO QT-BCO
           INSPECT NR-EMS-TIT-EDIT
                   TALLYING  QT-BCO FOR LEADING SPACES
           ADD 1 TO QT-BCO GIVING INI-NR-EMS-TIT
           COMPUTE TAM-NR-EMS-TIT = 13 - QT-BCO
                 ON SIZE ERROR
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 0143             TO MSG1-CD-ERRO-TS
                 MOVE 'Erro no cálculo do tamanho do texto.'
                                       TO MSG1-TX-ERRO-TS
                 PERFORM 999000-ERRO
           END-COMPUTE

           MOVE ZEROES TO QT-BCO
           INSPECT NR-SRE-EMS-EDIT
                   TALLYING  QT-BCO FOR LEADING SPACES
           ADD 1 TO QT-BCO GIVING INI-NR-SRE-EMS
           COMPUTE TAM-NR-SRE-EMS = 13 - QT-BCO
                 ON SIZE ERROR
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 0143             TO MSG1-CD-ERRO-TS
                 MOVE 'Erro no cálculo do tamanho do texto.'
                                       TO MSG1-TX-ERRO-TS
                 PERFORM 999000-ERRO
           END-COMPUTE

           MOVE ZEROES TO QT-BCO
           INSPECT CD-ITC-CVM-EDIT
                   TALLYING  QT-BCO FOR LEADING SPACES
           ADD 1 TO QT-BCO GIVING INI-CD-ITC-CVM
           COMPUTE TAM-CD-ITC-CVM = 6 - QT-BCO
                 ON SIZE ERROR
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 0143             TO MSG1-CD-ERRO-TS
                 MOVE 'Erro no cálculo do tamanho do texto.'
                                       TO MSG1-TX-ERRO-TS
                 PERFORM 999000-ERRO
           END-COMPUTE

           IF SGTS0083-NR-SRE-EMS-TIT-OPR  EQUAL 0
              STRING
                  NR-EMS-TIT-EDIT(INI-NR-EMS-TIT:TAM-NR-EMS-TIT)
                                           DELIMITED BY SIZE
                  'ª Emissão, Série Única, '  DELIMITED BY SIZE
                  CD-ITC-CVM-EDIT(INI-CD-ITC-CVM:TAM-CD-ITC-CVM)
                                           DELIMITED BY SIZE
                  '-'  DELIMITED BY SIZE
                  SGTS0083-NM-ITC-CVM          DELIMITED BY '  '
                                       INTO SGTS0245-TX-EMS-TIT
              END-STRING
           ELSE
              STRING
                  NR-EMS-TIT-EDIT(INI-NR-EMS-TIT:TAM-NR-EMS-TIT)
                                           DELIMITED BY SIZE
                  'ª Emissão, '  DELIMITED BY SIZE
                  NR-SRE-EMS-EDIT(INI-NR-SRE-EMS:TAM-NR-SRE-EMS)
                                           DELIMITED BY SIZE
                  'ª Série, '  DELIMITED BY SIZE
                  CD-ITC-CVM-EDIT(INI-CD-ITC-CVM:TAM-CD-ITC-CVM)
                                           DELIMITED BY SIZE
                  '-'  DELIMITED BY SIZE
                  SGTS0083-NM-ITC-CVM          DELIMITED BY '  '
                                       INTO SGTS0245-TX-EMS-TIT
              END-STRING
           END-IF
           .
       250400-SAI.
           EXIT.
      *-----------------------------------------------------------------
       250500-FORMATA-DT-EMS           SECTION.
      *  Formata Data de Emissão da Operação Estruturada.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

           IF IN-TB-MSG  EQUAL 'S'
              MOVE 'N'                 TO  AUX-ACHOU
              PERFORM VARYING IDX-MSG FROM 1 BY 1
              UNTIL IDX-MSG > QT-TAB-ITEM
                 IF TAB-ITEM(IDX-MSG)  EQUAL 15
                    MOVE 'S'           TO  AUX-ACHOU
                    MOVE QT-TAB-ITEM   TO  IDX-MSG
                 END-IF
              END-PERFORM
           END-IF

      *   SGTS0245-IN-ITEM-SELD-DT-EMS com 'S' -> O item foi selecionado
      *           previamente ou não tem nenhum item selecionado.
      *   SGTS0245-IN-ITEM-SELD-DT-EMS com 'N'-> O item não foi selec.
      *           previamente e tem pelo menos 1 item gravado como selec
           IF IN-TB-MSG  EQUAL 'S'
              IF AUX-ACHOU  =  'S'
                 MOVE 'S'              TO SGTS0245-IN-ITEM-SELD-DT-EMS
              ELSE
                 MOVE 'N'              TO SGTS0245-IN-ITEM-SELD-DT-EMS
              END-IF
           ELSE
              MOVE 'S'                 TO SGTS0245-IN-ITEM-SELD-DT-EMS
           END-IF

           MOVE 15                     TO  SGTS0245-CD-ITEM-MOD-DT-EMS
           MOVE SGTSL052-NR-SEQ-EXB-ITEM(SGTSL052-IC-TAB)
                                       TO SGTS0245-NR-SEQ-EXB-DT-EMS
           MOVE CORR SGTS0083-DT-EMS-TIT-OPR-R   TO
                                       SGTS0245-DT-EMS-TIT-OPR-R
           .
       250500-SAI.
           EXIT.
      *-----------------------------------------------------------------
       250600-FORMATA-PRAZO            SECTION.
      *  Formata Prazo da Operação Estruturada em meses
      *-----------------------------------------------------------------
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

           IF IN-TB-MSG  EQUAL 'S'
              MOVE 'N'                 TO  AUX-ACHOU
              PERFORM VARYING IDX-MSG FROM 1 BY 1
              UNTIL IDX-MSG > QT-TAB-ITEM
                 IF TAB-ITEM(IDX-MSG)  EQUAL 16
                    MOVE 'S'           TO  AUX-ACHOU
                    MOVE QT-TAB-ITEM   TO  IDX-MSG
                 END-IF
              END-PERFORM
           END-IF

      *   SGTS0245-IN-ITEM-SELD-PZ com 'S' -> O item foi selecionado
      *           previamente ou não tem nenhum item selecionado.
      *   SGTS0245-IN-ITEM-SELD-PZ com 'N'-> O item não foi selec.
      *           previamente e tem pelo menos 1 item gravado como selec
           IF IN-TB-MSG  EQUAL 'S'
              IF AUX-ACHOU  =  'S'
                 MOVE 'S'              TO SGTS0245-IN-ITEM-SELD-PZ
              ELSE
                 MOVE 'N'              TO SGTS0245-IN-ITEM-SELD-PZ
              END-IF
           ELSE
              MOVE 'S'                 TO SGTS0245-IN-ITEM-SELD-PZ
           END-IF

           MOVE 16                     TO  SGTS0245-CD-ITEM-MOD-PZ
           MOVE SGTSL052-NR-SEQ-EXB-ITEM(SGTSL052-IC-TAB)
                                       TO SGTS0245-NR-SEQ-EXB-PZ
      *    MOVE SGTS0156               TO  NM-PGM

      *    MOVE SGTS0245-CD-CTRA-TVM   TO  SGTS0156-CD-CTRA-TIT-VL
      *    MOVE SGTS0245-CD-OPR-ETRD-MCP
      *                                TO  SGTS0156-CD-OPR-ETRD-MCP
      *    MOVE SGTS0245-NR-VRS-OPR-ETRD
      *                                TO  SGTS0156-NR-VRS-OPR-ETRD
      *    MOVE SGTS0083-DT-EMS-TIT-OPR
      *                                TO  SGTS0156-DT-EMS-TIT-OPR
      *    MOVE SGTS0083-DT-VNCT-OPR   TO  SGTS0156-DT-VNCT-OPR
      *
      *    PERFORM 900000-EXECUTAR-MODULO

      *    MOVE SGTS0156-QT-MM-DRCO-OPR TO QT-MM-DRCO-OPR-DISP
           MOVE SGTS0083-QT-AA-DRCO-OPR TO QT-AA-DRCO-OPR-DISP
           MOVE QT-AA-DRCO-OPR-DISP     TO QT-AA-DRCO-OPR-EDIT

           MOVE ZEROES TO QT-BCO
           INSPECT QT-AA-DRCO-OPR-EDIT
                   TALLYING  QT-BCO FOR LEADING SPACES
           ADD 1 TO QT-BCO GIVING INI-NR-EMS-TIT
           COMPUTE TAM-NR-EMS-TIT = 8 - QT-BCO
                 ON SIZE ERROR
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 0143             TO MSG1-CD-ERRO-TS
                 MOVE 'Erro no cálculo do tamanho do texto.'
                                       TO MSG1-TX-ERRO-TS
                 PERFORM 999000-ERRO
           END-COMPUTE

           STRING QT-AA-DRCO-OPR-EDIT(INI-NR-EMS-TIT:TAM-NR-EMS-TIT)
              ' ano(s)'      DELIMITED BY SIZE
                             INTO  SGTS0245-QT-AA-DRCO-OPR
           .
       250600-SAI.
           EXIT.
      *-----------------------------------------------------------------
       250700-FORMATA-DT-VNCT          SECTION.
      *  Formata Data de Vencimento da Operação Estruturada.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

           IF IN-TB-MSG  EQUAL 'S'
              MOVE 'N'                 TO  AUX-ACHOU
              PERFORM VARYING IDX-MSG FROM 1 BY 1
              UNTIL IDX-MSG > QT-TAB-ITEM
                 IF TAB-ITEM(IDX-MSG)  EQUAL 17
                    MOVE 'S'           TO  AUX-ACHOU
                    MOVE QT-TAB-ITEM   TO  IDX-MSG
                 END-IF
              END-PERFORM
           END-IF

      *  SGTS0245-IN-ITEM-SELD-DT-VNCT com 'S' -> O item foi selecionado
      *           previamente ou não tem nenhum item selecionado.
      *   SGTS0245-IN-ITEM-SELD-DT-VNCT com 'N'-> O item não foi selec.
      *           previamente e tem pelo menos 1 item gravado como selec
           IF IN-TB-MSG  EQUAL 'S'
              IF AUX-ACHOU  =  'S'
                 MOVE 'S'              TO SGTS0245-IN-ITEM-SELD-DT-VNCT
              ELSE
                 MOVE 'N'              TO SGTS0245-IN-ITEM-SELD-DT-VNCT
              END-IF
           ELSE
              MOVE 'S'                 TO SGTS0245-IN-ITEM-SELD-DT-VNCT
           END-IF

           MOVE 17                     TO  SGTS0245-CD-ITEM-MOD-DT-VNCT
           MOVE SGTSL052-NR-SEQ-EXB-ITEM(SGTSL052-IC-TAB)
                                       TO SGTS0245-NR-SEQ-EXB-DT-VNCT
           MOVE CORR  SGTS0083-DT-VNCT-OPR-R     TO
                                           SGTS0245-DT-VNCT-OPR-R
           .
       250700-SAI.
           EXIT.
      *-----------------------------------------------------------------
       250800-FORMATA-DT-TRAN          SECTION.
      *  Listar Data da Transação da Operação Estruturada.
      *-----------------------------------------------------------------
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

           IF IN-TB-MSG  EQUAL 'S'
              MOVE 'N'                 TO  AUX-ACHOU
              PERFORM VARYING IDX-MSG FROM 1 BY 1
              UNTIL IDX-MSG > QT-TAB-ITEM
                 IF TAB-ITEM(IDX-MSG)  EQUAL 18
                    MOVE 'S'           TO  AUX-ACHOU
                    MOVE QT-TAB-ITEM   TO  IDX-MSG
                 END-IF
              END-PERFORM
           END-IF

      *  SGTS0245-IN-ITEM-SELD-DT-TRAN com 'S' -> O item foi selecionado
      *           previamente ou não tem nenhum item selecionado.
      *   SGTS0245-IN-ITEM-SELD-DT-TRAN com 'N'-> O item não foi selec.
      *           previamente e tem pelo menos 1 item gravado como selec
           IF IN-TB-MSG  EQUAL 'S'
              IF AUX-ACHOU  =  'S'
                 MOVE 'S'              TO SGTS0245-IN-ITEM-SELD-DT-TRAN
              ELSE
                 MOVE 'N'              TO SGTS0245-IN-ITEM-SELD-DT-TRAN
              END-IF
           ELSE
              MOVE 'S'                 TO SGTS0245-IN-ITEM-SELD-DT-TRAN
           END-IF

           MOVE 18                     TO  SGTS0245-CD-ITEM-MOD-DT-TRAN
           MOVE SGTSL052-NR-SEQ-EXB-ITEM(SGTSL052-IC-TAB)
                                       TO SGTS0245-NR-SEQ-EXB-DT-TRAN
           MOVE CORR  SGTS0083-DT-CPR-OPR-R
                                       TO  SGTS0245-DT-CPR-OPR-R
           .
       250800-SAI.
           EXIT.

      *-----------------------------------------------------------------
       250810-FORMATA-CATEGORIA-BACEN  SECTION.
      *  Formata campos referente categoria BACEN.
      *-----------------------------------------------------------------
           MOVE  0017                  TO  MSG1-CD-SEQL-ERRO

           IF IN-TB-MSG  EQUAL 'S'
              MOVE 'N'                 TO  AUX-ACHOU
              PERFORM VARYING IDX-MSG FROM 1 BY 1
              UNTIL IDX-MSG > QT-TAB-ITEM
                 IF TAB-ITEM(IDX-MSG)  EQUAL 25
                    MOVE 'S'           TO  AUX-ACHOU
                    MOVE QT-TAB-ITEM   TO  IDX-MSG
                 END-IF
              END-PERFORM
           END-IF

      *  SGTS0245-IN-ITEM-SELD-CTGR-BACEN com 'S'
      *                                        -> O item foi selecionado
      *           previamente ou não tem nenhum item selecionado.
      *  SGTS0245-IN-ITEM-SELD-CTGR-BACEN com 'N'-> O item não foi selec
      *           previamente e tem pelo menos 1 item gravado como selec
           IF IN-TB-MSG  EQUAL 'S'
              IF AUX-ACHOU  =  'S'
                 MOVE 'S'         TO   SGTS0245-IN-ITEM-SELD-CTGR-BC
              ELSE
                 MOVE 'N'         TO   SGTS0245-IN-ITEM-SELD-CTGR-BC
              END-IF
           ELSE
              MOVE 'S'            TO   SGTS0245-IN-ITEM-SELD-CTGR-BC
           END-IF

           MOVE 25                TO   SGTS0245-CD-ITEM-MOD-CTGR-BC
           MOVE SGTSL052-NR-SEQ-EXB-ITEM(SGTSL052-IC-TAB)
                                  TO   SGTS0245-NR-SEQ-EXB-CTGR-BC
           MOVE SGTS0083-NM-CTGR-TVM
                                  TO   SGTS0245-NM-CTGR-TVM
           .
       250810-SAI.
           EXIT.

      *-----------------------------------------------------------------
       250900-FORMATA-CD-TIT           SECTION.
      *  Listar Código do Papel (CETIP).
      *-----------------------------------------------------------------
           MOVE  0018                  TO  MSG1-CD-SEQL-ERRO

           IF IN-TB-MSG  EQUAL 'S'
              MOVE 'N'                 TO  AUX-ACHOU
              PERFORM VARYING IDX-MSG FROM 1 BY 1
              UNTIL IDX-MSG > QT-TAB-ITEM
                 IF TAB-ITEM(IDX-MSG)  EQUAL 26
                    MOVE 'S'           TO  AUX-ACHOU
                    MOVE QT-TAB-ITEM   TO  IDX-MSG
                 END-IF
              END-PERFORM
           END-IF

      *  SGTS0245-IN-ITEM-SELD-CD-TIT com 'S' -> O item foi selecionado
      *           previamente ou não tem nenhum item selecionado.
      *   SGTS0245-IN-ITEM-SELD-CD-TIT com 'N'-> O item não foi selec.
      *           previamente e tem pelo menos 1 item gravado como selec
           IF IN-TB-MSG  EQUAL 'S'
              IF AUX-ACHOU  =  'S'
                 MOVE 'S'              TO SGTS0245-IN-ITEM-SELD-CD-TIT
              ELSE
                 MOVE 'N'              TO SGTS0245-IN-ITEM-SELD-CD-TIT
              END-IF
           ELSE
              MOVE 'S'                 TO SGTS0245-IN-ITEM-SELD-CD-TIT
           END-IF

           MOVE 26                     TO  SGTS0245-CD-ITEM-MOD-CD-TIT
           MOVE SGTSL052-NR-SEQ-EXB-ITEM(SGTSL052-IC-TAB)
                                       TO SGTS0245-NR-SEQ-EXB-CD-TIT
           MOVE SGTS0083-CD-TIT-OPR-ETRD   TO
                                       SGTS0245-CD-TIT-OPR-ETRD
           .
       250900-SAI.
           EXIT.
      *-----------------------------------------------------------------
       251000-FORMATA-VL-NMNL          SECTION.
      *  Formata Valor Nominal da Operação Estruturada.
      *-----------------------------------------------------------------
           MOVE  0019                  TO  MSG1-CD-SEQL-ERRO

           IF IN-TB-MSG  EQUAL 'S'
              MOVE 'N'                 TO  AUX-ACHOU
              PERFORM VARYING IDX-MSG FROM 1 BY 1
              UNTIL IDX-MSG > QT-TAB-ITEM
                 IF TAB-ITEM(IDX-MSG)  EQUAL 27
                    MOVE 'S'           TO  AUX-ACHOU
                    MOVE QT-TAB-ITEM   TO  IDX-MSG
                 END-IF
              END-PERFORM
           END-IF

      *  SGTS0245-IN-ITEM-SELD-NMNL com 'S' -> O item foi selecionado
      *           previamente ou não tem nenhum item selecionado.
      *   SGTS0245-IN-ITEM-SELD-NMNL com 'N'-> O item não foi selec.
      *           previamente e tem pelo menos 1 item gravado como selec
           IF IN-TB-MSG  EQUAL 'S'
              IF AUX-ACHOU  =  'S'
                 MOVE 'S'              TO SGTS0245-IN-ITEM-SELD-NMNL
              ELSE
                 MOVE 'N'              TO SGTS0245-IN-ITEM-SELD-NMNL
              END-IF
           ELSE
              MOVE 'S'                 TO SGTS0245-IN-ITEM-SELD-NMNL
           END-IF

           MOVE 27                     TO  SGTS0245-CD-ITEM-MOD-NMNL
           MOVE SGTSL052-NR-SEQ-EXB-ITEM(SGTSL052-IC-TAB)
                                       TO SGTS0245-NR-SEQ-EXB-NMNL
           MOVE SGTS0083-VL-CPR-OPR    TO  VL-CPR-OPR
           MOVE VL-CPR-OPR             TO  VL-CPR-OPR-EDIT

           MOVE ZEROES TO QT-BCO
           INSPECT VL-CPR-OPR-EDIT
                   TALLYING  QT-BCO FOR LEADING SPACES
           ADD 1 TO QT-BCO GIVING INI-NR-EMS-TIT
           COMPUTE TAM-NR-EMS-TIT = 22 - QT-BCO
                 ON SIZE ERROR
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 0143             TO MSG1-CD-ERRO-TS
                 MOVE 'Erro no cálculo do tamanho do texto.'
                                       TO MSG1-TX-ERRO-TS
                 PERFORM 999000-ERRO
           END-COMPUTE

           MOVE VL-CPR-OPR-EDIT(INI-NR-EMS-TIT:TAM-NR-EMS-TIT)
                                       TO  SGTS0245-VL-CPR-OPR
           .
       251000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       251100-FORMATA-VL-NMNL-PU       SECTION.
      *  Formata Valor Manual do PU da Operação Estruturada.
      *-----------------------------------------------------------------
           MOVE  0020                  TO  MSG1-CD-SEQL-ERRO

           IF IN-TB-MSG  EQUAL 'S'
              MOVE 'N'                 TO  AUX-ACHOU
              PERFORM VARYING IDX-MSG FROM 1 BY 1
              UNTIL IDX-MSG > QT-TAB-ITEM
                 IF TAB-ITEM(IDX-MSG)  EQUAL 28
                    MOVE 'S'           TO  AUX-ACHOU
                    MOVE QT-TAB-ITEM   TO  IDX-MSG
                 END-IF
              END-PERFORM
           END-IF

      *  SGTS0245-IN-ITEM-SELD-NMNL-PU com 'S' -> O item foi selecionado
      *           previamente ou não tem nenhum item selecionado.
      *   SGTS0245-IN-ITEM-SELD-NMNL-PU com 'N'-> O item não foi selec.
      *           previamente e tem pelo menos 1 item gravado como selec
           IF IN-TB-MSG  EQUAL 'S'
              IF AUX-ACHOU  =  'S'
                 MOVE 'S'              TO SGTS0245-IN-ITEM-SELD-NMNL-PU
              ELSE
                 MOVE 'N'              TO SGTS0245-IN-ITEM-SELD-NMNL-PU
              END-IF
           ELSE
              MOVE 'S'                 TO SGTS0245-IN-ITEM-SELD-NMNL-PU
           END-IF

           MOVE 28                     TO  SGTS0245-CD-ITEM-MOD-NMNL-PU
           MOVE SGTSL052-NR-SEQ-EXB-ITEM(SGTSL052-IC-TAB)
                                       TO SGTS0245-NR-SEQ-EXB-NMNL-PU
           MOVE SGTS0083-VL-PU-CPR-TIT-OPR TO VL-PU-CPR
           MOVE VL-PU-CPR              TO VL-PU-CPR-EDIT

           MOVE ZEROES TO QT-BCO
           INSPECT VL-PU-CPR-EDIT
                   TALLYING  QT-BCO FOR LEADING SPACES
           ADD 1 TO QT-BCO GIVING INI-NR-EMS-TIT
           COMPUTE TAM-NR-EMS-TIT = 22 - QT-BCO
                 ON SIZE ERROR
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 0143             TO MSG1-CD-ERRO-TS
                 MOVE 'Erro no cálculo do tamanho do texto.'
                                       TO MSG1-TX-ERRO-TS
                 PERFORM 999000-ERRO
           END-COMPUTE

           MOVE VL-PU-CPR-EDIT(INI-NR-EMS-TIT:TAM-NR-EMS-TIT)
                                       TO SGTS0245-VL-PU-CPR-TIT-OPR
           .
       251100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       251200-FORMATA-QTDE             SECTION.
      *  Formata Quantidade de Papéis do Título da Operação Estruturada.
      *-----------------------------------------------------------------
           MOVE  0021                  TO  MSG1-CD-SEQL-ERRO

            IF IN-TB-MSG  EQUAL 'S'
              MOVE 'N'                 TO  AUX-ACHOU
              PERFORM VARYING IDX-MSG FROM 1 BY 1
              UNTIL IDX-MSG > QT-TAB-ITEM
                 IF TAB-ITEM(IDX-MSG)  EQUAL 29
                    MOVE 'S'           TO  AUX-ACHOU
                    MOVE QT-TAB-ITEM   TO  IDX-MSG
                 END-IF
              END-PERFORM
           END-IF

      *  SGTS0245-IN-ITEM-SELD-QT com 'S' -> O item foi selecionado
      *           previamente ou não tem nenhum item selecionado.
      *   SGTS0245-IN-ITEM-SELD-QT com 'N'-> O item não foi selec.
      *           previamente e tem pelo menos 1 item gravado como selec
           IF IN-TB-MSG  EQUAL 'S'
              IF AUX-ACHOU  =  'S'
                 MOVE 'S'              TO SGTS0245-IN-ITEM-SELD-QT
              ELSE
                 MOVE 'N'              TO SGTS0245-IN-ITEM-SELD-QT
              END-IF
           ELSE
              MOVE 'S'                 TO SGTS0245-IN-ITEM-SELD-QT
           END-IF

           MOVE 29                     TO  SGTS0245-CD-ITEM-MOD-QT
           MOVE SGTSL052-NR-SEQ-EXB-ITEM(SGTSL052-IC-TAB)
                                       TO SGTS0245-NR-SEQ-EXB-QT
           MOVE SGTS0083-QT-PAPL-TIT-CPR   TO  QT-PAPL-TIT-CPR
           MOVE QT-PAPL-TIT-CPR        TO  QT-PAPL-TIT-CPR-EDIT

           MOVE ZEROES TO QT-BCO
           INSPECT QT-PAPL-TIT-CPR-EDIT
                   TALLYING  QT-BCO FOR LEADING SPACES
           ADD 1 TO QT-BCO GIVING INI-NR-EMS-TIT
           COMPUTE TAM-NR-EMS-TIT = 13 - QT-BCO
                 ON SIZE ERROR
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 0143             TO MSG1-CD-ERRO-TS
                 MOVE 'Erro no cálculo do tamanho do texto.'
                                       TO MSG1-TX-ERRO-TS
                 PERFORM 999000-ERRO
           END-COMPUTE

           MOVE QT-PAPL-TIT-CPR-EDIT(INI-NR-EMS-TIT:TAM-NR-EMS-TIT)
                                       TO  SGTS0245-QT-PAPL-TIT-CPR
           .
       251200-SAI.
           EXIT.
      *-----------------------------------------------------------------
       251300-FORMATA-TAXA-RMNC        SECTION.
      *  Formanta Taxa Remuneração da Operação Estruturada.
      *-----------------------------------------------------------------
           MOVE  0022                  TO  MSG1-CD-SEQL-ERRO

            IF IN-TB-MSG  EQUAL 'S'
              MOVE 'N'                 TO  AUX-ACHOU
              PERFORM VARYING IDX-MSG FROM 1 BY 1
              UNTIL IDX-MSG > QT-TAB-ITEM
                 IF TAB-ITEM(IDX-MSG)  EQUAL 30
                    MOVE 'S'           TO  AUX-ACHOU
                    MOVE QT-TAB-ITEM   TO  IDX-MSG
                 END-IF
              END-PERFORM
           END-IF

      *  SGTS0245-IN-ITEM-SELD-RMNC com 'S' -> O item foi selecionado
      *           previamente ou não tem nenhum item selecionado.
      *   SGTS0245-IN-ITEM-SELD-RMNC com 'N'-> O item não foi selec.
      *           previamente e tem pelo menos 1 item gravado como selec
           IF IN-TB-MSG  EQUAL 'S'
              IF AUX-ACHOU  =  'S'
                 MOVE 'S'              TO SGTS0245-IN-ITEM-SELD-RMNC
              ELSE
                 MOVE 'N'              TO SGTS0245-IN-ITEM-SELD-RMNC
              END-IF
           ELSE
              MOVE 'S'                 TO SGTS0245-IN-ITEM-SELD-RMNC
           END-IF

           MOVE 30                     TO  SGTS0245-CD-ITEM-MOD-RMNC
           MOVE SGTSL052-NR-SEQ-EXB-ITEM(SGTSL052-IC-TAB)
                                       TO SGTS0245-NR-SEQ-EXB-RMNC
           MOVE SGTS0245-CD-CTRA-TVM   TO  SGTS0158-CD-CTRA-TIT-VL
           MOVE SGTS0245-CD-OPR-ETRD-MCP
                                       TO  SGTS0158-CD-OPR-ETRD-MCP
           MOVE SGTS0245-NR-VRS-OPR-ETRD
                                       TO  SGTS0158-NR-VRS-OPR-ETRD
           MOVE SGTS0158               TO NM-PGM
           PERFORM 900000-EXECUTAR-MODULO

           IF SGTS0158-QT-OCR-TAB  > 0
              MOVE 'Conforme tabela abaixo'
                                       TO SGTS0245-TX-RMNC-OPR

              MOVE SGTS0158-QT-OCR-TAB TO SGTS0245-QT-OCR-TAB-RMNC
           END-IF

           PERFORM VARYING  SGTS0158-IC-TAB  FROM 1 BY 1
               UNTIL SGTS0158-IC-TAB  > SGTS0158-QT-OCR-TAB
               MOVE CORR SGTS0158-DT-INC-TAXA-RMNC-R(SGTS0158-IC-TAB) TO
                   DT-INC-RMNC-OPR-R
               MOVE CORR SGTS0158-DT-FIM-TAXA-RMNC-R(SGTS0158-IC-TAB) TO
                   DT-FIM-RMNC-OPR-R
               MOVE '  -  '            TO TX-ESP
               MOVE PER-RMNC-OPR       TO
                                   PER-RMNC-OPR-RMNC(SGTS0158-IC-TAB)

      *        MOVE SGTS0158-CD-TIP-FMA-APL(SGTS0158-IC-TAB)  TO
               MOVE SGTS0158-PC-TAXA-RMNC-OPR(SGTS0158-IC-TAB)  TO
                                   PC-TAXA-RMNC-EDIT

      *    1 = Percentual
      *    2 = Spread
      *    3 = Taxa Fixa
               MOVE SPACES             TO TAXA-RMNC-OPR(SGTS0158-IC-TAB)
               EVALUATE SGTS0158-CD-TIP-FMA-APL(SGTS0158-IC-TAB)
               WHEN 1
                   STRING PC-TAXA-RMNC-EDIT  DELIMITED BY SIZE
                   ' % '                     DELIMITED BY SIZE
                   SGTS0158-SG-IEC(SGTS0158-IC-TAB)
                                             DELIMITED BY '  '
                   ' a.a'                    DELIMITED BY SIZE
                   INTO TAXA-RMNC-OPR(SGTS0158-IC-TAB)

               WHEN 2
                   STRING  SGTS0158-SG-IEC(SGTS0158-IC-TAB)
                                             DELIMITED BY '  '
                   ' + '                     DELIMITED BY SIZE
                    PC-TAXA-RMNC-EDIT  DELIMITED BY SIZE
                   ' % a.a'                  DELIMITED BY SIZE
                   INTO TAXA-RMNC-OPR(SGTS0158-IC-TAB)

               WHEN 3
                   STRING PC-TAXA-RMNC-EDIT  DELIMITED BY SIZE
                   ' % a.a'                  DELIMITED BY SIZE
                   INTO TAXA-RMNC-OPR(SGTS0158-IC-TAB)

               WHEN OTHER
                   MOVE 1010             TO  MSG1-CD-ERRO-TS
                   MOVE SGTS0158-CD-TIP-FMA-APL(SGTS0158-IC-TAB)
                                         TO  CD-TIP-APL-PREF
                   MOVE CD-TIP-APL-PREF  TO  CD-TIP-APL-PREF-EDIT
                   STRING 'Código Tipo Forma Aplicação '
                      CD-TIP-APL-PREF-EDIT  'não prevista.'
                      DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-EVALUATE
           END-PERFORM
           .
       251300-SAI.
           EXIT.
      *-----------------------------------------------------------------
       251400-FORMATA-TARF             SECTION.
      *  Formata Tarifa de Estruturação da Operação Estruturada.
      *-----------------------------------------------------------------
           MOVE  0023                  TO  MSG1-CD-SEQL-ERRO

            IF IN-TB-MSG  EQUAL 'S'
              MOVE 'N'                 TO  AUX-ACHOU
              PERFORM VARYING IDX-MSG FROM 1 BY 1
              UNTIL IDX-MSG > QT-TAB-ITEM
                 IF TAB-ITEM(IDX-MSG)  EQUAL 31
                    MOVE 'S'           TO  AUX-ACHOU
                    MOVE QT-TAB-ITEM   TO  IDX-MSG
                 END-IF
              END-PERFORM
           END-IF

      *  SGTS0245-IN-ITEM-SELD-TARF com 'S' -> O item foi selecionado
      *           previamente ou não tem nenhum item selecionado.
      *   SGTS0245-IN-ITEM-SELD-TARF com 'N'-> O item não foi selec.
      *           previamente e tem pelo menos 1 item gravado como selec
           IF IN-TB-MSG  EQUAL 'S'
              IF AUX-ACHOU  =  'S'
                 MOVE 'S'              TO SGTS0245-IN-ITEM-SELD-TARF
              ELSE
                 MOVE 'N'              TO SGTS0245-IN-ITEM-SELD-TARF
              END-IF
           ELSE
              MOVE 'S'                 TO SGTS0245-IN-ITEM-SELD-TARF
           END-IF

           MOVE 31                     TO  SGTS0245-CD-ITEM-MOD-TARF
           MOVE SGTSL052-NR-SEQ-EXB-ITEM(SGTSL052-IC-TAB)
                                       TO SGTS0245-NR-SEQ-EXB-TARF
      *
           MOVE SGTS0092               TO  NM-PGM
           MOVE SGTS0245-CD-CTRA-TVM   TO  SGTS0092-CD-CTRA-TIT-VL
           MOVE SGTS0245-CD-OPR-ETRD-MCP
                                       TO  SGTS0092-CD-OPR-ETRD-MCP
           MOVE SGTS0245-NR-VRS-OPR-ETRD
                                       TO  SGTS0092-NR-VRS-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO

           MOVE SGTS0092-PC-TARF-OPR-ETRO
                                       TO  PC-TARF-OPR-ETRO-DISP
           MOVE PC-TARF-OPR-ETRO-DISP
                                       TO  PC-TARF-OPR-ETRO-EDIT
           MOVE ZEROES TO QT-BCO
           INSPECT PC-TARF-OPR-ETRO-EDIT
                   TALLYING  QT-BCO FOR LEADING SPACES
           ADD 1 TO QT-BCO GIVING INI-PC-TARF-OPR
           COMPUTE TAM-PC-TARF-OPR = 16 - QT-BCO
                 ON SIZE ERROR
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 0143             TO MSG1-CD-ERRO-TS
                 MOVE 'Erro no cálculo do tamanho do texto.'
                                       TO MSG1-TX-ERRO-TS
                 PERFORM 999000-ERRO
           END-COMPUTE

           STRING
              PC-TARF-OPR-ETRO-EDIT(INI-PC-TARF-OPR:TAM-PC-TARF-OPR)
                                         DELIMITED BY SIZE
              '%  recebida pelo BB-BI.'  DELIMITED BY SIZE
                                         INTO SGTS0245-TX-PC-TARF-ETRO
           END-STRING
           .
       251400-SAI.
           EXIT.
      *-----------------------------------------------------------------
       251500-FORMATA-RATING           SECTION.
      *  Formata Rating da Operação Estruturada.
      *-----------------------------------------------------------------
           MOVE  0024                  TO  MSG1-CD-SEQL-ERRO

            IF IN-TB-MSG  EQUAL 'S'
              MOVE 'N'                 TO  AUX-ACHOU
              PERFORM VARYING IDX-MSG FROM 1 BY 1
              UNTIL IDX-MSG > QT-TAB-ITEM
                 IF TAB-ITEM(IDX-MSG)  EQUAL 32
                    MOVE 'S'           TO  AUX-ACHOU
                    MOVE QT-TAB-ITEM   TO  IDX-MSG
                 END-IF
              END-PERFORM
           END-IF

      *  SGTS0245-IN-ITEM-SELD-CLSC com 'S' -> O item foi selecionado
      *           previamente ou não tem nenhum item selecionado.
      *   SGTS0245-IN-ITEM-SELD-CLSC com 'N'-> O item não foi selec.
      *           previamente e tem pelo menos 1 item gravado como selec
           IF IN-TB-MSG  EQUAL 'S'
              IF AUX-ACHOU  =  'S'
                 MOVE 'S'              TO SGTS0245-IN-ITEM-SELD-CLSC
              ELSE
                 MOVE 'N'              TO SGTS0245-IN-ITEM-SELD-CLSC
              END-IF
           ELSE
              MOVE 'S'                 TO SGTS0245-IN-ITEM-SELD-CLSC
           END-IF

           MOVE 32                     TO  SGTS0245-CD-ITEM-MOD-CLSC
           MOVE SGTSL052-NR-SEQ-EXB-ITEM(SGTSL052-IC-TAB)
                                       TO SGTS0245-NR-SEQ-EXB-CLSC
           MOVE SGTSL074               TO  NM-PGM
           MOVE 3                      TO  SGTSL074-CD-TIP-SRVC
           MOVE SGTS0245-CD-CTRA-TVM   TO  SGTSL074-E-CD-CTRA-TIT-VL
           MOVE SGTS0245-CD-OPR-ETRD-MCP
                                       TO  SGTSL074-E-CD-OPR-ETRD-MCP
           MOVE SGTS0245-NR-VRS-OPR-ETRD
                                       TO  SGTSL074-E-NR-VRS-OPR-ETRD
           PERFORM 900000-EXECUTAR-MODULO

           IF SGTSL074-QT-OCR-TAB  EQUAL 0
              MOVE 'Não'               TO  SGTS0245-TX-NOTA-CLSC
           ELSE
              STRING 'Sim, '
                     SGTSL074-SG-NOTA-AG-CLSC(1)  DELIMITED BY '  '
                     '  ('  DELIMITED BY SIZE
                     SGTSL074-NM-AG-CLSC-RSCO(1)  DELIMITED BY '  '
                     ')'  DELIMITED BY SIZE
              INTO SGTS0245-TX-NOTA-CLSC
           END-IF
           .
       251500-SAI.
           EXIT.
      *-----------------------------------------------------------------
       251700-FORMATA-CDT              SECTION.
      *  Formata Originador/Cedente da Operação Estruturada.
      *-----------------------------------------------------------------
           MOVE  0025                  TO  MSG1-CD-SEQL-ERRO

      *    IF SGTS0083-NM-CLI-CDT  NOT EQUAL SPACES
              IF IN-TB-MSG  EQUAL 'S'
                 MOVE 'N'                 TO  AUX-ACHOU
                 PERFORM VARYING IDX-MSG FROM 1 BY 1
                 UNTIL IDX-MSG > QT-TAB-ITEM
                    IF TAB-ITEM(IDX-MSG)  EQUAL 35
                       MOVE 'S'           TO  AUX-ACHOU
                       MOVE QT-TAB-ITEM   TO  IDX-MSG
                    END-IF
                 END-PERFORM
              END-IF

      *  SGTS0245-IN-ITEM-SELD-CDT com 'S' -> O item foi selecionado
      *           previamente ou não tem nenhum item selecionado.
      *   SGTS0245-IN-ITEM-SELD-CDT com 'N'-> O item não foi selec.
      *           previamente e tem pelo menos 1 item gravado como selec
              IF IN-TB-MSG  EQUAL 'S'
                 IF AUX-ACHOU  =  'S'
                    MOVE 'S'              TO SGTS0245-IN-ITEM-SELD-CDT
                 ELSE
                    MOVE 'N'              TO SGTS0245-IN-ITEM-SELD-CDT
                 END-IF
              ELSE
                 MOVE 'S'                 TO SGTS0245-IN-ITEM-SELD-CDT
              END-IF

              MOVE 35                  TO  SGTS0245-CD-ITEM-MOD-CDT
              MOVE SGTSL052-NR-SEQ-EXB-ITEM(SGTSL052-IC-TAB)
                                       TO SGTS0245-NR-SEQ-EXB-CDT
              MOVE SGTS0083-NM-CLI-CDT TO  SGTS0245-NM-CLI-CDT
      *    END-IF
           .
       251700-SAI.
           EXIT.
      *-----------------------------------------------------------------
       252000-FORMATA-BFF              SECTION.
      *  Formata BFF (Breaking Funding Fee) da Operação Estruturada.
      *-----------------------------------------------------------------
           MOVE  0026                  TO  MSG1-CD-SEQL-ERRO

           MOVE SGTSL068               TO NM-PGM
           MOVE SGTS0245-CD-CTRA-TVM   TO  SGTSL068-CD-CTRA-TIT-VL
           MOVE SGTS0245-CD-OPR-ETRD-MCP
                                       TO  SGTSL068-CD-OPR-ETRD-MCP
           MOVE SGTS0245-NR-VRS-OPR-ETRD
                                       TO  SGTSL068-NR-VRS-OPR-ETRD
      *    SGTSL068-CD-TIP-TAXA  = 2 (BFF - Quebra de Garantia)
           MOVE 2                      TO  SGTSL068-CD-TIP-TAXA
      *
           PERFORM 900000-EXECUTAR-MODULO

           IF SGTSL068-QT-OCR-TAB  > 0
              IF IN-TB-MSG  EQUAL 'S'
                 MOVE 'N'                 TO  AUX-ACHOU
                 PERFORM VARYING IDX-MSG FROM 1 BY 1
                 UNTIL IDX-MSG > QT-TAB-ITEM
                    IF TAB-ITEM(IDX-MSG)  EQUAL 48
                       MOVE 'S'           TO  AUX-ACHOU
                       MOVE QT-TAB-ITEM   TO  IDX-MSG
                    END-IF
                 END-PERFORM
              END-IF

      *  SGTS0245-IN-ITEM-SELD-BFF com 'S' -> O item foi selecionado
      *           previamente ou não tem nenhum item selecionado.
      *   SGTS0245-IN-ITEM-SELD-BFF com 'N'-> O item não foi selec.
      *           previamente e tem pelo menos 1 item gravado como selec
              IF IN-TB-MSG  EQUAL 'S'
                 IF AUX-ACHOU  =  'S'
                    MOVE 'S'              TO SGTS0245-IN-ITEM-SELD-BFF
                 ELSE
                    MOVE 'N'              TO SGTS0245-IN-ITEM-SELD-BFF
                 END-IF
              ELSE
                 MOVE 'S'                 TO SGTS0245-IN-ITEM-SELD-BFF
              END-IF

              MOVE 48                     TO SGTS0245-CD-ITEM-MOD-BFF
              MOVE SGTSL052-NR-SEQ-EXB-ITEM(SGTSL052-IC-TAB)
                                       TO SGTS0245-NR-SEQ-EXB-BFF
              MOVE 'Conforme tabela abaixo'
                                           TO SGTS0245-TX-BFF
              MOVE SGTSL068-QT-OCR-TAB    TO SGTS0245-QT-OCR-TAB-BFF
           END-IF

           PERFORM VARYING SGTSL068-IC-TAB  FROM 1 BY 1
                   UNTIL SGTSL068-IC-TAB  > SGTSL068-QT-OCR-TAB

               MOVE CORR SGTSL068-DT-INC-CND-TAXA-R(SGTSL068-IC-TAB) TO
                     DT-INC-CND-TAXA-R
               MOVE CORR SGTSL068-DT-FIM-CND-TAXA-R(SGTSL068-IC-TAB) TO
                     DT-FIM-CND-TAXA-R
               MOVE '  -  '            TO TX-ESP-BFF
               MOVE PER-CND-TAXA       TO
                                       PER-CND-TAXA-BFF(SGTSL068-IC-TAB)

      *        EVALUATE SGTSL068-CD-TIP-APL-PREF(SGTSL068-IC-TAB)
      *        WHEN 1
                   MOVE '%'            TO TX-TIP-APL-PREF
      *        WHEN 2
      *            MOVE 'Spread '      TO TX-TIP-APL-PREF
      *        WHEN 3
      *            MOVE 'Taxa Fixa'    TO TX-TIP-APL-PREF
      *        WHEN OTHER
      *             MOVE 9999          TO MSG1-CD-ERRO-TS
      *             MOVE SGTSL068-CD-TIP-APL-PREF(SGTSL068-IC-TAB)
      *                                TO  CD-TIP-APL-PREF
      *             MOVE CD-TIP-APL-PREF   TO  CD-TIP-APL-PREF-EDIT
      *             STRING 'Código Tipo Aplicação '
      *                CD-TIP-APL-PREF-EDIT  'não prevista.'
      *                DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
      *             PERFORM 999000-GRAVA-ERRO-E-ENCERRA
      *        END-EVALUATE

               MOVE SPACES             TO TX-TIP-APL-PREF-BFF
                                          (SGTSL068-IC-TAB)
               MOVE SGTSL068-PC-MIN-TAXA(SGTSL068-IC-TAB)  TO
                     PC-MIN-TAXA-DISP
               MOVE PC-MIN-TAXA-DISP   TO PC-MIN-TAXA-EDIT

               MOVE ZEROES TO QT-BCO
               INSPECT PC-MIN-TAXA-EDIT
                   TALLYING  QT-BCO FOR LEADING SPACES
               ADD 1 TO QT-BCO GIVING INI-NR-EMS-TIT
               COMPUTE TAM-NR-EMS-TIT = 16 - QT-BCO
                     ON SIZE ERROR
                     SET ERRO-PROGRAMADO   TO TRUE
                     MOVE 0143             TO MSG1-CD-ERRO-TS
                     MOVE 'Erro no cálculo do tamanho do texto.'
                                        TO MSG1-TX-ERRO-TS
                     PERFORM 999000-ERRO
               END-COMPUTE

               STRING PC-MIN-TAXA-EDIT (INI-NR-EMS-TIT:TAM-NR-EMS-TIT)
                       DELIMITED BY SIZE
                      ' - '   DELIMITED BY SIZE
                      TX-TIP-APL-PREF
               DELIMITED BY SIZE INTO
                      TX-TIP-APL-PREF-BFF(SGTSL068-IC-TAB)
           END-PERFORM
           .
       252000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       260000-CARREGA-TAB              SECTION.
      *  Carrega as tabelas para container resposta.
      *  Essa carga é necessária aqui, pois o container de resposta
      *  possui 2 tabelas, todas com depending on, e que não necessa-
      *  riamente são carregadas na ordem, depende do nro exibição
      *  do atributo(a leitura é feita classificada por nro exib).
      *-----------------------------------------------------------------
           MOVE  0027                  TO  MSG1-CD-SEQL-ERRO

      *    Carrega tabela BFF
           PERFORM VARYING IC-TAB FROM 1 BY 1
              UNTIL IC-TAB  >  SGTS0245-QT-OCR-TAB-BFF
              MOVE  PER-CND-TAXA-BFF(IC-TAB)       TO
                                       SGTS0245-PER-CND-TAXA(IC-TAB)
              MOVE  TX-TIP-APL-PREF-BFF(IC-TAB)    TO
                                       SGTS0245-TX-TIP-APL-PREF(IC-TAB)
           END-PERFORM

      *    Carrega tabela Remununeração
           PERFORM VARYING IC-TAB FROM 1 BY 1
              UNTIL IC-TAB  >  SGTS0245-QT-OCR-TAB-RMNC
              MOVE PER-RMNC-OPR-RMNC (IC-TAB)      TO
                                       SGTS0245-PER-RMNC-OPR(IC-TAB)
              MOVE TAXA-RMNC-OPR(IC-TAB)           TO
                                       SGTS0245-TAXA-RMNC-OPR(IC-TAB)
           END-PERFORM
           .
       260000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      *   Section da Faixa de 900000 a 969900 para Demais Rotinas
      *-----------------------------------------------------------------
      * Executa módulos implementados com CONTAINER
      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *-----------------------------------------------------------------

      *    Grava o container de requisição do módulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM

               WHEN SGTS0083
                    EXEC CICS LINK
                              PROGRAM ( SGTS0083   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTS0092
                    EXEC CICS LINK
                              PROGRAM ( SGTS0092   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTSL074
                    EXEC CICS LINK
                              PROGRAM ( SGTSL074   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTS0155
                    EXEC CICS LINK
                              PROGRAM ( SGTS0155   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTS0156
                    EXEC CICS LINK
                              PROGRAM ( SGTS0156   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTS0158
                    EXEC CICS LINK
                              PROGRAM ( SGTS0158   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTSL052
                    EXEC CICS LINK
                              PROGRAM ( SGTSL052   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTSL068
                    EXEC CICS LINK
                              PROGRAM ( SGTSL068   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTSLA75
                    EXEC CICS LINK
                              PROGRAM ( SGTSLA75   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN OTHER
                    MOVE 9006          TO MSG1-CD-ERRO-TS
                    MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
      *
           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    MOVE 3004          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro ao acionar o módulo.'
                                       TO MSG1-TX-ERRO-TS
                    SET ERRO-CICS      TO TRUE
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *    Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.
      *---------------------------------------------------------------*
       910000-CHAMAR-SERVICO           SECTION.
      *---------------------------------------------------------------*
      *    Acionar programa IIBP2000 para requisição do serviço.
      *---------------------------------------------------------------*
      *    Grava o container com dados do serviço/operação a chamar
           PERFORM 981000-GRAVAR-CONTAINER-SRVC

      *    Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS LINK
                     PROGRAM   ( IIBP2000     )
                     CHANNEL   ( NM-CNL       )
                     RESP      ( MSG1-RESP1   )
                     RESP2     ( MSG1-RESP2   )
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET   ERRO-PROGRAMA-ACIONADO    TO  TRUE
                    MOVE  9000         TO  MSG1-CD-ERRO-TS
                    STRING 'Erro ao acionar serviço: '
                           SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
                           SRVC-ID-OPR  '.' SRVC-VRS-OPR
                           'via IIBP2000, '
                           'RESP=' MSG1-RESP1 ', RESP-2=' MSG1-RESP2
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    SET ERRO-CICS      TO TRUE
                    PERFORM 999000-ERRO
           END-EVALUATE

      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *    Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA
           .
       910000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      *920000-CHAMAR-COMMAREA          SECTION.
      *-----------------------------------------------------------------
      *
      *--> Movimento nome do módulo para monitoração de variáveis

      *    EVALUATE  NM-PGM

      *    WHEN SGTST009
      *         EXEC CICS LINK
      *              PROGRAM  (SGTST009)
      *              COMMAREA (SGTST009-DADOS)
      *              FLENGTH  (LENGTH OF SGTST009-DADOS)
      *         END-EXEC

      *    WHEN OTHER
      *         CONTINUE
      *    END-EVALUATE
      *    .
      *920000-SAI.
      *    EXIT.
      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM
           WHEN  SGTS0083
                 MOVE 'SGTS0083-CD-CTRA-TIT-VL'
                                               TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0083-CD-CTRA-TIT-VL TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0083-CD-OPR-ETRD'   TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0083-CD-OPR-ETRD    TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0083-NR-VRS-OPR-ETRD'
                                               TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0083-NR-VRS-OPR-ETRD
                                               TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

           WHEN  SGTS0092
                 MOVE 'SGTS0092-CD-CTRA-TIT-VL'
                                               TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0092-CD-CTRA-TIT-VL TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0092-CD-OPR-ETRD-MCP'
                                               TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0092-CD-OPR-ETRD-MCP
                                               TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0092-NR-VRS-OPR-ETRD'
                                               TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0092-NR-VRS-OPR-ETRD
                                               TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

           WHEN  SGTSL074
                 MOVE 'SGTSL074-E-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTSL074-E-CD-CTRA-TIT-VL TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTSL074-E-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTSL074-E-CD-OPR-ETRD-MCP
                                                 TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTSL074-E-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTSL074-E-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTSL074-E-NR-SEQL-NOTA-OPR'
                                                 TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTSL074-E-NR-SEQL-NOTA-OPR
                                                 TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

           WHEN  SGTS0155
                 MOVE 'SGTS0155-E-CD-CTRA-TVM'   TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0155-E-CD-CTRA-TVM    TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0155-E-CD-TIP-ATB-CTRA'
                                                 TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0155-E-CD-TIP-ATB-CTRA
                                                 TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

           WHEN  SGTS0156
                 MOVE 'SGTS0156-CD-CTRA-TIT-VL'  TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0156-CD-CTRA-TIT-VL   TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0156-CD-OPR-ETRD-MCP' TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0156-CD-OPR-ETRD-MCP  TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0156-NR-VRS-OPR-ETRD' TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0156-NR-VRS-OPR-ETRD  TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0156-DT-EMS-TIT-OPR'  TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0156-DT-EMS-TIT-OPR   TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0156-DT-VNCT-OPR'     TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0156-DT-VNCT-OPR      TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

           WHEN  SGTS0158
                 MOVE 'SGTS0158-CD-CTRA-TIT-VL'  TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0158-CD-CTRA-TIT-VL   TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0158-CD-OPR-ETRD-MCP' TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0158-CD-OPR-ETRD-MCP  TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0158-NR-VRS-OPR-ETRD' TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0158-NR-VRS-OPR-ETRD  TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'CD-TIP-APL-PREF-EDIT'     TO  MSGKW999-NM-VRV-MNT
                 MOVE  CD-TIP-APL-PREF-EDIT      TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

           WHEN  SGTSL052
                 MOVE 'SGTSL052-CD-MOD-ORD-NEGO' TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTSL052-CD-MOD-ORD-NEGO  TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTSL052-NR-SEQ-EXB-ITEM-PAG'
                                                 TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTSL052-NR-SEQ-EXB-ITEM-PAG
                                                 TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

           WHEN  SGTSL068
                 MOVE 'SGTSL068-CD-CTRA-TIT-VL'  TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTSL068-CD-CTRA-TIT-VL   TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTSL068-CD-OPR-ETRD-MCP' TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTSL068-CD-OPR-ETRD-MCP  TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTSL068-NR-VRS-OPR-ETRD' TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTSL068-NR-VRS-OPR-ETRD  TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTSL068-CD-TIP-TAXA'     TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTSL068-CD-TIP-TAXA      TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTSL068-CD-ULT-TIP-TAXA' TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTSL068-CD-ULT-TIP-TAXA  TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTSL068-DT-ULT-INC-VLD'  TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTSL068-DT-ULT-INC-VLD   TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

           WHEN  SGTSLA75
                 MOVE 'SGTSLA75-E-CD-TIP-SRVC'   TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTSLA75-E-CD-TIP-SRVC    TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTSLA75-E-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTSLA75-E-CD-CTRA-TIT-VL TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTSLA75-E-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTSLA75-E-CD-OPR-ETRD-MCP
                                                 TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

                 MOVE 'SGTSLA75-E-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                 MOVE  SGTSLA75-E-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                 PERFORM  999003-ADICIONA-VRV-MNT

      *    WHEN  SGTST009
      *          MOVE 'SGTST009-ACAO'            TO  MSGKW999-NM-VRV-MNT
      *          MOVE  SGTST009-ACAO             TO  MSGKW999-VL-VRV-MNT
      *          PERFORM  999003-ADICIONA-VRV-MNT

      *          MOVE 'SGTST009-CD-CTRA-TIT-VL'  TO  MSGKW999-NM-VRV-MNT
      *          MOVE  SGTST009-CD-CTRA-TIT-VL   TO  MSGKW999-VL-VRV-MNT
      *          PERFORM  999003-ADICIONA-VRV-MNT

      *          MOVE 'SGTST009-CD-OPR-ETRD-MCP' TO  MSGKW999-NM-VRV-MNT
      *          IF  SGTST009-CD-OPR-ETRD-MCP    NUMERIC
      *              MOVE  SGTST009-CD-OPR-ETRD-MCP
      *                                          TO CD-OPR-ETRD-MCP-DISP
      *              MOVE  CD-OPR-ETRD-MCP-TX    TO  MSGKW999-VL-VRV-MNT
      *          ELSE
      *              MOVE 'Valor não numérico'   TO  MSGKW999-VL-VRV-MNT
      *          END-IF
      *          PERFORM  999003-ADICIONA-VRV-MNT

      *          MOVE 'SGTST009-NR-VRS-OPR-ETRD' TO  MSGKW999-NM-VRV-MNT
      *          MOVE  SGTST009-NR-VRS-OPR-ETRD  TO  MSGKW999-VL-VRV-MNT
      *          PERFORM  999003-ADICIONA-VRV-MNT

      *          MOVE 'SGTST009-TS-EST-OPR-ETRD' TO  MSGKW999-NM-VRV-MNT
      *          MOVE  SGTST009-TS-EST-OPR-ETRD  TO  MSGKW999-VL-VRV-MNT
      *          PERFORM  999003-ADICIONA-VRV-MNT

      *          MOVE 'SGTST009-CD-USU-RSP-ATL'  TO  MSGKW999-NM-VRV-MNT
      *          MOVE  SGTST009-CD-USU-RSP-ATL   TO  MSGKW999-VL-VRV-MNT
      *          PERFORM  999003-ADICIONA-VRV-MNT

           WHEN ATCS6000
                MOVE 'K6000-CD-FUC'              TO  MSGKW999-NM-VRV-MNT
                MOVE K6000-CD-FUC                TO  MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'K6000-CD-USU'              TO  MSGKW999-NM-VRV-MNT
                MOVE K6000-CD-USU                TO  MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'K6000-CD-RTN'              TO  MSGKW999-NM-VRV-MNT
                MOVE K6000-CD-RTN                TO  MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'K6000-TX-LVRE'             TO  MSGKW999-NM-VRV-MNT
                MOVE K6000-TX-LVRE               TO  MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'K6000-CD-RTN-AUX'          TO MSGKW999-NM-VRV-MNT
                MOVE K6000-CD-RTN-AUX            TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'K6000-CD-REA-AUX'          TO MSGKW999-NM-VRV-MNT
                MOVE K6000-CD-REA-AUX            TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

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
           MOVE  0028                  TO  MSG1-CD-SEQL-ERRO

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
           MOVE  'SGTS0245'            TO  NM-PGM
           MOVE  'SGTS0245-RQSC'       TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM  973100-LER-CONTAINER-NODATA

           MOVE  0029                  TO  MSG1-CD-SEQL-ERRO

      *--> Obtem o tamanho esperado da requisicao
           MOVE  LENGTH OF SGTS0245-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisicao X tamanho recebido

           IF  QT-CCT-LIDO                 NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA
                                       TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0245-RQSC)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       971000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *--> A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------
           MOVE  0030                  TO  MSG1-CD-SEQL-ERRO

      *--> Le o container de erro dos modulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           INITIALIZE                  IIBKERRO-PRM-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  GET
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
               WHEN  MSG1-RESP1            EQUAL DFHRESP(CONTAINERERR)
                     CONTINUE
               WHEN  MSG1-RESP1            EQUAL DFHRESP(NORMAL)
                     SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                     MOVE  NM-PGM      TO  MSG1-NM-PGM-ACND
                     IF  NM-PGM(1:3)       EQUAL 'SGT'
                         MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                                           MSG1-CD-ERRO-TS
                         MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
                     ELSE
                         MOVE  3004    TO  MSG1-CD-ERRO-TS
                         STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                                 DELIMITED BY SIZE
                                 INTO      MSG1-TX-ERRO-TS
                         MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                         MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                     END-IF
      *              Monitorar variáveis do módulo chamado
                     PERFORM  969000-MONITORAR-VARIAVEIS
                     PERFORM  999000-ERRO
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
      *-----------------------------------------------------------------
      *    Recupera o container com os dados de resposta (saída).
      *-----------------------------------------------------------------

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           EVALUATE NM-PGM
               WHEN ATCS6000
                    MOVE 'PRM-DADOS-RPST' TO NM-CTNR

               WHEN OTHER
                    MOVE NM-PGM        TO NM-CTNR(1:8)
                    MOVE '-RPST'       TO NM-CTNR(9:5)
           END-EVALUATE

      *    Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

      *Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Lê a RESPOSTA de acordo com o programa que a gravou

           EVALUATE NM-PGM

      *    Calcula a quantidade de caracteres esperado e limpa a área de
      *    dados antes de executar o GET (somentepara matrizes com dados
      *    não variáveis)

               WHEN SGTS0083
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTS0083-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTS0092
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTS0092-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL074
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSL074-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTS0155
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTS0155-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTS0156
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTS0156-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTS0158
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTS0158-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL052
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSL052-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL068
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSL068-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSLA75
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSLA75-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN ATCS6000
                    INITIALIZE ATCS6000-DADOS
                    MOVE LENGTH OF ATCS6000-DADOS TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (ATCS6000-DADOS   )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN OTHER
                    MOVE 9003          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro na leitura de resposta do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------

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
           MOVE  0031                  TO  MSG1-CD-SEQL-ERRO

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
      *   Section da Faixa de 980000 a 989900 para Gravação de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RSPT    SECTION.
      *-----------------------------------------------------------------
           MOVE  0032                  TO  MSG1-CD-SEQL-ERRO

      *--> Exclui eventual container de erro de outros programas
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO

      *--> Monta o nome do container de resposta
           MOVE  'SGTS0245-RPST'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                )
                      FROM      ( SGTS0245-RPST          )
                      FLENGTH   ( LENGTH OF SGTS0245-RPST)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       980000-SAI.
           EXIT.
      *---------------------------------------------------------------*
       981000-GRAVAR-CONTAINER-SRVC    SECTION.
      *---------------------------------------------------------------*
      *    Postar container de requisição de serviço.
      *---------------------------------------------------------------*
      *    Monta o nome do container de requisicao do servico
           MOVE 'PRM-RQSC-SRVC'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

           EVALUATE NM-PGM
               WHEN ATCS6000
                    MOVE 71739         TO SRVC-ID-SRVC
                    MOVE 1             TO SRVC-VRS-SRVC
                    MOVE 292018        TO SRVC-ID-OPR
                    MOVE 1             TO SRVC-VRS-OPR

               WHEN OTHER
                    SET   ERRO-PROGRAMADO  TO  TRUE
                    MOVE  9004             TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na gravação da requisição do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                     )
                     FROM      ( SRVC-PRM-RQSC-SRVC          )
                     FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC)
           END-EXEC

      *Ao chegar aqui, há certeza de que Não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       981000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *-----------------------------------------------------------------
      *    Postar container com dados de requisição (entrada)
      *-----------------------------------------------------------------

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_modulo -RQSC

           EVALUATE NM-PGM
               WHEN ATCS6000
                    MOVE 'PRM-DADOS-RQSC' TO NM-CTNR

               WHEN OTHER
                    MOVE NM-PGM        TO NM-CTNR(1:8)
                    MOVE '-RQSC'       TO NM-CTNR(9:5)
           END-EVALUATE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisição do programa a executar
           EVALUATE NM-PGM
               WHEN SGTS0083
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTS0083-RQSC           )
                              FLENGTH   ( LENGTH OF SGTS0083-RQSC )
                    END-EXEC

               WHEN SGTS0092
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTS0092-RQSC           )
                              FLENGTH   ( LENGTH OF SGTS0092-RQSC )
                    END-EXEC

               WHEN SGTSL074
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSL074-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSL074-RQSC )
                    END-EXEC

               WHEN SGTS0155
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTS0155-RQSC           )
                              FLENGTH   ( LENGTH OF SGTS0155-RQSC )
                    END-EXEC

               WHEN SGTS0156
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTS0156-RQSC           )
                              FLENGTH   ( LENGTH OF SGTS0156-RQSC )
                    END-EXEC

               WHEN SGTS0158
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTS0158-RQSC           )
                              FLENGTH   ( LENGTH OF SGTS0158-RQSC )
                    END-EXEC

               WHEN SGTSL052
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSL052-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSL052-RQSC )
                    END-EXEC

               WHEN SGTSL068
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSL068-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSL068-RQSC )
                    END-EXEC

               WHEN SGTSLA75
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSLA75-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSLA75-RQSC )
                    END-EXEC

               WHEN ATCS6000
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (ATCS6000-DADOS )
                              FLENGTH   (LENGTH OF ATCS6000-DADOS )
                    END-EXEC

               WHEN OTHER
                    MOVE 9004          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro na gravação da requisição do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       982000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *--> Exclui o container de resposta e de erro para evitar
      *    problemas em módulos executados mais de uma vez dentro
      *    da mesma sessão do CICS.
      *    Equivale ao initialize na área de resposta dos
      *    programas que utilizam COMMAREA.
      *-----------------------------------------------------------------

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           EVALUATE NM-PGM
               WHEN ATCS6000
                    MOVE 'PRM-DADOS-RPST' TO NM-CTNR

               WHEN OTHER
                    MOVE NM-PGM        TO NM-CTNR(1:8)
                    MOVE '-RPST'       TO NM-CTNR(9:5)
           END-EVALUATE

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

      *--> Limpar variaveis container de erro
           INITIALIZE                  IIBKERRO-PRM-ERRO

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

           IF  MSG1-CD-ERRO-TS             EQUAL ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL SPACES
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
           MOVE 'SGTS0245-CD-CTRA-TVM'         TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0245-CD-CTRA-TVM          TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0245-CD-OPR-ETRD-MCP'     TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0245-CD-OPR-ETRD-MCP    NUMERIC
               MOVE  SGTS0245-CD-OPR-ETRD-MCP  TO  CD-OPR-ETRD-MCP-DISP
               MOVE  CD-OPR-ETRD-MCP-TX        TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'      TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0245-NR-VRS-OPR-ETRD'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0245-NR-VRS-OPR-ETRD      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'CD-TIP-APL-PREF-EDIT'         TO  MSGKW999-NM-VRV-MNT
           MOVE  CD-TIP-APL-PREF-EDIT          TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS               TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS               TO  IIBKERRO-TX-ERRO

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
      *---------------------- FIM SGTS0245 -----------------------------