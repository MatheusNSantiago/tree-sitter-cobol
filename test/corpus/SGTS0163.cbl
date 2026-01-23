       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0163
      * ANALISTA..: F2054109 - CINTIA IGLESIAS
      * AUTOR.....: F8785286 - Roseli Akemi Kawahata
      * DATA......: 24/09/2016
      * OBJETIVO..: Negocial verifica se uma operação entra ou não em
      *              trat. exceção
      * COMPILACAO: 52 - PSOSE600 - Cobol 4.2 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS001 04/12/2019 - F0950413 - DE 2016/145195 - Inclusão monito-
      *        ração da variável AUX-ATBC da mensagem 0648.
      *        28/11/2019 - F0950413 - DE 2016/145195 - Inclusão monito-
      *        ração das variáveis AUX-RGR-DISPLAY,  AUX-VRS-DISPLAY,
      *         IDX-SEARCH-DISPLAY E AUX-ITC-DISPLAY.
      *        21/08/2019 - F8785286 - DE 2016/145195 - Inclusão section
      *                     160000-ZERA-TAB-0163 para zerar tab e cpos
      *                     não ficarem com low-values.
      *        05/02/2019 - F8785286 - DE 2016/145195 - Inclusão cpo
      *                     SGTS0189-CD-CTGR-TVM.
      *        01/02/2019 - F2804421 - DE 2016/145195 - Excluir campos
      *                     SBDATAF não utilizados.
      *        31/01/2019 - F2804421 - DE 2016/145195 - Incluir cpos de
      *                     monitoração da rotina SBTIMEDT.
      *        14/12/2018 - F9188847 - DE 2016/145195 - inclusão campos:
      *  movimentação SGTS0163-CD-CTRA-TIT-VL p/ SGTSL092-CD-CTRA-TIT-VL
      *        02/01/2018 - F8785286 - DE 2016/145195 -
      *                     Alteração no book SGTKPE13
      *                     inclusão do campo SGTSPE13-TS-EST-RGR-VLDC
      *        25/04/2017 - F8785286 - DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0163.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------

       77  CTE-INICIO-SS                   PIC  X(040)  VALUE
                    '*** S.S. COMECA AQUI ***'.

       77  CTE-PROG                        PIC  X(018)  VALUE
                    '*** SGTS0163 ***'.
       77  CTE-VERS                        PIC  X(006)  VALUE 'VRS001'.

      *--> Módulos acionados
       77  IIBP2000                        PIC  X(008) VALUE 'IIBP2000'.
       77  SGTS0189                        PIC  X(008) VALUE 'SGTS0189'.
       77  SGTSL089                        PIC  X(008) VALUE 'SGTSL089'.
       77  SGTSL092                        PIC  X(008) VALUE 'SGTSL092'.
       77  SGTSLA06                        PIC  X(008) VALUE 'SGTSLA06'.
       77  SGTSPE10                        PIC  X(008) VALUE 'SGTSPE10'.
       77  SGTSPE13                        PIC  X(008) VALUE 'SGTSPE13'.
       77  SGTST005                        PIC  X(008) VALUE 'SGTST005'.
       77  SBTIMEDT                        PIC  X(008) VALUE 'SBTIMEDT'.
      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Areas de trabalho
       01  AUX-CAMPOS.
           03  IDX-L089                    PIC  S9(4) COMP-5 VALUE 0.
           03  IDX-SMLC                    PIC  S9(4) COMP-5 VALUE 0.
           03  IDX-0163                    PIC  S9(4) COMP-5 VALUE 0.
           03  IDX-ATBC                    PIC  S9(4) COMP-5 VALUE 0.
           03  IDX-ATBC-MAX                PIC  S9(4) COMP-5 VALUE 0.
           03  IDX-SBTC                    PIC  S9(4) COMP-5 VALUE 0.
           03  IDX-EXCO                    PIC  S9(4) COMP-5 VALUE 0.
           03  IDX-SEARCH                  PIC  S9(4) COMP-5 VALUE 0.
           03  IDX-SEARCH-RGR              PIC  S9(4) COMP-5 VALUE 0.
           03  IDX-TAB-SEARCH              PIC  S9(4) COMP   VALUE 0.
           03  IDX-RGR                     PIC  S9(4) COMP-5 VALUE 0.
           03  IDX-2                       PIC  S9(4) COMP-5 VALUE 0.
           03  IDX-ERRO                    PIC  S9(4) COMP-5 VALUE 0.
           03  MAX-EXCO                    PIC  S9(4) COMP-5 VALUE 0.
           03  AUX-ACHOU                   PIC   X(1) VALUE  SPACES.
           03  AUX-MMA-RGR                 PIC   X(1) VALUE  SPACES.

           03  AUX-MAX-RGR                 PIC  S9(4)        VALUE 0.
           03  AUX-IC-TAB                  PIC  S9(4) COMP-5 VALUE 0.
           03  AUX-VL-ITC-A                PIC   X(19) VALUE SPACES.
           03            REDEFINES         AUX-VL-ITC-A.
               05  AUX-VL-ITC              PIC   9(15)V9(4).
           03  AUX-NR-SEQL                 PIC   9(4)        VALUE 0.
           03  AX-NR-GR                    PIC   9(4)        VALUE 0.
           03  AUX-ATBC                    PIC   9(4)        VALUE 0.
           03  AUX-RESET                   PIC   X(1) VALUE  SPACES.
           03  AUX-ENTROU-COND             PIC   X(1) VALUE  SPACES.
           03  AUX-ATENDEU                 PIC   X(1) VALUE  SPACES.
           03  AUX-ENCONTROU-FIM-CND       PIC   X(1) VALUE  SPACES.
           03  AUX-ENCONTROU-EXCO          PIC   X(1) VALUE  SPACES.
           03  AUX-IN-REG-INI-RGR          PIC   X(1) VALUE  SPACES.
           03  AUX-ENTROU-COND-ELSE        PIC   X(1) VALUE  SPACES.
           03  AUX-RGR-DISPLAY             PIC   9(9)        VALUE 0.
           03  AUX-VRS-DISPLAY             PIC   9(9)        VALUE 0.
           03  AUX-ITC-DISPLAY             PIC   9(4)        VALUE 0.
           03  AUX-CD-PRD                  PIC   9(4)        VALUE 0.
           03  AUX-CD-MDLD                 PIC   9(4)        VALUE 0.
           03  AUX-TIP-CMD-VLDC            PIC   9(4)        VALUE 0.
           03  AUX-RGR-PPL-DISPLAY         PIC   9(9)        VALUE 0.
           03  AUX-VRS-PPL-DISPLAY         PIC   9(9)        VALUE 0.
           03  IDX-SEARCH-DISPLAY          PIC   9(4)        VALUE 0.
           03  AUX-CD-TIP-EST              PIC  S9(4) COMP-5 VALUE 0.
           03  AUX-CD-TIP-EST-PPL          PIC  S9(4) COMP-5 VALUE 0.

       01  VL-CPR-OPR                      PIC 9(15)V9(2) VALUE 0.
       01  VL-CPR-OPR-A                    REDEFINES VL-CPR-OPR.
           03  VL-CPR-OPR-D                PIC X(17).

       01  VL-PU-EMS-TIT-OPR               PIC 9(9)V9(8) VALUE 0.
       01  VL-PU-EMS-TIT-OPR-A             REDEFINES VL-PU-EMS-TIT-OPR.
           03  VL-PU-EMS-TIT-OPR-D         PIC X(17).

       01  WS-RESP-CICS.
           03  WS-RESP                     PIC  9(08)   VALUE 0.
           03  WS-RESP2                    PIC  9(08)   VALUE 0.
       01  RGR-SBTC                        PIC  X(01)   VALUE 'N'.
       01  CD-RGR-SBTC                     PIC S9(09)   COMP-5 VALUE 0.
       01  IDX-CARGA                       PIC S9(04)   COMP-5 VALUE 0.
       01  IDX-LISTA                       PIC S9(04)   COMP-5 VALUE 0.
       01  AUX-QT-AA-DRCO-OPR              PIC S9(04)V9(2) COMP-3
                                                        VALUE 0.
      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *--> Data invertida
       01  DT-ATL-INV                      PIC  9(08).
       01  DT-ATL-INV-R REDEFINES DT-ATL-INV.
           03 ANO                          PIC  9(04).
           03 MES                          PIC  9(02).
           03 DIA                          PIC  9(02).

      *--> Data Composição Futura invertida
       01  DT-CMPS-FUT-INV                      PIC  9(08).
       01  DT-CMPS-FUT-INV-R REDEFINES DT-CMPS-FUT-INV.
           03 ANO                          PIC  9(04).
           03 MES                          PIC  9(02).
           03 DIA                          PIC  9(02).

      *--> Data Atual
       01  DT-SQL-ATU                     PIC  X(10) VALUE '  .  .    '.
       01  DT-SQL-ATU-R REDEFINES DT-SQL-ATU.
           03 DIA                          PIC  9(02).
           03 FILLER                       PIC  X(01).
           03 MES                          PIC  9(02).
           03 FILLER                       PIC  X(01).
           03 ANO                          PIC  9(04).

      *-----------------------------------------------------------------
      *    AREA PARA RECUPERAR TIMESTAMP E DATA
      *-----------------------------------------------------------------
       01  DADOS-SBTIMEDT.
           03  WK-CD-FUC                   PIC X(03)      VALUE 'F01'.
           03  WK-AREA-TRB.
               05  WK-AREA-TRB-TAM         PIC S9(9) COMP VALUE 1024.
               05  WK-AREA-TRB-DADOS       PIC  X(1024)   VALUE ' '.
      *        X'0002' - DD.MM.AAAA-HH:MM:SS.CCCCCCXXXX
           03  WK-FMT-TS                   PIC S9(4) COMP VALUE 1.
      *         X'001A'(26)
           03  WK-TAM-TS                   PIC S9(4) COMP VALUE 30.
           03  WK-TS-FMTD.
               05  ANO                     PIC  9(4).
               05  FILLER                  PIC  X(1).
               05  MES                     PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  DIA                     PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  HORA                    PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  MINUTO                  PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  SEGUNDO                 PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  MICRO-SEGUNDO           PIC  9(6).
               05  FILLER                  PIC  X(4).

      *--> Data atual
       01  TS-SQL-ATU                      PIC  X(26) VALUE ' '.
       01  TS-SQL-ATU-R                    REDEFINES TS-SQL-ATU.
           03 DT-REF                       PIC  X(10).
           03 DT-REF-R                     REDEFINES   DT-REF.
             05 ANO                        PIC  9(04).
             05 FILLER                     PIC  X(01).
             05 MES                        PIC  9(02).
             05 FILLER                     PIC  X(01).
             05 DIA                        PIC  9(02).
           03  ANO                         PIC  9(04).
           03  FILLER                      PIC  X(01).
           03  MES                         PIC  9(02).
           03  FILLER                      PIC  X(01).
           03  DIA                         PIC  9(02).
           03  FILLER                      PIC  X(01).
           03  HORA                        PIC  9(02).
           03  FILLER                      PIC  X(01).
           03  MINUTO                      PIC  9(02).
           03  FILLER                      PIC  X(01).
           03  SEGUNDO                     PIC  9(02).
           03  FILLER                      PIC  X(01).
           03  MICRO-SEGUNDO               PIC  9(06).

      *--> Tabelas auxiliares
       01  TB-SEARCH.
         03  TB-SEARCH-OCC  OCCURS 100 TIMES  INDEXED BY IDX-SEARCH-OCC.
           05  TB-SEARCH-CD-RGR            PIC S9(09)      COMP-5.
      *
       01  TB-RGR.
         03  TB-RGR-OCC                    OCCURS  200 TIMES.
           05  TB-RGR-CD-RGR               PIC S9(09)      COMP-5.
           05  TB-RGR-NR-VRS-RGR-VLDC      PIC S9(09)      COMP-5.
           05  TB-RGR-NM-RTL-RGR-VLDC      PIC X(60).
           05  TB-RGR-CD-TIP-EST-DCS       PIC S9(04)      COMP-5.
           05  TB-RGR-IN-RGR-PPL           PIC X(1).
           05  TB-RGR-IN-RGR-VLDC-ATV      PIC X(1).
      *
       01  TB-SMLC.
         03  TB-SMLC-OCC                   OCCURS  300 TIMES.
           05  TB-SMLC-CD-RGR              PIC S9(09)      COMP-5.
           05  TB-SMLC-NR-VRS-RGR-VLDC     PIC S9(09)      COMP-5.
           05  TB-SMLC-CD-TIP-EST          PIC S9(04)      COMP-5.
           05  TB-SMLC-NR-SEQL-ITC         PIC S9(04)      COMP-5.
           05  TB-SMLC-TIP-CMD-VLDC        PIC S9(04)      COMP-5.
           05  TB-SMLC-NR-GR-CND-ITC       PIC S9(04)      COMP-5.
           05  TB-SMLC-CD-ATB-VLDC         PIC S9(04)      COMP-5.
           05  TB-SMLC-CD-TIP-CFT-LGC      PIC S9(04)      COMP-5.
           05  TB-SMLC-CD-TIP-CFT-ITEM     PIC S9(04)      COMP-5.
           05  TB-SMLC-VL-ITC-RGR-VLDC     PIC S9(15)V9(4) COMP-3.
           05  TB-SMLC-NR-ITC-RGR-VLDC     PIC S9(09)      COMP-5.
           05  TB-SMLC-TX-ITC-RGR-VLDC     PIC X(20).
           05  TB-SMLC-CD-TIP-EST-SCDR     PIC S9(04)      COMP-5.
           05  TB-SMLC-IN-RGR-PPL          PIC X(1).
           05  TB-SMLC-CD-RGR-PPL          PIC S9(09)      COMP-5.
           05  TB-SMLC-NR-VRS-RGR-PPL      PIC S9(09)      COMP-5.
           05  TB-SMLC-TRUE                PIC X(1).
           05  TB-SMLC-NR-SEQL-OGNL        PIC S9(04)      COMP-5.
           05  TB-SMLC-CD-TIP-ATB-VLDC     PIC S9(04)      COMP-5.
      *
       01  TB-ATBC.
         03  TB-ATBC-OCC                   OCCURS  100 TIMES.
           05  TB-ATBC-CD-TIP-ATB-VLDC     PIC S9(04)      COMP-5.
      *
       01  TB-L089.
         03  TB-L089-OCC                   OCCURS  200 TIMES.
           05  TB-L089-CD-RGR              PIC S9(09)     COMP-5.
           05  TB-L089-NR-VRS-RGR-VLDC     PIC S9(09)     COMP-5.
           05  TB-L089-NR-SEQL-ITC         PIC S9(04)     COMP-5.
           05  TB-L089-CD-TIP-CMDO-VLDC    PIC S9(04)     COMP-5.
           05  TB-L089-NR-GR-CND-ITC       PIC S9(04)     COMP-5.
           05  TB-L089-CD-ATB-VLDC         PIC S9(04)     COMP-5.
           05  TB-L089-CD-TIP-CFT-LGC      PIC S9(04)     COMP-5.
           05  TB-L089-CD-TIP-CFT-ITEM     PIC S9(04)     COMP-5.
           05  TB-L089-CD-RGR-VLDC-EXAR    PIC S9(09)     COMP-5.
           05  TB-L089-VL-ITC-RGR-VLDC     PIC S9(15)V9(4) COMP-3.
           05  TB-L089-NR-ITC-RGR-VLDC     PIC S9(09)     COMP-5.
           05  TB-L089-TX-ITC-RGR-VLDC     PIC X(01).
           05  TB-L089-CD-TIP-ATB-VLDC     PIC S9(04)      COMP-5.
      *
       01  TB-EXCO.
         03  TB-EXCO-OCC                   OCCURS  50 TIMES.
           05  TB-EXCO-CD-RGR              PIC S9(09)      COMP-5.
           05  TB-EXCO-NR-VRS-RGR-VLDC     PIC S9(09)      COMP-5.
           05  TB-EXCO-NR-SEQL-ITC         PIC S9(04)      COMP-5.
           05  TB-EXCO-TIP-CMD-VLDC        PIC S9(04)      COMP-5.
           05  TB-EXCO-NR-GR-CND-ITC       PIC S9(04)      COMP-5.
           05  TB-EXCO-IN-RGR-PPL          PIC X(1).
           05  TB-EXCO-CD-RGR-PPL          PIC S9(09)      COMP-5.
           05  TB-EXCO-NR-VRS-RGR-PPL      PIC S9(09)      COMP-5.
      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------
      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      * Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999

      * Book para captura de erro via MSG1
-INC MSGKWMG1

      * Book do container de requisição e resposta deste modulo
-INC SGTK0163

      * Book SGT -  Listar Pessoa de Contato Interno da Operação
      *      Estruturada - tabela DB2SGT.CLI_OPR_ETRD
       01 SGTSLA06-DADOS.
-INC SGTKLA06

      * Book SGT -  Busca valores realizados
-INC SGTK0189

      * Book SGT -  Consulta da tabela DB2SGT.ITC_RGR_VLDC_CRCT
       01 SGTSL089-DADOS.
-INC SGTKL089

      * Book SGT -  Consulta da tabela DB2SGT.RGR_VLDC_CRCT_OPR
-INC SGTKL092

      * Book SGT -  Consulta dados da regra da versão aprovada
       01 SGTSPE10-DADOS.
-INC SGTKPE10

      * Book SGT -  Consulta última versão/estado de uma regra validação
       01 SGTSPE13-DADOS.
-INC SGTKPE13

      * Book SGT -  Lista da tabela DB2SGT.OPR_ETRD_MCP
       01  SGTST005-DADOS.
-INC SGTKT005

      * Book do Container de Erro
-INC IIBKERRO
      *
      * Book do container de Serviços (para acionamento de serviços)

      *
      * Book dos dados do usuário


       77  CTE-FINAL-SS                  PIC X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

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

           MOVE 0001                   TO  MSG1-CD-SEQL-ERRO

      *    Ativa a captura automatica de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM 970000-LER-CANAL

           MOVE 'SGTS0163'             TO  NM-PGM

           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO  MSG1-CD-SEQL-ERRO

           IF  SGTS0163-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0002               TO  MSG1-CD-ERRO-TS
               MOVE 'Código Carteira Operação Estruturada deve ser infor
      -         'mado.'                TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0163-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0003               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada de mercado de capita
      -        'is não numérico.'      TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0163-CD-OPR-ETRD-MCP    EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 0005                   TO  MSG1-CD-ERRO-TS
               MOVE 'Codigo Operação Estruturada Mercado de Capitais dev
      -        'e ser informado'           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0163-NR-VRS-OPR-ETRD    EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 0007                   TO  MSG1-CD-ERRO-TS
               MOVE 'Numero Versão Operação Estruturado deve ser informa
      -        'do.'                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           PERFORM 120000-BUSCA-TS-DT

           PERFORM 130000-ZERA-TAB-L089
              VARYING IDX-L089  FROM 1 BY 1
              UNTIL IDX-L089 > 200

           PERFORM 140000-ZERA-TAB-ATBC
              VARYING IDX-ATBC  FROM 1 BY 1
              UNTIL IDX-ATBC > 100

           PERFORM 150000-ZERA-TAB-SMLC
              VARYING IDX-SMLC  FROM 1 BY 1
              UNTIL IDX-SMLC > 300

           PERFORM 160000-ZERA-TAB-0163
              VARYING IDX-0163  FROM 1 BY 1
              UNTIL IDX-0163 > 100
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-BUSCA-TS-DT              SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO  MSG1-CD-SEQL-ERRO

           CALL SBTIMEDT            USING  WK-CD-FUC
               WK-AREA-TRB
               WK-FMT-TS
               WK-TAM-TS
               WK-TS-FMTD

           IF  RETURN-CODE      NOT EQUAL  ZEROS
               SET  ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE 0050                   TO MSG1-CD-ERRO-TS
               MOVE 'Erro ao obter data e hora atual.'
                                           TO MSG1-TX-ERRO-TS
               MOVE  SBTIMEDT              TO MSG1-NM-PGM-ACND
                                              NM-PGM
               MOVE RETURN-CODE            TO MSG1-CD-RTN-PGM-ACND

               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE WK-TS-FMTD             TO  TS-SQL-ATU
           MOVE CORR DT-REF-R          TO  DT-ATL-INV-R
           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-ZERA-TAB-L089            SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROES                 TO
                                     TB-L089-CD-RGR (IDX-L089)
                                     TB-L089-NR-VRS-RGR-VLDC(IDX-L089)
                                     TB-L089-NR-SEQL-ITC(IDX-L089)
                                     TB-L089-CD-TIP-CMDO-VLDC(IDX-L089)
                                     TB-L089-NR-GR-CND-ITC(IDX-L089)
                                     TB-L089-CD-ATB-VLDC(IDX-L089)
                                     TB-L089-CD-TIP-CFT-LGC(IDX-L089)
                                     TB-L089-CD-TIP-CFT-ITEM(IDX-L089)
                                     TB-L089-CD-RGR-VLDC-EXAR(IDX-L089)
                                     TB-L089-VL-ITC-RGR-VLDC(IDX-L089)
                                     TB-L089-NR-ITC-RGR-VLDC (IDX-L089)
                                     TB-L089-CD-TIP-ATB-VLDC (IDX-L089)

           MOVE SPACES                 TO
                                     TB-L089-TX-ITC-RGR-VLDC (IDX-L089)
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-ZERA-TAB-ATBC            SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROES             TO TB-ATBC-CD-TIP-ATB-VLDC(IDX-ATBC)
      *    MOVE ZEROES                 TO TB-ATBC-CD-ATB-VLDC (IDX-ATBC)
      *                              TB-ATBC-VL-RLZD(IDX-ATBC)
      *                              TB-ATBC-NR-RLZD(IDX-ATBC)

      *    MOVE SPACES                 TO TB-ATBC-TX-RLZD (IDX-ATBC)

      *    MOVE 'N'                    TO  TB-CFT-COND-ELSE (IDX-ATBC)
      *                                    TB-CFT-COND-ENDIF (IDX-ATBC)
      *                                    TB-CFT-ENTROU-COND (IDX-ATBC)
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-ZERA-TAB-SMLC            SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROES                 TO TB-SMLC-CD-RGR (IDX-SMLC)
                                   TB-SMLC-NR-VRS-RGR-VLDC (IDX-SMLC)
                                   TB-SMLC-CD-TIP-EST (IDX-SMLC)
                                   TB-SMLC-NR-SEQL-ITC (IDX-SMLC)
                                   TB-SMLC-TIP-CMD-VLDC (IDX-SMLC)
                                   TB-SMLC-NR-GR-CND-ITC (IDX-SMLC)
                                   TB-SMLC-CD-ATB-VLDC (IDX-SMLC)
                                   TB-SMLC-CD-TIP-CFT-LGC (IDX-SMLC)
                                   TB-SMLC-CD-TIP-CFT-ITEM (IDX-SMLC)
                                   TB-SMLC-VL-ITC-RGR-VLDC (IDX-SMLC)
                                   TB-SMLC-NR-ITC-RGR-VLDC (IDX-SMLC)
                                   TB-SMLC-CD-TIP-EST-SCDR (IDX-SMLC)
                                   TB-SMLC-CD-RGR-PPL (IDX-SMLC)
                                   TB-SMLC-NR-VRS-RGR-PPL (IDX-SMLC)
                                   TB-SMLC-NR-SEQL-OGNL (IDX-SMLC)
                                   TB-SMLC-CD-TIP-ATB-VLDC (IDX-SMLC)

           MOVE SPACES                 TO
                                   TB-SMLC-TX-ITC-RGR-VLDC (IDX-SMLC)
                                   TB-SMLC-IN-RGR-PPL (IDX-SMLC)
                                   TB-SMLC-TRUE (IDX-SMLC)
           .
       150000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       160000-ZERA-TAB-0163            SECTION.
      *-----------------------------------------------------------------
           MOVE 0029                   TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROES                 TO
                                   SGTS0163-CD-RGR-PPL (IDX-0163)
                                   SGTS0163-NR-VRS-RGR-PPL (IDX-0163)
                                   SGTS0163-CD-TIP-EST (IDX-0163)
                                   SGTS0163-CD-RGR-SCDR (IDX-0163)
                                   SGTS0163-NR-VRS-RGR-SCDR (IDX-0163)
                                   SGTS0163-CD-TIP-EST-SCDR (IDX-0163)
                                   SGTS0163-NR-SEQL-ITC (IDX-0163)
                                   SGTS0163-NR-SEQL-OGNL (IDX-0163)
                                   SGTS0163-TIP-CMD-VLDC (IDX-0163)
                                   SGTS0163-NR-GR-CND (IDX-0163)
                                   SGTS0163-CD-ATB-VLDC (IDX-0163)
                                   SGTS0163-CD-TIP-CFT-LGC (IDX-0163)
                                   SGTS0163-CD-TIP-CFT-ITEM (IDX-0163)
                                   SGTS0163-VL-ITC-RGR (IDX-0163)
                                   SGTS0163-NR-ITC-RGR (IDX-0163)
                                   SGTS0163-VL-ITC-RLZD (IDX-0163)
                                   SGTS0163-NR-ITC-RLZD (IDX-0163)

           MOVE SPACES                 TO
                                   SGTS0163-IN-RGR-PPL (IDX-0163)
                                   SGTS0163-IN-LNH-EXCO (IDX-0163)
                                   SGTS0163-TX-ITC-RGR (IDX-0163)
                                   SGTS0163-TX-ITC-RLZD (IDX-0163)
           .
       160000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROES                 TO  IDX-L089
                                           IDX-ATBC
                                           IDX-SMLC

           PERFORM 210000-BUSCA-PRD-OPR

           PERFORM 220000-BUSCA-RGR-VLDC

           IF SGTSL092-QT-OCR-TAB > ZEROS

              MOVE ZEROS                  TO  SGTS0163-QT-OCR-TAB
              PERFORM 230000-BUSCA-ITC-RGR
                  VARYING SGTSL092-IC-TAB FROM 1 BY 1
                  UNTIL SGTSL092-IC-TAB   > IDX-RGR

              PERFORM 241000-BUSCA-CLI-EMS

              PERFORM 240000-BUSCA-RLZD

              PERFORM 250000-TRATA-TAB-VDD
                  VARYING IDX-SEARCH FROM 1 BY 1
                  UNTIL IDX-SEARCH   > IDX-SMLC

              MOVE IDX-EXCO               TO  MAX-EXCO
              PERFORM 260000-VERIF-EXCO
                  VARYING IDX-SEARCH FROM 1 BY 1
                  UNTIL IDX-SEARCH   > IDX-SMLC
           END-IF

           PERFORM 980000-GRAVAR-CONTAINER-RPST
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-BUSCA-PRD-OPR            SECTION.
      *  Efetua busca de informações da operação (recupera prod + mod)
      *-----------------------------------------------------------------
           MOVE 0008                   TO  MSG1-CD-SEQL-ERRO

           MOVE SGTST005                   TO  NM-PGM
           SET  SGTST005-CONSULTAR         TO  TRUE
           MOVE SGTS0163-CD-CTRA-TIT-VL    TO  SGTST005-CD-CTRA-TIT-VL
           MOVE SGTS0163-CD-OPR-ETRD-MCP   TO  SGTST005-CD-OPR-ETRD-MCP
           MOVE SGTS0163-NR-VRS-OPR-ETRD   TO  SGTST005-NR-VRS-OPR-ETRD

           PERFORM 920000-CHAMAR-COMMAREA

           IF  SGTST005-CD-RTN-PGM  NOT EQUAL  ZEROES
               SET  ERRO-PROGRAMA-ACIONADO  TO  TRUE
               MOVE  SGTST005-CD-RTN-PGM    TO  MSG1-CD-RTN-PGM-ACND
                                                MSG1-CD-ERRO-TS
               MOVE SGTST005-MSG-ERRO       TO  MSG1-TX-ERRO-TS
                                                MSG1-TX-RTN-PGM-ACND

               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-BUSCA-RGR-VLDC           SECTION.
      *--> Efetua chamada ao lista regra para formatação das regras de
      *    validação existentes. Esse módulo retorna o maior timestamp
      *    de cada versão da regra. Para a simulação, importa somente a
      *    versão que está no estado 1 (em aprovação) ou 2 (aprovada).
      *-----------------------------------------------------------------
           MOVE 0009                   TO  MSG1-CD-SEQL-ERRO

           MOVE SGTST005-CD-PRD        TO  SGTSL092-CD-PRD-INI
                                           SGTSL092-CD-PRD-FIN
           MOVE SGTST005-CD-MDLD       TO  SGTSL092-CD-MDLD-INI
                                           SGTSL092-CD-MDLD-FIN
           MOVE SGTS0163-CD-CTRA-TIT-VL
                                       TO  SGTSL092-CD-CTRA-TIT-VL-INI
                                           SGTSL092-CD-CTRA-TIT-VL-FIM
           SET SGTSL092-LISTA-ULT-EST  TO  TRUE

           MOVE SGTSL092               TO  NM-PGM

           PERFORM 900000-EXECUTAR-MODULO

           PERFORM 221000-CARREGA-RGR
               VARYING SGTSL092-IC-TAB FROM 1 BY 1
               UNTIL SGTSL092-IC-TAB   >   SGTSL092-QT-OCR-TAB
           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       221000-CARREGA-RGR              SECTION.
      *--> Carrega tabela interna de regras.
      * Seleciona somente a primeira regra/versão, despreza os duplic,
      * pois a ordenação do SGTSL092 é decrescente por timestamp.
      * Caso enviada alguma regra/versão na entrada, deve ser recuperado
      * de SGTSL092 a versão com estado = 1 (aguardando aprovação)
      *-----------------------------------------------------------------
           MOVE 0010                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO  AUX-ACHOU
           SET  IDX-SEARCH-OCC         TO  1
           MOVE 1                      TO  IDX-SEARCH
           SEARCH TB-SEARCH-OCC
               VARYING  IDX-SEARCH
               AT END
                    CONTINUE
               WHEN SGTSL092-CD-RGR-VLDC(SGTSL092-IC-TAB)  =
                                       TB-SEARCH-CD-RGR(IDX-SEARCH-OCC)
                    MOVE 'S'           TO  AUX-ACHOU
           END-SEARCH

           PERFORM 221100-VERIF-RGR-SIMUL

           IF  SGTSL092-IN-RGR-VLDC-ATV(SGTSL092-IC-TAB) EQUAL 'S'  AND
               AUX-ACHOU = 'N'

               IF ((AUX-MMA-RGR = 'S'  AND
                    SGTSL092-CD-TIP-EST-DCS(SGTSL092-IC-TAB)  = 1)  OR
                    SGTSL092-CD-TIP-EST-DCS(SGTSL092-IC-TAB)  = 2)

                   ADD  1              TO  IDX-TAB-SEARCH
                                           IDX-RGR
                   MOVE SGTSL092-CD-RGR-VLDC (SGTSL092-IC-TAB)
                                   TO  TB-SEARCH-CD-RGR (IDX-TAB-SEARCH)
                                       TB-RGR-CD-RGR (IDX-RGR)
                   MOVE SGTSL092-NR-VRS-RGR-VLDC (SGTSL092-IC-TAB)
                                   TO  TB-RGR-NR-VRS-RGR-VLDC(IDX-RGR)
                   MOVE SGTSL092-CD-TIP-EST-DCS(SGTSL092-IC-TAB)
                                   TO  TB-RGR-CD-TIP-EST-DCS  (IDX-RGR)
                   MOVE SGTSL092-NM-RTL-RGR-VLDC(SGTSL092-IC-TAB)
                                   TO  TB-RGR-NM-RTL-RGR-VLDC(IDX-RGR)
                   MOVE SGTSL092-IN-RGR-PPL(SGTSL092-IC-TAB)
                                   TO  TB-RGR-IN-RGR-PPL(IDX-RGR)
                   MOVE SGTSL092-IN-RGR-VLDC-ATV(SGTSL092-IC-TAB)
                                   TO  TB-RGR-IN-RGR-VLDC-ATV(IDX-RGR)
               ELSE
                   IF  AUX-MMA-RGR = 'N'
                       MOVE SGTSL092-CD-RGR-VLDC (SGTSL092-IC-TAB)
                                   TO  SGTSPE10-CD-RGR-VLDC
                       MOVE SGTSPE10               TO  NM-PGM
                       PERFORM 900000-EXECUTAR-MODULO
                       IF SGTSPE10-NR-VRS-RGR-VLDC NOT EQUAL ZEROS
      *                   SET  ERRO-PROGRAMADO     TO  TRUE
      *                   MOVE 0585                TO  MSG1-CD-ERRO-TS
      *                   MOVE SGTSPE10-CD-RGR-VLDC  TO  AUX-RGR-DISPLAY
      *                   STRING 'Não há versão aprovada para a regra '
      *                          AUX-RGR-DISPLAY
      *                   DELIMITED BY SIZE      INTO  MSG1-TX-ERRO-TS
      *                   PERFORM 969000-MONITORAR-VARIAVEIS
      *                   PERFORM 999000-ERRO
      *                END-IF
                          ADD  1      TO  IDX-TAB-SEARCH
                                       IDX-RGR
                          MOVE SGTSPE10-NR-VRS-RGR-VLDC
                                   TO  TB-RGR-NR-VRS-RGR-VLDC(IDX-RGR)
                          MOVE SGTSL092-CD-RGR-VLDC (SGTSL092-IC-TAB)
                                   TO  TB-SEARCH-CD-RGR (IDX-TAB-SEARCH)
                                       TB-RGR-CD-RGR (IDX-RGR)
                          MOVE SGTSL092-CD-TIP-EST-DCS(SGTSL092-IC-TAB)
                                   TO  TB-RGR-CD-TIP-EST-DCS  (IDX-RGR)
                          MOVE SGTSL092-NM-RTL-RGR-VLDC(SGTSL092-IC-TAB)
                                   TO  TB-RGR-NM-RTL-RGR-VLDC(IDX-RGR)
                          MOVE SGTSL092-IN-RGR-PPL(SGTSL092-IC-TAB)
                                   TO  TB-RGR-IN-RGR-PPL(IDX-RGR)
                          MOVE SGTSL092-IN-RGR-VLDC-ATV(SGTSL092-IC-TAB)
                                   TO  TB-RGR-IN-RGR-VLDC-ATV(IDX-RGR)
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       221000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       221100-VERIF-RGR-SIMUL          SECTION.
      *--> Se for enviada alguma regra para simular (nesse caso, a
      *    respectiva regra estará no estado 1 (em aprovação)), deve-se
      *    substituir a regra na tabela retornada do SGTSL092.
      *    Caso a regra enviada para simular não esteja no estado 1
      *    (Pendente Confirmação), então deve-se utilizar a
      *    última versão aprovada
      *-----------------------------------------------------------------
           MOVE 0011                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'N'           TO  AUX-MMA-RGR

           IF SGTS0163-QT-OCR-TAB-E    GREATER ZEROES
               PERFORM
                  VARYING IDX-LISTA FROM 1 BY 1
                  UNTIL IDX-LISTA   > SGTS0163-QT-OCR-TAB-E
                   IF  SGTS0163-CD-RGR-VLDC-E (IDX-LISTA)         EQUAL
                           SGTSL092-CD-RGR-VLDC (SGTSL092-IC-TAB) AND
                       SGTS0163-NR-VRS-RGR-VLDC-E(IDX-LISTA)      EQUAL
                           SGTSL092-NR-VRS-RGR-VLDC(SGTSL092-IC-TAB)
                    MOVE 'S'           TO  AUX-MMA-RGR
                   END-IF
               END-PERFORM
           END-IF

           .
       221100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-BUSCA-ITC-RGR            SECTION.
      *--> Para cada regra/versão(PRINCIP)selecionados, busca as
      *    instruções respectivas, para formatação da
      *    tabela de instrução
      *-----------------------------------------------------------------
           MOVE 0012                   TO  MSG1-CD-SEQL-ERRO

           IF TB-RGR-IN-RGR-PPL(SGTSL092-IC-TAB)   EQUAL 'S'
              MOVE TB-RGR-CD-RGR (SGTSL092-IC-TAB) TO
                                       SGTSL089-CD-RGR-VLDC
              MOVE TB-RGR-NR-VRS-RGR-VLDC (SGTSL092-IC-TAB)  TO
                                       SGTSL089-NR-VRS-RGR-VLDC

              MOVE SGTSL089            TO  NM-PGM

              PERFORM 900000-EXECUTAR-MODULO
              IF  SGTSL089-QT-OCR-TAB    EQUAL  ZEROS

                  MOVE  TB-RGR-CD-RGR (SGTSL092-IC-TAB)
                                               TO  AUX-RGR-DISPLAY
                  MOVE  TB-RGR-NR-VRS-RGR-VLDC (SGTSL092-IC-TAB)
                                               TO  AUX-VRS-DISPLAY
                  SET ERRO-PROGRAMADO    TO  TRUE
      *           Falar com Denise
                  MOVE 0662              TO  MSG1-CD-ERRO-TS
                  STRING 'Não encontrada nenhuma instrução para regra '
                        AUX-RGR-DISPLAY ' ' AUX-VRS-DISPLAY
                           DELIMITED BY SIZE INTO  MSG1-TX-ERRO-TS
                  SET MNT-VRV            TO  TRUE
                  PERFORM 999000-ERRO
              END-IF

              PERFORM 231000-CARREGA-TAB-ITC
                  VARYING IDX-L089    FROM 1 BY 1
                  UNTIL IDX-L089       > SGTSL089-QT-OCR-TAB

              SUBTRACT 1 FROM IDX-L089

              PERFORM 232000-CARREGA-ITC-RGR
                  VARYING AUX-IC-TAB  FROM 1 BY 1
                  UNTIL AUX-IC-TAB     > IDX-L089
           END-IF
           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       231000-CARREGA-TAB-ITC          SECTION.
      *--> Carrega tabela cópia do SGTSL089.
      *    Essa tabela foi necessária porque, ao ler a lista SGTSL089,
      *    caso encontre um tipo de comando = 6 (executar regra),
      *    é necessário efetuar nova chamada ao SGTSL089
      *    com a regra secundária, havendo perda de área.
      *-----------------------------------------------------------------
           MOVE 0013                   TO  MSG1-CD-SEQL-ERRO

           MOVE SGTSL089-CD-RGR-VLDC   TO  TB-L089-CD-RGR (IDX-L089)
           MOVE SGTSL089-NR-VRS-RGR-VLDC
                               TO  TB-L089-NR-VRS-RGR-VLDC (IDX-L089)
           MOVE SGTSL089-NR-SEQL-ITC(IDX-L089)
                               TO  TB-L089-NR-SEQL-ITC (IDX-L089)
           MOVE SGTSL089-CD-TIP-CMDO-VLDC(IDX-L089)
                               TO  TB-L089-CD-TIP-CMDO-VLDC   (IDX-L089)
           MOVE SGTSL089-NR-GR-CND-ITC(IDX-L089)
                               TO  TB-L089-NR-GR-CND-ITC (IDX-L089)
           MOVE SGTSL089-CD-ATB-VLDC(IDX-L089)
                               TO  TB-L089-CD-ATB-VLDC (IDX-L089)
           MOVE SGTSL089-CD-TIP-CFT-LGC(IDX-L089)
                               TO  TB-L089-CD-TIP-CFT-LGC (IDX-L089)
           MOVE SGTSL089-CD-TIP-CFT-ITEM(IDX-L089)
                               TO  TB-L089-CD-TIP-CFT-ITEM (IDX-L089)
           MOVE SGTSL089-CD-RGR-VLDC-EXAR(IDX-L089)
                               TO  TB-L089-CD-RGR-VLDC-EXAR (IDX-L089)
           MOVE SGTSL089-VL-ITC-RGR-VLDC(IDX-L089)
                               TO  TB-L089-VL-ITC-RGR-VLDC (IDX-L089)
           MOVE SGTSL089-NR-ITC-RGR-VLDC(IDX-L089)
                               TO  TB-L089-NR-ITC-RGR-VLDC (IDX-L089)
           MOVE SGTSL089-TX-ITC-RGR-VLDC(IDX-L089)
                               TO  TB-L089-TX-ITC-RGR-VLDC (IDX-L089)
           MOVE SGTSL089-CD-TIP-ATB-VLDC(IDX-L089)
                               TO  TB-L089-CD-TIP-ATB-VLDC (IDX-L089)
           .
       231000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       232000-CARREGA-ITC-RGR          SECTION.
      *--> Carrega tabela interna de Instruções para Efetuar a Simulação
      *-----------------------------------------------------------------
           MOVE 0014                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO  AUX-ACHOU
           IF TB-L089-CD-TIP-CMDO-VLDC(AUX-IC-TAB) EQUAL   6
              MOVE TB-L089-CD-RGR-VLDC-EXAR (AUX-IC-TAB)
                                       TO  SGTSL089-CD-RGR-VLDC
              PERFORM VARYING  IDX-SEARCH-RGR      FROM 1 BY 1
                      UNTIL IDX-SEARCH-RGR         >   IDX-RGR
                 IF  TB-RGR-CD-RGR(IDX-SEARCH-RGR) =
                       TB-L089-CD-RGR-VLDC-EXAR (AUX-IC-TAB)
                     MOVE TB-RGR-NR-VRS-RGR-VLDC(IDX-SEARCH-RGR)
                                       TO  SGTSL089-NR-VRS-RGR-VLDC
                     ADD  1  TO  IDX-RGR   GIVING  IDX-SEARCH-RGR
                     MOVE 'S'          TO  AUX-ACHOU
                 END-IF
              END-PERFORM

              IF AUX-ACHOU = 'N'
                 PERFORM 232300-CHAMA-PE13
                 MOVE SGTSPE13-CD-TIP-EST-DCS TO AUX-CD-TIP-EST
                 EVALUATE SGTSPE13-CD-TIP-EST-DCS
                 WHEN 1
                    PERFORM 221100-VERIF-RGR-SIMUL
                    IF AUX-MMA-RGR  =  'S'
                       MOVE SGTSPE13-NR-VRS-RGR-VLDC  TO
                                          SGTSL089-NR-VRS-RGR-VLDC
                    ELSE
                       MOVE SGTSPE13-CD-RGR-VLDC
                                       TO  SGTSPE10-CD-RGR-VLDC
                       MOVE SGTSPE10               TO  NM-PGM
                       PERFORM 900000-EXECUTAR-MODULO
                       IF SGTSPE10-NR-VRS-RGR-VLDC EQUAL ZEROS
                          SET  ERRO-PROGRAMADO     TO  TRUE
                          MOVE 0585                TO  MSG1-CD-ERRO-TS
                          MOVE SGTSPE10-CD-RGR-VLDC  TO  AUX-RGR-DISPLAY
                          STRING '(1)Não há versão aprovada para a regra
      -                   ' '          AUX-RGR-DISPLAY
                          DELIMITED BY SIZE      INTO  MSG1-TX-ERRO-TS
                          PERFORM 969000-MONITORAR-VARIAVEIS
                          PERFORM 999000-ERRO
                       END-IF
                       MOVE SGTSPE10-NR-VRS-RGR-VLDC
                                      TO  SGTSL089-NR-VRS-RGR-VLDC
                       MOVE 2 TO AUX-CD-TIP-EST
                    END-IF

                 WHEN 2
                    PERFORM 221100-VERIF-RGR-SIMUL
                    IF AUX-MMA-RGR  =  'S'
                       SET ERRO-PROGRAMADO     TO  TRUE
                       MOVE 0663               TO  MSG1-CD-ERRO-TS
                       MOVE SGTSL092-CD-RGR-VLDC (SGTSL092-IC-TAB)
                                               TO  AUX-RGR-DISPLAY
                       STRING  'Versão atual da regra '
                           AUX-RGR-DISPLAY
                           ' é 2, não permite simulação.'
                           DELIMITED BY SIZE INTO  MSG1-TX-ERRO-TS
                       SET MNT-VRV             TO  TRUE
                       PERFORM 999000-ERRO
                    ELSE
                       MOVE SGTSPE13-NR-VRS-RGR-VLDC  TO
                                          SGTSL089-NR-VRS-RGR-VLDC
                    END-IF

                 WHEN OTHER
                    MOVE 2 TO AUX-CD-TIP-EST
                    MOVE SGTSPE13-CD-RGR-VLDC
                                       TO  SGTSPE10-CD-RGR-VLDC
                    MOVE SGTSPE10               TO  NM-PGM
                    PERFORM 900000-EXECUTAR-MODULO
                    IF SGTSPE10-NR-VRS-RGR-VLDC EQUAL ZEROS
                       SET  ERRO-PROGRAMADO     TO  TRUE
                       MOVE 0585                TO  MSG1-CD-ERRO-TS
                       MOVE SGTSPE10-CD-RGR-VLDC  TO  AUX-RGR-DISPLAY
                       STRING '(2) Não há versão aprovada para a regra '
                              AUX-RGR-DISPLAY
                       DELIMITED BY SIZE      INTO  MSG1-TX-ERRO-TS
                       PERFORM 969000-MONITORAR-VARIAVEIS
                       PERFORM 999000-ERRO
                    END-IF

                    MOVE SGTSPE10-NR-VRS-RGR-VLDC
                                      TO  SGTSL089-NR-VRS-RGR-VLDC
                 END-EVALUATE
              END-IF

              MOVE SGTSL089            TO  NM-PGM
              PERFORM 900000-EXECUTAR-MODULO

              IF  SGTSL089-QT-OCR-TAB  EQUAL  ZEROS
                  SET ERRO-PROGRAMADO  TO  TRUE
                  MOVE 0662            TO  MSG1-CD-ERRO-TS
                  MOVE 'Não encontrada nenhuma instrução para regra ativ
      -                'a(2).'         TO  MSG1-TX-ERRO-TS
                  SET MNT-VRV          TO  TRUE
                  PERFORM 999000-ERRO
              END-IF

              PERFORM 232100-CARGA-SIMUL-SECUND
                  VARYING IDX-SBTC     FROM 1 BY 1
                  UNTIL IDX-SBTC       >  SGTSL089-QT-OCR-TAB
           ELSE
              PERFORM 232200-CARGA-SIMUL
           END-IF
           .
       232000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       232100-CARGA-SIMUL-SECUND       SECTION.
      *--> Carrega dados da regra secundária para a tabela simulação
      *    (contém todas as instruções para serem simuladas).
      *-----------------------------------------------------------------
           MOVE 0015                   TO  MSG1-CD-SEQL-ERRO

           ADD  1                      TO  AUX-NR-SEQL     IDX-SMLC
           IF SGTSL089-CD-TIP-CMDO-VLDC(IDX-SBTC)    EQUAL 1 AND
              SGTSL089-CD-TIP-CFT-LGC(IDX-SBTC)      EQUAL 0
              ADD  1                   TO  AX-NR-GR
           END-IF

           IF SGTSL089-CD-ATB-VLDC(IDX-SBTC)  > 0
              MOVE SGTSL089-CD-ATB-VLDC(IDX-SBTC)     TO  AUX-ATBC
           END-IF

           IF SGTSL089-CD-ATB-VLDC(IDX-SBTC)  >  SGTS0189-QT-OCR-TAB-E
              MOVE SGTSL089-CD-ATB-VLDC(IDX-SBTC)  TO
                                                 SGTS0189-QT-OCR-TAB-E
           END-IF


           MOVE  AUX-CD-TIP-EST  TO TB-SMLC-CD-TIP-EST-SCDR  (IDX-SMLC)

           MOVE SGTSL089-CD-RGR-VLDC
                                 TO  TB-SMLC-CD-RGR (IDX-SMLC)
           MOVE SGTSL089-NR-VRS-RGR-VLDC
                                 TO  TB-SMLC-NR-VRS-RGR-VLDC (IDX-SMLC)

           MOVE SGTSL089-NR-SEQL-ITC(IDX-SBTC)
                                 TO  TB-SMLC-NR-SEQL-OGNL (IDX-SMLC)

           MOVE AUX-NR-SEQL      TO  TB-SMLC-NR-SEQL-ITC (IDX-SMLC)

           MOVE SGTSL089-CD-TIP-CMDO-VLDC(IDX-SBTC)
                                 TO  TB-SMLC-TIP-CMD-VLDC   (IDX-SMLC)

           MOVE AX-NR-GR         TO  TB-SMLC-NR-GR-CND-ITC (IDX-SMLC)

           MOVE SGTSL089-CD-ATB-VLDC(IDX-SBTC)
                                 TO  TB-SMLC-CD-ATB-VLDC (IDX-SMLC)

           IF SGTSL089-CD-ATB-VLDC(IDX-SBTC)  > 0
              MOVE SGTSL089-CD-ATB-VLDC(IDX-SBTC)
                       TO SGTS0189-CD-ATB-VLDC-E(AUX-ATBC)
              MOVE SGTSL089-CD-TIP-ATB-VLDC(IDX-SBTC)
                                 TO  TB-ATBC-CD-TIP-ATB-VLDC(AUX-ATBC)
           END-IF

           MOVE SGTSL089-CD-TIP-CFT-LGC(IDX-SBTC)
                                 TO  TB-SMLC-CD-TIP-CFT-LGC (IDX-SMLC)
           MOVE SGTSL089-CD-TIP-CFT-ITEM(IDX-SBTC)
                                 TO  TB-SMLC-CD-TIP-CFT-ITEM (IDX-SMLC)
           MOVE SGTSL089-VL-ITC-RGR-VLDC(IDX-SBTC)
                                 TO  TB-SMLC-VL-ITC-RGR-VLDC (IDX-SMLC)
           MOVE SGTSL089-NR-ITC-RGR-VLDC(IDX-SBTC)
                                 TO  TB-SMLC-NR-ITC-RGR-VLDC (IDX-SMLC)
           MOVE SGTSL089-TX-ITC-RGR-VLDC(IDX-SBTC)
                                 TO  TB-SMLC-TX-ITC-RGR-VLDC (IDX-SMLC)
           MOVE SGTSL089-CD-TIP-ATB-VLDC(IDX-SBTC)
                                 TO  TB-SMLC-CD-TIP-ATB-VLDC (IDX-SMLC)

           MOVE 'N'              TO  TB-SMLC-IN-RGR-PPL (IDX-SMLC)
           MOVE TB-L089-CD-RGR (AUX-IC-TAB)
                                 TO TB-SMLC-CD-RGR-PPL(IDX-SMLC)
           MOVE TB-L089-NR-VRS-RGR-VLDC (AUX-IC-TAB)
                                 TO TB-SMLC-NR-VRS-RGR-PPL(IDX-SMLC)

           IF SGTSL089-CD-TIP-CMDO-VLDC(IDX-SBTC) EQUAL 3
              SUBTRACT  1           FROM    AX-NR-GR
           END-IF
           .
       232100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       232200-CARGA-SIMUL              SECTION.
      *-----------------------------------------------------------------
      *--> Carrega dados da regra principal para a tabela de simulação
      *    (contém todas as instruções para serem simuladas).
      *-----------------------------------------------------------------
           MOVE 0016                   TO  MSG1-CD-SEQL-ERRO

           ADD  1                      TO  AUX-NR-SEQL     IDX-SMLC

           IF TB-L089-CD-TIP-CMDO-VLDC(AUX-IC-TAB) EQUAL 1 AND
              TB-L089-CD-TIP-CFT-LGC(AUX-IC-TAB)   EQUAL 0
              ADD  1  TO  AX-NR-GR
           END-IF

           IF TB-L089-CD-ATB-VLDC(AUX-IC-TAB)  > 0
              MOVE TB-L089-CD-ATB-VLDC(AUX-IC-TAB)    TO  AUX-ATBC
           END-IF

           IF TB-L089-CD-ATB-VLDC(AUX-IC-TAB)  >  SGTS0189-QT-OCR-TAB-E
              MOVE TB-L089-CD-ATB-VLDC(AUX-IC-TAB)  TO
                                                 SGTS0189-QT-OCR-TAB-E
           END-IF

           MOVE  TB-RGR-CD-TIP-EST-DCS(SGTSL092-IC-TAB)
                                 TO TB-SMLC-CD-TIP-EST(IDX-SMLC)

           MOVE TB-L089-CD-RGR (AUX-IC-TAB)
                                 TO  TB-SMLC-CD-RGR (IDX-SMLC)
           MOVE TB-L089-NR-VRS-RGR-VLDC (AUX-IC-TAB)
                                 TO  TB-SMLC-NR-VRS-RGR-VLDC (IDX-SMLC)

           MOVE TB-L089-NR-SEQL-ITC (AUX-IC-TAB)
                                 TO  TB-SMLC-NR-SEQL-OGNL (IDX-SMLC)

           MOVE AUX-NR-SEQL      TO  TB-SMLC-NR-SEQL-ITC (IDX-SMLC)

           MOVE TB-L089-CD-TIP-CMDO-VLDC(AUX-IC-TAB)
                                 TO  TB-SMLC-TIP-CMD-VLDC   (IDX-SMLC)

           MOVE AX-NR-GR         TO  TB-SMLC-NR-GR-CND-ITC (IDX-SMLC)

           MOVE TB-L089-CD-ATB-VLDC(AUX-IC-TAB)
                                 TO  TB-SMLC-CD-ATB-VLDC (IDX-SMLC)
           IF TB-L089-CD-ATB-VLDC(AUX-IC-TAB)  > 0
              MOVE TB-L089-CD-ATB-VLDC(AUX-IC-TAB)
                        TO SGTS0189-CD-ATB-VLDC-E(AUX-ATBC)

              MOVE TB-L089-CD-TIP-ATB-VLDC(AUX-IC-TAB)
                                 TO  TB-ATBC-CD-TIP-ATB-VLDC(AUX-ATBC)
           END-IF

           MOVE TB-L089-CD-TIP-CFT-LGC(AUX-IC-TAB)
                                 TO  TB-SMLC-CD-TIP-CFT-LGC (IDX-SMLC)
           MOVE TB-L089-CD-TIP-CFT-ITEM(AUX-IC-TAB)
                                 TO  TB-SMLC-CD-TIP-CFT-ITEM (IDX-SMLC)
           MOVE TB-L089-VL-ITC-RGR-VLDC(AUX-IC-TAB)
                                 TO  TB-SMLC-VL-ITC-RGR-VLDC (IDX-SMLC)
           MOVE TB-L089-NR-ITC-RGR-VLDC(AUX-IC-TAB)
                                 TO  TB-SMLC-NR-ITC-RGR-VLDC (IDX-SMLC)
           MOVE TB-L089-TX-ITC-RGR-VLDC(AUX-IC-TAB)
                                 TO  TB-SMLC-TX-ITC-RGR-VLDC (IDX-SMLC)
           MOVE TB-L089-CD-TIP-ATB-VLDC(AUX-IC-TAB)
                                 TO  TB-SMLC-CD-TIP-ATB-VLDC (IDX-SMLC)

           MOVE 'S'              TO  TB-SMLC-IN-RGR-PPL (IDX-SMLC)
           MOVE TB-L089-CD-RGR (AUX-IC-TAB)
                                 TO TB-SMLC-CD-RGR-PPL(IDX-SMLC)
           MOVE TB-L089-NR-VRS-RGR-VLDC (AUX-IC-TAB)
                                 TO TB-SMLC-NR-VRS-RGR-PPL(IDX-SMLC)

           IF TB-L089-CD-TIP-CMDO-VLDC(AUX-IC-TAB) EQUAL 3
              SUBTRACT 1 FROM AX-NR-GR
           END-IF
           .
       232200-SAI.
           EXIT.

      *-----------------------------------------------------------------
       232300-CHAMA-PE13              SECTION.
      *-----------------------------------------------------------------
      *--> Consulta o estado mais recente da regra.
      *-----------------------------------------------------------------
           MOVE 0017                   TO  MSG1-CD-SEQL-ERRO

           MOVE SGTSPE13               TO  NM-PGM
           MOVE TB-L089-CD-RGR-VLDC-EXAR (AUX-IC-TAB)
                                       TO  SGTSPE13-CD-RGR-VLDC
           PERFORM 900000-EXECUTAR-MODULO
           .
       232300-SAI.
           EXIT.

      *-----------------------------------------------------------------
       240000-BUSCA-RLZD               SECTION.
      *-----------------------------------------------------------------
      *--> Busca os valores realizados para a operação pesquisada
      *    (recebida na entrada do SGTS0163)
      *-----------------------------------------------------------------
           MOVE 0018                   TO  MSG1-CD-SEQL-ERRO

           MOVE SGTS0163-CD-CTRA-TIT-VL    TO SGTS0189-CD-CTRA-TIT-VL
           MOVE SGTS0163-CD-OPR-ETRD-MCP   TO SGTS0189-CD-OPR-ETRD-MCP
           MOVE SGTS0163-NR-VRS-OPR-ETRD   TO SGTS0189-NR-VRS-OPR-ETRD
           MOVE SGTSLA06-S-CD-CLI(1)       TO SGTS0189-CD-CLI
           MOVE SGTST005-QT-MM-CARE-RSGT   TO SGTS0189-QT-MM-CARE-RSGT
           MOVE SGTST005-VL-CPR-OPR        TO SGTS0189-VL-CPR-OPR
           MOVE SGTST005-VL-PU-EMS-TIT-OPR TO SGTS0189-VL-PU-EMS-TIT-OPR
           MOVE SGTST005-DT-CMPS-FUT-NOTA  TO SGTS0189-DT-CMPS-FUT-NOTA
           MOVE SGTST005-DT-VNCT-OPR       TO SGTS0189-DT-VNCT-OPR
           MOVE SGTST005-DT-EMS-TIT-OPR    TO SGTS0189-DT-EMS-TIT-OPR
           MOVE SGTST005-CD-CTGR-TVM       TO SGTS0189-CD-CTGR-TVM

           MOVE SGTS0189            TO  NM-PGM

           PERFORM 900000-EXECUTAR-MODULO

           IF  SGTS0189-QT-OCR-TAB    EQUAL  ZEROS
               SET ERRO-PROGRAMADO    TO  TRUE
               MOVE 0688              TO  MSG1-CD-ERRO-TS
               MOVE 'Informações intervenientes inexistentes.'
                                      TO  MSG1-TX-ERRO-TS
               SET MNT-VRV            TO  TRUE
               PERFORM 999000-ERRO
           END-IF
           .
       240000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       241000-BUSCA-CLI-EMS            SECTION.
      *--> Busca Relacao de Cliente Emissor
      *-----------------------------------------------------------------
           MOVE 0019                   TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTS0163-CD-CTRA-TIT-VL
                                       TO  SGTSLA06-CD-CTRA-TVM
           MOVE  SGTS0163-CD-OPR-ETRD-MCP
                                       TO  SGTSLA06-CD-OPR-ETRD-MCP
           MOVE  SGTS0163-NR-VRS-OPR-ETRD
                                       TO  SGTSLA06-NR-VRS-OPR-ETRD

      *--> Tipo de Função do Cliente = 0001 - Cliente Emissor
           MOVE  0001                  TO  SGTSLA06-CD-TIP-FUC-CLI

           MOVE  SGTSLA06              TO  NM-PGM

           PERFORM 900000-EXECUTAR-MODULO

           IF  SGTSLA06-QT-OCR-TAB         EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0664              TO  MSG1-CD-ERRO-TS
               MOVE  'Cliente emissor não encontrado.'
                                       TO  MSG1-TX-ERRO-TS
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               PERFORM  969000-MONITORAR-VARIAVEIS
               SET   MNT-VRV           TO  TRUE
               PERFORM  999000-ERRO
           END-IF
           .
       241000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       250000-TRATA-TAB-VDD            SECTION.
      *--> Consiste se operação entra ou não em exceção a partir de
      *    Tabela Verdade.
      *-----------------------------------------------------------------
           MOVE 0020                   TO  MSG1-CD-SEQL-ERRO

            IF IDX-SEARCH = 1
               MOVE 'S'     TO   AUX-IN-REG-INI-RGR
            ELSE
              IF TB-SMLC-CD-RGR-PPL (IDX-SEARCH)
                 NOT EQUAL TB-SMLC-CD-RGR-PPL (IDX-SEARCH - 1)
                   MOVE 'S'     TO   AUX-IN-REG-INI-RGR
              ELSE
               MOVE 'N'     TO   AUX-IN-REG-INI-RGR
               END-IF
            END-IF

           MOVE TB-SMLC-NR-GR-CND-ITC(IDX-SEARCH) TO AX-NR-GR
           MOVE 'N'  TO  AUX-ENTROU-COND-ELSE

           EVALUATE TB-SMLC-TIP-CMD-VLDC(IDX-SEARCH)
           WHEN 1
               PERFORM 250100-EFETUA-COMPAR
               PERFORM 250200-APURAR-TAB-VDD
               IF TB-SMLC-TIP-CMD-VLDC(IDX-SEARCH  +  1)  NOT EQUAL 1
                  OR TB-SMLC-NR-GR-CND-ITC(IDX-SEARCH) NOT EQUAL
                               TB-SMLC-NR-GR-CND-ITC(IDX-SEARCH + 1 )
                  IF AUX-ENTROU-COND = 'N'
                     MOVE 'N'    TO AUX-ENCONTROU-FIM-CND
                     ADD 1 TO IDX-SEARCH GIVING IDX-2
                     PERFORM UNTIL
                                AUX-ENCONTROU-FIM-CND = 'S'
                                OR IDX-2  >  IDX-SMLC
                         IF (TB-SMLC-TIP-CMD-VLDC(IDX-2) = 2 OR 3)
                             AND TB-SMLC-NR-GR-CND-ITC(IDX-2) =
                                       TB-SMLC-NR-GR-CND-ITC(IDX-SEARCH)
                             MOVE IDX-2 TO  IDX-SEARCH
                             MOVE 'S' TO AUX-ENCONTROU-FIM-CND
                         ELSE
                             ADD 1 TO IDX-2
                         END-IF
                     END-PERFORM

                     IF AUX-ENCONTROU-FIM-CND = 'N'
                        SET  ERRO-PROGRAMADO     TO  TRUE
                        MOVE 0665                TO  MSG1-CD-ERRO-TS
                        MOVE IDX-SEARCH          TO  IDX-SEARCH-DISPLAY
                        MOVE TB-SMLC-CD-RGR(IDX-SEARCH)
                                                 TO  AUX-RGR-DISPLAY
                        MOVE TB-SMLC-NR-SEQL-OGNL(IDX-SEARCH)
                                                 TO AUX-ITC-DISPLAY
                        STRING 'Erro na interpretação da regra. Término'
                          ' da estrutura de decisão não encontrada(1). '
                              IDX-SEARCH-DISPLAY  ' ' AUX-RGR-DISPLAY
                              ' ' AUX-ITC-DISPLAY
                             DELIMITED BY SIZE INTO  MSG1-TX-ERRO-TS
                        PERFORM 999000-ERRO
                       END-IF
                  END-IF
               END-IF

           WHEN 2
               MOVE 'S'  TO  AUX-ENTROU-COND-ELSE
               MOVE 'N'    TO AUX-ENCONTROU-FIM-CND
               ADD 1 TO IDX-SEARCH GIVING IDX-2
               PERFORM UNTIL
                   AUX-ENCONTROU-FIM-CND = 'S'
                   OR IDX-2  >  IDX-SMLC
                   IF (TB-SMLC-TIP-CMD-VLDC(IDX-2) =  3)
                    AND TB-SMLC-NR-GR-CND-ITC(IDX-2) =
                               TB-SMLC-NR-GR-CND-ITC(IDX-SEARCH)
                       MOVE IDX-2     TO IDX-SEARCH
                       MOVE 'S' TO AUX-ENCONTROU-FIM-CND
                   ELSE
                       ADD 1 TO IDX-2
                   END-IF
               END-PERFORM

               IF AUX-ENCONTROU-FIM-CND = 'N'
                  SET  ERRO-PROGRAMADO    TO  TRUE
                  MOVE 0665               TO  MSG1-CD-ERRO-TS
                        MOVE IDX-SEARCH          TO  IDX-SEARCH-DISPLAY
                        MOVE TB-SMLC-CD-RGR(IDX-SEARCH)
                                                 TO  AUX-RGR-DISPLAY
                        MOVE TB-SMLC-NR-SEQL-OGNL(IDX-SEARCH)
                                                 TO AUX-ITC-DISPLAY
                        STRING 'Erro na interpretação da regra. Término'
                          ' da estrutura de decisão não encontrada(2). '
                              IDX-SEARCH-DISPLAY  ' ' AUX-RGR-DISPLAY
                              ' ' AUX-ITC-DISPLAY
                             DELIMITED BY SIZE INTO  MSG1-TX-ERRO-TS
                  PERFORM 999000-ERRO
               END-IF

           WHEN 3
           WHEN 4
               CONTINUE

           WHEN 5
                MOVE 100 TO TB-SMLC-NR-VRS-RGR-PPL (IDX-SEARCH)
                PERFORM 250400-GDA-REG-EXCO

           WHEN 7
                MOVE ZEROES            TO SGTS0163-QT-OCR-TAB
                MOVE 'N'               TO SGTS0163-IN-CNTD-PSQ
                PERFORM  980000-GRAVAR-CONTAINER-RPST
                PERFORM  999999-ENCERRAR

           END-EVALUATE
           .
       250000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       250100-EFETUA-COMPAR            SECTION.
      *--> Compara o realizado com o parametrizado, de acordo com o
      *    operando cadastrado (> ; < ; = ; <> ; >= ; =<)
      *    Pode ser parametrizado nro, ou valor ou texto).
      *    Obrigatoriamente um dos três e somente um dos três.
      *
      *    Código Tipo Atributo:
      *    TEXTO 1 - Indicador        VALOR 2 - Prazo
      *    NUMERO 3 - Segmento        TEXTO 4 - Risco
      *    VALOR 5 - Valor            NUMERO 6 - Natureza Juridica
      *    NUMERO 7 - Unidade Organizacional  TEXTO 8 - Data
      *-----------------------------------------------------------------
           MOVE 0021                   TO  MSG1-CD-SEQL-ERRO

           MOVE TB-SMLC-CD-ATB-VLDC(IDX-SEARCH)     TO  AUX-ATBC
           EVALUATE TB-SMLC-CD-TIP-CFT-ITEM(IDX-SEARCH)
      *    TB-SMLC-CD-TIP-CFT-ITEM
      *     1 >  /  2 < / 3 = / 4 <> / 5 >= / 6 =<
           WHEN 1
               EVALUATE TB-SMLC-CD-TIP-ATB-VLDC(IDX-SEARCH)
               WHEN 1
               WHEN 4
               WHEN 8
                   IF SGTS0189-TX-RLZD(AUX-ATBC)    >
                             TB-SMLC-TX-ITC-RGR-VLDC(IDX-SEARCH)

                      MOVE 'S'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   ELSE
                      MOVE 'N'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   END-IF
               WHEN 2
               WHEN 5
                   IF SGTS0189-VL-RLZD(AUX-ATBC)  >
                             TB-SMLC-VL-ITC-RGR-VLDC(IDX-SEARCH)

                      MOVE 'S'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   ELSE
                      MOVE 'N'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   END-IF
               WHEN 3
               WHEN 6
               WHEN 7
                   IF SGTS0189-NR-RLZD(AUX-ATBC)  >
                             TB-SMLC-NR-ITC-RGR-VLDC(IDX-SEARCH)

                      MOVE 'S'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   ELSE
                      MOVE 'N'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   END-IF
               WHEN OTHER
                   SET  ERRO-PROGRAMADO    TO  TRUE
                   MOVE 0648               TO  MSG1-CD-ERRO-TS
                   STRING 'Não consta Tipo Atributo para o Atributo '
                      AUX-ATBC
                      DELIMITED BY SIZE INTO  MSG1-TX-ERRO-TS
                   PERFORM 999000-ERRO
               END-EVALUATE

           WHEN 2
               EVALUATE TB-SMLC-CD-TIP-ATB-VLDC(IDX-SEARCH)
               WHEN 1
               WHEN 4
               WHEN 8
                   IF SGTS0189-TX-RLZD(AUX-ATBC)   <
                             TB-SMLC-TX-ITC-RGR-VLDC(IDX-SEARCH)

                      MOVE 'S'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   ELSE
                      MOVE 'N'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   END-IF
               WHEN 2
               WHEN 5
                   IF SGTS0189-VL-RLZD(AUX-ATBC)   <
                             TB-SMLC-VL-ITC-RGR-VLDC(IDX-SEARCH)

                      MOVE 'S'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   ELSE
                      MOVE 'N'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   END-IF
               WHEN 3
               WHEN 6
               WHEN 7
                   IF SGTS0189-NR-RLZD(AUX-ATBC)   <
                             TB-SMLC-NR-ITC-RGR-VLDC(IDX-SEARCH)

                      MOVE 'S'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   ELSE
                      MOVE 'N'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   END-IF
               WHEN OTHER
                   SET  ERRO-PROGRAMADO    TO  TRUE
                   MOVE 0648               TO  MSG1-CD-ERRO-TS
                   STRING 'Não consta Tipo Atributo para o Atributo '
                      AUX-ATBC
                      DELIMITED BY SIZE INTO  MSG1-TX-ERRO-TS
                   PERFORM 999000-ERRO
               END-EVALUATE

           WHEN 3
               EVALUATE TB-SMLC-CD-TIP-ATB-VLDC(IDX-SEARCH)
               WHEN 1
               WHEN 4
               WHEN 8
                   IF SGTS0189-TX-RLZD(AUX-ATBC)   EQUAL
                             TB-SMLC-TX-ITC-RGR-VLDC(IDX-SEARCH)

                      MOVE 'S'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   ELSE
                      MOVE 'N'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   END-IF
               WHEN 2
               WHEN 5
                   IF SGTS0189-VL-RLZD(AUX-ATBC)   EQUAL
                             TB-SMLC-VL-ITC-RGR-VLDC(IDX-SEARCH)

                      MOVE 'S'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   ELSE
                      MOVE 'N'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   END-IF
               WHEN 3
               WHEN 6
               WHEN 7
                   IF SGTS0189-NR-RLZD(AUX-ATBC) EQUAL
                             TB-SMLC-NR-ITC-RGR-VLDC(IDX-SEARCH)

                      MOVE 'S'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   ELSE
                      MOVE 'N'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   END-IF
               WHEN OTHER
                   SET  ERRO-PROGRAMADO    TO  TRUE
                   MOVE 0648               TO  MSG1-CD-ERRO-TS
                   STRING 'Não consta Tipo Atributo para o Atributo '
                      AUX-ATBC
                      DELIMITED BY SIZE INTO  MSG1-TX-ERRO-TS
                   PERFORM 999000-ERRO
               END-EVALUATE

           WHEN 4
               EVALUATE TB-SMLC-CD-TIP-ATB-VLDC(IDX-SEARCH)
               WHEN 1
               WHEN 4
               WHEN 8
                   IF SGTS0189-TX-RLZD(AUX-ATBC)   NOT EQUAL
                             TB-SMLC-TX-ITC-RGR-VLDC(IDX-SEARCH)

                      MOVE 'S'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   ELSE
                      MOVE 'N'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   END-IF
               WHEN 2
               WHEN 5
                   IF SGTS0189-VL-RLZD(AUX-ATBC)  NOT EQUAL
                             TB-SMLC-VL-ITC-RGR-VLDC(IDX-SEARCH)

                      MOVE 'S'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   ELSE
                      MOVE 'N'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   END-IF
               WHEN 3
               WHEN 6
               WHEN 7
                   IF SGTS0189-NR-RLZD(AUX-ATBC)  NOT EQUAL
                             TB-SMLC-NR-ITC-RGR-VLDC(IDX-SEARCH)

                      MOVE 'S'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   ELSE
                      MOVE 'N'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   END-IF
               WHEN OTHER
                   SET  ERRO-PROGRAMADO    TO  TRUE
                   MOVE 0648               TO  MSG1-CD-ERRO-TS
                   STRING 'Não consta Tipo Atributo para o Atributo '
                      AUX-ATBC
                      DELIMITED BY SIZE INTO  MSG1-TX-ERRO-TS
                   PERFORM 999000-ERRO
               END-EVALUATE

           WHEN 5
               EVALUATE TB-SMLC-CD-TIP-ATB-VLDC(IDX-SEARCH)
               WHEN 1
               WHEN 4
               WHEN 8
                   IF  SGTS0189-TX-RLZD(AUX-ATBC)  NOT LESS
                             TB-SMLC-TX-ITC-RGR-VLDC(IDX-SEARCH)

                      MOVE 'S'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   ELSE
                      MOVE 'N'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   END-IF
               WHEN 2
               WHEN 5
                   IF SGTS0189-VL-RLZD(AUX-ATBC)   NOT LESS
                             TB-SMLC-VL-ITC-RGR-VLDC(IDX-SEARCH)

                      MOVE 'S'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   ELSE
                      MOVE 'N'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   END-IF
               WHEN 3
               WHEN 6
               WHEN 7
                   IF SGTS0189-NR-RLZD(AUX-ATBC)  NOT LESS
                             TB-SMLC-NR-ITC-RGR-VLDC(IDX-SEARCH)

                      MOVE 'S'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   ELSE
                      MOVE 'N'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   END-IF
               WHEN OTHER
                   SET  ERRO-PROGRAMADO    TO  TRUE
                   MOVE 0648               TO  MSG1-CD-ERRO-TS
                   STRING 'Não consta Tipo Atributo para o Atributo '
                      AUX-ATBC
                      DELIMITED BY SIZE INTO  MSG1-TX-ERRO-TS
                   PERFORM 999000-ERRO
               END-EVALUATE

           WHEN 6
               EVALUATE TB-SMLC-CD-TIP-ATB-VLDC(IDX-SEARCH)
               WHEN 1
               WHEN 4
               WHEN 8
                   IF SGTS0189-TX-RLZD(AUX-ATBC)  NOT GREATER
                             TB-SMLC-TX-ITC-RGR-VLDC(IDX-SEARCH)

                      MOVE 'S'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   ELSE
                      MOVE 'N'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   END-IF
               WHEN 2
               WHEN 5
                   IF SGTS0189-VL-RLZD(AUX-ATBC)  NOT GREATER
                             TB-SMLC-VL-ITC-RGR-VLDC(IDX-SEARCH)

                      MOVE 'S'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   ELSE
                      MOVE 'N'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   END-IF
               WHEN 3
               WHEN 6
               WHEN 7
                   IF SGTS0189-NR-RLZD(AUX-ATBC)  NOT GREATER
                             TB-SMLC-NR-ITC-RGR-VLDC(IDX-SEARCH)

                      MOVE 'S'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   ELSE
                      MOVE 'N'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                   END-IF
               WHEN OTHER
                   SET  ERRO-PROGRAMADO    TO  TRUE
                   MOVE 0648               TO  MSG1-CD-ERRO-TS
                   STRING 'Não consta Tipo Atributo para o Atributo '
                      AUX-ATBC
                      DELIMITED BY SIZE INTO  MSG1-TX-ERRO-TS
                   PERFORM 999000-ERRO
               END-EVALUATE

           END-EVALUATE
           .
       250100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       250200-APURAR-TAB-VDD           SECTION.
      *--> Consiste se operação entra ou não em exceção a partir de
      *    Tabela Verdade.
      *-----------------------------------------------------------------
           MOVE 0022                   TO  MSG1-CD-SEQL-ERRO

           IF TB-SMLC-TRUE(IDX-SEARCH)     NOT   EQUAL SPACES
              IF AUX-IN-REG-INI-RGR = 'N'

                 IF  TB-SMLC-TRUE(IDX-SEARCH - 1) NOT EQUAL SPACES
                    IF  TB-SMLC-CD-TIP-CFT-LGC(IDX-SEARCH) EQUAL 2  OR
                                                              ZEROS
                        IF TB-SMLC-TRUE(IDX-SEARCH)           = 'S' AND
                           TB-SMLC-TRUE(IDX-SEARCH - 1)       = 'S'
                           CONTINUE
                        ELSE
                           MOVE 'N'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                        END-IF
                    ELSE
                        IF TB-SMLC-TRUE(IDX-SEARCH)      =  'S'  OR
                           TB-SMLC-TRUE(IDX-SEARCH - 1)  =  'S'
                           CONTINUE
                        ELSE
                           MOVE 'N'       TO  TB-SMLC-TRUE(IDX-SEARCH)
                        END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF

           IF  TB-SMLC-TRUE(IDX-SEARCH) =  'S'
               MOVE 'S'  TO  AUX-ENTROU-COND
           ELSE
               MOVE 'N'  TO  AUX-ENTROU-COND
           END-IF
           .
       250200-SAI.
           EXIT.

      *-----------------------------------------------------------------
       250300-FORMATA-REGISTRO         SECTION.
      *--> Formata registro de saida, com os dados da exceção gerada.
      *    Somente para fins de facilitar a instrução STRING, foi
      *    movido para o campo texto se foi parametrizado nro ou valor
      *    (comando EVALUATE abaixo).
      *-----------------------------------------------------------------
           MOVE 0023                   TO  MSG1-CD-SEQL-ERRO

           ADD  1  TO  SGTS0163-QT-OCR-TAB
           EVALUATE TRUE
           WHEN TB-SMLC-VL-ITC-RGR-VLDC (IDX-SEARCH) NOT EQUAL ZEROS
                MOVE TB-SMLC-VL-ITC-RGR-VLDC (IDX-SEARCH)
                               TO  AUX-VL-ITC
                MOVE AUX-VL-ITC-A
                               TO  TB-SMLC-TX-ITC-RGR-VLDC(IDX-SEARCH)
                MOVE SGTS0189-VL-RLZD(AUX-ATBC)
                               TO  AUX-VL-ITC
                MOVE AUX-VL-ITC-A  TO   SGTS0189-TX-RLZD(AUX-ATBC)

           WHEN TB-SMLC-NR-ITC-RGR-VLDC (IDX-SEARCH) NOT EQUAL ZEROS
                MOVE TB-SMLC-NR-ITC-RGR-VLDC (IDX-SEARCH)
                              TO  TB-SMLC-TX-ITC-RGR-VLDC (IDX-SEARCH)
                MOVE SGTS0189-NR-RLZD(AUX-ATBC)
                              TO  SGTS0189-TX-RLZD(AUX-ATBC)
           END-EVALUATE

           MOVE  TB-SMLC-CD-RGR (IDX-SEARCH)  TO  AUX-RGR-DISPLAY
           MOVE  TB-SMLC-NR-VRS-RGR-VLDC (IDX-SEARCH)
                                              TO  AUX-VRS-DISPLAY
           MOVE  TB-SMLC-NR-SEQL-ITC  (IDX-SEARCH)
                                              TO  AUX-ITC-DISPLAY
           .
       250300-SAI.
           EXIT.

      *-----------------------------------------------------------------
       250400-GDA-REG-EXCO             SECTION.
      *--> Guarda em tabela interna as linhas do comando 5 que geram
      *    registro de exceção.
      *-----------------------------------------------------------------
           MOVE 0024                   TO  MSG1-CD-SEQL-ERRO

           ADD  1              TO  IDX-EXCO

           MOVE TB-SMLC-CD-RGR (IDX-SEARCH)
                               TO  TB-EXCO-CD-RGR(IDX-EXCO)
           MOVE TB-SMLC-NR-VRS-RGR-VLDC(IDX-SEARCH)
                               TO  TB-EXCO-NR-VRS-RGR-VLDC (IDX-EXCO)
           MOVE TB-SMLC-NR-SEQL-ITC(IDX-SEARCH)
                               TO  TB-EXCO-NR-SEQL-ITC(IDX-EXCO)
           MOVE TB-SMLC-TIP-CMD-VLDC (IDX-SEARCH)
                               TO  TB-EXCO-TIP-CMD-VLDC(IDX-EXCO)
           MOVE TB-SMLC-NR-GR-CND-ITC(IDX-SEARCH)
                               TO  TB-EXCO-NR-GR-CND-ITC(IDX-EXCO)
           MOVE TB-SMLC-IN-RGR-PPL  (IDX-SEARCH)
                               TO  TB-EXCO-IN-RGR-PPL(IDX-EXCO)
           MOVE TB-SMLC-CD-RGR-PPL (IDX-SEARCH)
                               TO  TB-EXCO-CD-RGR-PPL(IDX-EXCO)
           MOVE TB-SMLC-NR-VRS-RGR-PPL (IDX-SEARCH)
                               TO  TB-EXCO-NR-VRS-RGR-PPL(IDX-EXCO)
           .
       250400-SAI.
           EXIT.

      *-----------------------------------------------------------------
       260000-VERIF-EXCO               SECTION.
      *--> Verifica quais regras entraram em tratamento de exceção
      *    para a operação enviada.
      *-----------------------------------------------------------------
           MOVE 0025                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'N'      TO AUX-ENCONTROU-EXCO

           PERFORM VARYING IDX-EXCO FROM 1 BY 1
                    UNTIL   IDX-EXCO > MAX-EXCO
                    OR AUX-ENCONTROU-EXCO  =  'S'
               IF TB-SMLC-CD-RGR-PPL  (IDX-SEARCH)  EQUAL
                                       TB-EXCO-CD-RGR-PPL (IDX-EXCO)
                  MOVE 'S'      TO  AUX-ENCONTROU-EXCO
               END-IF
           END-PERFORM

           SUBTRACT 1 FROM IDX-EXCO
           IF AUX-ENCONTROU-EXCO = 'S'
              PERFORM 261000-GRAVA-EXCO
           END-IF
           .
       260000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       261000-GRAVA-EXCO               SECTION.
      *--> Formata registros com as instruções das regras que geraram
      *    registro de exceção.
      *-----------------------------------------------------------------
           MOVE 0026                   TO  MSG1-CD-SEQL-ERRO

           ADD  1                   TO  SGTS0163-QT-OCR-TAB

           MOVE 'N'  TO  SGTS0163-IN-LNH-EXCO(SGTS0163-QT-OCR-TAB)

           IF TB-SMLC-NR-SEQL-ITC(IDX-SEARCH) EQUAL
                                       TB-EXCO-NR-SEQL-ITC  (IDX-EXCO)
              MOVE 'S'  TO  SGTS0163-IN-LNH-EXCO(SGTS0163-QT-OCR-TAB)
           END-IF

           MOVE TB-SMLC-CD-RGR-PPL(IDX-SEARCH)          TO
                       SGTS0163-CD-RGR-PPL(SGTS0163-QT-OCR-TAB)
           MOVE TB-SMLC-NR-VRS-RGR-PPL(IDX-SEARCH)      TO
                       SGTS0163-NR-VRS-RGR-PPL (SGTS0163-QT-OCR-TAB)
           MOVE TB-SMLC-CD-TIP-EST (IDX-SEARCH)         TO
                       SGTS0163-CD-TIP-EST (SGTS0163-QT-OCR-TAB)
           MOVE TB-SMLC-CD-RGR(IDX-SEARCH)              TO
                       SGTS0163-CD-RGR-SCDR (SGTS0163-QT-OCR-TAB)
           MOVE TB-SMLC-NR-VRS-RGR-VLDC(IDX-SEARCH)     TO
                       SGTS0163-NR-VRS-RGR-SCDR (SGTS0163-QT-OCR-TAB)
           MOVE TB-SMLC-CD-TIP-EST-SCDR (IDX-SEARCH)    TO
                       SGTS0163-CD-TIP-EST-SCDR (SGTS0163-QT-OCR-TAB)
           MOVE TB-SMLC-IN-RGR-PPL(IDX-SEARCH)          TO
                       SGTS0163-IN-RGR-PPL (SGTS0163-QT-OCR-TAB)
           MOVE TB-SMLC-NR-SEQL-ITC(IDX-SEARCH)         TO
                       SGTS0163-NR-SEQL-ITC (SGTS0163-QT-OCR-TAB)
           MOVE TB-SMLC-TIP-CMD-VLDC(IDX-SEARCH)        TO
                       SGTS0163-TIP-CMD-VLDC (SGTS0163-QT-OCR-TAB)
           MOVE TB-SMLC-NR-GR-CND-ITC(IDX-SEARCH)       TO
                       SGTS0163-NR-GR-CND (SGTS0163-QT-OCR-TAB)
           MOVE TB-SMLC-CD-ATB-VLDC(IDX-SEARCH)         TO
                       SGTS0163-CD-ATB-VLDC (SGTS0163-QT-OCR-TAB)
           MOVE TB-SMLC-CD-TIP-CFT-LGC(IDX-SEARCH)      TO
                       SGTS0163-CD-TIP-CFT-LGC (SGTS0163-QT-OCR-TAB)
           MOVE TB-SMLC-CD-TIP-CFT-ITEM(IDX-SEARCH)     TO
                       SGTS0163-CD-TIP-CFT-ITEM (SGTS0163-QT-OCR-TAB)
           MOVE TB-SMLC-VL-ITC-RGR-VLDC(IDX-SEARCH)     TO
                       SGTS0163-VL-ITC-RGR (SGTS0163-QT-OCR-TAB)
           MOVE TB-SMLC-NR-ITC-RGR-VLDC(IDX-SEARCH)     TO
                       SGTS0163-NR-ITC-RGR (SGTS0163-QT-OCR-TAB)
           MOVE TB-SMLC-TX-ITC-RGR-VLDC(IDX-SEARCH)     TO
                       SGTS0163-TX-ITC-RGR (SGTS0163-QT-OCR-TAB)

           IF TB-SMLC-CD-ATB-VLDC(IDX-SEARCH)  > 0
              MOVE TB-SMLC-CD-ATB-VLDC(IDX-SEARCH)      TO  AUX-ATBC
              MOVE SGTS0189-VL-RLZD (AUX-ATBC)           TO
                       SGTS0163-VL-ITC-RLZD (SGTS0163-QT-OCR-TAB)
              MOVE SGTS0189-NR-RLZD (AUX-ATBC)           TO
                       SGTS0163-NR-ITC-RLZD (SGTS0163-QT-OCR-TAB)
              MOVE SGTS0189-TX-RLZD (AUX-ATBC)           TO
                       SGTS0163-TX-ITC-RLZD (SGTS0163-QT-OCR-TAB)
           END-IF

           MOVE TB-SMLC-NR-SEQL-OGNL(IDX-SEARCH)     TO
                    SGTS0163-NR-SEQL-OGNL (SGTS0163-QT-OCR-TAB)
           .
       261000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *   Section da Faixa de 900000 a 969900 para Demais Rotinas
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *--> Executa um modulo implementado com CONTAINER
      *    Nao incluir nessa section os modulos chamados via COMMAREA
      *-----------------------------------------------------------------

      *--> Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o modulo
      *     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM
               WHEN SGTS0189
                   EXEC CICS LINK
                             PROGRAM ( SGTS0189  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSL089
                   EXEC CICS LINK
                             PROGRAM ( SGTSL089  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSL092
                   EXEC CICS LINK
                             PROGRAM ( SGTSL092  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSLA06
                   EXEC CICS LINK
                             PROGRAM ( SGTSLA06  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSPE10
                   EXEC CICS LINK
                             PROGRAM ( SGTSPE10  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSPE13
                   EXEC CICS LINK
                             PROGRAM ( SGTSPE13  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO  TO  TRUE
                    MOVE  9006          TO  MSG1-CD-ERRO-TS
                    STRING  'Não definida a execução do módulo'
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO            MSG1-TX-ERRO-TS
                   PERFORM 999000-ERRO
           END-EVALUATE

           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    STRING
                    'Erro na execução do módulo ' NM-PGM ', '
                    'RESP  ='   ' ' MSG1-RESP1  ', '
                    'RESP2 =' ' ' MSG1-RESP2
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    MOVE 3004          TO MSG1-CD-ERRO-TS
                    SET ERRO-CICS      TO TRUE
                    PERFORM 999000-ERRO
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a le itura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       920000-CHAMAR-COMMAREA          SECTION.
      *-----------------------------------------------------------------

      *--> Movimento nome do módulo para monitoração de variáveis
           EVALUATE  NM-PGM
           WHEN SGTST005
                EXEC CICS LINK
                     PROGRAM  (SGTST005)
                     COMMAREA (SGTST005-DADOS)
                     FLENGTH  (LENGTH OF SGTST005-DADOS)
                END-EXEC

           WHEN OTHER
                CONTINUE
           END-EVALUATE
           .
       920000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE NM-PGM
               WHEN SGTS0189
                 MOVE 'SGTS0189-CD-CTRA-TIT-VL'  TO MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0189-CD-CTRA-TIT-VL   TO MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0189-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
                 IF SGTS0189-CD-OPR-ETRD-MCP     IS NUMERIC
                    MOVE  SGTS0189-CD-OPR-ETRD-MCP  TO CD-OPR-ETRD-MCP
                    MOVE CD-OPR-ETRD-MCP-TX      TO MSGKW999-VL-VRV-MNT
                 ELSE
                    MOVE 'Valor não numérico.'   TO MSGKW999-VL-VRV-MNT
                 END-IF
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0189-NR-VRS-OPR-ETRD' TO  MSGKW999-NM-VRV-MNT
                 MOVE SGTS0189-NR-VRS-OPR-ETRD   TO  MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0189-CD-CLI'   TO  MSGKW999-NM-VRV-MNT
                 MOVE SGTS0189-CD-CLI     TO  MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0189-QT-MM-CARE-RSGT' TO  MSGKW999-NM-VRV-MNT
                 MOVE SGTS0189-QT-MM-CARE-RSGT   TO  MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0189-DT-CMPS-FUT-NOTA' TO MSGKW999-NM-VRV-MNT
                 MOVE SGTS0189-DT-CMPS-FUT-NOTA   TO MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT


                 MOVE 'SGTS0189-VL-CPR-OPR'      TO  MSGKW999-NM-VRV-MNT
                 IF  SGTS0189-VL-CPR-OPR     IS NUMERIC
                     MOVE SGTS0189-VL-CPR-OPR    TO  VL-CPR-OPR
                     MOVE VL-CPR-OPR-D           TO  MSGKW999-VL-VRV-MNT
                 ELSE
                     MOVE 'Valor não numérico.'  TO  MSGKW999-VL-VRV-MNT
                 END-IF
                 PERFORM 999003-ADICIONA-VRV-MNT


                 MOVE 'SGTS0189-VL-CPR-OPR'  TO  MSGKW999-NM-VRV-MNT
                 IF  SGTS0189-VL-PU-EMS-TIT-OPR  IS NUMERIC
                     MOVE SGTS0189-VL-PU-EMS-TIT-OPR TO
                                                 VL-PU-EMS-TIT-OPR
                     MOVE VL-PU-EMS-TIT-OPR-D    TO  MSGKW999-VL-VRV-MNT
                 ELSE
                     MOVE 'Valor não numérico.'  TO  MSGKW999-VL-VRV-MNT
                 END-IF
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0189-DT-VNCT-OPR'     TO  MSGKW999-NM-VRV-MNT
                 MOVE SGTS0189-DT-VNCT-OPR       TO  MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0189-DT-EMS-TIT-OPR'  TO  MSGKW999-NM-VRV-MNT
                 MOVE SGTS0189-DT-EMS-TIT-OPR    TO  MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0189-QT-OCR-TAB-E'    TO  MSGKW999-NM-VRV-MNT
                 MOVE SGTS0189-QT-OCR-TAB-E      TO  MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

                 PERFORM VARYING  IDX-ERRO   FROM 1 BY 1
                      UNTIL IDX-ERRO     >  SGTS0189-QT-OCR-TAB-E
                    MOVE 'SGTS0189-CD-ATB-VLDC-E'
                                               TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTS0189-CD-ATB-VLDC-E(IDX-ERRO)
                                               TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
                 END-PERFORM

               WHEN SGTSL089
                    MOVE 'SGTSL089-CD-RGR-VLDC'  TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSL089-CD-RGR-VLDC    TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL089-NR-VRS-RGR-VLDC'
                                                 TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSL089-NR-VRS-RGR-VLDC
                                                 TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL089-NR-SEQL-ITC-ULT'
                                                 TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSL089-NR-SEQL-ITC-ULT
                                                 TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-TX-ERRO'      TO  MSGKW999-NM-VRV-MNT
                    MOVE IIBKERRO-TX-ERRO        TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-CD-ERRO'      TO  MSGKW999-NM-VRV-MNT
                    MOVE IIBKERRO-CD-ERRO        TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL092
                    MOVE 'SGTSL092-CD-TIP-SRVC'  TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSL092-CD-TIP-SRVC    TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL092-CD-RGR-VLDC-INI'
                                                 TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSL092-CD-RGR-VLDC-INI
                                                 TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL092-CD-RGR-VLDC-FIM'
                                                 TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSL092-CD-RGR-VLDC-FIM
                                                 TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL092-CD-TIP-EST-DCS-INI'
                                                 TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSL092-CD-TIP-EST-DCS-INI
                                                 TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL092-CD-TIP-EST-DCS-FIN'
                                                 TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSL092-CD-TIP-EST-DCS-FIN
                                                 TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL092-DT-PSQ-INI'   TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSL092-DT-PSQ-INI     TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL092-DT-PSQ-FIN'   TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSL092-DT-PSQ-FIN     TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL092-DT-PSQ'       TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSL092-DT-PSQ         TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL092-ULT-TS-EST'   TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSL092-ULT-TS-EST     TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL092-NM-RGR-VLDC-CRCT-E'
                                                 TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSL092-NM-RGR-VLDC-CRCT-E
                                                 TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL092-IN-RGR-PPL-E' TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSL092-IN-RGR-PPL-E   TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL092-NR-NOTA-TCN-OPR-E'
                                                 TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSL092-NR-NOTA-TCN-OPR-E
                                                 TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL092-CD-PRD-INI'   TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSL092-CD-PRD-INI     TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL092-CD-PRD-FIN'   TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSL092-CD-PRD-FIN     TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL092-CD-MDLD-INI'  TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSL092-CD-MDLD-INI    TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL092-CD-MDLD-FIN'  TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSL092-CD-MDLD-FIN    TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL092-CD-CTRA-TIT-VL-INI'
                                              TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL092-CD-CTRA-TIT-VL-INI
                                              TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL092-CD-CTRA-TIT-VL-FIM'
                                              TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL092-CD-CTRA-TIT-VL-FIM
                                              TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-TX-ERRO'      TO  MSGKW999-NM-VRV-MNT
                    MOVE IIBKERRO-TX-ERRO        TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-CD-ERRO'      TO  MSGKW999-NM-VRV-MNT
                    MOVE IIBKERRO-CD-ERRO        TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSLA06
                    MOVE 'SGTSLA06-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                    IF  SGTSLA06-CD-OPR-ETRD-MCP     IS NUMERIC
                        MOVE SGTSLA06-CD-OPR-ETRD-MCP TO CD-OPR-ETRD-MCP
                        MOVE CD-OPR-ETRD-MCP-TX  TO MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE 'Valor não numérico.'
                                                 TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSLA06-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSLA06-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSLA06-CD-CLI'       TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSLA06-CD-CLI         TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSLA06-CD-TIP-FUC-CLI'
                                                 TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSLA06-CD-TIP-FUC-CLI TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-TX-ERRO'      TO  MSGKW999-NM-VRV-MNT
                    MOVE IIBKERRO-TX-ERRO        TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-CD-ERRO'      TO  MSGKW999-NM-VRV-MNT
                    MOVE IIBKERRO-CD-ERRO        TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSPE10
                    MOVE 'SGTSPE10-CD-RGR-VLDC'  TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSPE10-CD-RGR-VLDC    TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSPE13
                    MOVE 'SGTSPE13-CD-RGR-VLDC'  TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSPE13-CD-RGR-VLDC    TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN  SBTIMEDT

                    MOVE  'WK-CD-FUC'          TO  MSGKW999-NM-VRV-MNT
                    MOVE  WK-CD-FUC            TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'WK-AREA-TRB-TAM'    TO  MSGKW999-NM-VRV-MNT
                    MOVE  WK-AREA-TRB-TAM      TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'WK-FMT-TS'          TO  MSGKW999-NM-VRV-MNT
                    MOVE  WK-FMT-TS            TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'WK-TAM-TS'          TO  MSGKW999-NM-VRV-MNT
                    MOVE  WK-TAM-TS            TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'WK-TS-FMTD'         TO  MSGKW999-NM-VRV-MNT
                    MOVE  WK-TS-FMTD           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

               WHEN OTHER
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

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CANAL             TO  TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       970000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------
           MOVE 0027                   TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao
           MOVE 'SGTS0163'             TO  NM-PGM
           MOVE 'SGTS0163-RQSC'        TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      * A requisição é variável (cláusula DEPENDING ON), não haverá a
      * comparação de tamanho lido e esperado

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0163-RQSC)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *--> A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------

      *--> Le o container de erro dos modulos chamados
           SET GET-CONTAINER           TO  TRUE
           MOVE 'PRM-ERRO'             TO  NM-CTNR
           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS GET
                     CONTAINER ( NM-CTNR                     )
                     INTO      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC

           EVALUATE TRUE
      *        Caso deseje personalizar ou não interromper o
      *        processamento, alterar para não dar erro aqui SOMENTE
      *        PARA O MÓDULO CHAMADO e tratar na section chamadora
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    SET   ERRO-PROGRAMA-ACIONADO TO  TRUE
                    MOVE  NM-PGM                 TO  MSG1-NM-PGM-ACND

                    IF NM-PGM(1:3) EQUAL 'SGT'
                       MOVE  IIBKERRO-CD-ERRO  TO  MSG1-CD-RTN-PGM-ACND
                                                   MSG1-CD-ERRO-TS
                       MOVE  IIBKERRO-TX-ERRO  TO  MSG1-TX-RTN-PGM-ACND
                                                   MSG1-TX-ERRO-TS
                    ELSE
                       MOVE 3004          TO MSG1-CD-ERRO-TS
                       STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                       MOVE IIBKERRO-TX-ERRO TO MSG1-TX-RTN-PGM-ACND
                       MOVE IIBKERRO-CD-ERRO TO MSG1-CD-RTN-PGM-ACND
                    END-IF

      *             Monitorar variáveis do módulo chamado
                     PERFORM 969000-MONITORAR-VARIAVEIS
                     PERFORM 999000-ERRO

               WHEN OTHER
                    SET ERRO-CONTAINER  TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *--> Recupera o container com os dados de resposta (saída).
      *    Módulo CONTAINER  -     NOME_PGM-RPST
      *    Módulo IIB (FIXO) -     PRM-DADOS-RPST
      *-----------------------------------------------------------------

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO  NM-CTNR

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           EVALUATE NM-PGM
               WHEN 'MSTS8000'
                    MOVE 'PRM-DADOS-RPST'  TO  NM-CTNR

               WHEN OTHER
                    MOVE NM-PGM        TO  NM-CTNR(1:8)
                    MOVE '-RPST'       TO  NM-CTNR(9:5)
           END-EVALUATE

      *--> Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

      *--> Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO  TRUE

      *--> Le a RESPOSTA de acordo com o programa que a gravou
           EVALUATE NM-PGM

      *        Calcular a quantidade de caracteres esperado e limpar
      *        a area de dados antes de executar o GET
      *        Para matrizes com dados variaveis (clausula depending
      *        on) nao ha como utilizar o comando INITIALIZE

               WHEN SGTS0189
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTS0189-RPST)
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSL089
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL089-RPST)
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSL092
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL092-RPST)
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSLA06
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSLA06-RPST)
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSPE10
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSPE10-RPST)
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSPE13
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSPE13-RPST)
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO    TO  TRUE
                    MOVE  9003               TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na leitura da resposta do módulo '
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       973000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO  TRUE

      *--> Obtem a quantidade de caracteres no container
           MOVE ZEROS                  TO  QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       973100-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *   Section da Faixa de 980000 a 989900 para Gravação de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------
           MOVE 0028                   TO  MSG1-CD-SEQL-ERRO

      *--> Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

      *--> Monta o nome do container de resposta
           MOVE 'SGTS0163-RPST'        TO  NM-CTNR
           SET PUT-CONTAINER           TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO  TRUE

           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                 )
                     FROM      ( SGTS0163-RPST           )
                     FLENGTH   ( LENGTH OF SGTS0163-RPST )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       980000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *-----------------------------------------------------------------
      *    Postar container com dados de requisição (entrada)
      *
      *                        NOME DO CONTAINER REQUISICAO
      *                        ----------------------------
      *
      *    Modulo CONTAINER  -     NOME_PGM-RQSC
      *    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
      *-----------------------------------------------------------------

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO  NM-CTNR
           SET PUT-CONTAINER           TO  TRUE

           MOVE NM-PGM        TO  NM-CTNR(1:8)
           MOVE '-RQSC'       TO  NM-CTNR(9:5)

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO  TRUE

      *    Faz put da requisicao do programa a executar
           EVALUATE NM-PGM
               WHEN 'SGTS0189'
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTS0189-RQSC    )
                              FLENGTH   (LENGTH OF SGTS0189-RQSC    )
                    END-EXEC

               WHEN 'SGTSL089'
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSL089-RQSC    )
                              FLENGTH   (LENGTH OF SGTSL089-RQSC    )
                    END-EXEC

               WHEN 'SGTSL092'
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSL092-RQSC    )
                              FLENGTH   (LENGTH OF SGTSL092-RQSC    )
                    END-EXEC

               WHEN 'SGTSLA06'
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSLA06-RQSC    )
                              FLENGTH   (LENGTH OF SGTSLA06-RQSC    )
                    END-EXEC

               WHEN 'SGTSPE10'
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSPE10-RQSC    )
                              FLENGTH   (LENGTH OF SGTSPE10-RQSC    )
                    END-EXEC

               WHEN 'SGTSPE13'
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSPE13-RQSC    )
                              FLENGTH   (LENGTH OF SGTSPE13-RQSC    )
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO  TO  TRUE
                    MOVE  9004             TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na gravação da requisição do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
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

           MOVE NM-PGM        TO  NM-CTNR(1:8)
           MOVE '-RPST'       TO  NM-CTNR(9:5)

           SET DEL-CONTAINER           TO  TRUE

      *    Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
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
                    SET ERRO-CONTAINER TO  TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *--> Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------

      *    Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE 'PRM-ERRO'             TO  NM-CTNR
           SET DEL-CONTAINER           TO  TRUE

      *    Não há captura automática de erro CICS por usar RESP e RESP2
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
                    SET ERRO-CONTAINER TO  TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *--> Grava o erro gerado no container de erro
      *-----------------------------------------------------------------

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS PUT
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
               MOVE 8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL SPACES
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
               WHEN ERRO-CONTAINER
                    PERFORM 999004-TRATA-ERRO-CTNR
               WHEN ERRO-TAMANHO-AREA
                    PERFORM 999005-TRATA-ERRO-AREA-DADOS
               WHEN ERRO-CANAL
                    PERFORM 999006-TRATA-ERRO-CANAL
               WHEN OTHER
                    CONTINUE
           END-EVALUATE

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE 'SGTS0163-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0163-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0163-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
           IF SGTS0163-CD-OPR-ETRD-MCP     NUMERIC
              MOVE  SGTS0163-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
              MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
              MOVE 'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0163-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE SGTS0163-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0163-QT-OCR-TAB-E'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0163-QT-OCR-TAB-E
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           PERFORM  VARYING  IDX-SEARCH
                    FROM  0001 BY 0001
                    UNTIL IDX-SEARCH     >  SGTS0163-QT-OCR-TAB-E
                    MOVE 'SGTS0163-CD-RGR-VLDC-E'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTS0163-CD-RGR-VLDC-E(IDX-SEARCH)
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTS0163-NR-VRS-RGR-VLDC-E'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTS0163-NR-VRS-RGR-VLDC-E(IDX-SEARCH)
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
           END-PERFORM

      *--> Monitorar variáveis das mensagens
           MOVE 'AUX-ATBC'              TO  MSGKW999-NM-VRV-MNT
           MOVE AUX-ATBC                TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'AUX-RGR-DISPLAY'      TO  MSGKW999-NM-VRV-MNT
           MOVE AUX-RGR-DISPLAY        TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'AUX-VRS-DISPLAY'      TO  MSGKW999-NM-VRV-MNT
           MOVE AUX-VRS-DISPLAY        TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'IDX-SEARCH-DISPLAY'   TO  MSGKW999-NM-VRV-MNT
           MOVE IDX-SEARCH-DISPLAY     TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'AUX-ITC-DISPLAY'      TO  MSGKW999-NM-VRV-MNT
           MOVE AUX-ITC-DISPLAY        TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

      *--> Encerra a transacao
           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      *----------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
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
      *---------------------- FIM SGTS0163 -----------------------------