       CBL ARITH(EXTEND)
      *----------------------------------------------------------------*
      * PROGRAMA..: SGTS001A
      * ANALISTA..: F2634012 - EDILENE AKEMI HORITA
      * AUTOR.....: F2634012 - EDILENE AKEMI HORITA
      * DATA......: 13/09/2018
      * OBJETIVO..: Enviar operaçoes da carteira diária TVM do sistema
      *             SGT para o sistema OPR.
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      *----------------------------------------------------------------*
      * VRS017 11/02/2025 F9158937- Envio da data de Posição ao módulo
      *                             CACS00IC
      * VRS016 24/12/2024 F9158937- Inclusão do acesso a Perda Esperada
      *                             e envio ao OPR
      * VRS015 18/12/2024 F9158937- Inclusão da tag ANC para carteira
      *                   400.000.
      * VRS014 04/12/2024 F2054109- Recompilação por alteração do book
      *                             CACK045V
      * VRS013 08/10/2024 F9158937- Resolução 4966: Alteração do domínio
      *                   do campo CD-EST-VNCT-SCTR.
      * VRS012 14/08/2024 F9158937-Alteração no envio do Estado Negocial
      *                   e passar a enviar o Motivo do Estado Negocial
      *                   para OPR.
      *                           -Mudar TAG que indica qual é o grupo
      *                   da Carteira (TVM, BI ou TESOU) para os campos
      *                   CD-PRF-DEPE-CDU e CD-UOR-DEPE-CDTR.
      * VRS011 02/08/2024 F9158937- Acerto chamada rotina que verifica
      *                   último cliente no OPR.
      * VRS010 25/06/2024 F9158937-  Novo envio da Taxonomia e envio
      *                   da Classificação Contábil ao OPR pelos progra-
      *                   mas OPRS033I e OPRS033U.
      * VRS009 14/06/2024 F9158937- Inibir consistência da taxa
      *                   CACS045V-VL-TAXA-JUR-EFT  com apontamento de
      *                   erro até estabilizar CAC/RiscWatch
      * VRS008 05/04/2024 F9158937- Incluir rotinas para envio da TJE -
      *                   Taxa de Juros Efetiva ao OPR.
      * VRS007 04/01/2024 F9158937- Criar procedimentos novos quando
      *                   tiver ocorrido a Reserva Número Unico OPR
      *                   - Substituir o pgm SGTST005 por SGTSX005 para
      *                   contemplar campos novos.
      * VRS006 28/12/2023 F9158937- Acerto no cálculo do campo
      *                   GDA-VL-EM-ATRASO
      * VRS005 08/03/2022 F9158937- Consolidação das informações  do
      *                   Emissor e  Impacto no Limite no campo
      *                   CD-PSS-CTR-OPR  e campo CD-CIA-RSP-CTR sempre
      *                   preenchido com zeros, conforme solicitação
      *                   Gestor na história 396848.
      * VRS004 23/02/2022 F9158937- Incluir envio de CD-EST-NEGL
      *                   "11-Comercialização BBBI" também para a
      *                   Carteira 337 (nova).
      * VRS003 07/07/2021 F9158937- Incluir envio de CD-EST-NEGL especí-
      *                   fíco para carteiras BI (330 e 333):
      *                   "11-Comercialização BBBI"
      * VRS002 26/03/2019 F2634012 - Incluir tratamento paras as opera-
      *                   ções encerradas.
      * VRS001 13/09/2018 F2645012  DE 2016/145195 - Implantação.
      *----------------------------------------------------------------*
      
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS001A.
      
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
      
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *--------------------Variaveis do Sistema------------------------*
       77  CTE-INICIO-SS          PIC  X(035)         VALUE
           '*** STORAGE SECTION COMECA AQUI ***'.
      
       01  CTE-PROG               PIC X(17)  VALUE '*** SGTS001A ***'.
       01  CTE-VERS               PIC X(06)  VALUE 'VRS017'.
      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  OPRS9401           PIC  X(008)         VALUE 'OPRS9401'.
           03  OPRS9502           PIC  X(008)         VALUE 'OPRS9502'.
           03  OPRS9503           PIC  X(008)         VALUE 'OPRS9503'.
           03  OPRS9504           PIC  X(008)         VALUE 'OPRS9504'.
           03  OPRS9524           PIC  X(008)         VALUE 'OPRS9524'.
           03  OPRS9804           PIC  X(008)         VALUE 'OPRS9804'.
      
           03  OPRS9601           PIC  X(008)         VALUE 'OPRS9601'.
           03  OPRS9602           PIC  X(008)         VALUE 'OPRS9602'.
           03  OPRS9603           PIC  X(008)         VALUE 'OPRS9603'.
           03  OPRS9604           PIC  X(008)         VALUE 'OPRS9604'.
           03  OPRS9624           PIC  X(008)         VALUE 'OPRS9624'.
           03  OPRS9904           PIC  X(008)         VALUE 'OPRS9904'.

           03  OPRS033I           PIC  X(008)         VALUE 'OPRS033I'.
           03  OPRS033U           PIC  X(008)         VALUE 'OPRS033U'.

           03  OPRS9532           PIC  X(008)         VALUE 'OPRS9532'.
           03  OPRS9632           PIC  X(008)         VALUE 'OPRS9632'.

           03  OPRS0214           PIC  X(008)         VALUE 'OPRS0214'.
      
           03  MSTS8042           PIC  X(008)         VALUE 'MSTS8042'.
           03  CACSV045           PIC  X(008)         VALUE 'CACSV045'.
           03  CACS00IC           PIC  X(008)         VALUE 'CACS00IC'.
           03  IIBP2000           PIC  X(008)         VALUE 'IIBP2000'.
           03  SBTIMEDT           PIC  X(008)         VALUE 'SBTIMEDT'.
      
           03  SGTST004           PIC  X(008)         VALUE 'SGTST004'.
           03  SGTSX005           PIC  X(008)         VALUE 'SGTSX005'.
           03  SGTST009           PIC  X(008)         VALUE 'SGTST009'.
           03  SGTST011           PIC  X(008)         VALUE 'SGTST011'.
           03  SGTST137           PIC  X(008)         VALUE 'SGTST137'.
           03  SGTSLJ11           PIC  X(008)         VALUE 'SGTSLJ11'.
           03  SGTSLJ12           PIC  X(008)         VALUE 'SGTSLJ12'.
           03  SGTSPE22           PIC  X(008)         VALUE 'SGTSPE22'.
           03  SGTSPE25           PIC  X(008)         VALUE 'SGTSPE25'.
      
      
       77  PREFIXADO              PIC  S9(04) COMP-5  VALUE 1313.
       77  TIPO-EMISSOR           PIC  S9(04) COMP-5  VALUE 1.
       77  TIPO-IMPACTO-LC        PIC  S9(04) COMP-5  VALUE 2.
       77  SEM-TRAVA              PIC  S9(04) COMP-5  VALUE 99.
       77  EFETIVA                PIC  S9(04) COMP-5  VALUE 1.
       77  AO-DIA                 PIC  S9(04) COMP-5  VALUE 3.
       77  AO-ANO                 PIC  S9(04) COMP-5  VALUE 1.
       77  SEM-INCIDENCIA-10      PIC  S9(04) COMP-5  VALUE 10.
       77  SEM-INCIDENCIA-4       PIC  S9(04) COMP-5  VALUE 4.
       77  PORCENTAGEM            PIC  S9(04) COMP-5  VALUE 1.
       77  TAXA-FIXA              PIC  S9(04) COMP-5  VALUE 1.
       77  TITULOS                PIC  S9(04) COMP-5  VALUE 30.
       77  NEGOCIACAO-TITULO      PIC  S9(04) COMP-5  VALUE 72.
       77  AMORTIZACAO            PIC  S9(04) COMP-5  VALUE 3.
       77  PAGTO-ATRASO           PIC  S9(04) COMP-5  VALUE 11.
       77  K-ZERO                 PIC  S9(04) COMP-5  VALUE 0.
       77  EST-PRCT-SUCESSO       PIC  S9(04) COMP-5  VALUE 2.
       77  EST-ERRO-PRCT          PIC  S9(04) COMP-5  VALUE 3.
       77  EST-ENV-ITEM-MAIS-RECENTE    PIC  S9(04) COMP-5  VALUE 4.

      *--> Definição dos Variaveis do Cobolito
       COPY ALMKCBL1.
      

      *----------------------------------------------------------------*
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
       COPY HLPKE01B.
       LOCAL-STORAGE SECTION.
       01  IC-TAB                      PIC S9(04) COMP-5 VALUE 0.
       01  IC-ERRO                     PIC S9(04) COMP-5 VALUE 0.
       01  CH-ROLLBACK                 PIC X(01)  VALUE 'S'.
      
       01  IN-PROCESSAMENTO            PIC  9(01) VALUE 0.
           88 ERRO-PROCESSAMENTO                  VALUE 1.
      *
       01  IN-PSC-SDO                  PIC  9(01) VALUE 0.
           88 EXISTE-SALDO-MAIS-RECENTE           VALUE 1.
      *
       01  IN-OPR-ENV                  PIC  9(01) VALUE 0.
           88 OPR-INCLUSAO                        VALUE 1.
      *
       01  IN-OPR-ATRASADA             PIC  9(01) VALUE 0.
           88 OPR-ATRASADA                        VALUE 1.

       01  IN-OPR-EFE                  PIC  9(01) VALUE 0.
           88 OPR-EFETIVA                         VALUE 1.

      *    Chamada à subrotina SBDATA
      *
       01  SBDATAF                     PIC  X(08) VALUE 'SBDATAF'.
       01  FUNCAO                      PIC  X(03) VALUE 'F03'.
       01  AREA-WORK                   PIC  X(4095).
       01  AREA-WORK-RED  REDEFINES  AREA-WORK.
           03 A-TAM                      PIC  9(0008) COMP.
           03 FILLER                     PIC  X(4071).
           03 A-FIM                      PIC  X(0020).
       01  ARG01                       PIC  9(08).
       01  ARG01-R  REDEFINES  ARG01.
         03  DIA                       PIC  9(02).
         03  MES                       PIC  9(02).
         03  ANO                       PIC  9(04).
       01  ARG02                       PIC  9(08).
       01  ARG02-R  REDEFINES  ARG02.
         03  DIA                       PIC  9(02).
         03  MES                       PIC  9(02).
         03  ANO                       PIC  9(04).
       01  ARG03                       PIC S9(05) VALUE ZEROS.
       01  ARG03-ABS                   PIC  9(05) VALUE ZEROS.

      * Area de subrotina  SBTIMEDT

       01 DADOS-SBTIMEDT.
          03  CD-FUC              PIC X(03)      VALUE 'F01'.
          03 AREA-TRB.
             05 AREA-TRB-TAM      PIC S9(9) COMP VALUE 1024.
             05 AREA-TRB-DADOS    PIC  X(1024)   VALUE ' '.
          03 FMT-TS               PIC S9(4) COMP VALUE 1.
          03 TAM-TS               PIC S9(4) COMP VALUE 30.
          03 TS-FMTD.
              05  ANO                      PIC  9(4).
              05  FILLER                   PIC  X(1).
              05  MES                      PIC  9(2).
              05  FILLER                   PIC  X(1).
              05  DIA                      PIC  9(2).
              05  FILLER                   PIC  X(1).
              05  HORA                     PIC  9(2).
              05  FILLER                   PIC  X(1).
              05  MINUTO                   PIC  9(2).
              05  FILLER                   PIC  X(1).
              05  SEGUNDO                  PIC  9(2).
              05  FILLER                   PIC  X(1).
              05  MICRO-SEGUNDO            PIC  9(6).
              05  FILLER                   PIC  X(4).

      *
      * Constantes de parametros
       01  N-INI                 PIC S9(1) COMP-5 VALUE 1.
       01  N-FIM-NOVE            PIC S9(9) COMP-5 VALUE 999999999.
       01  PC-TAXA-AUX           PIC S9(3)V9(8) COMP-3  VALUE
                                                        999,99999999.
      *
       01  CD-RTN-OPR            PIC  9(09)  VALUE 0.
       01  MSG-RTN-OPR           PIC  X(60).
       01  CD-ERR-OPR            PIC  9(04)  VALUE 0.
       01  LST-ERR-OPR           PIC  X(180).
       01  MSG-REGISTRO-ERRO     PIC  X(256).
      *
       01  DADOS-ERRO-IIB.
           03  CD-ERRO-IIB             PIC  9(9)   VALUE 0.
           03  CD-ERRO-IIB-R           REDEFINES   CD-ERRO-IIB.
               05  TIP-RTN-PGM-IIB     PIC  9(1).
               05  FILLER              PIC  9(1).
               05  CD-RTN-PGM-IIB      PIC  9(4).
               05  PSC-ERRO-PGM-IIB    PIC  9(3).
           03  TX-ERRO-IIB             PIC  X(120) VALUE ' '.
      *
       01  SQLCODE-8002                PIC S9(005).
       01  CD-RTN-CICS.
           05  CD-RTN-RESP             PIC  9(08)  VALUE 0.
           05  CD-RTN-RESP2            PIC  9(08)  VALUE 0.
      
       01  CT-OPR-SUCESSO            PIC S9(9) COMP-5 VALUE 0.
       01  CT-OPR-ENVIADA            PIC S9(9) COMP-5 VALUE 0.

       01  GDA-TIMESTAMP             PIC X(26) VALUE
                                     '    -  -  -  .  .  .'.
       01  GDA-TIMESTAMP-R  REDEFINES GDA-TIMESTAMP.
           03 ANO                    PIC 9(04).
           03 FILLER                 PIC X(01).
           03 MES                    PIC 9(02).
           03 FILLER                 PIC X(01).
           03 DIA                    PIC 9(02).
           03 FILLER                 PIC X(01).
           03 HORA                   PIC  9(02).
           03 FILLER                 PIC  X(01).
           03 MINUTO                 PIC  9(02).
           03 FILLER                 PIC  X(01).
           03 SEGUNDO                PIC  9(02).
           03 FILLER                 PIC  X(01).
           03 MICRO-SEGUNDO          PIC  9(06).

       01  GDA-CLI-EMISSOR           PIC S9(9) COMP-5 VALUE 0.
       01  GDA-ULT-CLI-EMISSOR       PIC S9(9) COMP-5 VALUE 0.
       01  GDA-CLI-IMPACTO-LC        PIC S9(9) COMP-5 VALUE 0.
       01  GDA-NR-UNCO-CTR-OPR       PIC S9(017) COMP-3 VALUE 0.
       01  GDA-VL-SDO-JUR            PIC S9(15)V9(2) COMP-3 VALUE 0.
       01  GDA-VL-SDO-CPTL           PIC S9(15)V9(2) COMP-3 VALUE 0.
       01  GDA-VL-EM-ATRASO          PIC S9(15)V9(2) COMP-3 VALUE 0.
       01  GDA-VL-CMP-CARECA         PIC S9(15)V9(2) COMP-3 VALUE 0.
       01  GDA-VL-SDO-TOL            PIC S9(15)V9(2) COMP-3 VALUE 0.
       01  GDA-CD-EST-VNCT-SCTR      PIC S9(04)      COMP-3 VALUE 0.
       01  GDA-FATOR-TOLERANCIA      PIC S9(4)V9(4)  COMP-3 VALUE 0.
       01  GDA-DT-PRMO-CRNG          PIC  X(10) VALUE '  .  .    '.
       01  GDA-DT-PRMO-CRNG-R   REDEFINES   GDA-DT-PRMO-CRNG.
           07 DIA                       PIC  9(2).
           07 FILLER                    PIC  X(1).
           07 MES                       PIC  9(2).
           07 FILLER                    PIC  X(1).
           07 ANO                       PIC  9(4).
       01  GDA-DT-EST-CDU               PIC  X(10) VALUE '  .  .    '.
       01  GDA-DT-EST-CDU-R  REDEFINES  GDA-DT-EST-CDU.
           07 DIA                       PIC  9(2).
           07 FILLER                    PIC  X(1).
           07 MES                       PIC  9(2).
           07 FILLER                    PIC  X(1).
           07 ANO                       PIC  9(4).
      
       01  GDA-DEP-OPR                  PIC S9(004) COMP-3 VALUE 0.
      
       01  GDA-EST-SGT                  PIC S9(9) COMP-5 VALUE 0.
           88 EST-SGT-ENCARTEIRADA               VALUE 5.
           88 EST-SGT-ENCERRADA                  VALUE 6.
       01  GDA-EST-OPR                  PIC S9(9) COMP-5 VALUE 0.
           88 EST-OPR-ATIVA                      VALUE 1.
           88 EST-OPR-ENCERRADA                  VALUE 2.
           88 EST-OPR-DEFERIDO-N-FRMO            VALUE 10.

       01  GDA-CD-EST-NEGL              PIC  S9(04) COMP-5  VALUE 0.
       01  GDA-CD-MTV-EST-NEGL          PIC  S9(04) COMP-5  VALUE 0.

       01  DT-DB2                       PIC  X(10) VALUE '    -  -  '.
       01  DT-DB2-R REDEFINES  DT-DB2.
           07 ANO                       PIC  9(4).
           07 FILLER                    PIC  X(1).
           07 MES                       PIC  9(2).
           07 FILLER                    PIC  X(1).
           07 DIA                       PIC  9(2).
      
       01  DT-PSC                      PIC  X(10) VALUE '  .  .    '.
       01  DT-PSC-R REDEFINES  DT-PSC.
           07 DIA                       PIC  9(2).
           07 FILLER                    PIC  X(1).
           07 MES                       PIC  9(2).
           07 FILLER                    PIC  X(1).
           07 ANO                       PIC  9(4).
      
       01  DT-PSC-INVERTIDA            PIC  9(08).
       01  DT-PSC-INVERTIDA-R  REDEFINES DT-PSC-INVERTIDA.
           07 ANO                       PIC  9(4).
           07 MES                       PIC  9(2).
           07 DIA                       PIC  9(2).
      
       01  DT-PRMO-CRNG-INVERTIDA      PIC  9(08).
       01  DT-PRMO-CRNG-INVERTIDA-R  REDEFINES DT-PRMO-CRNG-INVERTIDA.
           07 ANO                       PIC  9(4).
           07 MES                       PIC  9(2).
           07 DIA                       PIC  9(2).
      
       01  DT-OPR                      PIC  9(08).
       01  DT-OPR-R      REDEFINES DT-OPR.
           07 ANO                       PIC  9(4).
           07 MES                       PIC  9(2).
           07 DIA                       PIC  9(2).

       01  DT-CAC                      PIC  X(10) VALUE '  .  .    '.
       01  DT-CAC-R      REDEFINES DT-CAC.
           07 DIA                       PIC  9(2).
           07 FILLER                    PIC  X(1).
           07 MES                       PIC  9(2).
           07 FILLER                    PIC  X(1).
           07 ANO                       PIC  9(4).
      
       01  DT-AAAAMMDD                 PIC  9(08).
       01  DT-AAAAMMDD-R REDEFINES DT-AAAAMMDD.
           07 ANO                       PIC  9(4).
           07 MES                       PIC  9(2).
           07 DIA                       PIC  9(2).
       01  NM-PGM-ERRO                  PIC  X(8).

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      ---------------9.
       01  MCR-S9-17                   PIC      -----------------9.
       01  VL-15V2                     PIC  ---.---.---.---.--9,99.
       01  VL-4V10                     PIC  ---9,9999999999.
       01  VL-9V8                      PIC  ---.---.--9,99999999.
       01  VL-5V6                      PIC  -.---9,999999.
       01  VL-15V14
                               PIC  ---.---.---.---.--9,99999999999999.


           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      
      *-------------------------- B O O K S ---------------------------*
      
      *-----> Book do container de Erro (IIB)
       COPY IIBKERRO.
      
      *-----> INCLUSAO CONTRATAO DE OPERACAO E PESSOA NO OPR
       01  OPRS9401-DADOS.
       COPY OPRK9401.
      
      *-----> INCLUSAO EMPREENDIMENTO FINANCIADO NO OPR
       01  OPRS9502-DADOS.
       COPY OPRK9502.
      
      *-----> INCLUSAO SUB CONTRATO DA OPERACAO NO OPR
       01  OPRS9503-DADOS.
       COPY OPRK9503.
      
      *-----> INCLUSAO DE CLIENTES VINCULADOS NA PARTICIPANTE DA
      *       PESSOA CONTRATO - DB2OPR.PRTC_PSS_CTR_OPR
       01  OPRS9504-DADOS.
       COPY OPRK9504.
      *-----> INCLUSAO SALDO DE SUBCONTRATO DA OPERACAO
       01  OPRS9524-DADOS.
       COPY OPRK9524.
      *-----> INCLUSAO SALDO DE SUBCONTRATO DA OPERACAO
      *-----> DE APLICAÇÃO E TESOURARIA
       01  OPRS9804-DADOS.
       COPY OPRK9804.
      
      *-----> ALTERACAO CONTRATAO DE OPERACAO NO OPR
       01  OPRS9601-DADOS.
       COPY OPRK9601.
      
      *-----> ALTERACAO EMPREENDIMENTO FINANCIADO NO OPR
       01  OPRS9602-DADOS.
       COPY OPRK9602.
      
      *-----> ALTERACAO SUB CONTRATO DA OPERACAO NO OPR
       01  OPRS9603-DADOS.
       COPY OPRK9603.                                                           0000
      
      *-----> ALTERACAO PARTICIPACAO DE PESSOA NA OPERACO
       01  OPRS9604-DADOS.
       COPY OPRK9604.                                                           0000
      
      *-----> ALTERACAO SALDO DE SUBCONTRATO DA OPERACAO
       01  OPRS9624-DADOS.
       COPY OPRK9624.

      *-----> ALTERACAO SALDO DE SUBCONTRATO DA OPERACAO
      *-----> DE APLICAÇÃO E TESOURARIA
       01  OPRS9904-DADOS.
       COPY OPRK9904.

      *----->  OPR  - INCLUSAO NA TABELA TAXA-FNCR_SCTR  do OPR
       01  OPRS9532-DADOS.
       COPY OPRK9532.

      *----->  OPR  - ALTERAÇÃO NA TABELA TAXA-FNCR_SCTR  do OPR
       01  OPRS9632-DADOS.
       COPY OPRK9632.

      *----->  OPR  - INCLUSAO NA TABELA DB2OPR.TXNM_OPR
       01  OPRS033I-DADOS.
       COPY OPRK033I.

      *----->  OPR  - ALTERAÇÃO NA TABELA DB2OPR.TXNM_OPR
       01  OPRS033U-DADOS.
       COPY OPRK033U.

      *----->  OPR  - INCLUSÃO NA TABELA DB2OPR.EST_NEGL_SCTR_OPR
       01  OPRS0214-DADOS.
       COPY OPRKS214.

      *-----> CACSV045 -  Retornar  Valor  Taxa de Juros Efetiva
       01  CACSV045-DADOS.
       COPY CACK045V.

      *-----> CACS00IC  -  Consulta Indicador Ativo Problematico
       01  CACS00IC-DADOS.
       COPY CACK00IC.

      *-----> BOOK DA SUBROTINA SGTSLJ11
       01  SGTSLJ11-DADOS.
       COPY SGTKLJ11.                                                           0000
      
      *-----> BOOK DA SUBROTINA SGTSLJ12
       01  SGTSLJ12-DADOS.
       COPY SGTKLJ12.                                                           0000
0
      *-----> BOOK DA SUBROTINA SGTSPE22
       01  SGTSPE22-DADOS.
       COPY SGTKPE22.

      *-----> BOOK DA SUBROTINA SGTSPE25
       01  SGTSPE25-DADOS.
       COPY SGTKPE25.

      *-----> BUSCAR PREFIXO DA UOR VIA IIB
       COPY MSTK8042.                                                           0000
      
      *-----> PERSISTENCIA DA TABELA  PSC_DRIA_SDO_OPR
       01  SGTST004-DADOS.
       COPY SGTKT004.                                                           0000
      
      *-----> PERSISTENCIA DA TABELA  OPR_ETRD_MCP - BOOK NOVO
       01  SGTSX005-DADOS.
       COPY SGTKX005.                                                           0000
      
      *-----> PERSISTENCIA DA TABELA  RMNC_OPR_ETRD
       01  SGTST011-DADOS.
       COPY SGTKT011.

      *-----> PERSISTENCIA DA TABELA   PDA_EPRO_OPR
       01  SGTST137-DADOS.
       COPY SGTKT137.

      *-----> PERSISTENCIA DA TABELA  EST_OPR_ETRD
       01  SGTST009-DADOS.
       COPY SGTKT009.                                                           0000
      
      *-----> BOOK DA SUBROTINA SGTS001A
       01  SGTS001A-DADOS.
       COPY SGTK001A.                                                           0000
      
      * Books da definicao de dados a serem usados na subrotina de erros
      *-INC HLPKE001
       COPY MSGKWMG1.
       COPY HLPKW999.
      
      * Book dos dados do usuário
       COPY IIBKOPRD.
      *--> Book do container de Serviços (para acionamento de serviços)
       COPY IIBKSRVC.
      * Book das variáveis auxiliares do tratamento de erro via MSG
       COPY SGTKERR1.

      *------------------- Variaveis do Sistema -----------------------*
       01  CTE-SAI-SS             PIC  X(040)         VALUE
           '*** STORAGE SECTION TERMINA AQUI ***'.
      
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
       COPY ALMKCBL2.

      * Habilita captura automatica de abend e erros CICS
       COPY HLPKE999.

      * 000000-PRINCIPAL                SECTION.
           MOVE 0001                             TO MSG1-CD-SEQL-ERRO
           INITIALIZE CD-ERRO-IIB
           MOVE 4095                   TO A-TAM
           MOVE 'SBDATAF -WRKAREA-FIM' TO A-FIM

           PERFORM 100000-DESCOBRIR-CANAL
           MOVE 'SGTS001A'             TO NM-PGM
           PERFORM 120000-BUSCAR-REQUISICAO-IIB
           PERFORM 200000-BUSCAR-MATRICULA-IIB
           PERFORM 970124-BUSCA-TS-DT
           MOVE CORR TS-FMTD           TO GDA-TIMESTAMP-R
           PERFORM 300000-PROCESSAR-NEGOCIO
           PERFORM 999999-ENCERRAR
           GOBACK
           .
      * 000000-SAI.
      *     EXIT.
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto      o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE

           .
       100000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       120000-BUSCAR-REQUISICAO-IIB    SECTION.
      *----------------------------------------------------------------*
      *    Obtém os parâmetros de requisição ou dados de entrada       *
      *----------------------------------------------------------------*
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
              CONTAINER ( 'SGTS001A-RQSC' )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

           EXEC CICS GET
                CONTAINER ( 'SGTS001A-RQSC'      )
                FLENGTH   ( QT-CCT-LIDO          )
                INTO      ( SGTS001A-RQSC        )
           END-EXEC
      * --> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE

           .
       120000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       200000-BUSCAR-MATRICULA-IIB     SECTION.
      *----------------------------------------------------------------*
      *     Obtém a matrícula do usuário logado no IIB                 *
      *----------------------------------------------------------------*
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO
      
           SET  GET-CONTAINER          TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto erro de container

           SET  ERRO-CONTAINER         TO TRUE
           INITIALIZE                  PRM-INF-OPRD

           EXEC CICS GET
                CONTAINER ( 'PRM-INF-OPRD'          )
                INTO      ( PRM-INF-OPRD            )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD  )
           END-EXEC
      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
      
           .
       200000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       300000-PROCESSAR-NEGOCIO        SECTION.
      *----------------------------------------------------------------*
           MOVE 0004                             TO MSG1-CD-SEQL-ERRO
      
           PERFORM 310000-VALIDAR-OPERACAO
                   VARYING IC-TAB FROM 1 BY 1
                          UNTIL IC-TAB > SGTS001A-QT-OCR-TAB
           MOVE ZEROS TO IC-TAB
           PERFORM 320000-TRATAR-ENVIO-OPR
                    VARYING IC-TAB FROM 1 BY 1
                           UNTIL IC-TAB > SGTS001A-QT-OCR-TAB
      * Somete para os casos de erro abaixo a chave ch-rollback será
      * igual a 'n'. Quando não houver o não envio de todas ou parcial
      * das operações ao OPR não deverá ser dado rollback na transação
      * pq está ocorrendo a não efetivação do último registro da posi-
      * ção de saldo nessa situação. Para as demais situações o rollba-
      * ck é devido.
           IF CT-OPR-SUCESSO = ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 'Erro no envio de todas as operaçoes ao OPR.'
                                       TO MSG1-TX-ERRO-TS
               MOVE 0829               TO MSG1-CD-ERRO-TS
               MOVE 'N'                TO CH-ROLLBACK
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
              ADD CT-OPR-ENVIADA  TO  CT-OPR-SUCESSO
              IF CT-OPR-SUCESSO   NOT EQUAL SGTS001A-QT-OCR-TAB
                  SET ERRO-PROGRAMADO     TO TRUE
                  MOVE 'Erro no envio de algumas operaçoes ao OPR.'
                                         TO MSG1-TX-ERRO-TS
                  MOVE 0830              TO MSG1-CD-ERRO-TS
                  MOVE 'N'               TO CH-ROLLBACK
                  SET MNT-VRV            TO TRUE
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-IF
           END-IF
           .
       300000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       310000-VALIDAR-OPERACAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0005                             TO MSG1-CD-SEQL-ERRO

           SET N-MNT-VRV               TO  TRUE
           IF  SGTS001A-CD-CTRA-TIT-VL(IC-TAB) EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0002               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'   TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF SGTS001A-NR-VRS-OPR-ETRD(IC-TAB) EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0007               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
      -                                TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      
           IF  SGTS001A-CD-OPR-ETRD-MCP(IC-TAB) EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0005               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais nã
      -             'o informado.'     TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
      
           IF SGTS001A-DT-PSC-DRV(IC-TAB) EQUAL SPACES
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0735               TO MSG1-CD-ERRO-TS
               MOVE 'Data da posição da drive inválida.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS001A-NR-SEQL-PSC-DRIA(IC-TAB) EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0730               TO MSG1-CD-ERRO-TS
               MOVE 'Número sequencial da posição diária inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       310000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       320000-TRATAR-ENVIO-OPR         SECTION.
      *----------------------------------------------------------------*
           MOVE 0006                        TO MSG1-CD-SEQL-ERRO
      
           PERFORM 321000-CONSULTA-DADOS-OPERACAO
           PERFORM 321500-CONSULTA-PERDA-ESPERADA
           MOVE  ZEROS      TO       IN-OPR-ENV
           MOVE  ZEROS      TO       IN-OPR-EFE
           IF  SGTSX005-NR-UNCO-CTR-OPR  NOT GREATER  ZEROS
      *------  Numero Único OPR   NÃO preenchido
               SET OPR-INCLUSAO     TO    TRUE
           ELSE
      *------  Numero Único OPR   PREENCHIDO
               EVALUATE SGTSX005-IN-REG-OPR-BB
               WHEN  'R'
      *--------  Numero Único OPR Reservado
                 SET OPR-EFETIVA     TO    TRUE
               WHEN  ' '
               WHEN  'E'
      *--------  Numero Único OPR Efetivado
                 CONTINUE
               END-EVALUATE
           END-IF
           PERFORM 323000-CONSULTA-PSC-DRIA-SDO
           IF SGTST004-CD-EST-RSTD-ITE EQUAL EST-PRCT-SUCESSO OR
              EST-ENV-ITEM-MAIS-RECENTE
              ADD  1    TO   CT-OPR-ENVIADA ON SIZE ERROR
                   SET MNT-VRV             TO TRUE
                   SET ERRO-PROGRAMADO     TO TRUE
                   MOVE 0706                TO MSG1-CD-ERRO-TS
                   MOVE 'Erro em operação aritmética.'
                                            TO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-ADD
           ELSE
      
              PERFORM 322000-CONSULTA-PRTC-CLI
              PERFORM 327000-TRATAR-REG-PRE-EXISTENT
      
              PERFORM 323000-CONSULTA-PSC-DRIA-SDO
      
              IF NOT ERRO-PROCESSAMENTO
                 PERFORM 326000-CONSULTA-EST-OPR
                 IF EST-SGT-ENCERRADA
                    INITIALIZE  GDA-VL-SDO-TOL GDA-VL-SDO-CPTL
                                GDA-VL-EM-ATRASO GDA-VL-SDO-JUR
                                GDA-VL-CMP-CARECA
                    MOVE '  .  .    '  TO    GDA-DT-PRMO-CRNG
                    MOVE 24            TO    GDA-CD-EST-VNCT-SCTR
                 ELSE
                     PERFORM 325000-APURAR-VALORES
                     PERFORM 328000-CALCULAR-EST-VNCT-SCTR
                 END-IF
                 PERFORM 329100-FMT-DEP-OPR
                 PERFORM 329200-OBTER-TAXA-REMUNERACAO
      
                 MOVE '*'           TO  LST-ERR-OPR

      *----------****   INCLUSÃO   ****--------------------------------*
                 IF OPR-INCLUSAO
      
                    IF NOT ERRO-PROCESSAMENTO
                       PERFORM   960001-INCLUIR-CTR-OPR
                    END-IF
      
                    IF NOT ERRO-PROCESSAMENTO
                       PERFORM   960003-INCLUIR-EPRD-FNCD
                    END-IF
      
                    IF NOT ERRO-PROCESSAMENTO
                       PERFORM   960004-INCLUIR-SUB-CTR-OPR
                    END-IF
      
                    IF NOT ERRO-PROCESSAMENTO
                       PERFORM   960005-INCLUIR-SDO-SUB-CTR-OPR
                    END-IF
                    IF NOT ERRO-PROCESSAMENTO
                       PERFORM   960011-INCLUIR-SDO-CTR-APL-TSR
                    END-IF

                 ELSE
      *----------****   EFETIVAÇÃO   ****------------------------------*
                   IF  OPR-EFETIVA
                      MOVE SGTSX005-NR-UNCO-CTR-OPR    TO
                                             GDA-NR-UNCO-CTR-OPR

                      IF NOT ERRO-PROCESSAMENTO
                         PERFORM   960006-ALTERAR-CTR-OPR
                      END-IF

                      IF NOT ERRO-PROCESSAMENTO
                         PERFORM 960013-INCLUIR-PRTC-PSS
                      END-IF

                      IF NOT ERRO-PROCESSAMENTO
                         PERFORM   960003-INCLUIR-EPRD-FNCD
                      END-IF

                      IF NOT ERRO-PROCESSAMENTO
                         PERFORM   960004-INCLUIR-SUB-CTR-OPR
                      END-IF

                      IF NOT ERRO-PROCESSAMENTO
                         PERFORM   960005-INCLUIR-SDO-SUB-CTR-OPR
                      END-IF
                      IF NOT ERRO-PROCESSAMENTO
                         PERFORM   960011-INCLUIR-SDO-CTR-APL-TSR
                      END-IF

                   ELSE
      *----------****   ALTERAÇÃO   ****-------------------------------*
                    MOVE SGTSX005-NR-UNCO-CTR-OPR    TO
                                           GDA-NR-UNCO-CTR-OPR
      
                    IF NOT ERRO-PROCESSAMENTO
                       PERFORM   960012-ALTERAR-SDO-CTR-APL-TSR
                    END-IF
      
                    IF NOT ERRO-PROCESSAMENTO
                       PERFORM   960010-ALTERAR-SDO-SUB-CTR-OPR
                    END-IF
      
                    IF NOT ERRO-PROCESSAMENTO
                       PERFORM   960009-ALTERAR-SUB-CTR-OPR
                    END-IF
      
                    IF NOT ERRO-PROCESSAMENTO
                       PERFORM   960008-ALTERAR-EPRD-FNCD
                    END-IF

                    IF NOT ERRO-PROCESSAMENTO
                       PERFORM 335000-BUSCAR-ULT-CLI-EMS
                       PERFORM 960007-ALTERAR-PRTC-PSS-CTR
                    END-IF

                    IF NOT ERRO-PROCESSAMENTO
                       PERFORM   960006-ALTERAR-CTR-OPR
                    END-IF

                   END-IF
                 END-IF
              END-IF

      *====== Trata TJE - Taxa de Juros Efetiva
              IF NOT ERRO-PROCESSAMENTO
                 PERFORM   340000-ACESSA-CAC
                 IF  CACS045V-CD-RTN-PGM         EQUAL   ZEROS   AND
                     CACS045V-VL-TAXA-JUR-EFT  NOT EQUAL ZEROS   AND
                     CACS045V-VL-TAXA-JUR-EFT  NOT GREATER  PC-TAXA-AUX
                     PERFORM   960015-ALTERAR-TJE
                     IF K9632-COD-RETORNO  EQUAL 15  OR
                        K9632-COD-RETORNO  EQUAL 100
                        PERFORM   960014-INCLUIR-TJE
                     END-IF
                 END-IF
              END-IF

      *====== Trata Perda Esperada
              IF NOT ERRO-PROCESSAMENTO
                 PERFORM   960021-ALTERAR-PE
                 IF K9632-COD-RETORNO  EQUAL 15  OR
                    K9632-COD-RETORNO  EQUAL 100
                    PERFORM   960020-INCLUIR-PE
                 END-IF
              END-IF

      *====== Trata Taxonomia e Classificação Contábil
              IF NOT ERRO-PROCESSAMENTO
                 PERFORM   960017-ALTERAR-TAXO
                 IF OPRS033U-CD-RTN-PGM EQUAL 100
                    PERFORM   960016-INCLUIR-TAXO
                 END-IF
              END-IF

      *====== Trata Estado Negocial
              IF NOT ERRO-PROCESSAMENTO
                 PERFORM  960019-TRATA-EST-NEGL
              END-IF

              IF ERRO-PROCESSAMENTO
                 PERFORM 330000-DESFAZER-GRAVACAO
      *           PERFORM 323000-CONSULTA-PSC-DRIA-SDO
                 PERFORM 329000-ATLZ-ERRO-PSC-DRIA-SDO
              ELSE
      *           PERFORM 323000-CONSULTA-PSC-DRIA-SDO
                 PERFORM 329300-ATLZ-PRCT-SUCESSO
                 PERFORM 321000-CONSULTA-DADOS-OPERACAO
                 PERFORM 329400-ATLZ-OPR-SGT-SUCESSO
                 PERFORM 329500-TRATAR-NR-UNCO-CTR-OPR
                 ADD 1    TO  CT-OPR-SUCESSO ON SIZE ERROR
                   MOVE 0706                TO MSG1-CD-ERRO-TS
                   MOVE 'Erro em operação aritmética.'
                                            TO MSG1-TX-ERRO-TS
                   SET MNT-VRV              TO TRUE
                   SET ERRO-PROGRAMADO      TO TRUE
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
                 END-ADD
              END-IF
              PERFORM  331000-EFETIVAR-GRAVACAO
              MOVE  ZEROS    TO   IN-PROCESSAMENTO
           END-IF
      
           .
       320000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       321000-CONSULTA-DADOS-OPERACAO  SECTION.
      *----------------------------------------------------------------*
           MOVE 0007                        TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE         SGTSX005-DADOS
                              REPLACING NUMERIC BY  ZEROS
      
           SET    SGTSX005-CONSULTAR     TO  TRUE
           MOVE   SGTS001A-CD-OPR-ETRD-MCP(IC-TAB)
                                         TO  SGTSX005-CD-OPR-ETRD-MCP
           MOVE   SGTS001A-NR-VRS-OPR-ETRD(IC-TAB)
                                         TO  SGTSX005-NR-VRS-OPR-ETRD
           MOVE   SGTS001A-CD-CTRA-TIT-VL(IC-TAB)
                                         TO  SGTSX005-CD-CTRA-TIT-VL
      
           PERFORM   970113-CHAMA-SGTSX005
           .
       321000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       321500-CONSULTA-PERDA-ESPERADA  SECTION.
      *----------------------------------------------------------------*
           MOVE         0080             TO MSG1-CD-SEQL-ERRO

           INITIALIZE         SGTST137-DADOS
                              REPLACING NUMERIC BY  ZEROS
           MOVE           7              TO        SGTST137-ACAO
           MOVE   SGTS001A-CD-OPR-ETRD-MCP(IC-TAB)
                                         TO  SGTST137-CD-OPR-ETRD-MCP
           MOVE   SGTS001A-NR-VRS-OPR-ETRD(IC-TAB)
                                         TO  SGTST137-NR-VRS-OPR-ETRD
           MOVE   SGTS001A-CD-CTRA-TIT-VL(IC-TAB)
                                         TO  SGTST137-CD-CTRA-TIT-VL
           MOVE   '..........'           TO  SGTST137-DT-PRV
           MOVE   CORR SGTS001A-DT-PSC-DRV-R(IC-TAB)
                                         TO  SGTST137-DT-PRV-R

           PERFORM   970129-CHAMA-SGTST137
           .
       321500-SAI.
           EXIT.
      *----------------------------------------------------------------*
       322000-CONSULTA-PRTC-CLI        SECTION.
      *----------------------------------------------------------------*
           MOVE 0008                        TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE         SGTSLJ11-DADOS
                              REPLACING NUMERIC BY  ZEROS
      
           SET    SGTSLJ11-CONS-CLI-TP-PRTC TO  TRUE
           MOVE   SGTS001A-CD-CTRA-TIT-VL(IC-TAB)
                                         TO  SGTSLJ11-CD-CTRA-TIT-VL
           MOVE   SGTS001A-CD-OPR-ETRD-MCP(IC-TAB)
                                         TO  SGTSLJ11-CD-OPR-ETRD-MCP
           MOVE   SGTS001A-NR-VRS-OPR-ETRD(IC-TAB)
                                         TO  SGTSLJ11-NR-VRS-OPR-ETRD
           MOVE   TIPO-EMISSOR           TO  SGTSLJ11-CD-TIP-FUC-CLI
           PERFORM   970112-CHAMA-SGTSLJ11
           MOVE   SGTSLJ11-CD-CLI        TO  GDA-CLI-EMISSOR
      
           INITIALIZE         SGTSLJ11-DADOS
      
           SET    SGTSLJ11-CONS-CLI-TP-PRTC TO  TRUE
           MOVE   SGTS001A-CD-CTRA-TIT-VL(IC-TAB)
                                         TO  SGTSLJ11-CD-CTRA-TIT-VL
           MOVE   SGTS001A-CD-OPR-ETRD-MCP(IC-TAB)
                                         TO  SGTSLJ11-CD-OPR-ETRD-MCP
           MOVE   SGTS001A-NR-VRS-OPR-ETRD(IC-TAB)
                                         TO  SGTSLJ11-NR-VRS-OPR-ETRD
           MOVE   TIPO-IMPACTO-LC        TO  SGTSLJ11-CD-TIP-FUC-CLI
           PERFORM   970112-CHAMA-SGTSLJ11
           MOVE   SGTSLJ11-CD-CLI        TO  GDA-CLI-IMPACTO-LC
           .
       322000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       323000-CONSULTA-PSC-DRIA-SDO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0009                        TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE         SGTST004-DADOS
                              REPLACING NUMERIC BY  ZEROS
      
           SET    SGTST004-CONSULTAR     TO  TRUE
           MOVE   SGTS001A-CD-CTRA-TIT-VL(IC-TAB)
                                         TO  SGTST004-CD-CTRA-TIT-VL
           MOVE   CORR SGTS001A-DT-PSC-DRV-R(IC-TAB)
                                         TO  DT-DB2-R
           MOVE   DT-DB2                 TO  SGTST004-DT-PSC
      
           MOVE   SGTSX005-CD-TIT-OPR-ETRD
                                         TO  SGTST004-CD-TIT-OPR-ETRD
           MOVE   SGTSX005-NR-DVS-TIT
                                         TO  SGTST004-NR-DVS-TIT
           MOVE   SGTS001A-NR-SEQL-PSC-DRIA(IC-TAB)
                                         TO  SGTST004-NR-SEQL-PSC-DRIA
      
           PERFORM   970116-CHAMA-SGTST004
      
           .
       323000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       325000-APURAR-VALORES       SECTION.
      *----------------------------------------------------------------*
           MOVE 0011                        TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE         SGTSPE22-DADOS SGTSLJ11-DADOS
                              GDA-VL-SDO-TOL GDA-VL-SDO-CPTL
                              GDA-VL-EM-ATRASO GDA-VL-SDO-JUR
                              REPLACING NUMERIC BY  ZEROS

           MOVE  ZEROS        TO       IN-OPR-ATRASADA
           MOVE  '00.00.0000' TO SGTSLJ11-DT-PSC-E
           MOVE   SGTS001A-CD-OPR-ETRD-MCP(IC-TAB)
                                         TO  SGTSPE22-CD-OPR-ETRD-MCP
                                             SGTSLJ11-CD-OPR-ETRD-MCP
           MOVE   SGTS001A-NR-VRS-OPR-ETRD(IC-TAB)
                                         TO  SGTSPE22-NR-VRS-OPR-ETRD
                                             SGTSLJ11-NR-VRS-OPR-ETRD
           MOVE   SGTS001A-CD-CTRA-TIT-VL(IC-TAB)
                                         TO  SGTSPE22-CD-CTRA-TIT-VL
                                             SGTSLJ11-CD-CTRA-TIT-VL
           PERFORM   970117-CHAMA-SGTSPE22

           MOVE   SGTSPE22-NR-SEQL-RCBT-PRVT
                                         TO  SGTSLJ11-NR-SEQL-RCBT-PRVT
           MOVE   CORR SGTS001A-DT-PSC-DRV-R(IC-TAB)
                                         TO  SGTSLJ11-DT-PSC-E-R
           MOVE   CORR SGTS001A-DT-PSC-DRV-R(IC-TAB)
                                         TO   DT-PSC-INVERTIDA-R
           SET    SGTSLJ11-OBTER-VALORES TO  TRUE
           PERFORM 970112-CHAMA-SGTSLJ11
           MOVE   SGTSLJ11-SLDO-AMRTZ-CARECA
                                         TO  GDA-VL-SDO-CPTL

           MOVE   CORR SGTSLJ11-DT-PRMO-CRNG-ABTO-R TO DT-AAAAMMDD-R
      *    operação encerrada ou não atrasada
           IF     EST-SGT-ENCERRADA          OR
                  SGTSLJ11-DT-PRMO-CRNG-ABTO EQUAL '01.01.1900' OR
                  DT-AAAAMMDD-R              GREATER
                                             DT-PSC-INVERTIDA-R
                  MOVE ZEROS             TO  GDA-VL-EM-ATRASO
           ELSE
      *    operação atrasada
             IF  SGTSX005-NR-VRS-OPR-ETRD  EQUAL 1
                 MULTIPLY SGTSX005-QT-PAPL-TIT-CPR   BY
                          SGTSX005-VL-PU-EMS-TIT-OPR GIVING
                          GDA-VL-CMP-CARECA
                   ON SIZE ERROR
                    MOVE 'Erro em operação aritmética.'
                                       TO MSG1-TX-ERRO-TS
                    MOVE 0706          TO MSG1-CD-ERRO-TS
                    SET MNT-VRV        TO TRUE
                    SET ERRO-PROGRAMADO TO TRUE
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
                 END-MULTIPLY
             ELSE
                 MOVE SGTSX005-VL-EVT-VCLD-VRS
                                          TO GDA-VL-CMP-CARECA
             END-IF
             SET OPR-ATRASADA             TO TRUE
      ****** FORMA ANTIGA DE CALCULAR VALOR EM ATRASO ******************
      *      COMPUTE GDA-VL-EM-ATRASO = SGTST004-VL-SDO-TIT-DT-PSC -
      *              (GDA-VL-CMP-CARECA - SGTSLJ11-SLDO-RCBT-PRVT)
      *        ON SIZE ERROR
      *        MOVE 'Erro em operação aritmética.'
      *                                TO MSG1-TX-ERRO-TS
      *        MOVE 0706               TO MSG1-CD-ERRO-TS
      *        SET MNT-VRV             TO TRUE
      *        SET ERRO-PROGRAMADO     TO TRUE
      *        PERFORM 999000-GRAVA-ERRO-E-ENCERRA
      *      END-COMPUTE
           END-IF
           MOVE   CORR SGTSLJ11-DT-PRMO-CRNG-ABTO-R
                                                 TO  GDA-DT-PRMO-CRNG-R

      *   Calcula novo saldo da drive quando o amortizável for menor
      *    que o da drive. Cuidado que estou alterando aqui o saldo que
      *    vem da tabela da posição diária do saldo

           IF     SGTST004-VL-SDO-TIT-DT-PSC
                                         LESS SGTSLJ11-SLDO-AMRTZ-CARECA
                  PERFORM 325001-CALC-TOLERANCIA
                  COMPUTE GDA-VL-SDO-TOL = SGTST004-VL-SDO-TIT-DT-PSC
                          + (SGTST004-VL-SDO-TIT-DT-PSC *
                             (GDA-FATOR-TOLERANCIA / 100))
                   ON SIZE ERROR
                    MOVE 'Erro em operação aritmética.'
                                           TO MSG1-TX-ERRO-TS
                    MOVE 0706              TO MSG1-CD-ERRO-TS
                    SET MNT-VRV            TO TRUE
                    SET ERRO-PROGRAMADO    TO TRUE
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
                  END-COMPUTE
                  IF GDA-VL-SDO-TOL      NOT LESS
                                         SGTSLJ11-SLDO-AMRTZ-CARECA
                     MOVE SGTSLJ11-SLDO-AMRTZ-CARECA
                                         TO SGTST004-VL-SDO-TIT-DT-PSC
                  ELSE
                     SET ERRO-PROCESSAMENTO TO  TRUE
                     MOVE 'Saldo calc. c/ fator tolerancia < amortiz. ca
      -                   'reca.'      TO   MSG-REGISTRO-ERRO
                  END-IF
           END-IF

           COMPUTE GDA-VL-SDO-JUR =  SGTST004-VL-SDO-TIT-DT-PSC -
                                      GDA-VL-SDO-CPTL
              ON SIZE ERROR
                 MOVE 'Erro em operação aritmética.'
                                      TO MSG1-TX-ERRO-TS
                 MOVE 0706            TO MSG1-CD-ERRO-TS
                 SET ERRO-PROGRAMADO  TO TRUE
                 SET MNT-VRV         TO TRUE
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-COMPUTE

           IF OPR-ATRASADA
      ****** NOVA FORMA DE CALCULAR VALOR EM ATRASO  *******************
              IF SGTSLJ11-QTDE-JUR-ABTO GREATER  ZEROS
                 COMPUTE GDA-VL-EM-ATRASO =
                   ((GDA-VL-SDO-JUR / SGTSLJ11-QTDE-JUR-ABTO)  *
                                            SGTSLJ11-QTDE-JUR-ATRASO)
                                          +  SGTSLJ11-VL-CPTL-ATRASO
                   ON SIZE ERROR
                   MOVE 'Erro em operação aritmética.'
                                         TO MSG1-TX-ERRO-TS
                   MOVE 0706               TO MSG1-CD-ERRO-TS
                   SET MNT-VRV             TO TRUE
                   SET ERRO-PROGRAMADO     TO TRUE
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
                 END-COMPUTE
              ELSE
                 COMPUTE GDA-VL-EM-ATRASO =
                   GDA-VL-SDO-JUR  +  SGTSLJ11-VL-CPTL-ATRASO
                   ON SIZE ERROR
                   MOVE 'Erro em operação aritmética.'
                                         TO MSG1-TX-ERRO-TS
                   MOVE 0706               TO MSG1-CD-ERRO-TS
                   SET MNT-VRV             TO TRUE
                   SET ERRO-PROGRAMADO     TO TRUE
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
                 END-COMPUTE
              END-IF
           END-IF

           IF   NOT ERRO-PROCESSAMENTO
                IF GDA-VL-SDO-CPTL         LESS ZEROS
                   SET ERRO-PROCESSAMENTO TO  TRUE
                   MOVE 'Saldo amortizável careca menor que zero.'
      -                                   TO   MSG-REGISTRO-ERRO
                 ELSE
                   IF GDA-VL-EM-ATRASO     LESS ZEROS
                      SET ERRO-PROCESSAMENTO TO  TRUE
                      MOVE 'Valor em atraso menor que  zero.'
                                           TO   MSG-REGISTRO-ERRO
                   ELSE
                     IF GDA-VL-SDO-JUR     LESS ZEROS
                        SET ERRO-PROCESSAMENTO TO  TRUE
                        MOVE 'Valor de juros menor que zero.'
                                           TO   MSG-REGISTRO-ERRO
                     END-IF
                    END-IF
                  END-IF
           END-IF
           .
       325000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       325001-CALC-TOLERANCIA       SECTION.
      *----------------------------------------------------------------*
           MOVE 0012                        TO MSG1-CD-SEQL-ERRO

           MOVE   CORR SGTS001A-DT-PSC-DRV-R(IC-TAB)
                                         TO  GDA-TIMESTAMP-R

           EXEC SQL
              SELECT A.PC_VCLD_CRCT_CTRA
              INTO   :GDA-FATOR-TOLERANCIA
              FROM DB2SGT.CRCT_CTRA_TVM  A
              INNER JOIN DB2SGT.EST_CRCT_CTRA_TVM  B
                    ON  A.CD_CTRA_TVM       =  B.CD_CTRA_TVM
                    AND A.CD_TIP_ATB_CTRA   =  B.CD_TIP_ATB_CTRA
                    AND A.NR_ETRD_CRCT_CTRA =  B.NR_ETRD_CRCT_CTRA

              WHERE A.CD_CTRA_TVM         = :SGTSPE22-CD-CTRA-TIT-VL
                AND A.CD_TIP_ATB_CTRA     = 11
                AND A.NR_ETRD_CRCT_CTRA   > 0
                AND B.TS_EST_CRCT_CTRA = (SELECT MAX(C.TS_EST_CRCT_CTRA)
                  FROM DB2SGT.EST_CRCT_CTRA_TVM C
                  WHERE B.CD_CTRA_TVM        =  C.CD_CTRA_TVM
                    AND B.CD_TIP_ATB_CTRA    =  C.CD_TIP_ATB_CTRA
                    AND B.NR_ETRD_CRCT_CTRA  =  C.NR_ETRD_CRCT_CTRA
                    AND C.CD_TIP_EST_DCS     = 2
                    AND C.CD_TIP_ATB_CTRA    = 11
                    AND C.TS_EST_CRCT_CTRA   <= :GDA-TIMESTAMP)
                    ORDER BY B.TS_EST_CRCT_CTRA  DESC
                    FETCH FIRST 1 ROW ONLY
           END-EXEC

           .
       325001-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       326000-CONSULTA-EST-OPR         SECTION.
      *----------------------------------------------------------------*
           MOVE 0013                        TO MSG1-CD-SEQL-ERRO
      
      
           INITIALIZE         SGTST009-DADOS
                              REPLACING NUMERIC BY  ZEROS
      
           SET    SGTST009-CONSULTAR-EST-ATIVO TO TRUE
           MOVE   SGTS001A-CD-OPR-ETRD-MCP(IC-TAB)
                                         TO  SGTST009-CD-OPR-ETRD-MCP
           MOVE   SGTS001A-NR-VRS-OPR-ETRD(IC-TAB)
                                         TO  SGTST009-NR-VRS-OPR-ETRD
           MOVE   SGTS001A-CD-CTRA-TIT-VL(IC-TAB)
                                         TO  SGTST009-CD-CTRA-TIT-VL
      
           PERFORM   970118-CHAMA-SGTST009
      
           MOVE   SGTST009-CD-TIP-EST-OPR  TO GDA-EST-SGT
      
           EVALUATE   TRUE
             WHEN     EST-SGT-ENCARTEIRADA
                   MOVE SGTSX005-DT-CPR-OPR TO GDA-DT-EST-CDU
                   SET EST-OPR-ATIVA    TO   TRUE
             WHEN     EST-SGT-ENCERRADA
                   MOVE SGTSX005-DT-LQDC-OPR-DRIV TO GDA-DT-EST-CDU
                   SET EST-OPR-ENCERRADA  TO   TRUE
             WHEN  OTHER
                   MOVE CORR SGTS001A-DT-PSC-DRV-R(IC-TAB)
                                               TO GDA-DT-EST-CDU-R
                   SET EST-OPR-DEFERIDO-N-FRMO  TO   TRUE
           END-EVALUATE
      
           .
       326000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       327000-TRATAR-REG-PRE-EXISTENT  SECTION.
      *----------------------------------------------------------------*
           MOVE 0014                     TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE SGTSLJ11-DADOS
                      REPLACING NUMERIC BY  ZEROS
           SET   SGTSLJ11-CONS-ULT-DT-PSC-SDO      TO  TRUE
           MOVE  SGTSX005-CD-CTRA-TIT-VL   TO SGTSLJ11-CD-CTRA-TIT-VL
           MOVE  SGTSX005-CD-TIT-OPR-ETRD  TO SGTSLJ11-CD-TIT-OPR-ETRD
           MOVE  SGTSX005-NR-DVS-TIT       TO SGTSLJ11-NR-DVS-TIT
           PERFORM  970112-CHAMA-SGTSLJ11
           MOVE  CORR SGTSLJ11-DT-PSC-R     TO  DT-AAAAMMDD-R
           MOVE CORR SGTS001A-DT-PSC-DRV-R(IC-TAB) TO DT-PSC-INVERTIDA-R
           IF DT-PSC-INVERTIDA < DT-AAAAMMDD
                 SET ERRO-PROCESSAMENTO  TO  TRUE
                 MOVE 'Data posição inferior a ultima data enviada ao OP
      -               'R.'                TO   MSG-REGISTRO-ERRO
      
           ELSE
              SET SGTSLJ12-LISR-PSC-DRIA-SDO-PND      TO TRUE
              MOVE SGTS001A-CD-CTRA-TIT-VL(IC-TAB)    TO
                                       SGTSLJ12-CD-CTRA-TIT-VL-E
              MOVE SGTS001A-CD-OPR-ETRD-MCP(IC-TAB)   TO
                                       SGTSLJ12-CD-OPR-ETRD-MCP-E

              MOVE ZEROS                              TO
                                       SGTSLJ12-ULT-NR-SEQL-PSC-DRIA
              MOVE SPACES                             TO
                                       SGTSLJ12-ULT-DT-PSC-E
      
              PERFORM   970119-CHAMA-SGTSLJ12
      
              MOVE ZEROS                  TO   IN-PSC-SDO
      
              PERFORM   332000-PROCESSA-LISTA
                     VARYING SGTSLJ12-IC-TAB   FROM 1 BY 1 UNTIL
                     SGTSLJ12-IC-TAB GREATER SGTSLJ12-QT-OCR-TAB  OR
                     EXISTE-SALDO-MAIS-RECENTE
           END-IF
      
           .
       327000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       328000-CALCULAR-EST-VNCT-SCTR   SECTION.
      *----------------------------------------------------------------*
      * ARG01 = data do primeiro cronograma (GDA-DT-PRMO-CRNG)
      * ARG02 = data posição (DT-PSC)
      * ARG03 = retorna a quantidade de dias da diferença entre as datas
      *----------------------------------------------------------------*
      
           MOVE 0015                     TO MSG1-CD-SEQL-ERRO

           IF NOT OPR-INCLUSAO
      *       Verifica se é Ativo Problemático
              PERFORM 328100-ACESSA-CACS00IC
           ELSE
              MOVE  '0'   TO  CACK00IC-IN-ATPB
           END-IF

           IF (CACK00IC-IN-ATPB EQUAL '9'  OR
               CACK00IC-IN-ATPB EQUAL 'P'     )
      *       É Ativo Problemático
              MOVE 30 TO GDA-CD-EST-VNCT-SCTR
           ELSE
      *       Não é  Ativo Problemático então verifica se é operação
      *       Vincenda ou Vencida
              MOVE  CORR GDA-DT-PRMO-CRNG-R  TO  ARG01-R
              MOVE CORR SGTS001A-DT-PSC-DRV-R(IC-TAB)  TO  ARG02-R
      
              MOVE CORR GDA-DT-PRMO-CRNG-R   TO DT-PRMO-CRNG-INVERTIDA-R
              MOVE CORR SGTS001A-DT-PSC-DRV-R(IC-TAB)
                                             TO DT-PSC-INVERTIDA-R

              CALL SBDATAF USING FUNCAO AREA-WORK ARG01 ARG02 ARG03
              IF   RETURN-CODE             NOT EQUAL ZEROS
                SET ERRO-PROGRAMA-ACIONADO
                                        TO  TRUE
                MOVE 0051               TO  MSG1-CD-ERRO-TS
                MOVE 'Erro no tratamento de datas.'
                                        TO  MSG1-TX-ERRO-TS
                                            MSG1-TX-RTN-PGM-ACND
                MOVE SBDATAF            TO  MSG1-NM-PGM-ACND
                                            NM-PGM
                MOVE RETURN-CODE        TO  MSG1-CD-RTN-PGM-ACND
                SET MNT-VRV             TO TRUE
                PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-IF
      
              MOVE ARG03    TO   ARG03-ABS
      
              IF DT-PRMO-CRNG-INVERTIDA > DT-PSC-INVERTIDA
      *       Vincenda
      *
                IF ARG03-ABS > 90
                   MOVE 20 TO GDA-CD-EST-VNCT-SCTR
      
                END-IF
                IF ARG03-ABS > 60 AND ARG03-ABS < 91
                   MOVE 21 TO GDA-CD-EST-VNCT-SCTR

                END-IF
                IF ARG03-ABS > 30 AND ARG03-ABS < 61
                   MOVE 22 TO GDA-CD-EST-VNCT-SCTR
      
                END-IF
                IF ARG03-ABS < 31
                   MOVE 23 TO GDA-CD-EST-VNCT-SCTR
      
                END-IF
      *
              ELSE
      *       Vencida
      *
                IF ARG03-ABS < 31
                   MOVE 24 TO GDA-CD-EST-VNCT-SCTR
      
                END-IF
                IF ARG03-ABS > 30 AND ARG03-ABS < 61
                   MOVE 25 TO GDA-CD-EST-VNCT-SCTR
      
                END-IF
                IF ARG03-ABS > 60 AND ARG03-ABS < 91
                   MOVE 26 TO GDA-CD-EST-VNCT-SCTR

                END-IF
                IF ARG03-ABS > 90
                   MOVE 27 TO GDA-CD-EST-VNCT-SCTR
      
                END-IF
              END-IF
           END-IF
           .
       328000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       328100-ACESSA-CACS00IC  SECTION.
      * Verifica se é ativo problemático no CACS00IC
      *----------------------------------------------------------------*
           MOVE         0079              TO MSG1-CD-SEQL-ERRO

           INITIALIZE         CACS00IC-DADOS
                              REPLACING NUMERIC BY  ZEROS
           MOVE SGTSX005-NR-UNCO-CTR-OPR  TO  CACK00IC-NR-UNCO-CTR-OPR
           MOVE   CORR SGTS001A-DT-PSC-DRV-R(IC-TAB)
                                          TO   DT-CAC-R
           MOVE     DT-CAC                TO  CACK00IC-DT-ANL-RSCO-ATPB
           MOVE      'O'                  TO  CACK00IC-CD-AMB
           MOVE   'SGTS001A'              TO  CACK00IC-NM-PGM

      *---> CHAMA  CACS00IC
           EXEC CICS LINK
                        PROGRAM ( CACS00IC       )
                        COMMAREA( CACS00IC-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo CACS00IC.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF CACK00IC-CD-ERRO NOT EQUAL ZEROS
              IF CACK00IC-CD-SQLCODE EQUAL +100
                 MOVE  '0'   TO  CACK00IC-IN-ATPB
              ELSE
                 SET ERRO-PROGRAMA-ACIONADO   TO  TRUE
                 MOVE CACK00IC-CD-ERRO        TO  MSG1-CD-ERRO-TS
                                                  MSG1-CD-RTN-PGM-ACND
                 MOVE CACK00IC-TX-SQLERRM     TO  MSG1-TX-ERRO-TS
                                                  MSG1-TX-RTN-PGM-ACND
                 MOVE     CACS00IC            TO  MSG1-NM-PGM-ACND
                                                  NM-PGM
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-IF
           END-IF
           .
       328100-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       329000-ATLZ-ERRO-PSC-DRIA-SDO   SECTION.
      *----------------------------------------------------------------*
           MOVE 0016                        TO MSG1-CD-SEQL-ERRO
      
           SET    SGTST004-ALTERAR       TO  TRUE
           MOVE   EST-ERRO-PRCT          TO  SGTST004-CD-EST-RSTD-ITE
           MOVE   MSG-REGISTRO-ERRO      TO  SGTST004-TX-RTN-PRCT-OPR
           MOVE   SGTS001A-CD-CTRA-TIT-VL(IC-TAB)
                                         TO  SGTST004-CD-CTRA-TIT-VL
      
           PERFORM   970116-CHAMA-SGTST004
      
           .
       329000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       329100-FMT-DEP-OPR              SECTION.
      *----------------------------------------------------------------*
           MOVE 0017                        TO MSG1-CD-SEQL-ERRO
      *
           IF  SGTSX005-CD-UOR-BB    >   ZEROS
      * BUSCA DEPENDENCIA
              INITIALIZE   MSTS8042-DADOS
                           REPLACING NUMERIC BY  ZEROS
              MOVE  MSTS8042                   TO NM-PGM
              MOVE  SGTSX005-CD-UOR-BB         TO MSTS8042-CD-UOR
              PERFORM 980000-CHAMA-SERVICO
              MOVE MSTS8042-CD-DEPE-UOR        TO GDA-DEP-OPR
           END-IF
      
           .
       329100-SAI.
           EXIT.
      *----------------------------------------------------------------*
       329200-OBTER-TAXA-REMUNERACAO   SECTION.
      *----------------------------------------------------------------*
           MOVE  0018                  TO  MSG1-CD-SEQL-ERRO
      
           INITIALIZE  SGTST011-DADOS REPLACING NUMERIC BY  ZEROS
      
           SET   SGTST011-CSUL-TX-RMNC-DRIA
                                       TO  TRUE
           MOVE  SGTS001A-CD-CTRA-TIT-VL(IC-TAB)
                                       TO  SGTST011-CD-CTRA-TIT-VL
           MOVE  SGTS001A-CD-OPR-ETRD-MCP(IC-TAB)
                                       TO  SGTST011-CD-OPR-ETRD-MCP
           MOVE  SGTS001A-NR-VRS-OPR-ETRD(IC-TAB)
                                       TO  SGTST011-NR-VRS-OPR-ETRD
           MOVE  CORR  SGTS001A-DT-PSC-DRV-R(IC-TAB)
                                       TO  DT-DB2-R
           MOVE  DT-DB2                TO  SGTST011-DT-INC-TAXA-RMNC
           MOVE  KOPRD-CD-OPRD         TO  SGTST011-CD-USU-RSP-ATL
      
           PERFORM  970120-CHAMA-SGTST011
           .
       329200-SAI.
           EXIT.
      *----------------------------------------------------------------*
       329300-ATLZ-PRCT-SUCESSO        SECTION.
      *----------------------------------------------------------------*
           MOVE 0019                        TO MSG1-CD-SEQL-ERRO
      
           SET    SGTST004-ALTERAR       TO  TRUE
           MOVE   'CURRENT'              TO  SGTST004-TS-ENVO-PSC
           MOVE   KOPRD-CD-OPRD          TO  SGTST004-CD-USU-ENVO-PSC
           MOVE   EST-PRCT-SUCESSO       TO  SGTST004-CD-EST-RSTD-ITE
           MOVE   LST-ERR-OPR            TO  SGTST004-TX-RTN-PRCT-OPR
           MOVE   SGTS001A-CD-CTRA-TIT-VL(IC-TAB)
                                         TO  SGTST004-CD-CTRA-TIT-VL
      
           PERFORM   970116-CHAMA-SGTST004
      
           .
       329300-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       329400-ATLZ-OPR-SGT-SUCESSO     SECTION.
      *----------------------------------------------------------------*
           MOVE 0020                        TO MSG1-CD-SEQL-ERRO
      
           SET    SGTSX005-ALTERAR       TO  TRUE
           MOVE   GDA-NR-UNCO-CTR-OPR    TO  SGTSX005-NR-UNCO-CTR-OPR
           MOVE   KOPRD-CD-OPRD          TO  SGTSX005-CD-CHV-USU
           IF OPR-EFETIVA
              MOVE     'E'               TO  SGTSX005-IN-REG-OPR-BB
           END-IF
      
           PERFORM   970113-CHAMA-SGTSX005
      
           .
       329400-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       329500-TRATAR-NR-UNCO-CTR-OPR   SECTION.
      *----------------------------------------------------------------*
           MOVE 0021                     TO MSG1-CD-SEQL-ERRO
      
           SET SGTSLJ12-LISR-VRS-OPR-N-ENV         TO TRUE
           MOVE SGTS001A-CD-CTRA-TIT-VL(IC-TAB)    TO
                                       SGTSLJ12-CD-CTRA-TIT-VL-E
           MOVE SGTS001A-CD-OPR-ETRD-MCP(IC-TAB)   TO
                                       SGTSLJ12-CD-OPR-ETRD-MCP-E
           MOVE ZEROS                              TO
                                        SGTSLJ12-ULT-NR-VRS-OPR
      
           PERFORM   970119-CHAMA-SGTSLJ12
      
           PERFORM   339510-ATLZ-NR-UNCO-CTR-OPR
                     VARYING SGTSLJ12-IC-TAB   FROM 1 BY 1 UNTIL
                     SGTSLJ12-IC-TAB GREATER SGTSLJ12-QT-OCR-TAB
      
           .
       329500-SAI.
           EXIT.
      *----------------------------------------------------------------*
       339510-ATLZ-NR-UNCO-CTR-OPR     SECTION.
      *----------------------------------------------------------------*
           MOVE 0022                   TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE         SGTSX005-DADOS
                              REPLACING NUMERIC BY  ZEROS
      
           SET    SGTSX005-CONSULTAR     TO  TRUE
           MOVE   SGTS001A-CD-OPR-ETRD-MCP(IC-TAB)
                                         TO  SGTSX005-CD-OPR-ETRD-MCP
           MOVE   SGTSLJ12-NR-VRS-OPR-ETRD(SGTSLJ12-IC-TAB)
                                         TO  SGTSX005-NR-VRS-OPR-ETRD
           MOVE   SGTS001A-CD-CTRA-TIT-VL(IC-TAB)
                                         TO  SGTSX005-CD-CTRA-TIT-VL
      
           PERFORM   970113-CHAMA-SGTSX005
      
           SET    SGTSX005-ALTERAR       TO  TRUE
           MOVE   GDA-NR-UNCO-CTR-OPR    TO  SGTSX005-NR-UNCO-CTR-OPR
           MOVE   KOPRD-CD-OPRD          TO  SGTSX005-CD-CHV-USU
      
           PERFORM   970113-CHAMA-SGTSX005
           IF SGTSLJ12-PREENCHIDO-TAB AND SGTSLJ12-TEM-MAIS-REGISTRO
              MOVE SGTSLJ12-NR-VRS-OPR-ETRD(SGTSLJ12-IC-TAB) TO
                                           SGTSLJ12-ULT-NR-VRS-OPR
              PERFORM 970119-CHAMA-SGTSLJ12
              IF SGTSLJ12-QT-OCR-TAB > ZEROS
                   MOVE   ZEROS    TO  SGTSLJ12-IC-TAB
              END-IF
           END-IF
           .
       329510-SAI.
           EXIT.
      
      
      *----------------------------------------------------------------*
       330000-DESFAZER-GRAVACAO        SECTION.
      *----------------------------------------------------------------*
           MOVE 0023                   TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS
                 SYNCPOINT ROLLBACK
           END-EXEC
           .
       330000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       331000-EFETIVAR-GRAVACAO        SECTION.
      *----------------------------------------------------------------*
           MOVE 0024                   TO MSG1-CD-SEQL-ERRO
      
           EXEC  CICS
                SYNCPOINT
           END-EXEC
      
           .
       331000-SAI.
           EXIT.
      
      
      *----------------------------------------------------------------*
       332000-PROCESSA-LISTA           SECTION.
      *----------------------------------------------------------------*
           MOVE 0025                   TO MSG1-CD-SEQL-ERRO
      
           MOVE CORR SGTSLJ12-DT-PSC-R(SGTSLJ12-IC-TAB) TO DT-AAAAMMDD-R
           MOVE CORR SGTS001A-DT-PSC-DRV-R(IC-TAB) TO DT-PSC-INVERTIDA-R
           IF DT-AAAAMMDD  NOT = DT-PSC-INVERTIDA OR
              SGTSLJ12-NR-SEQL-PSC-DRIA(SGTSLJ12-IC-TAB) NOT =
                                       SGTS001A-NR-SEQL-PSC-DRIA(IC-TAB)
              IF (DT-AAAAMMDD  = DT-PSC-INVERTIDA
                  AND SGTSLJ12-NR-SEQL-PSC-DRIA(SGTSLJ12-IC-TAB) >
                      SGTS001A-NR-SEQL-PSC-DRIA(IC-TAB)) OR
                      DT-AAAAMMDD    > DT-PSC-INVERTIDA
                  MOVE 'Existe pos. diária de saldo mais recente para en
      -                'vio.'                   TO  MSG-REGISTRO-ERRO
                  SET EXISTE-SALDO-MAIS-RECENTE TO TRUE
                  SET ERRO-PROCESSAMENTO        TO TRUE
      
              ELSE
                  PERFORM 334000-ENVDO-ITEM-MAIS-RECENTE
              END-IF
            END-IF
      
            IF SGTSLJ12-PREENCHIDO-TAB AND SGTSLJ12-TEM-MAIS-REGISTRO
               MOVE SGTSLJ12-DT-PSC(SGTSLJ12-IC-TAB) TO
                                           SGTSLJ12-ULT-DT-PSC-E
               MOVE SGTSLJ12-NR-SEQL-PSC-DRIA(SGTSLJ12-IC-TAB) TO
                                           SGTSLJ12-ULT-NR-SEQL-PSC-DRIA
               PERFORM 970119-CHAMA-SGTSLJ12
               IF SGTSLJ12-QT-OCR-TAB > ZEROS
                    MOVE   ZEROS    TO  SGTSLJ12-IC-TAB
               END-IF
            END-IF
           .
       332000-SAI.
           EXIT.
      
      
      *----------------------------------------------------------------*
       334000-ENVDO-ITEM-MAIS-RECENTE  SECTION.
      *----------------------------------------------------------------*
           MOVE 0026                   TO MSG1-CD-SEQL-ERRO
      
           SET  SGTST004-CONSULTAR       TO TRUE
           MOVE SGTSLJ12-CD-TIT-OPR-ETRD(SGTSLJ12-IC-TAB)
                                         TO SGTST004-CD-TIT-OPR-ETRD
           MOVE SGTSLJ12-NR-DVS-TIT(SGTSLJ12-IC-TAB)
                                         TO SGTST004-NR-DVS-TIT
           MOVE CORR SGTSLJ12-DT-PSC-R(SGTSLJ12-IC-TAB) TO DT-DB2-R
           MOVE DT-DB2                   TO SGTST004-DT-PSC
           MOVE SGTSLJ12-NR-SEQL-PSC-DRIA(SGTSLJ12-IC-TAB)
                                         TO SGTST004-NR-SEQL-PSC-DRIA
           MOVE SGTSLJ12-CD-CTRA-TIT-VL(SGTSLJ12-IC-TAB)
                                         TO SGTST004-CD-CTRA-TIT-VL
      
           PERFORM   970116-CHAMA-SGTST004
      
           SET  SGTST004-ALTERAR          TO TRUE
           MOVE 4                         TO  SGTST004-CD-EST-RSTD-ITE
           MOVE KOPRD-CD-OPRD             TO  SGTST004-CD-USU-ENVO-PSC
           MOVE 'CURRENT'                 TO  SGTST004-TS-ENVO-PSC
      
           PERFORM   970116-CHAMA-SGTST004
           .
       334000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       335000-BUSCAR-ULT-CLI-EMS       SECTION.
      *----------------------------------------------------------------*

           MOVE 0027                   TO MSG1-CD-SEQL-ERRO
           INITIALIZE         SGTSLJ11-DADOS
                              REPLACING NUMERIC BY  ZEROS
      
           SET    SGTSLJ11-CONS-ULT-CLI-EMS  TO TRUE
           MOVE   SGTS001A-CD-OPR-ETRD-MCP(IC-TAB)
                                            TO  SGTSLJ11-CD-OPR-ETRD-MCP
           MOVE   SGTS001A-CD-CTRA-TIT-VL(IC-TAB)
                                            TO  SGTSLJ11-CD-CTRA-TIT-VL
           MOVE   SGTS001A-NR-VRS-OPR-ETRD(IC-TAB)
                                            TO  SGTSLJ11-NR-VRS-OPR-ETRD
           IF     SGTSLJ11-NR-VRS-OPR-ETRD GREATER  1
                  COMPUTE SGTSLJ11-NR-VRS-OPR-ETRD =
                          SGTSLJ11-NR-VRS-OPR-ETRD - 1
                  ON SIZE ERROR
                     MOVE 'Erro em operação aritmética.'
                                            TO MSG1-TX-ERRO-TS
                     MOVE 0706              TO MSG1-CD-ERRO-TS
                     SET MNT-VRV            TO TRUE
                     SET ERRO-PROGRAMADO    TO TRUE
                     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
                   END-COMPUTE
           END-IF

           PERFORM   970112-CHAMA-SGTSLJ11
           MOVE   SGTSLJ11-CD-CLI        TO  GDA-ULT-CLI-EMISSOR
           .
       335000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       340000-ACESSA-CAC  SECTION.
      *----------------------------------------------------------------*
           MOVE        0067              TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE         CACSV045-DADOS
                              REPLACING NUMERIC BY  ZEROS
           MOVE          4               TO   CACS045V-CTL-CD-CNL
           MOVE         'O'              TO   CACS045V-CTL-CD-MODO-OPR
           MOVE   KOPRD-CD-OPRD          TO   CACS045V-CD-CHV-USU
           MOVE          1               TO   CACS045V-ACAO
           MOVE  GDA-NR-UNCO-CTR-OPR     TO   CACS045V-NR-UNCO-OPR
           MOVE   CORR SGTS001A-DT-PSC-DRV-R(IC-TAB)
                                         TO   DT-CAC-R
           MOVE      DT-CAC              TO   CACS045V-DT-PU-PSC-ITMT
      
           PERFORM   970115-CHAMA-CACSV045
           .
       340000-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       960001-INCLUIR-CTR-OPR          SECTION.
      * Aciona rotina do sistema OPR para incluir contrato no contrato
      *  da operação e incluir participacao da pessoa
      *-----------------------------------------------------------------
           MOVE 0028                   TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE   OPRS9401-DADOS
      
           MOVE  1                          TO W9401-FUNCAO
           MOVE  'SGTS001A'                 TO W9401-PRG-CHAMADA
           MOVE  'O'                        TO W9401-AMB-CHAMADA

      *    Dados do contrato

           IF SGTSX005-CD-UOR-DTTR-OPR   > ZEROS
      * BUSCA DEPENDENCIAS
              INITIALIZE   MSTS8042-DADOS
              MOVE  MSTS8042                   TO NM-PGM
              MOVE  SGTSX005-CD-UOR-DTTR-OPR   TO MSTS8042-CD-UOR
              PERFORM 980000-CHAMA-SERVICO
              MOVE MSTS8042-CD-DEPE-UOR        TO W9401-CD-PRF-DEPE-NEGR
           END-IF
      
      *    Nova TAG para ANC descobrir grupo da Carteira
           EVALUATE SGTSX005-CD-CTRA-TIT-VL
           WHEN 300000
           WHEN 400000
           WHEN 500000
           WHEN 999999
               MOVE         9867           TO  W9401-CD-PRF-DEPE-CDU
           WHEN 300001
               MOVE         8595           TO  W9401-CD-PRF-DEPE-CDU
           WHEN 330
           WHEN 333
           WHEN 337
               MOVE         8875           TO  W9401-CD-PRF-DEPE-CDU
      *     WHEN 200000
      *         MOVE         8555           TO  W9401-CD-PRF-DEPE-CDU
           END-EVALUATE

           MOVE 'SGT'                       TO W9401-SG-SIS-RSP-CDU
           MOVE SGTSX005-CD-OPR-ETRD-MCP    TO W9401-NR-CTR-OPR
           MOVE SGTSX005-CD-PRD             TO W9401-CD-PRD
           MOVE SGTSX005-CD-MDLD            TO W9401-CD-MDLD
           MOVE GDA-EST-OPR                 TO W9401-CD-EST-CDU
           MOVE CORR SGTSX005-DT-CPR-OPR-R  TO DT-OPR-R
           MOVE  DT-OPR                     TO W9401-DT-FRMZ
           MOVE CORR  SGTSX005-DT-VNCT-OPR-R  TO DT-OPR-R
           MOVE DT-OPR                      TO W9401-DT-VNCT
           MOVE SGTSX005-VL-CPR-OPR         TO W9401-VL-OPR
           MOVE 103                         TO W9401-CD-IEC-MOE
      *    MOVE PORTE DO CLIENTE            TO W9401-CD-PRTE-CLI
           MOVE ZEROS                       TO W9401-CD-CNL-FRMZ-CTR
           MOVE CORR GDA-DT-EST-CDU-R       TO DT-OPR-R
           MOVE DT-OPR                      TO W9401-DT-EST-CDU
           MOVE GDA-DEP-OPR                 TO W9401-CD-PRF-DEPE-EXEC

           MOVE ZEROS                       TO  W9401-CD-CIA-RSP-CTR
           MOVE 1                           TO  W9401-CD-IOR-RSP-CNSO
           MOVE 1                           TO  W9401-CD-IOR-OGM
           MOVE 1                           TO  W9401-CD-TIP-OBJ
           MOVE K-ZERO                      TO  W9401-CD-ITEM-CMZC-MDLD
           MOVE 1                           TO  W9401-CD-INST-CRS-BCRO
           MOVE K-ZERO                      TO  W9401-CD-CRS-BCRO
           MOVE 'CNV'                       TO  W9401-SG-SIS-OGM-CVN
           MOVE SGTSX005-CD-UOR-DTTR-OPR    TO  W9401-CD-UOR-DEPE-NEGR

      *    Nova TAG para ANC descobrir grupo da Carteira
           EVALUATE SGTSX005-CD-CTRA-TIT-VL
           WHEN 300000
           WHEN 400000
           WHEN 500000
           WHEN 999999
               MOVE         20237           TO  W9401-CD-UOR-DEPE-CDTR
           WHEN 300001
               MOVE         18951           TO  W9401-CD-UOR-DEPE-CDTR
           WHEN 330
           WHEN 333
           WHEN 337
               MOVE         19230           TO  W9401-CD-UOR-DEPE-CDTR
      *     WHEN 200000
      *         MOVE         18910           TO  W9401-CD-UOR-DEPE-CDTR
           END-EVALUATE

           MOVE K-ZERO                      TO  W9401-CD-TIP-PRTB-BCRO
           MOVE 00010101                    TO  W9401-DT-PRTB-BCRO
           MOVE K-ZERO                      TO  W9401-CD-ISPB-OGM-PRTB
           MOVE 1                           TO  W9401-CD-PAIS-NEGO
      *    Dados do cliente

           IF  SGTSX005-CD-UOR-BB-CRD-VL > ZEROS
      * BUSCA DEPENDENCIA
               INITIALIZE   MSTS8042-DADOS
               MOVE  MSTS8042                   TO NM-PGM
               MOVE  SGTSX005-CD-UOR-BB-CRD-VL  TO MSTS8042-CD-UOR
               PERFORM 980000-CHAMA-SERVICO
               MOVE MSTS8042-CD-DEPE-UOR        TO W9401-CD-DEPE-CC-VCLD
           END-IF

           MOVE GDA-CLI-EMISSOR              TO W9401-CD-PSS-CTR-OPR
           IF   GDA-CLI-EMISSOR    NOT EQUAL  GDA-CLI-IMPACTO-LC   AND
                GDA-CLI-IMPACTO-LC NOT EQUAL  ZEROS
               MOVE GDA-CLI-IMPACTO-LC       TO W9401-CD-PSS-CTR-OPR
           END-IF

           MOVE SGTSX005-NR-CC-BB           TO W9401-NR-CC-VCLD
           MOVE CORR SGTSX005-DT-CPR-OPR-R  TO DT-OPR-R
           MOVE DT-OPR                      TO W9401-DT-INC-PRTC
           MOVE 1                           TO W9401-NR-SEQL-PRTC
           MOVE 1                           TO W9401-CD-TIP-PRTC
           MOVE GDA-EST-OPR                 TO W9401-CD-EST-PRTC
           IF EST-OPR-ENCERRADA
              MOVE SGTSX005-DT-LQDC-OPR-DRIV-R TO DT-OPR-R
              MOVE DT-OPR                      TO W9401-DT-FIM-PRTC
           ELSE
              MOVE 99991231                    TO W9401-DT-FIM-PRTC
           END-IF
           MOVE 001                         TO W9401-CD-PAIS
           MOVE SPACES                      TO W9401-NM-PSS-PSZD
           MOVE OPRS9401                    TO  NM-PGM
           PERFORM  980000-CHAMA-SERVICO
           PERFORM  970101-TRATA-RET-OPRS9401
      
           MOVE  W9401-NR-UNCO-CTR-OPR      TO GDA-NR-UNCO-CTR-OPR
      
           .
       960001-SAI.
           EXIT.
      *-----------------------------------------------------------------
       960003-INCLUIR-EPRD-FNCD        SECTION.
      * Aciona rotina do sistema OPR para incluir empreendimento financi
      * ado no contrato da operação
      *-----------------------------------------------------------------
           MOVE 0029                   TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE   OPRS9502-DADOS
      
           MOVE  1                          TO W9502-FUNCAO
           MOVE  'SGTS001A'                 TO W9502-PRG-CHAMADA
           MOVE  'O'                        TO W9502-AMB-CHAMADA
      
           MOVE 'SGT'                       TO W9502-SG-SIS-RSP
           MOVE GDA-NR-UNCO-CTR-OPR         TO W9502-NR-UNCO-CTR
           MOVE 1                           TO W9502-NR-EPRD-FNCD
           MOVE NEGOCIACAO-TITULO           TO W9502-CD-ITEM-FNCD
           MOVE K-ZERO                      TO W9502-NR-SEQL-EPRD
           MOVE 1                           TO W9502-QT-EPRD-FNCD
           MOVE 1                           TO W9502-QT-ITEM-FNCD
           MOVE SGTSX005-VL-CPR-OPR         TO W9502-VL-EPRD-FNCD
           MOVE GDA-EST-OPR                 TO W9502-CD-EST-CDU-EPRD
           MOVE CORR GDA-DT-EST-CDU-R       TO DT-OPR-R
           MOVE DT-OPR                      TO W9502-DT-EST-CDU-EPRD
      
           PERFORM   970102-CHAMA-OPRS9502
      
           .
       960003-SAI.
           EXIT.
      *-----------------------------------------------------------------
       960004-INCLUIR-SUB-CTR-OPR      SECTION.
      * Aciona rotina do sistema OPR para incluir subcontrato no
      * contrato da operação
      *-----------------------------------------------------------------
           MOVE 0030                        TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE   OPRS9503-DADOS
      
           MOVE  1                          TO W9503-FUNCAO
           MOVE  'SGTS001A'                 TO W9503-PRG-CHAMADA
           MOVE  'O'                        TO W9503-AMB-CHAMADA
      
           MOVE GDA-NR-UNCO-CTR-OPR         TO W9503-NR-UNCO-CTR-OPR
           MOVE 'SGT'                       TO W9503-SG-SIS-RSP-CDU
           MOVE 1                           TO W9503-NR-EPRD-FNCD
           MOVE 1                           TO W9503-NR-SCTR-OPR
           MOVE TITULOS                     TO W9503-CD-DTNC-CRD
      
           IF SGTST011-CD-IEC-RMNC = TAXA-FIXA
              MOVE PREFIXADO                   TO W9503-CD-IEC-CM
              MOVE 100                         TO W9503-PC-APL-CM
              MOVE AO-ANO                      TO W9503-CD-TX-JUR-NM
              MOVE SGTST011-PC-TAXA-RMNC-OPR   TO W9503-PC-TX-JUR-NM
           ELSE
              MOVE SGTST011-CD-IEC-RMNC    TO W9503-CD-IEC-CM
              IF SGTST011-CD-TIP-FMA-APL = PORCENTAGEM
                 MOVE SGTST011-PC-TAXA-RMNC-OPR   TO W9503-PC-APL-CM
                 MOVE SEM-INCIDENCIA-10           TO W9503-CD-TX-JUR-NM
                 MOVE ZEROS                       TO W9503-PC-TX-JUR-NM
              ELSE
                 MOVE 100                         TO W9503-PC-APL-CM
                 MOVE AO-ANO                      TO W9503-CD-TX-JUR-NM
                 MOVE SGTST011-PC-TAXA-RMNC-OPR   TO W9503-PC-TX-JUR-NM
              END-IF
           END-IF
      
           MOVE PREFIXADO                   TO W9503-CD-IEC-CM-IN
           MOVE 100                         TO W9503-PC-APL-CM-IN
      
      
           MOVE 1                           TO W9503-CD-EST-CTB
           MOVE CORR SGTSX005-DT-CPR-OPR-R  TO DT-OPR-R
           MOVE DT-OPR                      TO W9503-DT-EST-CTB
           MOVE 10                          TO W9503-CD-TX-JUR-IN
      *    MOVE ZEROS                       TO W9503-PC-TX-JUR-IN
           MOVE ZEROS                       TO W9503-PC-MULT-INDP
      
           MOVE PREFIXADO                   TO W9503-CD-CM-IN-ESP
           MOVE 100                         TO W9503-PC-CM-IN-ESP
      
           MOVE SEM-TRAVA                   TO W9503-CD-EST-ESPL
           MOVE 1                           TO W9503-CD-RSCO-OPRL
           MOVE SGTSX005-VL-CPR-OPR         TO W9503-VL-SCTR-OPR
           MOVE SGTSX005-CD-PRD             TO W9503-CD-PRD
           MOVE SGTSX005-CD-MDLD            TO W9503-CD-MDLD
           MOVE 103                         TO W9503-CD-IEC-MOE
           MOVE GDA-EST-OPR                 TO W9503-CD-EST-CDU-SCTR
           MOVE CORR GDA-DT-EST-CDU-R       TO DT-OPR-R
           MOVE DT-OPR                      TO W9503-DT-EST-CDU-SCTR

           MOVE CORR SGTSX005-DT-CPR-OPR-R  TO DT-OPR-R
           MOVE DT-OPR                      TO W9503-DT-EST-NEGL

           PERFORM  960018-CALCULA-EST-NEGL
           MOVE GDA-CD-EST-NEGL             TO W9503-CD-EST-NEGL

           MOVE 100                         TO W9503-PC-RSCO-BB-OPR
           MOVE CORR SGTSX005-DT-CPR-OPR-R  TO DT-OPR-R
           MOVE DT-OPR                      TO W9503-DT-FRMZ-SCTR
           MOVE CORR SGTSX005-DT-VNCT-OPR-R TO DT-OPR-R
           MOVE DT-OPR                      TO W9503-DT-VNCT-SCTR
      
           IF W9503-CD-TX-JUR-NM  EQUAL SEM-INCIDENCIA-10
              MOVE SEM-INCIDENCIA-10          TO W9503-CD-PERC-CPTZ
              MOVE SEM-INCIDENCIA-4           TO W9503-CD-TIP-TAXA-JUR
           ELSE
              MOVE AO-DIA                      TO W9503-CD-PERC-CPTZ
              MOVE EFETIVA                     TO W9503-CD-TIP-TAXA-JUR
           END-IF
      
           MOVE SEM-TRAVA                   TO W9503-CD-TIP-BLQ-INDP
                                            W9503-CD-DOC-BLQ-INDP
           MOVE 00010101                    TO W9503-DT-INC-VGC-BLQ
           MOVE 00010101                    TO W9503-DT-FIM-VGC-BLQ

      *   NO CASO DOS CAMPOS DA TAXONOMIA ESTAREM VAZIOS PREENCHER
      *   COMO ABAIXO: IGUAL SITUAÇÃO ANTERIOR
      *   Campo 130 (Carteira) . : 002 (Não Negociação)
      *   Campo 131 (Grupo) .... : 105 (Grupo Comercial TVM)
      *   Campo 132 (Livro) .... : 146 (Grupo Comercial TVM)
      *   Campo 133 (Sublivro) . : 264 (Títulos Privados)
      *
           IF SGTSX005-CD-CMCP EQUAL ZEROS
               MOVE 002                     TO  W9503-CD-TIP-CTRA-BC
           ELSE
               MOVE SGTSX005-CD-CMCP        TO  W9503-CD-TIP-CTRA-BC
           END-IF

           IF SGTSX005-CD-GR-MCP EQUAL ZEROS
               MOVE 105                     TO  W9503-CD-GR-BC
           ELSE
               MOVE SGTSX005-CD-GR-MCP      TO  W9503-CD-GR-BC
           END-IF

           IF SGTSX005-CD-LVRO-MC-CPTL EQUAL ZEROS
               MOVE 146                     TO  W9503-CD-LVRO-BC
           ELSE
               MOVE SGTSX005-CD-LVRO-MC-CPTL TO W9503-CD-LVRO-BC
           END-IF

           IF SGTSX005-CD-SMCP EQUAL ZEROS
               MOVE 264                     TO  W9503-CD-SUB-LVRO-BC
           ELSE
               MOVE SGTSX005-CD-SMCP        TO  W9503-CD-SUB-LVRO-BC
           END-IF

           MOVE SGTST137-PC-PDA-EPRO        TO  W9503-PC-PDA-EPRO

           MOVE 1                           TO  W9503-CD-TIP-OBJ
           MOVE 1                           TO  W9503-CD-INST-ITEM-CMZC
           MOVE K-ZERO                      TO  W9503-CD-ITEM-CMZC-MDLD
           MOVE 1                           TO  W9503-CD-INST-CRS-BCRO
           MOVE K-ZERO                      TO  W9503-CD-CRS-BCRO
           MOVE K-ZERO                      TO  W9503-CD-CM-AUX-NMLD
           MOVE K-ZERO                      TO  W9503-CD-CM-AUX-INDP
           MOVE 9                           TO  W9503-CD-FMA-CM-NMLD
           MOVE K-ZERO                      TO  W9503-PC-CM-AUX-NMLD
           MOVE 9                           TO  W9503-CD-FMA-CM-INDP
           MOVE K-ZERO                      TO  W9503-PC-CM-AUX-INDP
           MOVE K-ZERO                      TO  W9503-PC-TAXA-RBTE
           MOVE 'N'                         TO  W9503-IN-FNTO-IOF
           MOVE 99                          TO  W9503-CD-TIP-MIC-CRD
           MOVE 'N'                         TO  W9503-IN-ARRD-CML
           MOVE K-ZERO                      TO  W9503-PC-TIR
           MOVE K-ZERO                      TO  W9503-PC-TAXA-EFT
           MOVE K-ZERO                      TO  W9503-PC-TAXA-CSS
           MOVE K-ZERO                      TO  W9503-PC-TAXA-AQS

           MOVE CORR SGTS001A-DT-PSC-DRV-R(IC-TAB) TO DT-PSC-INVERTIDA-R
           MOVE DT-PSC-INVERTIDA            TO W9503-DT-EST-VNCT-SCTR

           MOVE GDA-CD-EST-VNCT-SCTR        TO  W9503-CD-EST-VNCT-SCTR
           MOVE CORR GDA-DT-PRMO-CRNG-R     TO  DT-OPR-R
           MOVE DT-OPR                      TO  W9503-DT-PRMO-CRNG-ABTO



      ***** OS CAMPOS ABAIXOS DEVERÃO SER INFORMADOS NA IMPLEMENTAÇÃO
      * DO SISTEMA CORPORATIVO
           MOVE K-ZERO                      TO  W9503-QT-MED-DD-PZ-SCTR
           MOVE K-ZERO                      TO  W9503-QT-PCL-VCLD-OPR
           MOVE K-ZERO                      TO  W9503-PC-CBT-GRT-FMAL
           MOVE K-ZERO                      TO  W9503-PC-CBT-GRT-IFMA
      
           PERFORM   970103-CHAMA-OPRS9503
      
           .
       960004-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       960005-INCLUIR-SDO-SUB-CTR-OPR  SECTION.
      * Aciona rotina do sistema OPR para incluir saldo do subcontrato
      * da operacao
      *-----------------------------------------------------------------
           MOVE 0031                   TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE   OPRS9524-DADOS
      
           MOVE  1                          TO W9524-FUNCAO
           MOVE  'SGTS001A'                 TO W9524-PRG-CHAMADA
           MOVE  'O'                        TO W9524-AMB-CHAMADA
      
           MOVE 'SGT'                       TO W9524-SG-SIS-RSP-CDU
           MOVE GDA-NR-UNCO-CTR-OPR         TO W9524-NR-UNCO-CTR-OPR
           MOVE 1                           TO W9524-NR-EPRD-FNCD
           MOVE 1                           TO W9524-NR-SCTR-OPR
           MOVE SGTST004-VL-SDO-TIT-DT-PSC  TO W9524-VL-SDO-SCTR
           MOVE SGTST004-VL-SDO-TIT-DT-PSC  TO W9524-VL-ATU-SCTR
           MOVE K-ZERO                      TO W9524-VL-ENCG-CPTZ-SCTR
           MOVE K-ZERO                      TO W9524-VL-REN-APPR-SCTR
           MOVE K-ZERO                      TO W9524-VL-TRND-PRJZ
           MOVE K-ZERO                      TO W9524-VL-CMB-MOEN
           MOVE K-ZERO                      TO W9524-VL-CMB-MOEE
           MOVE K-ZERO                      TO W9524-VL-SDO-MOEE
           MOVE GDA-VL-EM-ATRASO            TO W9524-VL-ATR-SCTR
           MOVE K-ZERO                      TO W9524-VL-CTR-UTZA
           MOVE GDA-CD-EST-VNCT-SCTR        TO W9524-CD-EST-VNCT-SCTR
           MOVE CORR SGTS001A-DT-PSC-DRV-R(IC-TAB) TO DT-PSC-INVERTIDA-R
           MOVE DT-PSC-INVERTIDA            TO W9524-DT-SDO-SCTR
                                               W9524-DT-VL-ATU-SCTR
                                               W9524-DT-EST-VNCT-SCTR
      
           MOVE CORR GDA-DT-PRMO-CRNG-R     TO DT-OPR-R
           MOVE DT-OPR                      TO W9524-DT-PRMO-CRNG-ABTO
      
           MOVE GDA-VL-SDO-CPTL             TO W9524-VL-SDO-CPTL
           MOVE GDA-VL-SDO-JUR              TO W9524-VL-SDO-JUR
           MOVE K-ZERO                      TO W9524-VL-SDO-CRC
           MOVE K-ZERO                      TO W9524-VL-SDO-ACSR
           MOVE K-ZERO                      TO W9524-VL-JUR-CPTZ-SCTR
           MOVE K-ZERO                      TO W9524-VL-CRC-CPTZ-SCTR
           MOVE K-ZERO                      TO W9524-VL-JUR-APPR-SCTR
           MOVE K-ZERO                      TO W9524-VL-CRC-APPR-SCTR
           MOVE K-ZERO                      TO W9524-VL-ACSR-APPR-SCTR
           MOVE K-ZERO                      TO W9524-DT-TRNS-PRJZ
           MOVE K-ZERO                      TO W9524-VL-SDO-SCTR-USD
           MOVE K-ZERO                      TO W9524-VL-REN-APPR-USD
           MOVE K-ZERO                      TO W9524-CD-TIP-SDO
      
           PERFORM   970105-CHAMA-OPRS9524
      
           .
       960005-SAI.
           EXIT.
      *-----------------------------------------------------------------
       960006-ALTERAR-CTR-OPR          SECTION.
      * Aciona rotina do sistema OPR para alterar contrato de operação
      *-----------------------------------------------------------------
           MOVE 0032                   TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE   OPRS9601-DADOS
      
           MOVE  1                          TO W9601-FUNCAO
           MOVE  'SGTS001A'                 TO W9601-PRG-CHAMADA
           MOVE  'O'                        TO W9601-AMB-CHAMADA
      
           IF SGTSX005-CD-UOR-DTTR-OPR   > ZEROS
      * BUSCA DEPENDENCIAS
              INITIALIZE   MSTS8042-DADOS
              MOVE  MSTS8042                   TO NM-PGM
              MOVE  SGTSX005-CD-UOR-DTTR-OPR   TO MSTS8042-CD-UOR
              PERFORM 980000-CHAMA-SERVICO
              MOVE MSTS8042-CD-DEPE-UOR        TO W9601-CD-PRF-DEPE-NEGR
           END-IF
      
           MOVE GDA-DEP-OPR                 TO W9601-CD-PRF-DEPE-EXEC

      *    Nova TAG para ANC descobrir grupo da Carteira
           EVALUATE SGTSX005-CD-CTRA-TIT-VL
           WHEN 300000
           WHEN 400000
           WHEN 500000
           WHEN 999999
               MOVE         9867           TO  W9601-CD-PRF-DEPE-CDU
           WHEN 300001
               MOVE         8595           TO  W9601-CD-PRF-DEPE-CDU
           WHEN 330
           WHEN 333
           WHEN 337
               MOVE         8875           TO  W9601-CD-PRF-DEPE-CDU
      *     WHEN 200000
      *         MOVE         8555           TO  W9601-CD-PRF-DEPE-CDU
           END-EVALUATE

           MOVE 'SGT'                       TO W9601-SG-SIS-RSP-I
           MOVE SGTSX005-NR-UNCO-CTR-OPR    TO W9601-NR-UNCO-CTR-OPR-I
                                               GDA-NR-UNCO-CTR-OPR
           MOVE SGTSX005-CD-OPR-ETRD-MCP    TO W9601-NR-CTR-OPR
           MOVE SGTSX005-CD-PRD             TO W9601-CD-PRD
           MOVE SGTSX005-CD-MDLD            TO W9601-CD-MDLD
           MOVE GDA-EST-OPR                 TO W9601-CD-EST-CDU
           MOVE CORR SGTSX005-DT-CPR-OPR-R  TO DT-OPR-R
           MOVE DT-OPR                      TO W9601-DT-FRMZ
           MOVE CORR SGTSX005-DT-VNCT-OPR-R TO DT-OPR-R
           MOVE DT-OPR                      TO W9601-DT-VNCT
           MOVE SGTSX005-VL-CPR-OPR         TO W9601-VL-OPR
           MOVE 103                         TO W9601-CD-IEC-MOE
      *    MOVE PORTE DO CLIENTE            TO W9601-CD-PRTE-CLI
           MOVE ZEROS                       TO W9601-CD-CNL-FRMZ-CTR
           MOVE CORR GDA-DT-EST-CDU-R       TO DT-OPR-R
           MOVE DT-OPR                      TO W9601-DT-EST-CDU
           MOVE GDA-DEP-OPR                 TO W9601-CD-PRF-DEPE-EXEC
           MOVE ZEROS                       TO W9601-CD-CIA-RSP-CTR

      *    MOVE CD-CHV-USU                  TO W9601-CD-MTC-USU
           MOVE 1                           TO  W9601-CD-IOR-RSP-CNSO
           MOVE 1                           TO  W9601-CD-IOR-OGM
           MOVE 1                           TO  W9601-CD-TIP-OBJ
           MOVE K-ZERO                      TO  W9601-CD-ITEM-CMZC-MDLD
           MOVE 1                           TO  W9601-CD-INST-CRS-BCRO
           MOVE K-ZERO                      TO  W9601-CD-CRS-BCRO
           MOVE 'CNV'                       TO  W9601-SG-SIS-OGM-CVN
           MOVE SGTSX005-CD-UOR-DTTR-OPR    TO  W9601-CD-UOR-DEPE-NEGR

      *    Nova TAG para ANC descobrir grupo da Carteira
           EVALUATE SGTSX005-CD-CTRA-TIT-VL
           WHEN 300000
           WHEN 400000
           WHEN 500000
           WHEN 999999
               MOVE         20237           TO  W9601-CD-UOR-DEPE-CDTR
           WHEN 300001
               MOVE         18951           TO  W9601-CD-UOR-DEPE-CDTR
           WHEN 330
           WHEN 333
           WHEN 337
               MOVE         19230           TO  W9601-CD-UOR-DEPE-CDTR
      *     WHEN 200000
      *         MOVE         18910           TO  W9601-CD-UOR-DEPE-CDTR
           END-EVALUATE

           MOVE K-ZERO                      TO  W9601-CD-TIP-PRTB-BCRO
           MOVE 00010101                    TO  W9601-DT-PRTB-BCRO
           MOVE K-ZERO                      TO  W9601-CD-ISPB-OGM-PRTB
           MOVE 1                           TO  W9601-CD-PAIS-NEGO
           MOVE OPRS9601                    TO  NM-PGM
           PERFORM   980000-CHAMA-SERVICO
           PERFORM   970106-TRATA-RET-OPRS9601
      
           .
       960006-SAI.
           EXIT.
      *-----------------------------------------------------------------
       960007-ALTERAR-PRTC-PSS-CTR     SECTION.
      * Aciona rotina do sistema OPR para alterar participacao da pessoa
      * no contrato da operação
      *-----------------------------------------------------------------
           MOVE 0033                   TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE   OPRS9604-DADOS
      
           MOVE  1                          TO W9604-FUNCAO
           MOVE  'SGTS001A'                 TO W9604-PRG-CHAMADA
           MOVE  'O'                        TO W9604-AMB-CHAMADA
      
           IF  SGTSX005-CD-UOR-BB-CRD-VL > ZEROS
      * BUSCA DEPENDENCIAS
               INITIALIZE   MSTS8042-DADOS
               MOVE  MSTS8042                   TO NM-PGM
               MOVE  SGTSX005-CD-UOR-BB-CRD-VL  TO MSTS8042-CD-UOR
               PERFORM 980000-CHAMA-SERVICO
               MOVE MSTS8042-CD-DEPE-UOR        TO W9604-CD-DEPE-CC-VCLD
           END-IF
      
           MOVE 'SGT'                       TO W9604-SG-SIS-RSP-I
           MOVE GDA-NR-UNCO-CTR-OPR         TO W9604-NR-UNCO-CTR-OPR-I
      
           MOVE GDA-CLI-EMISSOR             TO W9604-CD-PSS-CTR-OPR
           IF   GDA-CLI-EMISSOR    NOT EQUAL  GDA-CLI-IMPACTO-LC   AND
                GDA-CLI-IMPACTO-LC NOT EQUAL  ZEROS
                MOVE GDA-CLI-IMPACTO-LC     TO W9604-CD-PSS-CTR-OPR
           END-IF

           MOVE GDA-ULT-CLI-EMISSOR         TO W9604-CD-PSS-CTR-OPR-I
      
           MOVE SGTSX005-NR-CC-BB           TO W9604-NR-CC-VCLD
           MOVE CORR SGTSX005-DT-CPR-OPR-R  TO DT-OPR-R
           MOVE DT-OPR                      TO W9604-DT-INC-PRTC
           MOVE 1                           TO W9604-NR-SEQL-PRTC
                                               W9604-NR-SEQL-PRTC-I
           MOVE 1                           TO W9604-CD-TIP-PRTC
                                               W9604-CD-TIP-PRTC-I
           MOVE GDA-EST-OPR                 TO W9604-CD-EST-PRTC
           IF EST-OPR-ENCERRADA
              MOVE CORR SGTSX005-DT-LQDC-OPR-DRIV-R TO DT-OPR-R
              MOVE DT-OPR                      TO W9604-DT-FIM-PRTC
           ELSE
              MOVE 99991231                    TO W9604-DT-FIM-PRTC
           END-IF
           MOVE 001                         TO W9604-CD-PAIS
           MOVE SPACES                      TO W9604-NM-PSS-PSZD
           MOVE OPRS9604                    TO  NM-PGM
           PERFORM   980000-CHAMA-SERVICO
           PERFORM   970109-TRATA-RET-OPRS9604
      
           .
       960007-SAI.
           EXIT.
      *-----------------------------------------------------------------
       960008-ALTERAR-EPRD-FNCD        SECTION.
      * Aciona rotina do sistema OPR para alterar empreendimento financi
      * ado no contrato da operação
      *-----------------------------------------------------------------
           MOVE 0034                   TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE   OPRS9602-DADOS
      
           MOVE  1                          TO W9602-FUNCAO
           MOVE  'SGTS001A'                 TO W9602-PRG-CHAMADA
           MOVE  'O'                        TO W9602-AMB-CHAMADA
      
           MOVE 'SGT'                       TO W9602-SG-SIS-RSP
           MOVE GDA-NR-UNCO-CTR-OPR         TO W9602-NR-UNCO-CTR-OPR
           MOVE 1                           TO W9602-NR-EPRD-FNCD
           MOVE NEGOCIACAO-TITULO           TO W9602-CD-ITEM-FNCD
           MOVE ZEROS                       TO W9602-NR-SEQL-EPRD
           MOVE 1                           TO W9602-QT-EPRD-FNCD
           MOVE 1                           TO W9602-QT-ITEM-FNCD
           MOVE SGTSX005-VL-CPR-OPR         TO W9602-VL-EPRD-FNCD
           MOVE GDA-EST-OPR                 TO W9602-CD-EST-CDU-EPRD
           MOVE CORR GDA-DT-EST-CDU-R       TO DT-OPR-R
           MOVE DT-OPR                      TO W9602-DT-EST-CDU-EPRD
      
      
           PERFORM   970107-CHAMA-OPRS9602
      
           .
       960008-SAI.
           EXIT.
      *-----------------------------------------------------------------
       960009-ALTERAR-SUB-CTR-OPR      SECTION.
      * Aciona rotina do sistema OPR para alterar subcontrato no
      * contrato da operação
      *-----------------------------------------------------------------
           MOVE 0035                   TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE   OPRS9603-DADOS
      
           MOVE  1                          TO W9603-FUNCAO
           MOVE  'SGTS001A'                 TO W9603-PRG-CHAMADA
           MOVE  'O'                        TO W9603-AMB-CHAMADA
      
           MOVE GDA-NR-UNCO-CTR-OPR         TO W9603-NR-UNCO-CTR-OPR
           MOVE 'SGT'                       TO W9603-SG-SIS-RSP-CDU
           MOVE 1                           TO W9603-NR-EPRD-FNCD
           MOVE 1                           TO W9603-NR-SCTR-OPR
           MOVE TITULOS                     TO W9603-CD-DTNC-CRD
      
           IF SGTST011-CD-IEC-RMNC = TAXA-FIXA
              MOVE PREFIXADO                   TO W9603-CD-IEC-CM
              MOVE 100                         TO W9603-PC-APL-CM
              MOVE AO-ANO                      TO W9603-CD-TX-JUR-NM
              MOVE SGTST011-PC-TAXA-RMNC-OPR   TO W9603-PC-TX-JUR-NM
           ELSE
              MOVE SGTST011-CD-IEC-RMNC    TO W9603-CD-IEC-CM
              IF SGTST011-CD-TIP-FMA-APL = PORCENTAGEM
                 MOVE SGTST011-PC-TAXA-RMNC-OPR   TO W9603-PC-APL-CM
                 MOVE SEM-INCIDENCIA-10           TO W9603-CD-TX-JUR-NM
                 MOVE ZEROS                       TO W9603-PC-TX-JUR-NM
              ELSE
                 MOVE 100                         TO W9603-PC-APL-CM
                 MOVE AO-ANO                      TO W9603-CD-TX-JUR-NM
                 MOVE SGTST011-PC-TAXA-RMNC-OPR   TO W9603-PC-TX-JUR-NM
              END-IF
           END-IF
      
           MOVE PREFIXADO                   TO W9603-CD-IEC-CM-IN
           MOVE 100                         TO W9603-PC-APL-CM-IN
           MOVE 1                           TO W9603-CD-EST-CTB
           MOVE CORR SGTSX005-DT-CPR-OPR-R  TO DT-OPR-R
           MOVE DT-OPR                      TO W9603-DT-EST-CTB
           MOVE 10                          TO W9603-CD-TX-JUR-IN
      *    MOVE ZEROS                       TO W9603-PC-TX-JUR-IN
           MOVE ZEROS                       TO W9603-PC-MULT-INDP
           MOVE PREFIXADO                   TO W9603-CD-CM-IN-ESP
           MOVE 100                         TO W9603-PC-CM-IN-ESP
           MOVE SEM-TRAVA                   TO W9603-CD-EST-ESPL
           MOVE 1                           TO W9603-CD-RSCO-OPRL
           MOVE SGTSX005-VL-CPR-OPR         TO W9603-VL-SCTR-OPR
           MOVE SGTSX005-CD-PRD             TO W9603-CD-PRD
           MOVE SGTSX005-CD-MDLD            TO W9603-CD-MDLD
           MOVE 103                         TO W9603-CD-IEC-MOE
           MOVE GDA-EST-OPR                 TO W9603-CD-EST-CDU-SCTR
           MOVE CORR GDA-DT-EST-CDU-R       TO DT-OPR-R
           MOVE DT-OPR                      TO W9603-DT-EST-CDU-SCTR

           MOVE CORR SGTSX005-DT-CPR-OPR-R  TO DT-OPR-R
           MOVE DT-OPR                      TO W9603-DT-EST-NEGL

           PERFORM  960018-CALCULA-EST-NEGL
           MOVE GDA-CD-EST-NEGL             TO W9603-CD-EST-NEGL

           MOVE 100                         TO W9603-PC-RSCO-BB-OPR
           MOVE CORR SGTSX005-DT-CPR-OPR-R  TO DT-OPR-R
           MOVE DT-OPR                      TO W9603-DT-FRMZ-SCTR
           MOVE CORR SGTSX005-DT-VNCT-OPR-R TO DT-OPR-R
           MOVE DT-OPR                      TO W9603-DT-VNCT-SCTR
      
           IF W9603-CD-TX-JUR-NM  EQUAL SEM-INCIDENCIA-10
              MOVE SEM-INCIDENCIA-10          TO W9603-CD-PERC-CPTZ
              MOVE SEM-INCIDENCIA-4           TO W9603-CD-TIP-TAXA-JUR
           ELSE
              MOVE AO-DIA                      TO W9603-CD-PERC-CPTZ
              MOVE EFETIVA                     TO W9603-CD-TIP-TAXA-JUR
           END-IF
      
           MOVE SEM-TRAVA                   TO W9603-CD-TIP-BLQ-INDP
                                            W9603-CD-DOC-BLQ-INDP
           MOVE 00010101                    TO W9603-DT-INC-VGC-BLQ
           MOVE 00010101                    TO W9603-DT-FIM-VGC-BLQ

      *   NO CASO DOS CAMPOS DA TAXONOMIA ESTAREM VAZIOS PREENCHER
      *   COMO ABAIXO: IGUAL SITUAÇÃO ANTERIOR
      *   Campo 130 (Carteira) . : 002 (Não Negociação)
      *   Campo 131 (Grupo) .... : 105 (Grupo Comercial TVM)
      *   Campo 132 (Livro) .... : 146 (Grupo Comercial TVM)
      *   Campo 133 (Sublivro) . : 264 (Títulos Privados)
      *
           IF SGTSX005-CD-CMCP EQUAL ZEROS
               MOVE 002                      TO W9603-CD-TIP-CTRA-BC
           ELSE
               MOVE SGTSX005-CD-CMCP         TO W9603-CD-TIP-CTRA-BC
           END-IF

           IF SGTSX005-CD-GR-MCP EQUAL ZEROS
               MOVE 105                      TO W9603-CD-GR-BC
           ELSE
               MOVE SGTSX005-CD-GR-MCP       TO W9603-CD-GR-BC
           END-IF

           IF SGTSX005-CD-LVRO-MC-CPTL EQUAL ZEROS
               MOVE 146                      TO W9603-CD-LVRO-BC
           ELSE
               MOVE SGTSX005-CD-LVRO-MC-CPTL TO W9603-CD-LVRO-BC
           END-IF

           IF SGTSX005-CD-SMCP EQUAL ZEROS
               MOVE 264                      TO W9603-CD-SUB-LVRO-BC
           ELSE
               MOVE SGTSX005-CD-SMCP         TO W9603-CD-SUB-LVRO-BC
           END-IF

      *    MOVE CD-CHV-USU                  TO W9603-CD-MTC-USU

           MOVE SGTST137-PC-PDA-EPRO        TO  W9603-PC-PDA-EPRO

           MOVE 1                           TO  W9603-CD-TIP-OBJ
           MOVE 1                           TO  W9603-CD-INST-ITEM-CMZC
           MOVE K-ZERO                      TO  W9603-CD-ITEM-CMZC-MDLD
           MOVE 1                           TO  W9603-CD-INST-CRS-BCRO
           MOVE K-ZERO                      TO  W9603-CD-CRS-BCRO
           MOVE K-ZERO                      TO  W9603-CD-CM-AUX-NMLD
           MOVE K-ZERO                      TO  W9603-CD-CM-AUX-INDP
           MOVE 9                           TO  W9603-CD-FMA-CM-NMLD
           MOVE K-ZERO                      TO  W9603-PC-CM-AUX-NMLD
           MOVE 9                           TO  W9603-CD-FMA-CM-INDP
           MOVE K-ZERO                      TO  W9603-PC-CM-AUX-INDP
           MOVE K-ZERO                      TO  W9603-PC-TAXA-RBTE
           MOVE 'N'                         TO  W9603-IN-FNTO-IOF
           MOVE 99                          TO  W9603-CD-TIP-MIC-CRD
           MOVE 'N'                         TO  W9603-IN-ARRD-CML
           MOVE K-ZERO                      TO  W9603-PC-TIR
           MOVE K-ZERO                      TO  W9603-PC-TAXA-EFT
           MOVE K-ZERO                      TO  W9603-PC-TAXA-CSS
           MOVE K-ZERO                      TO  W9603-PC-TAXA-AQS

           MOVE GDA-CD-EST-VNCT-SCTR        TO  W9603-CD-EST-VNCT-SCTR
           MOVE CORR GDA-DT-PRMO-CRNG-R     TO  DT-OPR-R
           MOVE DT-OPR                      TO  W9603-DT-PRMO-CRNG-ABTO

           MOVE CORR SGTS001A-DT-PSC-DRV-R(IC-TAB) TO DT-PSC-INVERTIDA-R
           MOVE DT-PSC-INVERTIDA            TO W9603-DT-EST-VNCT-SCTR


      ***** OS CAMPOS ABAIXOS DEVERÃO SER INFORMADOS NA IMPLEMENTAÇÃO
      * DO SISTEMA CORPORATIVO
           MOVE K-ZERO                      TO  W9603-QT-MED-DD-PZ-SCTR
           MOVE K-ZERO                      TO  W9603-QT-PCL-VCLD-OPR
           MOVE K-ZERO                      TO  W9603-PC-CBT-GRT-FMAL
           MOVE K-ZERO                      TO  W9603-PC-CBT-GRT-IFMA

           PERFORM   970108-CHAMA-OPRS9603
      
           .
       960009-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       960010-ALTERAR-SDO-SUB-CTR-OPR  SECTION.
      * Aciona rotina do sistema OPR para alterar saldo do subcontrato
      * da operacao
      *-----------------------------------------------------------------
           MOVE 0036                   TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE   OPRS9624-DADOS
      
           IF EST-OPR-ENCERRADA
              MOVE  2                          TO W9624-FUNCAO
           ELSE
              MOVE  1                          TO W9624-FUNCAO
           END-IF
           MOVE  'SGTS001A'                 TO W9624-PRG-CHAMADA
           MOVE  'O'                        TO W9624-AMB-CHAMADA
      
           MOVE 'SGT'                       TO W9624-SG-SIS-RSP-CDU
           MOVE GDA-NR-UNCO-CTR-OPR         TO W9624-NR-UNCO-CTR-OPR
           MOVE 1                           TO W9624-NR-EPRD-FNCD
           MOVE 1                           TO W9624-NR-SCTR-OPR
           MOVE SGTST004-VL-SDO-TIT-DT-PSC  TO W9624-VL-SDO-SCTR
           MOVE SGTST004-VL-SDO-TIT-DT-PSC  TO W9624-VL-ATU-SCTR
           MOVE K-ZERO                      TO W9624-VL-ENCG-CPTZ-SCTR
           MOVE K-ZERO                      TO W9624-VL-REN-APPR-SCTR
           MOVE K-ZERO                      TO W9624-VL-TRND-PRJZ
           MOVE K-ZERO                      TO W9624-VL-CMB-MOEN
           MOVE K-ZERO                      TO W9624-VL-CMB-MOEE
           MOVE K-ZERO                      TO W9624-VL-SDO-MOEE
           MOVE GDA-VL-EM-ATRASO            TO W9624-VL-ATR-SCTR
           MOVE K-ZERO                      TO W9624-VL-CTR-UTZA
           MOVE GDA-CD-EST-VNCT-SCTR        TO W9624-CD-EST-VNCT-SCTR
           MOVE CORR SGTS001A-DT-PSC-DRV-R(IC-TAB) TO DT-PSC-INVERTIDA-R
           MOVE DT-PSC-INVERTIDA            TO W9624-DT-SDO-SCTR
                                               W9624-DT-VL-ATU-SCTR
                                               W9624-DT-EST-VNCT-SCTR
      
           MOVE CORR GDA-DT-PRMO-CRNG-R     TO DT-OPR-R
           MOVE DT-OPR                      TO W9624-DT-PRMO-CRNG-ABTO
      
           MOVE GDA-VL-SDO-CPTL             TO W9624-VL-SDO-CPTL
           MOVE GDA-VL-SDO-JUR              TO W9624-VL-SDO-JUR
           MOVE K-ZERO                      TO W9624-VL-SDO-CRC
           MOVE K-ZERO                      TO W9624-VL-SDO-ACSR
           MOVE K-ZERO                      TO W9624-VL-JUR-CPTZ-SCTR
           MOVE K-ZERO                      TO W9624-VL-CRC-CPTZ-SCTR
           MOVE K-ZERO                      TO W9624-VL-JUR-APPR-SCTR
           MOVE K-ZERO                      TO W9624-VL-CRC-APPR-SCTR
           MOVE K-ZERO                      TO W9624-VL-ACSR-APPR-SCTR
           MOVE K-ZERO                      TO W9624-DT-TRNS-PRJZ
           MOVE K-ZERO                      TO W9624-VL-SDO-SCTR-USD
           MOVE K-ZERO                      TO W9624-VL-REN-APPR-USD
           MOVE K-ZERO                      TO W9624-CD-TIP-SDO
      
           PERFORM   970110-CHAMA-OPRS9624
      
           .
       960010-SAI.
           EXIT.

      *-----------------------------------------------------------------
       960011-INCLUIR-SDO-CTR-APL-TSR  SECTION.
      * Aciona rotina do sistema OPR para incluir saldo do subcontrato
      * da operacao de aplicação de tesouraria
      *-----------------------------------------------------------------
           MOVE 0037                   TO MSG1-CD-SEQL-ERRO
      
           PERFORM 970124-BUSCA-TS-DT

           MOVE CORR TS-FMTD                TO DT-AAAAMMDD-R
      
           INITIALIZE   OPRS9804-DADOS

           MOVE  1                          TO W9804-FUNCAO
           MOVE  'SGTS001A'                 TO W9804-PRG-CHAMADA
           MOVE  'O'                        TO W9804-AMB-CHAMADA
      
           MOVE 'SGT'                       TO W9804-SG-SIS-RSP-CDU
           MOVE GDA-NR-UNCO-CTR-OPR         TO W9804-NR-UNCO-CTR-OPR
           MOVE 1                           TO W9804-NR-EPRD-FNCD
           MOVE 1                           TO W9804-NR-SCTR-OPR
           MOVE 1                           TO W9804-CD-TIP-VL-CPNT
           MOVE 1                           TO W9804-NR-CMPS-SDO-OPR

           MOVE '  .  .    '                TO W9804-DT-PSC-REF-SDO
           MOVE '  .  .    '                TO W9804-DT-ENVO-SDO

           MOVE CORR SGTS001A-DT-PSC-DRV-R(IC-TAB)
                              TO W9804-DT-PSC-REF-SDO-R
           MOVE CORR DT-AAAAMMDD-R          TO W9804-DT-ENVO-SDO-R
           MOVE 103                         TO W9804-CD-MOE-CPNT
           MOVE SGTST004-VL-SDO-TIT-DT-PSC  TO W9804-VL-CPNT-SDO-SCTR
           MOVE TS-FMTD                     TO W9804-TS-ALT-REG
           MOVE SGTSX005-CD-PRD             TO W9804-CD-PRD
           MOVE SGTSX005-CD-MDLD            TO W9804-CD-MDLD
           MOVE 'SGT'                       TO W9804-SG-SIS-OGM-SDO
           MOVE 'N'                         TO W9804-IN-FCHT-SDO-MM
           MOVE '31.12.9999'                TO W9804-DT-FCHT-SDO-MM
           MOVE 1                           TO W9804-CD-TIP-SDO

           PERFORM   970122-CHAMA-OPRS9804

           .
       960011-SAI.
           EXIT.


      *-----------------------------------------------------------------
       960012-ALTERAR-SDO-CTR-APL-TSR  SECTION.
      * Aciona rotina do sistema OPR para alterar saldo do subcontrato
      * da operacao de aplicação de tesouraria
      *-----------------------------------------------------------------
           MOVE 0038                   TO MSG1-CD-SEQL-ERRO
      
           PERFORM 970124-BUSCA-TS-DT

           MOVE CORR TS-FMTD               TO DT-AAAAMMDD-R
      
           INITIALIZE   OPRS9804-DADOS

           MOVE  1                          TO W9904-FUNCAO
           MOVE  'SGTS001A'                 TO W9904-PRG-CHAMADA
           MOVE  'O'                        TO W9904-AMB-CHAMADA
      
           MOVE 'SGT'                       TO W9904-SG-SIS-RSP-CDU
           MOVE GDA-NR-UNCO-CTR-OPR         TO W9904-NR-UNCO-CTR-OPR
           MOVE 1                           TO W9904-NR-EPRD-FNCD
           MOVE 1                           TO W9904-NR-SCTR-OPR
           MOVE 1                           TO W9904-CD-TIP-VL-CPNT
           MOVE 1                           TO W9904-NR-CMPS-SDO-OPR

           MOVE '  .  .    '                TO W9904-DT-PSC-REF-SDO
           MOVE '  .  .    '                TO W9904-DT-ENVO-SDO

           MOVE CORR SGTS001A-DT-PSC-DRV-R(IC-TAB)
                                            TO W9904-DT-PSC-REF-SDO-R
           MOVE CORR DT-AAAAMMDD-R          TO W9904-DT-ENVO-SDO-R
           MOVE 103                         TO W9904-CD-MOE-CPNT
           MOVE SGTST004-VL-SDO-TIT-DT-PSC  TO W9904-VL-CPNT-SDO-SCTR
           MOVE TS-FMTD                     TO W9904-TS-ALT-REG
           MOVE SGTSX005-CD-PRD             TO W9904-CD-PRD
           MOVE SGTSX005-CD-MDLD            TO W9904-CD-MDLD
           MOVE 'SGT'                       TO W9904-SG-SIS-OGM-SDO
           MOVE 'N'                         TO W9904-IN-FCHT-SDO-MM
           MOVE '31.12.9999'                TO W9904-DT-FCHT-SDO-MM
           MOVE 1                           TO W9904-CD-TIP-SDO

           PERFORM   970123-CHAMA-OPRS9904

           .
       960012-SAI.
           EXIT.

      *-----------------------------------------------------------------
       960013-INCLUIR-PRTC-PSS         SECTION.
      * Aciona rotina do sistema OPR para incluir participacao da pessoa
      * contrato
      *-----------------------------------------------------------------
           MOVE 0063                        TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE   OPRS9504-DADOS
      
           MOVE  1                          TO W9504-FUNCAO
           MOVE  'SGTS001A'                 TO W9504-PRG-CHAMADA
           MOVE  'O'                        TO W9504-AMB-CHAMADA
           MOVE 'SGT'                       TO W9504-SG-SIS-RSP
           MOVE GDA-NR-UNCO-CTR-OPR         TO W9504-NR-UNCO-CTR-OPR
           MOVE GDA-CLI-EMISSOR             TO W9504-CD-PSS-CTR-OPR
           IF   GDA-CLI-EMISSOR    NOT EQUAL  GDA-CLI-IMPACTO-LC   AND
                GDA-CLI-IMPACTO-LC NOT EQUAL  ZEROS
               MOVE GDA-CLI-IMPACTO-LC      TO W9504-CD-PSS-CTR-OPR
           END-IF
           IF  SGTSX005-CD-UOR-BB-CRD-VL > ZEROS
      * BUSCA DEPENDENCIA
               INITIALIZE   MSTS8042-DADOS
               MOVE  MSTS8042                   TO NM-PGM
               MOVE  SGTSX005-CD-UOR-BB-CRD-VL  TO MSTS8042-CD-UOR
               PERFORM 980000-CHAMA-SERVICO
               MOVE MSTS8042-CD-DEPE-UOR    TO W9504-CD-DEPE-CC-VCLD
           END-IF

           MOVE SGTSX005-NR-CC-BB           TO W9504-NR-CC-VCLD
           MOVE CORR SGTSX005-DT-CPR-OPR-R  TO DT-OPR-R
           MOVE DT-OPR                      TO W9504-DT-INC-PRTC
           MOVE 1                           TO W9504-NR-SEQL-PRTC
           MOVE 1                           TO W9504-CD-TIP-PRTC
           MOVE GDA-EST-OPR                 TO W9504-CD-EST-PRTC
           IF EST-OPR-ENCERRADA
              MOVE SGTSX005-DT-LQDC-OPR-DRIV-R TO DT-OPR-R
              MOVE DT-OPR                      TO W9504-DT-FIM-PRTC
           ELSE
              MOVE 99991231                    TO W9504-DT-FIM-PRTC
           END-IF
           MOVE 001                         TO W9504-CD-PAIS
           MOVE SPACES                      TO W9504-NM-PSS-PSZD

           PERFORM   970104-CHAMA-OPRS9504
      
           .
       960013-SAI.
           EXIT.

      *-----------------------------------------------------------------
       960014-INCLUIR-TJE      SECTION.
      * Aciona rotina do sistema OPR para incluir TJE - Taxa de Juros
      * Efetiva
      *-----------------------------------------------------------------
           MOVE       0065                  TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE   OPRS9532-DADOS
      
           MOVE  'SGTS001A'                 TO K9532-PRG-CHAMADA
           MOVE  'O'                        TO K9532-AMB-CHAMADA
      
           MOVE GDA-NR-UNCO-CTR-OPR         TO K9532-NR-UNCO-CTR-OPR
           MOVE         1                   TO K9532-NR-EPRD-FNCD
           MOVE         1                   TO K9532-NR-SCTR
           MOVE        31                   TO K9532-CD-TIP-TAXA
      *    Por enquanto inibir esta consistência
      *     IF CACS045V-VL-TAXA-JUR-EFT  GREATER  PC-TAXA-AUX
      *        SET ERRO-PROCESSAMENTO        TO  TRUE
      *        MOVE     999                  TO  CD-RTN-OPR
      *        MOVE 'Taxa com mais de 3 digitos inteiros'
      *                                      TO  MSG-RTN-OPR
      *        MOVE     OPRS9532             TO  NM-PGM-ERRO
      *        PERFORM    970111-TRATA-ERRO-OPR
      *     ELSE
              MOVE CACS045V-VL-TAXA-JUR-EFT TO  K9532-PC-TAXA

              MOVE        12                TO  K9532-CD-FMA-TAXA
              MOVE SGTST011-CD-IEC-RMNC     TO  K9532-CD-IEC-TAXA
              IF SGTST011-CD-IEC-RMNC  EQUAL  1313
                 MOVE 100                   TO  K9532-PC-IEC-TAXA
              ELSE
      *=======  Percentual
                 IF SGTST011-CD-TIP-FMA-APL = 1
                    MOVE SGTST011-PC-TAXA-RMNC-OPR  TO K9532-PC-IEC-TAXA
                 END-IF
      *=======  Spread
                 IF SGTST011-CD-TIP-FMA-APL = 2
                    MOVE           100      TO  K9532-PC-IEC-TAXA
                 END-IF
              END-IF

              MOVE        ZEROS             TO  K9532-NR-CRV-FTR-RSCO
              MOVE         2                TO  K9532-CD-EST-TAXA

              MOVE   CORR SGTS001A-DT-PSC-DRV-R(IC-TAB)
                                            TO  DT-OPR-R
              MOVE      DT-OPR              TO  K9532-DT-EST-TAXA
              MOVE         1                TO  K9532-CD-TIP-TAXA-JUR
              MOVE       'SGT'              TO  K9532-SG-SIS

              PERFORM   970114-CHAMA-OPRS9532
      *     END-IF
           .
       960014-SAI.
           EXIT.
      *-----------------------------------------------------------------
       960015-ALTERAR-TJE      SECTION.
      * Aciona rotina do sistema OPR para alterar TJE - Taxa de Juros
      * Efetiva
      *-----------------------------------------------------------------
           MOVE       0069                  TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE   OPRS9632-DADOS
      
           MOVE  'SGTS001A'                 TO K9632-PRG-CHAMADA
           MOVE  'O'                        TO K9632-AMB-CHAMADA
      
           MOVE GDA-NR-UNCO-CTR-OPR         TO K9632-NR-UNCO-CTR-OPR
           MOVE         1                   TO K9632-NR-EPRD-FNCD
           MOVE         1                   TO K9632-NR-SCTR
           MOVE        31                   TO K9632-CD-TIP-TAXA
      *    Por enquanto inibir esta consistência
      *     IF CACS045V-VL-TAXA-JUR-EFT  GREATER  PC-TAXA-AUX
      *        SET ERRO-PROCESSAMENTO        TO  TRUE
      *        MOVE     999                  TO  CD-RTN-OPR
      *        MOVE 'Taxa com mais de 3 digitos inteiros'
      *                                      TO  MSG-RTN-OPR
      *        MOVE     OPRS9632             TO  NM-PGM-ERRO
      *        PERFORM    970111-TRATA-ERRO-OPR
      *     ELSE
              MOVE CACS045V-VL-TAXA-JUR-EFT TO  K9632-PC-TAXA

              MOVE        12                TO  K9632-CD-FMA-TAXA
              MOVE SGTST011-CD-IEC-RMNC     TO  K9632-CD-IEC-TAXA
              IF SGTST011-CD-IEC-RMNC  EQUAL  1313
                 MOVE 100                   TO  K9632-PC-IEC-TAXA
              ELSE
      *=======  Percentual
                 IF SGTST011-CD-TIP-FMA-APL = 1
                    MOVE SGTST011-PC-TAXA-RMNC-OPR  TO K9632-PC-IEC-TAXA
                 END-IF
      *=======  Spread
                 IF SGTST011-CD-TIP-FMA-APL = 2
                    MOVE           100      TO  K9632-PC-IEC-TAXA
                 END-IF
              END-IF

              MOVE        ZEROS             TO  K9632-NR-CRV-FTR-RSCO
              MOVE         2                TO  K9632-CD-EST-TAXA

              MOVE   CORR SGTS001A-DT-PSC-DRV-R(IC-TAB)
                                            TO  DT-OPR-R
              MOVE      DT-OPR              TO  K9632-DT-EST-TAXA
              MOVE         1                TO  K9632-CD-TIP-TAXA-JUR
              MOVE       'SGT'              TO  K9632-SG-SIS

              PERFORM   970121-CHAMA-OPRS9632
      *     END-IF
           .
       960015-SAI.
           EXIT.
      *-----------------------------------------------------------------
       960016-INCLUIR-TAXO      SECTION.
      * Aciona rotina do sistema OPR para incluir Taxonomia e Classifi-
      * cação Contábil
      *-----------------------------------------------------------------
           MOVE       0071                  TO  MSG1-CD-SEQL-ERRO
      
           INITIALIZE   OPRS033I-DADOS
      
           MOVE  'SGTS001A'                 TO  OPRS033I-PRG-CHAMADA

           MOVE GDA-NR-UNCO-CTR-OPR         TO  OPRS033I-NR-UNCO-CTR-OPR
           MOVE         1                   TO  OPRS033I-NR-EPRD
           MOVE         1                   TO  OPRS033I-NR-SCTR
      *   NO CASO DOS CAMPOS DA TAXONOMIA ESTAREM VAZIOS PREENCHER
      *   COMO ABAIXO: IGUAL SITUAÇÃO ANTERIOR
      *   Campo 130 (Carteira) . : 002 (Não Negociação)
      *   Campo 131 (Grupo) .... : 105 (Grupo Comercial TVM)
      *   Campo 132 (Livro) .... : 146 (Grupo Comercial TVM)
      *   Campo 133 (Sublivro) . : 264 (Títulos Privados)
      *
           IF SGTSX005-CD-CMCP EQUAL ZEROS
               MOVE 002                     TO OPRS033I-CD-CMCP-OPR
           ELSE
               MOVE SGTSX005-CD-CMCP        TO OPRS033I-CD-CMCP-OPR
           END-IF

           IF SGTSX005-CD-GR-MCP EQUAL ZEROS
               MOVE 105                     TO OPRS033I-CD-GR-MCP-OPR
           ELSE
               MOVE SGTSX005-CD-GR-MCP      TO OPRS033I-CD-GR-MCP-OPR
           END-IF

           IF SGTSX005-CD-LVRO-MC-CPTL EQUAL ZEROS
               MOVE 146                      TO OPRS033I-CD-LMCP-OPR
           ELSE
               MOVE SGTSX005-CD-LVRO-MC-CPTL TO OPRS033I-CD-LMCP-OPR
           END-IF

           IF SGTSX005-CD-SMCP EQUAL ZEROS
               MOVE 264                     TO OPRS033I-CD-SMCP
           ELSE
               MOVE SGTSX005-CD-SMCP        TO OPRS033I-CD-SMCP
           END-IF

           EVALUATE SGTSX005-CD-CTGR-TVM
           WHEN 1
           WHEN 21
              MOVE          1               TO OPRS033I-CD-TIP-CLSC-CTB
           WHEN 2
           WHEN 22
              MOVE          2               TO OPRS033I-CD-TIP-CLSC-CTB
           WHEN 3
           WHEN 23
              MOVE          3               TO OPRS033I-CD-TIP-CLSC-CTB
           END-EVALUATE

           MOVE        'SGT'                TO OPRS033I-SG-SIS

           PERFORM   970125-CHAMA-OPRS033I
           .
       960016-SAI.
           EXIT.
      *-----------------------------------------------------------------
       960017-ALTERAR-TAXO      SECTION.
      ** Aciona rotina do sistema OPR para alterar Taxonomia e Classifi-
      * cação Contábil
      *-----------------------------------------------------------------
           MOVE       0072                  TO  MSG1-CD-SEQL-ERRO
      
           INITIALIZE   OPRS033U-DADOS
      
           MOVE  'SGTS001A'                 TO  OPRS033U-PRG-CHAMADA

           MOVE GDA-NR-UNCO-CTR-OPR         TO  OPRS033U-NR-UNCO-CTR-OPR
           MOVE         1                   TO  OPRS033U-NR-EPRD
           MOVE         1                   TO  OPRS033U-NR-SCTR
      *   NO CASO DOS CAMPOS DA TAXONOMIA ESTAREM VAZIOS PREENCHER
      *   COMO ABAIXO: IGUAL SITUAÇÃO ANTERIOR
      *   Campo 130 (Carteira) . : 002 (Não Negociação)
      *   Campo 131 (Grupo) .... : 105 (Grupo Comercial TVM)
      *   Campo 132 (Livro) .... : 146 (Grupo Comercial TVM)
      *   Campo 133 (Sublivro) . : 264 (Títulos Privados)
      *
           IF SGTSX005-CD-CMCP EQUAL ZEROS
               MOVE 002                     TO OPRS033U-CD-CMCP-OPR
           ELSE
               MOVE SGTSX005-CD-CMCP        TO OPRS033U-CD-CMCP-OPR
           END-IF

           IF SGTSX005-CD-GR-MCP EQUAL ZEROS
               MOVE 105                     TO OPRS033U-CD-GR-MCP-OPR
           ELSE
               MOVE SGTSX005-CD-GR-MCP      TO OPRS033U-CD-GR-MCP-OPR
           END-IF

           IF SGTSX005-CD-LVRO-MC-CPTL EQUAL ZEROS
               MOVE 146                      TO OPRS033U-CD-LMCP-OPR
           ELSE
               MOVE SGTSX005-CD-LVRO-MC-CPTL TO OPRS033U-CD-LMCP-OPR
           END-IF

           IF SGTSX005-CD-SMCP EQUAL ZEROS
               MOVE 264                     TO OPRS033U-CD-SMCP
           ELSE
               MOVE SGTSX005-CD-SMCP        TO OPRS033U-CD-SMCP
           END-IF

           EVALUATE SGTSX005-CD-CTGR-TVM
           WHEN 1
           WHEN 21
              MOVE          1               TO OPRS033U-CD-TIP-CLSC-CTB
           WHEN 2
           WHEN 22
              MOVE          2               TO OPRS033U-CD-TIP-CLSC-CTB
           WHEN 3
           WHEN 23
              MOVE          3               TO OPRS033U-CD-TIP-CLSC-CTB
           END-EVALUATE

           PERFORM   970126-CHAMA-OPRS033U
           .
       960017-SAI.
           EXIT.
      *-----------------------------------------------------------------
       960018-CALCULA-EST-NEGL      SECTION.
      * Calcula o Estado negocial a partir do Tipo de Versionamento
      *-----------------------------------------------------------------
           MOVE         0075                TO  MSG1-CD-SEQL-ERRO

      *    Pesquisa o último Estado Negocial válido para tipo de versio-
      *    namento 3 e 4 e o Motivo do Estado Negocial para todos
           MOVE SGTSX005-CD-CTRA-TIT-VL     TO SGTSPE25-CD-CTRA-TIT-VL
           MOVE SGTSX005-CD-OPR-ETRD-MCP    TO SGTSPE25-CD-OPR-ETRD-MCP
           MOVE SGTSX005-NR-VRS-OPR-ETRD    TO SGTSPE25-NR-VRS-OPR-ETRD
           PERFORM  970127-CHAMA-SGTSPE25
           MOVE  SGTSPE25-CD-MTV-EST-NEGL   TO GDA-CD-MTV-EST-NEGL

           EVALUATE  SGTSX005-CD-TIP-VRS-OPR
      *   >> Se Estado Negocial 1-Normalidade     2-Renegociação
      *                         5-Reestruturação
           WHEN  1
               MOVE         256             TO  GDA-CD-EST-NEGL
           WHEN  2
               MOVE          1              TO  GDA-CD-EST-NEGL
           WHEN  5
               MOVE         8192            TO  GDA-CD-EST-NEGL
      *   >> Se Estado Negocial 3- Repactuação, 4- Alteração
           WHEN 3
           WHEN 4
               EVALUATE  SGTSPE25-CD-TIP-VRS-OPR
               WHEN  1
                  MOVE        256           TO GDA-CD-EST-NEGL
               WHEN  2
                  MOVE         1            TO GDA-CD-EST-NEGL
               WHEN  5
                  MOVE        8192          TO GDA-CD-EST-NEGL
               END-EVALUATE
           END-EVALUATE
           .
       960018-SAI.
           EXIT.
      *-----------------------------------------------------------------
       960019-TRATA-EST-NEGL      SECTION.
      * Aciona rotina do sistema OPR para incluir Estado Negocial e
      * Motivo do Estado Negocial
      *-----------------------------------------------------------------
           MOVE         0078             TO  MSG1-CD-SEQL-ERRO
      
           INITIALIZE   OPRS0214-RTN-PGM  REPLACING NUMERIC BY  ZEROS

           MOVE KOPRD-CD-OPRD            TO  OPRS0214-CD-CHV-USU
           MOVE         1                TO  OPRS0214-ACAO
           MOVE GDA-NR-UNCO-CTR-OPR      TO  OPRS0214-NR-UNCO-CTR-OPR
           MOVE         1                TO  OPRS0214-NR-EPRD-FNCD
           MOVE         1                TO  OPRS0214-NR-SCTR-OPR
           MOVE        'SGT'             TO  OPRS0214-SG-SIS
           MOVE GDA-CD-EST-NEGL          TO  OPRS0214-CD-EST-NEGL-SCTR
           MOVE         1                TO  OPRS0214-QT-MTV-EST-NEGL

           IF   GDA-CD-MTV-EST-NEGL  NOT POSITIVE  OR
                GDA-CD-MTV-EST-NEGL  EQUAL ZEROS
                MOVE      99             TO  OPRS0214-CD-MTV-EST-NEGL(1)
           ELSE
                MOVE GDA-CD-MTV-EST-NEGL TO  OPRS0214-CD-MTV-EST-NEGL(1)
           END-IF

           PERFORM   970128-CHAMA-OPRS0214
           .
       960019-SAI.
           EXIT.

      *-----------------------------------------------------------------
       960020-INCLUIR-PE      SECTION.
      * Aciona rotina do sistema OPR para incluir PE - Perda Esperada
      *-----------------------------------------------------------------
           MOVE       0082                  TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE   OPRS9532-DADOS
      
           MOVE  'SGTS001A'                 TO K9532-PRG-CHAMADA
           MOVE  'O'                        TO K9532-AMB-CHAMADA

           MOVE GDA-NR-UNCO-CTR-OPR         TO K9532-NR-UNCO-CTR-OPR
           MOVE         1                   TO K9532-NR-EPRD-FNCD
           MOVE         1                   TO K9532-NR-SCTR
           MOVE        33                   TO K9532-CD-TIP-TAXA
           MOVE SGTST137-PC-PDA-EPRO        TO K9532-PC-TAXA
           MOVE        02                   TO K9532-CD-FMA-TAXA
           MOVE       1313                  TO K9532-CD-IEC-TAXA
           MOVE        100                  TO K9532-PC-IEC-TAXA
           MOVE        ZEROS                TO K9532-NR-CRV-FTR-RSCO
           MOVE         2                   TO K9532-CD-EST-TAXA
           MOVE   CORR SGTS001A-DT-PSC-DRV-R(IC-TAB)
                                            TO DT-OPR-R
           MOVE      DT-OPR                 TO K9532-DT-EST-TAXA
           MOVE         2                   TO K9532-CD-TIP-TAXA-JUR
           MOVE       'SGT'                 TO K9532-SG-SIS

           PERFORM   970114-CHAMA-OPRS9532
           .
       960020-SAI.
           EXIT.
      *-----------------------------------------------------------------
       960021-ALTERAR-PE      SECTION.
      * Aciona rotina do sistema OPR para alterar PE - Perda Esperada
      *-----------------------------------------------------------------
           MOVE       0083                  TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE   OPRS9632-DADOS
      
           MOVE  'SGTS001A'                 TO K9632-PRG-CHAMADA
           MOVE  'O'                        TO K9632-AMB-CHAMADA

           MOVE GDA-NR-UNCO-CTR-OPR         TO K9632-NR-UNCO-CTR-OPR
           MOVE         1                   TO K9632-NR-EPRD-FNCD
           MOVE         1                   TO K9632-NR-SCTR
           MOVE        33                   TO K9632-CD-TIP-TAXA
           MOVE SGTST137-PC-PDA-EPRO        TO K9632-PC-TAXA
           MOVE        02                   TO K9632-CD-FMA-TAXA
           MOVE       1313                  TO K9632-CD-IEC-TAXA
           MOVE        100                  TO K9632-PC-IEC-TAXA
           MOVE        ZEROS                TO K9632-NR-CRV-FTR-RSCO
           MOVE         2                   TO K9632-CD-EST-TAXA
           MOVE   CORR SGTS001A-DT-PSC-DRV-R(IC-TAB)
                                            TO DT-OPR-R
           MOVE      DT-OPR                 TO K9632-DT-EST-TAXA
           MOVE         2                   TO K9632-CD-TIP-TAXA-JUR
           MOVE       'SGT'                 TO K9632-SG-SIS

           PERFORM   970121-CHAMA-OPRS9632
           .
       960021-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970101-TRATA-RET-OPRS9401          SECTION.
      *-->Trata retorno da rotina dea inclusão contrato de operação
      *-----------------------------------------------------------------
           MOVE 0039                   TO MSG1-CD-SEQL-ERRO

           IF  W9401-COD-RETORNO   NOT EQUAL ZEROS
               IF  W9401-QTD-ERROS  >  ZEROS
                   MOVE 1             TO  IC-ERRO
                   PERFORM UNTIL IC-ERRO > W9401-QTD-ERROS
                      MOVE W9401-COD-ERRO(IC-ERRO) TO CD-ERR-OPR
                      STRING LST-ERR-OPR DELIMITED BY '*'
                             CD-ERR-OPR  DELIMITED BY SIZE
                             ',*' DELIMITED BY SIZE
                      INTO  LST-ERR-OPR
                      ADD 1    TO   IC-ERRO
                   END-PERFORM
               END-IF
               IF  W9401-COD-RETORNO   NOT EQUAL 1
                   SET ERRO-PROCESSAMENTO TO  TRUE
                   MOVE W9401-COD-RETORNO TO  CD-RTN-OPR
                   MOVE W9401-MSG-RETORNO TO  MSG-RTN-OPR
                   MOVE OPRS9401          TO  NM-PGM-ERRO
                   PERFORM    970111-TRATA-ERRO-OPR
               END-IF
           END-IF
           .
       970101-SAI.
           EXIT.
      
      * Para as rotinas do OPR quando ocorrer o erro 3004 ficou defini-
      * do que a mensageria ficaria no pgm principal (sgts001a) com
      * a mensagem de erro de acionamento do módulo

      *-----------------------------------------------------------------
       970102-CHAMA-OPRS9502           SECTION.
      *-->Aciona rotina OPR para incluir empreendimento financiado
      *-----------------------------------------------------------------
           MOVE 0040                   TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( OPRS9502       )
                        COMMAREA( OPRS9502-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               SET MNT-VRV             TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo OPRS9502.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  W9502-COD-RETORNO   NOT EQUAL ZEROS
               IF  W9502-QTD-ERROS  >  ZEROS
                   MOVE 1             TO  IC-ERRO
                   PERFORM UNTIL IC-ERRO > W9502-QTD-ERROS
                      MOVE W9502-COD-ERRO(IC-ERRO) TO CD-ERR-OPR
                      STRING LST-ERR-OPR DELIMITED BY '*'
                             CD-ERR-OPR  DELIMITED BY SIZE
                             ',*' DELIMITED BY SIZE
                      INTO  LST-ERR-OPR
                      ADD 1    TO   IC-ERRO
                   END-PERFORM
               END-IF
               IF  W9502-COD-RETORNO   NOT EQUAL 1
                   SET ERRO-PROCESSAMENTO TO  TRUE
                   MOVE W9502-COD-RETORNO TO  CD-RTN-OPR
                   MOVE W9502-MSG-RETORNO TO  MSG-RTN-OPR
                   MOVE OPRS9502          TO  NM-PGM-ERRO
                   PERFORM    970111-TRATA-ERRO-OPR
               END-IF
           END-IF
           .
       970102-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       970103-CHAMA-OPRS9503           SECTION.
      *-->Aciona rotina OPR para incluir subcontrato da operação
      *-----------------------------------------------------------------
           MOVE 0041                   TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( OPRS9503       )
                        COMMAREA( OPRS9503-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               SET MNT-VRV             TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo OPRS9503.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
      
           IF  W9503-COD-RETORNO   NOT EQUAL ZEROS
               IF  W9503-QTD-ERROS  >  ZEROS
                   MOVE 1             TO  IC-ERRO
                   PERFORM UNTIL IC-ERRO > W9503-QTD-ERROS
                      MOVE W9503-COD-ERRO(IC-ERRO) TO CD-ERR-OPR
                      STRING LST-ERR-OPR DELIMITED BY '*'
                             CD-ERR-OPR  DELIMITED BY SIZE
                             ',*' DELIMITED BY SIZE
                      INTO  LST-ERR-OPR
                      ADD 1    TO   IC-ERRO
                   END-PERFORM
               END-IF
               IF  W9503-COD-RETORNO   NOT EQUAL 1
                   SET ERRO-PROCESSAMENTO TO  TRUE
                   MOVE W9503-COD-RETORNO TO  CD-RTN-OPR
                   MOVE W9503-MSG-RETORNO TO  MSG-RTN-OPR
                   MOVE OPRS9503          TO  NM-PGM-ERRO
                   PERFORM    970111-TRATA-ERRO-OPR
               END-IF
           END-IF
           .
       970103-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970104-CHAMA-OPRS9504           SECTION.
      *-->Aciona rotina OPR para incluir participação da pessoa	contrato
      *-----------------------------------------------------------------
           MOVE 0064                   TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( OPRS9504       )
                        COMMAREA( OPRS9504-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               SET MNT-VRV             TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo OPRS9504.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  W9504-COD-RETORNO   NOT EQUAL ZEROS
               IF  W9504-QTD-ERROS  >  ZEROS
                   MOVE 1             TO  IC-ERRO
                   PERFORM UNTIL IC-ERRO > W9504-QTD-ERROS
                      MOVE W9504-COD-ERRO(IC-ERRO) TO CD-ERR-OPR
                      STRING LST-ERR-OPR DELIMITED BY '*'
                             CD-ERR-OPR  DELIMITED BY SIZE
                             ',*' DELIMITED BY SIZE
                      INTO  LST-ERR-OPR
                      ADD 1    TO   IC-ERRO
                   END-PERFORM
               END-IF
               IF  W9504-COD-RETORNO   NOT EQUAL 1
                   SET ERRO-PROCESSAMENTO TO  TRUE
                   MOVE W9504-COD-RETORNO TO  CD-RTN-OPR
                   MOVE W9504-MSG-RETORNO TO  MSG-RTN-OPR
                   MOVE OPRS9504          TO  NM-PGM-ERRO
                   PERFORM    970111-TRATA-ERRO-OPR
               END-IF
           END-IF
           .
       970104-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       970105-CHAMA-OPRS9524           SECTION.
      *-->Aciona rotina OPR para incluir saldo de subcontrado operação
      *-----------------------------------------------------------------
           MOVE 0042                   TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( OPRS9524       )
                        COMMAREA( OPRS9524-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               SET MNT-VRV             TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo OPRS9524.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  W9524-COD-RETORNO   NOT EQUAL ZEROS
               SET ERRO-PROCESSAMENTO TO  TRUE
               MOVE W9524-COD-RETORNO TO  CD-RTN-OPR
               MOVE W9524-MSG-RETORNO TO  MSG-RTN-OPR
               MOVE '*'               TO  LST-ERR-OPR
               MOVE OPRS9524          TO  NM-PGM-ERRO
               PERFORM    970111-TRATA-ERRO-OPR
           END-IF
           .
       970105-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       970106-TRATA-RET-OPRS9601           SECTION.
      *-->Aciona rotina OPR para alterar contrato de operação
      *-----------------------------------------------------------------
           MOVE 0043                   TO MSG1-CD-SEQL-ERRO
      
           IF  W9601-COD-RETORNO   NOT EQUAL ZEROS
               IF  W9601-QTD-ERROS  >  ZEROS
                   MOVE 1             TO  IC-ERRO
                   PERFORM UNTIL IC-ERRO > W9601-QTD-ERROS
                      MOVE W9601-COD-ERRO(IC-ERRO) TO CD-ERR-OPR
                      STRING LST-ERR-OPR DELIMITED BY '*'
                             CD-ERR-OPR  DELIMITED BY SIZE
                             ',*' DELIMITED BY SIZE
                      INTO  LST-ERR-OPR
                      ADD 1    TO   IC-ERRO
                   END-PERFORM
               END-IF
               IF  W9601-COD-RETORNO   NOT EQUAL 1 AND 2
                   SET ERRO-PROCESSAMENTO TO  TRUE
                   MOVE W9601-COD-RETORNO TO  CD-RTN-OPR
                   MOVE W9601-MSG-RETORNO TO  MSG-RTN-OPR
                   MOVE OPRS9601          TO  NM-PGM-ERRO
                   PERFORM    970111-TRATA-ERRO-OPR
               END-IF
           END-IF
           .
       970106-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       970107-CHAMA-OPRS9602           SECTION.
      *-->Aciona rotina OPR para alterar empreendimento financiado
      *-----------------------------------------------------------------
           MOVE 0044                   TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( OPRS9602       )
                        COMMAREA( OPRS9602-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               SET MNT-VRV             TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo OPRS9602.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  W9602-COD-RETORNO   NOT EQUAL ZEROS
               IF  W9602-QTD-ERROS  >  ZEROS
                   MOVE 1             TO  IC-ERRO
                   PERFORM UNTIL IC-ERRO > W9602-QTD-ERROS
                      MOVE W9602-COD-ERRO(IC-ERRO) TO CD-ERR-OPR
                      STRING LST-ERR-OPR DELIMITED BY '*'
                             CD-ERR-OPR  DELIMITED BY SIZE
                             ',*' DELIMITED BY SIZE
                      INTO  LST-ERR-OPR
                      ADD 1    TO   IC-ERRO
                   END-PERFORM
               END-IF
               IF  W9602-COD-RETORNO   NOT EQUAL 1 AND 988
                   SET ERRO-PROCESSAMENTO TO  TRUE
                   MOVE W9602-COD-RETORNO TO  CD-RTN-OPR
                   MOVE W9602-MSG-RETORNO TO  MSG-RTN-OPR
                   MOVE OPRS9602          TO  NM-PGM-ERRO
                   PERFORM    970111-TRATA-ERRO-OPR
               END-IF
           END-IF
           .
       970107-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       970108-CHAMA-OPRS9603           SECTION.
      *-->Aciona rotina OPR para alterar subcontrato da operação
      *-----------------------------------------------------------------
           MOVE 0045                   TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( OPRS9603       )
                        COMMAREA( OPRS9603-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               SET MNT-VRV             TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo OPRS9603.'
                      'RESP  = '   MSG1-RESP1  ', '
                      'RESP2 = '   MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  W9603-COD-RETORNO   NOT EQUAL ZEROS
               IF  W9603-QTD-ERROS  >  ZEROS
                   MOVE 1             TO  IC-ERRO
                   PERFORM UNTIL IC-ERRO > W9603-QTD-ERROS
                      MOVE W9603-COD-ERRO(IC-ERRO) TO CD-ERR-OPR
                      STRING LST-ERR-OPR DELIMITED BY '*'
                             CD-ERR-OPR  DELIMITED BY SIZE
                             ',*' DELIMITED BY SIZE
                      INTO  LST-ERR-OPR
                      ADD 1    TO   IC-ERRO
                   END-PERFORM
               END-IF
               IF  W9603-COD-RETORNO   NOT EQUAL 1 AND 2
                   SET ERRO-PROCESSAMENTO TO  TRUE
                   MOVE W9603-COD-RETORNO TO  CD-RTN-OPR
                   MOVE W9603-MSG-RETORNO TO  MSG-RTN-OPR
                   MOVE OPRS9603          TO  NM-PGM-ERRO
                   PERFORM    970111-TRATA-ERRO-OPR
               END-IF
           END-IF
           .
       970108-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       970109-TRATA-RET-OPRS9604           SECTION.
      *-->Aciona rotina OPR para alterar participação de pessoa em
      *-->subcontrato da operação
      *-----------------------------------------------------------------
           MOVE 0046                   TO MSG1-CD-SEQL-ERRO
      
           IF  W9604-COD-RETORNO   NOT EQUAL ZEROS
               IF  W9604-QTD-ERROS  >  ZEROS
                   MOVE 1             TO  IC-ERRO
                   PERFORM UNTIL IC-ERRO > W9604-QTD-ERROS
                      MOVE W9604-COD-ERRO(IC-ERRO) TO CD-ERR-OPR
                      STRING LST-ERR-OPR DELIMITED BY '*'
                             CD-ERR-OPR  DELIMITED BY SIZE
                             ',*' DELIMITED BY SIZE
                      INTO  LST-ERR-OPR
                      ADD 1    TO   IC-ERRO
                   END-PERFORM
               END-IF
               IF  W9604-COD-RETORNO   NOT EQUAL 1 AND 2 AND 64
                   SET ERRO-PROCESSAMENTO TO  TRUE
                   MOVE W9604-COD-RETORNO TO  CD-RTN-OPR
                   MOVE W9604-MSG-RETORNO TO  MSG-RTN-OPR
                   MOVE OPRS9604          TO  NM-PGM-ERRO
                   PERFORM    970111-TRATA-ERRO-OPR
               END-IF
           END-IF
           .
       970109-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       970110-CHAMA-OPRS9624           SECTION.
      *-->Aciona rotina OPR para alterar saldo de subcontrado operação
      *-----------------------------------------------------------------
           MOVE 0047                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS LINK
                        PROGRAM ( OPRS9624       )
                        COMMAREA( OPRS9624-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               SET MNT-VRV             TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo OPRS9624.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  W9624-COD-RETORNO   NOT EQUAL ZEROS
               IF  W9624-COD-RETORNO   EQUAL 18
                  PERFORM 960005-INCLUIR-SDO-SUB-CTR-OPR
               ELSE
                  SET ERRO-PROCESSAMENTO TO  TRUE
                  MOVE W9624-COD-RETORNO TO  CD-RTN-OPR
                  MOVE W9624-MSG-RETORNO TO  MSG-RTN-OPR
                  IF  W9624-QTD-ERROS  >  ZEROS
                      MOVE '*'           TO  LST-ERR-OPR
                      MOVE 1             TO  IC-ERRO
                      PERFORM UNTIL IC-ERRO > W9624-QTD-ERROS
                         MOVE W9624-COD-ERRO(IC-ERRO) TO CD-ERR-OPR
                         STRING LST-ERR-OPR DELIMITED BY '*'
                                CD-ERR-OPR  DELIMITED BY SIZE
                                ',*' DELIMITED BY SIZE
                         INTO  LST-ERR-OPR
                         ADD 1    TO   IC-ERRO
                      END-PERFORM
                  END-IF
                  MOVE OPRS9624          TO  NM-PGM-ERRO
                  PERFORM    970111-TRATA-ERRO-OPR
              END-IF
           END-IF
           .
       970110-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       970111-TRATA-ERRO-OPR           SECTION.
      *-->Monta string de erro retornado do opr para gravaÇão em tabela
      *-----------------------------------------------------------------
               STRING NM-PGM-ERRO DELIMITED BY SIZE
                      '-'         DELIMITED BY SIZE
                      CD-RTN-OPR  DELIMITED BY SIZE
                      '-'         DELIMITED BY SIZE
                      MSG-RTN-OPR DELIMITED BY SIZE
                      '-'         DELIMITED BY SIZE
                      LST-ERR-OPR DELIMITED BY '*'
               INTO  MSG-REGISTRO-ERRO
           .
       970111-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970112-CHAMA-SGTSLJ11           SECTION.
      *-----------------------------------------------------------------
           MOVE 0048                        TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( SGTSLJ11       )
                        COMMAREA( SGTSLJ11-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo SGTSLJ11.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSLJ11-CD-RTN-PGM      NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO
                                        TO  TRUE
               MOVE SGTSLJ11-CD-RTN-PGM TO  MSG1-CD-ERRO-TS
               MOVE SGTSLJ11-MSG-ERRO   TO  MSG1-TX-ERRO-TS
                                            MSG1-TX-RTN-PGM-ACND
               MOVE SGTSLJ11            TO  MSG1-NM-PGM-ACND
                                            NM-PGM
               MOVE SGTSLJ11-CD-RTN-PGM
                                        TO  MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA

           END-IF
           .
       970112-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       970113-CHAMA-SGTSX005           SECTION.
      *-----------------------------------------------------------------
           MOVE 0049                        TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( SGTSX005       )
                        COMMAREA( SGTSX005-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo SGTSX005.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSX005-CD-RTN-PGM      NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO
                                        TO  TRUE
               MOVE SGTSX005-CD-RTN-PGM TO  MSG1-CD-ERRO-TS
               MOVE SGTSX005-MSG-ERRO   TO  MSG1-TX-ERRO-TS
                                            MSG1-TX-RTN-PGM-ACND
               MOVE SGTSX005            TO  MSG1-NM-PGM-ACND
                                            NM-PGM
               MOVE SGTSX005-CD-RTN-PGM
                                        TO  MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       970113-SAI.
           EXIT.
      
       970114-CHAMA-OPRS9532           SECTION.
      *-->Aciona rotina OPR para incluir TJE
      *-----------------------------------------------------------------
           MOVE 0066                  TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( OPRS9532       )
                        COMMAREA( OPRS9532-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               SET MNT-VRV             TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo OPRS9532.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  (K9532-COD-RETORNO   NOT EQUAL ZEROS)
               SET ERRO-PROCESSAMENTO TO  TRUE
               MOVE K9532-COD-RETORNO TO  CD-RTN-OPR
               MOVE K9532-MSG-RETORNO TO  MSG-RTN-OPR
               MOVE OPRS9532          TO  NM-PGM-ERRO
               PERFORM    970111-TRATA-ERRO-OPR
           END-IF
           .
       970114-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970115-CHAMA-CACSV045           SECTION.
      *-----------------------------------------------------------------
           MOVE 0068                        TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( CACSV045       )
                        COMMAREA( CACSV045-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo CACSV045.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--- Vamos considerar por enquanto a possibilidade de não achar
      *    o registro  CACS045V-CD-RTN-PGM = +100
           IF  (CACS045V-CD-RTN-PGM      NOT EQUAL ZEROS)   AND
               (CACS045V-CD-RTN-PGM      NOT EQUAL +100)
               SET ERRO-PROGRAMA-ACIONADO
                                        TO  TRUE
               MOVE CACS045V-CD-RTN-PGM TO  MSG1-CD-ERRO-TS
               MOVE CACS045V-MSG-ERRO   TO  MSG1-TX-ERRO-TS
                                            MSG1-TX-RTN-PGM-ACND
               MOVE CACSV045            TO  MSG1-NM-PGM-ACND
                                            NM-PGM
               MOVE CACS045V-CD-RTN-PGM
                                        TO  MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       970115-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970116-CHAMA-SGTST004           SECTION.
      *-----------------------------------------------------------------
           MOVE 0050                        TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( SGTST004       )
                        COMMAREA( SGTST004-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo SGTST004.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTST004-CD-RTN-PGM      NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO
                                        TO  TRUE
               MOVE SGTST004-CD-RTN-PGM TO  MSG1-CD-ERRO-TS
               MOVE SGTST004-MSG-ERRO   TO  MSG1-TX-ERRO-TS
                                            MSG1-TX-RTN-PGM-ACND
               MOVE SGTST004            TO  MSG1-NM-PGM-ACND
                                            NM-PGM
               MOVE SGTST004-CD-RTN-PGM
                                        TO  MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       970116-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       970117-CHAMA-SGTSPE22           SECTION.
      *-----------------------------------------------------------------
           MOVE 0051                        TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( SGTSPE22       )
                        COMMAREA( SGTSPE22-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo SGTSPE22.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSPE22-CD-RTN-PGM      NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO
                                        TO  TRUE
               MOVE SGTSPE22-CD-RTN-PGM TO  MSG1-CD-ERRO-TS
               MOVE SGTSPE22-MSG-ERRO   TO  MSG1-TX-ERRO-TS
                                            MSG1-TX-RTN-PGM-ACND
               MOVE SGTSPE22            TO  MSG1-NM-PGM-ACND
                                            NM-PGM
               MOVE SGTSPE22-CD-RTN-PGM
                                        TO  MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       970117-SAI.
           EXIT.
      
      
      *-----------------------------------------------------------------
       970118-CHAMA-SGTST009           SECTION.
      *-----------------------------------------------------------------
           MOVE 0052                        TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( SGTST009       )
                        COMMAREA( SGTST009-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo SGTST009.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTST009-CD-RTN-PGM      NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO
                                        TO  TRUE
               MOVE SGTST009-CD-RTN-PGM TO  MSG1-CD-ERRO-TS
               MOVE SGTST009-MSG-ERRO   TO  MSG1-TX-ERRO-TS
                                            MSG1-TX-RTN-PGM-ACND
               MOVE SGTST009            TO  MSG1-NM-PGM-ACND
                                            NM-PGM
               MOVE SGTST009-CD-RTN-PGM
                                        TO  MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       970118-SAI.
           EXIT.
      
      
      *-----------------------------------------------------------------
       970119-CHAMA-SGTSLJ12           SECTION.
      *-----------------------------------------------------------------
           MOVE 0053                        TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( SGTSLJ12       )
                        COMMAREA( SGTSLJ12-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo SGTSLJ12.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSLJ12-CD-RTN-PGM      NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO
                                        TO  TRUE
               MOVE SGTSLJ12-CD-RTN-PGM TO  MSG1-CD-ERRO-TS
               MOVE SGTSLJ12-MSG-ERRO   TO  MSG1-TX-ERRO-TS
                                            MSG1-TX-RTN-PGM-ACND
               MOVE SGTSLJ12            TO  MSG1-NM-PGM-ACND
                                            NM-PGM
               MOVE SGTSLJ12-CD-RTN-PGM
                                        TO  MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       970119-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970120-CHAMA-SGTST011           SECTION.
      *-----------------------------------------------------------------
           MOVE  0054                  TO  MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( SGTST011       )
                        COMMAREA( SGTST011-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo SGTST011.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      
           IF  SGTST011-CD-RTN-PGM     NOT EQUAL  ZEROS
               SET ERRO-PROGRAMA-ACIONADO
                                        TO  TRUE
               MOVE SGTST011-CD-RTN-PGM TO  MSG1-CD-ERRO-TS
               MOVE SGTST011-MSG-ERRO   TO  MSG1-TX-ERRO-TS
                                            MSG1-TX-RTN-PGM-ACND
               MOVE SGTST011            TO  MSG1-NM-PGM-ACND
                                            NM-PGM
               MOVE SGTST011-CD-RTN-PGM
                                        TO  MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       970120-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970121-CHAMA-OPRS9632           SECTION.
      *-->Aciona rotina OPR para alterar TJE
      *-----------------------------------------------------------------
           MOVE 0070                  TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( OPRS9632       )
                        COMMAREA( OPRS9632-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               SET MNT-VRV             TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo OPRS9632.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--- Considera OK também se não achou o registro
           IF  (K9632-COD-RETORNO   NOT EQUAL ZEROS)   AND
               (K9632-COD-RETORNO   NOT EQUAL 15)      AND
               (K9632-COD-RETORNO   NOT EQUAL 100)
               SET ERRO-PROCESSAMENTO TO  TRUE
               MOVE K9632-COD-RETORNO TO  CD-RTN-OPR
               MOVE K9632-MSG-RETORNO TO  MSG-RTN-OPR
               MOVE OPRS9632          TO  NM-PGM-ERRO
               PERFORM    970111-TRATA-ERRO-OPR
           END-IF
           .
       970121-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970122-CHAMA-OPRS9804           SECTION.
      *-->Aciona rotina OPR para incluir subcontrato da operação de
      *--> aplicação de tesouraria
      *-----------------------------------------------------------------
           MOVE 0055                   TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( OPRS9804       )
                        COMMAREA( OPRS9804-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               SET MNT-VRV             TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo OPRS9804.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  W9804-COD-RETORNO   NOT EQUAL ZEROS
               IF  W9804-QTD-ERROS  >  ZEROS
                   MOVE 1             TO  IC-ERRO
                   PERFORM UNTIL IC-ERRO > W9804-QTD-ERROS
                      MOVE W9804-COD-ERRO(IC-ERRO) TO CD-ERR-OPR
                      STRING LST-ERR-OPR DELIMITED BY '*'
                             CD-ERR-OPR  DELIMITED BY SIZE
                             ',*' DELIMITED BY SIZE
                      INTO  LST-ERR-OPR
                      ADD 1    TO   IC-ERRO
                   END-PERFORM
               END-IF
               IF  W9804-COD-RETORNO   NOT EQUAL 1
                   SET ERRO-PROCESSAMENTO TO  TRUE
                   MOVE W9804-COD-RETORNO TO  CD-RTN-OPR
                   MOVE W9804-MSG-RETORNO TO  MSG-RTN-OPR
                   MOVE OPRS9804          TO  NM-PGM-ERRO
                   PERFORM    970111-TRATA-ERRO-OPR
               END-IF
           END-IF
           .
       970122-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970123-CHAMA-OPRS9904           SECTION.
      *-->Aciona rotina OPR para alterar subcontrato da operação de
      *--> aplicação de tesouraria
      *-----------------------------------------------------------------
           MOVE 0056                   TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( OPRS9904       )
                        COMMAREA( OPRS9904-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               SET MNT-VRV             TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo OPRS9904.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  W9904-COD-RETORNO   NOT EQUAL ZEROS
               IF  W9904-QTD-ERROS  >  ZEROS
                   MOVE 1             TO  IC-ERRO
                   PERFORM UNTIL IC-ERRO > W9904-QTD-ERROS
                      MOVE W9904-COD-ERRO(IC-ERRO) TO CD-ERR-OPR
                      STRING LST-ERR-OPR DELIMITED BY '*'
                             CD-ERR-OPR  DELIMITED BY SIZE
                             ',*' DELIMITED BY SIZE
                      INTO  LST-ERR-OPR
                      ADD 1    TO   IC-ERRO
                   END-PERFORM
               END-IF
               IF  W9904-COD-RETORNO   EQUAL 23
                   PERFORM 960011-INCLUIR-SDO-CTR-APL-TSR
               ELSE

                  MOVE W9904-COD-RETORNO TO  CD-RTN-OPR
                  MOVE W9904-MSG-RETORNO TO  MSG-RTN-OPR
                  STRING 'OPRS9904' DELIMITED BY SIZE
                      '-'         DELIMITED BY SIZE
                      CD-RTN-OPR  DELIMITED BY SIZE
                      '-'         DELIMITED BY SIZE
                      MSG-RTN-OPR DELIMITED BY SIZE
                      '-'         DELIMITED BY SIZE
                  INTO  LST-ERR-OPR
               END-IF
           END-IF
           .
       970123-SAI.
           EXIT.

      *------------------------------------------------------------
       970124-BUSCA-TS-DT              SECTION.
      *------------------------------------------------------------
           MOVE 0057                   TO MSG1-CD-SEQL-ERRO
      
           CALL SBTIMEDT            USING CD-FUC
                                          AREA-TRB
                                          FMT-TS
                                          TAM-TS
                                          TS-FMTD
      
           IF  RETURN-CODE      NOT EQUAL ZEROS
               MOVE 0050               TO MSG1-CD-ERRO-TS
               MOVE 'Erro ao obter data e hora atual.'
                                       TO  MSG1-TX-ERRO-TS
                                           MSG1-TX-RTN-PGM-ACND
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       970124-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970125-CHAMA-OPRS033I           SECTION.
      *-->Aciona rotina OPR para incluir Taxonomia e Classificação
      *   Contábil
      *-----------------------------------------------------------------
           MOVE 0073                  TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( OPRS033I       )
                        COMMAREA( OPRS033I-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               SET MNT-VRV             TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo OPRS033I.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  (OPRS033I-CD-RTN-PGM    NOT EQUAL ZEROS)
               SET    ERRO-PROCESSAMENTO  TO  TRUE
               MOVE OPRS033I-CD-RTN-PGM   TO  CD-RTN-OPR
               MOVE OPRS033I-MSG-ERRO-PGM TO  MSG-RTN-OPR
               MOVE     OPRS033I          TO  NM-PGM-ERRO
               PERFORM    970111-TRATA-ERRO-OPR
           END-IF
           .
       970125-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970126-CHAMA-OPRS033U           SECTION.
      *-->Aciona rotina OPR para alterar Taxonomia e Classificação
      *   Contábil
      *-----------------------------------------------------------------
           MOVE 0074                  TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( OPRS033U       )
                        COMMAREA( OPRS033U-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               SET MNT-VRV             TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo OPRS033U.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  (OPRS033U-CD-RTN-PGM   NOT EQUAL ZEROS)  AND
               (OPRS033U-CD-RTN-PGM   NOT EQUAL 100)
                SET    ERRO-PROCESSAMENTO  TO  TRUE
                MOVE OPRS033U-CD-RTN-PGM   TO  CD-RTN-OPR
                MOVE OPRS033U-MSG-ERRO-PGM TO  MSG-RTN-OPR
                MOVE     OPRS033U          TO  NM-PGM-ERRO
                PERFORM    970111-TRATA-ERRO-OPR
           END-IF
           .
       970126-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970127-CHAMA-SGTSPE25          SECTION.
      *-->Pesquisa último Estado Negocial válido nas versões anteriores
      *-----------------------------------------------------------------
           MOVE 0076                  TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( SGTSPE25       )
                        COMMAREA( SGTSPE25-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               SET MNT-VRV             TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo SGTSPE25.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  (SGTSPE25-CD-RTN-PGM   NOT EQUAL ZEROS)
                SET    ERRO-PROCESSAMENTO  TO  TRUE
                MOVE SGTSPE25-CD-RTN-PGM   TO  CD-RTN-OPR
                MOVE SGTSPE25-MSG-ERRO-PGM TO  MSG-RTN-OPR
                MOVE    SGTSPE25          TO  NM-PGM-ERRO
                PERFORM    970111-TRATA-ERRO-OPR
           END-IF
           .
       970127-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970128-CHAMA-OPRS0214           SECTION.
      *-->Aciona rotina OPR para incluir Estado Negocial
      *-----------------------------------------------------------------
           MOVE 0077                  TO MSG1-CD-SEQL-ERRO
      
           EXEC CICS LINK
                        PROGRAM ( OPRS0214       )
                        COMMAREA( OPRS0214-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               SET MNT-VRV             TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo OPRS0214.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--- Considera OK também se achou o registro
           IF  (OPRS0214-CD-RTN-PGM   NOT EQUAL ZEROS)   AND
               (OPRS0214-CD-RTN-PGM   NOT EQUAL 803)
               SET ERRO-PROCESSAMENTO   TO  TRUE
               MOVE OPRS0214-CD-RTN-PGM TO  CD-RTN-OPR
               MOVE OPRS0214-MSG-ERRO   TO  MSG-RTN-OPR
               MOVE OPRS0214            TO  NM-PGM-ERRO
               PERFORM    970111-TRATA-ERRO-OPR
           END-IF
           .
       970128-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970129-CHAMA-SGTST137           SECTION.
      *-----------------------------------------------------------------
           MOVE        0081            TO MSG1-CD-SEQL-ERRO

           EXEC CICS LINK
                        PROGRAM ( SGTST137       )
                        COMMAREA( SGTST137-DADOS )
                        RESP    ( MSG1-RESP1)
                        RESP2   ( MSG1-RESP2)
           END-EXEC

           IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo SGTST137.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTST137-CD-RTN-PGM      NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO
                                        TO  TRUE
               MOVE SGTST137-CD-RTN-PGM TO  MSG1-CD-ERRO-TS
                                            MSG1-CD-RTN-PGM-ACND
               MOVE SGTST137-MSG-ERRO   TO  MSG1-TX-ERRO-TS
                                            MSG1-TX-RTN-PGM-ACND
               MOVE SGTST137            TO  MSG1-NM-PGM-ACND
                                            NM-PGM

               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       970129-SAI.
           EXIT.
      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *--> A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------

      *--> Le o container de erro dos modulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           INITIALIZE                      IIBKERRO-PRM-ERRO

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
      *        PARA O M DULO CHAMADO e tratar na section chamadora
               WHEN MSG1-RESP1             EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN MSG1-RESP1             EQUAL DFHRESP(NORMAL)
                    SET   ERRO-PROGRAMA-ACIONADO TO  TRUE
                    MOVE  NM-PGM    TO  MSG1-NM-PGM-ACND
                    IF NM-PGM(1:3) EQUAL 'SGT'
                       MOVE  IIBKERRO-CD-ERRO
                                    TO  MSG1-CD-RTN-PGM-ACND
                                        MSG1-CD-ERRO-TS
                       MOVE  IIBKERRO-TX-ERRO
                                      TO  MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
                    ELSE
                       SET MNT-VRV        TO TRUE
                       MOVE 3004          TO MSG1-CD-ERRO-TS
                       STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                                DELIMITED BY SIZE
                                INTO           MSG1-TX-ERRO-TS

                       MOVE IIBKERRO-TX-ERRO
                                   TO MSG1-TX-RTN-PGM-ACND
                       MOVE IIBKERRO-CD-ERRO
                                   TO MSG1-CD-RTN-PGM-ACND

                    END-IF
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               WHEN OTHER
                    SET   ERRO-CONTAINER
                                  TO  TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *--> Recupera o container com os dados de resposta (saída).
      *    Modulo CONTAINER  -     NOME_PGM-RPST
      *    Modulo IIB (FIXO) -     PRM-DADOS-RPST
      *----------------------------------------------------------------

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           EVALUATE NM-PGM
               WHEN MSTS8042
                    MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN OPRS9401
                    MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN OPRS9601
                    MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN OPRS9604
                    MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN OTHER
                    MOVE  NM-PGM       TO  NM-CTNR(1:8)
                    MOVE  '-RPST'      TO  NM-CTNR(9:5)
           END-EVALUATE

      *    Recupera o tamanho do container de resposta
            PERFORM 973100-LER-CONTAINER-NODATA

            PERFORM 973200-TAMANHO-ESPERADO

      *--> Configura dados para leitura do container de resposta
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Le a RESPOSTA de acordo com o programa que a gravou

           EVALUATE NM-PGM

      *        Calcular a quantidade de caracteres esperado e limpar
      *        a area de dados antes de executar o GET
      *        Para matrizes com dados variaveis (clausula depending
      *        on) nao ha como utilizar o comando INITIALIZE

           WHEN MSTS8042
                INITIALIZE             MSTS8042-RPST
                EXEC CICS GET
                          CONTAINER (NM-CTNR          )
                          INTO      (MSTS8042-RPST    )
                          FLENGTH   (QT-CCT-LIDO      )
                END-EXEC
           WHEN OPRS9401
                INITIALIZE             OPRS9401-DADOS
                EXEC CICS GET
                          CONTAINER (NM-CTNR          )
                          INTO      (OPRS9401-DADOS   )
                          FLENGTH   (QT-CCT-LIDO      )
                END-EXEC
           WHEN OPRS9601
                INITIALIZE             OPRS9601-DADOS
                EXEC CICS GET
                          CONTAINER (NM-CTNR          )
                          INTO      (OPRS9601-DADOS   )
                          FLENGTH   (QT-CCT-LIDO      )
                END-EXEC
           WHEN OPRS9604
                INITIALIZE             OPRS9604-DADOS
                EXEC CICS GET
                          CONTAINER (NM-CTNR          )
                          INTO      (OPRS9604-DADOS    )
                          FLENGTH   (QT-CCT-LIDO      )
                END-EXEC
           WHEN OTHER
                SET   ERRO-PROGRAMADO
                                    TO  TRUE
                SET MNT-VRV         TO TRUE
                MOVE  9003          TO  MSG1-CD-ERRO-TS
                STRING  'Erro na leitura da resposta do módulo '
                        NM-PGM '.'
                        DELIMITED BY SIZE
                        INTO           MSG1-TX-ERRO-TS
                PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
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
           EXEC CICS GET
                     CONTAINER (NM-CTNR    )
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC
      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       973200-TAMANHO-ESPERADO         SECTION.
      *-----------------------------------------------------------------
           MOVE  0058                  TO  MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM

               WHEN MSTS8042
                    MOVE  LENGTH OF MSTS8042-RPST
                                         TO  QT-CCT-EPRO
               WHEN OPRS9401
                    MOVE  LENGTH OF OPRS9401-DADOS
                                         TO  QT-CCT-EPRO
               WHEN OPRS9601
                    MOVE  LENGTH OF OPRS9601-DADOS
                                         TO  QT-CCT-EPRO
               WHEN OPRS9604
                    MOVE  LENGTH OF OPRS9604-DADOS
                                         TO  QT-CCT-EPRO
               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                         TO  TRUE
                    SET MNT-VRV          TO  TRUE
                    MOVE  9003           TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na leitura da resposta do módulo '
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                    END-STRING
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           IF  QT-CCT-LIDO                 NOT EQUAL QT-CCT-EPRO
               SET   ERRO-PROGRAMADO
                                           TO  TRUE
               MOVE  9001              TO  MSG1-CD-ERRO-TS
               MOVE  QT-CCT-EPRO       TO  VL-EXBR-1
               MOVE  QT-CCT-LIDO       TO  VL-EXBR-2
               STRING
                    'Erro no tamanho de área de requisição '
                    ', TAMANHO ESPERADO: '  VL-EXBR-1
                    ', TAMANHO RECEBIDO: '  VL-EXBR-2
                    ' - ' NM-PGM
                    DELIMITED BY SIZE   INTO  MSG1-TX-ERRO-TS
              END-STRING
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       973200-SAI.
           EXIT.

      *------------------------------------------------------------
       980000-CHAMA-SERVICO             SECTION.
      *------------------------------------------------------------
           MOVE 0059                   TO MSG1-CD-SEQL-ERRO

      *--> Grava o container com dados do serviço/operação a chamar
           PERFORM 981000-GRAVAR-CONTAINER-SRVC

      *--> Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
              EXEC CICS LINK
                   PROGRAM   ( IIBP2000     )
                   CHANNEL   ( NM-CNL       )
                   RESP      ( MSG1-RESP1   )
                   RESP2     ( MSG1-RESP2   )
              END-EXEC
              EVALUATE TRUE
                  WHEN MSG1-RESP1             EQUAL DFHRESP(NORMAL)
                       CONTINUE
                  WHEN OTHER
                       SET   ERRO-CICS    TO  TRUE
                       MOVE  9000         TO  MSG1-CD-ERRO-TS
                       STRING  'Erro ao acionar serviço: '
                               SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
                               SRVC-ID-OPR  '.' SRVC-VRS-OPR
                               'via IIBP2000, ' 'RESP=' MSG1-RESP1
                               ', RESP-2=' MSG1-RESP2 '.'
                               DELIMITED BY SIZE
                               INTO  MSG1-TX-ERRO-TS
                       END-STRING

                       PERFORM 999000-GRAVA-ERRO-E-ENCERRA
            END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA

           .
       980000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       981000-GRAVAR-CONTAINER-SRVC    SECTION.
      *--> Postar container de requisição de serviço.
      *-----------------------------------------------------------------
             MOVE  0060                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao do servico
             MOVE  'PRM-RQSC-SRVC'       TO  NM-CTNR
             SET   PUT-CONTAINER         TO  TRUE

            EVALUATE NM-PGM

                WHEN MSTS8042
                     MOVE  ZEROS        TO  SRVC-ID-SRVC
                     MOVE  ZEROS        TO  SRVC-VRS-SRVC
                     MOVE  1998646      TO  SRVC-ID-OPR
                     MOVE  0001         TO  SRVC-VRS-OPR

                WHEN OPRS9401
                     MOVE  ZEROS        TO  SRVC-ID-SRVC
                     MOVE  ZEROS        TO  SRVC-VRS-SRVC
                     MOVE  3316292      TO  SRVC-ID-OPR
                     MOVE  0001         TO  SRVC-VRS-OPR

                WHEN OPRS9601
                     MOVE  ZEROS        TO  SRVC-ID-SRVC
                     MOVE  ZEROS        TO  SRVC-VRS-SRVC
                     MOVE  3319025      TO  SRVC-ID-OPR
                     MOVE  0001         TO  SRVC-VRS-OPR

                WHEN OPRS9604
                     MOVE  ZEROS        TO  SRVC-ID-SRVC
                     MOVE  ZEROS        TO  SRVC-VRS-SRVC
                     MOVE  3389179      TO  SRVC-ID-OPR
                     MOVE  0001         TO  SRVC-VRS-OPR
           END-EVALUATE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS PUT
                CONTAINER ( NM-CTNR                     )
                FROM      ( SRVC-PRM-RQSC-SRVC          )
                FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       981000-SAI.
            EXIT.

      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *    Postar container com dados de requisição (entrada)
      *    Modulo CONTAINER  -     NOME_PGM-RQSC
      *    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
      *-----------------------------------------------------------------
            MOVE  0061                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_modulo -RQSC

           EVALUATE NM-PGM
               WHEN MSTS8042
                    MOVE  'PRM-DADOS-RQSC'   TO  NM-CTNR

               WHEN OPRS9401
                    MOVE  'PRM-DADOS-RQSC'   TO  NM-CTNR

               WHEN OPRS9601
                    MOVE  'PRM-DADOS-RQSC'   TO  NM-CTNR

               WHEN OPRS9604
                    MOVE  'PRM-DADOS-RQSC'   TO  NM-CTNR

               WHEN OTHER
                    MOVE  NM-PGM       TO  NM-CTNR(1:8)
                    MOVE  '-RQSC'      TO  NM-CTNR(9:5)
           END-EVALUATE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Faz PUT da requisicao do programa a executar
            EVALUATE NM-PGM
                WHEN MSTS8042
                     EXEC CICS PUT
                               CONTAINER (NM-CTNR                 )
                               FROM      (MSTS8042-RQSC           )
                               FLENGTH   (LENGTH OF MSTS8042-RQSC )
                     END-EXEC

                WHEN OPRS9401
                     EXEC CICS PUT
                               CONTAINER (NM-CTNR                 )
                               FROM      (OPRS9401-DADOS          )
                               FLENGTH   (LENGTH OF OPRS9401-DADOS )
                     END-EXEC

                WHEN OPRS9601
                     EXEC CICS PUT
                               CONTAINER (NM-CTNR                 )
                               FROM      (OPRS9601-DADOS          )
                               FLENGTH   (LENGTH OF OPRS9601-DADOS )
                     END-EXEC

                WHEN OPRS9604
                     EXEC CICS PUT
                               CONTAINER (NM-CTNR                 )
                               FROM      (OPRS9604-DADOS          )
                               FLENGTH   (LENGTH OF OPRS9604-DADOS )
                     END-EXEC

                WHEN OTHER
                     SET   ERRO-PROGRAMADO
                                        TO  TRUE
                     MOVE  9004         TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na gravação da requisição do módulo '
                              NM-PGM '.'
                              DELIMITED BY SIZE
                              INTO           MSG1-TX-ERRO-TS
                     END-STRING
                     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
            SET   SEM-ERRO              TO  TRUE
            .
       982000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      * Exclui o container de resposta e de erro para evitar problemas
      * em modulos executados mais de uma vez dentro da mesma sessao do
      * CICS. Equivale ao initialize na area de resposta dos programas
      * que utilizam COMMAREA.
      *-----------------------------------------------------------------
           MOVE  0062                  TO  MSG1-CD-SEQL-ERRO

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           EVALUATE NM-PGM

               WHEN MSTS8042
                    MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN OPRS9401
                    MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN OPRS9601
                    MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN OPRS9604
                    MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN OTHER
                    MOVE  NM-PGM       TO  NM-CTNR(1:8)
                    MOVE  '-RPST'      TO  NM-CTNR(9:5)
           END-EVALUATE

           SET   DEL-CONTAINER         TO  TRUE

      *--> Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                CONTAINER (NM-CTNR   )
                RESP      (MSG1-RESP1)
                RESP2     (MSG1-RESP2)
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1             EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1             EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET   ERRO-CONTAINER
                                   TO  TRUE
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

      *--> Limpar variaveis container de erro
           INITIALIZE                      IIBKERRO-PRM-ERRO
           REPLACING NUMERIC BY  ZEROS

      *--> Exclui o container de erro
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           SET   DEL-CONTAINER         TO  TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                CONTAINER (NM-CTNR   )
                RESP      (MSG1-RESP1)
                RESP2     (MSG1-RESP2)
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1             EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1             EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET   ERRO-CONTAINER
                                     TO  TRUE
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

      *-->>SBDATAF<<
              WHEN SBDATAF
                    MOVE 'FUNCAO'      TO    MSGKW999-NM-VRV-MNT
                    MOVE FUNCAO        TO    MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG01'        TO   MSGKW999-NM-VRV-MNT
                    MOVE ARG01          TO   MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG02'         TO   MSGKW999-NM-VRV-MNT
                    MOVE ARG02           TO   MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG03'         TO   MSGKW999-NM-VRV-MNT
                    MOVE ARG03           TO   MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'A-TAM'          TO  MSGKW999-NM-VRV-MNT
                    MOVE A-TAM            TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'A-FIM'          TO  MSGKW999-NM-VRV-MNT
                    MOVE  A-FIM           TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

      *-->>SGTSLJ11<<
               WHEN SGTSLJ11
                    MOVE  'SGTSLJ11-ACAO'  TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-ACAO    TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-DT-IMP-PSC-SDO'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-DT-IMP-PSC-SDO
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-HR-IMP-PSC-SDO'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-HR-IMP-PSC-SDO
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-DT-PSC-E'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-DT-PSC-E
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-CD-CTRA-TIT-VL'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-CD-CTRA-TIT-VL
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-CD-OPR-ETRD-MCP '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-CD-OPR-ETRD-MCP
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-NR-VRS-OPR-ETRD '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-NR-VRS-OPR-ETRD
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-NR-SEQL-RCBT-PRVT '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-NR-SEQL-RCBT-PRVT
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-CD-TIP-FUC-CLI '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-CD-TIP-FUC-CLI
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-CD-TIT-OPR-ETRD '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-CD-TIT-OPR-ETRD
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-NR-DVS-TIT '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-NR-DVS-TIT
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-SUM-VL-SDO-TIT'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-SUM-VL-SDO-TIT
                                           TO  VL-15V2
                    MOVE VL-15V2           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-QT-GERAL'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-QT-GERAL
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-IN-GERAL'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-IN-GERAL
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-DT-PSC'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-DT-PSC  TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-SLDO-AMRTZ-CARECA'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-SLDO-AMRTZ-CARECA
                                           TO  VL-15V2
                    MOVE VL-15V2           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-SLDO-RCBT-PRVT'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-SLDO-RCBT-PRVT
                                           TO  VL-15V2
                    MOVE VL-15V2           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-DT-PRMO-CRNG-ABTO '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-DT-PRMO-CRNG-ABTO
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT


      *-->>SGTSLJ12<<
               WHEN SGTSLJ12
                    MOVE  'SGTSLJ12-ACAO'  TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ12-ACAO    TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ12-CD-CTRA-TIT-VL-E'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ12-CD-CTRA-TIT-VL-E
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ12-CD-OPR-ETRD-MCP-E '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ12-CD-OPR-ETRD-MCP-E
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ12-NR-VRS-OPR-ETRD-E '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ12-NR-VRS-OPR-ETRD-E
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ12-CD-EST-RSTD-ITE-E '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ12-CD-EST-RSTD-ITE-E
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ12-CD-CLI-E '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ12-CD-CLI-E
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ12-CD-TIT-OPR-ETRD-E '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ12-CD-TIT-OPR-ETRD-E
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ12-ULT-CD-CTRA-TIT-VL'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ12-ULT-CD-CTRA-TIT-VL
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ12-ULT-CD-OPR-ETRD '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ12-ULT-CD-OPR-ETRD
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ12-ULT-NR-VRS-OPR '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ12-ULT-NR-VRS-OPR
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ12-ULT-NR-SEQL-PSC-DRIA '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ12-ULT-NR-SEQL-PSC-DRIA
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ12-ULT-DT-PSC-E'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ12-ULT-DT-PSC-E
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTSX005<<
               WHEN SGTSX005
                    MOVE  'SGTSX005-ACAO'  TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-ACAO    TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-CD-CTRA-TIT-VL'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-CD-CTRA-TIT-VL
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-CD-OPR-ETRD-MCP '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-CD-OPR-ETRD-MCP
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-NR-VRS-OPR-ETRD '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-NR-VRS-OPR-ETRD
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-TS-ULT-ATL'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-TS-ULT-ATL
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-CD-BCO-CRD-VL-OPR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-CD-BCO-CRD-VL-OPR
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-CD-DEPE-CRD-VL-OPR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-CD-DEPE-CRD-VL-OPR
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-CD-UOR-DTTR-OPR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-CD-UOR-DTTR-OPR
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-CD-PRD '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-CD-PRD  TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-CD-MDLD'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-CD-MDLD TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-CD-UOR-BB-CRD-VL'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-CD-UOR-BB-CRD-VL
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-NR-CC-BB'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-NR-CC-BB
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-NR-CC-OTR-BCO'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-NR-CC-OTR-BCO
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-DV-CC-OTR-BCO'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-DV-CC-OTR-BCO
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-QT-PAPL-TIT-EMS'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-QT-PAPL-TIT-EMS
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-QT-PAPL-TIT-CPR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-QT-PAPL-TIT-CPR
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-DT-CPR-OPR '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-DT-CPR-OPR
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-DT-VNCT-OPR '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-DT-VNCT-OPR
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-DT-LQDC-OPR-DRIV '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-DT-LQDC-OPR-DRIV
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-CD-TIT-OPR-ETRD '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-CD-TIT-OPR-ETRD
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-NR-DVS-TIT'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-NR-DVS-TIT
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-NR-UNCO-CTR-OPR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-NR-UNCO-CTR-OPR
                                           TO  MCR-S9-17
                    MOVE MCR-S9-17         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-CD-TIP-OPR-ETRD'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-CD-TIP-OPR-ETRD
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-CD-ITC-CVM-OPR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-CD-ITC-CVM-OPR
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-CD-CTGR-TVM'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSX005-CD-CTGR-TVM
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-CD-ESPC-DBTR-OPR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-CD-ESPC-DBTR-OPR
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-CD-TIP-COTA-TIT'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-CD-TIP-COTA-TIT
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-NR-VRS-TIP-COTA'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-NR-VRS-TIP-COTA
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-CD-TIP-MC-CPR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-CD-TIP-MC-CPR
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-CD-RSCO-OPR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-CD-RSCO-OPR
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-CD-CMRA-OPR-ETRD'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-CD-CMRA-OPR-ETRD
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-CD-TIP-VRS-OPR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-CD-TIP-VRS-OPR
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-NR-EMS-TIT-OPR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-NR-EMS-TIT-OPR
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-NR-SRE-EMS-TIT-OPR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-NR-SRE-EMS-TIT-OPR
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-DT-INC-VGC-VRS-OPR '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-DT-INC-VGC-VRS-OPR
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-DT-EMS-TIT-OPR '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-DT-EMS-TIT-OPR
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-QT-DD-PZ-VLD-PREF'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-QT-DD-PZ-VLD-PREF
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-QT-MM-CARE-JUR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-QT-MM-CARE-JUR
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-QT-MM-CARE-OPR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-QT-MM-CARE-OPR
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-QT-MM-CARE-RSGT'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-QT-MM-CARE-RSGT
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-QT-MM-CARE-AMTZ'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-QT-MM-CARE-AMTZ
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-QT-MM-CARE-AQS'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-QT-MM-CARE-AQS
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-PC-TARF-GRT-TAXA'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-PC-TARF-GRT-TAXA
                                           TO  VL-4V10
                    MOVE VL-4V10           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-VL-PRV-ENCT'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-VL-PRV-ENCT
                                           TO  VL-15V2
                    MOVE  VL-15V2          TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-VL-CPR-OPR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-VL-CPR-OPR
                                           TO  VL-15V2
                    MOVE  VL-15V2          TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-VL-MNDT-OPR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-VL-MNDT-OPR
                                           TO  VL-15V2
                    MOVE  VL-15V2          TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-VL-PU-EMS-TIT-OPR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-VL-PU-EMS-TIT-OPR
                                           TO  VL-9V8
                    MOVE  VL-9V8          TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-VL-APVD-BB-OPR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-VL-APVD-BB-OPR
                                           TO  VL-15V2
                    MOVE  VL-15V2          TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-VL-PU-CPR-TIT-OPR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-VL-PU-CPR-TIT-OPR
                                           TO  VL-9V8
                    MOVE  VL-9V8          TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-VL-EVT-VCLD-VRS'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-VL-EVT-VCLD-VRS
                                           TO  VL-15V2
                    MOVE  VL-15V2          TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSX005-DT-EVT-VCLD-VRS'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSX005-DT-EVT-VCLD-VRS
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTST004<<
               WHEN SGTST004
                    MOVE  'SGTST004-ACAO'  TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-ACAO    TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-CD-CHV-USU'  TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-CD-CHV-USU    TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-CD-CTRA-TIT-VL'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-CD-CTRA-TIT-VL
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-DT-PSC'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-DT-PSC  TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-CD-TIT-OPR-ETRD'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-CD-TIT-OPR-ETRD
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-NR-DVS-TIT'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-NR-DVS-TIT
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-NR-SEQL-PSC-DRIA'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-NR-SEQL-PSC-DRIA
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-TS-ULT-ATL'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-TS-ULT-ATL
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-CD-USU-RSP-ATL'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-CD-USU-RSP-ATL
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-QT-PAPL-TIT-DT-PSC'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-QT-PAPL-TIT-DT-PSC
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-VL-PU-TIT-DT-PSC'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-VL-PU-TIT-DT-PSC
                                           TO  VL-15V2
                    MOVE  VL-15V2          TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-VL-SDO-TIT-DT-PSC'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-VL-SDO-TIT-DT-PSC
                                           TO  VL-15V2
                    MOVE  VL-15V2          TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-CD-EST-RSTD-ITE'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-CD-EST-RSTD-ITE
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-DT-APL-TIT'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-DT-APL-TIT
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-VL-TAXA-IDX'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-VL-TAXA-IDX
                                           TO  VL-5V6
                    MOVE  VL-5V6           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-CD-IC' TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-CD-IC   TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-VL-EMS-PPL-TIT'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-VL-EMS-PPL-TIT
                                           TO  VL-15V14
                    MOVE  VL-15V14         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-VL-CTB-APL-MOEN'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-VL-CTB-APL-MOEN
                                           TO  VL-15V2
                    MOVE  VL-15V2          TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-VL-JUR-ULT-MM'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-VL-JUR-ULT-MM
                                           TO  VL-15V2
                    MOVE  VL-15V2          TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-VL-AMTZ'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-VL-AMTZ
                                           TO  VL-15V2
                    MOVE  VL-15V2          TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-VL-PVS-APL-MOEN'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-VL-PVS-APL-MOEN
                                           TO  VL-15V2
                    MOVE  VL-15V2          TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-TX-RTN-PRCT-OPR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-TX-RTN-PRCT-OPR
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-CD-USU-ENVO-PSC'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-CD-USU-ENVO-PSC
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-TS-ENVO-PSC'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-TS-ENVO-PSC
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-DT-IMP-PSC-SDO'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-DT-IMP-PSC-SDO
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-HR-IMP-PSC-SDO'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-HR-IMP-PSC-SDO
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST004-NR-IMP-PSC-SDO'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST004-NR-IMP-PSC-SDO
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTST009<<
               WHEN SGTST009
                    MOVE  'SGTST009-ACAO'  TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST009-ACAO    TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST009-CD-CHV-USU'  TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST009-CD-CHV-USU    TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST009-CD-CTRA-TIT-VL'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST009-CD-CTRA-TIT-VL
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST009-CD-OPR-ETRD-MCP '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST009-CD-OPR-ETRD-MCP
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST009-NR-VRS-OPR-ETRD '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST009-NR-VRS-OPR-ETRD
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST009-TS-EST-OPR-ETRD'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST009-TS-EST-OPR-ETRD
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST009-CD-USU-RSP-ATL'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST009-CD-USU-RSP-ATL
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST009-CD-USU-RSP-EST'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST009-CD-USU-RSP-EST
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST009-CD-TIP-EST-OPR '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST009-CD-TIP-EST-OPR
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST009-TX-JST-EST'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST009-TX-JST-EST
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST009-TS-EST-OPR-ETRD-S'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST009-TS-EST-OPR-ETRD-S
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTST011<<
               WHEN SGTST011
                    MOVE  'SGTST011-ACAO'  TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST011-ACAO    TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST011-CD-CHV-USU'  TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST011-CD-CHV-USU    TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST011-CD-CTRA-TIT-VL'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST011-CD-CTRA-TIT-VL
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST011-CD-OPR-ETRD-MCP '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST011-CD-OPR-ETRD-MCP
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST011-NR-VRS-OPR-ETRD '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST011-NR-VRS-OPR-ETRD
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST011-DT-INC-TAXA-RMNC'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST011-DT-INC-TAXA-RMNC
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST011-TS-ULT-ATL'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST011-TS-ULT-ATL
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST011-CD-USU-RSP-ATL'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST011-CD-USU-RSP-ATL
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST011-PC-TAXA-RMNC-OPR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST011-PC-TAXA-RMNC-OPR
                                           TO  VL-4V10
                    MOVE VL-4V10           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST011-DT-FIM-TAXA-RMNC'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST011-DT-FIM-TAXA-RMNC
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST011-CD-TIP-FMA-APL '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST011-CD-TIP-FMA-APL
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST011-CD-IEC-RMNC '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST011-CD-IEC-RMNC
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTST011-CD-PERC-CPTZ-JUR '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST011-CD-PERC-CPTZ-JUR
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

      *-->>SGTSPE22<<
               WHEN SGTSPE22
                    MOVE  'SGTSPE22-CD-CTRA-TIT-VL'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSPE22-CD-CTRA-TIT-VL
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSPE22-CD-OPR-ETRD-MCP '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSPE22-CD-OPR-ETRD-MCP
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSPE22-NR-VRS-OPR-ETRD '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSPE22-NR-VRS-OPR-ETRD
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTSPE22-NR-SEQL-RCBT-PRVT '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSPE22-NR-SEQL-RCBT-PRVT
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

      *-->>MSTS8042<<
               WHEN MSTS8042
                    MOVE  'MSTS8042-CD-UOR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  MSTS8042-CD-UOR
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'MSTS8042-CD-DEPE-UOR '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  MSTS8042-CD-DEPE-UOR
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'MSTS8042-CD-ENT-LCZC-UOR '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  MSTS8042-CD-ENT-LCZC-UOR
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'MSTS8042-CD-EST-UOR '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  MSTS8042-CD-EST-UOR
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'MSTS8042-CD-INST-ORGC '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE MSTS8042-CD-INST-ORGC
                                           TO  MCR-S9-15
                    MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'MSTS8042-CD-ITEM-AQTT-UOR '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  MSTS8042-CD-ITEM-AQTT-UOR
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'MSTS8042-CD-PSS-UOR'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE MSTS8042-CD-PSS-UOR
                                           TO  MCR-S9-15
                    MOVE  MCR-S9-15        TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'MSTS8042-CD-RSP-ATL '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  MSTS8042-CD-RSP-ATL
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'MSTS8042-CD-TIP-UOR '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE MSTS8042-CD-TIP-UOR
                                           TO  MCR-S9-15
                    MOVE  MCR-S9-15        TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'MSTS8042-DT-ATL'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  MSTS8042-DT-ATL  TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'MSTS8042-DT-FIM-VLD '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  MSTS8042-DT-FIM-VLD
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'MSTS8042-DT-INC-ATVD-UOR '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  MSTS8042-DT-INC-ATVD-UOR
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'MSTS8042-DT-INC-VLD '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  MSTS8042-DT-INC-VLD
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'MSTS8042-DT-PRVT-ECR-UOR '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  MSTS8042-DT-PRVT-ECR-UOR
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'MSTS8042-DV-CD-DEPE-UOR '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE MSTS8042-DV-CD-DEPE-UOR
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'MSTS8042-HR-ATL'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE MSTS8042-HR-ATL   TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'MSTS8042-IN-RST-ACSS-PSS '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE MSTS8042-IN-RST-ACSS-PSS
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'MSTS8042-IN-UOR-UNCO-MUN '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE MSTS8042-IN-UOR-UNCO-MUN
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'MSTS8042-NM-UOR '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE MSTS8042-NM-UOR   TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'MSTS8042-NM-UOR-RDZ '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE MSTS8042-NM-UOR-RDZ
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'MSTS8042-NR-CNPJ-UOR '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE MSTS8042-NR-CNPJ-UOR
                                           TO  MCR-S9-15
                    MOVE  MCR-S9-15        TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'MSTS8042-NR-ORD-DEPE-SBDD '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE MSTS8042-NR-ORD-DEPE-SBDD
                                           TO  MCR-S9-15
                    MOVE  MCR-S9-15        TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'MSTS8042-SG-UOR '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE MSTS8042-SG-UOR   TO  MSGKW999-VL-VRV-MNT
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
      *--> Monitorar variàveis do pgm, grava e encerra processamento

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Em caso de erro, desfaz as atualizaçoes em banco de dados
      *    A única condicção que essa chave é dif. de 's' é quando houve
      *    a rejeição total ou parcial das operações ao opr que é a úl-
      *    tima situação tratada no pgm.
           IF   CH-ROLLBACK          EQUAL 'S'
                EXEC CICS SYNCPOINT
                      ROLLBACK
                END-EXEC
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
           IF  MNT-VRV
            IF IC-TAB > SGTS001A-QT-OCR-TAB
      *        Nessa condição deu erro no envio de todas ou parcial das
      *        operações para o OPR. Essa condição foi colocada para
      *        no caso de vir o nr.máximo de ocorrencias previstas. e
      *        não haver invasão de área.
               MOVE  'SGTS001A-CD-CTRA-TIT-VL'
                                      TO  MSGKW999-NM-VRV-MNT
               MOVE  ZEROS            TO  MCR-S9-15
               MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'SGTS001A-NR-VRS-OPR-ETRD '
                                      TO  MSGKW999-NM-VRV-MNT
               MOVE  ZEROS            TO  MCR-S9-15
               MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'SGTS001A-CD-OPR-ETRD-MCP '
                                      TO  MSGKW999-NM-VRV-MNT
               MOVE  ZEROS            TO  MCR-S9-15
               MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'SGTS001A-DT-PSC-DRV  '
                                      TO  MSGKW999-NM-VRV-MNT
               MOVE  SPACES
                                      TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'SGTS001A-NR-SEQL-PSC-DRIA '
                                      TO  MSGKW999-NM-VRV-MNT
               MOVE  ZEROS            TO  MCR-S9-15
               MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

            ELSE

               MOVE  'SGTS001A-CD-CTRA-TIT-VL'
                                      TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS001A-CD-CTRA-TIT-VL(IC-TAB)
                                      TO  MCR-S9-15
               MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'SGTS001A-NR-VRS-OPR-ETRD '
                                      TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS001A-NR-VRS-OPR-ETRD(IC-TAB)
                                      TO  MCR-S9-15
               MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'SGTS001A-CD-OPR-ETRD-MCP '
                                      TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS001A-CD-OPR-ETRD-MCP(IC-TAB)
                                      TO  MCR-S9-15
               MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'SGTS001A-DT-PSC-DRV  '
                                      TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS001A-DT-PSC-DRV(IC-TAB)
                                      TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'SGTS001A-NR-SEQL-PSC-DRIA '
                                      TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS001A-NR-SEQL-PSC-DRIA(IC-TAB)
                                      TO  MCR-S9-15
               MOVE MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT
            END-IF
           END-IF

      *--> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
      *--> Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO
           PERFORM 999999-ENCERRAR
           .
       999200-SAI.
           EXIT.

      * --------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * --------------------------------------*
       COPY HLPKP999.

      *---------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
       COPY SGTKP999.

      *---------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
      *---------------------------------------*
       COPY MSGKPMG1.

      *---------------------------------------------------------------*
       999999-ENCERRAR              SECTION.
      *---------------------------------------------------------------*
      *    Retorna para programa chamador.
      *---------------------------------------------------------------*
      
           EXEC CICS
                RETURN
           END-EXEC
      
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTS001A ----------------------------*
