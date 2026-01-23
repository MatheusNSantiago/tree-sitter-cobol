      *-----------------------------------------------------------------
      * Programa..: SGTS0160
      * Analista..: F2054109 Cintia Ap Soares Iglesias
      * Autor.....: F0950413 António Francisco Mendonça Silva
      *             F8650584 Roberto Fernandes Lima
      * Data......: 20.04.2017
      * Objetivo..: Manter Cronograma de uma Operação Estruturada
      *             Incluir, Alterar ou Excluir Recebimentos Previstos
      *             na Tabela DB2SGT.RCBT_PRVT_OPR_ETRD
      *             e seus Vínculos na Tabela DB2SGT.RCBT_RLZD_OPR
      *             conforme os registros na Tabela DB2SGT.DMTV_CX_TIT
      * Compilação: Cobol/CICS
      *-----------------------------------------------------------------
      * VRS002 26.04.2019 F2634012 Substituir a pesquisa do sgtsl087
      *         DE LISTA-TODO POR LISTA-ATV
      * VRS001 06.02.2019 F0950413 Inclusão da sigla 'SGT' na section
      *                   964000-LER-CONTAINER-DE-ERRO
      *        30.01.2019 F2804421 DE 2016/145195 - Alteração monitora-
      *                   ção de variávels SBDATAF.
      *                  - Incluir monitoração variáveis SBSORT.
      *        20.07.2018 F8658869  DE 2016/145195 - Inclusão das variá-
      *        veis de monitoração dos programas acionados SGTSC069,
      *        SGTSA087, SGTSI087, SGTSPE07,SGTS0081, SGTS0161, SGTS0162
      *        20.04.2017 F8650584  DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0160.

      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
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
                                       '*** SGTS0160 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS002'.

      *--> Subrotinas utilizadas neste programa
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTSA087                PIC  X(08) VALUE 'SGTSA087'.
           03  SGTSC069                PIC  X(08) VALUE 'SGTSC069'.
           03  SGTSC070                PIC  X(08) VALUE 'SGTSC070'.
           03  SGTSE087                PIC  X(08) VALUE 'SGTSE087'.
           03  SGTSI087                PIC  X(08) VALUE 'SGTSI087'.
           03  SGTSL087                PIC  X(08) VALUE 'SGTSL087'.
           03  SGTS0161                PIC  X(08) VALUE 'SGTS0161'.
           03  SGTS0162                PIC  X(08) VALUE 'SGTS0162'.
           03  SGTSC005                PIC  X(08) VALUE 'SGTSC005'.
           03  SGTS0081                PIC  X(08) VALUE 'SGTS0081'.
           03  SGTSPE07                PIC  X(08) VALUE 'SGTSPE07'.

       77  SBSORT                      PIC  X(08) VALUE 'SBSORT  '.
       77  CURRENT                     PIC  X(07) VALUE 'CURRENT'.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B
      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *----------------------------------------------------------------*
      *                     Containers utilizados
      *----------------------------------------------------------------*

      * Container de erro
       77  CTNR-PRM-ERRO               PIC  X(16) VALUE 'PRM-ERRO'.

      * Container do usuário logado
       77  CTNR-PRM-INF-OPRD           PIC  X(16) VALUE 'PRM-INF-OPRD'.


      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      *--> Areas de trabalho
       01  IC-TAB                      PIC S9(04) VALUE ZEROS.
       01  IC-TAB-01                   PIC S9(04) VALUE ZEROS.
       01  IC-TAB-02                   PIC S9(04) VALUE ZEROS.
       01  IC-TAB-03                   PIC S9(04) VALUE ZEROS.
       01  AUX-TAB-CRNG-CHV-TAB        PIC  X(36) VALUE ' '.

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP-NR      PIC  9(11).

      *--> ?reas de trabalho para monitoração de mensagens de erro
      *    Codigo do tipos de erro nas tabelas de entrada do modulo
       01  CD-ERRO-TAB-RCBT-PRVT       PIC   9(01)       VALUE 0.
           88 COM-ERRO-TAB-RCBT-PRVT                     VALUE 1.
       01  NM-IC-TAB-RCBT-PRVT         PIC  X(30)        VALUE ' '.
       01  VL-IC-TAB-RCBT-PRVT         PIC S9(04) COMP-5 VALUE 0.

       01  CD-ERRO-TAB-VCL             PIC   9(01)       VALUE 0.
           88 COM-ERRO-TAB-VCL                           VALUE 1.
       01  NM-IC-TAB-VCL               PIC  X(30)        VALUE ' '.
       01  VL-IC-TAB-VCL               PIC S9(04) COMP-5 VALUE 0.

       01  CD-ERRO-TAB-OBS-RCBT-PRVT   PIC   9(01)       VALUE 0.
           88 COM-ERRO-TAB-OBS-RCBT-PRVT                 VALUE 1.
       01  NM-IC-TAB-OBS-RCBT-PRVT     PIC  X(30)        VALUE ' '.
       01  VL-IC-TAB-OBS-RCBT-PRVT     PIC S9(04) COMP-5 VALUE 0.

       01  CD-ERRO-TAB-REALIZADO       PIC   9(01)       VALUE 0.
           88  COM-ERRO-TAB-REALIZADO                    VALUE 1.
       01  NM-IC-TAB-REALIZADO         PIC  X(30)        VALUE ' '.
       01  VL-IC-TAB-REALIZADO         PIC S9(04) COMP-5 VALUE 0.

       01  BATIMENTO-DE-VALOR          PIC   9(01)       VALUE 0.
           88  COM-ERRO-BAT-VL                           VALUE 1.

       01  MCR-S9-15-V9-02             PIC  ----.---.---.---.--9,99.
       01  MCR-S9-04                   PIC                 ----9.
       01  MCR-S9-09                   PIC            ---------9.
       01  MCR-S9-11                   PIC           ----------9.

      *--> Areas Auxiliares para Tabela DB2SGT.OPR_ETRD_MCP
       01  OPR-ETRD-MCP.
              05 CD-CTRA-TIT           PIC S9(09) COMP-5 VALUE ZEROS.
              05 CD-OPR-ETRD-MCP       PIC S9(11) COMP-3 VALUE ZEROS.
              05 NR-VRS-OPR-ETRD       PIC S9(04) COMP-5 VALUE ZEROS.

      *--> Auxiliares da Tabela DB2SGT.RCBT_PRVT_OPR_ETRD
       01  NR-SEQL-RCBT-PRVT           PIC S9(04) COMP-5 VALUE ZEROS.
       01  CD-TIP-RCBT-PRVT            PIC S9(04) VALUE ZEROS.
           88 PREVISTO-NORMAL-JUROS               VALUE 0001.
           88 PREVISTO-NORMAL-CAPITAL             VALUE 0002.
           88 PREVISTO-EXTRA-JUROS                VALUE 0003.
           88 PREVISTO-EXTRA-CAPITAL              VALUE 0004.
       01  TX-OBS-RCBT-PRVT            PIC  X(150) VALUE SPACES.

      *--> Auxiliares Movimentacao e Calculo
       01  PC-RCBT-PRVT-TX             PIC  X(14).
       01  PC-RCBT-PRVT-R              REDEFINES PC-RCBT-PRVT-TX.
           03  PC-RCBT-PRVT            PIC S9(04)V9(10).

       01  VL-RCBT-PRVT-TX             PIC  X(17).
       01  VL-RCBT-PRVT-R              REDEFINES VL-RCBT-PRVT-TX.
           03  VL-RCBT-PRVT            PIC S9(15)V9(02).

       01  VL-EVT-OPR-TX               PIC  X(17).
       01  VL-EVT-OPR-R                REDEFINES VL-EVT-OPR-TX.
           03  VL-EVT-OPR              PIC S9(15)V9(02).

       01  AUX-QT-RCBT-PRVT-TX         PIC ----9.
       01  AUX-QT-VCL-TX               PIC ----9.
       01  AUX-QT-TX-OBS-TX            PIC ----9.

      *--> Totalizadores
       01  TTL-VL-RCBT-PRVT            PIC S9(15)V99 COMP-3 VALUE ZEROS.

      *--> Indicadores para Validar o Codigo do Tipo de Movimentacao
       01  IN-TIP-MVTC-JUR             PIC  X(01) VALUE SPACES.
           88 CRNG-COM-JUROS                      VALUE 'S'.
       01  IN-TIP-MVTC-AMTZ            PIC  X(01) VALUE SPACES.
           88 CRNG-COM-AMORTIZACAO                VALUE 'S'.
       01  IN-TIP-MVTC-JUR-AMTZ        PIC  X(01) VALUE SPACES.
           88 CRNG-COM-AMORTIZACAO-E-JUROS        VALUE 'S'.

      *--> Datas Invertidas para Validacoes
       01  DT-EMS-OPR-AA-MM-DD          PIC  X(08) VALUE SPACES.
       01  DT-EMS-OPR-AA-MM-DD-R        REDEFINES DT-EMS-OPR-AA-MM-DD.
           03 ANO                       PIC  9(04).
           03 MES                       PIC  9(02).
           03 DIA                       PIC  9(02).
       01  DT-VNCT-OPR-AA-MM-DD         PIC  X(08) VALUE SPACES.
       01  DT-VNCT-OPR-AA-MM-DD-R       REDEFINES DT-VNCT-OPR-AA-MM-DD.
           03 ANO                       PIC  9(04).
           03 MES                       PIC  9(02).
           03 DIA                       PIC  9(02).
       01  DT-RCBT-PRVT-AA-MM-DD        PIC  X(08) VALUE SPACES.
       01  DT-RCBT-PRVT-AA-MM-DD-R      REDEFINES DT-RCBT-PRVT-AA-MM-DD.
           03 ANO                       PIC  9(04).
           03 MES                       PIC  9(02).
           03 DIA                       PIC  9(02).

       01  DT-RCBT-RLZD-AA-MM-DD        PIC  X(08) VALUE SPACES.
       01  DT-RCBT-RLZD-AA-MM-DD-R      REDEFINES DT-RCBT-RLZD-AA-MM-DD.
           03 ANO                       PIC  9(04).
           03 MES                       PIC  9(02).
           03 DIA                       PIC  9(02).

       01  DT-TAB-RLZD-AA-MM-DD        PIC  X(08) VALUE SPACES.
       01  DT-TAB-RLZD-AA-MM-DD-R      REDEFINES DT-TAB-RLZD-AA-MM-DD.
           03 ANO                       PIC  9(04).
           03 MES                       PIC  9(02).
           03 DIA                       PIC  9(02).

       01  DT-DB2-RLZD-AA-MM-DD        PIC  X(08) VALUE SPACES.
       01  DT-DB2-RLZD-AA-MM-DD-R      REDEFINES DT-DB2-RLZD-AA-MM-DD.
           03 ANO                       PIC  9(04).
           03 MES                       PIC  9(02).
           03 DIA                       PIC  9(02).

       01  DT-EVT-VCLD-VRS-AA-MM-DD     PIC  X(08) VALUE SPACES.
       01  DT-EVT-VCLD-VRS-AA-MM-DD-R   REDEFINES
                                               DT-EVT-VCLD-VRS-AA-MM-DD.
           03 ANO                       PIC  9(04).
           03 MES                       PIC  9(02).
           03 DIA                       PIC  9(02).

      *--> Area de Parametros da Rotina SBDATAF
       01  WKSBDATA                    PIC  X(4099).
       01  AREA-RED                    REDEFINES WKSBDATA.
           03  AREA-TAM                PIC  9(0008) COMP.
           03  FILLER                  PIC  X(4071).
           03  AREA-FIM                PIC  X(0020).
       01  SBDATAF                     PIC  X(08) VALUE 'SBDATAF'.
      *--> Função 01
      *    - Calcular Primeiro Dia util a partir ou após uma Data Base
      *    - Retornar a Quantidade de Dias entre a Data Base e a
      *      Data Calculada
      *    - Validar Datas no Formato Gregoriano
      *    - Validar Datas no Formato Juliano
       01  FUNCAO                      PIC  X(03) VALUE 'F01'.
       01  ARG01                       PIC  9(08).
       01  ARG01-R                     REDEFINES  ARG01.
           03  DIA                     PIC  9(02).
           03  MES                     PIC  9(02).
           03  ANO                     PIC  9(04).
      *--> F01 - ARG02 = 0004 - Validar Datas no Formato Gregoriano
       01  ARG02                       PIC  9(01) VALUE 4.


      *-----------------------------------------------------------------
      *    Tabelas Internas
      *-----------------------------------------------------------------

      *--> Auxiliares das Tabelas Interna Cronograma
       01  IC-TAB-CRNG                 PIC  9(04) VALUE ZEROS.
       01  DT-TAB-CRNG-AA-MM-DD        PIC  X(08) VALUE SPACES.
       01  DT-TAB-CRNG-AA-MM-DD-R      REDEFINES DT-TAB-CRNG-AA-MM-DD.
           03 ANO                      PIC  9(04).
           03 MES                      PIC  9(02).
           03 DIA                      PIC  9(02).
       01  IC-TAB-OBS                  PIC  9(04) VALUE ZEROS.

      *--> Auxiliares Texto Observacao
       01  NR-ITEM-TX-OBS              PIC S9(04) COMP-5 VALUE ZEROS.

      *--> Auxiliares das Tabelas Interna Registros Gravados no DB2
       01  IC-TAB-DB2                  PIC  9(04) VALUE ZEROS.
       01  DT-TAB-DB2-AA-MM-DD         PIC  X(08) VALUE SPACES.
       01  DT-TAB-DB2-AA-MM-DD-R       REDEFINES DT-TAB-DB2-AA-MM-DD.
           03 ANO                      PIC  9(04).
           03 MES                      PIC  9(02).
           03 DIA                      PIC  9(02).


      *--> Cronograma
      *    Lista dos Recebimentos Previstos e seus Vinculos Financeiros
       01  CRONOGRAMA.
           03  TAB-CRNG                OCCURS 0950 TIMES.
               05  TAB-CRNG-CHV-TAB.
                   07  TAB-CRNG-CD-TIP-RCBT-PRVT
                                       PIC S9(04) COMP-5 VALUE 9999.
                   07  TAB-CRNG-DT-RCBT-PRVT
                                       PIC  X(08) VALUE '99999999'.
                   07  TAB-CRNG-DT-RCBT-PRVT-R
                                       REDEFINES TAB-CRNG-DT-RCBT-PRVT.
                       09  DIA             PIC  9(02).
                       09  MES             PIC  9(02).
                       09  ANO             PIC  9(04).
               05  TAB-CRNG-NR-SEQL-RCBT-PRVT
                                       PIC S9(04) COMP-5 VALUE 9999.
               05  TAB-CRNG-CD-TIT-OPR-ETRD
                                       PIC  X(12) VALUE SPACES.
               05  TAB-CRNG-NR-DVS-TIT
                                       PIC S9(4) COMP-5 VALUE 0.
               05  TAB-CRNG-TS-REG-PGTO
                                       PIC  X(26) VALUE
                                       '9999-99-99-99.99.99.999999'.
               05  TAB-CRNG-IN-VCL-MNL
                                       PIC  X(01) VALUE SPACES.
               05  TAB-CRNG-VL-RLZD-VCLD
                                       PIC S9(15)V99 COMP-3 VALUE ZEROS.
               05  TAB-CRNG-VL-RCBT-PRVT
                                       PIC S9(15)V99 COMP-3 VALUE ZEROS.

      *--> Area de Parametros da Rotina SBSORT
       01  TAB-CRNG-QT-REGISTRO        PIC S9(08) COMP VALUE ZEROS.
       01  TAB-CRNG-TAMANHO            PIC S9(08) COMP VALUE 0071.
       01  TAB-CRNG-CHAVE.
           03  TAB-CRNG-QT-CHAVES      PIC S9(08) COMP VALUE 0005.
      *--> Chave 01 - Tipo de Movimentacao
           03  TAB-CRNG-01-INICIO      PIC S9(08) COMP VALUE 0001.
           03  TAB-CRNG-01-TAMANHO     PIC S9(08) COMP VALUE 0002.
           03  TAB-CRNG-01-FORMATO     PIC S9(08) COMP VALUE +004.
      *--> Chave 02 - Ano do Recebimento Previsto
           03  TAB-CRNG-02-INICIO      PIC S9(08) COMP VALUE 0007.
           03  TAB-CRNG-02-TAMANHO     PIC S9(08) COMP VALUE 0004.
           03  TAB-CRNG-02-FORMATO     PIC S9(08) COMP VALUE +001.
      *--> Chave 03 - Mes do Recebimento Previsto
           03  TAB-CRNG-03-INICIO      PIC S9(08) COMP VALUE 0005.
           03  TAB-CRNG-03-TAMANHO     PIC S9(08) COMP VALUE 0002.
           03  TAB-CRNG-03-FORMATO     PIC S9(08) COMP VALUE +001.
      *--> Chave 04 - Dia do Recebimento Previsto
           03  TAB-CRNG-04-INICIO      PIC S9(08) COMP VALUE 0003.
           03  TAB-CRNG-04-TAMANHO     PIC S9(08) COMP VALUE 0002.
           03  TAB-CRNG-04-FORMATO     PIC S9(08) COMP VALUE +001.
      *--> Chave 05 - Timestamp do Registro de Pagamento
           03  TAB-CRNG-05-INICIO      PIC S9(08) COMP VALUE 0011.
           03  TAB-CRNG-05-TAMANHO     PIC S9(08) COMP VALUE 0026.
           03  TAB-CRNG-05-FORMATO     PIC S9(08) COMP VALUE +001.

      *--> Lista Sumarizada dos Realizados Vinculados por:
      *    Codigo do Tipo de Movimentacao Financeira
      *    Data de Recebimento Previsto
       01  REALIZADO-VINCULADO-CD-DT.
           03  LS-TIP-QT-REGISTRO      PIC S9(08) COMP VALUE ZEROS.
           03  LS-TIP                  OCCURS 0650 TIMES.
               05  LS-TIP-CD-TIP-RCBT-PRVT
                                       PIC S9(04) COMP-5 VALUE 9999.
               05  LS-TIP-DT-RCBT-PRVT
                                       PIC  X(08) VALUE '99999999'.
               05  LS-TIP-DT-RCBT-PRVT-R
                                       REDEFINES LS-TIP-DT-RCBT-PRVT.
                   07  DIA             PIC  9(02).
                   07  MES             PIC  9(02).
                   07  ANO             PIC  9(04).
               05  LS-TIP-VL-RLZD-VCLD
                                       PIC S9(15)V99 COMP-3 VALUE ZEROS.
               05  LS-TIP-VL-RCBT-PRVT
                                       PIC S9(15)V99 COMP-3 VALUE ZEROS.

      *--> Lista dos Vinculos (Recebimentos Realizados) recuperados
      *    na Tabela DB2SGT.RCBT_RLZD_OPR
      *    pelo programa SGTSL087
       01  DB2-VINCULADO.
           03  DB2-VCL-QT-REGISTRO    PIC S9(04) VALUE ZEROS.
           03  DB2-VCL                 OCCURS 0950 TIMES.
               05  DB2-VCL-CHV-TAB.
                   07  DB2-VCL-CD-TIP-RCBT-PRVT
                                       PIC S9(04) VALUE ZEROS.
                   07  DB2-VCL-DT-RCBT-PRVT
                                       PIC  X(08) VALUE SPACES.
                   07  DB2-VCL-DT-RCBT-PRVT-R
                                       REDEFINES DB2-VCL-DT-RCBT-PRVT.
                       09 DIA          PIC  9(02).
                       09 MES          PIC  9(02).
                       09 ANO          PIC  9(04).
                   07  DB2-VCL-TS-REG-PGTO
                                       PIC  X(26) VALUE SPACES.
               05  DB2-VCL-IN-VCL-MNL
                                       PIC  X(01) VALUE SPACES.
               05  DB2-VCL-CD-USU
                                       PIC  X(08) VALUE SPACES.
               05  DB2-VCL-TS-ULT-ATL
                                       PIC  X(26) VALUE SPACES.
               05  DB2-VCL-VL-RLZD-VCLD
                                       PIC S9(15)V99  COMP-3
                                       VALUE ZEROS.


      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      *--> Book do Container de Erro
-INC IIBKERRO
      * Book do Container do Usuário Logado
-INC IIBKOPRD
      *--> Book para captura de erro via MSG1
-INC MSGKWMG1
      *--> Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999
      *--> Book do container de requisicao e resposta deste modulo
       01  SGTS0160-DADOS.
-INC SGTK0160

      *-----------------------------------------------------------------
      *    Book de Subrotinas Utilizadas
      *-----------------------------------------------------------------

      *--> Consultar a tabela DB2SGT.OPR_ETRD_MCP
      *01  SGTSC005-DADOS.
-INC SGTKC005

      *--> Consultar a tabela DB2SGT.EST_VRS_OPR_ETRD
       01  SGTSC069-DADOS.
-INC SGTKC069

      *--> Consultar a tabela DB2SGT.RCBT_PRVT_OPR_ETRD
       01  SGTSC070-DADOS.
-INC SGTKC070

      *--> Alteração na tabela DB2SGT.RCBT_RLZD_OPR
      *01  SGTSA087-DADOS.
-INC SGTKA087

      *--> Exclusão a tabela DB2SGT.RCBT_RLZD_OPR
       01  SGTSE087-DADOS.
-INC SGTKE087

      *--> Inclusão na tabela DB2SGT.RCBT_RLZD_OPR
       01  SGTSI087-DADOS.
-INC SGTKI087

      *--> Lista da tabela DB2SGT.DMTV_CX_TIT
      *01  SGTSL086-DADOS.
-INC SGTKL086

      *--> Lista da tabela DB2SGT.RCBT_RLZD_OPR
       01  SGTSL087-DADOS.
-INC SGTKL087

      *--> Cálculo no novo SEQL, DB2SGT.RCBT_PRVT_OPR_ETRD
       01  SGTSPE07-DADOS.
-INC SGTKPE07

       01  SGTS0161-DADOS.
-INC SGTK0161

      *01  SGTS0162-DADOS.
-INC SGTK0162

-INC SGTK0081

      *--> CRUD da tabela DB2SGT.EST_OPR_ETRD
       01  SGTST009-DADOS.
-INC SGTKT009

       01  CTE-FINAL-SS                PIC  X(40) VALUE
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

           PERFORM  100000-OBTER-CANAL
           PERFORM  110000-EXCLUIR-CTNR-ERRO
           PERFORM  120000-RECEBER-REQUISICAO

           PERFORM  130000-VALIDAR-PARAMETRO

           PERFORM  200000-TRATAR-PREVISTO

           IF SGTS0160-QT-VCL > 0
      *        PERFORM  300000-VALIDAR-DEMONSTRATIVO
              PERFORM  400000-GRAVAR-VINCULOS
           END-IF

           PERFORM  999999-ENCERRAR
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-OBTER-CANAL              SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

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
       110000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER   ('PRM-ERRO'               )
                      CHANNEL     ( NM-CNL                  )
                      NOHANDLE
           END-EXEC
           IF  NOT (    EIBRESP  EQUAL  DFHRESP(NORMAL      )
                    OR  EIBRESP  EQUAL  DFHRESP(CONTAINERERR) )
               PERFORM  999002-TRATA-CONDITION-CICS
           END-IF
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-CCT-LIDO
           EXEC CICS  GET
                      CONTAINER   ('SGTS0160-RQSC'          )
                      CHANNEL     ( NM-CNL                  )
                      NODATA
                      FLENGTH     ( QT-CCT-LIDO             )
           END-EXEC

           EXEC CICS  GET
                      CONTAINER   ( 'SGTS0160-RQSC'         )
                      CHANNEL     ( NM-CNL                  )
                      FLENGTH     ( QT-CCT-LIDO             )
                      INTO        ( SGTS0160-RQSC           )
           END-EXEC
           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  131000-VALIDAR-ENTRADA
           PERFORM  132000-CONSULTAR-OPERACAO
           PERFORM  133000-VALIDAR-VERSAO
           PERFORM  134000-VALIDAR-INDICADOR

      *--> Ordenar e Validar, tabela de entrada do Recebimento previsto
           PERFORM  135000-TRATAR-VALOR-PREVISTO

      *--> Ordenar e Validar, tabela de entrada dos Vínculos Financeiros
           IF  SGTS0160-QT-VCL        > 0
               PERFORM  136000-VALIDAR-VINCULOS
                        VARYING  IC-TAB-01  FROM 1  BY 1
                          UNTIL  IC-TAB-01  >  SGTS0160-QT-VCL
           END-IF
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       131000-VALIDAR-ENTRADA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada
           IF  SGTS0160-CD-CTRA-TIT        EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0160-CD-CTRA-TIT        NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0160-CD-OPR-ETRD-MCP   NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0160-CD-OPR-ETRD-MCP   EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0160-CD-OPR-ETRD-MCP   NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0160-NR-VRS-OPR-ETRD    EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0160-NR-VRS-OPR-ETRD    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Devem existir pelo menos dois recebimentos previstos:
      *--> Um de Amortização de Capital e outro de Juros
           IF  SGTS0160-QT-RCBT-PRVT   <   2
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0397              TO  MSG1-CD-ERRO-TS
               MOVE  'Quantidade de recebimentos previstos da operação i
      -              'nválida.'        TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           ELSE
               IF  SGTS0160-QT-RCBT-PRVT GREATER 650
                   SET  ERRO-PROGRAMADO
                                       TO TRUE
                   MOVE 0997           TO MSG1-CD-ERRO-TS
                   MOVE SGTS0160-QT-RCBT-PRVT
                                       TO AUX-QT-RCBT-PRVT-TX
                   STRING 'Quantidade de lançamentos previstos enviada:'
                           AUX-QT-RCBT-PRVT-TX ' (capital + juros), aci'
                          'ma do limite máximo de 650.'
                                       DELIMITED BY SIZE
                                       INTO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           END-IF

           IF  SGTS0160-QT-VCL         GREATER 950
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0998               TO MSG1-CD-ERRO-TS
               MOVE SGTS0160-QT-RCBT-PRVT
                                       TO AUX-QT-VCL-TX
               STRING 'Quantidade de pagamentos vinculados enviada: '
                       AUX-QT-VCL-TX ' está acima do limite máximo de 95
      -               '0.'             DELIMITED BY SIZE
                                       INTO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0160-QT-TX-OBS      GREATER 100
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0999               TO MSG1-CD-ERRO-TS
               MOVE SGTS0160-QT-RCBT-PRVT
                                       TO AUX-QT-TX-OBS-TX
               STRING 'Quant. de textos de observação de lançam. extraor
      -               'd. enviada: ' AUX-QT-TX-OBS-TX ', acima do limite
      -               ' máximo de 100.'
                      DELIMITED BY SIZE
                                       INTO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       131000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       132000-CONSULTAR-OPERACAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

      *--> Acionar o SGTSC005 para validar e buscar dados
      *--> da Operação Estruturada
           MOVE  SPACES                TO  SGTSC005-DADOS
           INITIALIZE                      SGTSC005-DADOS
                                           REPLACING NUMERIC
                                           BY  ZEROS

           MOVE  SGTS0160-CD-CTRA-TIT
                                       TO  SGTSC005-CD-CTRA-TIT-VL
           MOVE  SGTS0160-CD-OPR-ETRD-MCP
                                       TO  SGTSC005-CD-OPR-ETRD-MCP
           MOVE  SGTS0160-NR-VRS-OPR-ETRD
                                       TO  SGTSC005-NR-VRS-OPR-ETRD

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSC005              TO  NM-PGM
           PERFORM  960000-EXECUTAR-MODULO

           MOVE  CORR  SGTSC005-DT-EVT-VCLD-VRS-R
                                       TO  DT-EVT-VCLD-VRS-AA-MM-DD-R

           .
       132000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       133000-VALIDAR-VERSAO           SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTSC069-DADOS

           MOVE  SGTS0160-CD-CTRA-TIT
                                       TO  SGTSC069-CD-CTRA-TIT-VL
           MOVE  SGTS0160-CD-OPR-ETRD-MCP
                                       TO  SGTSC069-CD-OPR-ETRD-MCP
           MOVE  SGTS0160-NR-VRS-OPR-ETRD
                                       TO  SGTSC069-NR-VRS-OPR-ETRD
           MOVE  SPACES
                                       TO  SGTSC069-TS-EST-VRS

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSC069              TO  NM-PGM
           PERFORM  960000-EXECUTAR-MODULO

           IF  SGTSC069-CD-TIP-EST-VRS     EQUAL  2 OR 4
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0666              TO  MSG1-CD-ERRO-TS
               MOVE  'Estado da versão não permite atualização de cronog
      -              'rama.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       133000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       134000-VALIDAR-INDICADOR        SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTS0081-DADOS

           MOVE  SGTS0160-CD-CTRA-TIT  TO  SGTS0081-CD-CTRA-TIT-VL
           MOVE  SGTS0160-CD-OPR-ETRD-MCP
                                       TO  SGTS0081-CD-OPR-ETRD
           MOVE  SGTS0160-NR-VRS-OPR-ETRD
                                       TO  SGTS0081-NR-VRS-OPR-ETRD

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTS0081              TO  NM-PGM
           PERFORM  960000-EXECUTAR-MODULO

           IF  SGTS0081-IN-HBLT-VNCL-PGTO  EQUAL  'N'
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0098              TO  MSG1-CD-ERRO-TS
               MOVE  'Atualização não permitida para o estado da operaçã
      -              'o.'              TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       134000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       135000-TRATAR-VALOR-PREVISTO   SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  135100-VALIDAR-VALOR-PREVISTO
                    VARYING  IC-TAB-01  FROM  1  BY  1
                      UNTIL  IC-TAB-01  >  SGTS0160-QT-RCBT-PRVT

      *--> Compara o Total dos Valores dos Recebimentos Previstos
      *     com o Total do Valor do Evento que ocasionou
      *     a alteração do Cronograma de Pagamento
           IF  TTL-VL-RCBT-PRVT            LESS
                                           SGTSC005-VL-EVT-VCLD-VRS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0667              TO  MSG1-CD-ERRO-TS
               MOVE  'Valor total do previsto menor que o valor de capit
      -              'al.'             TO  MSG1-TX-ERRO-TS
               SET  COM-ERRO-BAT-VL    TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  TTL-VL-RCBT-PRVT            GREATER
                                           SGTSC005-VL-EVT-VCLD-VRS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0668              TO  MSG1-CD-ERRO-TS
               MOVE  'Valor total do previsto maior que o valor de capit
      -              'al.'             TO  MSG1-TX-ERRO-TS
               SET COM-ERRO-BAT-VL     TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       135000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       135100-VALIDAR-VALOR-PREVISTO     SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  135110-VALIDAR-DATA-PREVISTA

      *--> Validar Tipo de Recebimento Previsto
      *    01 - Normal de Juros
      *    02 - Normal de Capital
      *    03 - Extraordinario de Juros
      *    04 - Extraordinario de Capital
           IF SGTS0160-CD-TIP-RCBT-PRVT (IC-TAB-01)     < 1 OR > 4
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0109              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do tipo do recebimento previsto inválido.'
                                       TO  MSG1-TX-ERRO-TS

               SET  COM-ERRO-TAB-RCBT-PRVT  TO TRUE
               MOVE 'IC-TAB-01'        TO NM-IC-TAB-RCBT-PRVT
               MOVE  IC-TAB-01         TO VL-IC-TAB-RCBT-PRVT

               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Valida o Valor de Recebimento Previsto
      *    Valores Normal de Capital e Extraordinario de Capital
      *    devem possuir o Valor de Recebimento Previsto preenchido
           MOVE  SGTS0160-CD-TIP-RCBT-PRVT (IC-TAB-01)
                                       TO  CD-TIP-RCBT-PRVT
           MOVE  CORR  SGTS0160-DT-RCBT-PRVT-R (IC-TAB-01)
                                       TO  DT-RCBT-PRVT-AA-MM-DD-R

           IF  (PREVISTO-NORMAL-CAPITAL OR
               PREVISTO-EXTRA-CAPITAL) AND
               SGTS0160-VL-RCBT-PRVT (IC-TAB-01)
                                           NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0126              TO  MSG1-CD-ERRO-TS
               MOVE  'Valor previsto de recebimento inválido.'
                                       TO  MSG1-TX-ERRO-TS

               SET  COM-ERRO-TAB-RCBT-PRVT  TO TRUE
               MOVE 'IC-TAB-01'        TO NM-IC-TAB-RCBT-PRVT
               MOVE  IC-TAB-01         TO VL-IC-TAB-RCBT-PRVT

               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  (PREVISTO-NORMAL-CAPITAL OR PREVISTO-EXTRA-CAPITAL)  AND
               DT-RCBT-PRVT-AA-MM-DD  NOT LESS  DT-EVT-VCLD-VRS-AA-MM-DD

               ADD   SGTS0160-VL-RCBT-PRVT (IC-TAB-01)
                                       TO TTL-VL-RCBT-PRVT
                     ON SIZE ERROR
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9011        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro na totalização do valor de recebimento
      -                    'previsto.' TO  MSG1-TX-ERRO-TS

                     SET  COM-ERRO-TAB-RCBT-PRVT TO TRUE
                     MOVE 'IC-TAB-01'            TO NM-IC-TAB-RCBT-PRVT
                     MOVE  IC-TAB-01             TO VL-IC-TAB-RCBT-PRVT

                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               END-ADD
           END-IF
           .
       135100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       135110-VALIDAR-DATA-PREVISTA     SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           MOVE  4095                  TO  AREA-TAM
           MOVE  'SBDATAF -WRKAREA-FIM'
                                       TO  AREA-FIM
           MOVE  CORR  SGTS0160-DT-RCBT-PRVT-R(IC-TAB-01)
                                       TO  ARG01-R
      *==     MOVE 4                      TO  ARG02

           CALL  SBDATAF            USING  FUNCAO
                                           WKSBDATA
                                           ARG01
                                           ARG02

      *--> RETURN-CODE do SBDATAF
      *    22 -  Conteudo do Argumento Nao Numerico
      *    24 -  Data Informada Invalida
      *    46 -  Data Fora da Faixa de 01/01/0001 a 31/12/9999

           IF  RETURN-CODE                 NOT EQUAL  ZEROS  AND
                                           0022  AND  0024   AND 0046
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  0051              TO  MSG1-CD-ERRO-TS
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  'Erro no tratamento de datas.'
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
               MOVE  SBDATAF           TO  MSG1-NM-PGM-ACND

               SET  COM-ERRO-TAB-RCBT-PRVT  TO TRUE
               MOVE 'IC-TAB-01'        TO NM-IC-TAB-RCBT-PRVT
               MOVE  IC-TAB-01         TO VL-IC-TAB-RCBT-PRVT

               MOVE 'ARG01'            TO  MSGKW999-NM-VRV-MNT
               MOVE ARG01              TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'ARG02'            TO  MSGKW999-NM-VRV-MNT
               MOVE ARG02              TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'AREA-TAM'         TO  MSGKW999-NM-VRV-MNT
               MOVE AREA-TAM           TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'AREA-FIM'         TO  MSGKW999-NM-VRV-MNT
               MOVE AREA-FIM           TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               PERFORM  999000-GRAVA-ERRO-E-ENCERRA

           END-IF

           IF  RETURN-CODE                 EQUAL  0022  OR
                                                  0024  OR 0046
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  0049              TO  MSG1-CD-ERRO-TS
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  'Data informada inválida.'
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
               MOVE  SBDATAF           TO  MSG1-NM-PGM-ACND

               SET  COM-ERRO-TAB-RCBT-PRVT  TO TRUE
               MOVE 'IC-TAB-01'        TO NM-IC-TAB-RCBT-PRVT
               MOVE  IC-TAB-01         TO VL-IC-TAB-RCBT-PRVT

               MOVE 'ARG01'            TO  MSGKW999-NM-VRV-MNT
               MOVE ARG01              TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'ARG02'            TO  MSGKW999-NM-VRV-MNT
               MOVE ARG02              TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'AREA-TAM'         TO  MSGKW999-NM-VRV-MNT
               MOVE AREA-TAM           TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'AREA-FIM'         TO  MSGKW999-NM-VRV-MNT
               MOVE AREA-FIM           TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       135110-SAI.
           EXIT.

      *-----------------------------------------------------------------
       136000-VALIDAR-VINCULOS         SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar a tabela ordenada dos vínculos financeiros
      *    de um Recebimento Previsto
      *    O Numero do Item de Cronogram deve variar na faixa de 01
      *    até quantidade de ocorrencias do Recebimento Previsto
           IF  SGTS0160-NR-ITEM-CRNG (IC-TAB-01) < 1  AND
               SGTS0160-NR-ITEM-CRNG (IC-TAB-01) > SGTS0160-QT-RCBT-PRVT
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0669              TO  MSG1-CD-ERRO-TS
               MOVE  'Número de item de cronograma inválido.'
                                       TO  MSG1-TX-ERRO-TS

               SET  COM-ERRO-TAB-VCL   TO TRUE
               MOVE 'IC-TAB-01'        TO NM-IC-TAB-VCL
               MOVE  IC-TAB-01         TO VL-IC-TAB-VCL
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0160-TS-REG-PGTO (IC-TAB-01)   EQUAL SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0670              TO  MSG1-CD-ERRO-TS
               MOVE  'Timestamp do Registro de Pagamento inválido.'
                                       TO  MSG1-TX-ERRO-TS

               SET  COM-ERRO-TAB-VCL   TO TRUE
               MOVE 'IC-TAB-01'        TO NM-IC-TAB-VCL
               MOVE  IC-TAB-01         TO VL-IC-TAB-VCL

               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0160-VL-RLZD-VCLD (IC-TAB-01)  NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0452              TO  MSG1-CD-ERRO-TS
               MOVE  'Valor Realizado Inválido.'
                                       TO  MSG1-TX-ERRO-TS

               SET  COM-ERRO-TAB-VCL   TO TRUE
               MOVE 'IC-TAB-01'        TO NM-IC-TAB-VCL
               MOVE  IC-TAB-01         TO VL-IC-TAB-VCL

               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       136000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-TRATAR-PREVISTO          SECTION.
      *--> Aciona o Modulo SGTS0161 para validar e gravar tabela de
      *    recebimento previsto
      *-----------------------------------------------------------------
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimenta Operação Estruturada
           MOVE  SGTS0160-CD-CTRA-TIT
                                       TO  SGTS0161-CD-CTRA-TIT
           MOVE  SGTS0160-CD-OPR-ETRD-MCP
                                       TO  SGTS0161-CD-OPR-ETRD-MCP
           MOVE  SGTS0160-NR-VRS-OPR-ETRD
                                       TO  SGTS0161-NR-VRS-OPR-ETRD

      *--> Calcula e Movimenta Sequencial do Cronograma
           PERFORM   210000-CALCULAR-NOVO-SEQL
           MOVE  NR-SEQL-RCBT-PRVT     TO  SGTS0161-NR-SEQL-RCBT-PRVT

      *--> Movimenta Datas de Emissao e Vencimento
           MOVE  CORR  SGTSC005-DT-EMS-TIT-OPR-R
                                       TO  SGTS0161-DT-EMS-OPR-R
           MOVE  CORR  SGTSC005-DT-VNCT-OPR-R
                                       TO  SGTS0161-DT-VNCT-OPR-R

      *--> Movimenta Valor do Evento
           MOVE  SGTSC005-VL-EVT-VCLD-VRS
                                       TO  SGTS0161-VL-EVT-VCLD-VRS

      *--> Movimenta Quantidades de Ocorrencias
           MOVE  SGTS0160-QT-RCBT-PRVT
                                       TO  SGTS0161-QT-RCBT-PRVT
           MOVE  SGTS0160-QT-TX-OBS    TO  SGTS0161-QT-TX-OBS


           PERFORM  220000-MONTAR-RCBT-PRVT
                    VARYING                IC-TAB-01
                                           FROM  1  BY  1
                    UNTIL                  IC-TAB-01
                                           GREATER SGTS0160-QT-RCBT-PRVT

           PERFORM  230000-MONTAR-TX-OBS
                    VARYING                IC-TAB-01
                                           FROM  1  BY  1
                    UNTIL                  IC-TAB-01
                                           GREATER SGTS0160-QT-TX-OBS

           MOVE  SGTS0161              TO  NM-PGM

           PERFORM  960000-EXECUTAR-MODULO
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-CALCULAR-NOVO-SEQL       SECTION.
      *--> Aciona o Programa SGTSPE07 para buscar o último Sequencial
      *    de uma Operacao Estruturada
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

      *--> Consulta o Ultimo Sequencial do Cronograma (Sequencial Atual)
           MOVE  SGTS0160-CD-CTRA-TIT  TO  SGTSPE07-CD-CTRA-TIT
           MOVE  SGTS0160-CD-OPR-ETRD-MCP
                                       TO  SGTSPE07-CD-OPR-ETRD-MCP
           MOVE  SGTS0160-NR-VRS-OPR-ETRD
                                       TO  SGTSPE07-NR-VRS-OPR-ETRD

           MOVE  SGTSPE07              TO  NM-PGM

           PERFORM  960000-EXECUTAR-MODULO

           MOVE  SGTSPE07-NR-SEQL-RCBT-PRVT
                                       TO  NR-SEQL-RCBT-PRVT

           MOVE  'N'                   TO  SGTS0161-IN-NOVO-CRNG

      *--> Se o Estado da Versao da Operacao = 3 - APROVADA
           IF  SGTSC069-CD-TIP-EST-VRS    EQUAL  0003  OR
               NR-SEQL-RCBT-PRVT          EQUAL  ZEROS
      *--> Sequencial Novo = Ultimo Sequencial + 0001
               ADD  0001               TO  NR-SEQL-RCBT-PRVT
                    ON SIZE ERROR
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  0252         TO  MSG1-CD-ERRO-TS
                    MOVE  'Erro no cálculo do número sequencial.'
                                       TO  MSG1-TX-ERRO-TS
                    PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               END-ADD
               SET SGTS0161-CRONOGRAMA-NOVO
                                       TO TRUE
           END-IF

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-MONTAR-RCBT-PRVT         SECTION.
      *-----------------------------------------------------------------
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTS0160-CD-TIP-RCBT-PRVT                   (IC-TAB-01)
                             TO  SGTS0161-CD-TIP-RCBT-PRVT   (IC-TAB-01)
           MOVE  SGTS0160-DT-RCBT-PRVT                       (IC-TAB-01)
                             TO  SGTS0161-DT-RCBT-PRVT       (IC-TAB-01)
           MOVE  SGTS0160-VL-RCBT-PRVT                       (IC-TAB-01)
                             TO  SGTS0161-VL-RCBT-PRVT       (IC-TAB-01)
           MOVE  SGTS0160-NR-ITEM-TX-OBS                     (IC-TAB-01)
                             TO  SGTS0161-NR-ITEM-TX-OBS     (IC-TAB-01)
           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-MONTAR-TX-OBS            SECTION.
      *-----------------------------------------------------------------
           MOVE  0017                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTS0160-TX-OBS-RCBT-PRVT                   (IC-TAB-01)
                             TO  SGTS0161-TX-OBS-RCBT-PRVT   (IC-TAB-01)
           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-VALIDAR-DEMONSTRATIVO    SECTION.
      *-----------------------------------------------------------------
           MOVE  0018                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTS0160-CD-CTRA-TIT
                                       TO  SGTS0162-CD-CTRA-TIT-VL
           MOVE  SGTS0160-CD-OPR-ETRD-MCP
                                       TO  SGTS0162-CD-OPR-ETRD-MCP
           MOVE  SGTS0160-NR-VRS-OPR-ETRD
                                       TO  SGTS0162-NR-VRS-OPR-ETRD
           MOVE  SGTS0160-QT-VCL       TO  SGTS0162-QT-OCR-TAB-PGTO

           PERFORM  310000-MONTAR-RCBT-RLZD
                    VARYING                IC-TAB-01
                                           FROM  0001  BY  0001
                    UNTIL                  IC-TAB-01
                                           GREATER SGTS0160-QT-VCL

           MOVE  SGTS0162              TO  NM-PGM

           PERFORM  960000-EXECUTAR-MODULO

           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       310000-MONTAR-RCBT-RLZD  SECTION.
      *-----------------------------------------------------------------
           MOVE  0019                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTS0160-TS-REG-PGTO                        (IC-TAB-01)
                             TO  SGTS0162-TS-REG-PGTO        (IC-TAB-01)
           MOVE  SGTS0160-VL-RLZD-VCLD                       (IC-TAB-01)
                             TO  SGTS0162-VL-RLZD-VCLD       (IC-TAB-01)
           .
       310000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       400000-GRAVAR-VINCULOS          SECTION.
      *-----------------------------------------------------------------
           MOVE  0020                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  420000-MONTAR-TAB-CRONOGRAMA

           PERFORM  970000-LER-DADOS-OPRD

           IF  SGTS0161-CRONOGRAMA-NOVO
               PERFORM  430000-INCLUIR-REALIZADO
           ELSE
               PERFORM  440000-ALTERAR-REALIZADO
           END-IF
           .
       400000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       420000-MONTAR-TAB-CRONOGRAMA    SECTION.
      *-----------------------------------------------------------------
           MOVE  0021                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTS0160-QT-VCL       TO  TAB-CRNG-QT-REGISTRO

      *--> Monta a lista para ordenação do SBSORT
           PERFORM  421000-MOVER-VINCULOS
                    VARYING  IC-TAB-CRNG FROM  1 BY 1
                      UNTIL  IC-TAB-CRNG > 0950

           CALL  SBSORT             USING  CRONOGRAMA
                                           TAB-CRNG-QT-REGISTRO
                                           TAB-CRNG-TAMANHO
                                           TAB-CRNG-CHAVE

           IF  RETURN-CODE                 NOT EQUAL  ZEROS
               SET ERRO-PROGRAMA-ACIONADO  TO  TRUE
               MOVE RETURN-CODE            TO  MSG1-CD-RTN-PGM-ACND
               MOVE 'SBSORT'               TO  MSG1-NM-PGM-ACND
               MOVE 0536                   TO  MSG1-CD-ERRO-TS
               MOVE  'Erro na execução da ordenação da lista de cronogra
      -              'ma.'                 TO  MSG1-TX-ERRO-TS

               MOVE 'TAB-CRNG-QT-REGISTRO'
                                           TO MSGKW999-NM-VRV-MNT
               MOVE TAB-CRNG-QT-REGISTRO
                                           TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'TAB-CRNG-TAMANHO'     TO MSGKW999-NM-VRV-MNT
               MOVE TAB-CRNG-TAMANHO       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'TAB-CRNG-QT-CHAVES'   TO MSGKW999-NM-VRV-MNT
               MOVE TAB-CRNG-QT-CHAVES     TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'TAB-CRNG-01-INICIO'   TO MSGKW999-NM-VRV-MNT
               MOVE TAB-CRNG-01-INICIO     TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'TAB-CRNG-01-TAMANHO'  TO MSGKW999-NM-VRV-MNT
               MOVE TAB-CRNG-01-TAMANHO    TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'TAB-CRNG-01-FORMATO'  TO MSGKW999-NM-VRV-MNT
               MOVE TAB-CRNG-01-FORMATO    TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'TAB-CRNG-02-INICIO'   TO MSGKW999-NM-VRV-MNT
               MOVE TAB-CRNG-02-INICIO     TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'TAB-CRNG-02-TAMANHO'  TO MSGKW999-NM-VRV-MNT
               MOVE TAB-CRNG-02-TAMANHO    TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'TAB-CRNG-02-FORMATO'  TO MSGKW999-NM-VRV-MNT
               MOVE TAB-CRNG-02-FORMATO    TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'TAB-CRNG-03-INICIO'   TO MSGKW999-NM-VRV-MNT
               MOVE TAB-CRNG-03-INICIO     TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'TAB-CRNG-03-TAMANHO'  TO MSGKW999-NM-VRV-MNT
               MOVE TAB-CRNG-03-TAMANHO    TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'TAB-CRNG-03-FORMATO'  TO MSGKW999-NM-VRV-MNT
               MOVE TAB-CRNG-03-FORMATO    TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'TAB-CRNG-04-INICIO'   TO MSGKW999-NM-VRV-MNT
               MOVE TAB-CRNG-04-INICIO     TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'TAB-CRNG-04-TAMANHO'  TO MSGKW999-NM-VRV-MNT
               MOVE TAB-CRNG-04-TAMANHO    TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'TAB-CRNG-04-FORMATO'  TO MSGKW999-NM-VRV-MNT
               MOVE TAB-CRNG-04-FORMATO    TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'TAB-CRNG-05-INICIO'   TO MSGKW999-NM-VRV-MNT
               MOVE TAB-CRNG-05-INICIO     TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'TAB-CRNG-05-TAMANHO'  TO MSGKW999-NM-VRV-MNT
               MOVE TAB-CRNG-05-TAMANHO    TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'TAB-CRNG-05-FORMATO'  TO MSGKW999-NM-VRV-MNT
               MOVE TAB-CRNG-05-FORMATO    TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           PERFORM  422000-AGRUPAR-VINCULOS
                    VARYING  IC-TAB-03 FROM 1 BY 1
                    UNTIL    IC-TAB-03 > TAB-CRNG-QT-REGISTRO

      *--> Verifica os vínculos da lista de entrada
           PERFORM  423000-VALIDAR-VINCULOS
                    VARYING  IC-TAB-01 FROM 1 BY 1
                      UNTIL  IC-TAB-01 > LS-TIP-QT-REGISTRO
           .
       420000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       421000-MOVER-VINCULOS           SECTION.
      *-----------------------------------------------------------------
           MOVE  0022                  TO  MSG1-CD-SEQL-ERRO

           IF  IC-TAB-CRNG                 GREATER  TAB-CRNG-QT-REGISTRO
               MOVE  9999
                 TO  TAB-CRNG-CD-TIP-RCBT-PRVT             (IC-TAB-CRNG)
               MOVE  '99999999'
                 TO  TAB-CRNG-DT-RCBT-PRVT                 (IC-TAB-CRNG)
               MOVE  '999999999999'
                 TO  TAB-CRNG-CD-TIT-OPR-ETRD              (IC-TAB-CRNG)
               MOVE  '9999'
                 TO  TAB-CRNG-NR-DVS-TIT                   (IC-TAB-CRNG)
               MOVE  '9999-99-99-99.99.99.999999'
                 TO  TAB-CRNG-TS-REG-PGTO                  (IC-TAB-CRNG)
               MOVE  99999999999999
                 TO  TAB-CRNG-VL-RLZD-VCLD                 (IC-TAB-CRNG)
           ELSE
               MOVE  SGTS0160-NR-ITEM-CRNG                 (IC-TAB-CRNG)
                 TO  IC-TAB-01
               MOVE  SGTS0160-CD-TIP-RCBT-PRVT               (IC-TAB-01)
                 TO  TAB-CRNG-CD-TIP-RCBT-PRVT             (IC-TAB-CRNG)
               MOVE  SGTS0160-DT-RCBT-PRVT                   (IC-TAB-01)
                 TO  TAB-CRNG-DT-RCBT-PRVT                 (IC-TAB-CRNG)
               MOVE  SGTSC005-CD-TIT-OPR-ETRD
                 TO  TAB-CRNG-CD-TIT-OPR-ETRD              (IC-TAB-CRNG)
               MOVE  SGTSC005-NR-DVS-TIT
                 TO  TAB-CRNG-NR-DVS-TIT                   (IC-TAB-CRNG)
               MOVE  SGTS0160-TS-REG-PGTO                  (IC-TAB-CRNG)
                 TO  TAB-CRNG-TS-REG-PGTO                  (IC-TAB-CRNG)
               MOVE  'S'
                 TO  TAB-CRNG-IN-VCL-MNL                   (IC-TAB-CRNG)
               MOVE  SGTS0160-VL-RLZD-VCLD                 (IC-TAB-CRNG)
                 TO  TAB-CRNG-VL-RLZD-VCLD                 (IC-TAB-CRNG)
               MOVE  SGTS0160-VL-RCBT-PRVT                   (IC-TAB-01)
                 TO  TAB-CRNG-VL-RCBT-PRVT                 (IC-TAB-CRNG)
           END-IF
           .
       421000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       422000-AGRUPAR-VINCULOS         SECTION.
      *--> Sumarizar os Vinculos Financeiros por
      *    Tipo da Movimentacao do Recebimento Previsto,
      *    Data do Recebimento Previsto e Timestamp registro pagamento
      *-----------------------------------------------------------------
           MOVE  0023                  TO  MSG1-CD-SEQL-ERRO

           IF TAB-CRNG-CHV-TAB (IC-TAB-03)
                                       NOT EQUAL AUX-TAB-CRNG-CHV-TAB
              MOVE TAB-CRNG-CHV-TAB (IC-TAB-03)
                                       TO AUX-TAB-CRNG-CHV-TAB
              ADD 1                    TO LS-TIP-QT-REGISTRO
              MOVE TAB-CRNG-CD-TIP-RCBT-PRVT (IC-TAB-03)        TO
                   LS-TIP-CD-TIP-RCBT-PRVT (LS-TIP-QT-REGISTRO)
              MOVE TAB-CRNG-DT-RCBT-PRVT     (IC-TAB-03)        TO
                   LS-TIP-DT-RCBT-PRVT     (LS-TIP-QT-REGISTRO)
              MOVE TAB-CRNG-VL-RLZD-VCLD     (IC-TAB-03)        TO
                   LS-TIP-VL-RLZD-VCLD     (LS-TIP-QT-REGISTRO)
              MOVE TAB-CRNG-VL-RCBT-PRVT     (IC-TAB-03)        TO
                   LS-TIP-VL-RCBT-PRVT       (LS-TIP-QT-REGISTRO)
           ELSE
              ADD  TAB-CRNG-VL-RLZD-VCLD     (IC-TAB-03)        TO
                   LS-TIP-VL-RLZD-VCLD     (LS-TIP-QT-REGISTRO)
           END-IF
           .
       422000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       423000-VALIDAR-VINCULOS         SECTION.
      *--> Validar a tabela ordenada dos vínculos financeiros
      *    de um Recebimento Previsto
      *-----------------------------------------------------------------
           MOVE  0024                  TO  MSG1-CD-SEQL-ERRO

           IF  (LS-TIP-CD-TIP-RCBT-PRVT   (IC-TAB-01) EQUAL 2 OR 4)
            AND (LS-TIP-VL-RLZD-VCLD      (IC-TAB-01) NOT EQUAL
               LS-TIP-VL-RCBT-PRVT        (IC-TAB-01))
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0672              TO  MSG1-CD-ERRO-TS
               MOVE  'Valor total do realizado vinculado inválido.'
                                       TO  MSG1-TX-ERRO-TS

               SET  COM-ERRO-TAB-REALIZADO  TO TRUE
               MOVE 'IC-TAB-01'        TO NM-IC-TAB-REALIZADO
               MOVE  IC-TAB-01         TO VL-IC-TAB-REALIZADO

               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       423000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       430000-INCLUIR-REALIZADO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0025                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  930000-INCLUIR-VINCULO
                    VARYING                IC-TAB-02
                                           FROM  1  BY 1
                    UNTIL                  IC-TAB-02
                                           GREATER  TAB-CRNG-QT-REGISTRO
           .
       430000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       440000-ALTERAR-REALIZADO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0026                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  441000-LISTAR-VINCULOS

           MOVE  1                     TO  IC-TAB-01
           MOVE  1                     TO  IC-TAB-02

           PERFORM  442000-ALTERAR-VINCULO
                    UNTIL  IC-TAB-01  >  DB2-VCL-QT-REGISTRO
                      AND  IC-TAB-02  >  TAB-CRNG-QT-REGISTRO
           .
       440000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       441000-LISTAR-VINCULOS          SECTION.
      *-----------------------------------------------------------------
           MOVE  0027                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SPACES                TO  SGTSL087-TS-REG-PGTO-ULT
                                           SGTSL087-RQSC
           INITIALIZE                      SGTSL087-RQSC
                                           REPLACING NUMERIC
                                           BY  ZEROS
           INITIALIZE                      SGTSL087-QT-OCR
                                           SGTSL087-IN-CTNC-PSQ

           PERFORM  940000-LISTAR-VINCULOS
           MOVE  ZEROS                 TO  IC-TAB-02

           IF  SGTSL087-QT-OCR         GREATER  ZEROS
               PERFORM  441100-MOVER-REALIZADO
                        VARYING  IC-TAB-01  FROM  1  BY  1
                          UNTIL  IC-TAB-01  > SGTSL087-QT-OCR

               PERFORM  UNTIL IC-TAB-02    EQUAL 0950
                        OR NOT SGTSL087-TEM-MAIS-REGISTRO
                        PERFORM  441200-MONTAR-CHAVE
                        PERFORM  940000-LISTAR-VINCULOS
                        PERFORM  441100-MOVER-REALIZADO
                                 VARYING IC-TAB-01 FROM  1  BY  1
                                   UNTIL IC-TAB-01 > SGTSL087-QT-OCR
                        PERFORM  441200-MONTAR-CHAVE
               END-PERFORM
           END-IF
           .
       441000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       441100-MOVER-REALIZADO          SECTION.
      *-----------------------------------------------------------------
           MOVE  0028                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  952000-INCREMENTAR-IC-TAB-02

           MOVE  SGTSL087-CD-TIP-RCBT-PRVT  (IC-TAB-01)
                             TO  DB2-VCL-CD-TIP-RCBT-PRVT  (IC-TAB-02)
           MOVE  SGTSL087-DT-RCBT-PRVT      (IC-TAB-01)
                             TO  DB2-VCL-DT-RCBT-PRVT      (IC-TAB-02)
           MOVE  SGTSL087-TS-REG-PGTO       (IC-TAB-01)
                             TO  DB2-VCL-TS-REG-PGTO       (IC-TAB-02)
           MOVE  SGTSL087-IN-VCL-MNL        (IC-TAB-01)
                             TO  DB2-VCL-IN-VCL-MNL        (IC-TAB-02)
           MOVE  SGTSL087-CD-USU            (IC-TAB-01)
                             TO  DB2-VCL-CD-USU            (IC-TAB-02)
           MOVE  SGTSL087-TS-ULT-ATL        (IC-TAB-01)
                             TO  DB2-VCL-TS-ULT-ATL        (IC-TAB-02)
           MOVE  SGTSL087-VL-RLZD-VCLD      (IC-TAB-01)
                             TO  DB2-VCL-VL-RLZD-VCLD      (IC-TAB-02)
           .
       441100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       441200-MONTAR-CHAVE             SECTION.
      *-----------------------------------------------------------------
           MOVE  0029                  TO  MSG1-CD-SEQL-ERRO

           MOVE SGTSL087-CD-TIP-RCBT-PRVT (SGTSL087-QT-OCR)
                                       TO SGTSL087-CD-TIP-RCBT-PRVT-ULT
           MOVE SGTSL087-DT-RCBT-PRVT (SGTSL087-QT-OCR)
                                       TO  SGTSL087-DT-RCBT-PRVT-ULT
           MOVE SGTSL087-TS-REG-PGTO  (SGTSL087-QT-OCR)
                                       TO  SGTSL087-TS-REG-PGTO-ULT
           .
       441200-SAI.
           EXIT.

      *-----------------------------------------------------------------
       442000-ALTERAR-VINCULO          SECTION.
      *-----------------------------------------------------------------
           MOVE  0030                  TO  MSG1-CD-SEQL-ERRO

           MOVE CORR DB2-VCL-DT-RCBT-PRVT-R  (IC-TAB-01)
                                       TO   DT-DB2-RLZD-AA-MM-DD-R

           MOVE CORR TAB-CRNG-DT-RCBT-PRVT-R (IC-TAB-02)
                                       TO   DT-TAB-RLZD-AA-MM-DD-R

           IF  IC-TAB-01  <=  DB2-VCL-QT-REGISTRO  AND
               IC-TAB-02  <=  TAB-CRNG-QT-REGISTRO
               IF  DB2-VCL-CD-TIP-RCBT-PRVT  (IC-TAB-01) =
                   TAB-CRNG-CD-TIP-RCBT-PRVT (IC-TAB-02)             AND
                   DT-DB2-RLZD-AA-MM-DD     = DT-TAB-RLZD-AA-MM-DD   AND
                   DB2-VCL-TS-REG-PGTO      (IC-TAB-01) =
                   TAB-CRNG-TS-REG-PGTO     (IC-TAB-02)
                   IF DB2-VCL-VL-RLZD-VCLD  (IC-TAB-01) NOT EQUAL
                      TAB-CRNG-VL-RLZD-VCLD (IC-TAB-02)
                      PERFORM  910000-ALTERAR-VINCULO
                   END-IF
                   PERFORM  951000-INCREMENTAR-IC-TAB-01
                   PERFORM  952000-INCREMENTAR-IC-TAB-02

               ELSE
                   IF  DB2-VCL-CD-TIP-RCBT-PRVT  (IC-TAB-01) =
                       TAB-CRNG-CD-TIP-RCBT-PRVT (IC-TAB-02) AND
                       DT-DB2-RLZD-AA-MM-DD   =  DT-TAB-RLZD-AA-MM-DD
      *--------------> Significa que os Timestamp são diferentes
                       IF  DB2-VCL-TS-REG-PGTO       (IC-TAB-01) GREATER
                           TAB-CRNG-TS-REG-PGTO       (IC-TAB-02)
      *------------------> Incluir realização de recebimento
                           PERFORM  930000-INCLUIR-VINCULO
                           PERFORM  952000-INCREMENTAR-IC-TAB-02

                       ELSE
      *------------------> Excluir realização de recebimento
                           PERFORM  920000-EXCLUIR-VINCULO
                           PERFORM  951000-INCREMENTAR-IC-TAB-01
                       END-IF

                   ELSE
                     IF  DB2-VCL-CD-TIP-RCBT-PRVT  (IC-TAB-01) =
                         TAB-CRNG-CD-TIP-RCBT-PRVT (IC-TAB-02)
                       IF  DT-DB2-RLZD-AA-MM-DD   >
                           DT-TAB-RLZD-AA-MM-DD
      *-------------------> Incluir realização de recebimento
                            PERFORM  930000-INCLUIR-VINCULO
                            PERFORM  952000-INCREMENTAR-IC-TAB-02
                       ELSE
      *-------------------> Excluir Registro Recebimento Realizado
                            PERFORM  920000-EXCLUIR-VINCULO
                            PERFORM  951000-INCREMENTAR-IC-TAB-01
                       END-IF
                     ELSE
                       IF  DB2-VCL-CD-TIP-RCBT-PRVT  (IC-TAB-01) >
                           TAB-CRNG-CD-TIP-RCBT-PRVT (IC-TAB-02)
      *-------------------> Incluir realização de recebimento
                           PERFORM  930000-INCLUIR-VINCULO
                           PERFORM  952000-INCREMENTAR-IC-TAB-02
                       ELSE
      *-------------------> Excluir Registro Recebimento Realizado
                            PERFORM  920000-EXCLUIR-VINCULO
                            PERFORM  951000-INCREMENTAR-IC-TAB-01
                       END-IF
                     END-IF
                   END-IF
               END-IF
      *--> No caso de chegar no final da Tabela de Entrada ou
      *    no fim dos Registros na Tabela DB2SGT.RCBT_RLZD_OPR
      *    interrompe o Balance Line
           ELSE
      *--> No caso de chegar no fim da Tabela DB2SGT.RCBT_RLZD_OPR,
      *    Incluir os Ultimos Registros de Entrada
               IF  IC-TAB-01  >  DB2-VCL-QT-REGISTRO
                   PERFORM  930000-INCLUIR-VINCULO
                   PERFORM  952000-INCREMENTAR-IC-TAB-02
               ELSE
      *--> No caso de chegar no final da Tabela de Entrada, Excluir
      *    os Ultimos Registros da Tabela DB2SGT.RCBT_RLZD_OPR
                 IF  IC-TAB-02  >  TAB-CRNG-QT-REGISTRO
                     PERFORM  920000-EXCLUIR-VINCULO
                     PERFORM  951000-INCREMENTAR-IC-TAB-01
                 END-IF
               END-IF
           END-IF
           .
       442000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       910000-ALTERAR-VINCULO          SECTION.
      *--> Acionar o SGTSA070 para alterar registro
      *    na Tabela RCBT_PRVT_OPR_ETRD
      *-----------------------------------------------------------------
           MOVE  0031                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SPACES                TO  SGTSA087-DADOS
           INITIALIZE                      SGTSA087-DADOS
                                           REPLACING NUMERIC
                                           BY  ZEROS
      *--> Dados da Operação Estruturada
           MOVE  SGTS0160-CD-CTRA-TIT
                                       TO  SGTSA087-CD-CTRA-TIT-VL
           MOVE  SGTS0160-CD-OPR-ETRD-MCP
                                       TO  SGTSA087-CD-OPR-ETRD-MCP
           MOVE  SGTS0160-NR-VRS-OPR-ETRD
                                       TO  SGTSA087-NR-VRS-OPR-ETRD

      *--> Movimenta Chave do Recebimento Previsto
           MOVE  NR-SEQL-RCBT-PRVT     TO  SGTSA087-NR-SEQL-RCBT-PRVT

           MOVE  TAB-CRNG-CD-TIP-RCBT-PRVT                   (IC-TAB-02)
                                       TO  SGTSA087-CD-TIP-RCBT-PRVT
           MOVE  TAB-CRNG-DT-RCBT-PRVT                       (IC-TAB-02)
                                       TO  SGTSA087-DT-RCBT-PRVT

      *--> Movimenta Chave do Demonstrativo de Caixa
           MOVE  TAB-CRNG-CD-TIT-OPR-ETRD                    (IC-TAB-02)
                                       TO  SGTSA087-CD-TIT-OPR-ETRD
           MOVE  TAB-CRNG-NR-DVS-TIT                         (IC-TAB-02)
                                       TO  SGTSA087-NR-DVS-TIT
           MOVE  TAB-CRNG-TS-REG-PGTO                        (IC-TAB-02)
                                       TO  SGTSA087-TS-REG-PGTO

      *--> Movimenta Recebimento Realizado
           MOVE  TAB-CRNG-IN-VCL-MNL                         (IC-TAB-02)
                                       TO  SGTSA087-IN-VCL-MNL
           MOVE  KOPRD-CD-OPRD
                                       TO  SGTSA087-CD-USU-VCL
           MOVE  CURRENT
                                       TO  SGTSA087-TS-ULT-ATL
           MOVE  TAB-CRNG-VL-RLZD-VCLD                       (IC-TAB-02)
                                       TO  SGTSA087-VL-RLZD-VCLD

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSA087              TO  NM-PGM

           PERFORM 960000-EXECUTAR-MODULO
           .
       910000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       920000-EXCLUIR-VINCULO          SECTION.
      *--> Acionar o SGTSE087 para excluir vínculo financeiro de um
      *    Recebimento Previsto
      *-----------------------------------------------------------------
           MOVE  0032                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SPACES                TO  SGTSE087-DADOS
           INITIALIZE                      SGTSE087-DADOS
                                           REPLACING NUMERIC
                                           BY  ZEROS

           MOVE  SGTS0160-CD-CTRA-TIT
                                       TO  SGTSE087-CD-CTRA-TIT-VL
           MOVE  SGTS0160-CD-OPR-ETRD-MCP
                                       TO  SGTSE087-CD-OPR-ETRD-MCP
           MOVE  SGTS0160-NR-VRS-OPR-ETRD
                                       TO  SGTSE087-NR-VRS-OPR-ETRD
           MOVE  NR-SEQL-RCBT-PRVT
                                       TO  SGTSE087-NR-SEQL-RCBT-PRVT
           MOVE  DB2-VCL-CD-TIP-RCBT-PRVT                    (IC-TAB-01)
                                       TO  SGTSE087-CD-TIP-RCBT-PRVT
           MOVE  DB2-VCL-DT-RCBT-PRVT                        (IC-TAB-01)
                                       TO  SGTSE087-DT-RCBT-PRVT
           MOVE  SGTSC005-CD-TIT-OPR-ETRD
                                       TO  SGTSE087-CD-TIT-OPR-ETRD
           MOVE  SGTSC005-NR-DVS-TIT
                                       TO  SGTSE087-NR-DVS-TIT
           MOVE  DB2-VCL-TS-REG-PGTO                         (IC-TAB-01)
                                       TO  SGTSE087-TS-REG-PGTO

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSE087              TO  NM-PGM

           PERFORM 960000-EXECUTAR-MODULO
           .
       920000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       930000-INCLUIR-VINCULO          SECTION.
      *--> Acionar o SGTSI087 para incluir Registros
      *    na Tabela DB2SGT.RCBT_RLZD_OPR
      *-----------------------------------------------------------------
           MOVE  0033                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SPACES                TO  SGTSI087-DADOS
           INITIALIZE                      SGTSI087-DADOS
                                           REPLACING NUMERIC
                                           BY  ZEROS

      *--> Movimenta Operação Estruturada
           MOVE  SGTS0160-CD-CTRA-TIT
                                       TO  SGTSI087-CD-CTRA-TIT-VL
           MOVE  SGTS0160-CD-OPR-ETRD-MCP
                                       TO  SGTSI087-CD-OPR-ETRD-MCP
           MOVE  SGTS0160-NR-VRS-OPR-ETRD
                                       TO  SGTSI087-NR-VRS-OPR-ETRD

      *--> Movimenta Chave do Recebimento Previsto
           MOVE  NR-SEQL-RCBT-PRVT     TO  SGTSI087-NR-SEQL-RCBT-PRVT

           MOVE  TAB-CRNG-CD-TIP-RCBT-PRVT (IC-TAB-02)
                                       TO  SGTSI087-CD-TIP-RCBT-PRVT
           MOVE  TAB-CRNG-DT-RCBT-PRVT     (IC-TAB-02)
                                       TO  SGTSI087-DT-RCBT-PRVT

      *--> Movimenta Chave do Demonstrativo de Caixa
           MOVE  TAB-CRNG-CD-TIT-OPR-ETRD  (IC-TAB-02)
                                       TO  SGTSI087-CD-TIT-OPR-ETRD
           MOVE  TAB-CRNG-NR-DVS-TIT       (IC-TAB-02)
                                       TO  SGTSI087-NR-DVS-TIT
           MOVE  TAB-CRNG-TS-REG-PGTO      (IC-TAB-02)
                                       TO  SGTSI087-TS-REG-PGTO

      *--> Movimenta Recebimento Realizado
           MOVE  TAB-CRNG-IN-VCL-MNL       (IC-TAB-02)
                                       TO  SGTSI087-IN-VCL-MNL
           MOVE  KOPRD-CD-OPRD
                                       TO  SGTSI087-CD-USU-VCL
           MOVE  CURRENT
                                       TO  SGTSI087-TS-ULT-ATL(1:7)
           MOVE  TAB-CRNG-VL-RLZD-VCLD     (IC-TAB-02)
                                       TO  SGTSI087-VL-RLZD-VCLD

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSI087              TO  NM-PGM

           PERFORM 960000-EXECUTAR-MODULO
           .
       930000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       940000-LISTAR-VINCULOS          SECTION.
      *--> Acionar o SGTSL087 e consultar os Recebimentos Realizados
      *-----------------------------------------------------------------
           MOVE  0034                  TO  MSG1-CD-SEQL-ERRO

           SET   SGTSL087-LISTA-ATV  TO TRUE

      *--> Dados da Operação Estruturada
           MOVE  SGTS0160-CD-CTRA-TIT
                                       TO  SGTSL087-CD-CTRA-TIT-VL
           MOVE  SGTS0160-CD-OPR-ETRD-MCP
                                       TO  SGTSL087-CD-OPR-ETRD-MCP
           MOVE  SGTS0160-NR-VRS-OPR-ETRD
                                       TO  SGTSL087-NR-VRS-OPR-ETRD
           MOVE  NR-SEQL-RCBT-PRVT
                                       TO  SGTSL087-NR-SEQL-RCBT-PRVT
           MOVE  SGTSC005-CD-TIT-OPR-ETRD
                                       TO  SGTSL087-CD-TIT-OPR-ETRD
           MOVE  SGTSC005-NR-DVS-TIT   TO  SGTSL087-NR-DVS-TIT

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSL087              TO  NM-PGM

           PERFORM 960000-EXECUTAR-MODULO

           ADD  SGTSL087-QT-OCR        TO DB2-VCL-QT-REGISTRO
           .
       940000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       951000-INCREMENTAR-IC-TAB-01    SECTION.
      *-----------------------------------------------------------------
           MOVE  0035                  TO  MSG1-CD-SEQL-ERRO

           ADD                         1
                 TO                    IC-TAB-01
                 ON SIZE ERROR
                 SET   ERRO-PROGRAMADO
                                       TO  TRUE
                 MOVE  0082            TO  MSG1-CD-ERRO-TS
                 MOVE  'Erro na totalização do índice 01.'
                                       TO  MSG1-TX-ERRO-TS
                 PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-ADD
           .
       951000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       952000-INCREMENTAR-IC-TAB-02    SECTION.
      *-----------------------------------------------------------------
           MOVE  0036                  TO  MSG1-CD-SEQL-ERRO

           ADD                             1
                 TO                        IC-TAB-02
                 ON SIZE ERROR
                 SET   ERRO-PROGRAMADO
                                       TO  TRUE
                 MOVE  0082            TO  MSG1-CD-ERRO-TS
                 MOVE  'Erro na totalização do índice 02.'
                                       TO  MSG1-TX-ERRO-TS
                 PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-ADD
           .
       952000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       960000-EXECUTAR-MODULO          SECTION.
      *-----------------------------------------------------------------

      *--> Grava o container de requisicao do modulo a executar
           PERFORM  962000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  963000-EXCLUIR-RESPOSTA-ERRO

      *--> Nao ha captura automatica de erro CICS por usar RESP e RESP2

           EVALUATE  NM-PGM
               WHEN  SGTSC005
                     EXEC CICS  LINK
                                PROGRAM  ( SGTSC005                  )
                                CHANNEL  ( NM-CNL                    )
                                RESP     ( MSG1-RESP1                )
                                RESP2    ( MSG1-RESP2                )
                     END-EXEC
               WHEN  SGTSC069
                     EXEC CICS  LINK
                                PROGRAM  ( SGTSC069                  )
                                CHANNEL  ( NM-CNL                    )
                                RESP     ( MSG1-RESP1                )
                                RESP2    ( MSG1-RESP2                )
                     END-EXEC
               WHEN  SGTSC070
                     EXEC CICS  LINK
                                PROGRAM  ( SGTSC070                  )
                                CHANNEL  ( NM-CNL                    )
                                RESP     ( MSG1-RESP1                )
                                RESP2    ( MSG1-RESP2                )
                     END-EXEC
               WHEN  SGTSA087
                     EXEC CICS  LINK
                                PROGRAM  ( SGTSA087                  )
                                CHANNEL  ( NM-CNL                    )
                                RESP     ( MSG1-RESP1                )
                                RESP2    ( MSG1-RESP2                )
                     END-EXEC
               WHEN  SGTSE087
                     EXEC CICS  LINK
                                PROGRAM  ( SGTSE087                  )
                                CHANNEL  ( NM-CNL                    )
                                RESP     ( MSG1-RESP1                )
                                RESP2    ( MSG1-RESP2                )
                     END-EXEC
               WHEN  SGTSI087
                     EXEC CICS  LINK
                                PROGRAM  ( SGTSI087                  )
                                CHANNEL  ( NM-CNL                    )
                                RESP     ( MSG1-RESP1                )
                                RESP2    ( MSG1-RESP2                )
                     END-EXEC
               WHEN  SGTSL087
                     EXEC CICS  LINK
                                PROGRAM  ( SGTSL087                  )
                                CHANNEL  ( NM-CNL                    )
                                RESP     ( MSG1-RESP1                )
                                RESP2    ( MSG1-RESP2                )
                     END-EXEC
               WHEN  SGTS0161
                     EXEC CICS  LINK
                                PROGRAM  ( SGTS0161                  )
                                CHANNEL  ( NM-CNL                    )
                                RESP     ( MSG1-RESP1                )
                                RESP2    ( MSG1-RESP2                )
                     END-EXEC
               WHEN  SGTS0162
                     EXEC CICS  LINK
                                PROGRAM  ( SGTS0162                  )
                                CHANNEL  ( NM-CNL                    )
                                RESP     ( MSG1-RESP1                )
                                RESP2    ( MSG1-RESP2                )
                     END-EXEC
               WHEN  SGTS0081
                     EXEC CICS  LINK
                                PROGRAM  ( SGTS0081                  )
                                CHANNEL  ( NM-CNL                    )
                                RESP     ( MSG1-RESP1                )
                                RESP2    ( MSG1-RESP2                )
                     END-EXEC
                WHEN  SGTSPE07
                     EXEC CICS  LINK
                                PROGRAM  ( SGTSPE07                  )
                                CHANNEL  ( NM-CNL                    )
                                RESP     ( MSG1-RESP1                )
                                RESP2    ( MSG1-RESP2                )
                     END-EXEC
               WHEN  OTHER
                     MOVE    9005      TO  MSG1-CD-ERRO-TS
                     STRING  'Não definida a execução do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                   PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     STRING  'Erro na execução do módulo ' NM-PGM ', '
                             'RESP  = ' MSG1-RESP1  ', '
                             'RESP2 = ' MSG1-RESP2   '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     MOVE    3004      TO  MSG1-CD-ERRO-TS
                     SET  ERRO-CICS    TO  TRUE
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM  964000-LER-CONTAINER-DE-ERRO

      *--> Efetua a leitura do Container de Resposta
           PERFORM  965000-LER-RESPOSTA
           .
       960000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       962000-GRAVAR-CONTAINER-RQSC    SECTION.
      *-----------------------------------------------------------------
           MOVE  0037                  TO  MSG1-CD-SEQL-ERRO

      *--> Postar container com dados de requisicao (entrada)
      *    Modulo CONTAINER  -     NOME_PGM-RQSC
      *    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
      *    Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RQSC'               TO  NM-CTNR(9:5)

      *--> Por causa da captura automatica de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Faz PUT da requisicao do programa a executar
           EVALUATE  NM-PGM
               WHEN  SGTSC005
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                  )
                                FROM      ( SGTSC005-RQSC            )
                                FLENGTH   ( LENGTH OF SGTSC005-RQSC  )
                     END-EXEC
               WHEN  SGTSC069
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                  )
                                FROM      ( SGTSC069-RQSC            )
                                FLENGTH   ( LENGTH OF SGTSC069-RQSC  )
                     END-EXEC
               WHEN  SGTSC070
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                  )
                                FROM      ( SGTSC070-RQSC            )
                                FLENGTH   ( LENGTH OF SGTSC070-RQSC  )
                     END-EXEC
               WHEN  SGTSA087
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                  )
                                FROM      ( SGTSA087-RQSC            )
                                FLENGTH   ( LENGTH OF SGTSA087-RQSC  )
                     END-EXEC
               WHEN  SGTSE087
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                  )
                                FROM      ( SGTSE087-RQSC            )
                                FLENGTH   ( LENGTH OF SGTSE087-RQSC  )
                     END-EXEC
               WHEN  SGTSI087
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                  )
                                FROM      ( SGTSI087-RQSC            )
                                FLENGTH   ( LENGTH OF SGTSI087-RQSC  )
                     END-EXEC
               WHEN  SGTSL087
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                  )
                                FROM      ( SGTSL087-RQSC            )
                                FLENGTH   ( LENGTH OF SGTSL087-RQSC  )
                     END-EXEC
               WHEN  SGTS0161
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                  )
                                FROM      ( SGTS0161-RQSC            )
                                FLENGTH   ( LENGTH OF SGTS0161-RQSC  )
                     END-EXEC
               WHEN  SGTS0162
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                  )
                                FROM      ( SGTS0162-RQSC            )
                                FLENGTH   ( LENGTH OF SGTS0162-RQSC  )
                     END-EXEC
               WHEN  SGTS0081
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                  )
                                FROM      ( SGTS0081-RQSC            )
                                FLENGTH   ( LENGTH OF SGTS0081-RQSC  )
                     END-EXEC
               WHEN  SGTSPE07
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                  )
                                FROM      ( SGTSPE07-RQSC            )
                                FLENGTH   ( LENGTH OF SGTSPE07-RQSC  )
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9005        TO  MSG1-CD-ERRO-TS
                     STRING  'PUT não definido para requisição do '
                             'módulo ' NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Ao chegar aqui, ha certeza de que nao houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       962000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       963000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *-----------------------------------------------------------------

      *--> Exclui o container de resposta e de erro para evitar
      *    problemas em modulos executados mais de uma vez dentro da
      *    mesma sessao do CICS.
      *    Equivale ao initialize na area de resposta dos programas
      *    que utilizam COMMAREA.

           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)

           SET   DEL-CONTAINER         TO  TRUE

      *    Exclui o container de resposta
      *    Nao ha captura automatica de erro CICS por usar RESP e RESP2
           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR                            )
                      RESP      ( MSG1-RESP1                         )
                      RESP2     ( MSG1-RESP2                         )
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

           SET   SEM-ERRO              TO  TRUE

      *--> Excluir o container de erro
           PERFORM  963100-EXCLUIR-CONTAINER-ERRO
           .
       963000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       963100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------


           INITIALIZE                      IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE  CTNR-PRM-ERRO         TO  NM-CTNR
           SET   DEL-CONTAINER         TO  TRUE

      *--> Nao ha captura automatica de erro CICS por usar RESP e RESP2
           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR                            )
                      RESP      ( MSG1-RESP1                         )
                      RESP2     ( MSG1-RESP2                         )
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

           SET   SEM-ERRO              TO  TRUE
           .
       963100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       964000-LER-CONTAINER-DE-ERRO    SECTION.
      *-----------------------------------------------------------------

      *--> A presenca do container PRM-ERRO indica que houve erro de
      *    integracao, a ausencia do container indica sucesso.

      *--> Le o container de erro dos modulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE  CTNR-PRM-ERRO         TO  NM-CTNR
           MOVE  SPACES                TO  IIBKERRO-PRM-ERRO
           INITIALIZE                      IIBKERRO-PRM-ERRO
                                           REPLACING NUMERIC BY  ZEROS

      *--> Nao ha captura automatica de erro CICS por usar RESP e RESP2
           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                            )
                      INTO      ( IIBKERRO-PRM-ERRO                  )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO        )
                      RESP      ( MSG1-RESP1                         )
                      RESP2     ( MSG1-RESP2                         )
           END-EXEC

      *--- Caso deseje personalizar ou nao interromper o
      *    processamento, alterar para nao dar erro aqui Somente
      *    Para o Modulo Chamado e tratar na section chamadora
           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
                     CONTINUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
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
      *-->      Monitorar variáveis do módulo chamado
                PERFORM 969000-MONITORAR-VARIAVEIS
                PERFORM 999000-GRAVA-ERRO-E-ENCERRA

               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       964000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       965000-LER-RESPOSTA             SECTION.
      *--> Recupera o container com os dados de resposta (saida).
      *    Modulo CONTAINER  - NOME_PGM-RPST
      *    Modulo IIB (FIXO) - PRM-DADOS-RPST
      *-----------------------------------------------------------------
           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)

      *--> Recupera o tamanho do container de resposta
           PERFORM  965100-LER-CONTAINER-NODATA

      *--> Configura dados para leitura do container de resposta
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automatica de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Ler resposta de acordo com o programa que a gravou
           EVALUATE  NM-PGM
      *------> Lê container de resposta do SGTSC005
               WHEN  SGTSC005
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR                  )
                                INTO      ( SGTSC005-RPST            )
                                FLENGTH   ( QT-CCT-LIDO              )
                     END-EXEC
      *------> Lê container de resposta do SGTSC069
               WHEN  SGTSC069
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR                  )
                                INTO      ( SGTSC069-RPST            )
                                FLENGTH   ( QT-CCT-LIDO              )
                     END-EXEC
      *------> Lê container de resposta do SGTSC070
               WHEN  SGTSC070
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR                  )
                                INTO      ( SGTSC070-RPST            )
                                FLENGTH   ( QT-CCT-LIDO              )
                     END-EXEC
      *------> Lê container de resposta do SGTSA087
               WHEN  SGTSA087
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR                  )
                                INTO      ( SGTSA087-RPST            )
                                FLENGTH   ( QT-CCT-LIDO              )
                     END-EXEC
      *------> Lê container de resposta do SGTSE087
               WHEN  SGTSE087
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR                  )
                                INTO      ( SGTSE087-RPST            )
                                FLENGTH   ( QT-CCT-LIDO              )
                     END-EXEC
      *------> Lê container de resposta do SGTSI087
               WHEN  SGTSI087
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR                  )
                                INTO      ( SGTSI087-RPST            )
                                FLENGTH   ( QT-CCT-LIDO              )
                     END-EXEC
      *------> Lê container de resposta do SGTSE087
               WHEN  SGTSL087
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR                  )
                                INTO      ( SGTSL087-RPST            )
                                FLENGTH   ( QT-CCT-LIDO              )
                     END-EXEC
               WHEN  SGTS0161
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR                  )
                                INTO      ( SGTS0161-RPST            )
                                FLENGTH   ( QT-CCT-LIDO              )
                     END-EXEC
               WHEN  SGTS0162
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR                  )
                                INTO      ( SGTS0162-RPST            )
                                FLENGTH   ( QT-CCT-LIDO              )
                     END-EXEC
               WHEN  SGTS0081
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR                  )
                                INTO      ( SGTS0081-RPST            )
                                FLENGTH   ( QT-CCT-LIDO              )
                     END-EXEC
               WHEN  SGTSPE07
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR                  )
                                INTO      ( SGTSPE07-RPST            )
                                FLENGTH   ( QT-CCT-LIDO              )
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
               MOVE  9005              TO  MSG1-CD-ERRO-TS
                     STRING  'Get não definido para resposta do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Ao chegar aqui, ha certeza de que nao houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       965000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       965100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------
      *--> Configura dados para leitura do container de requisicao.


           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automatica de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Obtem a quantidade de caracteres no container
           MOVE  ZEROS                 TO  QT-CCT-LIDO
           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                            )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO                        )
           END-EXEC

           SET   SEM-ERRO              TO  TRUE
           .
       965100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *-----------------------------------------------------------------

      *--> Formatar variaveis conforme programa acionado
           EVALUATE  NM-PGM
                WHEN  SGTS0081
                     MOVE 'SGTS0081-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE SGTS0081-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTS0081-CD-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTS0081-CD-OPR-ETRD IS NUMERIC
                         MOVE  SGTS0081-CD-OPR-ETRD
                                                 TO CD-OPR-ETRD-MCP-NR
                         MOVE CD-OPR-ETRD-MCP-TX TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico.'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTS0081-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0081-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                WHEN  SGTS0161
                     MOVE 'SGTS0161-CD-CTRA-TIT' TO  MSGKW999-NM-VRV-MNT
                     MOVE SGTS0161-CD-CTRA-TIT   TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTS0161-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTS0161-CD-OPR-ETRD-MCP IS NUMERIC
                         MOVE  SGTS0161-CD-OPR-ETRD-MCP
                                                 TO CD-OPR-ETRD-MCP-NR
                         MOVE CD-OPR-ETRD-MCP-TX TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico.'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTS0161-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0161-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTS0161-NR-SEQL-RCBT-PRVT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0161-NR-SEQL-RCBT-PRVT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTS0161-IN-NOVO-CRNG'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE SGTS0161-IN-NOVO-CRNG  TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTS0161-DT-EMS-OPR' TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0161-DT-EMS-OPR   TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTS0161-DT-VNCT-OPR' TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0161-DT-VNCT-OPR  TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTS0161-VL-EVT-VCLD-VRS'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0161-VL-EVT-VCLD-VRS
                                                 TO  MCR-S9-15-V9-02
                     MOVE  MCR-S9-15-V9-02       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTS0161-QT-RCBT-PRVT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0161-QT-RCBT-PRVT TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTS0161-QT-TX-OBS'  TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0161-QT-TX-OBS    TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTS0162
                     MOVE  'SGTS0162-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0162-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTS0162-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTS0162-CD-OPR-ETRD-MCP IS NUMERIC
                         MOVE  SGTS0162-CD-OPR-ETRD-MCP
                                                 TO CD-OPR-ETRD-MCP-NR
                         MOVE CD-OPR-ETRD-MCP-TX TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico.'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTS0162-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0162-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTS0162-QT-OCR-TAB-PGTO'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0162-QT-OCR-TAB-PGTO
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSE087
                     MOVE  'SGTSE087-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE087-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSE087-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSE087-CD-OPR-ETRD-MCP IS NUMERIC
                         MOVE  SGTSE087-CD-OPR-ETRD-MCP
                                                 TO CD-OPR-ETRD-MCP-NR
                         MOVE CD-OPR-ETRD-MCP-TX TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico.'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSE087-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE087-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSE087-NR-SEQL-RCBT-PRVT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE087-NR-SEQL-RCBT-PRVT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSE087-CD-TIP-RCBT-PRVT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE087-CD-TIP-RCBT-PRVT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSE087-DT-RCBT-PRVT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE087-DT-RCBT-PRVT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSE087-CD-TIT-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE087-CD-TIT-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSE087-NR-DVS-TIT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE087-NR-DVS-TIT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSE087-TS-REG-PGTO'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE087-TS-REG-PGTO
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT


               WHEN  SGTSA087
                     MOVE  'SGTSA087-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA087-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT



                     MOVE  'SGTSA087-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSA087-CD-OPR-ETRD-MCP IS NUMERIC
                         MOVE  SGTSA087-CD-OPR-ETRD-MCP
                                                 TO CD-OPR-ETRD-MCP-NR
                         MOVE CD-OPR-ETRD-MCP-TX TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico.'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSA087-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA087-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSA087-NR-SEQL-RCBT-PRVT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA087-NR-SEQL-RCBT-PRVT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSA087-CD-TIP-RCBT-PRVT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA087-CD-TIP-RCBT-PRVT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSA087-DT-RCBT-PRVT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA087-DT-RCBT-PRVT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSA087-CD-TIT-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA087-CD-TIT-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSA087-NR-DVS-TIT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA087-NR-DVS-TIT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSA087-TS-REG-PGTO'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA087-TS-REG-PGTO
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSA087-IN-VCL-MNL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA087-IN-VCL-MNL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSA087-CD-USU-VCL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA087-CD-USU-VCL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSA087-TS-ULT-ATL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA087-TS-ULT-ATL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSA087-VL-RLZD-VCLD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA087-VL-RLZD-VCLD TO  MCR-S9-15-V9-02
                     MOVE  MCR-S9-15-V9-02       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSI087
                     MOVE  'SGTSI087-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI087-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSI087-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSI087-CD-OPR-ETRD-MCP IS NUMERIC
                         MOVE  SGTSI087-CD-OPR-ETRD-MCP
                                                 TO CD-OPR-ETRD-MCP-NR
                         MOVE CD-OPR-ETRD-MCP-TX TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico.'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSI087-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI087-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSI087-NR-SEQL-RCBT-PRVT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI087-NR-SEQL-RCBT-PRVT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSI087-CD-TIP-RCBT-PRVT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI087-CD-TIP-RCBT-PRVT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSI087-DT-RCBT-PRVT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI087-DT-RCBT-PRVT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSI087-CD-TIT-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI087-CD-TIT-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSI087-NR-DVS-TIT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI087-NR-DVS-TIT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSI087-TS-REG-PGTO'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI087-TS-REG-PGTO
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSI087-IN-VCL-MNL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI087-IN-VCL-MNL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSI087-CD-USU-VCL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI087-CD-USU-VCL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSI087-TS-ULT-ATL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI087-TS-ULT-ATL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSI087-VL-RLZD-VCLD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI087-VL-RLZD-VCLD
                                                 TO MCR-S9-15-V9-02
                     MOVE  MCR-S9-15-V9-02       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSL087
                     MOVE  'SGTSL087-CD-TIP-SRVC'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL087-CD-TIP-SRVC  TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL087-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL087-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL087-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSL087-CD-OPR-ETRD-MCP IS NUMERIC
                         MOVE  SGTSL087-CD-OPR-ETRD-MCP
                                                 TO CD-OPR-ETRD-MCP-NR
                         MOVE CD-OPR-ETRD-MCP-TX TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico.'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL087-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL087-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL087-NR-SEQL-RCBT-PRVT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL087-NR-SEQL-RCBT-PRVT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL087-CD-TIT-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL087-CD-TIT-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL087-CD-TIP-RCBT-PRVT-ULT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL087-CD-TIP-RCBT-PRVT-ULT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL087-DT-RCBT-PRVT-ULT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL087-DT-RCBT-PRVT-ULT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL087-CD-TIT-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL087-CD-TIT-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL087-NR-DVS-TIT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL087-NR-DVS-TIT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL087-TS-REG-PGTO-ULT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL087-TS-REG-PGTO-ULT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT


               WHEN  SGTSC005
                     MOVE  'SGTSC005-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSC005-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT


                     MOVE  'SGTSC005-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSC005-CD-OPR-ETRD-MCP  IS  NUMERIC
                         MOVE  SGTSC005-CD-OPR-ETRD-MCP
                                                 TO  CD-OPR-ETRD-MCP-NR
                         MOVE CD-OPR-ETRD-MCP-TX
                                                 TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico.'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSC005-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSC005-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------------> Adiciona Valor do Evento Vinculado
                     MOVE  'SGTSC005-VL-EVT-VCLD-VRS'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSC005-VL-EVT-VCLD-VRS
                                                 TO  MCR-S9-15-V9-02
                     MOVE  MCR-S9-15-V9-02
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSC069
                     MOVE  'SGTSC069-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSC069-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSC069-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSC069-CD-OPR-ETRD-MCP  IS  NUMERIC
                         MOVE  SGTSC069-CD-OPR-ETRD-MCP
                                                 TO  CD-OPR-ETRD-MCP-NR
                         MOVE CD-OPR-ETRD-MCP-TX
                                                 TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico.'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSC069-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSC069-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSC069-TS-EST-VRS' TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSC069-TS-EST-VRS   TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSC070
                     MOVE  'SGTSC070-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSC070-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSC070-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSC070-CD-OPR-ETRD-MCP  IS  NUMERIC
                         MOVE  SGTSC070-CD-OPR-ETRD-MCP
                                                 TO  CD-OPR-ETRD-MCP-NR
                         MOVE CD-OPR-ETRD-MCP-TX
                                                 TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico.'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSC070-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSC070-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSC070-NR-SEQL-RCBT-PRVT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSC070-NR-SEQL-RCBT-PRVT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSC070-CD-TIP-RCBT-PRVT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSC070-CD-TIP-RCBT-PRVT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSC070-DT-RCBT-PRVT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSC070-DT-RCBT-PRVT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSPE07
                     MOVE 'SGTSPE07-CD-CTRA-TIT' TO  MSGKW999-NM-VRV-MNT
                     MOVE SGTSPE07-CD-CTRA-TIT   TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSPE07-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSPE07-CD-OPR-ETRD-MCP IS NUMERIC
                         MOVE  SGTSPE07-CD-OPR-ETRD-MCP
                                                 TO CD-OPR-ETRD-MCP-NR
                         MOVE CD-OPR-ETRD-MCP-TX TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico.'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSPE07-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSPE07-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                     CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       970000-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE  0038                  TO  MSG1-CD-SEQL-ERRO

      * Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           MOVE CTNR-PRM-INF-OPRD      TO NM-CTNR

      * Por causa da captura automática de erro CICS, deve ser previsto
      * o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           INITIALIZE PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       970000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA                     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Em caso de erro, desfaz as atualizações em banco de dados
           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC

      *--> Verifica se o tipo de erro gerado possui tratamento interno
           EVALUATE  TRUE
               WHEN  ERRO-CONTAINER
                     PERFORM 999004-TRATA-ERRO-CTNR
               WHEN  ERRO-TAMANHO-AREA
                     PERFORM 999005-TRATA-ERRO-AREA-DADOS
               WHEN  ERRO-CANAL
                     PERFORM 999006-TRATA-ERRO-CANAL
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

      *--> Adiciona as Variáveis de Entrada para Monitoração
           MOVE 'MSG1-TX-ERRO-TS'             TO  MSGKW999-NM-VRV-MNT
           MOVE  MSG1-TX-ERRO-TS              TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0160-CD-CTRA-TIT'           TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0160-CD-CTRA-TIT            TO  MCR-S9-09
           MOVE  MCR-S9-09                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0160-CD-OPR-ETRD-MCP'       TO  MSGKW999-NM-VRV-MNT
           IF    SGTS0160-CD-OPR-ETRD-MCP        IS  NUMERIC
               MOVE  SGTS0160-CD-OPR-ETRD-MCP    TO  MCR-S9-11
               MOVE  MCR-S9-11                   TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Código não numérico.'   TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0160-NR-VRS-OPR-ETRD'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0160-NR-VRS-OPR-ETRD        TO  MCR-S9-04
           MOVE  MCR-S9-04                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0160-QT-RCBT-PRVT'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0160-QT-RCBT-PRVT           TO  MCR-S9-04
           MOVE  MCR-S9-04                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0160-QT-VCL'                TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0160-QT-VCL                 TO  MCR-S9-04
           MOVE  MCR-S9-04                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTS0160-QT-TX-OBS'            TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0160-QT-TX-OBS              TO  MCR-S9-04
           MOVE  MCR-S9-04                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Inconsistência batimento valor previsto apurado x capital
           IF  COM-ERRO-BAT-VL
               MOVE 'TTL-VL-RCBT-PRVT'           TO  MSGKW999-NM-VRV-MNT
               IF    TTL-VL-RCBT-PRVT            IS  NUMERIC
                   MOVE  TTL-VL-RCBT-PRVT        TO  MCR-S9-15-V9-02
                   MOVE  MCR-S9-15-V9-02         TO  MSGKW999-VL-VRV-MNT
               ELSE
                   MOVE 'Código não numérico.'   TO  MSGKW999-VL-VRV-MNT
               END-IF
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSC005-VL-EVT-VCLD-VRS'   TO  MSGKW999-NM-VRV-MNT
               IF    SGTSC005-VL-EVT-VCLD-VRS    IS  NUMERIC
                   MOVE  SGTSC005-VL-EVT-VCLD-VRS TO  MCR-S9-15-V9-02
                   MOVE  MCR-S9-15-V9-02         TO  MSGKW999-VL-VRV-MNT
               ELSE
                   MOVE 'Código não numérico.'   TO  MSGKW999-VL-VRV-MNT
               END-IF
               PERFORM 999003-ADICIONA-VRV-MNT
           END-IF

      *--> Verifica se o erro foi em registro de Tabela
           IF COM-ERRO-TAB-RCBT-PRVT
              PERFORM 999100-ADICIONA-TAB-RCBT-PRVT
           END-IF

           IF COM-ERRO-TAB-VCL
              PERFORM 999200-ADICIONA-OCR-TAB-VCL
           END-IF

           IF COM-ERRO-TAB-OBS-RCBT-PRVT
              PERFORM 999300-ADICIONA-TAB-OBS-RCBT
           END-IF

           IF COM-ERRO-TAB-REALIZADO
              PERFORM 999400-ADICIONA-TAB-REALIZADO
           END-IF

      *--> Grava o erro no MSG
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                         )
                      CHANNEL   ( NM-CNL                             )
                      FROM      ( IIBKERRO-PRM-ERRO                  )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO        )
                      NOHANDLE
           END-EXEC

           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999100-ADICIONA-TAB-RCBT-PRVT   SECTION.
      *-----------------------------------------------------------------

           MOVE  NM-IC-TAB-RCBT-PRVT             TO  MSGKW999-NM-VRV-MNT
           MOVE  VL-IC-TAB-RCBT-PRVT             TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0160-CD-TIP-RCBT-PRVT'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0160-CD-TIP-RCBT-PRVT       (VL-IC-TAB-RCBT-PRVT)
                                                 TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0160-DT-RCBT-PRVT'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0160-DT-RCBT-PRVT           (VL-IC-TAB-RCBT-PRVT)
                                                 TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0160-VL-RCBT-PRVT'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0160-VL-RCBT-PRVT           (VL-IC-TAB-RCBT-PRVT)
                                                 TO  MCR-S9-15-V9-02
           MOVE  MCR-S9-15-V9-02                 TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0160-NR-ITEM-TX-OBS'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0160-NR-ITEM-TX-OBS         (VL-IC-TAB-RCBT-PRVT)
                                                 TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'TTL-VL-RCBT-PRVT'               TO  MSGKW999-NM-VRV-MNT
           MOVE  TTL-VL-RCBT-PRVT                TO  MCR-S9-15-V9-02
           MOVE  MCR-S9-15-V9-02                 TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           .
       999100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999200-ADICIONA-OCR-TAB-VCL     SECTION.
      *-----------------------------------------------------------------

           MOVE  NM-IC-TAB-VCL                   TO  MSGKW999-NM-VRV-MNT
           MOVE  VL-IC-TAB-VCL                   TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0160-NR-ITEM-CRNG'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0160-NR-ITEM-CRNG           (VL-IC-TAB-VCL)
                                                 TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0160-TS-REG-PGTO'           TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0160-TS-REG-PGTO            (VL-IC-TAB-VCL)
                                                 TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0160-VL-RLZD-VCLD'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0160-VL-RLZD-VCLD           (VL-IC-TAB-VCL)
                                                 TO  MCR-S9-15-V9-02
           MOVE  MCR-S9-15-V9-02                 TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           .
       999200-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999300-ADICIONA-TAB-OBS-RCBT    SECTION.
      *-----------------------------------------------------------------

           MOVE  NM-IC-TAB-OBS-RCBT-PRVT         TO  MSGKW999-NM-VRV-MNT
           MOVE  VL-IC-TAB-OBS-RCBT-PRVT         TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0160-TX-OBS-RCBT-PRVT'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0160-TX-OBS-RCBT-PRVT    (VL-IC-TAB-OBS-RCBT-PRVT)
                                                 TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           .
       999300-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999400-ADICIONA-TAB-REALIZADO   SECTION.
      *-----------------------------------------------------------------

           MOVE  NM-IC-TAB-REALIZADO             TO  MSGKW999-NM-VRV-MNT
           MOVE  VL-IC-TAB-REALIZADO             TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'LS-TIP-CD-TIP-RCBT-PRVT'        TO  MSGKW999-NM-VRV-MNT
           MOVE  LS-TIP-CD-TIP-RCBT-PRVT         (VL-IC-TAB-REALIZADO)
                                                 TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'LS-TIP-DT-RCBT-PRVT'            TO  MSGKW999-NM-VRV-MNT
           MOVE  LS-TIP-DT-RCBT-PRVT             (VL-IC-TAB-REALIZADO)
                                                 TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'LS-TIP-VL-RLZD-VCLD'            TO  MSGKW999-NM-VRV-MNT
           MOVE  LS-TIP-VL-RLZD-VCLD             (VL-IC-TAB-REALIZADO)
                                                 TO  MCR-S9-15-V9-02
           MOVE  MCR-S9-15-V9-02                 TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'LS-TIP-VL-RCBT-PRVT'            TO  MSGKW999-NM-VRV-MNT
           MOVE  LS-TIP-VL-RCBT-PRVT             (VL-IC-TAB-REALIZADO)
                                                 TO  MCR-S9-15-V9-02
           MOVE  MCR-S9-15-V9-02                 TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           .
       999400-SAI.
           EXIT.
      * ---------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * ---------------------------------------
-INC HLPKP999
      *----------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
-INC SGTKP999
      * ---------------------------------------
      * MSG1-NTF-MSG                   SECTION.
      * ---------------------------------------
-INC MSGKPMG1
      * ---------------------------------------
       999999-ENCERRAR                 SECTION.
      * ---------------------------------------
           EXEC CICS
                 RETURN
           END-EXEC.
      *------------------------- Fim SGTS0160 -------------------------*