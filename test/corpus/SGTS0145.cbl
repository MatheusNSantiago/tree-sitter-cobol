       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * Programa..: SGTS0145
      * Analista..: F2054109 - Cintia Ap Soares Iglesias
      * Autor.....: F8658869 - Roberto Ioshimoto
      * Data......: 06/04/2017
      * Objetivo..: Gerar versão de operação estruturada
      * Compilacao: Cobol/CICS/DB2
      *-----------------------------------------------------------------
      * VRS009 07/01/2025 F9158937 Acerto nas Rotinas tratamento Insol-
      *                   vência (362000-INCLUI-INSOLVENCIA e
      *                                     360000-INSOLVENCIA-OPERACAO)
      * VRS008 21/06/2024 F9188847 História:1427980, inclusão de campo:
      *                     CD-MTV-EST-NEGL
      * VRS007 06/11/2023 F9188847 História:1094276, inclusão de campo:
      *                     CD-CLSC-ITMT-FNCO
      *         Utilizar o SGTSX005 em novas alterações ou em programas
      *         novos no lugar do SGTST005.
      * VRS006 03/03/2023 F9158937 - Inclusão campo CD-FMA-APRC nos
      *        books da Condição Negociada SGTKI079 e SGTKL079.
      * VRS005 09/11/2020 - F8650584 - DE 2020/155748
      *                     Ajuste para preservar o Preservar Matrícula
      *                     do Responsável pelo Cadastramento da
      *                     Ordem de Negociação
      * VRS004 25/10/2019 - F2804421 - DE 2016/145195
      *                     Incluir verificação do último Estado da
      *                     Versão antes de gerar nova versão.
      *        24/10/2019 - F8785286 DE 2016/145195
      *                     Recompilação por alteração Book SGTKI078
      *        10/10/2019 - F2804421 - DE 2016/D145195
      *                     Inclusão dos campos SGTSL078-CD-TIP-SRVC
      *                     (p/chamada do SGTSL078), utilizar
      *                     opção SGTSL078-LISTA-TODOS e carregar
      *                     o campo SGTSL078-TX-JST na inclusão.
      *        19/09/2019 - F8785286 - DE 2016/145195 -
      *                     Exclusão do container SGTSI076-RPST.
      *                     Inclusão do campo SGTSL076-CD-TIP-CLI-CPRD
      *                     e movimentação desse campo para SGTSI076.
      *                     Inclusão dos campos SGTSL075-DT-VND,
      *                     SGTSL075-QT-TTL-PAPL, SGTSL075-QT-PAPL-CMPD
      *                     e SGTSL075-QT-PAPL-LVRE e movimentação
      *                     desses campos para o SGTSI075.
      * VRS003 03/05/2019 - F8785286 - DE 2016/145195
      *                     Recompilação por alteração no Book SGTKL075.
      *        23/04/2019 - F2804421 - DE 2016/145195
      *                     Acerto no nome do módulo atual 'SGTS0145'
      *                     para variável de trabalho NM-PGM.
      *        03/04/2019 - F2804421 - DE 2016/145195
      *                     Inclusão de movimentação para campo
      *                     SGTSL063-CD-ACAO (ação 2).
      *        06/03/2019 - F8785286 - DE 2016/145195
      *                     Inclusão de movimentação para o
      *                     campo SGTSL075-CD-ACAO (ação 1).
      * VRS002 18/02/2019 - F2804421 - DE 2016/145195
      *                     Recompilação por alteração no Book SGTKL006.
      * VRS001 17/01/2018 - F9188847 - DE 2016/145195
      *                     Inclusão de Monitoração SBDATAF
      *        17/12/2018 - F2804421 - DE 2016/145195
      *                     Incluir tratamento para tabela
      *                     JST_VLDC_OPR_ETRD.
      *        20/09/2018 - F8785286 - DE 2016/145195
      *                     Recompilação por alteração no Book SGTKL083.
      *        20/07/2018 - F2804421 - DE 2016/145195
      *                     Retirar tratamento campo tipo de serviço da
      *                     chamada do pgm de lista SGTSL080.
      *        06/04/2017 - F8658869 - DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0145.
      *
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

       77  CTE-INICIO-SS               PIC  X(40)  VALUE
                                       '*** S.S. COMECA AQUI ***'.
       77  CTE-PROG                    PIC  X(18)  VALUE
                                       '*** SGTS0145 ***'.
       77  CTE-VERS                    PIC  X(06)  VALUE 'VRS009'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTSX005                PIC  X(08) VALUE 'SGTSX005'.
           03  SGTSL006                PIC  X(08) VALUE 'SGTSL006'.
           03  SGTST006                PIC  X(08) VALUE 'SGTST006'.
           03  SGTST009                PIC  X(08) VALUE 'SGTST009'.
           03  SGTSL011                PIC  X(08) VALUE 'SGTSL011'.
           03  SGTSI011                PIC  X(08) VALUE 'SGTSI011'.
           03  SGTSL035                PIC  X(08) VALUE 'SGTSL035'.
           03  SGTSI035                PIC  X(08) VALUE 'SGTSI035'.
           03  SGTSL060                PIC  X(08) VALUE 'SGTSL060'.
           03  SGTSI060                PIC  X(08) VALUE 'SGTSI060'.
           03  SGTSL061                PIC  X(08) VALUE 'SGTSL061'.
           03  SGTSI061                PIC  X(08) VALUE 'SGTSI061'.
           03  SGTSL063                PIC  X(08) VALUE 'SGTSL063'.
           03  SGTSI063                PIC  X(08) VALUE 'SGTSI063'.
           03  SGTSL064                PIC  X(08) VALUE 'SGTSL064'.
           03  SGTSI064                PIC  X(08) VALUE 'SGTSI064'.
           03  SGTSI067                PIC  X(08) VALUE 'SGTSI067'.
           03  SGTSL067                PIC  X(08) VALUE 'SGTSL067'.
           03  SGTSL068                PIC  X(08) VALUE 'SGTSL068'.
           03  SGTSI068                PIC  X(08) VALUE 'SGTSI068'.
           03  SGTSI069                PIC  X(08) VALUE 'SGTSI069'.
           03  SGTSL070                PIC  X(08) VALUE 'SGTSL070'.
           03  SGTSI070                PIC  X(08) VALUE 'SGTSI070'.
           03  SGTSI071                PIC  X(08) VALUE 'SGTSI071'.
           03  SGTSL074                PIC  X(08) VALUE 'SGTSL074'.
           03  SGTSI074                PIC  X(08) VALUE 'SGTSI074'.
           03  SGTSL075                PIC  X(08) VALUE 'SGTSL075'.
           03  SGTSIV75                PIC  X(08) VALUE 'SGTSIV75'.
           03  SGTSL076                PIC  X(08) VALUE 'SGTSL076'.
           03  SGTSI076                PIC  X(08) VALUE 'SGTSI076'.
           03  SGTSL077                PIC  X(08) VALUE 'SGTSL077'.
           03  SGTSI077                PIC  X(08) VALUE 'SGTSI077'.
           03  SGTSL078                PIC  X(08) VALUE 'SGTSL078'.
           03  SGTSI078                PIC  X(08) VALUE 'SGTSI078'.
           03  SGTSL079                PIC  X(08) VALUE 'SGTSL079'.
           03  SGTSI079                PIC  X(08) VALUE 'SGTSI079'.
           03  SGTSL080                PIC  X(08) VALUE 'SGTSL080'.
           03  SGTSI080                PIC  X(08) VALUE 'SGTSI080'.
           03  SGTSL081                PIC  X(08) VALUE 'SGTSL081'.
           03  SGTSI081                PIC  X(08) VALUE 'SGTSI081'.
           03  SGTSL082                PIC  X(08) VALUE 'SGTSL082'.
           03  SGTSI082                PIC  X(08) VALUE 'SGTSI082'.
           03  SGTSL083                PIC  X(08) VALUE 'SGTSL083'.
           03  SGTSI083                PIC  X(08) VALUE 'SGTSI083'.
           03  SGTSL084                PIC  X(08) VALUE 'SGTSL084'.
           03  SGTSI084                PIC  X(08) VALUE 'SGTSI084'.
           03  SGTSL087                PIC  X(08) VALUE 'SGTSL087'.
           03  SGTSI087                PIC  X(08) VALUE 'SGTSI087'.
           03  SGTSL507                PIC  X(08) VALUE 'SGTSL507'.
           03  SGTSL095                PIC  X(08) VALUE 'SGTSL095'.
           03  SGTSI095                PIC  X(08) VALUE 'SGTSI095'.
           03  SGTSL117                PIC  X(08) VALUE 'SGTSL117'.
           03  SGTSI117                PIC  X(08) VALUE 'SGTSI117'.
           03  SGTSPE05                PIC  X(08) VALUE 'SGTSPE05'.

      *--> Definição dos Variaveis do Cobolito
       COPY ALMKCBL1.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Areas de trabalho

       01  IN-CONTAINER-ERRO           PIC  X(01) VALUE SPACES.
           88 TEM-CONTAINER-ERRO                  VALUE 'S'.
           88 NAO-TEM-CONTAINER-ERRO              VALUE 'N'.

       01  AREA-AUX.
           03  NR-SEQL-RCBT-PRVT-INI   PIC S9(04) COMP-5 VALUE ZEROS.

       01  SGTSL117-IC-TAB             PIC S9(04) COMP-5 VALUE ZEROS.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).
       01  CD-OPR-ETRD-MCP-DISPLAY     PIC  9(11) VALUE ZEROS.
       01  VL-15V2                     PIC ----.---.---.---.--9,99.
       01  VL-15V4                     PIC ---.---.---.---.--9,9999.
       01  VL-04V04                    PIC --.--9,9999.
       01  VL-04V10                    PIC -.--9,9999999999.

      *--> Chamada à subrotina SBDATAF
       01  WKSBDATA                    PIC X(4099).
       01  AREA-RED                    REDEFINES WKSBDATA.
           03  AREA-TAM                PIC 9(0008) COMP.
           03  FILLER                  PIC X(4071).
           03  AREA-FIM                PIC X(0020).
       01  SBDATAF                     PIC  X(08) VALUE 'SBDATAF'.
       01  FUNCAO                     PIC  X(3) VALUE 'F01'.
       01  ARG01                      PIC  9(8) VALUE 0.
       01  ARG01-R REDEFINES ARG01.
           03  DIA                    PIC  9(2).
           03  MES                    PIC  9(2).
           03  ANO                    PIC  9(4).
       01  ARG02                      PIC  9(1) VALUE 0.


      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book dos dados do usuário
       COPY IIBKOPRD.

      *--> Book do container de Serviços (para acionamento de serviços)
       COPY IIBKSRVC.

      *--> Book do Container de Erro
       COPY IIBKERRO.

      *--> Book para captura de abend cics da bloco codigo HLPKW999
       COPY HLPKW999.

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
       COPY SGTKERR1.

      *--> Book para captura de erro via MSG1
       COPY MSGKWMG1.

      *--> Book do Container de Requisicao e Resposta deste Módulo
       01  SGTS0145-DADOS.
       COPY SGTK0145.

      *-----------------------------------------------------------------
      *    Book das Subrotinas Acionadas
      *-----------------------------------------------------------------

      *--> Consultar o estado e o tipo da última versão da operação nas
      *    tabelas OPR_ETRD_MCP e EST_VRS_OPR_ETRD.
       01  SGTSPE05-DADOS.
       COPY SGTKPE05.

      *--> Persistência da Tabela OPR_ETRD_MCP
       01  SGTSX005-DADOS.
       COPY SGTKX005.

      *-->  Inclusão da Tabela EST_VRS_OPR_ETRD
       01  SGTSI069-DADOS.
       COPY SGTKI069.

      *--> Persistência da Tabela EST_OPR_ETRD
       01  SGTST009-DADOS.
       COPY SGTKT009.

      * Lista OPR_PG_OTR_OPR
       01  SGTKL507-DADOS.
       COPY SGTKL507.
      * Inclusão OPR_PG_OTR_OPR
       01  SGTSI071-DADOS.
       COPY SGTKI071.
      * Lista IN_OPR_ETRD
       COPY SGTKL063.
      * Inclusão IN_OPR_ETRD
       COPY SGTKI063.
      * Lista    TARF_OPR_ETRD
       COPY SGTKL084.
      * Inclusão TARF_OPR_ETRD
       COPY SGTKI084.
      * Inclusão RPAC_PGMD_OPR
       COPY SGTKI061.
      * Lista    RPAC_PGMD_OPR
       COPY SGTKL061.
      * Inclusão RCBT_PRVT_OPR_ETRD
       01  SGTSI070-DADOS.
       COPY SGTKI070.
      * Lista    RCBT_PRVT_OPR_ETRD
       01  SGTSL070-DADOS.
       COPY SGTKL070.
      * Lista    RCBT_RLZD_OPR
       01  SGTSL087-DADOS.
       COPY SGTKL087.
      * Inclusão RCBT_RLZD_OPR
       01  SGTSI087-DADOS.
       COPY SGTKI087.
      * Lista    BCO_PCT_OPR
       COPY SGTKL064.
      * Inclusão BCO_PCT_OPR
       COPY SGTKI064.

      * Lista    RMNC_OPR_ETRD
       01  SGTSL011-DADOS.
       COPY SGTKL011.
      * Inclusão RMNC_OPR_ETRD
       COPY SGTKI011.
      * Lista    PREF_TAXA_OPR
       COPY SGTKL068.
      * Inclusão PREF_TAXA_OPR
       COPY SGTKI068.
      * Lista    CND-NEGD-OPR
       COPY SGTKL079.
      * Inclusão CND-NEGD-OPR
       01  SGTSI079-DADOS.
       COPY SGTKI079.
      * Lista    ANX_OPR_ETRD
       COPY SGTKL060.
      * Inclusão ANX_OPR_ETRD
       COPY SGTKI060.
      * Lista    ISLC_OPR_ETRD
       COPY SGTKL080.
      * Inclusão ISLC_OPR_ETRD
       COPY SGTKI080.
      * Lista    PRV_CMPS_GRT
       COPY SGTKL081.
      * Inclusão PRV_CMPS_GRT
       COPY SGTKI081.
      * Lista CLI_OPR_ETRD
       01  SGTSL006-DADOS.
       COPY SGTKL006.
      * CRUD CLI_OPR_ETRD
       01  SGTST006-DADOS.
       COPY SGTKT006.
      * Lista    NOTA_CLSC_OPR
       COPY SGTKL074.
      * Inclusão NOTA_CLSC_OPR
       COPY SGTKI074.
      * Lista CLI_GRTR
       COPY SGTKL082.
      * Inclusão CLI_GRTR
       COPY SGTKI082.
      * Lista EXCO_PRFL_SELD
       COPY SGTKL083.
      * Inclusão EXCO_PRFL_SELD
       COPY SGTKI083.
      * Lista    CTT_EXNO_OPR_ETRD
       COPY SGTKL035.
      * Inclusão CTT_EXNO_OPR_ETRD
       COPY SGTKI035.
      * Lista    NOTA_TCN_VCLD_OPR
       COPY SGTKL067.
      * Inclusão NOTA_TCN_VCLD_OPR
       COPY SGTKI067.
      * Lista    ITEM_EXCO_OPR
       COPY SGTKL095.
      * Inclusão ITEM_EXCO_OPR
       COPY SGTKI095.
      * Lista    JST_VLDC_OPR_ETRD
       01  SGTSL117-DADOS.
       COPY SGTKL117.
      * Inclusão JST_VLDC_OPR_ETRD
       COPY SGTKI117.

      *-----------------------------------------------------------------
      *--> Books Tabelas Ordem de Negociação
      *-----------------------------------------------------------------

      *--> Lista Tabela DB2SGT.ORD_NEGO_OPR
       COPY SGTKL075.
      *--> Inclusão Tabela DB2SGT.ORD_NEGO_OPR
       01  SGTSIV75-DADOS.
       COPY SGTKIV75.

      *--> Lista Tabela DB2SGT.EST_ORD_NEGO_OPR
       01  SGTSL078-DADOS.
       COPY SGTKL078.
      *--> Inclusão Tabela DB2SGT.EST_ORD_NEGO_OPR
       01  SGTSI078-DADOS.
       COPY SGTKI078.

      *--> Lista Tabela DB2SGT.CLI_CPRD_NEGO_VND
      *01  SGTSL076-DADOS.
       COPY SGTKL076.
      *--> Inclusão Tabela DB2SGT.CLI_CPRD_NEGO_VND
       01  SGTSI076-DADOS.
       COPY SGTKI076.

      *--> Lista Tabela DB2SGT.AUTZ_NEGO_OPR_ETRD
       01  SGTSL077-DADOS.
       COPY SGTKL077.
      *--> Inclusão Tabela DB2SGT.AUTZ_NEGO_OPR_ETRD
       01  SGTSI077-DADOS.
       COPY SGTKI077.


       01  CTE-FINAL-SS                PIC  X(40) VALUE
                                       '*** S.S. TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
       COPY ALMKCBL2.

      *--> Habilita captura automatica de abend e erros CICS
       COPY HLPKE999.

           PERFORM  100000-INICIAR

           PERFORM  200000-PROCESSAR

           PERFORM  500000-MOVIMENTAR-SAIDA

           PERFORM  980000-GRAVAR-CONTAINER-RPST

           PERFORM  999999-ENCERRAR

           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

           PERFORM 970000-LER-CANAL

      *--> Define o nome do programa p/ fins manipulação container
           MOVE 'SGTS0145'             TO NM-PGM

      *--> Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

      *--> Caso não seja necessário utilização do Usuário Logado,
      *    a section 974000-LER-DADOS-OPRD poderá ser excluída
           PERFORM 974000-LER-DADOS-OPRD

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *--> Valida Campo a Campo da Requisição
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTS0145-CD-CTRA-TIT-VL     NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0145-CD-CTRA-TIT-VL     EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0145-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0145-CD-OPR-ETRD-MCP    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais in
      -              'válido.'         TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0145-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'    TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0145-NR-VRS-REF         NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0656              TO  MSG1-CD-ERRO-TS
               MOVE  'Numero da versão da operação de referência inválid
      -              'o.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0145-NR-VRS-REF         EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0657              TO  MSG1-CD-ERRO-TS
               MOVE  'Numero da versão da operação de referência não inf
      -              'ormado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0145-CD-TIP-VRS-OPR     EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0241              TO  MSG1-CD-ERRO-TS
               MOVE  'Código tipo de versão da operação não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0145-CD-TIP-VRS-OPR     NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0242              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do tipo de versão da operação inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Tipo de Versão da Operação  = 0001 - (Normalidade, antigo
      *    Original)
           IF  SGTS0145-CD-TIP-VRS-OPR     NOT EQUAL  1
               IF  SGTS0145-DT-INC-VGC     EQUAL  SPACES
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0658          TO  MSG1-CD-ERRO-TS
                   MOVE  'Data início de vigência não informada.'
                                       TO  MSG1-TX-ERRO-TS
                   PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               ELSE
                   MOVE  CORR SGTS0145-DT-INC-VGC-R
                                       TO  ARG01-R
                   MOVE  0004          TO  ARG02
                   PERFORM  910000-VALIDAR-DATAS
               END-IF
           END-IF

           IF  SGTS0145-TX-OBS-VRS         EQUAL  SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0623              TO  MSG1-CD-ERRO-TS
               MOVE  'Texto observação do versionamento não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0145-CD-CLSC-ITMT-FNCO < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
                MOVE  1123          TO  MSG1-CD-ERRO-TS
                MOVE  'Código do Classificação do Instrumento Financeiro
      -               ' inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0145-CD-CLSC-ITMT-FNCO EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1124          TO  MSG1-CD-ERRO-TS
               MOVE  'Código do Classificação do Instrumento Financeiro
      -              'não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Tipo de Versão da Operação  = 0001 - Normalidade
      *--> Tipo de Versão da Operação  = 0002 - Renegociação
      *--> Tipo de Versão da Operação  = 0005 - Reestruturação
           IF  (SGTS0145-CD-TIP-VRS-OPR  EQUAL  1 OR
                SGTS0145-CD-TIP-VRS-OPR  EQUAL  2 OR
                SGTS0145-CD-TIP-VRS-OPR  EQUAL  5)
             IF SGTS0145-CD-MTV-EST-NEGL EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1132              TO  MSG1-CD-ERRO-TS
               MOVE 'Código Motivo do Estado Negocial não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
             END-IF
           END-IF

      *--> Tipo de Versão da Operação  = 0001 - Normalidade
      *--> Tipo de Versão da Operação  = 0002 - Renegociação
      *--> Tipo de Versão da Operação  = 0005 - Reestruturação
           IF  (SGTS0145-CD-TIP-VRS-OPR  EQUAL  1 OR
                SGTS0145-CD-TIP-VRS-OPR  EQUAL  2 OR
                SGTS0145-CD-TIP-VRS-OPR  EQUAL  5)
             IF SGTS0145-CD-MTV-EST-NEGL < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1133              TO  MSG1-CD-ERRO-TS
               MOVE 'Código do Motivo do Estado Negocial inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
             END-IF
           END-IF

      * Tipo de versionamento 2  Renegociação
      * Motivo do Estado Negocial:1 - Renegociação - Origem Atraso,
      *                           2 - Renegociação - RAO
      *                           3 - Renegociação - Origem Renegociação
      *                           99 -Demais

           IF   SGTS0145-CD-TIP-VRS-OPR  EQUAL 2
             IF (( SGTS0145-CD-MTV-EST-NEGL NOT EQUAL 1) AND
                ( SGTS0145-CD-MTV-EST-NEGL NOT EQUAL 2) AND
                ( SGTS0145-CD-MTV-EST-NEGL NOT EQUAL 3) AND
                ( SGTS0145-CD-MTV-EST-NEGL NOT EQUAL 99))

               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 1137               TO MSG1-CD-ERRO-TS
               MOVE 'Código do Motivo do Estado Negocial de Renegociação
      -             'inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
             END-IF
           END-IF

      * Tipo de versionamento 5  Reestruturação
      * Motivo do Estado Negocial:
      *    13 - Reestruturação - Atraso
      *    14 - Reestruturação - Abatimento Negocial
      *    15 - Reestruturação - Origem Ativo Problemático
      *    16 - Reestruturação - Origens Perdas
      *    17 - Reestruturação - Stand Still
      *    18 - Reestruturação - Origens - Agro
      *    19 - Reestruturação - Outros
      *    99 - Demais

           IF   SGTS0145-CD-TIP-VRS-OPR  EQUAL 5
             IF ((SGTS0145-CD-MTV-EST-NEGL NOT EQUAL 13) AND
                (SGTS0145-CD-MTV-EST-NEGL NOT EQUAL 14) AND
                (SGTS0145-CD-MTV-EST-NEGL NOT EQUAL 15) AND
                (SGTS0145-CD-MTV-EST-NEGL NOT EQUAL 16) AND
                (SGTS0145-CD-MTV-EST-NEGL NOT EQUAL 17) AND
                (SGTS0145-CD-MTV-EST-NEGL NOT EQUAL 18) AND
                (SGTS0145-CD-MTV-EST-NEGL NOT EQUAL 19) AND
                (SGTS0145-CD-MTV-EST-NEGL NOT EQUAL 99))

               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 1138               TO MSG1-CD-ERRO-TS
               MOVE 'Código do Motivo do Estado Negocial de Reestruturaç
      -         'ão inválido.'         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
             END-IF
           END-IF

      * Necessidade da tabela do OPR, o motivo do estado negocial nao po
      * de ser Zeros  quando tipo versionamento = 1 Normalidade.

           IF   SGTS0145-CD-TIP-VRS-OPR  EQUAL 1
             IF SGTS0145-CD-MTV-EST-NEGL NOT EQUAL 99
                SET ERRO-PROGRAMADO     TO TRUE
                MOVE 1139               TO MSG1-CD-ERRO-TS
                MOVE 'Código do Motivo do Estado Negocial de normalidade
      -         ' inválido.'            TO MSG1-TX-ERRO-TS
                PERFORM 999000-GRAVA-ERRO-E-ENCERRA
             END-IF
           END-IF

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           PERFORM  200100-VER-ULT-ESTADO-VERSAO

           PERFORM  210000-CONSULTAR-OPERACAO

           PERFORM  220000-INCLUIR-NOVA-VERSAO

           PERFORM 230000-INCLUIR-ESTADO-VERSAO
           PERFORM 240000-OPERACAO-VINCULADA
           PERFORM 250000-INDICADOR-OPERACAO
           PERFORM 260000-TARIFA-OPERACAO
           PERFORM 270000-REPACTUACAO-PROGRAMADA
           PERFORM 280000-RECEBIMENTO-PREVISTO
           PERFORM 300000-BANCO-PARTICIPANTE

           PERFORM  310000-ESTADO-OPERACAO

           PERFORM 320000-REMUNERACAO-OPERACAO
           PERFORM 330000-PRECIFICACAO-TAXA
           PERFORM 340000-CONDICAO-NEGOCIADA
           PERFORM 350000-ANEXO-OPERACAO
           PERFORM 360000-INSOLVENCIA-OPERACAO
           PERFORM 370000-PREVISAO-COMPOSICAO
           PERFORM 380000-CLIENTE-OPERACAO
           PERFORM 390000-NOTA-CLASSIFICACAO

           PERFORM  400000-ORDEM-NEGOCIACAO

           PERFORM  410000-ESTADO-ORDEM-NEGOCIACAO

           PERFORM  420000-CLI-COMPRA-NEGOCIACAO

           PERFORM  430000-AUTORIZACAO-NEGOCIACAO

           PERFORM 440000-EXCECAO-PERFIL
           PERFORM 450000-CONTATO-EXTERNO
           PERFORM 460000-NOTA-TECNICA
           PERFORM 470000-ITEM-EXCECAO-OPERACAO
           PERFORM 480000-CLIENTE-GARANTIDOR
           PERFORM 490000-JUSTIFICATIVAS

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200100-VER-ULT-ESTADO-VERSAO    SECTION.
      *--> Consulta o Estado e Tipo da Última Versão da Operação.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

      *--> Verificar último estado da versão antes de realizar o
      *    versionamento, não poderá existir nenhuma versão com o
      *    estado diferente de 0003 - aprovado.
      *    Esta verificação é feita para evitar que dois versionamentos
      *    simultaneos sejam realizados em telas diferentes.

      *--> Movimentar o Nome do Programa Executado
           MOVE  SGTSPE05              TO  NM-PGM

      *--> Movimentar os Campos de Entrada
           MOVE  SGTS0145-CD-CTRA-TIT-VL
                                       TO  SGTSPE05-CD-CTRA-TIT-VL
           MOVE  SGTS0145-CD-OPR-ETRD-MCP
                                       TO  SGTSPE05-CD-OPR-ETRD-MCP

           PERFORM 900000-EXECUTAR-MODULO

           IF  SGTSPE05-CD-TIP-EST-VRS     LESS  0003
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1092              TO  MSG1-CD-ERRO-TS
               MOVE  'Inclusão de versão não permitida, existe outra ver
      -              'são não aprovada no sistema.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       200100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-CONSULTAR-OPERACAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SPACES                TO  SGTSX005-DADOS
           INITIALIZE                      SGTSX005-DADOS
                                           REPLACING NUMERIC  BY ZEROS

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE  SGTSX005              TO  NM-PGM

      *--> Movimentar os campos de entrada
           SET   SGTSX005-CONSULTAR    TO  TRUE
           MOVE  SGTS0145-CD-CTRA-TIT-VL
                                       TO  SGTSX005-CD-CTRA-TIT-VL
           MOVE  SGTS0145-CD-OPR-ETRD-MCP
                                       TO  SGTSX005-CD-OPR-ETRD-MCP
           MOVE  SGTS0145-NR-VRS-REF   TO  SGTSX005-NR-VRS-OPR-ETRD

           PERFORM  920000-CHAMAR-SGTSX005

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-INCLUIR-NOVA-VERSAO      SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE  SGTSX005              TO  NM-PGM

      *--> Movimentar os campos de entrada
           SET   SGTSX005-INCLUIR      TO  TRUE
           MOVE  SGTS0145-CD-CTRA-TIT-VL
                                       TO  SGTSX005-CD-CTRA-TIT-VL
           MOVE  SGTS0145-CD-OPR-ETRD-MCP
                                       TO  SGTSX005-CD-OPR-ETRD-MCP
           MOVE  ZEROS                 TO  SGTSX005-NR-VRS-OPR-ETRD

           MOVE  KOPRD-CD-OPRD         TO  SGTSX005-CD-USU-ULT-ATL
           MOVE  'CURRENT'             TO  SGTSX005-TS-ULT-ATL

           MOVE  SGTS0145-NR-VRS-REF
                                       TO  SGTSX005-NR-VRS-REF
      *--> Tipo de versão da operação  - Original = 1
           IF  SGTS0145-CD-TIP-VRS-OPR     EQUAL  0001
               MOVE  '  .  .    '      TO  SGTSX005-DT-INC-VGC-VRS-OPR
               MOVE  CORR SGTSX005-DT-CPR-OPR-R
                                       TO  SGTSX005-DT-INC-VGC-VRS-OPR-R
           ELSE
               MOVE  '  .  .    '      TO  SGTSX005-DT-INC-VGC-VRS-OPR

               MOVE  CORR SGTS0145-DT-INC-VGC-R
                                       TO  SGTSX005-DT-INC-VGC-VRS-OPR-R
           END-IF

      *CD-TIP-VRS-OPR = 1-Normalidade->CD-CLSC-ITMT-FNCO=1-Normal
      *CD-TIP-VRS-OPR = 2-Renegociação ou 3-Repactuação ->
      *      CD-CLSC-ITMT-FNCO=2-RC-Risco de crédito aumentado
      *CD-TIP-VRS-OPR = 5-Reestruturação->
      *      CD-CLSC-ITMT-FNCO=3-Ativo Problemático(stop accrual)

           MOVE SGTS0145-CD-TIP-VRS-OPR
                                       TO  SGTSX005-CD-TIP-VRS-OPR
           MOVE SGTS0145-CD-CLSC-ITMT-FNCO
                                       TO SGTSX005-CD-CLSC-ITMT-FNCO

      * CD-CLSC-ITMT-FNCO EQUAL = 1- Normal ou
      * CD-CLSC-ITMT-FNCO EQUAL = 2- RC- Risco de crédito aumentado->
      * Estágio Negocial = 1-Normalidade

           IF SGTS0145-CD-CLSC-ITMT-FNCO EQUAL 1 OR
              SGTS0145-CD-CLSC-ITMT-FNCO EQUAL 2
                 MOVE 1                    TO SGTSX005-CD-ESG-NEGL
           END-IF

      * CD-CLSC-ITMT-FNCO EQUAL=3- Problema de recuperação de crédito->
      * Estágio Negocial = 3-Ativo Problemático(stop accrual)

           IF SGTS0145-CD-CLSC-ITMT-FNCO EQUAL 3
                MOVE 3                 TO SGTSX005-CD-ESG-NEGL
           END-IF

           MOVE SGTS0145-CD-MTV-EST-NEGL
                                        TO SGTSX005-CD-MTV-EST-NEGL

           PERFORM  920000-CHAMAR-SGTSX005

           IF  SGTSX005-CD-RTN-PGM         EQUAL ZEROS
               MOVE  SGTSX005-NR-VRS-OPR-ETRD
                                       TO  SGTS0145-NR-VRS-NOVA
           END-IF

           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-INCLUIR-ESTADO-VERSAO    SECTION.
      *--> Inclui o tipo de estado da versao da operaçao: 1- Em Edição
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSI069-DADOS

      *    Movimentar os campos de entrada
           MOVE SGTSI069               TO NM-PGM
           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSI069-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSI069-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-NOVA   TO SGTSI069-NR-VRS-OPR-ETRD
      *    EM-EDICAO = 1
           MOVE  1                     TO SGTSI069-CD-TIP-EST-VRS
           MOVE  SGTS0145-TX-OBS-VRS   TO SGTSI069-TX-OBS-EST-VRS

           PERFORM 900000-EXECUTAR-MODULO
           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       240000-OPERACAO-VINCULADA       SECTION.
      * Operação que é paga por outra operação
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL507-RQSC
                                       SGTSL507-QT-OCR-TAB
                                       SGTSL507-IC-TAB
                                       SGTSL507-IN-CTNC-PSQ

      *    Movimentar os campos de entrada
           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSL507-CD-CTRA-OPR-PGDR
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSL507-CD-OPR-PGDR
           MOVE SGTS0145-NR-VRS-REF
                                       TO SGTSL507-NR-VRS-OPR-PGDR
           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSL507-CD-CTRA-OPR-VCLD-E


           PERFORM UNTIL SGTSL507-IN-CTNC-PSQ  EQUAL  'N'
              PERFORM 241000-LISTAR-PAGAMENTO
              PERFORM 242000-INCLUI-PAGAMENTO
                         VARYING SGTSL507-IC-TAB FROM 1 BY 1
                           UNTIL SGTSL507-IC-TAB > SGTSL507-QT-OCR-TAB

      * Campos de rechamada da Operação que paga outra operação
              IF SGTSL507-QT-OCR-TAB > ZEROS
                 MOVE SGTSL507-CD-OPR-ETRD-VCLD (SGTSL507-QT-OCR-TAB)
                                       TO SGTSL507-CD-OPR-ETRD-VCLD-E
                 MOVE SGTSL507-NR-VRS-OPR-VCLD (SGTSL507-QT-OCR-TAB)
                                       TO SGTSL507-NR-VRS-OPR-VCLD-E
              END-IF

           END-PERFORM

           .
       240000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       241000-LISTAR-PAGAMENTO         SECTION.
      *----------------------------------------------------------------*
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSL507               TO NM-PGM

           PERFORM 900000-EXECUTAR-MODULO
           .
       241000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       242000-INCLUI-PAGAMENTO         SECTION.
      *----------------------------------------------------------------*
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSI071               TO NM-PGM

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSI071-CD-CTRA-OPR-PGDR
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSI071-CD-OPR-PGDR
           MOVE SGTS0145-NR-VRS-NOVA
                                       TO SGTSI071-NR-VRS-OPR-PGDR
           MOVE SGTSL507-CD-CTRA-OPR-VCLD (SGTSL507-IC-TAB)
                                       TO SGTSI071-CD-CTRA-OPR-VCLD
           MOVE SGTSL507-CD-OPR-ETRD-VCLD (SGTSL507-IC-TAB)
                                       TO SGTSI071-CD-OPR-ETRD-VCLD
           MOVE SGTSL507-NR-VRS-OPR-VCLD (SGTSL507-IC-TAB)
                                       TO SGTSI071-NR-VRS-OPR-VCLD
           MOVE SGTSL507-VL-PGTO (SGTSL507-IC-TAB)
                                       TO SGTSI071-VL-PGTO
           MOVE 'CURRENT'              TO SGTSI071-TS-ULT-ATL


           PERFORM 900000-EXECUTAR-MODULO

           .
       242000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       250000-INDICADOR-OPERACAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL063-RQSC
                                       SGTSL063-QT-OCR-TAB
                                       SGTSL063-IC-TAB
                                       SGTSL063-IN-CTNC-PSQ
      *    Movimentar os campos de entrada

           MOVE 2                      TO SGTSL063-CD-ACAO
           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSL063-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSL063-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-REF
                                       TO SGTSL063-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL063-IN-CTNC-PSQ  EQUAL  'N'
              PERFORM 251000-LISTAR-INDICADOR
              PERFORM 252000-INCLUI-INDICADOR
                      VARYING SGTSL063-IC-TAB FROM 1 BY 1
                        UNTIL SGTSL063-IC-TAB > SGTSL063-QT-OCR-TAB

      * Campos de rechamada dos Indicadores da Operação
              IF SGTSL063-QT-OCR-TAB > ZEROS
                 MOVE SGTSL063-CD-TIP-IN (SGTSL063-QT-OCR-TAB)
                                       TO SGTSL063-CD-TIP-IN-E
              END-IF
           END-PERFORM

           .
       250000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       251000-LISTAR-INDICADOR         SECTION.
      *----------------------------------------------------------------*
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO


      *    Movimentar o nome do programa a ser executado
           MOVE SGTSL063               TO NM-PGM

           PERFORM 900000-EXECUTAR-MODULO
           .
       251000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       252000-INCLUI-INDICADOR         SECTION.
      *----------------------------------------------------------------*
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSI063               TO NM-PGM

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSI063-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSI063-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-NOVA   TO SGTSI063-NR-VRS-OPR-ETRD
           MOVE SGTSL063-CD-TIP-IN (SGTSL063-IC-TAB)
                                       TO SGTSI063-CD-TIP-IN


           PERFORM 900000-EXECUTAR-MODULO
           .
       252000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       260000-TARIFA-OPERACAO          SECTION.
      *----------------------------------------------------------------*
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO
      *
           INITIALIZE                  SGTSL084-RQSC
                                       SGTSL084-QT-OCR-TAB
                                       SGTSL084-IC-TAB
                                       SGTSL084-IN-CNTO-PSQ

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSL084-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSL084-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-REF    TO SGTSL084-NR-VRS-OPR-ETRD


           PERFORM UNTIL SGTSL084-IN-CNTO-PSQ  EQUAL  'N'
               PERFORM 261000-LISTAR-TARIFA
               PERFORM 262000-INCLUI-TARIFA
                      VARYING SGTSL084-IC-TAB FROM 1 BY 1
                        UNTIL SGTSL084-IC-TAB > SGTSL084-QT-OCR-TAB

      * Campos de rechamada da Tarifa da Operação
              IF  SGTSL084-QT-OCR-TAB > ZEROS
                   MOVE SGTSL084-CD-TIP-TARF (SGTSL084-QT-OCR-TAB)
                                       TO SGTSL084-CD-TIP-TARF-E
              END-IF
           END-PERFORM
           .
       260000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       261000-LISTAR-TARIFA            SECTION.
      *----------------------------------------------------------------*
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSL084               TO NM-PGM


           PERFORM 900000-EXECUTAR-MODULO

           .
       261000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       262000-INCLUI-TARIFA            SECTION.
      *----------------------------------------------------------------*
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSI084               TO NM-PGM

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSI084-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSI084-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-NOVA
                                       TO SGTSI084-NR-VRS-OPR-ETRD
           MOVE SGTSL084-CD-TIP-TARF (SGTSL084-IC-TAB)
                                       TO SGTSI084-CD-TIP-TARF
           MOVE KOPRD-CD-OPRD          TO SGTSI084-CD-USU-ULT-ATL
           MOVE 'CURRENT'              TO SGTSI084-TS-ULT-ATL
           MOVE SGTSL084-PC-TARF-OPR (SGTSL084-IC-TAB)
                                       TO SGTSI084-PC-TARF-OPR


           PERFORM 900000-EXECUTAR-MODULO

           .
       262000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       270000-REPACTUACAO-PROGRAMADA   SECTION.
      *----------------------------------------------------------------*
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL061-RQSC
                                       SGTSL061-QT-OCR-TAB
                                       SGTSL061-IC-TAB
                                       SGTSL061-IN-CNTO-PSQ

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSL061-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSL061-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-REF
                                       TO SGTSL061-NR-VRS-OPR-ETRD


           PERFORM UNTIL SGTSL061-IN-CNTO-PSQ  EQUAL  'N'
               PERFORM 271000-LISTAR-REPACTUACAO
               PERFORM 272000-INCLUI-REPACTUACAO
                      VARYING SGTSL061-IC-TAB FROM 1 BY 1
                      UNTIL SGTSL061-IC-TAB > SGTSL061-QT-OCR-TAB

      * Campos de rechamada da Repactuação Programada
              IF SGTSL061-QT-OCR-TAB > ZEROS
                 MOVE SGTSL061-DT-RPAC-PGMD (SGTSL061-QT-OCR-TAB)
                                       TO SGTSL061-DT-RPAC-ULT
              END-IF
           END-PERFORM

           .
       270000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       271000-LISTAR-REPACTUACAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO


      *    Movimentar o nome do programa a ser executado
           MOVE SGTSL061               TO NM-PGM


           PERFORM 900000-EXECUTAR-MODULO

           .
       271000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       272000-INCLUI-REPACTUACAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO


           MOVE SGTSI061               TO NM-PGM

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSI061-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSI061-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-NOVA
                                       TO SGTSI061-NR-VRS-OPR-ETRD

           MOVE '  .  .    '        TO SGTSI061-DT-RPAC-PGMD
           MOVE CORR SGTSL061-DT-RPAC-PGMD-R (SGTSL061-IC-TAB)
                                       TO SGTSI061-DT-RPAC-PGMD-R

           MOVE KOPRD-CD-OPRD          TO SGTSI061-CD-USU-ULT-ATL
           MOVE 'CURRENT'             TO SGTSI061-TS-ULT-ATL
           MOVE SGTSL061-TX-MTV-RPAC-PGMD (SGTSL061-IC-TAB)
                                       TO SGTSI061-TX-MTV-RPAC-PGMD


           PERFORM 900000-EXECUTAR-MODULO
           .
       272000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       280000-RECEBIMENTO-PREVISTO     SECTION.
      *----------------------------------------------------------------*
           MOVE 0020                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL070-RQSC
                                       SGTSL070-QT-OCR
                                       SGTSL070-IC-TAB
                                       SGTSL070-IN-CTNC-PSQ

           SET SGTSL070-LISTA-ULT-NR-SEQL
                                       TO TRUE

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSL070-E-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSL070-E-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-REF
                                       TO SGTSL070-E-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL070-IN-CTNC-PSQ  EQUAL  'N'
                 PERFORM 281000-LISTAR-RCBT-PREVISTO
                 PERFORM 282000-INCLUI-RCBT-PREVISTO
                         VARYING SGTSL070-IC-TAB FROM 1 BY 1
                         UNTIL SGTSL070-IC-TAB > SGTSL070-QT-OCR

      * Campos de rechamada do Recebimento Previsto
              IF  SGTSL070-QT-OCR > ZEROS
                  MOVE SGTSL070-CD-TIP-RCBT-PRVT (SGTSL070-QT-OCR)
                                       TO SGTSL070-CD-TIP-RCBT-PRVT-ULT
                  MOVE SGTSL070-DT-RCBT-PRVT (SGTSL070-QT-OCR)
                                       TO SGTSL070-DT-RCBT-PRVT-ULT
              END-IF
           END-PERFORM

           .
       280000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       281000-LISTAR-RCBT-PREVISTO     SECTION.
      *----------------------------------------------------------------*
           MOVE 0021                   TO MSG1-CD-SEQL-ERRO


           MOVE SGTSL070               TO NM-PGM

           PERFORM 900000-EXECUTAR-MODULO

           .
       281000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       282000-INCLUI-RCBT-PREVISTO     SECTION.
      *----------------------------------------------------------------*
           MOVE 0022                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSI070               TO NM-PGM

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSI070-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSI070-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-NOVA
                                       TO SGTSI070-NR-VRS-OPR-ETRD
           MOVE 1                      TO SGTSI070-NR-SEQL-RCBT-PRVT
           MOVE SGTSL070-CD-TIP-RCBT-PRVT (SGTSL070-IC-TAB)
                                       TO SGTSI070-CD-TIP-RCBT-PRVT

           MOVE SGTSL070-DT-RCBT-PRVT (SGTSL070-IC-TAB)
                                       TO SGTSI070-DT-RCBT-PRVT
           MOVE 'CURRENT'              TO SGTSI070-TS-ULT-ATL

           MOVE SGTSL070-PC-RCBT-PRVT (SGTSL070-IC-TAB)
                                       TO SGTSI070-PC-RCBT-PRVT
           MOVE SGTSL070-VL-RCBT-PRVT (SGTSL070-IC-TAB)
                                       TO SGTSI070-VL-RCBT-PRVT
           MOVE SGTSL070-TX-OBS-RCBT-PRVT (SGTSL070-IC-TAB)
                                       TO SGTSI070-TX-OBS-RCBT-PRVT

           PERFORM 900000-EXECUTAR-MODULO

      * Para cada registro do Recebimento Previsto faz o tratamento do
      * Recebimento Realizado
           PERFORM 290000-RECEBIMENTO-REALIZADO


           .
       282000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       290000-RECEBIMENTO-REALIZADO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0023                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL087-RQSC
                                       SGTSL087-QT-OCR
                                       SGTSL087-IC-TAB
                                       SGTSL087-IN-CTNC-PSQ
      * NR-SEQL-RCBT-PRVT (1): número máximo recuperado para utilizar no
      * Recebimento Realizado
           IF  SGTSL070-NR-SEQL-RCBT-PRVT (1)  > ZEROS
               SET SGTSL087-LISTA-TIPO-E-DATA
                                       TO TRUE

               MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSL087-CD-CTRA-TIT-VL
               MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSL087-CD-OPR-ETRD-MCP
               MOVE SGTS0145-NR-VRS-REF
                                       TO SGTSL087-NR-VRS-OPR-ETRD

               MOVE SGTSL070-NR-SEQL-RCBT-PRVT (1)
                                       TO SGTSL087-NR-SEQL-RCBT-PRVT
               MOVE SGTSX005-CD-TIT-OPR-ETRD
                                       TO SGTSL087-CD-TIT-OPR-ETRD
               MOVE SGTSX005-NR-DVS-TIT
                                       TO SGTSL087-NR-DVS-TIT
               MOVE SGTSL070-CD-TIP-RCBT-PRVT (SGTSL070-IC-TAB)
                                       TO SGTSL087-CD-TIP-RCBT-PRVT-ULT
               MOVE SGTSL070-DT-RCBT-PRVT (SGTSL070-IC-TAB)
                                       TO SGTSL087-DT-RCBT-PRVT-ULT

              PERFORM UNTIL SGTSL087-IN-CTNC-PSQ  EQUAL  'N'
                 PERFORM 291000-LISTAR-RCBT-REALIZADO
                 PERFORM 292000-INCLUI-RCBT-REALIZADO
                         VARYING SGTSL087-IC-TAB FROM 1 BY 1
                        UNTIL SGTSL087-IC-TAB > SGTSL087-QT-OCR

      * Campos de rechamada da Recebimento Realizado
                IF  SGTSL087-QT-OCR > ZEROS
                  MOVE SGTSL087-TS-REG-PGTO (SGTSL087-QT-OCR)
                                       TO SGTSL087-TS-REG-PGTO-ULT
                 END-IF
              END-PERFORM
           END-IF
           .
       290000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       291000-LISTAR-RCBT-REALIZADO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0024                   TO MSG1-CD-SEQL-ERRO
      *
           MOVE SGTSL087               TO NM-PGM


           PERFORM 900000-EXECUTAR-MODULO

           .
       291000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       292000-INCLUI-RCBT-REALIZADO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0025                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSI087               TO NM-PGM

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSI087-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSI087-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-NOVA
                                       TO SGTSI087-NR-VRS-OPR-ETRD
           MOVE   1
                                       TO SGTSI087-NR-SEQL-RCBT-PRVT
           MOVE SGTSL087-CD-TIP-RCBT-PRVT (SGTSL087-IC-TAB)
                                       TO SGTSI087-CD-TIP-RCBT-PRVT
           MOVE SGTSL087-DT-RCBT-PRVT (SGTSL087-IC-TAB)
                                       TO SGTSI087-DT-RCBT-PRVT
           MOVE SGTSX005-CD-TIT-OPR-ETRD
                                       TO SGTSI087-CD-TIT-OPR-ETRD
           MOVE SGTSX005-NR-DVS-TIT
                                       TO SGTSI087-NR-DVS-TIT
           MOVE SGTSL087-TS-REG-PGTO (SGTSL087-IC-TAB)
                                       TO SGTSI087-TS-REG-PGTO
           MOVE SGTSL087-IN-VCL-MNL (SGTSL087-IC-TAB)
                                       TO SGTSI087-IN-VCL-MNL
           MOVE SGTSL087-VL-RLZD-VCLD (SGTSL087-IC-TAB)
                                       TO SGTSI087-VL-RLZD-VCLD
           MOVE 'CURRENT'
                                       TO SGTSI087-TS-ULT-ATL
           MOVE KOPRD-CD-OPRD
                                       TO SGTSI087-CD-USU-VCL


           PERFORM 900000-EXECUTAR-MODULO
           .
       292000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       300000-BANCO-PARTICIPANTE       SECTION.
      *----------------------------------------------------------------*
           MOVE 0026                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL064-RQSC
                                       SGTSL064-QT-OCR-TAB
                                       SGTSL064-IC-TAB
                                       SGTSL064-IN-CNTO-PSQ

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSL064-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSL064-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-REF
                                       TO SGTSL064-NR-VRS-OPR-ETRD
           MOVE ZEROS
                                       TO SGTSL064-CD-TIP-PRTC

           PERFORM UNTIL SGTSL064-IN-CNTO-PSQ  EQUAL  'N'
              PERFORM 301000-LISTAR-BCO-PCT-OPR
              PERFORM 302000-INCLUI-BCO-PCT-OPR
                         VARYING SGTSL064-IC-TAB FROM 1 BY 1
                         UNTIL   SGTSL064-IC-TAB > SGTSL064-QT-OCR-TAB

      * Campos de rechamada da lista de Bancos Participantes
              IF SGTSL064-QT-OCR-TAB > ZEROS
                 MOVE SGTSL064-CD-TIP-PRTC-S (SGTSL064-QT-OCR-TAB)
                                       TO SGTSL064-CD-ULT-TIP-PRTC
                 MOVE SGTSL064-CD-BCO-PCT (SGTSL064-QT-OCR-TAB)
                                       TO SGTSL064-CD-ULT-BCO-PCT
              END-IF
           END-PERFORM
           .
       300000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       301000-LISTAR-BCO-PCT-OPR       SECTION.
      * Lista Banco Participante da Operação
      *----------------------------------------------------------------*
           MOVE 0027                   TO MSG1-CD-SEQL-ERRO


           MOVE SGTSL064               TO NM-PGM

           PERFORM 900000-EXECUTAR-MODULO

           .
       301000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       302000-INCLUI-BCO-PCT-OPR       SECTION.
      * Inclusão do Banco Participante da Operação
      *----------------------------------------------------------------*
           MOVE 0028                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSI064               TO NM-PGM

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSI064-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSI064-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-NOVA
                                       TO SGTSI064-NR-VRS-OPR-ETRD
           MOVE SGTSL064-CD-BCO-PCT (SGTSL064-IC-TAB)
                                       TO SGTSI064-CD-BCO-PCT
           MOVE SGTSL064-CD-TIP-PRTC-S (SGTSL064-IC-TAB)
                                       TO SGTSI064-CD-TIP-PRTC
           MOVE SGTSL064-IN-BCO-LDR (SGTSL064-IC-TAB)
                                       TO SGTSI064-IN-BCO-LDR
           MOVE SGTSL064-PC-PRTC-BCO (SGTSL064-IC-TAB)
                                       TO SGTSI064-PC-PRTC-BCO
           MOVE 'CURRENT'             TO SGTSI064-TS-ULT-ATL
           MOVE KOPRD-CD-OPRD          TO SGTSI064-CD-USU-ULT-ATL

           PERFORM 900000-EXECUTAR-MODULO

           .
       302000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       310000-ESTADO-OPERACAO          SECTION.
      *-----------------------------------------------------------------
           MOVE  0029                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  311000-CONSULTA-ULTIMO-EST-OPR

           PERFORM  312000-INCLUI-EST-OPR
           .
       310000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       311000-CONSULTA-ULTIMO-EST-OPR  SECTION.
      *--> Recupera o ultimo estado a versão referencia
      *-----------------------------------------------------------------
           MOVE  0030                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SPACES                TO  SGTST009-DADOS
           INITIALIZE                      SGTST009-DADOS
                                           REPLACING NUMERIC  BY ZEROS

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE  SGTST009              TO  NM-PGM

      *--> Movimentar os campos de entrada
           SET   SGTST009-CONSULTAR-EST-ATIVO
                                       TO  TRUE
           MOVE  SGTS0145-CD-CTRA-TIT-VL
                                       TO  SGTST009-CD-CTRA-TIT-VL
           MOVE  SGTS0145-CD-OPR-ETRD-MCP
                                       TO  SGTST009-CD-OPR-ETRD-MCP
           MOVE  KOPRD-CD-OPRD         TO  SGTST009-CD-USU-RSP-ATL

           PERFORM  930000-CHAMAR-SGTST009

           .
       311000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       312000-INCLUI-EST-OPR           SECTION.
      *--> Inclui Estado da Operação Estruturada
      *-----------------------------------------------------------------
           MOVE 0031                   TO MSG1-CD-SEQL-ERRO

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE  SGTST009              TO  NM-PGM

      *--> Movimentar os campos de entrada
           SET   SGTST009-INCLUIR      TO  TRUE
           MOVE  SGTS0145-CD-CTRA-TIT-VL
                                       TO  SGTST009-CD-CTRA-TIT-VL
           MOVE  SGTS0145-CD-OPR-ETRD-MCP
                                       TO  SGTST009-CD-OPR-ETRD-MCP
           MOVE  SGTS0145-NR-VRS-NOVA
                                       TO  SGTST009-NR-VRS-OPR-ETRD
           MOVE  'CURRENT'             TO  SGTST009-TS-EST-OPR-ETRD
           MOVE  KOPRD-CD-OPRD         TO  SGTST009-CD-USU-RSP-EST
           MOVE  KOPRD-CD-OPRD         TO  SGTST009-CD-USU-RSP-ATL

           PERFORM  930000-CHAMAR-SGTST009

           .
       312000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       320000-REMUNERACAO-OPERACAO     SECTION.
      *-----------------------------------------------------------------
           MOVE 0032                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL011-RQSC
                                       SGTSL011-QT-OCR-TAB
                                       SGTSL011-IC-TAB
                                       SGTSL011-IN-CTNC-PSQ
      *    Movimentar os campos de entrada
           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSL011-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSL011-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-REF
                                       TO SGTSL011-NR-VRS-OPR-ETRD


           PERFORM UNTIL SGTSL011-IN-CTNC-PSQ  EQUAL  'N'
              PERFORM 321000-LISTAR-REMUNERACAO
              PERFORM 322000-INCLUI-REMUNERACAO
                         VARYING SGTSL011-IC-TAB FROM 1 BY 1
                        UNTIL SGTSL011-IC-TAB > SGTSL011-QT-OCR-TAB

      * Campos de rechamada da lista de Remuneração da Operação
              IF SGTSL011-QT-OCR-TAB > ZEROS
                 MOVE SGTSL011-DT-INC-TAXA-RMNC (SGTSL011-QT-OCR-TAB)
                                       TO SGTSL011-DT-INC-TAXA-ULT
              END-IF

           END-PERFORM

           .
       320000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       321000-LISTAR-REMUNERACAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0033                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSL011               TO NM-PGM

           PERFORM 900000-EXECUTAR-MODULO

           .
       321000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       322000-INCLUI-REMUNERACAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0034                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSI011               TO NM-PGM

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSI011-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSI011-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-NOVA
                                       TO SGTSI011-NR-VRS-OPR-ETRD

           MOVE '  .  .    '           TO SGTSI011-DT-INC-TAXA-RMNC

           MOVE CORR SGTSL011-DT-INC-TAXA-RMNC-R (SGTSL011-IC-TAB)
                                       TO SGTSI011-DT-INC-TAXA-RMNC-R

           MOVE 'CURRENT'              TO SGTSI011-TS-ULT-ATL

           MOVE KOPRD-CD-OPRD          TO SGTSI011-CD-USU-RSP-ATL

           MOVE SGTSL011-PC-TAXA-RMNC-OPR(SGTSL011-IC-TAB)
                                       TO SGTSI011-PC-TAXA-RMNC-OPR

           MOVE '  .  .    '           TO SGTSI011-DT-FIM-TAXA-RMNC

           MOVE CORR SGTSL011-DT-FIM-TAXA-RMNC-R (SGTSL011-IC-TAB)
                                       TO SGTSI011-DT-FIM-TAXA-RMNC-R

           MOVE SGTSL011-CD-TIP-FMA-APL (SGTSL011-IC-TAB)
                                       TO SGTSI011-CD-TIP-FMA-APL
           MOVE SGTSL011-CD-IEC-RMNC (SGTSL011-IC-TAB)
                                       TO SGTSI011-CD-IEC-RMNC
           MOVE SGTSL011-CD-PERC-CPTZ-JUR (SGTSL011-IC-TAB)
                                       TO SGTSI011-CD-PERC-CPTZ-JUR

           PERFORM 900000-EXECUTAR-MODULO

           .
       322000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       330000-PRECIFICACAO-TAXA     SECTION.
      *----------------------------------------------------------------*
           MOVE 0035                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL068-RQSC
                                       SGTSL068-QT-OCR-TAB
                                       SGTSL068-IC-TAB
                                       SGTSL068-IN-CNTO-PSQ

      *    Movimentar os campos de entrada
           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSL068-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSL068-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-REF
                                       TO SGTSL068-NR-VRS-OPR-ETRD


           PERFORM UNTIL SGTSL068-IN-CNTO-PSQ  EQUAL  'N'
              PERFORM 331000-LISTAR-PRECIFICACAO
              PERFORM 332000-INCLUIR-PRECIFICACAO
                         VARYING SGTSL068-IC-TAB FROM 1 BY 1
                         UNTIL   SGTSL068-IC-TAB > SGTSL068-QT-OCR-TAB
      * Campos de rechamada da lista de Precificação da Taxa da Opr
           IF SGTSL068-QT-OCR-TAB > ZEROS
              MOVE SGTSL068-CD-TIP-TAXA-S (SGTSL068-QT-OCR-TAB)
                                       TO SGTSL068-CD-ULT-TIP-TAXA
              MOVE SGTSL068-DT-INC-CND-TAXA (SGTSL068-QT-OCR-TAB)
                                       TO SGTSL068-DT-ULT-INC-VLD
           END-IF
           END-PERFORM

           .
       330000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       331000-LISTAR-PRECIFICACAO   SECTION.
      *----------------------------------------------------------------*
           MOVE 0036                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSL068               TO NM-PGM


           PERFORM 900000-EXECUTAR-MODULO

           .
       331000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       332000-INCLUIR-PRECIFICACAO  SECTION.
      *----------------------------------------------------------------*
           MOVE 0037                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSI068               TO NM-PGM

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSI068-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSI068-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-NOVA
                                       TO SGTSI068-NR-VRS-OPR-ETRD
           MOVE  SGTSL068-CD-TIP-TAXA-S (SGTSL068-IC-TAB)
                                       TO SGTSI068-CD-TIP-TAXA

           MOVE '  .  .    '           TO SGTSI068-DT-INC-CND-TAXA

           MOVE CORR SGTSL068-DT-INC-CND-TAXA-R (SGTSL068-IC-TAB)
                                       TO SGTSI068-DT-INC-CND-TAXA-R

           MOVE 'CURRENT'              TO SGTSI068-TS-ULT-ATL

           MOVE KOPRD-CD-OPRD          TO SGTSI068-CD-USU-ULT-ATL

           MOVE '  .  .    '           TO SGTSI068-DT-FIM-CND-TAXA

           MOVE CORR SGTSL068-DT-FIM-CND-TAXA-R (SGTSL068-IC-TAB)
                                       TO SGTSI068-DT-FIM-CND-TAXA-R

           MOVE SGTSL068-PC-MIN-TAXA (SGTSL068-IC-TAB)
                                       TO SGTSI068-PC-MIN-TAXA
           MOVE SGTSL068-CD-TIP-APL-PREF (SGTSL068-IC-TAB)
                                       TO SGTSI068-CD-TIP-APL-PREF
           MOVE SGTSL068-CD-PERC-APL-PREF (SGTSL068-IC-TAB)
                                       TO SGTSI068-CD-PERC-APL-PREF
           MOVE SGTSL068-CD-IEC-PREF (SGTSL068-IC-TAB)
                                       TO SGTSI068-CD-IEC-PREF


           PERFORM 900000-EXECUTAR-MODULO

           .
       332000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       340000-CONDICAO-NEGOCIADA       SECTION.
      *----------------------------------------------------------------*
           MOVE 0038                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL079-RQSC
                                       SGTSL079-IC-TAB
                                       SGTSL079-QT-OCR-TAB
                                       SGTSL079-IN-CNTO-PSQ

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSL079-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSL079-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-REF
                                       TO SGTSL079-NR-VRS-OPR-ETRD


           PERFORM UNTIL SGTSL079-IN-CNTO-PSQ  EQUAL  'N'
              PERFORM 341000-LISTAR-CND-NEGD-OPR
              PERFORM 342000-INCLUI-CND-NEGD-OPR
                         VARYING SGTSL079-IC-TAB FROM 1 BY 1
                         UNTIL   SGTSL079-IC-TAB > SGTSL079-QT-OCR-TAB
      * Campos de rechamada da lista de Condição Negociada da Operação
             IF SGTSL079-QT-OCR-TAB > ZEROS
                MOVE SGTSL079-CD-TIP-CND-NEGD-S
                                          (SGTSL079-QT-OCR-TAB)
                                       TO SGTSL079-CD-ULT-TIP-CND-NEGD
                MOVE CORR SGTSL079-DT-INC-VLD-CND-S-R
                                          (SGTSL079-QT-OCR-TAB)
                                       TO SGTSL079-DT-INC-VLD-CND-R
             END-IF

           END-PERFORM

           .
       340000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       341000-LISTAR-CND-NEGD-OPR       SECTION.
      * Lista Condição Negociada da Operação
      *----------------------------------------------------------------*
           MOVE 0039                   TO MSG1-CD-SEQL-ERRO


           MOVE SGTSL079               TO NM-PGM


           PERFORM 900000-EXECUTAR-MODULO


           .
       341000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       342000-INCLUI-CND-NEGD-OPR       SECTION.
      * Inclusão da Condição Negociada da Operação
      *----------------------------------------------------------------*
           MOVE 0040                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSI079               TO NM-PGM

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSI079-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSI079-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-NOVA
                                       TO SGTSI079-NR-VRS-OPR-ETRD
           MOVE SGTSL079-CD-TIP-CND-NEGD-S (SGTSL079-IC-TAB)
                                       TO SGTSI079-CD-TIP-CND-NEGD

           MOVE SGTSL079-DT-INC-VLD-CND-S(SGTSL079-IC-TAB)
                                       TO SGTSI079-DT-INC-VLD-CND

           MOVE SGTSL079-CD-TIP-CBT-S (SGTSL079-IC-TAB)
                                       TO SGTSI079-CD-TIP-CBT

           MOVE SGTSL079-DT-FIM-VLD-CND-S(SGTSL079-IC-TAB)
                                       TO SGTSI079-DT-FIM-VLD-CND

           MOVE SGTSL079-IN-OBGD-CND-S (SGTSL079-IC-TAB)
                                       TO SGTSI079-IN-OBGD-CND

           MOVE SGTSL079-PC-TAXA-QBRA-FNDO-S (SGTSL079-IC-TAB)
                                       TO SGTSI079-PC-TAXA-QBRA-FNDO

           MOVE SGTSL079-CD-FMA-APRC-S (SGTSL079-IC-TAB)
                                       TO SGTSI079-CD-FMA-APRC

           PERFORM 900000-EXECUTAR-MODULO

           .
       342000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       350000-ANEXO-OPERACAO           SECTION.
      *----------------------------------------------------------------*
           MOVE 0041                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL060-RQSC
                                       SGTSL060-IC-TAB
                                       SGTSL060-QT-OCR-TAB
                                       SGTSL060-IN-CTNC-PSQ

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSL060-CD-CTRA-TIT-VL

           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSL060-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-REF
                                       TO SGTSL060-NR-VRS-OPR-ETRD
           MOVE ZEROS                  TO SGTSL060-CD-ARQ-OPR-ULT
                                          SGTSL060-CD-TIP-ARQ


           PERFORM UNTIL SGTSL060-IN-CTNC-PSQ  EQUAL  'N'
              PERFORM 351000-LISTAR-ANEXO-OPERACAO
              PERFORM 352000-INCLUI-ANEXO
                         VARYING SGTSL060-IC-TAB FROM 1 BY 1
                         UNTIL   SGTSL060-IC-TAB > SGTSL060-QT-OCR-TAB
      * Campos de rechamada da lista Anexo da Operação
             IF SGTSL060-QT-OCR-TAB > ZEROS
                MOVE SGTSL060-CD-ARQ-OPR-ETRD (SGTSL060-QT-OCR-TAB)
                                       TO SGTSL060-CD-ARQ-OPR-ULT
             END-IF
           END-PERFORM

           .
       350000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       351000-LISTAR-ANEXO-OPERACAO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0042                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSL060               TO NM-PGM

           PERFORM 900000-EXECUTAR-MODULO
           .
       351000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       352000-INCLUI-ANEXO             SECTION.
      *----------------------------------------------------------------*
           MOVE 0043                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSI060               TO NM-PGM

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSI060-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSI060-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-NOVA
                                       TO SGTSI060-NR-VRS-OPR-ETRD
           MOVE SGTSL060-CD-ARQ-OPR-ETRD (SGTSL060-IC-TAB)
                                       TO SGTSI060-CD-ARQ-OPR-ETRD
           MOVE KOPRD-CD-OPRD
                                       TO SGTSI060-CD-USU-ULT-ATL
           MOVE 'CURRENT'
                                       TO SGTSI060-TS-ULT-ATL
           MOVE SGTSL060-TX-DCR-ANX (SGTSL060-IC-TAB)
                                       TO SGTSI060-TX-DCR-ANX

           PERFORM 900000-EXECUTAR-MODULO

           .
       352000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       360000-INSOLVENCIA-OPERACAO     SECTION.
      *----------------------------------------------------------------*
           MOVE 0044                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL080-RQSC
                                       SGTSL080-IC-TAB
                                       SGTSL080-QT-OCR-TAB
                                       SGTSL080-IN-CTNC-PSQ

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSL080-E-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSL080-E-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-REF
                                       TO SGTSL080-E-NR-VRS-OPR-ETRD

           PERFORM 361000-LISTAR-INSOLVENCIA
           IF SGTSL080-QT-OCR-TAB > ZEROS
              PERFORM 362000-INCLUI-INSOLVENCIA
                   VARYING SGTSL080-IC-TAB FROM 1 BY 1
                   UNTIL   SGTSL080-IC-TAB > SGTSL080-QT-OCR-TAB
           END-IF
      * Campos de rechamada da lista Insolvencia da Operação
      *       IF SGTSL080-QT-OCR-TAB > ZEROS
      *           MOVE SGTSL080-NR-ISLC-OPR (SGTSL080-QT-OCR-TAB)
      *                                 TO SGTSL080-E-NR-ISLC-OPR
      *       END-IF
           .
       360000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       361000-LISTAR-INSOLVENCIA       SECTION.
      *----------------------------------------------------------------*
           MOVE 0045                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSL080               TO NM-PGM


           PERFORM 900000-EXECUTAR-MODULO

           .
       361000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       362000-INCLUI-INSOLVENCIA       SECTION.
      *----------------------------------------------------------------*
           MOVE 0046                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSI080               TO NM-PGM

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSI080-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSI080-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-NOVA
                                       TO SGTSI080-NR-VRS-OPR-ETRD
           MOVE SGTSL080-IC-TAB        TO SGTSI080-NR-ISLC-OPR
           MOVE 'CURRENT'              TO SGTSI080-TS-ULT-ATL
           MOVE SGTSL080-CD-ARQ-OPR-ETRD (SGTSL080-IC-TAB)
                                       TO SGTSI080-CD-ARQ-OPR-ETRD
           MOVE SGTSL080-IN-ISLC-OPR-ATI (SGTSL080-IC-TAB)
                                       TO SGTSI080-IN-ISLC-OPR-ATI
           MOVE SGTSL080-DT-INC-ISLC-OPR (SGTSL080-IC-TAB)
                                       TO SGTSI080-DT-INC-ISLC-OPR
           MOVE SGTSL080-DT-FIM-ISLC-OPR (SGTSL080-IC-TAB)
                                       TO SGTSI080-DT-FIM-ISLC-OPR
           MOVE SGTSL080-TX-DCR-ISLC-OPR (SGTSL080-IC-TAB)
                                       TO SGTSI080-TX-DCR-ISLC-OPR

           PERFORM 900000-EXECUTAR-MODULO


           .
       362000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       370000-PREVISAO-COMPOSICAO      SECTION.
      *----------------------------------------------------------------*
           MOVE 0047                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL081-RQSC
                                       SGTSL081-IC-TAB
                                       SGTSL081-QT-OCR-TAB
                                       SGTSL081-IN-CTNC-PSQ

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSL081-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSL081-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-REF
                                       TO SGTSL081-NR-VRS-OPR-ETRD


           PERFORM UNTIL SGTSL081-IN-CTNC-PSQ  EQUAL  'N'
              PERFORM 371000-LISTAR-PRV-CMPS-GRT
              PERFORM 372000-INCLUI-PRV-CMPS-GRT
                         VARYING SGTSL081-IC-TAB FROM 1 BY 1
                         UNTIL   SGTSL081-IC-TAB > SGTSL081-QT-OCR-TAB

      * Campos de rechamada da lista Previsão Composição da Garantia
              IF SGTSL081-QT-OCR-TAB > ZEROS
                MOVE SGTSL081-NR-PRV-CMPS-GRT (SGTSL081-QT-OCR-TAB)
                                      TO SGTSL081-NR-ULT-PRV-CMPS-GRT
              END-IF
           END-PERFORM
           .
       370000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       371000-LISTAR-PRV-CMPS-GRT      SECTION.
      *----------------------------------------------------------------*
           MOVE 0048                   TO MSG1-CD-SEQL-ERRO
      *
           MOVE SGTSL081               TO NM-PGM


           PERFORM 900000-EXECUTAR-MODULO

           .
       371000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       372000-INCLUI-PRV-CMPS-GRT      SECTION.
      *----------------------------------------------------------------*
           MOVE 0049                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSI081               TO NM-PGM

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSI081-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSI081-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-NOVA
                                       TO SGTSI081-NR-VRS-OPR-ETRD
           MOVE SGTSL081-CD-NTZ-GRT (SGTSL081-IC-TAB)
                                       TO SGTSI081-CD-NTZ-GRT
           MOVE SGTSL081-CD-FMA-VCLC (SGTSL081-IC-TAB)
                                       TO SGTSI081-CD-FMA-VCLC
           MOVE '  .  .    '           TO SGTSI081-DT-LIM-GRT

           MOVE CORR SGTSL081-DT-LIM-GRT-R (SGTSL081-IC-TAB)
                                       TO SGTSI081-DT-LIM-GRT-R
           MOVE SGTSL081-PC-GRT-CMPS-PRVT (SGTSL081-IC-TAB)
                                       TO SGTSI081-PC-GRT-CMPS-PRVT
           MOVE SGTSL081-VL-GRT-CMPS-PRVT (SGTSL081-IC-TAB)
                                       TO SGTSI081-VL-GRT-CMPS-PRVT
           MOVE 'CURRENT'              TO SGTSI081-TS-ULT-ATL
           MOVE KOPRD-CD-OPRD          TO SGTSI081-CD-USU-ULT-ATL


           PERFORM 900000-EXECUTAR-MODULO

           .
       372000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       380000-CLIENTE-OPERACAO         SECTION.
      *-----------------------------------------------------------------
           MOVE  0050                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTSL006-CHAVE
                                           SGTSL006-IC-TAB
                                           SGTSL006-QT-OCR-TAB
                                           SGTSL006-IN-CNTO-PSQ

           SET   SGTSL006-CLI          TO  TRUE
           MOVE  SGTS0145-CD-CTRA-TIT-VL
                                       TO  SGTSL006-CD-CTRA-TIT-VL
           MOVE  SGTS0145-CD-OPR-ETRD-MCP
                                       TO  SGTSL006-ULT-OPR-ETRD-MCP
           MOVE  SGTS0145-NR-VRS-REF   TO  SGTSL006-ULT-VRS-OPR-ETRD
           MOVE  ZEROS                 TO  SGTSL006-CD-TIP-FUC-CLI-E

           PERFORM UNTIL SGTSL006-IN-CNTO-PSQ   EQUAL  'N'
              PERFORM 381000-LISTAR-CLIENTE
              PERFORM 382000-INCLUI-CLIENTE
                         VARYING SGTSL006-IC-TAB FROM 1 BY 1
                         UNTIL   SGTSL006-IC-TAB > SGTSL006-QT-OCR-TAB
      * Campos de rechamada da lista Cliente da Operação
              IF SGTSL006-QT-OCR-TAB > ZEROS
                 MOVE SGTSL006-CD-TIP-FUC-CLI (SGTSL006-QT-OCR-TAB )
                                       TO SGTSL006-ULT-TIP-FUC-CLI
                 MOVE SGTSL006-CD-CLI (SGTSL006-QT-OCR-TAB )
                                       TO SGTSL006-ULT-CD-CLI
              END-IF
           END-PERFORM
           .
       380000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       381000-LISTAR-CLIENTE           SECTION.
      *-----------------------------------------------------------------
           MOVE  0051                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTSL006              TO  NM-PGM

           PERFORM  940000-CHAMAR-SGTSL006
           .
       381000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       382000-INCLUI-CLIENTE           SECTION.
      *-----------------------------------------------------------------
           MOVE  0052                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTST006              TO  NM-PGM

           SET   SGTST006-INCLUIR      TO  TRUE

           MOVE  SGTS0145-CD-CTRA-TIT-VL
                                       TO  SGTST006-CD-CTRA-TIT-VL
           MOVE  SGTS0145-CD-OPR-ETRD-MCP
                                       TO  SGTST006-CD-OPR-ETRD-MCP
           MOVE  SGTS0145-NR-VRS-NOVA  TO  SGTST006-NR-VRS-OPR-ETRD
           MOVE  SGTSL006-CD-CLI (SGTSL006-IC-TAB)
                                       TO  SGTST006-CD-CLI
           MOVE  SGTSL006-CD-TIP-FUC-CLI (SGTSL006-IC-TAB)
                                       TO  SGTST006-CD-TIP-FUC-CLI
           MOVE  KOPRD-CD-OPRD         TO  SGTST006-CD-USU-RSP-ATL
           MOVE  'CURRENT'             TO  SGTST006-TS-ULT-ATL

           PERFORM  950000-CHAMAR-SGTST006

           .
       382000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       390000-NOTA-CLASSIFICACAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0053                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL074-RQSC
                                       SGTSL074-QT-OCR-TAB
                                       SGTSL074-IC-TAB
                                       SGTSL074-IN-CTNC-PSQ

      *    Movimentar os campos de entrada
           MOVE 1                      TO SGTSL074-CD-TIP-SRVC
           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSL074-E-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSL074-E-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-REF
                                       TO SGTSL074-E-NR-VRS-OPR-ETRD
           MOVE ZEROS
                                       TO SGTSL074-E-CD-AG-CLSC-RSCO
           MOVE ZEROS
                                       TO SGTSL074-E-NR-ETRD-AG-CLSC
           MOVE SPACES
                                       TO SGTSL074-E-SG-NOTA-AG-CLSC
           MOVE SPACES
                                       TO SGTSL074-E-DT-INC-ATBC-NOTA
           MOVE SPACES
                                       TO SGTSL074-E-DT-FIM-ATBC-NOTA


           PERFORM UNTIL SGTSL074-IN-CTNC-PSQ  EQUAL  'N'
              PERFORM 391000-LISTAR-NOTA-CLSC-OPR
              PERFORM 392000-INCLUI-NOTA-CLSC-OPR
                      VARYING SGTSL074-IC-TAB FROM 1 BY 1
                      UNTIL SGTSL074-IC-TAB > SGTSL074-QT-OCR-TAB
      * Campos de rechamada da lista Nota de Classificação da Operação
              IF  SGTSL074-QT-OCR-TAB     > ZEROS
                 MOVE SGTSL074-NR-SEQL-NOTA-OPR (SGTSL074-QT-OCR-TAB)
                                       TO SGTSL074-E-NR-SEQL-NOTA-OPR

              END-IF
           END-PERFORM
           .
       390000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       391000-LISTAR-NOTA-CLSC-OPR     SECTION.
      * Lista Nota Classificação da Operação
      *----------------------------------------------------------------*
           MOVE 0054                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSL074               TO NM-PGM


           PERFORM 900000-EXECUTAR-MODULO

           .
       391000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       392000-INCLUI-NOTA-CLSC-OPR     SECTION.
      * Inclui Nota de Classificação da Operação
      *----------------------------------------------------------------*
           MOVE 0055                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSI074               TO NM-PGM

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSI074-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSI074-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-NOVA
                                       TO SGTSI074-NR-VRS-OPR-ETRD
           MOVE SGTSL074-CD-AG-CLSC-RSCO (SGTSL074-IC-TAB)
                                       TO SGTSI074-CD-AG-CLSC-RSCO
           MOVE SGTSL074-NR-ETRD-AG-CLSC (SGTSL074-IC-TAB)
                                       TO SGTSI074-NR-ETRD-AG-CLSC
           MOVE SGTSL074-NR-SEQL-NOTA-AG (SGTSL074-IC-TAB)
                                       TO SGTSI074-NR-SEQL-NOTA-AG
           MOVE SGTSL074-CD-ARQ-OPR-ETRD (SGTSL074-IC-TAB)
                                       TO SGTSI074-CD-ARQ-OPR-ETRD
           MOVE CORR SGTSL074-DT-INC-ATBC-NOTA-R (SGTSL074-IC-TAB)
                                       TO SGTSI074-DT-INC-ATBC-NOTA-R

           IF SGTSL074-DT-FIM-ATBC-NOTA(SGTSL074-IC-TAB) EQUAL SPACES
              MOVE SPACES              TO  SGTSI074-DT-FIM-ATBC-NOTA
           ELSE
              MOVE '  .  .    '        TO SGTSI074-DT-FIM-ATBC-NOTA
              MOVE CORR SGTSL074-DT-FIM-ATBC-NOTA-R (SGTSL074-IC-TAB)
                                       TO SGTSI074-DT-FIM-ATBC-NOTA-R
           END-IF
           MOVE 'CURRENT'              TO SGTSI074-TS-ULT-ATL
           MOVE KOPRD-CD-OPRD          TO SGTSI074-CD-USU-ULT-ATL
           MOVE SGTSL074-IN-NOTA-OPR-CNCD (SGTSL074-IC-TAB)
                                       TO SGTSI074-IN-NOTA-OPR-CNCD

           PERFORM 900000-EXECUTAR-MODULO

           .
       392000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       400000-ORDEM-NEGOCIACAO         SECTION.
      *-----------------------------------------------------------------
           MOVE  0056                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTSL075-RQSC
                                           SGTSL075-QT-OCR-TAB
                                           SGTSL075-IC-TAB
                                           SGTSL075-IN-CTNC-PSQ

           MOVE  0001                  TO  SGTSL075-CD-ACAO
           MOVE  SGTS0145-CD-CTRA-TIT-VL
                                       TO  SGTSL075-CD-CTRA-TIT-VL
           MOVE  SGTS0145-CD-OPR-ETRD-MCP
                                       TO  SGTSL075-CD-OPR-ETRD-MCP
           MOVE  SGTS0145-NR-VRS-REF
                                       TO  SGTSL075-NR-VRS-OPR-ETRD


           PERFORM  UNTIL SGTSL075-IN-CTNC-PSQ
                                           EQUAL  'N'
                    PERFORM 401000-LISTAR-ORDEM-NEGOCIACAO
                    PERFORM 402000-INCLUI-ORDEM-NEGOCIACAO
                            VARYING        SGTSL075-IC-TAB
                            FROM           0001 BY 0001
                            UNTIL SGTSL075-IC-TAB
                                           GREATER  SGTSL075-QT-OCR-TAB
      * Campos de rechamada da lista Ordem Negociação da Operação
                    IF  SGTSL075-QT-OCR-TAB     > ZEROS
                    MOVE SGTSL075-NR-SEQL-ORD-NEGO (SGTSL075-QT-OCR-TAB)
                                       TO  SGTSL075-NR-ULT-SEQL-ORD-NEGO
                    END-IF
           END-PERFORM

           .
       400000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       401000-LISTAR-ORDEM-NEGOCIACAO  SECTION.
      *-----------------------------------------------------------------
           MOVE  0057                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimenta o Nome do Módulo Acionado
           MOVE  SGTSL075              TO  NM-PGM

           PERFORM  900000-EXECUTAR-MODULO

           .
       401000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       402000-INCLUI-ORDEM-NEGOCIACAO  SECTION.
      *-----------------------------------------------------------------
           MOVE  0058                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimenta o Nome do Módulo Acionado
           MOVE  SGTSIV75              TO  NM-PGM

      *--> Movimenta Chave da Operação Estruturada da Nova Versão
           MOVE  SGTS0145-CD-CTRA-TIT-VL
                                       TO  SGTSIV75-CD-CTRA-TIT-VL
           MOVE  SGTS0145-CD-OPR-ETRD-MCP
                                       TO  SGTSIV75-CD-OPR-ETRD-MCP
           MOVE  SGTS0145-NR-VRS-NOVA  TO  SGTSIV75-NR-VRS-OPR-ETRD

      *--> Movimenta Numero Sequencial, Matrícula e Timestamp do Criador
      *    da Ordem de Negociação da versão anterior
           MOVE  SGTSL075-NR-SEQL-ORD-NEGO(SGTSL075-IC-TAB)
                                       TO  SGTSIV75-NR-SEQL-ORD-NEGO
           MOVE  SGTSL075-CD-USU-ULT-ATL(SGTSL075-IC-TAB)
                                       TO  SGTSIV75-CD-USU-ULT-ATL
           MOVE  SGTSL075-TS-ULT-ATL(SGTSL075-IC-TAB)
                                       TO  SGTSIV75-TS-ULT-ATL

      *--> Movimenta Demais Campos da Ordem de Negociação
           MOVE  SGTSL075-CD-MOD-ORD-NEGO(SGTSL075-IC-TAB)
                                       TO  SGTSIV75-CD-MOD-ORD-NEGO
           MOVE  SGTSL075-CD-ARQ-OPR-ETRD(SGTSL075-IC-TAB)
                                       TO  SGTSIV75-CD-ARQ-OPR-ETRD
           MOVE  SGTSL075-CD-MTV-NEGO-VND(SGTSL075-IC-TAB)
                                       TO  SGTSIV75-CD-MTV-NEGO-VND
           MOVE  SGTSL075-CD-TIP-APL-TAXA(SGTSL075-IC-TAB)
                                       TO  SGTSIV75-CD-TIP-APL-TAXA
           MOVE  SGTSL075-CD-IEC-TAXA-VND(SGTSL075-IC-TAB)
                                       TO  SGTSIV75-CD-IEC-TAXA-VND
           MOVE  SGTSL075-DT-EMS-ORD-NEGO(SGTSL075-IC-TAB)
                                       TO  SGTSIV75-DT-EMS-ORD-NEGO
           MOVE  SGTSL075-DT-ESTV-TAXA-VND(SGTSL075-IC-TAB)
                                       TO  SGTSIV75-DT-ESTV-TAXA-VND
           MOVE  SGTSL075-VL-PU-PAPL-VND(SGTSL075-IC-TAB)
                                       TO  SGTSIV75-VL-PU-PAPL-VND
           MOVE  SGTSL075-VL-TAXA-NEGO-VND(SGTSL075-IC-TAB)
                                       TO  SGTSIV75-VL-TAXA-NEGO-VND
           MOVE  SGTSL075-TX-JST-VND-OPR(SGTSL075-IC-TAB)
                                       TO  SGTSIV75-TX-JST-VND-OPR
           MOVE  SGTSL075-TX-OBS-ORD-NEGO(SGTSL075-IC-TAB)
                                       TO  SGTSIV75-TX-OBS-ORD-NEGO
           MOVE  SGTSL075-DT-VND(SGTSL075-IC-TAB)
                                       TO  SGTSIV75-DT-VND
           MOVE  SGTSL075-QT-TTL-PAPL(SGTSL075-IC-TAB)
                                       TO  SGTSIV75-QT-TTL-PAPL
           MOVE  SGTSL075-QT-PAPL-CMPD(SGTSL075-IC-TAB)
                                       TO  SGTSIV75-QT-PAPL-CMPD
           MOVE  SGTSL075-QT-PAPL-LVRE(SGTSL075-IC-TAB)
                                       TO  SGTSIV75-QT-PAPL-LVRE

           PERFORM  900000-EXECUTAR-MODULO

           .
       402000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       410000-ESTADO-ORDEM-NEGOCIACAO  SECTION.
      *-----------------------------------------------------------------
           MOVE  0059                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTSL078-RQSC
                                           SGTSL078-QT-OCR-TAB
                                           SGTSL078-IC-TAB
                                           SGTSL078-IN-CNTO-PSQ

           MOVE  SGTS0145-CD-CTRA-TIT-VL
                                       TO  SGTSL078-CD-CTRA-TIT-VL
           MOVE  SGTS0145-CD-OPR-ETRD-MCP
                                       TO  SGTSL078-CD-OPR-ETRD-MCP
           MOVE  SGTS0145-NR-VRS-REF   TO  SGTSL078-NR-VRS-OPR-ETRD
           MOVE  ZEROS                 TO  SGTSL078-NR-SEQL-ORD-ULT
           MOVE  '0001-01-01-00.00.00.000000'
                                       TO  SGTSL078-TS-EST-ULT
           SET   SGTSL078-LISTA-TODOS  TO  TRUE

           PERFORM  UNTIL SGTSL078-IN-CNTO-PSQ
                                           EQUAL  'N'
                    PERFORM  411000-LISTAR-EST-ORD-NEG
                    PERFORM  412000-INCLUIR-EST-ORD-NEG
                             VARYING       SGTSL078-IC-TAB
                             FROM          0001 BY 0001
                             UNTIL SGTSL078-IC-TAB
                                           GREATER SGTSL078-QT-OCR-TAB
      * Campos de rechamada da lista Estado Ordem Negociação da Operação
                    IF  SGTSL078-QT-OCR-TAB  > ZEROS
                    MOVE  SGTSL078-NR-SEQL-ORD-NEGO(SGTSL078-QT-OCR-TAB)
                                       TO  SGTSL078-NR-SEQL-ORD-ULT
                    MOVE  SGTSL078-TS-EST-ORD-NEGO(SGTSL078-QT-OCR-TAB)
                                       TO  SGTSL078-TS-EST-ULT
                    END-IF
           END-PERFORM

           .
       410000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       411000-LISTAR-EST-ORD-NEG       SECTION.
      *-----------------------------------------------------------------
           MOVE  0060                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTSL078              TO  NM-PGM

           PERFORM  900000-EXECUTAR-MODULO
           .
       411000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       412000-INCLUIR-EST-ORD-NEG      SECTION.
      *-----------------------------------------------------------------
           MOVE  0061                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimenta o Nome do Módulo Acionado
           MOVE  SGTSI078              TO  NM-PGM

      *--> Movimenta Chave da Operação Estruturada da Nova Versão
           MOVE  SGTS0145-CD-CTRA-TIT-VL
                                       TO  SGTSI078-CD-CTRA-TIT-VL
           MOVE  SGTS0145-CD-OPR-ETRD-MCP
                                       TO  SGTSI078-CD-OPR-ETRD-MCP
           MOVE  SGTS0145-NR-VRS-NOVA
                                       TO  SGTSI078-NR-VRS-OPR-ETRD

      *--> Movimenta a Matrícula e Timestamp do Criador
      *    do Estado da Ordem de Negociação
           MOVE  SGTSL078-CD-USU-ULT-ATL(SGTSL078-IC-TAB)
                                       TO  SGTSI078-CD-USU-ULT-ATL
           MOVE  SGTSL078-TS-EST-ORD-NEGO (SGTSL078-IC-TAB)
                                       TO  SGTSI078-TS-EST-ORD-NEGO

      *--> Movimenta Demais Campos da Ordem de Negociação
           MOVE  SGTSL078-NR-SEQL-ORD-NEGO(SGTSL078-IC-TAB)
                                       TO  SGTSI078-NR-SEQL-ORD-NEGO
           MOVE  SGTSL078-CD-TIP-EST-NEGO(SGTSL078-IC-TAB)
                                       TO  SGTSI078-CD-TIP-EST-NEGO
           MOVE  SGTSL078-TX-JST-EST-ORD(SGTSL078-IC-TAB)
                                       TO  SGTSI078-TX-JST-EST-ORD

           PERFORM 900000-EXECUTAR-MODULO

           .
       412000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       420000-CLI-COMPRA-NEGOCIACAO    SECTION.
      *-----------------------------------------------------------------
           MOVE  0062                  TO  MSG1-CD-SEQL-ERRO


           INITIALIZE                      SGTSL076-RQSC
                                           SGTSL076-QT-OCR-TAB
                                           SGTSL076-IC-TAB
                                           SGTSL076-IN-CNTO-PSQ

           MOVE  SGTS0145-CD-CTRA-TIT-VL
                                       TO  SGTSL076-CD-CTRA-TIT-VL
           MOVE  SGTS0145-CD-OPR-ETRD-MCP
                                       TO  SGTSL076-CD-OPR-ETRD-MCP
           MOVE  SGTS0145-NR-VRS-REF   TO  SGTSL076-NR-VRS-OPR-ETRD


           PERFORM UNTIL SGTSL076-IN-CNTO-PSQ  EQUAL  'N'
              PERFORM 421000-LISTAR-CLI-CPRD-NEGO
              PERFORM 422000-INCLUIR-CLI-CPRD-NEGO
                      VARYING SGTSL076-IC-TAB FROM 1 BY 1
                      UNTIL SGTSL076-IC-TAB > SGTSL076-QT-OCR-TAB
      * Campos de rechamada da lista Clientes Compradores na Negociação
      * de Venda da Operação
              IF  SGTSL076-QT-OCR-TAB  > ZEROS
                  MOVE SGTSL076-NR-SEQL-ORD-NEGO (SGTSL076-QT-OCR-TAB)
                                      TO SGTSL076-NR-SEQL-ORD-NEGO-ULT
                  MOVE SGTSL076-CD-CLI-CPRD (SGTSL076-QT-OCR-TAB)
                                      TO SGTSL076-CD-CLI-CPRD-ULT
              END-IF
           END-PERFORM

           .
       420000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       421000-LISTAR-CLI-CPRD-NEGO     SECTION.
      *-----------------------------------------------------------------
           MOVE  0063                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTSL076              TO  NM-PGM


           PERFORM  900000-EXECUTAR-MODULO
           .
       421000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       422000-INCLUIR-CLI-CPRD-NEGO    SECTION.
      *-----------------------------------------------------------------
           MOVE  0064                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTSI076              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0145-CD-CTRA-TIT-VL
                                       TO  SGTSI076-CD-CTRA-TIT-VL
           MOVE  SGTS0145-CD-OPR-ETRD-MCP
                                       TO  SGTSI076-CD-OPR-ETRD-MCP
           MOVE  SGTS0145-NR-VRS-NOVA
                                       TO  SGTSI076-NR-VRS-OPR-ETRD
           MOVE  SGTSL076-NR-SEQL-ORD-NEGO(SGTSL076-IC-TAB)
                                       TO  SGTSI076-NR-SEQL-ORD-NEGO
           MOVE  SGTSL076-CD-CLI-CPRD(SGTSL076-IC-TAB)
                                       TO  SGTSI076-CD-CLI-CPRD
           MOVE  SGTSL076-QT-PAPL-NEGO-VND(SGTSL076-IC-TAB)
                                       TO  SGTSI076-QT-PAPL-NEGO-VND
           MOVE  SGTSL076-CD-TIP-CLI-CPRD(SGTSL076-IC-TAB)
                                       TO  SGTSI076-CD-TIP-CLI-CPRD

           PERFORM  900000-EXECUTAR-MODULO

           .
       42200-SAI.
           EXIT.

      *-----------------------------------------------------------------
       430000-AUTORIZACAO-NEGOCIACAO   SECTION.
      *-----------------------------------------------------------------
           MOVE  0065                  TO  MSG1-CD-SEQL-ERRO


           INITIALIZE                      SGTSL077-RQSC
                                           SGTSL077-QT-OCR-TAB
                                           SGTSL077-IC-TAB
                                           SGTSL077-IN-CNTO-PSQ

           MOVE  SGTS0145-CD-CTRA-TIT-VL
                                       TO  SGTSL077-CD-CTRA-TIT-VL
           MOVE  SGTS0145-CD-OPR-ETRD-MCP
                                       TO  SGTSL077-CD-OPR-ETRD-MCP
           MOVE  SGTS0145-NR-VRS-REF
                                       TO  SGTSL077-NR-VRS-OPR-ETRD
           MOVE  '0001-01-01-00.00.00.000000'
                                       TO  SGTSL077-TS-AUTZ-ULT

           PERFORM UNTIL SGTSL077-IN-CNTO-PSQ  EQUAL  'N'
              PERFORM 431000-LISTAR-AUTZ-NEGO-OPR
              PERFORM 432000-INCLUIR-AUTZ-NEGO-OPR
                      VARYING SGTSL077-IC-TAB FROM 1 BY 1
                      UNTIL SGTSL077-IC-TAB > SGTSL077-QT-OCR-TAB
      * Campos de rechamada da lista Autorização na Negociação da Operaç
      * ão
              IF  SGTSL077-QT-OCR-TAB  > ZEROS
                  MOVE SGTSL077-NR-SEQL-ORD-NEGO (SGTSL077-QT-OCR-TAB)
                                      TO SGTSL077-NR-SEQL-ULT
                  MOVE SGTSL077-TS-AUTZ-NEGO (SGTSL077-QT-OCR-TAB)
                                      TO SGTSL077-TS-AUTZ-ULT
              END-IF
           END-PERFORM

           .
       430000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       431000-LISTAR-AUTZ-NEGO-OPR  SECTION.
      *-----------------------------------------------------------------
           MOVE 0066                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSL077               TO NM-PGM


           PERFORM  900000-EXECUTAR-MODULO
           .
       431000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       432000-INCLUIR-AUTZ-NEGO-OPR    SECTION.
      *-----------------------------------------------------------------
           MOVE  0067                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTSI077              TO  NM-PGM

           MOVE  SGTS0145-CD-CTRA-TIT-VL
                                       TO  SGTSI077-CD-CTRA-TIT-VL
           MOVE  SGTS0145-CD-OPR-ETRD-MCP
                                       TO  SGTSI077-CD-OPR-ETRD-MCP
           MOVE  SGTS0145-NR-VRS-NOVA
                                       TO  SGTSI077-NR-VRS-OPR-ETRD
           MOVE  SGTSL077-NR-SEQL-ORD-NEGO(SGTSL077-IC-TAB)
                                       TO  SGTSI077-NR-SEQL-ORD-NEGO
           MOVE  SGTSL077-TS-AUTZ-NEGO(SGTSL077-IC-TAB)
                                       TO  SGTSI077-TS-AUTZ-NEGO
           MOVE  SGTSL077-CD-USU-RSP-AUTZ(SGTSL077-IC-TAB)
                                       TO  SGTSI077-CD-USU-RSP-AUTZ
           MOVE  SGTSL077-IN-FLZ-EXCO-OPR(SGTSL077-IC-TAB)
                                       TO  SGTSI077-IN-FLZ-EXCO-OPR
           MOVE  SGTSL077-TX-JST-FLZ-EXCO(SGTSL077-IC-TAB)
                                       TO  SGTSI077-TX-JST-FLZ-EXCO

           PERFORM  900000-EXECUTAR-MODULO

           .
       432000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       440000-EXCECAO-PERFIL           SECTION.
      * Exceção da Operação do Pefil Escolhido
      *----------------------------------------------------------------*
           MOVE 0068                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL083-RQSC
                                       SGTSL083-QT-OCR-TAB
                                       SGTSL083-IC-TAB
                                       SGTSL083-IN-CTNC-PSQ

           SET SGTSL083-LISTA-TODOS    TO TRUE

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSL083-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSL083-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-REF
                                       TO SGTSL083-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL083-IN-CTNC-PSQ  EQUAL  'N'
              PERFORM 441000-LISTAR-EXCECAO
              PERFORM 442000-INCLUI-EXCECAO
                      VARYING SGTSL083-IC-TAB FROM 1 BY 1
                      UNTIL SGTSL083-IC-TAB > SGTSL083-QT-OCR-TAB
      * Campos de rechamada da lista Exceção da Operação do Perfil
              IF  SGTSL083-QT-OCR-TAB     > ZEROS
                 MOVE SGTSL083-NR-SEQL-EXCO-PRFL (SGTSL083-QT-OCR-TAB)
                                       TO SGTSL083-E-NR-SEQL-EXCO-PRFL
              END-IF
           END-PERFORM
           .
       440000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       441000-LISTAR-EXCECAO           SECTION.
      *----------------------------------------------------------------*
           MOVE 0069                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSL083               TO NM-PGM

           PERFORM 900000-EXECUTAR-MODULO

           .
       441000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       442000-INCLUI-EXCECAO           SECTION.
      *----------------------------------------------------------------*
           MOVE 0070                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSI083               TO NM-PGM

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSI083-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSI083-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-NOVA
                                       TO SGTSI083-NR-VRS-OPR-ETRD
           MOVE SGTSL083-NR-SEQL-EXCO-PRFL (SGTSL083-IC-TAB)
                                       TO SGTSI083-NR-SEQL-EXCO-PRFL
           MOVE KOPRD-CD-OPRD
                                       TO SGTSI083-CD-USU-ULT-ATL
           MOVE 'CURRENT'
                                       TO SGTSI083-TS-ULT-ATL
           MOVE SGTSL083-IN-EXCO-ATV (SGTSL083-IC-TAB)
                                       TO SGTSI083-IN-EXCO-ATV
           MOVE SGTSL083-TX-OBS-EXCO-PRFL(SGTSL083-IC-TAB)
                                       TO SGTSI083-TX-OBS-EXCO-PRFL

           PERFORM 900000-EXECUTAR-MODULO

           .
       442000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       450000-CONTATO-EXTERNO          SECTION.
      *----------------------------------------------------------------*
           MOVE 0071                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL035-RQSC
                                       SGTSL035-QT-OCR-TAB
                                       SGTSL035-IC-TAB
                                       SGTSL035-IN-CNTO-PSQ
           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSL035-CD-CTRA-TIT-VL

           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSL035-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-REF
                                       TO SGTSL035-NR-VRS-OPR-ETRD


           PERFORM UNTIL SGTSL035-IN-CNTO-PSQ  EQUAL  'N'
              PERFORM 451000-LISTAR-CONTATOS-EXTERNO
              PERFORM 452000-INCLUIR-CONTATO
                      VARYING SGTSL035-IC-TAB FROM 1 BY 1
                      UNTIL SGTSL035-IC-TAB > SGTSL035-QT-OCR-TAB
      * Campos de rechamada da lista Contato Externo
              IF  SGTSL035-QT-OCR-TAB  > ZEROS
                 MOVE SGTSL035-S-CD-PSS-CTT-EXNO (SGTSL035-QT-OCR-TAB)
                                       TO SGTSL035-CD-PSS-CTT-EXNO
              END-IF
           END-PERFORM

           .
       450000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       451000-LISTAR-CONTATOS-EXTERNO  SECTION.
      *----------------------------------------------------------------*
           MOVE 0072                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSL035               TO NM-PGM


           PERFORM 900000-EXECUTAR-MODULO
           .
       451000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       452000-INCLUIR-CONTATO          SECTION.
      *----------------------------------------------------------------*
           MOVE 0073                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSI035               TO NM-PGM

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSI035-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSI035-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-NOVA
                                       TO SGTSI035-NR-VRS-OPR-ETRD
           MOVE SGTSL035-S-CD-PSS-CTT-EXNO (SGTSL035-IC-TAB)
                                       TO SGTSI035-CD-PSS-CTT-EXNO
           MOVE 'CURRENT'
                                       TO SGTSI035-TS-ATL-CTT-EXNO
           MOVE KOPRD-CD-OPRD
                                       TO SGTSI035-CD-USU-RSP-ATL

           PERFORM 900000-EXECUTAR-MODULO

           .
       452000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       460000-NOTA-TECNICA             SECTION.
      * Nota Técnica Vinculada a Operação
      *----------------------------------------------------------------*
           MOVE 0074                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL067-RQSC
                                       SGTSL067-QT-OCR-TAB
                                       SGTSL067-IC-TAB
                                       SGTSL067-IN-CTNC-PSQ

           MOVE 2                      TO SGTSL067-CD-TIP-SRVC

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                      TO SGTSL067-E-CD-CTRA-TIT-VL

           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSL067-E-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-REF
                                       TO SGTSL067-E-NR-VRS-OPR-ETRD


           PERFORM UNTIL SGTSL067-IN-CTNC-PSQ  EQUAL  'N'
              PERFORM 461000-LISTAR-NOTA-TECNICA
              PERFORM 462000-INCLUI-NOTA-TECNICA
                      VARYING SGTSL067-IC-TAB FROM 1 BY 1
                      UNTIL SGTSL067-IC-TAB > SGTSL067-QT-OCR-TAB
      * Campos de rechamada da lista Nota Técnica da Operação
              IF  SGTSL067-QT-OCR-TAB     > ZEROS
                 MOVE SGTSL067-NR-NOTA-TCN-OPR (SGTSL067-QT-OCR-TAB)
                                       TO SGTSL067-E-NR-NOTA-TCN-OPR
              END-IF
           END-PERFORM

           .
       460000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       461000-LISTAR-NOTA-TECNICA      SECTION.
      *----------------------------------------------------------------*
           MOVE 0075                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSL067               TO NM-PGM


           PERFORM 900000-EXECUTAR-MODULO
           .
       461000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       462000-INCLUI-NOTA-TECNICA      SECTION.
      *----------------------------------------------------------------*
           MOVE 0076                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSI067               TO NM-PGM

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSI067-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSI067-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-NOVA
                                       TO SGTSI067-NR-VRS-OPR-ETRD
           MOVE SGTSL067-NR-NOTA-TCN-OPR (SGTSL067-IC-TAB)
                                       TO SGTSI067-NR-NOTA-TCN-OPR
           MOVE 'CURRENT'
                                       TO SGTSI067-TS-INCL-NOTA-TCN
           MOVE KOPRD-CD-OPRD
                                       TO SGTSI067-CD-USU-ULT-ATL


           PERFORM 900000-EXECUTAR-MODULO

           .
       462000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       470000-ITEM-EXCECAO-OPERACAO             SECTION.
      * Item da Exceção da Operação
      *----------------------------------------------------------------*
           MOVE 0077                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL095-RQSC
                                       SGTSL095-QT-OCR-TAB
                                       SGTSL095-IC-TAB
                                       SGTSL095-IN-CTNC-PSQ

           MOVE 2                      TO SGTSL095-CD-TIP-SRVC

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                      TO SGTSL095-CD-CTRA-TIT-VL

           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSL095-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-REF
                                       TO SGTSL095-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL095-IN-CTNC-PSQ  EQUAL  'N'
              PERFORM 471000-LISTAR-ITEM-EXCECAO
              PERFORM 472000-INCLUI-ITEM-EXCECAO
                      VARYING SGTSL095-IC-TAB FROM 1 BY 1
                      UNTIL SGTSL095-IC-TAB > SGTSL095-QT-OCR-TAB
      * Campos de rechamada da lista Item de Exceção da Operação
              IF  SGTSL095-QT-OCR-TAB     > ZEROS
                 MOVE SGTSL095-NR-SEQL-EXCO-PRFL (SGTSL095-QT-OCR-TAB)
                                       TO SGTSL095-E-NR-SEQL-EXCO-PRFL
                 MOVE SGTSL095-NR-SEQL-ITEM-EXCO (SGTSL095-QT-OCR-TAB)
                                       TO SGTSL095-E-NR-SEQL-ITEM-EXCO
              END-IF
           END-PERFORM

           .
       470000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       471000-LISTAR-ITEM-EXCECAO      SECTION.
      *----------------------------------------------------------------*
           MOVE 0078                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSL095               TO NM-PGM


           PERFORM 900000-EXECUTAR-MODULO
           .
       471000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       472000-INCLUI-ITEM-EXCECAO     SECTION.
      *----------------------------------------------------------------*
           MOVE 0079                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSI095               TO NM-PGM

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSI095-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSI095-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-NOVA
                                       TO SGTSI095-NR-VRS-OPR-ETRD
           MOVE SGTSL095-NR-SEQL-EXCO-PRFL (SGTSL095-IC-TAB)
                                       TO SGTSI095-NR-SEQL-EXCO-PRFL
           MOVE SGTSL095-NR-SEQL-ITEM-EXCO (SGTSL095-IC-TAB)
                                       TO SGTSI095-NR-SEQL-ITEM-EXCO
           MOVE SGTSL095-CD-RGR-VLDC (SGTSL095-IC-TAB)
                                       TO SGTSI095-CD-RGR-VLDC
           MOVE SGTSL095-NR-VRS-RGR-VLDC (SGTSL095-IC-TAB)
                                       TO SGTSI095-NR-VRS-RGR-VLDC
           MOVE SGTSL095-NR-SEQL-ITC (SGTSL095-IC-TAB)
                                       TO SGTSI095-NR-SEQL-ITC
           MOVE SGTSL095-IN-ITEM-CRIO-EXCO (SGTSL095-IC-TAB)
                                       TO SGTSI095-IN-ITEM-CRIO-EXCO
           MOVE SGTSL095-VL-RLZD-ITEM-EXCO (SGTSL095-IC-TAB)
                                       TO SGTSI095-VL-RLZD-ITEM-EXCO
           MOVE SGTSL095-TX-RLZD-ITEM-EXCO (SGTSL095-IC-TAB)
                                       TO SGTSI095-TX-RLZD-ITEM-EXCO
           MOVE SGTSL095-NR-RLZD-ITEM-EXCO (SGTSL095-IC-TAB)
                                       TO SGTSI095-NR-RLZD-ITEM-EXCO
           MOVE SGTSL095-TX-OBS-ITEM-EXCO (SGTSL095-IC-TAB)
                                       TO SGTSI095-TX-OBS-ITEM-EXCO
           PERFORM 900000-EXECUTAR-MODULO

           .
       472000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       480000-CLIENTE-GARANTIDOR             SECTION.
      * Clientes que fornecerão a garantia para a operação
      *----------------------------------------------------------------*
           MOVE 0080                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL082-RQSC
                                       SGTSL082-QT-OCR-TAB
                                       SGTSL082-IC-TAB
                                       SGTSL082-IN-CTNC-PSQ

           MOVE 2                      TO SGTSL082-CD-TIP-SRVC

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                      TO SGTSL082-CD-CTRA-TIT-VL

           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSL082-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-REF
                                       TO SGTSL082-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL082-IN-CTNC-PSQ  EQUAL  'N'
              PERFORM 481000-LISTAR-CLI-GARANTIDOR
              PERFORM 482000-INCLUI-CLI-GARANTIDOR
                      VARYING SGTSL082-IC-TAB FROM 1 BY 1
                      UNTIL SGTSL082-IC-TAB > SGTSL082-QT-OCR-TAB
      * Campos de rechamada da lista Cliente Garantidor
              IF  SGTSL082-QT-OCR-TAB     > ZEROS
                 MOVE SGTSL082-NR-PRV-CMPS-GRT-S (SGTSL082-QT-OCR-TAB)
                                       TO SGTSL082-NR-PRV-CMPS-GRT
                 MOVE SGTSL082-CD-CLI (SGTSL082-QT-OCR-TAB)
                                       TO SGTSL082-CD-ULT-CLI
              END-IF
           END-PERFORM

           .
       480000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       481000-LISTAR-CLI-GARANTIDOR      SECTION.
      *----------------------------------------------------------------*
           MOVE 0081                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSL082               TO NM-PGM


           PERFORM 900000-EXECUTAR-MODULO
           .
       481000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       482000-INCLUI-CLI-GARANTIDOR     SECTION.
      *----------------------------------------------------------------*
           MOVE 0082                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSI082               TO NM-PGM

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSI082-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSI082-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-NOVA
                                       TO SGTSI082-NR-VRS-OPR-ETRD

           MOVE SGTSL082-NR-PRV-CMPS-GRT-S (SGTSL082-IC-TAB)

                                       TO SGTSI082-NR-PRV-CMPS-GRT

           MOVE SGTSL082-CD-CLI (SGTSL082-IC-TAB)

                                       TO SGTSI082-CD-CLI
           PERFORM 900000-EXECUTAR-MODULO

           .
       482000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       490000-JUSTIFICATIVAS           SECTION.
      * Justificativas para validações da finalização de cadastramento.
      *----------------------------------------------------------------*
           MOVE 0083                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL117-RQSC
                                       SGTSL117-QT-OCR-TAB
                                       SGTSL117-IC-TAB
                                       SGTSL117-IN-CTNC-PSQ

           MOVE 1                     TO SGTSL117-CD-TIP-SRVC

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                      TO SGTSL117-CD-CTRA-TIT-VL

           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSL117-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-REF
                                       TO SGTSL117-NR-VRS-OPR-ETRD
           MOVE ZEROS                  TO SGTSL117-ULT-CD-TIP-VLDC-OPR

           PERFORM UNTIL SGTSL117-IN-CTNC-PSQ  EQUAL  'N'
              PERFORM 491000-LISTAR-JUSTIFICATIVAS
              PERFORM 492000-INCLUI-JUSTIFICATIVAS
                      VARYING SGTSL117-IC-TAB FROM 1 BY 1
                      UNTIL SGTSL117-IC-TAB > SGTSL117-QT-OCR-TAB

      * Campos de rechamada da lista Justificativas
              IF  SGTSL117-QT-OCR-TAB     > ZEROS
                  MOVE SGTSL117-CD-TIP-VLDC-OPR(SGTSL117-QT-OCR-TAB)
                                       TO SGTSL117-ULT-CD-TIP-VLDC-OPR
              END-IF
           END-PERFORM

           .
       490000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       491000-LISTAR-JUSTIFICATIVAS    SECTION.
      *----------------------------------------------------------------*
           MOVE 0084                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSL117              TO NM-PGM


           PERFORM 900000-EXECUTAR-MODULO
           .
       491000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       492000-INCLUI-JUSTIFICATIVAS    SECTION.
      *----------------------------------------------------------------*
           MOVE 0085                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSI117               TO NM-PGM

           MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO SGTSI117-CD-CTRA-TIT-VL
           MOVE SGTS0145-CD-OPR-ETRD-MCP
                                       TO SGTSI117-CD-OPR-ETRD-MCP
           MOVE SGTS0145-NR-VRS-NOVA
                                       TO SGTSI117-NR-VRS-OPR-ETRD

           MOVE SGTSL117-CD-TIP-VLDC-OPR (SGTSL117-IC-TAB)
                                       TO SGTSI117-CD-TIP-VLDC-OPR
           MOVE SGTSL117-CD-USU-ULT-ATL  (SGTSL117-IC-TAB)
                                       TO SGTSI117-CD-USU-ULT-ATL
           MOVE SGTSL117-TX-JST-VLDC-OPR (SGTSL117-IC-TAB)
                                       TO SGTSI117-TX-JST-VLDC-OPR
           MOVE SGTSL117-TS-ULT-ATL      (SGTSL117-IC-TAB)
                                       TO SGTSI117-TS-ULT-ATL

           PERFORM 900000-EXECUTAR-MODULO

           .
       492000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       500000-MOVIMENTAR-SAIDA         SECTION.
      *----------------------------------------------------------------*
           MOVE 0086                   TO MSG1-CD-SEQL-ERRO

           MOVE 'Geração da Versão da Operação efetuada com sucesso.'
                                       TO SGTS0145-TX-RTN-PRCT
           .
       500000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *----------------------------------------------------------------*
      * Executa um módulo implementado com CONTAINER                   *
      * Não incluir nessa section os módulos chamados via COMMAREA     *
      *                                                                *
      *----------------------------------------------------------------*
       900000-EXECUTAR-MODULO          SECTION.
      *----------------------------------------------------------------*
      *    Grava o container de requisição do módulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM

               WHEN SGTSPE05
                   EXEC CICS LINK
                             PROGRAM ( SGTSPE05  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSI069
                   EXEC CICS LINK
                             PROGRAM ( SGTSI069  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSL507
                   EXEC CICS LINK
                             PROGRAM ( SGTSL507  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSI071
                   EXEC CICS LINK
                             PROGRAM ( SGTSI071  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSL063
                   EXEC CICS LINK
                             PROGRAM ( SGTSL063  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSI063
                   EXEC CICS LINK
                             PROGRAM ( SGTSI063  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSL084
                   EXEC CICS LINK
                             PROGRAM ( SGTSL084  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSI084
                   EXEC CICS LINK
                             PROGRAM ( SGTSI084  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSI070
                   EXEC CICS LINK
                             PROGRAM ( SGTSI070  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSL070
                   EXEC CICS LINK
                             PROGRAM ( SGTSL070  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSI087
                   EXEC CICS LINK
                             PROGRAM ( SGTSI087  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSL087
                   EXEC CICS LINK
                             PROGRAM ( SGTSL087  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSL061
                  EXEC CICS LINK
                            PROGRAM (SGTSL061  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSI061
                  EXEC CICS LINK
                            PROGRAM (SGTSI061  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSI064
                   EXEC CICS LINK
                             PROGRAM ( SGTSI064  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSL064
                   EXEC CICS LINK
                             PROGRAM ( SGTSL064  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSL011
                   EXEC CICS LINK
                             PROGRAM ( SGTSL011  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSI011
                   EXEC CICS LINK
                             PROGRAM ( SGTSI011  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSL068
                   EXEC CICS LINK
                             PROGRAM ( SGTSL068  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSI068
                   EXEC CICS LINK
                             PROGRAM ( SGTSI068  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSI079
                   EXEC CICS LINK
                             PROGRAM ( SGTSI079  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSL079
                   EXEC CICS LINK
                             PROGRAM ( SGTSL079  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSL060
                   EXEC CICS LINK
                             PROGRAM ( SGTSL060  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSI060
                   EXEC CICS LINK
                             PROGRAM ( SGTSI060  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSL080
                   EXEC CICS LINK
                             PROGRAM ( SGTSL080  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSI080
                   EXEC CICS LINK
                             PROGRAM ( SGTSI080  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSL081
                   EXEC CICS LINK
                             PROGRAM ( SGTSL081  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
                WHEN SGTSI081
                   EXEC CICS LINK
                             PROGRAM ( SGTSI081  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSL074
                   EXEC CICS LINK
                             PROGRAM ( SGTSL074  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSI074
                   EXEC CICS LINK
                             PROGRAM ( SGTSI074  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSL075
                   EXEC CICS LINK
                             PROGRAM ( SGTSL075  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSIV75
                   EXEC CICS LINK
                             PROGRAM ( SGTSIV75  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSL076
                  EXEC CICS LINK
                            PROGRAM (SGTSL076 )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSI076
                  EXEC CICS LINK
                            PROGRAM (SGTSI076  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL077
                  EXEC CICS LINK
                            PROGRAM (SGTSL077  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSI077
                  EXEC CICS LINK
                            PROGRAM (SGTSI077  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC
               WHEN SGTSL078
                  EXEC CICS LINK
                            PROGRAM (SGTSL078  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC
               WHEN SGTSI078
                  EXEC CICS LINK
                            PROGRAM (SGTSI078  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC
               WHEN SGTSL082
                   EXEC CICS LINK
                             PROGRAM ( SGTSL082  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSI082
                   EXEC CICS LINK
                             PROGRAM ( SGTSI082  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSL083
                   EXEC CICS LINK
                             PROGRAM ( SGTSL083  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSI083
                   EXEC CICS LINK
                             PROGRAM ( SGTSI083  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSL035
                   EXEC CICS LINK
                             PROGRAM ( SGTSL035  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSI035
                   EXEC CICS LINK
                             PROGRAM ( SGTSI035  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSL067
                   EXEC CICS LINK
                             PROGRAM ( SGTSL067  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSI067
                   EXEC CICS LINK
                             PROGRAM ( SGTSI067  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSL095
                   EXEC CICS LINK
                             PROGRAM ( SGTSL095  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSI095
                   EXEC CICS LINK
                             PROGRAM ( SGTSI095  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSL117
                   EXEC CICS LINK
                             PROGRAM ( SGTSL117  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSI117
                   EXEC CICS LINK
                             PROGRAM ( SGTSI117  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN OTHER
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9006          TO  MSG1-CD-ERRO-TS
                    STRING  'Não definida a execução do módulo'
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO            MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
      *
           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET   ERRO-CICS    TO  TRUE
                    MOVE  3004         TO  MSG1-CD-ERRO-TS
                    STRING
                    'Erro  ao acionar o módulo  ' NM-PGM  ', '
                    'RESP  = '  MSG1-RESP1  ', '
                    'RESP2 = '  MSG1-RESP2
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *--  Efetua a leitura do Container de Resposta se não existir o
      *--  container de erro.
      *--  Módulo SGTSI076 não gera resposta, então não efetuar leitura.
           IF NOT TEM-CONTAINER-ERRO
              IF  NM-PGM   NOT EQUAL SGTSI076
                  PERFORM  973000-LER-RESPOSTA
              END-IF
           END-IF
           MOVE SPACES                TO IN-CONTAINER-ERRO
           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       910000-VALIDAR-DATAS            SECTION.
      *-----------------------------------------------------------------
           MOVE  0087                  TO  MSG1-CD-SEQL-ERRO

           MOVE  4095                  TO  AREA-TAM
           MOVE  'SBDATAF -WRKAREA-FIM'
                                       TO  AREA-FIM

           CALL  SBDATAF            USING  FUNCAO
                                           WKSBDATA
                                           ARG01
                                           ARG02

      *--> Return Code SBDATAF
      *    0022 - Conteúdo do Argumento Não Numérico
      *    0024 - Data Informada Inválida
      *    0046 - Data Fora da Faixa de 01/01/0001 a 31/12/9999
           IF  RETURN-CODE                 NOT EQUAL  ZEROS  AND  0022
                                                 AND  0024   AND  0046
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SBDATAF           TO  MSG1-NM-PGM-ACND
                                           NM-PGM
               MOVE  0051              TO  MSG1-CD-ERRO-TS
               MOVE  'Erro no tratamento de datas.'
                                       TO  MSG1-TX-ERRO-TS
                                           MSG1-TX-RTN-PGM-ACND
      *        Monitorar variáveis do módulo chamado
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA

           END-IF

           IF  RETURN-CODE                 EQUAL  0022  OR
                                                  0024  OR 0046
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  0049              TO  MSG1-CD-ERRO-TS
               MOVE  'Data informada inválida.'
                                       TO  MSG1-TX-ERRO-TS
                                           MSG1-TX-RTN-PGM-ACND
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SBDATAF           TO  MSG1-NM-PGM-ACND
                                           NM-PGM
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       910000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       920000-CHAMAR-SGTSX005          SECTION.
      *-----------------------------------------------------------------
           MOVE  0088                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  LINK
                      PROGRAM  ( SGTSX005       )
                      COMMAREA ( SGTSX005-DADOS )
           END-EXEC

           IF  SGTSX005-CD-RTN-PGM         NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  3004              TO  MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTSX005-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTSX005-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SGTSX005-PSC-ERRO-PGM
                                       TO  MSG1-CD-SEQL-PGM-ACND
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       920000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       930000-CHAMAR-SGTST009          SECTION.
      *-----------------------------------------------------------------
           MOVE  0089                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  LINK
                      PROGRAM  ( SGTST009       )
                      COMMAREA ( SGTST009-DADOS )
           END-EXEC

           IF  SGTST009-CD-RTN-PGM         NOT EQUAL  ZEROS  AND  0100
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  3004              TO  MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST009-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTST009-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SGTST009-PSC-ERRO-PGM
                                       TO  MSG1-CD-SEQL-PGM-ACND
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       930000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       940000-CHAMAR-SGTSL006         SECTION.
      *-----------------------------------------------------------------
           MOVE  0090                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  LINK
                      PROGRAM  ( SGTSL006       )
                      COMMAREA ( SGTSL006-DADOS )
           END-EXEC

           IF  SGTSL006-CD-RTN-PGM         NOT EQUAL  ZEROS  AND  0100
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  3004              TO  MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTSL006-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTSL006-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SGTSL006-PSC-ERRO-PGM
                                       TO  MSG1-CD-SEQL-PGM-ACND
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       940000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       950000-CHAMAR-SGTST006         SECTION.
      *-----------------------------------------------------------------
           MOVE  0091                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  LINK
                      PROGRAM  ( SGTST006       )
                      COMMAREA ( SGTST006-DADOS )
           END-EXEC

           IF  SGTST006-CD-RTN-PGM         NOT EQUAL  ZEROS  AND  0100
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  3004              TO  MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST006-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTST006-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SGTST006-PSC-ERRO-PGM
                                       TO  MSG1-CD-SEQL-PGM-ACND
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       950000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM
               WHEN  SBDATAF
                        MOVE 'FUNCAO' TO MSGKW999-NM-VRV-MNT
                        MOVE FUNCAO   TO MSGKW999-VL-VRV-MNT
                        PERFORM 999003-ADICIONA-VRV-MNT

                        MOVE 'ARG01'  TO MSGKW999-NM-VRV-MNT
                        MOVE ARG01    TO MSGKW999-VL-VRV-MNT
                        PERFORM 999003-ADICIONA-VRV-MNT

                        MOVE 'ARG02'  TO MSGKW999-NM-VRV-MNT
                        MOVE ARG02    TO MSGKW999-VL-VRV-MNT
                        PERFORM 999003-ADICIONA-VRV-MNT

                        MOVE 'AREA-TAM' TO MSGKW999-NM-VRV-MNT
                        MOVE AREA-TAM   TO MSGKW999-VL-VRV-MNT
                        PERFORM 999003-ADICIONA-VRV-MNT

                        MOVE 'AREA-FIM' TO MSGKW999-NM-VRV-MNT
                        MOVE AREA-FIM   TO MSGKW999-VL-VRV-MNT
                        PERFORM 999003-ADICIONA-VRV-MNT

      *------> SGTSPE05 - Consulta o Estado e Tipo da Ultima Versão
               WHEN  SGTSPE05
                     MOVE 'PE05-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSPE05-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'PE05-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSPE05-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTSPE05-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> SGTSX005 - Persistência da Operação Estruturada
               WHEN  SGTSX005
                     MOVE 'X005-001-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSX005-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'X005-002-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSX005-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'X005-003-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSX005-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTSX005-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'X005-004-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSX005-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     IF  SGTSX005-INCLUIR
                         MOVE 'X005-005-CD-USU-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                         MOVE  SGTSX005-CD-USU-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                         PERFORM  999003-ADICIONA-VRV-MNT

                         MOVE 'X005-006-TS-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                         MOVE  SGTSX005-TS-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                         PERFORM  999003-ADICIONA-VRV-MNT

                         MOVE 'X005-007-CD-TIP-VRS-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                         MOVE  SGTSX005-CD-TIP-VRS-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                         PERFORM  999003-ADICIONA-VRV-MNT

                         MOVE 'X005-008-NR-VRS-REF'
                                       TO  MSGKW999-NM-VRV-MNT
                         MOVE  SGTSX005-NR-VRS-REF
                                       TO  MSGKW999-VL-VRV-MNT
                         PERFORM  999003-ADICIONA-VRV-MNT

                         MOVE 'X005-009-DT-INC-VGC-VRS-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                         MOVE  SGTSX005-DT-INC-VGC-VRS-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                         PERFORM  999003-ADICIONA-VRV-MNT

                         MOVE 'X005-CD-CLSC-ITMT-FNCO'
                                       TO  MSGKW999-NM-VRV-MNT
                         MOVE  SGTSX005-CD-CLSC-ITMT-FNCO
                                       TO  MSGKW999-VL-VRV-MNT
                         PERFORM  999003-ADICIONA-VRV-MNT

                         MOVE 'X005-CD-ESG-NEGL'
                                       TO  MSGKW999-NM-VRV-MNT
                         MOVE  SGTSX005-CD-ESG-NEGL
                                       TO  MSGKW999-VL-VRV-MNT
                         PERFORM  999003-ADICIONA-VRV-MNT
                     END-IF

      *------> SGTSI069 - Incluir Estado da Versão da Operação
               WHEN  SGTSI069
                     MOVE 'I069-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI069-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I069-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSI069-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTSI069-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I069-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI069-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I069-004-CD-TIP-EST-VRS'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI069-CD-TIP-EST-VRS
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I069-005-TX-OBS-EST-VRS'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI069-TX-OBS-EST-VRS
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                WHEN SGTSL507

                    MOVE 'SGTSL507-CD-CTRA-OPR-PGDR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL507-CD-CTRA-OPR-PGDR
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL507-CD-OPR-PGDR'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSL507-CD-OPR-PGDR IS NUMERIC
                       MOVE SGTSL507-CD-OPR-PGDR
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL507-NR-VRS-OPR-PGDR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL507-NR-VRS-OPR-PGDR
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL507-CD-CTRA-OPR-VCLD-E'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL507-CD-CTRA-OPR-VCLD-E
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL507-CD-OPR-ETRD-VCLD-E'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSL507-CD-OPR-ETRD-VCLD-E IS NUMERIC
                       MOVE SGTSL507-CD-OPR-ETRD-VCLD-E
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL507-NR-VRS-OPR-VCLD-E'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL507-NR-VRS-OPR-VCLD-E
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT


                WHEN SGTSI071

                    MOVE 'SGTSI071-CD-CTRA-OPR-PGDR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI071-CD-CTRA-OPR-PGDR
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI071-CD-OPR-PGDR'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSI071-CD-OPR-PGDR IS NUMERIC
                       MOVE SGTSI071-CD-OPR-PGDR
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI071-NR-VRS-OPR-PGDR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI071-NR-VRS-OPR-PGDR
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI071-CD-CTRA-OPR-VCLD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI071-CD-CTRA-OPR-VCLD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI071-CD-OPR-ETRD-VCLD'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSI071-CD-OPR-ETRD-VCLD IS NUMERIC
                       MOVE SGTSI071-CD-OPR-ETRD-VCLD
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI071-NR-VRS-OPR-VCLD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI071-NR-VRS-OPR-VCLD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI071-VL-PGTO'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI071-VL-PGTO  TO  VL-15V2
                    MOVE VL-15V2        TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI071-TS-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI071-TS-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL061
                  MOVE 'SGTSL061-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL061-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL061-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  IF SGTSL061-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSL061-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                  ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                  END-IF
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL061-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL061-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL061-DT-RPAC-ULT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL061-DT-RPAC-ULT
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI061
                  MOVE 'SGTSI061-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSI061-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSI061-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  IF SGTSI061-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSI061-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                  ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                  END-IF
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSI061-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSI061-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSI061-DT-RPAC-PGMD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSI061-DT-RPAC-PGMD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSI063
                    MOVE 'SGTSI063-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI063-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI063-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSI063-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSI063-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI063-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI063-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI063-CD-TIP-IN'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI063-CD-TIP-IN
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSL063
                    MOVE 'SGTSL063-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL063-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL063-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSL063-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSL063-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL063-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL063-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL063-CD-TIP-IN-E'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL063-CD-TIP-IN-E
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSL084
                    MOVE 'SGTSL084-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL084-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL084-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSL084-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSL084-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL084-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL084-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL084-CD-TIP-TARF-E'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL084-CD-TIP-TARF-E
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT


                WHEN SGTSI084
                    MOVE 'SGTSI084-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI084-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI084-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSI084-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSI084-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI084-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI084-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI084-CD-TIP-TARF'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI084-CD-TIP-TARF
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI084-CD-USU-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI084-CD-USU-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT


                    MOVE 'SGTSI084-TS-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI084-TS-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI084-PC-TARF-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI084-PC-TARF-OPR
                                       TO VL-04V10
                    MOVE VL-04V10
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSI070

                    MOVE 'SGTSI070-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI070-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI070-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSI070-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSI070-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI070-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI070-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI070-NR-SEQL-RCBT-PRVT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI070-NR-SEQL-RCBT-PRVT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI070-CD-TIP-RCBT-PRVT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI070-CD-TIP-RCBT-PRVT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI070-DT-RCBT-PRVT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI070-DT-RCBT-PRVT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI070-TS-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI070-TS-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI070-PC-RCBT-PRVT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI070-PC-RCBT-PRVT  TO  VL-04V04
                    MOVE VL-04V04      TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI070-VL-RCBT-PRVT'  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI070-VL-RCBT-PRVT    TO VL-15V2
                    MOVE VL-15V2                  TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI070-TX-OBS-RCBT-PRVT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI070-TX-OBS-RCBT-PRVT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSL070

                    MOVE 'SGTSL070-E-CD-TIP-SRVC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL070-E-CD-TIP-SRVC
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
                    MOVE 'SGTSL070-E-CD-CTRA-OPR-PGDR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL070-E-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL070-E-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSL070-E-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSL070-E-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL070-E-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL070-E-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL070-E-NR-SEQL-RCBT-PRVT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL070-E-NR-SEQL-RCBT-PRVT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL070-CD-TIP-RCBT-PRVT-ULT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL070-CD-TIP-RCBT-PRVT-ULT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL070-DT-RCBT-PRVT-ULT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL070-DT-RCBT-PRVT-ULT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSL087
                    MOVE 'SGTSL087-CD-TIP-SRVC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL087-CD-TIP-SRVC
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL087-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL087-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL087-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSL087-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSL087-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL087-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL087-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL087-NR-SEQL-RCBT-PRVT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL087-NR-SEQL-RCBT-PRVT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL087-CD-TIT-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL087-CD-TIT-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL087-NR-DVS-TIT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL087-NR-DVS-TIT
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
                    MOVE SGTSL087-TS-REG-PGTO-ULT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSI087

                    MOVE 'SGTSI087-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI087-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI087-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSI087-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSI087-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI087-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI087-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI087-NR-SEQL-RCBT-PRVT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI087-NR-SEQL-RCBT-PRVT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI087-CD-TIP-RCBT-PRVT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI087-CD-TIP-RCBT-PRVT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI087-DT-RCBT-PRVT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI087-DT-RCBT-PRVT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI087-CD-TIT-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI087-CD-TIT-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI087-NR-DVS-TIT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI087-NR-DVS-TIT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI087-TS-REG-PGTO'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI087-TS-REG-PGTO
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI087-IN-VCL-MNL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI087-IN-VCL-MNL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI087-CD-USU-VCL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI087-CD-USU-VCL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI087-TS-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI087-TS-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI087-VL-RLZD-VCLD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI087-VL-RLZD-VCLD  TO  VL-15V2
                    MOVE VL-15V2    TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSL064

                    MOVE 'SGTSL064-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL064-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL064-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSL064-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSL064-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL064-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL064-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL064-CD-TIP-PRTC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL064-CD-TIP-PRTC
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL064-CD-ULT-TIP-PRTC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL064-CD-ULT-TIP-PRTC
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL064-CD-ULT-BCO-PCT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL064-CD-ULT-BCO-PCT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSI064

                    MOVE 'SGTSI064-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI064-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI064-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSI064-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSI064-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI064-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI064-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI064-CD-BCO-PCT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI064-CD-BCO-PCT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI064-CD-TIP-PRTC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI064-CD-TIP-PRTC
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI064-IN-BCO-LDR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI064-IN-BCO-LDR
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI064-PC-PRTC-BCO'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI064-PC-PRTC-BCO  TO  VL-04V04
                    MOVE VL-04V04      TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI064-TS-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI064-TS-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI064-CD-USU-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI064-CD-USU-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

      *------> SGTST009 - Incluir Estado da Operação Estruturada
               WHEN  SGTST009
                     MOVE 'T009-001-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T009-002-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T009-003-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTST009-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTST009-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T009-004-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     IF  SGTST009-INCLUIR
                         MOVE 'T009-005-TS-EST-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                         MOVE  SGTST009-TS-EST-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                         PERFORM  999003-ADICIONA-VRV-MNT

                         MOVE 'T009-006-CD-USU-RSP-EST'
                                       TO  MSGKW999-NM-VRV-MNT
                         MOVE  SGTST009-CD-USU-RSP-EST
                                       TO  MSGKW999-VL-VRV-MNT
                         PERFORM  999003-ADICIONA-VRV-MNT

                         MOVE 'T009-007-CD-TIP-EST-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                         MOVE  SGTST009-CD-TIP-EST-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                         PERFORM  999003-ADICIONA-VRV-MNT

                         MOVE 'T009-008-TX-JST-EST'
                                       TO  MSGKW999-NM-VRV-MNT
                         MOVE  SGTST009-TX-JST-EST
                                       TO  MSGKW999-VL-VRV-MNT
                         PERFORM  999003-ADICIONA-VRV-MNT
                     END-IF


                WHEN SGTSL011
                    MOVE 'SGTSL011-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL011-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL011-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSL011-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSL011-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL011-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL011-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL011-DT-INC-TAXA-ULT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL011-DT-INC-TAXA-ULT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT


                WHEN SGTSI011

                    MOVE 'SGTSI011-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI011-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI011-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSI011-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSI011-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI011-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI011-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI011-DT-INC-TAXA-RMNC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI011-DT-INC-TAXA-RMNC
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI011-TS-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI011-TS-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI011-CD-USU-RSP-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI011-CD-USU-RSP-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI011-PC-TAXA-RMNC-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI011-PC-TAXA-RMNC-OPR  TO  VL-04V10
                    MOVE VL-04V10             TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI011-DT-FIM-TAXA-RMNC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI011-DT-FIM-TAXA-RMNC
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI011-CD-TIP-FMA-APL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI011-CD-TIP-FMA-APL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI011-CD-IEC-RMNC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI011-CD-IEC-RMNC
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI011-CD-PERC-CPTZ-JUR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI011-CD-PERC-CPTZ-JUR
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSL068

                    MOVE 'SGTSL068-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL068-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL068-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSL068-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSL068-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL068-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL068-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL068-CD-ULT-TIP-TAXA'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL068-CD-ULT-TIP-TAXA
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL068-DT-ULT-INC-VLD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL068-DT-ULT-INC-VLD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSI068

                    MOVE 'SGTSI068-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI068-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI068-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSI068-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSI068-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI068-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI068-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI068-CD-TIP-TAXA'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI068-CD-TIP-TAXA
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI068-DT-INC-CND-TAXA'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI068-DT-INC-CND-TAXA
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI068-PC-MIN-TAXA'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI068-PC-MIN-TAXA  TO  VL-04V10
                    MOVE VL-04V10      TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI068-DT-FIM-CND-TAXA'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI068-DT-FIM-CND-TAXA
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI068-TS-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI068-TS-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI068-CD-USU-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI068-CD-USU-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI068-CD-TIP-APL-PREF'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI068-CD-TIP-APL-PREF
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI068-CD-PERC-APL-PREF'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI068-CD-PERC-APL-PREF
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI068-CD-IEC-PREF'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI068-CD-IEC-PREF
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL079

                    MOVE 'SGTSL079-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL079-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL079-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSL079-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSL079-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL079-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL079-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL079-CD-TIP-CND-NEGD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL079-CD-TIP-CND-NEGD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL079-DT-INC-VLD-CND'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL079-DT-INC-VLD-CND
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI079

                    MOVE 'SGTSI079-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI079-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI079-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSI079-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSI079-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI079-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI079-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI079-CD-TIP-CND-NEGD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI079-CD-TIP-CND-NEGD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI079-DT-INC-VLD-CND'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI079-DT-INC-VLD-CND
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI079-CD-TIP-CBT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI079-CD-TIP-CBT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI079-DT-FIM-VLD-CND'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI079-DT-FIM-VLD-CND
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI079-IN-OBGD-CND'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI079-IN-OBGD-CND
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI079-PC-TAXA-QBRA-FNDO'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI079-PC-TAXA-QBRA-FNDO TO VL-04V10
                    MOVE VL-04V10      TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL060
                    MOVE 'SGTSL060-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL060-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL060-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSL060-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSL060-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL060-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL060-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL060-CD-ARQ-OPR-ULT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL060-CD-ARQ-OPR-ULT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL060-CD-TIP-ARQ'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL060-CD-TIP-ARQ
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI060
                    MOVE 'SGTSI060-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI060-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI060-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSI060-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSI060-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI060-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI060-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI060-CD-ARQ-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI060-CD-ARQ-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI060-CD-USU-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI060-CD-USU-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI060-TS-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI060-TS-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI060-TX-DCR-ANX'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI060-TX-DCR-ANX
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL080

                    MOVE 'SGTSL080-E-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL080-E-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL080-E-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSL080-E-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSL080-E-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL080-E-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL080-E-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL080-E-NR-ISLC-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL080-E-NR-ISLC-OPR
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

              WHEN SGTSI080

                    MOVE 'SGTSI080-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI080-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI080-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSI080-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSI080-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI080-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI080-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI080-NR-ISLC-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI080-NR-ISLC-OPR
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI080-CD-ARQ-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI080-CD-ARQ-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI080-IN-ISLC-OPR-ATI'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI080-IN-ISLC-OPR-ATI
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI080-DT-INC-ISLC-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI080-DT-INC-ISLC-OPR
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI080-DT-FIM-ISLC-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI080-DT-FIM-ISLC-OPR
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI080-TX-DCR-ISLC-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI080-TX-DCR-ISLC-OPR
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI080-TS-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI080-TS-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL081

                    MOVE 'SGTSL081-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL081-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL081-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSL081-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSL081-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL081-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL081-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL081-NR-ULT-PRV-CMPS-GRT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL081-NR-ULT-PRV-CMPS-GRT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI081

                    MOVE 'SGTSI081-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI081-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI081-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSI081-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSI081-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI081-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI081-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI081-NR-PRV-CMPS-GRT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI081-NR-PRV-CMPS-GRT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI081-CD-NTZ-GRT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI081-CD-NTZ-GRT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI081-CD-FMA-VCLC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI081-CD-FMA-VCLC
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI081-DT-LIM-GRT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI081-DT-LIM-GRT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI081-PC-GRT-CMPS-PRVT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI081-PC-GRT-CMPS-PRVT TO VL-04V10
                    MOVE VL-04V10      TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI081-VL-GRT-CMPS-PRVT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI081-VL-GRT-CMPS-PRVT TO VL-15V2
                    MOVE VL-15V2       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI081-TS-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI081-TS-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI081-CD-USU-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI081-CD-USU-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT


      *------> SGTSL006 - Listar Clientes da Operação Estruturada
               WHEN  SGTSL006
                     MOVE 'L006-001-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL006-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L006-002-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL006-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L006-003-ULT-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSL006-ULT-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTSL006-ULT-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L006-004-ULT-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL006-ULT-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L006-005-ULT-CD-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL006-ULT-CD-CLI
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L006-006-ULT-TIP-FUC-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL006-ULT-TIP-FUC-CLI
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L006-007-DT-EMS-TIT-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL006-DT-EMS-TIT-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> SGTST006 - Incluir Clientes da Operação Estruturada
               WHEN  SGTST006
                     MOVE 'T006-001-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST006-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T006-002-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST006-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T006-003-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTST006-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTST006-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T006-004-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST006-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T006-005-CD-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST006-CD-CLI
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T006-006-CD-TIP-FUC-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST006-CD-TIP-FUC-CLI
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT


               WHEN SGTSL074
                    MOVE 'SGTSL074-CD-TIP-SRVC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL074-CD-TIP-SRVC
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL074-E-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL074-E-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL074-E-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSL074-E-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSL074-E-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL074-E-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL074-E-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL074-E-CD-AG-CLSC-RSCO'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL074-E-CD-AG-CLSC-RSCO
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL074-E-NR-ETRD-AG-CLSC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL074-E-NR-ETRD-AG-CLSC
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL074-E-NR-SEQL-NOTA-AG'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL074-E-NR-SEQL-NOTA-AG
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL074-E-SG-NOTA-AG-CLSC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL074-E-SG-NOTA-AG-CLSC
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL074-E-DT-INC-ATBC-NOTA'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL074-E-DT-INC-ATBC-NOTA
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL074-E-DT-FIM-ATBC-NOTA'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL074-E-DT-FIM-ATBC-NOTA
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL074-E-NR-SEQL-NOTA-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL074-E-NR-SEQL-NOTA-OPR
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI074
                    MOVE 'SGTSI074-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI074-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI074-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSI074-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSI074-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI074-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI074-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI074-CD-AG-CLSC-RSCO'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI074-CD-AG-CLSC-RSCO
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI074-NR-ETRD-AG-CLSC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI074-NR-ETRD-AG-CLSC
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI074-NR-SEQL-NOTA-AG'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI074-NR-SEQL-NOTA-AG
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI074-CD-ARQ-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI074-CD-ARQ-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI074-DT-INC-ATBC-NOTA'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI074-DT-INC-ATBC-NOTA
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI074-DT-FIM-ATBC-NOTA'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI074-DT-FIM-ATBC-NOTA
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI074-TS-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI074-TS-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI074-CD-USU-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI074-CD-USU-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT


      *------> SGTSL075 - Listar Ordens de Negociação da Operação
               WHEN  SGTSL075
                     MOVE 'L075-001-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSL075-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTSL075-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L075-002-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL075-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L075-003-NR-ULT-SEQL-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL075-NR-ULT-SEQL-ORD-NEGO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> SGTSIV75 - Incluir Ordem de Negociação no Versionamento
               WHEN  SGTSIV75
                     MOVE 'IV75-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSIV75-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IV75-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSIV75-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTSIV75-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                         PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IV75-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSIV75-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IV75-004-NR-SEQL-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSIV75-NR-SEQL-ORD-NEGO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IV75-005-CD-USU-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSIV75-CD-USU-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IV75-006-TS-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSIV75-TS-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IV75-007-CD-MOD-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSIV75-CD-MOD-ORD-NEGO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IV75-008-CD-ARQ-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSIV75-CD-ARQ-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IV75-009-CD-MTV-NEGO-VND'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSIV75-CD-MTV-NEGO-VND
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IV75-010-CD-TIP-APL-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSIV75-CD-TIP-APL-TAXA
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IV75-011-CD-IEC-TAXA-VND'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSIV75-CD-IEC-TAXA-VND
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IV75-012-DT-EMS-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSIV75-DT-EMS-ORD-NEGO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IV75-013-DT-ESTV-TAXA-VND'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSIV75-DT-ESTV-TAXA-VND
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IV75-014-VL-PU-PAPL-VND'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSIV75-VL-PU-PAPL-VND
                                       TO  VL-15V2
                     MOVE  VL-15V2     TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IV75-015-VL-TAXA-NEGO-VND'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSIV75-VL-TAXA-NEGO-VND
                                       TO  VL-15V2
                     MOVE  VL-15V2     TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IV75-016-TX-JST-VND-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSIV75-TX-JST-VND-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IV75-017-TX-OBS-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSIV75-TX-OBS-ORD-NEGO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IV75-018-TX-MSG-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSIV75-TX-MSG-ORD-NEGO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IV75-019-DT-VND'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSIV75-DT-VND
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IV75-020-QT-TTL-PAPL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSIV75-QT-TTL-PAPL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT


                     MOVE 'IV75-021-QT-PAPL-CMPD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSIV75-QT-PAPL-CMPD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT


                     MOVE 'IV75-022-QT-PAPL-LVRE'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSIV75-QT-PAPL-LVRE
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT



      *------> SGTSL076 - Listar Clientes Compradores da Ordem de Venda
               WHEN  SGTSL076
                     MOVE 'L076-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL076-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L076-002-076-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSL076-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTSL076-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L076-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL076-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L076-004-NR-SEQL-ORD-NEGO-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL076-NR-SEQL-ORD-NEGO-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L076-005-CD-CLI-CPRD-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL076-CD-CLI-CPRD-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> SGTSI076 - Incluir Clientes Comprador
               WHEN  SGTSI076
                     MOVE 'I076-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI076-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I076-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSI076-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTSI076-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I076-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI076-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I076-004-NR-SEQL-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI076-NR-SEQL-ORD-NEGO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I076-005-CD-CLI-CPRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI076-CD-CLI-CPRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I076-006-QT-PAPL-NEGO-VND'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI076-QT-PAPL-NEGO-VND
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> SGTSL077 - Listar Autorizações da Ordem de Negociação
               WHEN  SGTSL077
                     MOVE 'L077-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL077-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L077-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSL077-CD-OPR-ETRD-MCP
                                          NUMERIC
                         MOVE SGTSL077-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L077-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL077-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L077-004-NR-SEQL-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL077-NR-SEQL-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L077-005-TS-AUTZ-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL077-TS-AUTZ-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> SGTSI077 - Incluir Autorizações da Ordem de Negociação
               WHEN  SGTSI077
                     MOVE 'I077-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI077-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I077-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSI077-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTSI077-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I077-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI077-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I077-004-NR-SEQL-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI077-NR-SEQL-ORD-NEGO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I077-005-TS-AUTZ-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI077-TS-AUTZ-NEGO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I077-006-CD-USU-RSP-AUTZ'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI077-CD-USU-RSP-AUTZ
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I077-007-IN-FLZ-EXCO-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI077-IN-FLZ-EXCO-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I077-008-TX-JST-FLZ-EXCO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI077-TX-JST-FLZ-EXCO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> SGTSL078 - Listar Estado da Ordem de Negociação
               WHEN  SGTSL078
                     MOVE 'L078-001-CD-TIP-SRVC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL078-CD-TIP-SRVC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L078-002-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL078-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L078-003-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSL078-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTSL078-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L078-004-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL078-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L078-005-NR-SEQL-ORD-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL078-NR-SEQL-ORD-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L078-006-TS-EST-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL078-TS-EST-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> SGTSI078 - Incluir Estado da Ordem de Negociação
               WHEN  SGTSI078
                     MOVE 'I078-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I078-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSI078-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTSI078-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I078-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I078-004-NR-SEQL-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-NR-SEQL-ORD-NEGO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I078-005-TS-EST-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-TS-EST-ORD-NEGO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I078-006-CD-USU-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-CD-USU-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'I078-007-CD-TIP-EST-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-CD-TIP-EST-NEGO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT


               WHEN SGTSL082
                    MOVE 'SGTSL082-CD-TIP-SRVC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL082-CD-TIP-SRVC
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL082-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL082-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL082-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSL082-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSL082-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL082-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL082-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL082-NR-PRV-CMPS-GRT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL082-NR-PRV-CMPS-GRT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL082-CD-ULT-CLI'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL082-CD-ULT-CLI
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI082

                    MOVE 'SGTSI082-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI082-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI082-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSI082-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSI082-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI082-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI082-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI082-NR-PRV-CMPS-GRT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI082-NR-PRV-CMPS-GRT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI082-CD-CLI'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI082-CD-CLI
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL083
                    MOVE 'SGTSL083-E-CD-TIP-SRVC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL083-E-CD-TIP-SRVC
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL083-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL083-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL083-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSL083-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSL083-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL083-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL083-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL083-E-NR-SEQL-EXCO-PRFL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL083-E-NR-SEQL-EXCO-PRFL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI083

                    MOVE 'SGTSI083-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI083-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI083-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSI083-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSI083-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI083-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI083-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI083-NR-SEQL-EXCO-PRFL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI083-NR-SEQL-EXCO-PRFL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI083-CD-USU-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI083-CD-USU-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI083-TS-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI083-TS-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI083-IN-EXCO-ATV'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI083-IN-EXCO-ATV
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI083-TS-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI083-TS-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI083-TX-OBS-EXCO-PRFL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI083-TX-OBS-EXCO-PRFL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL035

                    MOVE 'SGTSL035-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL035-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL035-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSL035-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSL035-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL035-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL035-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL035-CD-PSS-CTT-EXNO'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL035-CD-PSS-CTT-EXNO
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI035
                    MOVE 'SGTSI035-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI035-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI035-CD-OPR-ETRD-MC'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSI035-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSI035-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI035-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI035-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI035-CD-PSS-CTT-EXNO'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI035-CD-PSS-CTT-EXNO
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI035-TS-ATL-CTT-EXNO'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI035-TS-ATL-CTT-EXNO
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI035-CD-USU-RSP-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI035-CD-USU-RSP-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT


               WHEN SGTSL067

                    MOVE 'SGTSL067-CD-TIP-SRVC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL067-CD-TIP-SRVC
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL067-E-CD-CTRA-TIT-VL'
                                      TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL067-E-CD-CTRA-TIT-VL
                                      TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL067-E-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSL067-E-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSL067-E-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL067-E-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL067-E-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL067-E-NR-NOTA-TCN-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL067-E-NR-NOTA-TCN-OPR
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI067

                    MOVE 'SGTSI067-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI067-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI067-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSI067-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSI067-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI067-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI067-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI067-NR-NOTA-TCN-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI067-NR-NOTA-TCN-OPR
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI067-TS-INCL-NOTA-TCN'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI067-TS-INCL-NOTA-TCN
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI067-CD-USU-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI067-CD-USU-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
               WHEN SGTSL095

                    MOVE 'SGTSL095-CD-TIP-SRVC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL095-CD-TIP-SRVC
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL095-CD-CTRA-TIT-VL'
                                      TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL095-CD-CTRA-TIT-VL
                                      TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL095-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSL095-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSL095-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL095-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL095-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL095-E-NR-SEQL-EXCO-PRFL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL095-E-NR-SEQL-EXCO-PRFL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL095-E-NR-SEQL-ITEM-EXCO'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL095-E-NR-SEQL-ITEM-EXCO
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI095

                    MOVE 'SGTSI095-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI095-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI095-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSI095-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSI095-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI095-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI095-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI095-NR-SEQL-EXCO-PRFL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI095-NR-SEQL-EXCO-PRFL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI095-NR-SEQL-ITEM-EXCO'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI095-NR-SEQL-ITEM-EXCO
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI095-CD-RGR-VLDC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI095-CD-RGR-VLDC
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI095-NR-SEQL-ITC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI095-NR-SEQL-ITC
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI095-IN-ITEM-CRIO-EXCO'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI095-IN-ITEM-CRIO-EXCO
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI095-VL-RLZD-ITEM-EXCO'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI095-VL-RLZD-ITEM-EXCO
                                       TO VL-15V4
                    MOVE VL-15V4
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI095-TX-RLZD-ITEM-EXCO'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI095-TX-RLZD-ITEM-EXCO
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI095-NR-RLZD-ITEM-EXCO'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI095-NR-RLZD-ITEM-EXCO
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI095-TX-OBS-ITEM-EXCO'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI095-TX-OBS-ITEM-EXCO
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL117

                    MOVE 'SGTSL117-CD-TIP-SRVC'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL117-CD-TIP-SRVC
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL117-CD-CTRA-TIT-VL'
                                      TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL117-CD-CTRA-TIT-VL
                                      TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL117-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSL117-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSL117-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL117-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL117-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL117-ULT-CD-TIP-VLDC-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL117-ULT-CD-TIP-VLDC-OPR
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSI117

                    MOVE 'SGTSI117-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI117-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI117-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF SGTSI117-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSI117-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI117-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI117-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI117-CD-TIP-VLDC-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI117-CD-TIP-VLDC-OPR
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI117-CD-USU-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI117-CD-USU-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI117-TX-JST-VLDC-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI117-TX-JST-VLDC-OPR
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI117-TS-ULT-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI117-TS-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT


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
           MOVE 0092                   TO MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       970000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------
           MOVE  0093                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do Programa e o nome do Container da Requisição
           MOVE 'SGTS0145'             TO  NM-PGM
           MOVE 'SGTS0145-RQSC'        TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM  973100-LER-CONTAINER-NODATA

      *--> Obtem o Tamanho Esperado da Requisição
           MOVE  LENGTH OF SGTS0145-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida Tamanho Esperado da Requisição X Tamanho Recebido
           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA
                                       TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Configura Dados para Leitura do Container de Requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Efetua a Leitura do Container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0145-RQSC)
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

      *--> Le o container de erro dos modulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           INITIALIZE                      IIBKERRO-PRM-ERRO

           MOVE  SPACES                TO  IN-CONTAINER-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS GET
                     CONTAINER ( NM-CTNR                     )
                     INTO      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
                     CONTINUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                     MOVE  3004        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     MOVE  NM-PGM      TO  MSG1-NM-PGM-ACND
                     MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                     MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                       PERFORM  969000-MONITORAR-VARIAVEIS
                       PERFORM  999000-GRAVA-ERRO-E-ENCERRA
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

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_módulo -RPST
           EVALUATE  NM-PGM
               WHEN  SGTSPE05
                     MOVE 'SGTSPE05-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSI069
                     MOVE 'SGTSI069-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSL070
                     MOVE 'SGTSL070-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSI070
                     MOVE 'SGTSI070-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSL087
                     MOVE 'SGTSL087-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSI087
                     MOVE 'SGTSI087-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSL074
                     MOVE 'SGTSL074-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSI074
                     MOVE 'SGTSI074-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSL075
                     MOVE 'SGTSL075-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSIV75
                     MOVE 'SGTSIV75-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSL078
                     MOVE 'SGTSL078-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSI078
                     MOVE 'SGTSI078-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSL076
                     MOVE 'SGTSL076-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSL077
                     MOVE 'SGTSL077-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSI077
                     MOVE 'SGTSI077-RPST'
                                       TO  NM-CTNR
               WHEN  OTHER
                     MOVE  NM-PGM      TO  NM-CTNR(1:8)
                     MOVE  '-RPST'     TO  NM-CTNR(9:5)
           END-EVALUATE

      *--> Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

      *--> Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE


      *    Lê a RESPOSTA de acordo com o programa que a gravou

           EVALUATE NM-PGM

               WHEN SGTSPE05
                    INITIALIZE SGTSPE05-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSPE05-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSI069
                    INITIALIZE SGTSI069-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI069-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSL507
                    MOVE LENGTH OF SGTSL507-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL507-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSI071
                    INITIALIZE SGTSI071-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI071-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSI063
                    INITIALIZE SGTSI063-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI063-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSL063
                    MOVE LENGTH OF SGTSL063-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL063-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSI084
                    INITIALIZE SGTSI084-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI084-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSL084
                    MOVE LENGTH OF SGTSL084-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL084-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSL061
                    MOVE LENGTH OF SGTSL061-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL061-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSI061
                    INITIALIZE SGTSI061-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI061-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSI070
                    INITIALIZE SGTSI070-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI070-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSL070
                    MOVE LENGTH OF SGTSL070-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL070-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

                WHEN SGTSL087
                    MOVE LENGTH OF SGTSL087-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL087-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

                WHEN SGTSI087
                    INITIALIZE SGTSI087-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI087-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

                WHEN SGTSL064
                    MOVE LENGTH OF SGTSL064-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL064-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

                WHEN SGTSI064
                    INITIALIZE SGTSI064-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI064-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

                WHEN SGTSL011
                    MOVE LENGTH OF SGTSL011-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL011-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

                WHEN SGTSI011
                    INITIALIZE SGTSI011-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI011-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

                WHEN SGTSL068
                    MOVE LENGTH OF SGTSL068-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL068-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
                WHEN SGTSI068
                    INITIALIZE SGTSI068-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI068-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSL079
                    MOVE LENGTH OF SGTSL079-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL079-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSI079
                    INITIALIZE SGTSI079-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI079-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSL060
                    MOVE LENGTH OF SGTSL060-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL060-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSI060
                    INITIALIZE SGTSI060-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI060-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSL080
                    MOVE LENGTH OF SGTSL080-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL080-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSI080
                    INITIALIZE SGTSI080-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI080-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
                WHEN SGTSL081
                    MOVE LENGTH OF SGTSL081-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL081-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSI081
                    INITIALIZE SGTSI081-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI081-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSL074
                    MOVE LENGTH OF SGTSL074-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL074-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSI074
                    INITIALIZE SGTSI074-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI074-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSL075
                    MOVE LENGTH OF SGTSL075-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL075-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

                WHEN SGTSIV75
                    INITIALIZE SGTSIV75-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSIV75-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
                WHEN SGTSL076
                    MOVE LENGTH OF SGTSL076-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL076-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL077
                    MOVE LENGTH OF SGTSL077-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL077-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSI077
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSI077-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL078
                    MOVE LENGTH OF SGTSL078-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL078-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSI078
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSI078-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

              WHEN SGTSL082
                    MOVE LENGTH OF SGTSL082-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL082-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSI082
                    INITIALIZE SGTSI082-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI082-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSL083
                    MOVE LENGTH OF SGTSL083-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL083-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSI083
                    INITIALIZE SGTSI083-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI083-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSL035
                    MOVE LENGTH OF SGTSL035-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL035-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSI035
                    INITIALIZE SGTSI035-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI035-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSL067
                    MOVE LENGTH OF SGTSL067-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL067-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSI067
                    INITIALIZE SGTSI067-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI067-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSL095
                    MOVE LENGTH OF SGTSL095-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL095-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSI095
                    INITIALIZE SGTSI095-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI095-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSL117
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL117-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTSI117
                    INITIALIZE SGTSI117-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI117-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9003         TO  MSG1-CD-ERRO-TS
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
           MOVE 0094                   TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Obtém a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO

           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       974000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE 0095                   TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           MOVE 'PRM-INF-OPRD'         TO NM-CTNR

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           INITIALIZE PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       974000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *   Section da Faixa de 980000 a 989900 para Gravação de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------

      *--> Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

           MOVE 0096                   TO MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de resposta
           MOVE 'SGTS0145-RPST'     TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                    )
                     FROM      (SGTS0145-RPST          )
                     FLENGTH   (LENGTH OF SGTS0145-RPST)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *--> Postar container com dados de requisição (entrada)
      *    Modulo CONTAINER  - NOME_PGM-RQSC
      *    Modulo IIB (FIXO) - PRM-DADOS-RQSC
      *-----------------------------------------------------------------
           MOVE 0097                   TO MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com módulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RQSC,
      *    senão nome_modulo -RQSC
           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RQSC'       TO NM-CTNR(9:5)

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Faz PUT da requisição do programa a executar
           EVALUATE NM-PGM
               WHEN SGTSPE05
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSPE05-RQSC )
                              FLENGTH   (LENGTH OF SGTSPE05-RQSC)
                    END-EXEC
               WHEN SGTSI069
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSI069-RQSC )
                              FLENGTH   (LENGTH OF SGTSI069-RQSC)
                    END-EXEC

               WHEN SGTSL507
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL507-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL507-RQSC )
                    END-EXEC
               WHEN SGTSI071
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI071-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI071-RQSC )
                    END-EXEC
               WHEN SGTSL063
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL063-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL063-RQSC )
                    END-EXEC
               WHEN SGTSI063
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI063-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI063-RQSC )
                    END-EXEC

               WHEN SGTSI084
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI084-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI084-RQSC )
                    END-EXEC

               WHEN SGTSL084
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL084-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL084-RQSC )
                    END-EXEC

               WHEN SGTSL061
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL061-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL061-RQSC )
                    END-EXEC

               WHEN SGTSI061
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI061-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI061-RQSC )
                    END-EXEC

               WHEN SGTSI070
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI070-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI070-RQSC )
                    END-EXEC

               WHEN SGTSL070
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL070-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL070-RQSC )
                    END-EXEC

               WHEN SGTSL087
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL087-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL087-RQSC )
                    END-EXEC

               WHEN SGTSI087
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI087-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI087-RQSC )
                    END-EXEC

               WHEN SGTSL064
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL064-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL064-RQSC )
                    END-EXEC

               WHEN SGTSI064
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI064-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI064-RQSC )
                    END-EXEC

               WHEN SGTSL011
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL011-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL011-RQSC )
                    END-EXEC

               WHEN SGTSI011
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI011-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI011-RQSC )
                    END-EXEC

               WHEN SGTSL068
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL068-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL068-RQSC )
                    END-EXEC

               WHEN SGTSI068
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI068-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI068-RQSC )
                    END-EXEC
               WHEN SGTSL079
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL079-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL079-RQSC )
                    END-EXEC
               WHEN SGTSI079
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI079-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI079-RQSC )
                    END-EXEC
               WHEN SGTSL060
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL060-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL060-RQSC )
                    END-EXEC
               WHEN SGTSI060
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI060-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI060-RQSC )
                    END-EXEC
                WHEN SGTSL080
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL080-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL080-RQSC )
                    END-EXEC
                WHEN SGTSI080
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI080-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI080-RQSC )
                    END-EXEC
                WHEN SGTSL081
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL081-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL081-RQSC )
                    END-EXEC
                WHEN SGTSI081
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI081-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI081-RQSC )
                    END-EXEC
               WHEN SGTSL074
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL074-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL074-RQSC )
                    END-EXEC
               WHEN SGTSI074
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI074-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI074-RQSC )
                    END-EXEC

                WHEN SGTSL075
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL075-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL075-RQSC )
                    END-EXEC

                WHEN SGTSIV75
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSIV75-RQSC           )
                              FLENGTH   (LENGTH OF SGTSIV75-RQSC )
                    END-EXEC

               WHEN SGTSL076
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL076-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL076-RQSC )
                    END-EXEC

               WHEN SGTSI076
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSI076-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSI076-RQSC )
                    END-EXEC

               WHEN SGTSL077
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL077-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL077-RQSC )
                    END-EXEC

               WHEN SGTSI077
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSI077-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSI077-RQSC )
                    END-EXEC

               WHEN SGTSL078
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL078-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL078-RQSC )
                    END-EXEC

               WHEN SGTSI078
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSI078-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSI078-RQSC )
                    END-EXEC

               WHEN SGTSL082
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL082-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL082-RQSC )
                    END-EXEC

               WHEN SGTSI082
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI082-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI082-RQSC )
                    END-EXEC

               WHEN SGTSL083
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL083-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL083-RQSC )
                    END-EXEC

               WHEN SGTSI083
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI083-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI083-RQSC )
                    END-EXEC

               WHEN SGTSL035
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL035-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL035-RQSC )
                    END-EXEC

              WHEN SGTSI035
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI035-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI035-RQSC )
                    END-EXEC

               WHEN SGTSL067
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSL067-RQSC )
                              FLENGTH   (LENGTH OF SGTSL067-RQSC)
                    END-EXEC
               WHEN SGTSI067
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI067-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI067-RQSC )
                    END-EXEC

               WHEN SGTSL095
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSL095-RQSC )
                              FLENGTH   (LENGTH OF SGTSL095-RQSC)
                    END-EXEC
               WHEN SGTSI095
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI095-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI095-RQSC )
                    END-EXEC

               WHEN SGTSL117
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSL117-RQSC )
                              FLENGTH   (LENGTH OF SGTSL117-RQSC)
                    END-EXEC
               WHEN SGTSI117
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI117-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI117-RQSC )
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9004         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na gravação da requisição do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
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
           MOVE 0098                   TO MSG1-CD-SEQL-ERRO

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST,
      *    senão nome_modulo -RPST
           EVALUATE  NM-PGM
               WHEN  SGTSPE05
                     MOVE  'SGTSPE05-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSI069
                     MOVE 'SGTSI069-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSL070
                     MOVE 'SGTSL070-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSI070
                     MOVE 'SGTSI070-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSL087
                     MOVE 'SGTSL087-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSI087
                     MOVE 'SGTSI087-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSL074
                     MOVE 'SGTSL074-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSI074
                     MOVE 'SGTSI074-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSL075
                     MOVE 'SGTSL075-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSIV75
                     MOVE 'SGTSIV75-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSL078
                     MOVE 'SGTSL078-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSI078
                     MOVE 'SGTSI078-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSL076
                     MOVE 'SGTSL076-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSL077
                     MOVE 'SGTSL077-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSI077
                     MOVE 'SGTSI077-RPST'
                                       TO  NM-CTNR
               WHEN  OTHER
                     MOVE  NM-PGM      TO  NM-CTNR(1:8)
                     MOVE  '-RPST'     TO  NM-CTNR(9:5)
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
           MOVE  0099                  TO  MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE                      IIBKERRO-PRM-ERRO

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
               WHEN  MSG1-RESP1            EQUAL DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL DFHRESP(NORMAL)
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
      *    As sections abaixo iniciadas com 999000 representam os
      *    tratamentos de erro utilizando o Sistema MSG
      *-----------------------------------------------------------------

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
                     PERFORM 999004-TRATA-ERRO-CTNR
               WHEN  ERRO-TAMANHO-AREA
                     PERFORM 999005-TRATA-ERRO-AREA-DADOS
               WHEN  ERRO-CANAL
                     PERFORM 999006-TRATA-ERRO-CANAL
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

      *--> Adiciona as Variáveis de Entrada para Monitoração
               MOVE '0145-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE SGTS0145-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE '0145-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
               IF  SGTS0145-CD-OPR-ETRD-MCP
                                           NUMERIC
                   MOVE  SGTS0145-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                   MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
               ELSE
                   MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
               END-IF
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE '0145-003-NR-VRS-REF'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE SGTS0145-NR-VRS-REF
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE '0145-004-DT-INC-VGC'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE SGTS0145-DT-INC-VGC
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE '0145-005-CD-TIP-VRS-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE SGTS0145-CD-TIP-VRS-OPR
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE '0145-006-TX-OBS-VRS'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE SGTS0145-TX-OBS-VRS
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE '0145-007-NR-VRS-NOVA'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE SGTS0145-NR-VRS-NOVA
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE '0145-008-CD-CLSC-ITMT-FNCO'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE SGTS0145-CD-CLSC-ITMT-FNCO
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava a Ocorrência de Erro no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava a Ocorrência de Erro no Container de erro
           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                  )
                      FROM      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      RESP      ( MSG1-RESP1                  )
                      RESP2     ( MSG1-RESP2                  )
           END-EXEC

      *--> Encerra a transacao
           PERFORM 999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      *----------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      *----------------------------------------
       COPY HLPKP999.
      *----------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
       COPY SGTKP999.
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
       COPY MSGKPMG1.

      *-----------------------------------------------------------------
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *---------------------- Fim SGTS0145 -----------------------------
