       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0146
      * ANALISTA..: F2054109 - Cintia Ap Soares Iglesias
      * AUTOR.....: F2804421 - Eliana Cristina Ramos da Silva
      * DATA......: 22/05/2017
      * OBJETIVO..: Negocial -  Excluir versão da operação estruturada.
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS004 03/03/2023 F9158937 - Recomplilção por alteração book
      *                   SGTKL079.
      * VRS003 22/09/2020 F9158937 - Voltar tratamento de exclusão da
      *                   tabela CLC_IN_OPR_ETRD
      * VRS002 10/10/2019 F2804421 DE 2016/D145195 - Inclusão do campo
      *                   SGTSL078-CD-TIP-SRVC(p/chamada do SGTSL078),
      *                   utilizar opção SGTSL078-LISTA-TODOS.
      *        19/09/2019 F8785286 DE 2016/145195 -
      *            Exclusão do container SGTSE076-RPST.
      *        25/07/2019 F2804421 DE 2016/145195  -  Retirar tratamen-
      *              to de exclusão da tabela CLC_IN_OPR_ETRD.
      *        03/05/2019 F8785286 DE 2016/145195  -  Recompilação por
      *              alteração no book SGTKL075.
      *        03/04/2019 F2804421 DE 2016/145195 - Inclusão dos campos
      *              -SGTSL063-CD-ACAO que deverá ser igual a 2;
      *              -SGTSL063-NR-IN-OPR-ETRD E SGTSE063-NR-IN-OPR-ETRD.
      *        28/03/2019 F2804421 DE 2016/145195 - Tratar return code
      *                   100 para SGTSL006.
      *        06/03/2019 F8785286 DE 2016/145195 - Inclusão de movimen-
      *                tação para o campo SGTSL075-CD-ACAO (ação 1).
      * VRS001 07/12/2018 F2804421 DE 2016/145195 -
      *               - Incluir tratamento para a tabela
      *                 JST_VLDC_OPR_ETRD.
      *        08/11/2018 F8785286 DE 2016/145195 - Recompilação por
      *                 alteração no book SGTKL009.
      *        02/10/2018 F8785286 DE 2016/145195 - Recompilação por
      *                 alteração no book SGTKL069.
      *        20/09/2018 F8785286 DE 2016/145195 - Recompilação por
      *                 alteração no book SGTKL083.
      *        06/08/2018 F2804421 DE 2016/145195 -
      *               - Incluir tratamento de exclusão para as tabelas:
      *                 CLC_IN_OPR_ETRD E CLI_GRTR.
      *               - Alterar a chamada da lista RCBT_RLZD,somente
      *                 por carteira/operação/versão.
      *        20/07/2018 F2804421 DE 2016/145195 -
      *               - Retirar tratamento campo tipo de serviço da
      *                 chamada do pgm de lista SGTSL080.
      *        23/05/2018 F2804421 DE 2016/145195
      *                   - Inclusão do tratamento de exclusão para a
      *                    tabela ITEM_EXCO_OPR S1SGT095   (filha da
      *                    tabela EXCO_PRFL_SELD).
      *        22/04/2017 F2804421 DE 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS0146.
      *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
      *

       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

       77  CTE-INICIO-SS                 PIC  X(040)  VALUE
                   '*** S.S. COMECA AQUI ***'.

       77  CTE-PROG                      PIC  X(018)  VALUE
                   '*** SGTS0146 ***'.
       77  CTE-VERS                      PIC  X(006)  VALUE 'VRS004'.

      * Módulos acionados
       77  SGTSPE03                       PIC  X(008) VALUE 'SGTSPE03'.
       77  SGTST005                       PIC  X(008) VALUE 'SGTST005'.
       77  SGTSL006                       PIC  X(008) VALUE 'SGTSL006'.
       77  SGTST006                       PIC  X(008) VALUE 'SGTST006'.
       77  SGTSL009                       PIC  X(008) VALUE 'SGTSL009'.
       77  SGTST009                       PIC  X(008) VALUE 'SGTST009'.
       77  SGTSL011                       PIC  X(008) VALUE 'SGTSL011'.
       77  SGTSE011                       PIC  X(008) VALUE 'SGTSE011'.
       77  SGTSL035                       PIC  X(008) VALUE 'SGTSL035'.
       77  SGTSE035                       PIC  X(008) VALUE 'SGTSE035'.
       77  SGTSL060                       PIC  X(008) VALUE 'SGTSL060'.
       77  SGTSE060                       PIC  X(008) VALUE 'SGTSE060'.
       77  SGTSL061                       PIC  X(008) VALUE 'SGTSL061'.
       77  SGTSE061                       PIC  X(008) VALUE 'SGTSE061'.
       77  SGTSL063                       PIC  X(008) VALUE 'SGTSL063'.
       77  SGTSE063                       PIC  X(008) VALUE 'SGTSE063'.
       77  SGTSL064                       PIC  X(008) VALUE 'SGTSL064'.
       77  SGTSE064                       PIC  X(008) VALUE 'SGTSE064'.
       77  SGTSL067                       PIC  X(008) VALUE 'SGTSL067'.
       77  SGTSE067                       PIC  X(008) VALUE 'SGTSE067'.
       77  SGTSL068                       PIC  X(008) VALUE 'SGTSL068'.
       77  SGTSE068                       PIC  X(008) VALUE 'SGTSE068'.
       77  SGTSL069                       PIC  X(008) VALUE 'SGTSL069'.
       77  SGTSE069                       PIC  X(008) VALUE 'SGTSE069'.
       77  SGTSL070                       PIC  X(008) VALUE 'SGTSL070'.
       77  SGTSE070                       PIC  X(008) VALUE 'SGTSE070'.
       77  SGTSL507                       PIC  X(008) VALUE 'SGTSL507'.
       77  SGTSE071                       PIC  X(008) VALUE 'SGTSE071'.
       77  SGTSL074                       PIC  X(008) VALUE 'SGTSL074'.
       77  SGTSE074                       PIC  X(008) VALUE 'SGTSE074'.
       77  SGTSL075                       PIC  X(008) VALUE 'SGTSL075'.
       77  SGTSE075                       PIC  X(008) VALUE 'SGTSE075'.
       77  SGTSL076                       PIC  X(008) VALUE 'SGTSL076'.
       77  SGTSE076                       PIC  X(008) VALUE 'SGTSE076'.
       77  SGTSL077                       PIC  X(008) VALUE 'SGTSL077'.
       77  SGTSE077                       PIC  X(008) VALUE 'SGTSE077'.
       77  SGTSL078                       PIC  X(008) VALUE 'SGTSL078'.
       77  SGTSE078                       PIC  X(008) VALUE 'SGTSE078'.
       77  SGTSL079                       PIC  X(008) VALUE 'SGTSL079'.
       77  SGTSE079                       PIC  X(008) VALUE 'SGTSE079'.
       77  SGTSL080                       PIC  X(008) VALUE 'SGTSL080'.
       77  SGTSE080                       PIC  X(008) VALUE 'SGTSE080'.
       77  SGTSL081                       PIC  X(008) VALUE 'SGTSL081'.
       77  SGTSE081                       PIC  X(008) VALUE 'SGTSE081'.
       77  SGTSL082                       PIC  X(008) VALUE 'SGTSL082'.
       77  SGTSE082                       PIC  X(008) VALUE 'SGTSE082'.
       77  SGTSL083                       PIC  X(008) VALUE 'SGTSL083'.
       77  SGTSE083                       PIC  X(008) VALUE 'SGTSE083'.
       77  SGTSL084                       PIC  X(008) VALUE 'SGTSL084'.
       77  SGTSE084                       PIC  X(008) VALUE 'SGTSE084'.
       77  SGTSL087                       PIC  X(008) VALUE 'SGTSL087'.
       77  SGTSE087                       PIC  X(008) VALUE 'SGTSE087'.
       77  SGTSL088                       PIC  X(008) VALUE 'SGTSL088'.
       77  SGTSE088                       PIC  X(008) VALUE 'SGTSE088'.
       77  SGTSL095                       PIC  X(008) VALUE 'SGTSL095'.
       77  SGTSE095                       PIC  X(008) VALUE 'SGTSE095'.
       77  SGTSL117                       PIC  X(008) VALUE 'SGTSL117'.
       77  SGTSE117                       PIC  X(008) VALUE 'SGTSE117'.

      *----------------------------------------------------------------*
      *                     Containers utilizados
      *----------------------------------------------------------------*
      * Container de erro
       77 PRM-ERRO                 PIC X(16) VALUE 'PRM-ERRO'.

      * Container do usuário logado
       77 CTNR-PRM-INF-OPRD        PIC X(16) VALUE 'PRM-INF-OPRD'.
      * ---------------------------------------------------------------
      * Container da requisição do serviço IIB acionado
       77 PRM-DADOS-RQSC           PIC X(16) VALUE 'PRM-DADOS-RQSC'.

      * Container da resposta do serviço IIB acionado
       77 PRM-DADOS-RPST           PIC X(16) VALUE 'PRM-DADOS-RPST'.

      * Campos auxiliares de data.
       01  DT-FMT                         PIC  X(10)
                                                VALUE '  .  .    '.
       01  DT-FMT-R REDEFINES DT-FMT.
          03 DIA                           PIC  9(02).
          03 FILLER                        PIC  X(01).
          03 MES                           PIC  9(02).
          03 FILLER                        PIC  X(01).
          03 ANO                           PIC  9(04).

       01 SGTSL117-IC-TAB                  PIC S9(4) COMP-5 VALUE 0.

      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*
      * Book dos Containers e das subrotinas chamadas
      *----------------------------------------------------------------*
      * Book das variáveis auxiliares do tratam ento de erro via MSG
       COPY SGTKERR1.

      * Book para captura de abend cics da bloco codigo MSGKP999
       COPY HLPKW999.

      * Book para captura de erro via MSG1
       COPY MSGKWMG1.

      * Book do container de requisição e resposta deste modulo
       01 SGTS0146-DADOS.
       COPY SGTK0146.

      * Consultar dados do tipo de versão da operação referente ao
      * número da versão da Operação da entrada. OPR_ETRD_MCP.
      * via commarea
       01  SGTST005-DADOS.
       COPY SGTKT005.

      * Exclusão CLI_OPR_ETRD
      * via commarea
       01  SGTST006-DADOS.
       COPY SGTKT006.

      * Exclusão EST_OPR_ETRD.
      * via commarea
       01  SGTST009-DADOS.
       COPY SGTKT009.

      * Exclusão RMNC_OPR_ETRD
       COPY SGTKE011.

      * Exclusão CTT_EXNO_OPR_ETRD
       COPY SGTKE035.

      * Exclusão ANX_OPRD_ETRD
       COPY SGTKE060.

      * Exclusão RPAC_PGMD_OPR
       COPY SGTKE061.

      * Exclusão IN_OPR_ETRD
       COPY SGTKE063.

      * Exclusão BCO_PCT_OPR
       COPY SGTKE064.

      * Exclusão NOTA_TCN_VCLD_OPR
       COPY SGTKE067.

      * Exclusão PREF_TAXA_OPR
       COPY SGTKE068.

      * Exclusão EST_VRS_OPR_ETRD
       COPY SGTKE069.

      * Exclusão RCBT_PRVT_OPR_ETRD
       01  SGTSE070-DADOS.
       COPY SGTKE070.

      * Exclusão OPR_PG_OTR_OPR
       COPY SGTKE071.

      * Exclusão NOTA_CLSC_OPR
       COPY SGTKE074.

      * Exclusão ORD_NEGO_OPR
       01  SGTSE075-DADOS.
       COPY SGTKE075.

      * Exclusão CLI_CPRD_NEGO_VND
       01 SGTSE076-DADOS.
       COPY SGTKE076.

      * Exclusão AUTZ_NEGO_OPR_ETRD
       01 SGTSE077-DADOS.
       COPY SGTKE077.

      * Exclusão EST_ORD_NEGO_OPR
       01 SGTSE078-DADOS.
       COPY SGTKE078.

      * Exclusão CND_NEGD_OPR
       01  SGTSE079-DADOS.
       COPY SGTKE079.

      * Exclusão ISLC_OPR_ETRD
       COPY SGTKE080.

      * Exclusão PRV_CMPS_GRT
       COPY SGTKE081.

      * Exclusão CLI_GRTR
       COPY SGTKE082.

      * Exclusão EXCO_PRFL_SELD
       01  SGTSE083-DADOS.
       COPY SGTKE083.

      * Exclusão TARF_OPR_ETRD
       01  SGTSE084-DADOS.
       COPY SGTKE084.

      * Exclusão RCBT_RLZD_OPR
       01  SGTSE087-DADOS.
       COPY SGTKE087.

      * Exclusão  CLC_IN_OPR_ETRD
       COPY SGTKE088.

      * Exclusão  ITEM_EXCO_OPR
       COPY SGTKE095.

      * Exclusão  JST_VLDC_OPR_ETRD
       COPY SGTKE117.
      *--------------------------Consultas ---------------------------*
      * Consultar a existência de cód.tipo estado da versão > 2
      * (A avaliar)  na tabela EST_VRS_OPR_ETRD.
      * via container.
       01  SGTSPE03-DADOS.
       COPY SGTKPE03.

      * Lista  CLI_OPR_ETRD
      * via commarea
       01  SGTSL006-DADOS.
       COPY SGTKL006.

      * Lista EST_OPR_ETRD.
      * via commarea
       01  SGTSL009-DADOS.
       COPY SGTKL009.

      * Lista RMNC_OPR_ETRD
       01  SGTSL011-DADOS.
       COPY SGTKL011.

      * Lista CTT_EXNO_OPR_ETRD
       COPY SGTKL035.

      * Lista  ANX_OPRD_ETRD
       COPY SGTKL060.

      * Lista RPAC_PGMD_OPR
       COPY SGTKL061.

      * Lista  IN_OPR_ETRD
       COPY SGTKL063.

      * Lista  BCO_PCT_OPR
       COPY SGTKL064.

      * Lista NOTA_TCN_VCLD_OPR
       COPY SGTKL067.

      * Lista PREF_TAXA_OPR
       COPY SGTKL068.

      * Lista EST_VRS_OPR_ETRD
       01  SGTSL069-DADOS.
       COPY SGTKL069.

      * Lista RCBT_PRVT_OPR_ETRD
       01  SGTSL070-DADOS.
       COPY SGTKL070.

      * Lista NOTA_CLSC_OPR
       COPY SGTKL074.

      * Lista ORD_NEGO_OPR
       COPY SGTKL075.

      * Lista CLI_CPRD_NEGO_VND
       COPY SGTKL076.

      * Lista AUTZ_NEGO_OPR_ETRD
       01 SGTKL077-DADOS.
       COPY SGTKL077.

      * Lista EST_ORD_NEGO_OPR
       01 SGTKL078-DADOS.
       COPY SGTKL078.

      * Lista CND_NEGD_OPR
       COPY SGTKL079.

      * Lista ISLC_OPR_ETRD
       COPY SGTKL080.

      * Lista PRV_CMPS_GRT
       COPY SGTKL081.

      * Lista CLI_GRTR
       COPY SGTKL082.

      * Lista EXCO_PRFL_SELD
       COPY SGTKL083.

      * Lista TARF_OPR_ETRD
       COPY SGTKL084.

      * Lista RCBT_RLZD_OPR
       01  SGTSL087-DADOS.
       COPY SGTKL087.

      * Lista CLC_IN_OPR_ETRD
       COPY SGTKL088.

      * Lista OPR_PG_OTR_OPR
       01  SGTSL507-DADOS.
       COPY SGTKL507.

      * Lista  ITEM_EXCO_OPR
       COPY SGTKL095.

      * Lista  JST_VLDC_OPR_ETRD
       01  SGTSL117-DADOS.
       COPY SGTKL117.

      * Book dos dados do usuário
       COPY IIBKOPRD.

      * Book do Container de Erro
       COPY IIBKERRO.

       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*
           PERFORM 100000-INICIAR

           PERFORM 200000-PROCESSAR

           PERFORM 990000-ENCERRAR

           GOBACK

           .
       000000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       100000-INICIAR                  SECTION.
      *----------------------------------------------------------------*
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *    Ativa a captura automatica de abend do CICS.
      *    WHENEVER CICS
       COPY HLPKE999.
           PERFORM 970000-LER-CANAL

      *    Define o nome do programa p/ fins manipulacao container
           MOVE 'SGTS0146'             TO NM-PGM

      *    Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

           PERFORM 974000-LER-DADOS-OPRD

           PERFORM 110000-VALIDAR-REQUISICAO

           .
       100000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       110000-VALIDAR-REQUISICAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *    Validação tamanho esperado da requisicao X tamanho recebido
      *    na section de leitura de container de requisicao 971000
      *
      *    Valida campo a campo da requisicao

           IF  SGTS0146-CD-CTRA-TIT-VL    <  ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 001                TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -              'inválido.'            TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0146-CD-CTRA-TIT-VL    =  ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 002               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -              'não informado.'      TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0146-CD-OPR-ETRD-MCP  NOT NUMERIC
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 003                TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0146-CD-OPR-ETRD-MCP   < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 004                TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado decapitais inv
      -             'válido.'          TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0146-CD-OPR-ETRD-MCP   =  ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 005                TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -          ' informado.'             TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0146-NR-VRS-OPR-ETRD   <  ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 006                TO  MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0146-NR-VRS-OPR-ETRD   =  ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 007                TO  MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       110000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 201000-VER-MAIOR-ESTADO-VRS

           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

      *    Verifica se possui estado maior que 2 - A avaliar.

           IF  SGTSPE03-IN-EST-VRS-02  =  'S'
               MOVE 597                TO MSG1-CD-ERRO-TS
               MOVE 'Exclusão não permitida. Operação já foi autorizada.
      -              ' '               TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *    Consulta dados da operação.
            PERFORM 202000-CONSULTAR-OPERACAO

            PERFORM 205000-TRATAR-OPR-PAGA
            PERFORM 210000-TRATAR-INDICADOR-OPR
            PERFORM 215000-TRATAR-TARIFA-OPR
            PERFORM 220000-TRATAR-REPACTUACAO
            PERFORM 225000-TRATAR-RECEB-REALIZADO
            PERFORM 230000-TRATAR-RECEB-PREVISTO
            PERFORM 235000-TRATAR-BANCO
            PERFORM 240000-TRATAR-ESTADO-OPR
            PERFORM 245000-TRATAR-REMUNERACAO-OPR
            PERFORM 250000-TRATAR-PRECIFICACAO
            PERFORM 255000-TRATAR-CONDICAO-NGD
            PERFORM 260000-TRATAR-ANEXO-OPR
            PERFORM 265000-TRATAR-INSOLVENCIA
            PERFORM 266000-TRATAR-CLIENTE-GRTR
            PERFORM 270000-TRATAR-PREVISAO-GRT
            PERFORM 275000-TRATAR-CLIENTE-OPR
            PERFORM 280000-TRATAR-NOTA-CLASSIF
            PERFORM 285000-TRATAR-ESTADO-ORDEM
            PERFORM 286000-TRATAR-CLIENTE-COMPRA
            PERFORM 287000-TRATAR-AUTORIZACAO
            PERFORM 288000-TRATAR-ORDEM-NEGOCIACAO
            PERFORM 290000-TRATAR-ITEM-EXCECAO-OPR
            PERFORM 291000-TRATAR-EXCECAO-OPR
            PERFORM 295000-TRATAR-CONTATO-EXTERNO
            PERFORM 296000-TRATAR-NOTA-VINCULADA
            PERFORM 297000-TRATAR-ESTADO-VERSAO
            PERFORM 298000-TRATAR-CALCULO-IN-OPR
            PERFORM 299000-TRATAR-JUSTIFICATIVAS

            PERFORM 300000-EXCLUIR-OPERACAO

           .
       200000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       201000-VER-MAIOR-ESTADO-VRS     SECTION.
      *----------------------------------------------------------------*
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSPE03-RPST

      *    Movimentar os campos de entrada
           MOVE SGTSPE03               TO NM-PGM

           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSPE03-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSPE03-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSPE03-NR-VRS-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO
           .
       201000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       202000-CONSULTAR-OPERACAO       SECTION.
      * Operação Estruturada de Mercado de Capitais.
      *----------------------------------------------------------------*
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTST005-DADOS

      *    Movimentar os campos de entrada
           SET  SGTST005-CONSULTAR     TO TRUE

           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTST005-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTST005-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTST005-NR-VRS-OPR-ETRD

           PERFORM 920000-CHAMAR-SGTST005
           .
       202000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       205000-TRATAR-OPR-PAGA          SECTION.
      * Operação paga por outra operação.
      *----------------------------------------------------------------*
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL507-RQSC
                                       SGTSL507-QT-OCR-TAB
                                       SGTSL507-IC-TAB
                                       SGTSL507-IN-CTNC-PSQ

      *    Movimentar os campos de entrada da lista
           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL507-CD-CTRA-OPR-PGDR
                                          SGTSL507-CD-CTRA-OPR-VCLD-E
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL507-CD-OPR-PGDR
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL507-NR-VRS-OPR-PGDR

      *  Trata lista
           PERFORM UNTIL SGTSL507-IN-CTNC-PSQ  EQUAL  'N'
              MOVE SGTSL507               TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 205100-EXCLUIR-OPR-PG-OTR-OPR
                         VARYING SGTSL507-IC-TAB FROM 1 BY 1
                           UNTIL SGTSL507-IC-TAB > SGTSL507-QT-OCR-TAB

      *  Campos de rechamada da lista
              IF  SGTSL507-QT-OCR-TAB > ZEROS
                 MOVE SGTSL507-CD-CTRA-OPR-VCLD (SGTSL507-QT-OCR-TAB)
                                       TO SGTSL507-CD-CTRA-OPR-VCLD-E
                 MOVE SGTSL507-CD-OPR-ETRD-VCLD (SGTSL507-QT-OCR-TAB)
                                       TO SGTSL507-CD-OPR-ETRD-VCLD-E
                 MOVE SGTSL507-NR-VRS-OPR-VCLD (SGTSL507-QT-OCR-TAB)
                                       TO SGTSL507-NR-VRS-OPR-VCLD-E
              END-IF
           END-PERFORM

           .
       205000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       205100-EXCLUIR-OPR-PG-OTR-OPR   SECTION.
      *----------------------------------------------------------------*
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE071-DADOS

      *    Movimentar os campos de entrada
           MOVE SGTSE071               TO NM-PGM

           MOVE SGTSL507-CD-CTRA-OPR-PGDR
                                       TO SGTSE071-CD-CTRA-OPR-PGDR
           MOVE SGTSL507-CD-OPR-PGDR
                                       TO SGTSE071-CD-OPR-PGDR
           MOVE SGTSL507-NR-VRS-OPR-PGDR
                                       TO SGTSE071-NR-VRS-OPR-PGDR
           MOVE SGTSL507-CD-CTRA-OPR-VCLD (SGTSL507-IC-TAB)
                                       TO SGTSE071-CD-CTRA-OPR-VCLD
           MOVE SGTSL507-CD-OPR-ETRD-VCLD (SGTSL507-IC-TAB)
                                       TO SGTSE071-CD-OPR-ETRD-VCLD
           MOVE SGTSL507-NR-VRS-OPR-VCLD (SGTSL507-IC-TAB)
                                       TO SGTSE071-NR-VRS-OPR-VCLD
      *    Chamar programa de exclusão
           PERFORM 900000-EXECUTAR-MODULO

           .
       205100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       210000-TRATAR-INDICADOR-OPR     SECTION.
      * Indicador da operação estruturada de mercado de capitais.
      *----------------------------------------------------------------*
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL063-RQSC
                                       SGTSL063-QT-OCR-TAB
                                       SGTSL063-IC-TAB
                                       SGTSL063-IN-CTNC-PSQ

      *    Movimentar os campos de entrada da lista
      *    Chama lista total da tabela IN_OPR_ETRD

           MOVE 2                      TO SGTSL063-CD-ACAO

           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL063-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL063-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL063-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL063-IN-CTNC-PSQ  EQUAL  'N'
              MOVE SGTSL063            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 210100-EXCLUIR-IN-OPR-ETRD
                         VARYING SGTSL063-IC-TAB FROM 1 BY 1
                           UNTIL SGTSL063-IC-TAB > SGTSL063-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL063-QT-OCR-TAB  > ZEROS
                  MOVE SGTSL063-CD-TIP-IN (SGTSL063-QT-OCR-TAB)
                                       TO SGTSL063-CD-TIP-IN-E
                  MOVE SGTSL063-NR-IN-OPR-ETRD (SGTSL063-QT-OCR-TAB)
                                       TO SGTSL063-NR-IN-OPR-ETRD-E
              END-IF
           END-PERFORM

           .
       210000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       210100-EXCLUIR-IN-OPR-ETRD      SECTION.
      *----------------------------------------------------------------*
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE063-DADOS

           MOVE SGTSE063               TO NM-PGM

      *    Movimentar os campos de entrada

           MOVE SGTSL063-CD-CTRA-TIT-VL
                                       TO SGTSE063-CD-CTRA-TIT-VL
           MOVE SGTSL063-CD-OPR-ETRD-MCP
                                       TO SGTSE063-CD-OPR-ETRD-MCP
           MOVE SGTSL063-NR-VRS-OPR-ETRD
                                       TO SGTSE063-NR-VRS-OPR-ETRD
           MOVE SGTSL063-CD-TIP-IN (SGTSL063-IC-TAB)
                                       TO SGTSE063-CD-TIP-IN
           MOVE SGTSL063-NR-IN-OPR-ETRD (SGTSL063-IC-TAB)
                                       TO SGTSE063-NR-IN-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO
           .
       210100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       215000-TRATAR-TARIFA-OPR        SECTION.
      * Tarifa da Operação Estruturada.
      *----------------------------------------------------------------*
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL084-RQSC
                                       SGTSL084-QT-OCR-TAB
                                       SGTSL084-IC-TAB
                                       SGTSL084-IN-CNTO-PSQ

      *    Movimentar os campos de entrada da lista
           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL084-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL084-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL084-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL084-IN-CNTO-PSQ  EQUAL  'N'
              MOVE SGTSL084            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 215100-EXCLUIR-TARF-OPR-ETRD
                         VARYING SGTSL084-IC-TAB FROM 1 BY 1
                           UNTIL SGTSL084-IC-TAB > SGTSL084-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL084-QT-OCR-TAB  > ZEROS
                  MOVE SGTSL084-CD-TIP-TARF (SGTSL084-QT-OCR-TAB)
                                       TO SGTSL084-CD-TIP-TARF-E
              END-IF
           END-PERFORM

           .
       215000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       215100-EXCLUIR-TARF-OPR-ETRD    SECTION.
      *----------------------------------------------------------------*
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE084-DADOS

           MOVE SGTSE084               TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTSL084-CD-CTRA-TIT-VL
                                       TO SGTSE084-CD-CTRA-TIT-VL
           MOVE SGTSL084-CD-OPR-ETRD-MCP
                                       TO SGTSE084-CD-OPR-ETRD-MCP
           MOVE SGTSL084-NR-VRS-OPR-ETRD
                                       TO SGTSE084-NR-VRS-OPR-ETRD
           MOVE SGTSL084-CD-TIP-TARF (SGTSL084-IC-TAB)
                                       TO SGTSE084-CD-TIP-TARF

           PERFORM 900000-EXECUTAR-MODULO
           .
       215100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       220000-TRATAR-REPACTUACAO       SECTION.
      * Repactuação programada da operaçao estruturada.
      *----------------------------------------------------------------*
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL061-RQSC
                                       SGTSL061-QT-OCR-TAB
                                       SGTSL061-IC-TAB
                                       SGTSL061-IN-CNTO-PSQ

      *    Movimentar os campos de entrada da lista
           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL061-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL061-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL061-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL061-IN-CNTO-PSQ  EQUAL  'N'
              MOVE SGTSL061            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 220100-EXCLUIR-RPAC-PGMD-OPR
                        VARYING SGTSL061-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL061-IC-TAB > SGTSL061-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL061-QT-OCR-TAB  > ZEROS
                  MOVE SGTSL061-DT-RPAC-PGMD (SGTSL061-QT-OCR-TAB)
                                       TO SGTSL061-DT-RPAC-ULT
              END-IF
           END-PERFORM

           .
       220000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       220100-EXCLUIR-RPAC-PGMD-OPR    SECTION.
      *----------------------------------------------------------------*
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE061-DADOS

           MOVE SGTSE061               TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTSL061-CD-CTRA-TIT-VL
                                       TO SGTSE061-CD-CTRA-TIT-VL
           MOVE SGTSL061-CD-OPR-ETRD-MCP
                                       TO SGTSE061-CD-OPR-ETRD-MCP
           MOVE SGTSL061-NR-VRS-OPR-ETRD
                                       TO SGTSE061-NR-VRS-OPR-ETRD

           MOVE CORR SGTSL061-DT-RPAC-PGMD-R(SGTSL061-IC-TAB)
                                       TO DT-FMT-R

           MOVE  DT-FMT                TO SGTSE061-DT-RPAC-PGMD


           PERFORM 900000-EXECUTAR-MODULO
           .
       220100-SAI.
           EXIT.
      *----------------------------------------------------------------*
       225000-TRATAR-RECEB-REALIZADO   SECTION.
      * Recebimento realizado da operação.
      *----------------------------------------------------------------*
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

            INITIALIZE                 SGTSL087-RQSC
                                       SGTSL087-QT-OCR
                                       SGTSL087-IC-TAB
                                       SGTSL087-IN-CTNC-PSQ

      *    Movimentar os campos de entrada da lista
           SET  SGTSL087-LISTA-POR-OPR TO TRUE
      *
           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL087-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL087-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL087-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL087-IN-CTNC-PSQ  EQUAL  'N'
              MOVE SGTSL087            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 225100-EXCLUIR-RCBT-RLZD-OPR
                        VARYING SGTSL087-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL087-IC-TAB > SGTSL087-QT-OCR
      * Campos de rechamada da lista
              IF  SGTSL087-QT-OCR  > ZEROS

                MOVE SGTSL087-NR-SEQL-RCBT-PRVT-S (SGTSL087-QT-OCR )
                                     TO SGTSL087-NR-SEQL-RCBT-PRVT-ULT
                MOVE SGTSL087-CD-TIP-RCBT-PRVT (SGTSL087-QT-OCR )
                                       TO SGTSL087-CD-TIP-RCBT-PRVT-ULT
                MOVE SGTSL087-DT-RCBT-PRVT (SGTSL087-QT-OCR )
                                       TO SGTSL087-DT-RCBT-PRVT-ULT
                MOVE SGTSL087-TS-REG-PGTO  (SGTSL087-QT-OCR )
                                       TO SGTSL087-TS-REG-PGTO-ULT
              END-IF
           END-PERFORM

           .
       225000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       225100-EXCLUIR-RCBT-RLZD-OPR    SECTION.
      *----------------------------------------------------------------*
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE087-DADOS

           MOVE SGTSE087               TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTSL087-CD-CTRA-TIT-VL
                                       TO SGTSE087-CD-CTRA-TIT-VL
           MOVE SGTSL087-CD-OPR-ETRD-MCP
                                       TO SGTSE087-CD-OPR-ETRD-MCP
           MOVE SGTSL087-NR-VRS-OPR-ETRD
                                       TO SGTSE087-NR-VRS-OPR-ETRD
           MOVE SGTSL087-NR-SEQL-RCBT-PRVT-S (SGTSL087-IC-TAB)
                                       TO SGTSE087-NR-SEQL-RCBT-PRVT
           MOVE SGTSL087-CD-TIP-RCBT-PRVT  (SGTSL087-IC-TAB)
                                       TO SGTSE087-CD-TIP-RCBT-PRVT
           MOVE SGTSL087-DT-RCBT-PRVT (SGTSL087-IC-TAB)
                                       TO SGTSE087-DT-RCBT-PRVT
           MOVE SGTSL087-CD-TIT-OPR-ETRD-S (SGTSL087-IC-TAB)
                                       TO SGTSE087-CD-TIT-OPR-ETRD
           MOVE SGTSL087-NR-DVS-TIT-S (SGTSL087-IC-TAB)
                                       TO SGTSE087-NR-DVS-TIT
           MOVE SGTSL087-TS-REG-PGTO (SGTSL087-IC-TAB)
                                       TO SGTSE087-TS-REG-PGTO

           PERFORM 900000-EXECUTAR-MODULO

           .
       225100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       230000-TRATAR-RECEB-PREVISTO    SECTION.
      * Repactuação programada da operaçao estruturada.
      *----------------------------------------------------------------*
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL070-RQSC
                                       SGTSL070-QT-OCR
                                       SGTSL070-IC-TAB
                                       SGTSL070-IN-CTNC-PSQ

      *    Movimentar os campos de entrada da lista
           SET SGTSL070-LISTA-POR-VERSAO TO TRUE
      *
           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL070-E-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL070-E-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL070-E-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL070-IN-CTNC-PSQ  EQUAL  'N'
              MOVE SGTSL070            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 230100-EXCLUIR-RCBT-PRVT-OPR
                        VARYING SGTSL070-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL070-IC-TAB > SGTSL070-QT-OCR

      * Campos de rechamada da lista
              IF  SGTSL070-QT-OCR  > ZEROS

                  MOVE SGTSL070-CD-TIP-RCBT-PRVT (SGTSL070-QT-OCR)
                                       TO SGTSL070-E-CD-TIP-RCBT-PRVT
                  MOVE SGTSL070-DT-RCBT-PRVT (SGTSL070-QT-OCR)
                                       TO SGTSL070-DT-RCBT-PRVT-ULT
              END-IF
           END-PERFORM

           .
       230000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       230100-EXCLUIR-RCBT-PRVT-OPR    SECTION.
      *----------------------------------------------------------------*
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE070-DADOS

           MOVE SGTSE070               TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTSL070-E-CD-CTRA-TIT-VL
                                       TO SGTSE070-CD-CTRA-TIT-VL
           MOVE SGTSL070-E-CD-OPR-ETRD-MCP
                                       TO SGTSE070-CD-OPR-ETRD-MCP
           MOVE SGTSL070-E-NR-VRS-OPR-ETRD
                                       TO SGTSE070-NR-VRS-OPR-ETRD
           MOVE SGTSL070-NR-SEQL-RCBT-PRVT (SGTSL070-IC-TAB)
                                       TO SGTSE070-NR-SEQL-RCBT-PRVT
           MOVE SGTSL070-CD-TIP-RCBT-PRVT  (SGTSL070-IC-TAB)
                                       TO SGTSE070-CD-TIP-RCBT-PRVT
           MOVE SGTSL070-DT-RCBT-PRVT (SGTSL070-IC-TAB)
                                       TO SGTSE070-DT-RCBT-PRVT

           PERFORM 900000-EXECUTAR-MODULO

           .
       230100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       235000-TRATAR-BANCO             SECTION.
      * Banco participante da compensação.
      *----------------------------------------------------------------*
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL064-RQSC
                                       SGTSL064-QT-OCR-TAB
                                       SGTSL064-IC-TAB
                                       SGTSL064-IN-CNTO-PSQ

      *    Movimentar os campos de entrada da lista
           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL064-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL064-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL064-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL064-IN-CNTO-PSQ  EQUAL  'N'
              MOVE SGTSL064            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 235100-EXCLUIR-BCO-PCT-OPR
                        VARYING SGTSL064-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL064-IC-TAB > SGTSL064-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL064-QT-OCR-TAB  > ZEROS

                  MOVE SGTSL064-CD-TIP-PRTC-S (SGTSL064-QT-OCR-TAB)
                                       TO SGTSL064-CD-ULT-TIP-PRTC
                  MOVE SGTSL064-CD-BCO-PCT (SGTSL064-QT-OCR-TAB)
                                       TO SGTSL064-CD-ULT-BCO-PCT
              END-IF
           END-PERFORM

           .
       235000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       235100-EXCLUIR-BCO-PCT-OPR      SECTION.
      *----------------------------------------------------------------*
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE064-DADOS

           MOVE SGTSE064               TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTSL064-CD-CTRA-TIT-VL
                                       TO SGTSE064-CD-CTRA-TIT-VL
           MOVE SGTSL064-CD-OPR-ETRD-MCP
                                       TO SGTSE064-CD-OPR-ETRD-MCP
           MOVE SGTSL064-NR-VRS-OPR-ETRD
                                       TO SGTSE064-NR-VRS-OPR-ETRD
           MOVE SGTSL064-CD-BCO-PCT (SGTSL064-IC-TAB)
                                       TO SGTSE064-CD-BCO-PCT
           MOVE SGTSL064-CD-TIP-PRTC-S (SGTSL064-IC-TAB)
                                       TO SGTSE064-CD-TIP-PRTC

           PERFORM 900000-EXECUTAR-MODULO

           .
       235100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       240000-TRATAR-ESTADO-OPR        SECTION.
      * Estado da operação estruturada.
      *----------------------------------------------------------------*

           MOVE 0020                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL009-RQSC
                                       SGTSL009-QT-OCR-TAB
                                       SGTSL009-IC-TAB
                                       SGTSL009-IN-CNTO-PSQ

      *    Movimentar os campos de entrada da lista
           SET SGTSL009-LISTA-POR-OPR TO TRUE
           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL009-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL009-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL009-NR-VRS-OPR-ETRD
           MOVE '1901-01-01-00.00.00.00000'
                                       TO SGTSL009-TS-EST-OPR-ETRD-ULT


           PERFORM UNTIL SGTSL009-IN-CNTO-PSQ  EQUAL  'N'
              MOVE SGTSL009            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 240100-EXCLUIR-EST-OPR-ETRD
                        VARYING SGTSL009-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL009-IC-TAB > SGTSL009-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL009-QT-OCR-TAB  > ZEROS

                  MOVE SGTSL009-TS-EST-OPR-ETRD (SGTSL009-QT-OCR-TAB)
                                       TO SGTSL009-TS-EST-OPR-ETRD-ULT
              END-IF
           END-PERFORM

           .
       240000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       240100-EXCLUIR-EST-OPR-ETRD     SECTION.
      *----------------------------------------------------------------*
           MOVE 0021                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTST009-DADOS


           SET SGTST009-EXCLUIR        TO TRUE

      *    Movimentar os campos de entrada
           MOVE KOPRD-CD-OPRD          TO SGTST009-CD-USU-RSP-ATL

           MOVE SGTSL009-CD-CTRA-TIT-VL
                                       TO SGTST009-CD-CTRA-TIT-VL
           MOVE SGTSL009-CD-OPR-ETRD-MCP
                                       TO SGTST009-CD-OPR-ETRD-MCP
           MOVE SGTSL009-NR-VRS-OPR-ETRD
                                       TO SGTST009-NR-VRS-OPR-ETRD
           MOVE SGTSL009-TS-EST-OPR-ETRD (SGTSL009-IC-TAB)
                                       TO SGTST009-TS-EST-OPR-ETRD

      *  Via commarea
           PERFORM 920002-CHAMAR-SGTST009

           .
       240100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       245000-TRATAR-REMUNERACAO-OPR   SECTION.
      * Remuneração da operação estruturada.
      *----------------------------------------------------------------*
           MOVE 0022                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL011-RQSC
                                       SGTSL011-QT-OCR-TAB
                                       SGTSL011-IC-TAB
                                       SGTSL011-IN-CTNC-PSQ

      *    Movimentar os campos de entrada da lista
           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL011-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL011-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL011-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL011-IN-CTNC-PSQ  EQUAL  'N'
              MOVE SGTSL011            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 245100-EXCLUIR-RMNC-OPR-ETRD
                        VARYING SGTSL011-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL011-IC-TAB > SGTSL011-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL011-QT-OCR-TAB  > ZEROS
                  MOVE SGTSL011-DT-INC-TAXA-RMNC (SGTSL011-QT-OCR-TAB)
                                       TO SGTSL011-DT-INC-TAXA-ULT
              END-IF
           END-PERFORM

           .
       245000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       245100-EXCLUIR-RMNC-OPR-ETRD    SECTION.
      *----------------------------------------------------------------*
           MOVE 0023                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE011-DADOS

           MOVE SGTSE011               TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTSL011-CD-CTRA-TIT-VL
                                       TO SGTSE011-CD-CTRA-TIT-VL
           MOVE SGTSL011-CD-OPR-ETRD-MCP
                                       TO SGTSE011-CD-OPR-ETRD-MCP
           MOVE SGTSL011-NR-VRS-OPR-ETRD
                                       TO SGTSE011-NR-VRS-OPR-ETRD

           MOVE CORR SGTSL011-DT-INC-TAXA-RMNC-R (SGTSL011-IC-TAB)
                                       TO DT-FMT-R
           MOVE  DT-FMT                TO SGTSE011-DT-INC-TAXA-RMNC

           PERFORM 900000-EXECUTAR-MODULO

           .
       245100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       250000-TRATAR-PRECIFICACAO       SECTION.
      * Precificação de taxa da Operação.
      *----------------------------------------------------------------*
           MOVE 0024                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL068-RQSC
                                       SGTSL068-QT-OCR-TAB
                                       SGTSL068-IC-TAB
                                       SGTSL068-IN-CNTO-PSQ

      *    Movimentar os campos de entrada da lista
           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL068-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL068-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL068-NR-VRS-OPR-ETRD


           PERFORM UNTIL SGTSL068-IN-CNTO-PSQ  EQUAL  'N'
              MOVE SGTSL068            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 250100-EXCLUIR-PREF-TAXA-OPR
                        VARYING SGTSL068-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL068-IC-TAB > SGTSL068-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL068-QT-OCR-TAB  > ZEROS
                  MOVE SGTSL068-CD-TIP-TAXA-S (SGTSL068-QT-OCR-TAB)
                                      TO SGTSL068-CD-ULT-TIP-TAXA
                  MOVE SGTSL068-DT-INC-CND-TAXA (SGTSL068-QT-OCR-TAB)
                                      TO SGTSL068-DT-ULT-INC-VLD
              END-IF
           END-PERFORM

           .
       250000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       250100-EXCLUIR-PREF-TAXA-OPR    SECTION.
      *----------------------------------------------------------------*
           MOVE 0025                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE068-DADOS

           MOVE SGTSE068               TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTSL068-CD-CTRA-TIT-VL
                                       TO SGTSE068-CD-CTRA-TIT-VL
           MOVE SGTSL068-CD-OPR-ETRD-MCP
                                       TO SGTSE068-CD-OPR-ETRD-MCP
           MOVE SGTSL068-NR-VRS-OPR-ETRD
                                       TO SGTSE068-NR-VRS-OPR-ETRD
           MOVE SGTSL068-CD-TIP-TAXA-S (SGTSL068-IC-TAB)
                                       TO SGTSE068-CD-TIP-TAXA
           MOVE CORR SGTSL068-DT-INC-CND-TAXA-R (SGTSL068-IC-TAB)
                                       TO DT-FMT-R
           MOVE  DT-FMT                TO SGTSE068-DT-INC-CND-TAXA

           PERFORM 900000-EXECUTAR-MODULO

           .
       250100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       255000-TRATAR-CONDICAO-NGD       SECTION.
      * Taxa de quebra de fundo da operação para condição negociada.
      *----------------------------------------------------------------*

           MOVE 0026                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL079-RQSC
                                       SGTSL079-QT-OCR-TAB
                                       SGTSL079-IC-TAB
                                       SGTSL079-IN-CNTO-PSQ

      *    Movimentar os campos de entrada da lista
           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL079-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL079-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL079-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL079-IN-CNTO-PSQ  EQUAL  'N'
              MOVE SGTSL079           TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 255100-EXCLUIR-CND-NEGD-OPR
                        VARYING SGTSL079-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL079-IC-TAB > SGTSL079-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL079-QT-OCR-TAB  > ZEROS

                 MOVE SGTSL079-CD-TIP-CND-NEGD-S (SGTSL079-QT-OCR-TAB)
                                       TO SGTSL079-CD-ULT-TIP-CND-NEGD
                 MOVE SGTSL079-DT-INC-VLD-CND-S (SGTSL079-QT-OCR-TAB)
                                       TO SGTSL079-DT-INC-VLD-CND
              END-IF
           END-PERFORM

           .
       250000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       255100-EXCLUIR-CND-NEGD-OPR     SECTION.
      *----------------------------------------------------------------*
           MOVE 0027                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE079-DADOS

           MOVE SGTSE079               TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTSL079-CD-CTRA-TIT-VL
                                       TO SGTSE079-CD-CTRA-TIT-VL
           MOVE SGTSL079-CD-OPR-ETRD-MCP
                                       TO SGTSE079-CD-OPR-ETRD-MCP
           MOVE SGTSL079-NR-VRS-OPR-ETRD
                                       TO SGTSE079-NR-VRS-OPR-ETRD
           MOVE SGTSL079-CD-TIP-CND-NEGD-S (SGTSL079-IC-TAB)
                                       TO SGTSE079-CD-TIP-CND-NEGD
           MOVE SGTSL079-DT-INC-VLD-CND-S (SGTSL079-IC-TAB)
                                       TO SGTSE079-DT-INC-VLD-CND

           PERFORM 900000-EXECUTAR-MODULO

           .
       255100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       260000-TRATAR-ANEXO-OPR         SECTION.
      * Anexo da operação estruturada.
      *----------------------------------------------------------------*
           MOVE 0028                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL060-RQSC
                                       SGTSL060-QT-OCR-TAB
                                       SGTSL060-IC-TAB
                                       SGTSL060-IN-CTNC-PSQ

      *    Movimentar os campos de entrada da lista
           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL060-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL060-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL060-NR-VRS-OPR-ETRD
           MOVE ZEROS                  TO SGTSL060-CD-ARQ-OPR-ULT
                                          SGTSL060-CD-TIP-ARQ

           PERFORM UNTIL SGTSL060-IN-CTNC-PSQ  EQUAL  'N'
              MOVE SGTSL060           TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 260100-EXCLUIR-ANX-OPR-ETRD
                        VARYING SGTSL060-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL060-IC-TAB > SGTSL060-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL060-QT-OCR-TAB  > ZEROS
                  MOVE SGTSL060-CD-ARQ-OPR-ETRD (SGTSL060-QT-OCR-TAB)
                                       TO SGTSL060-CD-ARQ-OPR-ULT
              END-IF
           END-PERFORM

           .
       260000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       260100-EXCLUIR-ANX-OPR-ETRD     SECTION.
      *----------------------------------------------------------------*
           MOVE 0029                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE060-DADOS

           MOVE SGTSE060               TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTSL060-CD-CTRA-TIT-VL
                                       TO SGTSE060-CD-CTRA-TIT-VL
           MOVE SGTSL060-CD-OPR-ETRD-MCP
                                       TO SGTSE060-CD-OPR-ETRD-MCP
           MOVE SGTSL060-NR-VRS-OPR-ETRD
                                       TO SGTSE060-NR-VRS-OPR-ETRD
           MOVE SGTSL060-CD-ARQ-OPR-ETRD (SGTSL060-IC-TAB)
                                       TO SGTSE060-CD-ARQ-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO

           .
       260100-SAI.
           EXIT.


      *----------------------------------------------------------------*
       265000-TRATAR-INSOLVENCIA       SECTION.
      * Insolvência da operação estruturada.
      *----------------------------------------------------------------*
           MOVE 0030                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL080-RQSC
                                       SGTSL080-QT-OCR-TAB
                                       SGTSL080-IC-TAB
                                       SGTSL080-IN-CTNC-PSQ

      *    Movimentar os campos de entrada da lista
           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL080-E-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL080-E-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL080-E-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL080-IN-CTNC-PSQ  EQUAL  'N'
              MOVE SGTSL080           TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 265100-EXCLUIR-ISLC-OPR-ETRD
                        VARYING SGTSL080-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL080-IC-TAB > SGTSL080-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL080-QT-OCR-TAB  > ZEROS
                  MOVE SGTSL080-NR-ISLC-OPR (SGTSL080-QT-OCR-TAB)
                                       TO SGTSL080-E-NR-ISLC-OPR
              END-IF
           END-PERFORM

           .
       265000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       265100-EXCLUIR-ISLC-OPR-ETRD    SECTION.
      *----------------------------------------------------------------*
           MOVE 0031                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE080-DADOS

           MOVE SGTSE080               TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTSL080-E-CD-CTRA-TIT-VL
                                       TO SGTSE080-CD-CTRA-TIT-VL
           MOVE SGTSL080-E-CD-OPR-ETRD-MCP
                                       TO SGTSE080-CD-OPR-ETRD-MCP
           MOVE SGTSL080-E-NR-VRS-OPR-ETRD
                                       TO SGTSE080-NR-VRS-OPR-ETRD
           MOVE SGTSL080-NR-ISLC-OPR (SGTSL080-IC-TAB)
                                       TO SGTSE080-NR-ISLC-OPR

           PERFORM 900000-EXECUTAR-MODULO

           .
       265100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       266000-TRATAR-CLIENTE-GRTR      SECTION.
      * Cliente garantidor (tab filha da PRV_CMPS_GRT).
      *----------------------------------------------------------------*
           MOVE 0032                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL082-RQSC
                                       SGTSL082-QT-OCR-TAB
                                       SGTSL082-IC-TAB
                                       SGTSL082-IN-CTNC-PSQ

      *    Movimentar os campos de entrada da lista

           MOVE 2                      TO SGTSL082-CD-TIP-SRVC

           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL082-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL082-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL082-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL082-IN-CTNC-PSQ  EQUAL  'N'
              MOVE SGTSL082           TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 266100-EXCLUIR-CLI-GRT
                        VARYING SGTSL082-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL082-IC-TAB > SGTSL082-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL082-QT-OCR-TAB  > ZEROS
                 MOVE SGTSL082-NR-PRV-CMPS-GRT-S (SGTSL082-QT-OCR-TAB)
                                       TO SGTSL082-NR-PRV-CMPS-GRT
                  MOVE SGTSL082-CD-CLI (SGTSL082-QT-OCR-TAB)
                                       TO SGTSL082-CD-ULT-CLI
              END-IF
           END-PERFORM

           .
       266000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       266100-EXCLUIR-CLI-GRT          SECTION.
      *----------------------------------------------------------------*
           MOVE 0033                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE082-DADOS

           MOVE SGTSE082               TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTSL082-CD-CTRA-TIT-VL
                                       TO SGTSE082-CD-CTRA-TIT-VL
           MOVE SGTSL082-CD-OPR-ETRD-MCP
                                       TO SGTSE082-CD-OPR-ETRD-MCP
           MOVE SGTSL082-NR-VRS-OPR-ETRD
                                       TO SGTSE082-NR-VRS-OPR-ETRD
           MOVE SGTSL082-NR-PRV-CMPS-GRT-S (SGTSL082-IC-TAB)
                                       TO SGTSE082-NR-PRV-CMPS-GRT
           MOVE SGTSL082-CD-CLI (SGTSL082-IC-TAB)
                                       TO SGTSE082-CD-CLI

           PERFORM 900000-EXECUTAR-MODULO

           .
       266100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       270000-TRATAR-PREVISAO-GRT      SECTION.
      * Previsão de constituição de garantia.
      *----------------------------------------------------------------*
           MOVE 0034                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL081-RQSC
                                       SGTSL081-QT-OCR-TAB
                                       SGTSL081-IC-TAB
                                       SGTSL081-IN-CTNC-PSQ

      *    Movimentar os campos de entrada da lista

           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL081-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL081-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL081-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL081-IN-CTNC-PSQ  EQUAL  'N'
              MOVE SGTSL081           TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 270100-EXCLUIR-PRV-CMPS-GRT
                        VARYING SGTSL081-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL081-IC-TAB > SGTSL081-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL081-QT-OCR-TAB  > ZEROS
                  MOVE SGTSL081-NR-PRV-CMPS-GRT (SGTSL081-QT-OCR-TAB)
                                       TO SGTSL081-NR-ULT-PRV-CMPS-GRT
              END-IF
           END-PERFORM

           .
       270000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       270100-EXCLUIR-PRV-CMPS-GRT     SECTION.
      *----------------------------------------------------------------*
           MOVE 0035                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE081-DADOS

           MOVE SGTSE081               TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTSL081-CD-CTRA-TIT-VL
                                       TO SGTSE081-CD-CTRA-TIT-VL
           MOVE SGTSL081-CD-OPR-ETRD-MCP
                                       TO SGTSE081-CD-OPR-ETRD-MCP
           MOVE SGTSL081-NR-VRS-OPR-ETRD
                                       TO SGTSE081-NR-VRS-OPR-ETRD
           MOVE SGTSL081-NR-PRV-CMPS-GRT (SGTSL081-IC-TAB)
                                       TO SGTSE081-NR-PRV-CMPS-GRT

           PERFORM 900000-EXECUTAR-MODULO

           .
       270100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       275000-TRATAR-CLIENTE-OPR       SECTION.
      * Cliente da operação estruturada.
      *----------------------------------------------------------------*
           MOVE 0036                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL006-CHAVE
                                       SGTSL006-QT-OCR-TAB
                                       SGTSL006-IC-TAB
                                       SGTSL006-IN-CNTO-PSQ

      *    Movimentar os campos de entrada da lista - via commarea

           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL006-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL006-ULT-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL006-ULT-VRS-OPR-ETRD
           SET SGTSL006-CLI            TO TRUE

           PERFORM UNTIL SGTSL006-IN-CNTO-PSQ  EQUAL  'N'
              MOVE SGTSL006            TO NM-PGM
              PERFORM 920003-CHAMAR-SGTSL006
              PERFORM 275100-EXCLUIR-CLI-OPR-ETRD
                        VARYING SGTSL006-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL006-IC-TAB > SGTSL006-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL006-QT-OCR-TAB  > ZEROS
                  MOVE SGTSL006-CD-TIP-FUC-CLI (SGTSL006-QT-OCR-TAB)
                                       TO SGTSL006-ULT-TIP-FUC-CLI
                  MOVE SGTSL006-CD-CLI (SGTSL006-QT-OCR-TAB)
                                       TO SGTSL006-ULT-CD-CLI
                  MOVE SGTSL006-DT-FIM-VLD-FUC (SGTSL006-QT-OCR-TAB)
                                       TO SGTST006-DT-FIM-VLD-FUC
              END-IF
           END-PERFORM

           .
       275000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       275100-EXCLUIR-CLI-OPR-ETRD     SECTION.
      *----------------------------------------------------------------*
           MOVE 0037                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTST006-DADOS

      *    Movimentar os campos de entrada
           SET SGTST006-EXCLUIR        TO TRUE
           MOVE KOPRD-CD-OPRD          TO SGTST006-CD-USU-RSP-ATL
           MOVE SGTSL006-CD-CTRA-TIT-VL
                                       TO SGTST006-CD-CTRA-TIT-VL
           MOVE SGTSL006-ULT-OPR-ETRD-MCP
                                       TO SGTST006-CD-OPR-ETRD-MCP
           MOVE SGTSL006-ULT-VRS-OPR-ETRD
                                       TO SGTST006-NR-VRS-OPR-ETRD
           MOVE SGTSL006-CD-CLI (SGTSL006-IC-TAB)
                                       TO SGTST006-CD-CLI
           MOVE SGTSL006-CD-TIP-FUC-CLI (SGTSL006-IC-TAB)
                                       TO SGTST006-CD-TIP-FUC-CLI
           MOVE SGTSL006-DT-FIM-VLD-FUC (SGTSL006-IC-TAB)
                                       TO SGTST006-DT-FIM-VLD-FUC

           PERFORM 920001-CHAMAR-SGTST006

           .
       275100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       280000-TRATAR-NOTA-CLASSIF      SECTION.
      * Nota de Classificação da Operação.
      *----------------------------------------------------------------*
           MOVE 0038                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL074-RQSC
                                       SGTSL074-QT-OCR-TAB
                                       SGTSL074-IC-TAB
                                       SGTSL074-IN-CTNC-PSQ

      *    Movimentar os campos de entrada da lista
           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL074-E-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL074-E-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL074-E-NR-VRS-OPR-ETRD
           MOVE    1                   TO SGTSL074-CD-TIP-SRVC

           PERFORM UNTIL SGTSL074-IN-CTNC-PSQ  EQUAL  'N'
              MOVE SGTSL074            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 280100-EXCLUIR-NOTA-CLSC-OPR
                        VARYING SGTSL074-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL074-IC-TAB > SGTSL074-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL074-QT-OCR-TAB  > ZEROS
                  MOVE SGTSL074-NR-SEQL-NOTA-OPR (SGTSL074-QT-OCR-TAB)
                                      TO SGTSL074-E-NR-SEQL-NOTA-OPR
              END-IF
           END-PERFORM

           .
       280000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       280100-EXCLUIR-NOTA-CLSC-OPR    SECTION.
      *----------------------------------------------------------------*
           MOVE 0039                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE074-DADOS

           MOVE SGTSE074               TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTSL074-E-CD-CTRA-TIT-VL
                                       TO SGTSE074-CD-CTRA-TIT-VL
           MOVE SGTSL074-E-CD-OPR-ETRD-MCP
                                       TO SGTSE074-CD-OPR-ETRD-MCP
           MOVE SGTSL074-E-NR-VRS-OPR-ETRD
                                       TO SGTSE074-NR-VRS-OPR-ETRD
           MOVE SGTSL074-NR-SEQL-NOTA-OPR (SGTSL074-IC-TAB)
                                       TO SGTSE074-NR-SEQL-NOTA-OPR

           PERFORM 900000-EXECUTAR-MODULO

           .
       280100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       285000-TRATAR-ESTADO-ORDEM      SECTION.
      * Estado da Ordem de Negociação da Operação.
      *----------------------------------------------------------------*
           MOVE 0040                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL078-RQSC
                                       SGTSL078-QT-OCR-TAB
                                       SGTSL078-IC-TAB
                                       SGTSL078-IN-CNTO-PSQ

      *    Movimentar os campos de entrada da lista
           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL078-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL078-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL078-NR-VRS-OPR-ETRD
           MOVE '1901-01-01-00.00.00.00000'
                                       TO SGTSL078-TS-EST-ULT
           SET  SGTSL078-LISTA-TODOS   TO TRUE

           PERFORM UNTIL SGTSL078-IN-CNTO-PSQ  EQUAL  'N'
              MOVE SGTSL078            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 285100-EXCLUIR-EST-ORD-NEGO
                        VARYING SGTSL078-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL078-IC-TAB > SGTSL078-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL078-QT-OCR-TAB  > ZEROS
                  MOVE SGTSL078-NR-SEQL-ORD-NEGO (SGTSL078-QT-OCR-TAB)
                                      TO SGTSL078-NR-SEQL-ORD-ULT
                  MOVE SGTSL078-TS-EST-ORD-NEGO (SGTSL078-QT-OCR-TAB)
                                      TO SGTSL078-TS-EST-ULT
              END-IF
           END-PERFORM

           .
       285000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       285100-EXCLUIR-EST-ORD-NEGO     SECTION.
      *----------------------------------------------------------------*
           MOVE 0041                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE078-DADOS

           MOVE SGTSE078               TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTSL078-CD-CTRA-TIT-VL
                                       TO SGTSE078-CD-CTRA-TIT-VL
           MOVE SGTSL078-CD-OPR-ETRD-MCP
                                       TO SGTSE078-CD-OPR-ETRD-MCP
           MOVE SGTSL078-NR-VRS-OPR-ETRD
                                       TO SGTSE078-NR-VRS-OPR-ETRD
           MOVE SGTSL078-NR-SEQL-ORD-NEGO (SGTSL078-IC-TAB)
                                       TO SGTSE078-NR-SEQL-ORD-NEGO
           MOVE SGTSL078-TS-EST-ORD-NEGO (SGTSL078-IC-TAB)
                                       TO SGTSE078-TS-EST-ORD-NEGO

           PERFORM 900000-EXECUTAR-MODULO

           .
       285100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       286000-TRATAR-CLIENTE-COMPRA    SECTION.
      * Clientes compradores na negociação de venda.
      *----------------------------------------------------------------*
           MOVE 0042                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL076-RQSC
                                       SGTSL076-QT-OCR-TAB
                                       SGTSL076-IC-TAB
                                       SGTSL076-IN-CNTO-PSQ

      *    Movimentar os campos de entrada da lista
           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL076-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL076-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL076-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL076-IN-CNTO-PSQ  EQUAL  'N'
              MOVE SGTSL076            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 286100-EXCLUIR-CLI-VPRD-NEGO
                        VARYING SGTSL076-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL076-IC-TAB > SGTSL076-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL076-QT-OCR-TAB  > ZEROS
                  MOVE SGTSL076-NR-SEQL-ORD-NEGO (SGTSL076-QT-OCR-TAB)
                                      TO SGTSL076-NR-SEQL-ORD-NEGO-ULT
                  MOVE SGTSL076-CD-CLI-CPRD (SGTSL076-QT-OCR-TAB)
                                      TO SGTSL076-CD-CLI-CPRD-ULT
              END-IF
           END-PERFORM

           .
       286000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       286100-EXCLUIR-CLI-VPRD-NEGO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0043                   TO MSG1-CD-SEQL-ERRO

               INITIALIZE                  SGTSE076-DADOS

           MOVE     SGTSE076               TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTSL076-CD-CTRA-TIT-VL
                                       TO SGTSE076-CD-CTRA-TIT-VL
           MOVE SGTSL076-CD-OPR-ETRD-MCP
                                       TO SGTSE076-CD-OPR-ETRD-MCP
           MOVE SGTSL076-NR-VRS-OPR-ETRD
                                       TO SGTSE076-NR-VRS-OPR-ETRD
           MOVE SGTSL076-NR-SEQL-ORD-NEGO (SGTSL076-IC-TAB)
                                       TO SGTSE076-NR-SEQL-ORD-NEGO
           MOVE SGTSL076-CD-CLI-CPRD (SGTSL076-IC-TAB)
                                       TO SGTSE076-CD-CLI-CPRD

           PERFORM 900000-EXECUTAR-MODULO

           .
       286100-SAI.
           EXIT.


      *----------------------------------------------------------------*
       287000-TRATAR-AUTORIZACAO       SECTION.
      * Autorização de negociação da operação estruturada.
      *----------------------------------------------------------------*
           MOVE 0044                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL077-RQSC
                                       SGTSL077-QT-OCR-TAB
                                       SGTSL077-IC-TAB
                                       SGTSL077-IN-CNTO-PSQ

      *    Movimentar os campos de entrada da lista
           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL077-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL077-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL077-NR-VRS-OPR-ETRD
           MOVE '1901-01-01-00.00.00.00000'
                                       TO SGTSL077-TS-AUTZ-ULT

           PERFORM UNTIL SGTSL077-IN-CNTO-PSQ  EQUAL  'N'
              MOVE SGTSL077            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 287100-EXCLUIR-AUTZ-NEGO-OPR
                        VARYING SGTSL077-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL077-IC-TAB > SGTSL077-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL077-QT-OCR-TAB  > ZEROS
                  MOVE SGTSL077-NR-SEQL-ORD-NEGO (SGTSL077-QT-OCR-TAB)
                                      TO SGTSL077-NR-SEQL-ULT
                  MOVE SGTSL077-TS-AUTZ-NEGO (SGTSL077-QT-OCR-TAB)
                                      TO SGTSL077-TS-AUTZ-ULT
              END-IF
           END-PERFORM

           .
       287000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       287100-EXCLUIR-AUTZ-NEGO-OPR    SECTION.
      *----------------------------------------------------------------*
           MOVE 0045                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE077-DADOS

           MOVE SGTSE077               TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTSL077-CD-CTRA-TIT-VL
                                       TO SGTSE077-CD-CTRA-TIT-VL
           MOVE SGTSL077-CD-OPR-ETRD-MCP
                                       TO SGTSE077-CD-OPR-ETRD-MCP
           MOVE SGTSL077-NR-VRS-OPR-ETRD
                                       TO SGTSE077-NR-VRS-OPR-ETRD
           MOVE SGTSL077-NR-SEQL-ORD-NEGO (SGTSL077-IC-TAB)
                                       TO SGTSE077-NR-SEQL-ORD-NEGO
           MOVE SGTSL077-TS-AUTZ-NEGO (SGTSL077-IC-TAB)
                                       TO SGTSE077-TS-AUTZ-NEGO

           PERFORM 900000-EXECUTAR-MODULO

           .
       287100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       288000-TRATAR-ORDEM-NEGOCIACAO  SECTION.
      * Ordem de negociação da operação.
      *----------------------------------------------------------------*
           MOVE 0046                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL075-RQSC
                                       SGTSL075-QT-OCR-TAB
                                       SGTSL075-IC-TAB
                                       SGTSL075-IN-CTNC-PSQ

      *    Movimentar os campos de entrada da lista

           MOVE 1                      TO SGTSL075-CD-ACAO

           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL075-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL075-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL075-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL075-IN-CTNC-PSQ  EQUAL  'N'
              MOVE SGTSL075            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 288100-EXCLUIR-ORD-NEGO-OPR
                        VARYING SGTSL075-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL075-IC-TAB > SGTSL075-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL075-QT-OCR-TAB  > ZEROS
                  MOVE SGTSL075-NR-SEQL-ORD-NEGO (SGTSL075-QT-OCR-TAB)
                                       TO SGTSL075-NR-ULT-SEQL-ORD-NEGO
              END-IF
           END-PERFORM

           .
       288000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       288100-EXCLUIR-ORD-NEGO-OPR     SECTION.
      *----------------------------------------------------------------*
           MOVE 0047                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE075-DADOS

           MOVE SGTSE075               TO NM-PGM

      *    Movimentar os campos de entrada

           MOVE SGTSL075-CD-CTRA-TIT-VL
                                       TO SGTSE075-CD-CTRA-TIT-VL
           MOVE SGTSL075-CD-OPR-ETRD-MCP
                                       TO SGTSE075-CD-OPR-ETRD-MCP
           MOVE SGTSL075-NR-VRS-OPR-ETRD
                                       TO SGTSE075-NR-VRS-OPR-ETRD
           MOVE SGTSL075-NR-SEQL-ORD-NEGO (SGTSL075-IC-TAB)
                                       TO SGTSE075-NR-SEQL-ORD-NEGO

           PERFORM 900000-EXECUTAR-MODULO

           .
       288100-SAI.
           EXIT.

      *----------------------------------------------------------------*
        290000-TRATAR-ITEM-EXCECAO-OPR  SECTION.
      * Item exceção da operação.
      *----------------------------------------------------------------*
           MOVE 0048                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL095-RQSC
                                       SGTSL095-QT-OCR-TAB
                                       SGTSL095-IC-TAB
                                       SGTSL095-IN-CTNC-PSQ
      *    Movimentar os campos de entrada da lista
           MOVE  2                     TO SGTSL095-CD-TIP-SRVC
           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL095-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL095-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL095-NR-VRS-OPR-ETRD
           PERFORM UNTIL SGTSL095-IN-CTNC-PSQ  EQUAL  'N'
              MOVE SGTSL095            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 290100-EXCLUIR-ITEM-EXCO-OPR
                        VARYING SGTSL095-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL095-IC-TAB > SGTSL095-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL095-QT-OCR-TAB  > ZEROS
                 MOVE SGTSL095-NR-SEQL-EXCO-PRFL (SGTSL095-QT-OCR-TAB)
                                      TO SGTSL095-E-NR-SEQL-EXCO-PRFL
                 MOVE SGTSL095-NR-SEQL-ITEM-EXCO (SGTSL095-QT-OCR-TAB)
                                      TO SGTSL095-E-NR-SEQL-ITEM-EXCO
              END-IF
           END-PERFORM

           .
       290100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       290100-EXCLUIR-ITEM-EXCO-OPR    SECTION.
      *----------------------------------------------------------------*
           MOVE 0049                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE095-DADOS

           MOVE SGTSE095               TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTSL095-CD-CTRA-TIT-VL
                                       TO SGTSE095-CD-CTRA-TIT-VL
           MOVE SGTSL095-CD-OPR-ETRD-MCP
                                       TO SGTSE095-CD-OPR-ETRD-MCP
           MOVE SGTSL095-NR-VRS-OPR-ETRD
                                       TO SGTSE095-NR-VRS-OPR-ETRD

           MOVE SGTSL095-NR-SEQL-EXCO-PRFL (SGTSL095-IC-TAB)
                                       TO SGTSE095-NR-SEQL-EXCO-PRFL

           MOVE SGTSL095-NR-SEQL-ITEM-EXCO (SGTSL095-IC-TAB)
                                       TO SGTSE095-NR-SEQL-ITEM-EXCO

           PERFORM 900000-EXECUTAR-MODULO

           .
       291100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       291000-TRATAR-EXCECAO-OPR       SECTION.
      * Exceção da operação do perfil escolhido.
      *----------------------------------------------------------------*
           MOVE 0050                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL083-RQSC
                                       SGTSL083-QT-OCR-TAB
                                       SGTSL083-IC-TAB
                                       SGTSL083-IN-CTNC-PSQ
      *    Movimentar os campos de entrada da lista
           SET  SGTSL083-LISTA-TODOS   TO TRUE
           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL083-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL083-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL083-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL083-IN-CTNC-PSQ  EQUAL  'N'
              MOVE SGTSL083            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 291100-EXCLUIR-EXCO-PRFL-SELD
                        VARYING SGTSL083-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL083-IC-TAB > SGTSL083-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL083-QT-OCR-TAB  > ZEROS
                 MOVE SGTSL083-NR-SEQL-EXCO-PRFL (SGTSL083-QT-OCR-TAB)
                                      TO SGTSL083-E-NR-SEQL-EXCO-PRFL
              END-IF
           END-PERFORM

           .
       291000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       291100-EXCLUIR-EXCO-PRFL-SELD   SECTION.
      *----------------------------------------------------------------*
           MOVE 0051                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE083-DADOS

           MOVE SGTSE083               TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTSL083-CD-CTRA-TIT-VL
                                       TO SGTSE083-CD-CTRA-TIT-VL
           MOVE SGTSL083-CD-OPR-ETRD-MCP
                                       TO SGTSE083-CD-OPR-ETRD-MCP
           MOVE SGTSL083-NR-VRS-OPR-ETRD
                                       TO SGTSE083-NR-VRS-OPR-ETRD

           MOVE SGTSL083-NR-SEQL-EXCO-PRFL (SGTSL083-IC-TAB)
                                       TO SGTSE083-NR-SEQL-EXCO-PRFL

           PERFORM 900000-EXECUTAR-MODULO

           .
       291100-SAI.
           EXIT.


      *----------------------------------------------------------------*
       295000-TRATAR-CONTATO-EXTERNO   SECTION.
      * Contato externo da operação estruturada.
      *----------------------------------------------------------------*
           MOVE 0052                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL035-RQSC
                                       SGTSL035-QT-OCR-TAB
                                       SGTSL035-IC-TAB
                                       SGTSL035-IN-CNTO-PSQ

      *    Movimentar os campos de entrada da lista

           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL035-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL035-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL035-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL035-IN-CNTO-PSQ  EQUAL  'N'
              MOVE SGTSL035            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 295100-EXCLUIR-CTT-EXNO-OPR
                        VARYING SGTSL035-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL035-IC-TAB > SGTSL035-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL035-QT-OCR-TAB  > ZEROS
                  MOVE SGTSL035-S-CD-PSS-CTT-EXNO (SGTSL035-QT-OCR-TAB)
                                       TO SGTSL035-CD-PSS-CTT-EXNO
              END-IF
           END-PERFORM

           .
       295000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       295100-EXCLUIR-CTT-EXNO-OPR     SECTION.
      *----------------------------------------------------------------*
           MOVE 0053                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE035-DADOS

           MOVE SGTSE035               TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTSL035-CD-CTRA-TIT-VL
                                       TO SGTSE035-CD-CTRA-TIT-VL
           MOVE SGTSL035-CD-OPR-ETRD-MCP
                                       TO SGTSE035-CD-OPR-ETRD-MC
           MOVE SGTSL035-NR-VRS-OPR-ETRD
                                       TO SGTSE035-NR-VRS-OPR-ETRD
           MOVE SGTSL035-S-CD-PSS-CTT-EXNO (SGTSL035-IC-TAB)
                                       TO SGTSE035-CD-PSS-CTT-EXNO

           PERFORM 900000-EXECUTAR-MODULO

           .
       295100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       296000-TRATAR-NOTA-VINCULADA    SECTION.
      * Nota tecnica vinculada a operação estruturada.
      *----------------------------------------------------------------*
           MOVE 0054                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL067-RQSC
                                       SGTSL067-QT-OCR-TAB
                                       SGTSL067-IC-TAB
                                       SGTSL067-IN-CTNC-PSQ

      *    Movimentar os campos de entrada da lista
           MOVE 2                      TO SGTSL067-CD-TIP-SRVC
           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL067-E-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL067-E-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL067-E-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL067-IN-CTNC-PSQ  EQUAL  'N'
              MOVE SGTSL067            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 296100-EXCLUIR-NOTA-TCN-VCLD
                        VARYING SGTSL067-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL067-IC-TAB > SGTSL067-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL067-QT-OCR-TAB  > ZEROS
                  MOVE SGTSL067-NR-NOTA-TCN-OPR (SGTSL067-QT-OCR-TAB)
                                       TO SGTSL067-E-NR-NOTA-TCN-OPR
              END-IF
           END-PERFORM

           .
       296000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       296100-EXCLUIR-NOTA-TCN-VCLD    SECTION.
      *----------------------------------------------------------------*
           MOVE 0055                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE067-DADOS

           MOVE SGTSE067               TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTSL067-E-CD-CTRA-TIT-VL
                                       TO SGTSE067-CD-CTRA-TIT-VL
           MOVE SGTSL067-E-CD-OPR-ETRD-MCP
                                       TO SGTSE067-CD-OPR-ETRD-MCP
           MOVE SGTSL067-E-NR-VRS-OPR-ETRD
                                       TO SGTSE067-NR-VRS-OPR-ETRD
           MOVE SGTSL067-NR-NOTA-TCN-OPR (SGTSL067-IC-TAB)
                                       TO SGTSE067-NR-NOTA-TCN-OPR

           PERFORM 900000-EXECUTAR-MODULO

           .
       296100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       297000-TRATAR-ESTADO-VERSAO    SECTION.
      * Estado da versão da operação estruturada.
      *----------------------------------------------------------------*
           MOVE 0056                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL069-RQSC
                                       SGTSL069-QT-OCR-TAB
                                       SGTSL069-IC-TAB
                                       SGTSL069-IN-CNTO-PSQ

      *    Movimentar os campos de entrada da lista

           SET SGTSL069-LISTA-TODOS    TO TRUE
           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL069-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL069-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL069-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL069-IN-CNTO-PSQ  EQUAL  'N'
              MOVE SGTSL069            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 297100-EXCLUIR-ESTADO-VERSAO
                        VARYING SGTSL069-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL069-IC-TAB > SGTSL069-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL069-QT-OCR-TAB  > ZEROS
                  MOVE SGTSL069-TS-EST-VRS (SGTSL069-QT-OCR-TAB)
                                       TO SGTSL069-TS-EST-VRS-PAG
              END-IF
           END-PERFORM

           .
       297000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       297100-EXCLUIR-ESTADO-VERSAO    SECTION.
      *   Exclui dado na tabela EST_VRS_OPR_ETRD
      *----------------------------------------------------------------*
           MOVE 0057                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE069-DADOS

           MOVE SGTSE069               TO NM-PGM

      *    Movimentar os campos de entrada

           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSE069-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSE069-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSE069-NR-VRS-OPR-ETRD
           MOVE SGTSL069-TS-EST-VRS (SGTSL069-IC-TAB)
                                       TO SGTSE069-TS-EST-VRS

           PERFORM 900000-EXECUTAR-MODULO

           .
       297100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       298000-TRATAR-CALCULO-IN-OPR    SECTION.
      * Cálculo Indicador da Operação.
      *----------------------------------------------------------------*
           MOVE 0058                   TO MSG1-CD-SEQL-ERRO
      *
           INITIALIZE                  SGTSL088-RQSC
                                       SGTSL088-QT-OCR-TAB
                                       SGTSL088-IC-TAB
                                       SGTSL088-IN-CTNC-PSQ
      *
      *    Movimentar os campos de entrada da lista
      *
           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL088-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL088-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL088-NR-VRS-OPR-ETRD

           PERFORM UNTIL SGTSL088-IN-CTNC-PSQ  EQUAL  'N'
              MOVE SGTSL088            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 298100-EXCLUIR-IN-OPR
                        VARYING SGTSL088-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL088-IC-TAB > SGTSL088-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL088-QT-OCR-TAB  > ZEROS
                  MOVE SGTSL088-TS-EXEA-CLC(SGTSL088-QT-OCR-TAB)
                                       TO SGTSL088-E-ULT-TS-EST-RET
              END-IF
           END-PERFORM
           .
       298000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       298100-EXCLUIR-IN-OPR           SECTION.
      *   Exclui dado na tabela CLC_IN_OPR_ETRD
      *----------------------------------------------------------------*
           MOVE 0059                   TO MSG1-CD-SEQL-ERRO
      *
           INITIALIZE                  SGTSE088-DADOS
      *
           MOVE SGTSE088               TO NM-PGM
      *
      *    Movimentar os campos de entrada
      *
           MOVE SGTSL088-CD-CTRA-TIT-VL
                                       TO SGTSE088-CD-CTRA-TIT-VL
           MOVE SGTSL088-CD-OPR-ETRD-MCP
                                       TO SGTSE088-CD-OPR-ETRD-MCP
           MOVE SGTSL088-NR-VRS-OPR-ETRD
                                       TO SGTSE088-NR-VRS-OPR-ETRD
           MOVE SGTSL088-TS-EXEA-CLC (SGTSL088-IC-TAB)
                                       TO SGTSE088-TS-EXEA-CLC
      *
           PERFORM 900000-EXECUTAR-MODULO
      *
           .
       298100-SAI.
           EXIT.
      *----------------------------------------------------------------*
       299000-TRATAR-JUSTIFICATIVAS    SECTION.
      * Tabela de justificativas da finalização de cadastro.
      *----------------------------------------------------------------*
           MOVE 0060                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL117-RQSC
                                       SGTSL117-QT-OCR-TAB
                                       SGTSL117-IC-TAB
                                       SGTSL117-IN-CTNC-PSQ

      *    Movimentar os campos de entrada da lista

           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTSL117-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTSL117-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTSL117-NR-VRS-OPR-ETRD
           MOVE 1                      TO SGTSL117-CD-TIP-SRVC

           PERFORM UNTIL SGTSL117-IN-CTNC-PSQ  EQUAL  'N'
              MOVE SGTSL117            TO NM-PGM
              PERFORM 900000-EXECUTAR-MODULO
              PERFORM 299100-EXCLUIR-IN-OPR
                        VARYING SGTSL117-IC-TAB FROM 1 BY 1
                          UNTIL SGTSL117-IC-TAB > SGTSL117-QT-OCR-TAB

      * Campos de rechamada da lista
              IF  SGTSL117-QT-OCR-TAB  > ZEROS
                  MOVE SGTSL117-CD-TIP-VLDC-OPR(SGTSL117-QT-OCR-TAB)
                                       TO SGTSL117-ULT-CD-TIP-VLDC-OPR
              END-IF
           END-PERFORM
           .
       299000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       299100-EXCLUIR-IN-OPR           SECTION.
      *   Exclui dado na tabela JST_VLDC_OPR_ETRD
      *----------------------------------------------------------------*
           MOVE 0061                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSE117-DADOS

           MOVE SGTSE117              TO NM-PGM

      *    Movimentar os campos de entrada

           MOVE SGTSL117-CD-CTRA-TIT-VL
                                       TO SGTSE117-CD-CTRA-TIT-VL
           MOVE SGTSL117-CD-OPR-ETRD-MCP
                                       TO SGTSE117-CD-OPR-ETRD-MCP
           MOVE SGTSL117-NR-VRS-OPR-ETRD
                                       TO SGTSE117-NR-VRS-OPR-ETRD
           MOVE SGTSL117-CD-TIP-VLDC-OPR(SGTSL117-IC-TAB)
                                       TO SGTSE117-CD-TIP-VLDC-OPR

           PERFORM 900000-EXECUTAR-MODULO

           .
       299100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       300000-EXCLUIR-OPERACAO         SECTION.
      *   Exclui dado na tabela OPR_ETRD_MCP             commarea
      *----------------------------------------------------------------*
           MOVE 0062                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTST005-DADOS

      *    Movimentar os campos de entrada
           SET SGTST005-EXCLUIR        TO TRUE

           MOVE KOPRD-CD-OPRD          TO SGTST005-CD-USU-ULT-ATL

           MOVE SGTS0146-CD-CTRA-TIT-VL
                                       TO SGTST005-CD-CTRA-TIT-VL
           MOVE SGTS0146-CD-OPR-ETRD-MCP
                                       TO SGTST005-CD-OPR-ETRD-MCP
           MOVE SGTS0146-NR-VRS-OPR-ETRD
                                       TO SGTST005-NR-VRS-OPR-ETRD

           PERFORM 920000-CHAMAR-SGTST005

           .
       300000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       900000-EXECUTAR-MODULO          SECTION.
      *----------------------------------------------------------------*

      *    Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           INITIALIZE MSG1-RESP1 MSG1-RESP2 EIBRESP EIBRESP2

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM

               WHEN SGTSPE03
                  EXEC CICS LINK
                            PROGRAM (SGTSPE03  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL009
                  EXEC CICS LINK
                            PROGRAM (SGTSL009  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL011
                  EXEC CICS LINK
                            PROGRAM (SGTSL011  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE011
                  EXEC CICS LINK
                            PROGRAM (SGTSE011  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL035
                  EXEC CICS LINK
                            PROGRAM (SGTSL035  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE035
                  EXEC CICS LINK
                            PROGRAM (SGTSE035  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL060
                  EXEC CICS LINK
                            PROGRAM (SGTSL060  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE060

                  EXEC CICS LINK
                            PROGRAM (SGTSE060  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL061
                  EXEC CICS LINK
                            PROGRAM (SGTSL061  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE061
                  EXEC CICS LINK
                            PROGRAM (SGTSE061  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL063
                  EXEC CICS LINK
                            PROGRAM (SGTSL063  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE063
                  EXEC CICS LINK
                            PROGRAM (SGTSE063  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL064
                  EXEC CICS LINK
                            PROGRAM (SGTSL064  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE064
                  EXEC CICS LINK
                            PROGRAM (SGTSE064  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL067
                  EXEC CICS LINK
                            PROGRAM (SGTSL067  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE067
                  EXEC CICS LINK
                            PROGRAM (SGTSE067  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL068
                  EXEC CICS LINK
                            PROGRAM (SGTSL068  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE068
                  EXEC CICS LINK
                            PROGRAM (SGTSE068  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL069
                  EXEC CICS LINK
                            PROGRAM (SGTSL069  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE069
                  EXEC CICS LINK
                            PROGRAM (SGTSE069  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL070
                  EXEC CICS LINK
                            PROGRAM (SGTSL070  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE070
                  EXEC CICS LINK
                            PROGRAM (SGTSE070  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC
               WHEN SGTSL507
                  EXEC CICS LINK
                            PROGRAM (SGTSL507  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE071
                  EXEC CICS LINK
                            PROGRAM (SGTSE071  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL074
                  EXEC CICS LINK
                            PROGRAM (SGTSL074  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE074
                  EXEC CICS LINK
                            PROGRAM (SGTSE074  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL075
                  EXEC CICS LINK
                            PROGRAM (SGTSL075  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE075
                  EXEC CICS LINK
                            PROGRAM (SGTSE075  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL076
                  EXEC CICS LINK
                            PROGRAM (SGTSL076 )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE076
                  EXEC CICS LINK
                            PROGRAM (SGTSE076  )
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

               WHEN SGTSE077
                  EXEC CICS LINK
                            PROGRAM (SGTSE077  )
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

               WHEN SGTSE078
                  EXEC CICS LINK
                            PROGRAM (SGTSE078  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL079
                  EXEC CICS LINK
                            PROGRAM (SGTSL079  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE079
                  EXEC CICS LINK
                            PROGRAM (SGTSE079  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL080
                  EXEC CICS LINK
                            PROGRAM (SGTSL080  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE080
                  EXEC CICS LINK
                            PROGRAM (SGTSE080  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL081
                  EXEC CICS LINK
                            PROGRAM (SGTSL081  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE081
                  EXEC CICS LINK
                            PROGRAM (SGTSE081  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL082
                  EXEC CICS LINK
                            PROGRAM (SGTSL082  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE082
                  EXEC CICS LINK
                            PROGRAM (SGTSE082  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC
               WHEN SGTSL083
                  EXEC CICS LINK
                            PROGRAM (SGTSL083  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE083
                  EXEC CICS LINK
                            PROGRAM (SGTSE083  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL084
                  EXEC CICS LINK
                            PROGRAM (SGTSL084  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE084
                  EXEC CICS LINK
                            PROGRAM (SGTSE084  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL087
                  EXEC CICS LINK
                            PROGRAM (SGTSL087  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE087
                  EXEC CICS LINK
                            PROGRAM (SGTSE087  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSL088
                  EXEC CICS LINK
                            PROGRAM (SGTSL088  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN SGTSE088
                  EXEC CICS LINK
                            PROGRAM (SGTSE088  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

              WHEN SGTSL095
                 EXEC CICS LINK
                           PROGRAM (SGTSL095  )
                           CHANNEL (NM-CNL    )
                           RESP    (MSG1-RESP1)
                           RESP2   (MSG1-RESP2)
                 END-EXEC

               WHEN SGTSE095
                  EXEC CICS LINK
                            PROGRAM (SGTSE095  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

              WHEN SGTSL117
                 EXEC CICS LINK
                           PROGRAM (SGTSL117  )
                           CHANNEL (NM-CNL    )
                           RESP    (MSG1-RESP1)
                           RESP2   (MSG1-RESP2)
                 END-EXEC

               WHEN SGTSE117
                  EXEC CICS LINK
                            PROGRAM (SGTSE117  )
                            CHANNEL (NM-CNL    )
                            RESP    (MSG1-RESP1)
                            RESP2   (MSG1-RESP2)
                  END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO     TO TRUE
                    MOVE  9006          TO  MSG1-CD-ERRO-TS

                    STRING
                    'Não definida a execução do módulo ' NM-PGM
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS

                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA

           END-EVALUATE
      *
           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CICS      TO TRUE
                    MOVE 3004          TO    MSG1-CD-ERRO-TS
                    STRING
                   'Erro na execução do módulo ' NM-PGM ', '
                   'RESP  ='   ' ' MSG1-RESP1  ', '
                   'RESP2 =' ' ' MSG1-RESP2
                    DELIMITED BY SIZE  INTO  MSG1-TX-ERRO-TS
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *    Efetua a leitura do Container de Resposta
      **   SGTSE035, e SGTSE076 não gravam container de resposta.
           IF NM-PGM  NOT EQUAL SGTSE035  AND
              NM-PGM  NOT EQUAL SGTSE076
              PERFORM 973000-LER-RESPOSTA
           END-IF
           .
       900000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       920000-CHAMAR-SGTST005          SECTION.
      *----------------------------------------------------------------*
           MOVE 0063                   TO MSG1-CD-SEQL-ERRO

      *    Movimento nome do módulo para monitoração de variáveis

           MOVE SGTST005               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST005)
                COMMAREA (SGTST005-DADOS)
           END-EXEC

           IF  SGTST005-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND              D
               MOVE SGTST005-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE SGTST005-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       920000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       920001-CHAMAR-SGTST006          SECTION.
      *----------------------------------------------------------------*
           MOVE 0064                   TO MSG1-CD-SEQL-ERRO

      *    Movimento nome do módulo para monitoração de variáveis

           MOVE SGTST006               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST006)
                COMMAREA (SGTST006-DADOS)
           END-EXEC

           IF  SGTST006-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND              D
               MOVE SGTST006-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE SGTST006-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       920001-SAI.
           EXIT.

      *----------------------------------------------------------------*
       920002-CHAMAR-SGTST009          SECTION.
      *----------------------------------------------------------------*
           MOVE 0065                   TO MSG1-CD-SEQL-ERRO

      *    Movimento nome do módulo para monitoração de variáveis

           MOVE SGTST009              TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST009)
                COMMAREA (SGTST009-DADOS)
           END-EXEC

           IF  SGTST009-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND
               MOVE SGTST009-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE SGTST009-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       920002-SAI.
           EXIT.

      *----------------------------------------------------------------*
       920003-CHAMAR-SGTSL006          SECTION.
      *----------------------------------------------------------------*
           MOVE 0066                   TO MSG1-CD-SEQL-ERRO

      *    Movimento nome do módulo para monitoração de variáveis

           MOVE SGTSL006               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTSL006)
                COMMAREA (SGTSL006-DADOS)
           END-EXEC

           IF  SGTSL006-CD-RTN-PGM     NOT EQUAL ZEROS AND 100
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND

               MOVE SGTSL006-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE SGTSL006-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       920003-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*
           EVALUATE NM-PGM

               WHEN SGTSPE03
                  MOVE 'SGTSPE03-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSPE03-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSPE03-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSPE03-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSPE03-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSPE03-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTST005
                  MOVE 'SGTST005-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST005-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTST005-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST005-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTST005-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST005-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTST006
                  MOVE 'SGTST006-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST006-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTST006-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST006-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTST006-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST006-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTST006-CD-CLI'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST006-CD-CLI
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTST006-CD-TIP-FUC-CLI'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST006-CD-TIP-FUC-CLI
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL006
                  MOVE 'SGTSL006-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL006-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL006-ULT-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL006-ULT-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL006-ULT-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL006-ULT-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL006-ULT-CD-CLI'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL006-ULT-CD-CLI
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL006-ULT-TIP-FUC-CLI'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL006-ULT-TIP-FUC-CLI
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL009
                  MOVE 'SGTSL009-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL009-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL009-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL009-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL009-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL009-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL009-TS-EST-OPR-ETRD-ULT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL009-TS-EST-OPR-ETRD-ULT
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT


               WHEN SGTST009
                  MOVE 'SGTST009-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST009-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTST009-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST009-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTST009-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST009-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTST009-TS-EST-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST009-TS-EST-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTST009-CD-USU-RSP-ATL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST009-CD-USU-RSP-ATL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTST009-MSG-ERRO-PGM'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST009-MSG-ERRO-PGM
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTST009-CD-RTN-PGM'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST009-CD-RTN-PGM
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTST009-TIP-RTN-PGM'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST009-TIP-RTN-PGM
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTST009-ATB-MSG'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST009-ATB-MSG
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                   MOVE 'SGTST009-TX-IDFC-ATNT-MSG'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTST009-TX-IDFC-ATNT-MSG
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL009
                  MOVE 'SGTSL009-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL009-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL009-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL009-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL009-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL009-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL009-TS-EST-OPR-ETRD-ULT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL009-TS-EST-OPR-ETRD-ULT
                                       TO MSGKW999-VL-VRV-MNT


               WHEN SGTSL011
                  MOVE 'SGTSL011-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL011-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL011-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL011-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
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

               WHEN SGTSE011
                  MOVE 'SGTSE011-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE011-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE011-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE011-CD-OPR-ETRD-MCP

                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE011-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE011-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE011-DT-INC-TAXA-RMNC'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE011-DT-INC-TAXA-RMNC
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT


               WHEN SGTSL035
                  MOVE 'SGTSL035-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL035-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
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

               WHEN SGTSE035
                  MOVE 'SGTSE035-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE035-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE035-CD-OPR-ETRD-MC'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE035-CD-OPR-ETRD-MC
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE035-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE035-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE035-CD-PSS-CTT-EXNO'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE035-CD-PSS-CTT-EXNO
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT


               WHEN SGTSL060
                  MOVE 'SGTSL060-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL060-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL060-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL060-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
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

               WHEN SGTSE060
                  MOVE 'SGTSE060-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE060-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE060-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE060-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE060-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE060-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE060-CD-ARQ-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE060-CD-ARQ-OPR-ETRD
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
                  MOVE SGTSL061-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
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

               WHEN SGTSE061
                  MOVE 'SGTSE061-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE061-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE061-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE061-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE061-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE061-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE061-DT-RPAC-PGMD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE061-DT-RPAC-PGMD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL063
                  MOVE 'SGTSL063-CD-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                  MOVE SGTSL063-CD-ACAO
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL063-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL063-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL063-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL063-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
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

               WHEN SGTSE063
                  MOVE 'SGTSE063-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE063-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE063-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE063-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE063-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE063-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE063-CD-TIP-IN'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE063-CD-TIP-IN
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL064
                  MOVE 'SGTSL064-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL064-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL064-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL064-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
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

               WHEN SGTSE064
                  MOVE 'SGTSE064-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE064-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE064-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE064-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE064-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE064-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE064-CD-BCO-PCT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE064-CD-BCO-PCT
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE064-CD-TIP-PRTC'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE064-CD-TIP-PRTC
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT


               WHEN SGTSL067
                  MOVE 'SGTSL067-CD-TIP-SRVC'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL067-CD-TIP-SRVC
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL067-E-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL067-E-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
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

               WHEN SGTSE067
                  MOVE 'SGTSE067-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE067-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE067-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE067-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE067-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE067-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL067-E-NR-NOTA-TCN-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL067-E-NR-NOTA-TCN-OPR
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
                  MOVE SGTSL068-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
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

               WHEN SGTSE068
                  MOVE 'SGTSE068-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE068-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE068-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE068-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE068-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE068-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE068-CD-TIP-TAXA'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE068-CD-TIP-TAXA
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE068-DT-INC-CND-TAXA'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE068-DT-INC-CND-TAXA
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL069
                  MOVE 'SGTSL069-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL069-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL069-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL069-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL069-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL069-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL069-TS-EST-VRS-PAG'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL069-TS-EST-VRS-PAG
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSE069
                  MOVE 'SGTSE069-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE069-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE069-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE069-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE069-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE069-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE069-TS-EST-VRS'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE069-TS-EST-VRS
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL070
                  MOVE 'SGTSL070-E-CD-TIP-SRVC'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL070-E-CD-TIP-SRVC
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL070-E-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL070-E-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL070-E-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL070-E-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
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

                  MOVE 'SGTSL070-E-CD-TIP-RCBT-PRVT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL070-E-CD-TIP-RCBT-PRVT
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL070-DT-RCBT-PRVT-ULT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL070-DT-RCBT-PRVT-ULT
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSE070
                  MOVE 'SGTSE070-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE070-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE070-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE070-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE070-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE070-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE070-NR-SEQL-RCBT-PRVT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE070-NR-SEQL-RCBT-PRVT
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE070-CD-TIP-RCBT-PRVT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE070-CD-TIP-RCBT-PRVT
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE070-DT-RCBT-PRVT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE070-DT-RCBT-PRVT
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL507
                  MOVE 'SGTSL507-CD-CTRA-OPR-PGDR'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL507-CD-CTRA-OPR-PGDR
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL507-CD-OPR-PGDR'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL507-CD-OPR-PGDR
                                       TO MSGKW999-VL-VRV-MNT
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

                  MOVE 'SGTSL507-NR-VRS-OPR-VCLD-E'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL507-NR-VRS-OPR-VCLD-E
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSE071
                  MOVE 'SGTSE071-CD-CTRA-OPR-PGDR'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE071-CD-CTRA-OPR-PGDR
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE071-CD-OPR-PGDR'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE071-CD-OPR-PGDR
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE071-NR-VRS-OPR-PGDR'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE071-NR-VRS-OPR-PGDR
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE ' SGTSE071-CD-CTRA-OPR-VCLD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE  SGTSE071-CD-CTRA-OPR-VCLD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE071-CD-OPR-ETRD-VCLD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE071-CD-OPR-ETRD-VCLD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE071-NR-VRS-OPR-VCLD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE071-NR-VRS-OPR-VCLD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

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
                  MOVE SGTSL074-E-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL074-E-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL074-E-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL074-E-NR-SEQL-NOTA-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL074-E-NR-SEQL-NOTA-OPR
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSE074
                  MOVE 'SGTSE074-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE074-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE074-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE074-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE074-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE074-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE074-NR-SEQL-NOTA-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE074-NR-SEQL-NOTA-OPR
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL075
                  MOVE 'SGTSL075-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL075-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL075-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL075-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL075-NR-ULT-SEQL-ORD-NEGO'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL075-NR-ULT-SEQL-ORD-NEGO
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSE075
                  MOVE 'SGTSE075-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE075-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE075-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE075-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE075-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE075-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE075-NR-SEQL-ORD-NEGO'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE075-NR-SEQL-ORD-NEGO
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL076
                  MOVE 'SGTSL076-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL076-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL076-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL076-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL076-NR-SEQL-ORD-NEGO-ULT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL076-NR-SEQL-ORD-NEGO-ULT
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL076-CD-CLI-CPRD-ULT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL076-CD-CLI-CPRD-ULT
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSE076
                  MOVE 'SGTSE076-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE076-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE076-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE076-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE076-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE076-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE076-NR-SEQL-ORD-NEGO'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE076-NR-SEQL-ORD-NEGO
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE076-CD-CLI-CPRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE076-CD-CLI-CPRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL077
                  MOVE 'SGTSL077-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL077-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE077-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE077-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL077-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL077-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL077-NR-SEQL-ULT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL077-NR-SEQL-ULT
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL077-TS-AUTZ-ULT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL077-TS-AUTZ-ULT
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSE077
                  MOVE 'SGTSE077-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE077-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE077-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE077-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE077-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE077-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE077-NR-SEQL-ORD-NEGO'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE077-NR-SEQL-ORD-NEGO
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE077-TS-AUTZ-NEGO'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE077-TS-AUTZ-NEGO
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL078
                  MOVE 'SGTSL078-CD-TIP-SRVC'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL078-CD-TIP-SRVC
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL078-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL078-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE078-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE078-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL078-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL078-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL078-NR-SEQL-ORD-ULT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL078-NR-SEQL-ORD-ULT
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL078-TS-EST-ULT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL078-TS-EST-ULT
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSE078
                  MOVE 'SGTSE078-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE078-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE078-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE078-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE078-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE078-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE078-NR-SEQL-ORD-NEGO'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE078-NR-SEQL-ORD-NEGO
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL078-TS-EST-ULT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL078-TS-EST-ULT
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
                  MOVE SGTSL079-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
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

               WHEN SGTSE079
                  MOVE 'SGTSE079-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE079-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE079-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE079-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE079-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE079-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE079-CD-TIP-CND-NEGD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE079-CD-TIP-CND-NEGD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE079-DT-INC-VLD-CND'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE079-DT-INC-VLD-CND
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
                  MOVE SGTSL080-E-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
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

               WHEN SGTSE080
                  MOVE 'SGTSE080-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE080-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE080-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE080-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE080-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE080-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE080-NR-ISLC-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE080-NR-ISLC-OPR
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL081
                  MOVE 'SGTSL081-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL081-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
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

               WHEN SGTSE081
                  MOVE 'SGTSE081-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE081-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE081-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE081-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE081-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE081-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE081-NR-PRV-CMPS-GRT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE081-NR-PRV-CMPS-GRT
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL082
                  MOVE 'SGTSL082-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL082-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL082-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL082-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
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

               WHEN SGTSE082
                  MOVE 'SGTSE082-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE082-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE082-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE082-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE082-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE082-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE082-NR-PRV-CMPS-GRT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE082-NR-PRV-CMPS-GRT
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE082-CD-CLI'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE082-CD-CLI
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL083
                  MOVE 'SGTSL083-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL083-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL083-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL083-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
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

               WHEN SGTSE083
                  MOVE 'SGTSE083-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE083-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE083-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE083-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE083-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE083-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE083-NR-SEQL-EXCO-PRFL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE083-NR-SEQL-EXCO-PRFL
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
                  MOVE SGTSL084-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
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

               WHEN SGTSE084
                  MOVE 'SGTSE084-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE084-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT
                  MOVE 'SGTSE084-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE084-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT
                  MOVE 'SGTSE084-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE084-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT
                  MOVE 'SGTSE084-CD-TIP-TARF'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE084-CD-TIP-TARF
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
                  MOVE SGTSL087-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
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

               WHEN SGTSE087
                  MOVE 'SGTSE087-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE087-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE087-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE087-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE087-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE087-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE087-NR-SEQL-RCBT-PRVT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE087-NR-SEQL-RCBT-PRVT
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE087-CD-TIP-RCBT-PRVT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE087-CD-TIP-RCBT-PRVT
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE087-DT-RCBT-PRVT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE087-DT-RCBT-PRVT
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE087-CD-TIT-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE087-CD-TIT-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE087-NR-DVS-TIT'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE087-NR-DVS-TIT
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE087-TS-REG-PGTO'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE087-TS-REG-PGTO
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL088
                  MOVE 'SGTSL088-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL088-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL088-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL088-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL088-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL088-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSL088-E-ULT-TS-EST-RET'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSL088-E-ULT-TS-EST-RET
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSE088
                  MOVE 'SGTSE088-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE088-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE088-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE088-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE088-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE088-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE088-TS-EXEA-CLC'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE088-TS-EXEA-CLC
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
                  MOVE SGTSL095-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
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

               WHEN SGTSE095
                  MOVE 'SGTSE095-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE095-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE095-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE095-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE095-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE095-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE095-NR-SEQL-EXCO-PRFL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE095-NR-SEQL-EXCO-PRFL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE095-NR-SEQL-ITEM-EXCO'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE095-NR-SEQL-ITEM-EXCO
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
                  MOVE SGTSL117-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
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

               WHEN SGTSE117
                  MOVE 'SGTSE117-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE117-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE117-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE117-CD-OPR-ETRD-MCP
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE117-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE117-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTSE117-CD-TIP-VLDC-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                  MOVE SGTSE117-CD-TIP-VLDC-OPR
                                       TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN OTHER
                    CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      * SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
      *                                                                *
      *----------------------------------------------------------------*
       970000-LER-CANAL                SECTION.
      *    Obtem o canal corrente
      *----------------------------------------------------------------*
           MOVE 0067                   TO MSG1-CD-SEQL-ERRO

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
      *
      *----------------------------------------------------------------*
       971000-LER-REQUISICAO           SECTION.
      *----------------------------------------------------------------*
           MOVE 0068                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisicao
           MOVE CTE-PROG(5:8)          TO NM-PGM
           MOVE 'SGTS0146-RQSC'        TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      * -->> Início

      *    Obtem o tamanho esperado da requisicao
           MOVE LENGTH OF SGTS0146-RQSC TO QT-CCT-EPRO

      *    Valida tamanho esperado da requisicao X tamanho recebido

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA   TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
      * -->> Fim

           MOVE 0069                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0146-RQSC)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       971000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *----------------------------------------------------------------*
      *    A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *----------------------------------------------------------------*
      *    Le o container de erro dos modulos chamados
           SET GET-CONTAINER           TO TRUE
           MOVE 'PRM-ERRO'             TO NM-CTNR
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
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
      *        PARA O MODULO CHAMADO e tratar na section chamadora
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                    MOVE  NM-PGM       TO  MSG1-NM-PGM-ACND

                    IF NM-PGM(1:3) EQUAL 'SGT'
                       MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                                           MSG1-CD-ERRO-TS
                       MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
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
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA

              WHEN OTHER
                    SET ERRO-CONTAINER  TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       973000-LER-RESPOSTA             SECTION.
      *----------------------------------------------------------------*
      *    Recupera o container com os dados de resposta (saída).
      *
      *                        NOME DO CONTAINER RESPOSTA
      *                        --------------------------
      *
      *    Modulo CONTAINER  -     NOME_PGM-RPST
      *    Modulo IIB (FIXO) -     PRM-DADOS-RPST
      *
      *---------------------------------------------------------------*
      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

      *    Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

      *Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Le a RESPOSTA de acordo com o programa que a gravou

           EVALUATE NM-PGM
      *
      *        Calcular a quantidade de caracteres esperado e limpar
      *        a area de dados antes de executar o GET
      *        Para matrizes com dados variaveis (clausula depending
      *        on) nao ha como utilizar o comando INITIALIZE
      *
      *** Foi retirado o tratamento de get para O SGTSE035, pois este
      * não gera mais container de resposta a partir de 03/2018.
               WHEN SGTSPE03
                     EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSPE03-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL009
                     EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL009-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL011
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL011-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE011
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE011-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL035
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL035-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL060
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL060-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE060
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE060-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL061
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL061-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE061
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE061-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL063
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL063-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE063
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE063-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL064
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL064-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE064
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE064-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL067
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL067-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE067
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE067-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL068
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL068-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE068
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE068-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL069
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL069-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE069
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE069-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL070
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL070-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE070
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE070-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL507
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL507-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE071
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE071-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL074
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL074-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE074
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE074-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL075
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL075-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE075
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE075-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL076
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL076-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

      *        WHEN SGTSE076
      *             EXEC CICS GET
      *                       CONTAINER (NM-CTNR       )
      *                       INTO      (SGTSE076-RPST)
      *                       FLENGTH   (QT-CCT-LIDO   )
      *             END-EXEC

               WHEN SGTSL077
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL077-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE077
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE077-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL078
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL078-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE078
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE078-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL079
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL079-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE079
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE079-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL080
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL080-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE080
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE080-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL081
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL081-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE081
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE081-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL082
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL082-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE082
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE082-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL083
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL083-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE083
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE083-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL084
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL084-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE084
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE084-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL087
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL087-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE087
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE087-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL088
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL088-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE088
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE088-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL095
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL095-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE095
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE095-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL117
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSL117-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSE117
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (SGTSE117-RPST)
                              FLENGTH   (QT-CCT-LIDO   )
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

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       973100-LER-CONTAINER-NODATA     SECTION.
      *----------------------------------------------------------------*
      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *    Obtem a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR    )
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       974000-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE 0070                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           MOVE 'PRM-INF-OPRD'         TO NM-CTNR

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
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
       974000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *----------------------------------------------------------------*
      *    Postar container com dados de requisição (entrada)
      *
      *                        NOME DO CONTAINER REQUISICAO
      *                        ----------------------------
      *    Modulo CONTAINER  -     NOME_PGM-RQSC
      *    Modulo IIB (FIXO) -     PRM-DADOS-RQSC para NM-CTNR
      *
      *----------------------------------------------------------------*
           MOVE 0071                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RQSC'                TO NM-CTNR(9:5)

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisicao do programa a executar

           EVALUATE NM-PGM

               WHEN SGTSPE03
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSPE03-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSPE03-RQSC )
                    END-EXEC

               WHEN SGTSL009
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL009-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL009-RQSC )
                    END-EXEC

               WHEN SGTSL011
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL011-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL011-RQSC )
                    END-EXEC

               WHEN SGTSE011
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE011-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE011-RQSC )
                    END-EXEC

               WHEN SGTSL035
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL035-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL035-RQSC )
                    END-EXEC

               WHEN SGTSE035
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE035-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE035-RQSC )
                    END-EXEC

               WHEN SGTSL060
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL060-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL060-RQSC )
                    END-EXEC

               WHEN SGTSE060
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE060-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE060-RQSC )
                    END-EXEC

               WHEN SGTSL061
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL061-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL061-RQSC )
                    END-EXEC

               WHEN SGTSE061
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE061-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE061-RQSC )
                    END-EXEC

               WHEN SGTSL063
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL063-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL063-RQSC )
                    END-EXEC

               WHEN SGTSE063
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE063-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE063-RQSC )
                    END-EXEC

               WHEN SGTSL064
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL064-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL064-RQSC )
                    END-EXEC

               WHEN SGTSE064
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE064-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE064-RQSC )
                    END-EXEC

               WHEN SGTSL067
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL067-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL067-RQSC )
                    END-EXEC

               WHEN SGTSE067
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE067-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE067-RQSC )
                    END-EXEC

               WHEN SGTSL068
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL068-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL068-RQSC )
                    END-EXEC

               WHEN SGTSE068
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE068-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE068-RQSC )
                    END-EXEC

               WHEN SGTSL069
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL069-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL069-RQSC )
                    END-EXEC

               WHEN SGTSE069
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE069-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE069-RQSC )
                    END-EXEC

               WHEN SGTSL070
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL070-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL070-RQSC )
                    END-EXEC

               WHEN SGTSE070
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE070-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE070-RQSC )
                    END-EXEC

               WHEN SGTSL507
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL507-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL507-RQSC )
                    END-EXEC

               WHEN SGTSE071
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE071-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE071-RQSC )
                    END-EXEC

               WHEN SGTSL074
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL074-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL074-RQSC )
                    END-EXEC

               WHEN SGTSE074
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE074-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE074-RQSC )
                    END-EXEC

               WHEN SGTSL075
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL075-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL075-RQSC )
                    END-EXEC

               WHEN SGTSE075
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE075-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE075-RQSC )
                    END-EXEC

               WHEN SGTSL076
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL076-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL076-RQSC )
                    END-EXEC

               WHEN SGTSE076
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE076-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE076-RQSC )
                    END-EXEC

               WHEN SGTSL077
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL077-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL077-RQSC )
                    END-EXEC

               WHEN SGTSE077
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE077-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE077-RQSC )
                    END-EXEC

               WHEN SGTSL078
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL078-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL078-RQSC )
                    END-EXEC

               WHEN SGTSE078
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE078-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE078-RQSC )
                    END-EXEC

               WHEN SGTSL079
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL079-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL079-RQSC )
                    END-EXEC

               WHEN SGTSE079
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE079-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE079-RQSC )
                    END-EXEC

               WHEN SGTSL080
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL080-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL080-RQSC )
                    END-EXEC

               WHEN SGTSE080
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE080-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE080-RQSC )
                    END-EXEC

               WHEN SGTSL081
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL081-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL081-RQSC )
                    END-EXEC

               WHEN SGTSE081
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE081-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE081-RQSC )
                    END-EXEC

              WHEN SGTSL082
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL082-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL082-RQSC )
                    END-EXEC

              WHEN SGTSE082
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE082-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE082-RQSC )
                    END-EXEC

              WHEN SGTSL083
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL083-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL083-RQSC )
                    END-EXEC

              WHEN SGTSE083
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE083-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE083-RQSC )
                    END-EXEC
               WHEN SGTSL084
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL084-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL084-RQSC )
                    END-EXEC

               WHEN SGTSE084
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE084-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE084-RQSC )
                    END-EXEC

               WHEN SGTSL087
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL087-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL087-RQSC )
                    END-EXEC

               WHEN SGTSE087
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE087-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE087-RQSC )
                    END-EXEC

               WHEN SGTSL088
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL088-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL088-RQSC )
                    END-EXEC

               WHEN SGTSE088
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE088-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE088-RQSC )
                    END-EXEC

               WHEN SGTSL095
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL095-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL095-RQSC )
                    END-EXEC

               WHEN SGTSE095
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE095-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE095-RQSC )
                    END-EXEC

               WHEN SGTSL117
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSL117-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSL117-RQSC )
                    END-EXEC

               WHEN SGTSE117
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTSE117-RQSC            )
                              FLENGTH   (LENGTH  OF SGTSE117-RQSC )
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9004         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na gravação da requisição do módulo '
                            NM-PGM '.' DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       982000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           MOVE NM-PGM          TO NM-CTNR(1:8)
           MOVE '-RPST'         TO NM-CTNR(9:5)

           SET DEL-CONTAINER           TO TRUE

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
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *    Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *----------------------------------------------------------------*
      *    Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE 'PRM-ERRO'             TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER (NM-CTNR   )
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
      *
      *----------------------------------------------------------------*
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *----------------------------------------------------------------*
      *    Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS PUT
                     CONTAINER (PRM-ERRO                   )
                     FROM      (IIBKERRO-PRM-ERRO          )
                     FLENGTH   (LENGTH OF IIBKERRO-PRM-ERRO)
                     RESP      (MSG1-RESP1                 )
                     RESP2     (MSG1-RESP2                 )
           END-EXEC
           .
       984000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       990000-ENCERRAR                 SECTION.
      *----------------------------------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC
           .
       990000-ENCERRAR-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *     As sections abaixo representam os tratamentos de erro
      *    utilizando o sistema MSG de mensageria (rotina MSGS0001)
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------
      *
           IF MSG1-CD-ERRO-TS EQUAL ZEROS
              MOVE 8888  TO MSG1-CD-ERRO-TS
           END-IF

           IF MSG1-TX-ERRO-TS EQUAL SPACES
              MOVE 'Erro não especificado no módulo.' TO MSG1-TX-ERRO-TS
           END-IF
      *
      *    Em caso de erro, desfaz as atualizações em banco de dados

           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC

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

      *Adiciona as variáveis de entrada para monitoração

           MOVE 'SGTS0146-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
           MOVE SGTS0146-CD-CTRA-TIT-VL   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0146-CD-OPR-ETRD-MCP'
                                          TO MSGKW999-NM-VRV-MNT
           IF  SGTS0146-CD-OPR-ETRD-MCP  IS NUMERIC
               MOVE SGTS0146-CD-OPR-ETRD-MCP
                                          TO MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Código não numérico.'
                                          TO  MSGKW999-VL-VRV-MNT
           END-IF

           MOVE 'SGTS0146-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTS0146-NR-VRS-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *    Grava o erro gerado no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG

           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO

      *    Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

      *    Encerra a transacao
           PERFORM 990000-ENCERRAR
           .
       999000-SAI.
           EXIT.
      * --------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * --------------------------------------*
       COPY HLPKP999.
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
       COPY SGTKP999.
      * --------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
      * --------------------------------------*
       COPY MSGKPMG1.
      *---------------------- FIM SGTS0146 ----------------------------*
